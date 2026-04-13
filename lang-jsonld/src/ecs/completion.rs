use std::{borrow::Cow, collections::HashSet};

use bevy_ecs::prelude::*;
use completion::{CompletionRequest, SimpleCompletion};
use swls_core::{
    lsp_types::{CompletionItemKind, TextEdit},
    prelude::*,
    systems::{prefix::prefix_completion_helper, PrefixEntry},
};
use swls_lov::LocalPrefix;
use tracing::instrument;

use crate::{ecs::JsonLdActiveContext, JsonLdLang};

/// Strip the leading and trailing JSON string quotes from a token's text.
/// Returns the unquoted content, or the original text if it is not a quoted string.
fn unquote(text: &str) -> &str {
    let s = text.strip_prefix('"').unwrap_or(text);
    s.strip_suffix('"').unwrap_or(s)
}

// ── context-editing helper ────────────────────────────────────────────────────

/// What to add to a JSON-LD `@context`.
pub enum ContextEntry<'a> {
    /// A prefix declaration: `"name": "namespace"` inside the context object.
    Prefix { name: &'a str, namespace: &'a str },
    /// A remote context URL added as a plain string.
    Url(&'a str),
}

/// Given that `bytes[start]` is the opening character of a JSON value, return
/// the index one past the closing character of that value.
fn find_value_end(bytes: &[u8], start: usize) -> Option<usize> {
    match bytes.get(start)? {
        b'"' => {
            let mut i = start + 1;
            while i < bytes.len() {
                match bytes[i] {
                    b'\\' => i += 2,
                    b'"' => return Some(i + 1),
                    _ => i += 1,
                }
            }
            None
        }
        b'{' | b'[' => {
            let (open, close) = if bytes[start] == b'{' {
                (b'{', b'}')
            } else {
                (b'[', b']')
            };
            let mut depth = 0usize;
            let mut in_str = false;
            let mut i = start;
            while i < bytes.len() {
                if in_str {
                    match bytes[i] {
                        b'\\' => i += 1,
                        b'"' => in_str = false,
                        _ => {}
                    }
                } else {
                    match bytes[i] {
                        b'"' => in_str = true,
                        c if c == open => depth += 1,
                        c if c == close => {
                            depth -= 1;
                            if depth == 0 {
                                return Some(i + 1);
                            }
                        }
                        _ => {}
                    }
                }
                i += 1;
            }
            None
        }
        _ => None,
    }
}

/// Locate the byte span of the `@context` value inside `source`.
fn find_context_value_span(source: &str) -> Option<std::ops::Range<usize>> {
    let key = "\"@context\"";
    let key_pos = source.find(key)?;
    let after_key = key_pos + key.len();

    let colon_offset = source[after_key..].find(':')?;
    let after_colon = after_key + colon_offset + 1;

    let ws_offset = source[after_colon..].find(|c: char| !c.is_whitespace())?;
    let value_start = after_colon + ws_offset;

    let value_end = find_value_end(source.as_bytes(), value_start)?;
    Some(value_start..value_end)
}

/// Produce the new `@context` value string with `entry` added, or `None` if
/// the entry is already present or the value shape is not supported.
fn build_new_context(current: &str, entry: &ContextEntry<'_>) -> Option<String> {
    match current.trim_start().as_bytes().first()? {
        b'{' => match entry {
            ContextEntry::Prefix { name, namespace } => {
                let close = current.rfind('}')?;
                let inner = current[1..close].trim();
                if inner.is_empty() {
                    Some(format!("{{\"{}\":\"{}\"}}", name, namespace))
                } else {
                    Some(format!(
                        "{}, \"{}\":\"{}\"}}",
                        &current[..close],
                        name,
                        namespace
                    ))
                }
            }
            ContextEntry::Url(url) => {
                // A URL cannot go inside an object — convert to array.
                Some(format!("[{}, \"{}\"]", current, url))
            }
        },
        b'"' => match entry {
            ContextEntry::Prefix { name, namespace } => {
                Some(format!("[{}, {{\"{}\":\"{}\"}}]", current, name, namespace))
            }
            ContextEntry::Url(url) => Some(format!("[{}, \"{}\"]", current, url)),
        },
        b'[' => {
            let close = current.rfind(']')?;
            let inner = current[1..close].trim();
            let new_entry = match entry {
                ContextEntry::Prefix { name, namespace } => {
                    format!("{{\"{}\":\"{}\"}}", name, namespace)
                }
                ContextEntry::Url(url) => format!("\"{}\"", url),
            };
            if inner.is_empty() {
                Some(format!("[{}]", new_entry))
            } else {
                Some(format!("{}, {}]", &current[..close], new_entry))
            }
        }
        _ => None,
    }
}

/// Find the byte position just after the first `{` of the top-level JSON object,
/// skipping leading whitespace. Returns `None` if no `{` is found.
fn find_toplevel_object_open(source: &str) -> Option<usize> {
    let offset = source.find('{')?;
    Some(offset + 1)
}

/// Build the new-context value string for a fresh `@context` insertion.
fn new_context_value(entry: &ContextEntry<'_>) -> String {
    match entry {
        ContextEntry::Prefix { name, namespace } => {
            format!("{{\"{}\":\"{}\"}}", name, namespace)
        }
        ContextEntry::Url(url) => format!("\"{}\"", url),
    }
}

/// Returns a `TextEdit` that inserts `entry` into the `@context` of the JSON-LD
/// document described by `source` / `rope`. Returns `None` when:
/// - the entry (prefix key or URL string) is already present, or
/// - the existing value shape cannot be extended.
///
/// When no `@context` key exists yet, a new one is inserted right after the
/// opening `{` of the top-level JSON object.
pub fn add_to_context(
    source: &str,
    rope: &ropey::Rope,
    entry: ContextEntry<'_>,
) -> Option<Vec<TextEdit>> {
    match find_context_value_span(source) {
        Some(span) => {
            let current = &source[span.clone()];

            // Check whether the entry is already present.
            match &entry {
                ContextEntry::Prefix { name, .. } => {
                    if current.contains(&format!("\"{}\"", name)) {
                        return None;
                    }
                }
                ContextEntry::Url(url) => {
                    if current.contains(&format!("\"{}\"", url)) {
                        return None;
                    }
                }
            }

            let new_text = build_new_context(current, &entry)?;
            let range = range_to_range(&span, rope)?;
            Some(vec![TextEdit { range, new_text }])
        }
        None => {
            // No @context present — insert one after the opening `{`.
            let insert_pos = find_toplevel_object_open(source)?;
            let context_value = new_context_value(&entry);
            let new_text = format!("\"@context\": {}, ", context_value);
            let range = range_to_range(&(insert_pos..insert_pos), rope)?;
            Some(vec![TextEdit { range, new_text }])
        }
    }
}

// ── prefix completion ─────────────────────────────────────────────────────────

/// JSON-LD prefix completion from LOV / prefix.cc.
///
/// Suggests prefixes that are not yet declared in the document's `@context` and
/// produces an additional [`TextEdit`] via [`add_to_context`] that inserts the
/// new prefix declaration into the `@context` value.
///
/// Uses [`DynLang::quote`] so the inserted predicate text is correctly wrapped
/// in JSON string quotes (e.g. `"foaf:"`).
// #[instrument(skip(query, lovs, prefix_cc, config))]
// pub fn jsonld_lov_undefined_prefix_completion(
//     mut query: Query<
//         (
//             &TokenComponent,
//             &Prefixes,
//             &Source,
//             &RopeC,
//             &DynLang,
//             &mut CompletionRequest,
//         ),
//         With<JsonLdLang>,
//     >,
//     lovs: Query<&LocalPrefix>,
//     prefix_cc: Query<&PrefixEntry>,
//     config: Res<ServerConfig>,
// ) {
//     for (word, prefixes, source, rope, lang, mut req) in &mut query {
//         let bare_text = unquote(&word.text);
//
//         let defined_namespaces: HashSet<&str> = prefixes.0.iter().map(|p| p.url.as_str()).collect();
//         let mut suggested: HashSet<String> = HashSet::new();
//
//         let candidates = lovs
//             .iter()
//             .map(|l| {
//                 (
//                     l.name.as_ref(),
//                     l.namespace.as_ref(),
//                     Some(l.title.as_ref()),
//                 )
//             })
//             .chain(
//                 prefix_cc
//                     .iter()
//                     .filter(|p| {
//                         !config
//                             .config
//                             .local
//                             .prefix_disabled
//                             .iter()
//                             .any(|x| p.name.starts_with(x.as_str()))
//                     })
//                     .map(|p| (p.name.as_ref(), p.namespace.as_ref(), None)),
//             );
//
//         for (name, namespace, title) in candidates {
//             if !name.starts_with(bare_text) {
//                 continue;
//             }
//             if defined_namespaces.contains(namespace) {
//                 continue;
//             }
//             if suggested.contains(namespace) {
//                 continue;
//             }
//
//             let Some(extra_edits) =
//                 add_to_context(&source.0, &rope.0, ContextEntry::Prefix { name, namespace })
//             else {
//                 continue;
//             };
//
//             // Quote the new predicate text for JSON-LD (e.g. "foaf:").
//             let new_text = lang.quote(&format!("{}:$0", name));
//             let mut completion = SimpleCompletion::new(
//                 CompletionItemKind::MODULE,
//                 name.to_string(),
//                 TextEdit {
//                     range: word.range.clone(),
//                     new_text,
//                 },
//             )
//             .documentation(namespace);
//
//             if let Some(t) = title {
//                 completion = completion.label_description(t);
//             }
//
//             let completion = extra_edits
//                 .into_iter()
//                 .fold(completion, |c, e| c.text_edit(e));
//
//             req.push(completion);
//             suggested.insert(namespace.to_string());
//         }
//     }
// }

pub fn jsonld_lov_undefined_prefix_completion(
    mut query: Query<
        (
            &Source,
            &RopeC,
            &TokenComponent,
            &Prefixes,
            &mut CompletionRequest,
            &DynLang,
        ),
        With<JsonLdLang>,
    >,
    lovs: Query<&LocalPrefix>,
    prefix_cc: Query<&PrefixEntry>,
    config: Res<ServerConfig>,
) {
    for (source, rope, word, prefixes, mut req, lang) in &mut query {
        prefix_completion_helper(
            word,
            prefixes,
            &mut req.0,
            |name, location| {
                add_to_context(
                    &source.0,
                    &rope.0,
                    ContextEntry::Prefix {
                        name,
                        namespace: location,
                    },
                )
            },
            lovs.iter(),
            prefix_cc.iter(),
            &config.config.local,
            lang,
        );
    }
}
/// JSON-LD property/predicate completion based on declared namespace prefixes.
///
/// When the cursor is in predicate position inside a JSON-LD document this
/// system offers property names shortened via the `@context`-declared prefixes
/// (e.g. `foaf:name`). Unlike the generic [`complete_properties`] system the
/// completion text is wrapped in JSON string quotes because JSON-LD predicates
/// are always JSON string values.
#[instrument(skip(query, hierarchy, resource, config))]
pub fn jsonld_property_completion(
    mut query: Query<
        (
            &TokenComponent,
            &TripleComponent,
            &Prefixes,
            &Types,
            &mut CompletionRequest,
        ),
        With<JsonLdLang>,
    >,
    hierarchy: Res<TypeHierarchy<'static>>,
    resource: Res<Ontologies>,
    config: Res<ServerConfig>,
) {
    for (token, triple, prefixes, types, mut request) in &mut query {
        if triple.target != TripleTarget::Predicate {
            continue;
        }

        let bare_text = unquote(&token.text);

        let tts = types.get(&triple.triple.subject.value);

        let subclasses: HashSet<_> = tts
            .iter()
            .flat_map(|x| x.iter())
            .flat_map(|t| hierarchy.iter_subclass(*t))
            .collect();

        for property in resource.properties.values() {
            let correct_domain = property
                .domains
                .iter()
                .any(|domain| subclasses.contains(domain.as_str()));

            if !subclasses.is_empty()
                && config
                    .config
                    .local
                    .completion
                    .correct_domain_required(&property.term.value)
                && !correct_domain
            {
                continue;
            }

            let to_beat = prefixes
                .shorten(&property.term.value)
                .map(Cow::Owned)
                .unwrap_or(property.term.value.clone());

            if to_beat.starts_with(bare_text) {
                let quoted = format!("\"{}\"", to_beat);
                let mut completion = SimpleCompletion::new(
                    CompletionItemKind::ENUM_MEMBER,
                    to_beat.to_string(),
                    TextEdit {
                        range: token.range.clone(),
                        new_text: quoted,
                    },
                )
                .label_description(&property.full_title())
                .documentation(&property.full_docs(prefixes));

                if correct_domain {
                    completion.kind = CompletionItemKind::FIELD;
                    request.push(completion.sort_text(format!("0{}", to_beat)));
                } else {
                    request.push(completion.sort_text(format!("1{}", to_beat)));
                }
            }
        }
    }
}

/// Suggests the short names defined in the document's `@context` (e.g. `"name"`
/// mapped to `foaf:name`) when the cursor is in predicate position. These
/// aliases are more concise than prefixed names and are idiomatic in JSON-LD.
#[instrument(skip(query))]
pub fn jsonld_context_alias_completion(
    mut query: Query<
        (
            &TokenComponent,
            &TripleComponent,
            &JsonLdActiveContext,
            &Prefixes,
            &mut CompletionRequest,
        ),
        With<JsonLdLang>,
    >,
) {
    for (token, triple, active_ctx, prefixes, mut request) in &mut query {
        if triple.target != TripleTarget::Predicate {
            continue;
        }

        let bare_text = unquote(&token.text);

        for (term_name, term_def) in &active_ctx.0.terms {
            let Some(ref iri) = term_def.iri else {
                continue;
            };

            if term_name.starts_with(bare_text) {
                let quoted = format!("\"{}\"", term_name);
                // Show the resolved IRI (or shortened form) as the label description.
                let label_desc = prefixes.shorten(iri).unwrap_or_else(|| iri.clone());

                request.push(
                    SimpleCompletion::new(
                        CompletionItemKind::FIELD,
                        term_name.clone(),
                        TextEdit {
                            range: token.range.clone(),
                            new_text: quoted,
                        },
                    )
                    .label_description(label_desc)
                    .sort_text(format!("0{}", term_name)),
                );
            }
        }
    }
}

pub fn setup_completion(world: &mut World) {
    use swls_core::feature::completion::*;
    world.schedule_scope(CompletionLabel, |_, schedule| {
        schedule.add_systems((
            // jsonld_property_completion.after(generate_completions),
            // jsonld_context_alias_completion.after(generate_completions),
            jsonld_lov_undefined_prefix_completion.after(generate_completions),
        ));
    });
}

#[cfg(test)]
mod tests {
    use completion::CompletionRequest;
    use swls_core::{components::*, prelude::*};
    use swls_test_utils::{create_file, setup_world, TestClient};
    use test_log::test;

    /// Helper: run parse + completion schedule and return completions at cursor.
    fn get_completions(
        world: &mut bevy_ecs::world::World,
        entity: bevy_ecs::entity::Entity,
        line: u32,
        character: u32,
    ) -> Vec<String> {
        world.run_schedule(ParseLabel);
        world.entity_mut(entity).insert((
            CompletionRequest(vec![]),
            PositionComponent(swls_core::lsp_types::Position { line, character }),
        ));
        world.run_schedule(CompletionLabel);
        world
            .entity_mut(entity)
            .take::<CompletionRequest>()
            .map(|r| {
                r.0.into_iter()
                    .map(|c| c.edits[0].new_text.clone())
                    .collect()
            })
            .unwrap_or_default()
    }

    #[test]
    fn prefix_property_completion_works() {
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        // Valid JSON-LD: a complete triple whose key is a prefixed predicate.
        // The cursor is positioned inside the key "foaf:name" to trigger predicate completion.
        let src = "{\n  \"@context\": { \"foaf\": \"http://xmlns.com/foaf/0.1/\" },\n  \"@id\": \"http://example.com/me\",\n  \"foaf:name\": \"John\"\n}";
        let entity = create_file(&mut world, src, "http://example.com/ns#", "jsonld", Open);

        world.run_schedule(ParseLabel);

        // Verify that a TripleComponent is set with Predicate target when the
        // cursor is inside the "foaf:name" key (line 3, char 3 ≈ inside the key).
        world.entity_mut(entity).insert((
            CompletionRequest(vec![]),
            PositionComponent(swls_core::lsp_types::Position {
                line: 3,
                character: 3,
            }),
        ));
        world.run_schedule(CompletionLabel);

        let triple_comp = world.entity(entity).get::<TripleComponent>();
        assert!(
            triple_comp.is_some(),
            "Expected TripleComponent to be set for cursor inside a JSON-LD predicate key"
        );
        assert_eq!(
            triple_comp.unwrap().target,
            TripleTarget::Predicate,
            "Expected Predicate target for cursor inside a JSON-LD key"
        );
    }

    #[test]
    fn context_alias_completion_works() {
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        // Valid JSON-LD: a context-defined term alias "name" maps to foaf:name.
        // The cursor is positioned inside the "name" key to test alias completion.
        let src = "{\n  \"@context\": {\n    \"foaf\": \"http://xmlns.com/foaf/0.1/\",\n    \"name\": \"foaf:name\"\n  },\n  \"@id\": \"http://example.com/me\",\n  \"name\": \"John\"\n}";
        let entity = create_file(&mut world, src, "http://example.com/ns#", "jsonld", Open);

        // Cursor inside "name" on the last property line (line 6, char 3).
        let completions = get_completions(&mut world, entity, 6, 3);
        assert!(
            completions.iter().any(|c| c == "\"name\""),
            "Expected \"name\" alias in completions, got: {:?}",
            completions
        );
    }

    #[test]
    fn add_to_context_inserts_when_absent() {
        use ropey::Rope;

        use crate::ecs::completion::{add_to_context, ContextEntry};

        // Document without any @context.
        let src = "{\n  \"@id\": \"http://example.com/me\"\n}";
        let rope = Rope::from_str(src);

        let edits = add_to_context(
            src,
            &rope,
            ContextEntry::Prefix {
                name: "foaf",
                namespace: "http://xmlns.com/foaf/0.1/",
            },
        );
        assert!(edits.is_some(), "Expected edits when @context is absent");
        let edits = edits.unwrap();
        assert_eq!(edits.len(), 1);
        assert!(
            edits[0].new_text.contains("@context"),
            "Edit should insert @context, got: {:?}",
            edits[0].new_text
        );
        assert!(
            edits[0].new_text.contains("foaf"),
            "Edit should contain the prefix name"
        );
    }

    #[test]
    fn add_to_context_no_duplicate() {
        use ropey::Rope;

        use crate::ecs::completion::{add_to_context, ContextEntry};

        // Document where "foaf" is already declared.
        let src = "{\n  \"@context\": {\"foaf\": \"http://xmlns.com/foaf/0.1/\"},\n  \"@id\": \"http://example.com/me\"\n}";
        let rope = Rope::from_str(src);

        let edits = add_to_context(
            src,
            &rope,
            ContextEntry::Prefix {
                name: "foaf",
                namespace: "http://xmlns.com/foaf/0.1/",
            },
        );
        assert!(
            edits.is_none(),
            "Should return None when prefix already present"
        );
    }
}
