use std::collections::{HashMap, HashSet};

use bevy_ecs::prelude::*;
use sophia_api::{quad::Quad as _, term::TermKind};
use swls_lov::LocalPrefix;
use tracing::instrument;

use crate::{
    feature::{code_action::CodeActionRequest, diagnostics::DiagnosticPublisher},
    lsp_types::{
        CodeAction, CodeActionKind, Diagnostic, DiagnosticSeverity, Position, Range, TextEdit,
        WorkspaceEdit,
    },
    prelude::*,
    systems::PrefixEntry,
    util::offset_to_position,
};

// ─── Token-level helper ───────────────────────────────────────────────────────

/// Given the raw source text of a single term token, return the prefix name if the
/// term is written as a prefixed name (`prefix:local`).
///
/// Returns `None` for full IRIs (`<...>` or containing `://`), blank nodes (`_:`),
/// and variables (`?`).
///
/// The default prefix (`:local`, empty name before the `:`) returns `Some("")`: it
/// is a real declared prefix (`@prefix : <...>`) whose usage must be tracked so it
/// is not wrongly reported "declared but never used".
pub fn extract_prefix_from_token(raw: &str) -> Option<&str> {
    let text = raw.trim_matches('"'); // JSON-LD wraps IRIs in quotes
    if text.starts_with('<') || text.starts_with("_:") || text.starts_with('?') {
        return None;
    }
    if text.contains("://") {
        return None;
    }
    let colon = text.find(':')?;
    Some(&text[..colon])
}

// ─── Core helper (mirrors prefix_completion_helper) ───────────────────────────

/// Analyse a document's triples against its declared prefixes and return
/// (diagnostics, code_actions).
///
/// The caller supplies `make_fix_edit(prefix_name, suggested_url) -> Vec<TextEdit>` —
/// identical in spirit to the `extra_edits` callback in `prefix_completion_helper`.
/// This callback is responsible for generating the language-specific text edit that
/// adds a prefix declaration (typically via [`LangHelper::prefix_edits`]).  It may
/// return an empty Vec if the language does not support auto-insertion.
///
/// LOV / prefix.cc iterators are used to suggest a URL for undefined prefixes.
/// Because there are usually only a handful of undefined prefixes, we collect the
/// undefined set first and then make a single pass over the LOV / prefix.cc data
/// to resolve just those — rather than materialising the entire prefix universe
/// into a lookup map.
pub fn prefix_diagnostic_helper<'a>(
    triples: &Triples,
    prefixes: &Prefixes,
    rope: &RopeC,
    label: &Label,
    lovs: impl Iterator<Item = &'a LocalPrefix>,
    prefix_cc: impl Iterator<Item = &'a PrefixEntry>,
    mut make_fix_edit: impl FnMut(&str, &str) -> Vec<TextEdit>,
    report_undefined: bool,
    report_unused: bool,
) -> (Vec<Diagnostic>, Vec<CodeAction>) {
    let declared: HashSet<&str> = prefixes.iter().map(|p| p.prefix.as_str()).collect();

    // Walk all IRI terms to record used prefix names and every undefined-prefix
    // occurrence.
    let mut used: HashSet<String> = HashSet::new();
    // One entry per undefined-prefix *occurrence* (→ one diagnostic each), in
    // document order. Deduped by (name, byte span): a subject that heads a
    // predicate-object list (`:a :b :c; :d :e .`) is shared across several quads
    // with the *same* span and must only be flagged once.
    let mut undefined_occurrences: Vec<(String, Range)> = Vec::new();
    // Distinct undefined prefix names (→ one quick-fix each, + URL lookup).
    let mut undefined_names: HashSet<String> = HashSet::new();
    let mut seen_spans: HashSet<(String, usize, usize)> = HashSet::new();
    // Resolved IRI values of all IRI terms — used to detect JSON-LD term-alias
    // usage (an alias is used as a bare term, not as `prefix:local`).
    let mut iri_values: HashSet<String> = HashSet::new();

    for quad in triples.0.iter() {
        for term in [quad.s(), quad.p(), quad.o()] {
            if term.ty != Some(TermKind::Iri) {
                continue;
            }
            iri_values.insert(term.value.to_string());
            let span = &term.span;
            if span.is_empty() {
                continue;
            }
            // `span` holds *byte* offsets, so slice by bytes. Slicing by chars
            // here would read from the wrong place after any multi-byte char
            // earlier in the line (e.g. an en-dash in a literal), mis-reading a
            // prefixed name such as `rdfs:domain` as `fs:domain` and reporting a
            // phantom undefined prefix.
            let raw = match rope.0.byte_slice(span.start..span.end) {
                Some(s) => s.to_string(),
                None => continue,
            };
            let Some(prefix_name) = extract_prefix_from_token(&raw) else {
                continue;
            };
            used.insert(prefix_name.to_string());

            if declared.contains(prefix_name) {
                continue;
            }
            // Record each distinct (prefix, span) occurrence once.
            if seen_spans.insert((prefix_name.to_string(), span.start, span.end)) {
                if let (Some(start), Some(end)) = (
                    offset_to_position(span.start, &rope.0),
                    offset_to_position(span.end, &rope.0),
                ) {
                    undefined_occurrences.push((prefix_name.to_string(), Range::new(start, end)));
                    undefined_names.insert(prefix_name.to_string());
                }
            }
        }
    }

    // Resolve suggested URLs for only the undefined prefixes in a single pass.
    let mut url_lookup: HashMap<&str, String> = HashMap::new();
    if !undefined_names.is_empty() {
        for lp in lovs {
            if undefined_names.contains(lp.name.as_ref()) {
                url_lookup
                    .entry(lp.name.as_ref())
                    .or_insert_with(|| lp.namespace.to_string());
            }
        }
        for pe in prefix_cc {
            if undefined_names.contains(pe.name.as_ref()) {
                url_lookup
                    .entry(pe.name.as_ref())
                    .or_insert_with(|| pe.namespace.to_string());
            }
        }
    }

    let mut diagnostics: Vec<Diagnostic> = Vec::new();
    let mut code_actions: Vec<CodeAction> = Vec::new();

    // ── ERROR: used but not declared ─────────────────────────────────────────
    if report_undefined {
        // One diagnostic per occurrence, so every offending token is underlined.
        // Keep them grouped by prefix so the per-prefix quick-fix can reference
        // every occurrence it resolves.
        let mut diags_by_prefix: HashMap<&str, Vec<Diagnostic>> = HashMap::new();
        for (prefix_name, range) in &undefined_occurrences {
            let diag = Diagnostic {
                range: range.clone(),
                severity: Some(DiagnosticSeverity::ERROR),
                message: format!("Undefined prefix \"{}\"", prefix_name),
                ..Default::default()
            };
            diagnostics.push(diag.clone());
            diags_by_prefix
                .entry(prefix_name.as_str())
                .or_default()
                .push(diag);
        }

        // One quick-fix per distinct prefix (in document order): the fix inserts
        // a single declaration at the top of the file, resolving *every*
        // occurrence at once — so per-occurrence quick-fixes would be duplicates.
        // The action links back to all of the prefix's diagnostics (via their
        // ranges) so editors associate the fix with each underlined occurrence.
        let mut emitted: HashSet<&str> = HashSet::new();
        for (prefix_name, _) in &undefined_occurrences {
            if !emitted.insert(prefix_name.as_str()) {
                continue;
            }
            let suggested_url = url_lookup
                .get(prefix_name.as_str())
                .map(|s| s.as_str())
                .unwrap_or("");

            let fix_edits = make_fix_edit(prefix_name, suggested_url);
            if fix_edits.is_empty() {
                continue;
            }

            let mut changes = std::collections::HashMap::new();
            changes.insert(label.0.clone(), fix_edits);
            code_actions.push(CodeAction {
                title: format!("Add prefix declaration for \"{}\"", prefix_name),
                kind: Some(CodeActionKind::QUICKFIX),
                diagnostics: diags_by_prefix.remove(prefix_name.as_str()),
                edit: Some(WorkspaceEdit {
                    changes: Some(changes),
                    ..Default::default()
                }),
                ..Default::default()
            });
        }
    }

    // ── WARNING: declared but not used ────────────────────────────────────────
    for prefix in report_unused.then(|| prefixes.iter()).into_iter().flatten() {
        if used.contains(prefix.prefix.as_str()) {
            continue;
        }

        // JSON-LD term aliases (e.g. `"name": "foaf:name"`) are not used as
        // `prefix:local`, but as a bare term whose resolved IRI is the alias
        // target. Such an entry's namespace does not end in `/` or `#`; treat it
        // as used if its target IRI appears among the document's IRI terms.
        let target = prefix.url.as_str();
        let is_alias = !target.ends_with('/') && !target.ends_with('#');
        if is_alias && iri_values.contains(target) {
            continue;
        }

        {
            let (start, end) = find_prefix_declaration_range(&rope.0, &prefix.prefix);
            diagnostics.push(Diagnostic {
                range: Range::new(start, end),
                severity: Some(DiagnosticSeverity::WARNING),
                message: format!("Prefix \"{}\" is declared but never used", prefix.prefix),
                ..Default::default()
            });
        }
    }

    (diagnostics, code_actions)
}

/// Scan the rope for the declaration line of `prefix_name`.
///
/// Handles three syntaxes:
/// - Turtle/TriG: `@prefix foaf: <…>.`
/// - SPARQL:       `PREFIX foaf: <…>`
/// - JSON-LD:      `"foaf": "http://…"` (inside `@context`)
fn find_prefix_declaration_range(rope: &LineIndex, prefix_name: &str) -> (Position, Position) {
    // Patterns that identify a declaration for this prefix, paired with the byte
    // offset (within the match) at which the highlighted key/name begins.
    let turtle_needle = format!("@prefix {}:", prefix_name);
    let sparql_needle = format!("PREFIX {}:", prefix_name);
    // JSON-LD: `"foaf":` — a context key. May appear mid-line (even on a
    // single-line document), so we search anywhere on the line, not just at the
    // start.
    let jsonld_needle = format!("\"{}\":", prefix_name);

    // (needle, offset of key start within needle, key length)
    let patterns = [
        (&turtle_needle, "@prefix ".len(), prefix_name.len()),
        (&sparql_needle, "PREFIX ".len(), prefix_name.len()),
        // Highlight the quoted key, including the surrounding quotes.
        (&jsonld_needle, 0, prefix_name.len() + 2),
    ];

    let candidate = (0..rope.len_lines()).find_map(|line_idx| {
        let line = rope.line_str(line_idx)?;
        let line_start = rope.line_start(line_idx)?;

        for (needle, key_off, key_len) in patterns.iter() {
            if let Some(idx) = line.find(needle.as_str()) {
                // Byte offsets within the whole document; `offset_to_position`
                // converts them (and the encoding) for us.
                let key_byte = line_start + idx + key_off;
                let start = offset_to_position(key_byte, rope)?;
                let end = offset_to_position(key_byte + key_len, rope)?;
                return Some((start, end));
            }
        }
        None
    });

    candidate.unwrap_or((Position::new(0, 0), Position::new(0, 0)))
}

// ─── ECS systems ─────────────────────────────────────────────────────────────

/// ECS system: runs `prefix_diagnostic_helper` for every open document whose
/// triples or declared prefixes changed.
///
/// Skips languages that opt out via [`LangHelper::supports_prefix_diagnostics`]
/// (e.g. JSON-LD, which pre-expands all terms before storing them as triples).
#[instrument(skip(query, client, lovs, prefix_cc, config))]
pub fn prefix_diagnostics(
    query: Query<
        (
            &Triples,
            &Prefixes,
            &Source,
            &RopeC,
            &Label,
            &Wrapped<crate::lsp_types::TextDocumentItem>,
            &DynLang,
        ),
        (Or<(Changed<Triples>, Changed<Prefixes>)>, With<Open>),
    >,
    mut client: ResMut<DiagnosticPublisher>,
    lovs: Query<&LocalPrefix>,
    prefix_cc: Query<&PrefixEntry>,
    config: Res<ServerConfig>,
) {
    let fmt = config.config.local.prefix_format.unwrap_or_default();
    let report_undefined = !config.config.local.is_disabled(Disabled::UndefinedPrefix);
    let report_unused = !config.config.local.is_disabled(Disabled::UnusedPrefix);
    for (triples, prefixes, source, rope, label, params, lang) in &query {
        if !lang.0.supports_prefix_diagnostics() {
            // Clear any stale prefix diagnostics for this language and skip.
            let _ = client.publish(&params.0, vec![], "prefix");
            continue;
        }

        let (diagnostics, _) = prefix_diagnostic_helper(
            triples,
            prefixes,
            rope,
            label,
            lovs.iter(),
            prefix_cc.iter(),
            |name, url| {
                lang.0
                    .prefix_edits(&source.0, &rope.0, name, url, fmt)
                    .unwrap_or_default()
            },
            report_undefined,
            report_unused,
        );

        let _ = client.publish(&params.0, diagnostics, "prefix");
    }
}

/// ECS system: runs `prefix_diagnostic_helper` for every open document to populate
/// `CodeActionRequest` with "Add prefix declaration" quickfixes.
///
/// Skips languages that opt out via [`LangHelper::supports_prefix_diagnostics`].
#[instrument(skip(query, lovs, prefix_cc, config))]
pub fn add_missing_prefix_code_action(
    mut query: Query<
        (
            &Triples,
            &Prefixes,
            &Source,
            &RopeC,
            &Label,
            &DynLang,
            &mut CodeActionRequest,
        ),
        With<Open>,
    >,
    lovs: Query<&LocalPrefix>,
    prefix_cc: Query<&PrefixEntry>,
    config: Res<ServerConfig>,
) {
    let fmt = config.config.local.prefix_format.unwrap_or_default();
    if config.config.local.is_disabled(Disabled::UndefinedPrefix) {
        return;
    }
    for (triples, prefixes, source, rope, label, lang, mut req) in &mut query {
        if !lang.0.supports_prefix_diagnostics() {
            continue;
        }

        let (_, actions) = prefix_diagnostic_helper(
            triples,
            prefixes,
            rope,
            label,
            lovs.iter(),
            prefix_cc.iter(),
            |name, url| {
                lang.0
                    .prefix_edits(&source.0, &rope.0, name, url, fmt)
                    .unwrap_or_default()
            },
            true,
            false,
        );

        req.0.extend(actions);
    }
}
