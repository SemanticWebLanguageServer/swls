use std::{collections::HashSet, ops::Deref};

use crate::lsp_types::Range;
use bevy_ecs::prelude::*;
use swls_lov::LocalPrefix;
use tracing::instrument;

use crate::{
    lsp_types::{CompletionItemKind, TextEdit},
    prelude::*,
    systems::PrefixEntry,
};

pub const PREFIX_CC: &str = include_str!("./prefix_cc.txt");

/// One defined prefix, maps prefix name to URL.
#[derive(Debug, Clone)]
pub struct Prefix {
    pub prefix: String,
    pub url: crate::lsp_types::Url,
}

/// [`Component`] containing defined prefixes and base URL.
#[derive(Component, Debug)]
pub struct Prefixes(pub Vec<Prefix>, pub crate::lsp_types::Url);

impl Deref for Prefixes {
    type Target = Vec<Prefix>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Prefixes {
    /// `value` as a prefixed name (`ex:name`) that is valid in Turtle, TriG,
    /// N3 and SPARQL, or `None` when no declared prefix gives one.
    ///
    /// The local part must be a plain `PN_LOCAL` (see [`is_local_name`]):
    /// `http://w3id.org/rml/shapes/X` is not shortened to `rml:shapes/X`.
    /// When several prefixes match, the longest namespace wins, so a
    /// declared `rmlsh: <http://w3id.org/rml/shapes/>` gives `rmlsh:X`.
    pub fn shorten(&self, value: &str) -> Option<String> {
        self.shorten_with(value, is_local_name)
    }

    /// `value` as a JSON-LD compact IRI (`prefix:suffix`), where the suffix
    /// may be anything, e.g. `css:dist/Server#port`. The longest matching
    /// namespace wins.
    pub fn shorten_compact(&self, value: &str) -> Option<String> {
        self.shorten_with(value, |_| true)
    }

    fn shorten_with(&self, value: &str, valid: impl Fn(&str) -> bool) -> Option<String> {
        self.0
            .iter()
            .filter_map(|prefix| {
                let local = value.strip_prefix(prefix.url.as_str())?;
                valid(local).then_some((prefix.url.as_str().len(), prefix, local))
            })
            // Longest namespace first; on a tie prefer a named prefix over the
            // empty one, then the alphabetically first, so the result does not
            // depend on declaration order.
            .max_by_key(|(len, prefix, _)| {
                (
                    *len,
                    !prefix.prefix.is_empty(),
                    std::cmp::Reverse(prefix.prefix.clone()),
                )
            })
            .map(|(_, prefix, local)| format!("{}:{}", prefix.prefix, local))
    }
}

fn is_pn_chars_base(c: char) -> bool {
    c.is_ascii_alphabetic() || (!c.is_ascii() && c.is_alphabetic())
}

fn is_pn_chars_u(c: char) -> bool {
    is_pn_chars_base(c) || c == '_'
}

fn is_pn_chars(c: char) -> bool {
    is_pn_chars_u(c)
        || c == '-'
        || c.is_ascii_digit()
        || c == '\u{B7}'
        || ('\u{300}'..='\u{36F}').contains(&c)
        || ('\u{203F}'..='\u{2040}').contains(&c)
}

/// Whether `local` can be written, unescaped, as the local part of a prefixed
/// name (`PN_LOCAL` in Turtle, TriG, N3 and SPARQL). The empty local name
/// (`ex:`) is valid; `%XX` escapes are accepted, backslash escapes are not
/// produced.
pub fn is_local_name(local: &str) -> bool {
    let chars: Vec<char> = local.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        let first = i == 0;
        if c == '%' {
            let hex = |j: usize| chars.get(j).is_some_and(|c| c.is_ascii_hexdigit());
            if !(hex(i + 1) && hex(i + 2)) {
                return false;
            }
            i += 3;
            continue;
        }
        let ok = if first {
            is_pn_chars_u(c) || c == ':' || c.is_ascii_digit()
        } else {
            is_pn_chars(c) || c == '.' || c == ':'
        };
        if !ok {
            return false;
        }
        i += 1;
    }
    !local.ends_with('.')
}

enum PrefixLike<'a> {
    Prefix(&'a Prefix),
    Local(&'a LocalPrefix),
    Entry(&'a PrefixEntry),
}
impl PrefixLike<'_> {
    fn as_completion(
        &self,
        lang: &DynLang,
        range: Range,
        extra: Option<Vec<TextEdit>>,
    ) -> SimpleCompletion {
        let (name, title, namespace) = match self {
            PrefixLike::Prefix(prefix) => (prefix.prefix.as_str(), None, prefix.url.as_str()),
            PrefixLike::Local(local_prefix) => (
                local_prefix.name.as_ref(),
                Some(local_prefix.title.as_ref()),
                local_prefix.namespace.as_ref(),
            ),
            PrefixLike::Entry(prefix_entry) => (
                prefix_entry.name.as_ref(),
                None,
                prefix_entry.namespace.as_ref(),
            ),
        };

        let nt = format!("{}:$0", name);
        let mut completion = SimpleCompletion::new(
            CompletionItemKind::MODULE,
            name.to_string(),
            crate::lsp_types::TextEdit {
                new_text: lang.quote(&nt),
                range,
            },
        )
        .documentation(namespace)
        // .filter_text(new_text)
        .as_snippet();

        if let Some(title) = title {
            completion = completion.label_description(title);
        }

        if let Some(extra) = extra {
            for e in extra {
                completion = completion.text_edit(e);
            }
        }

        completion
    }

    fn name(&self) -> &str {
        match self {
            PrefixLike::Prefix(prefix) => prefix.prefix.as_str(),
            PrefixLike::Local(local_prefix) => local_prefix.name.as_ref(),
            PrefixLike::Entry(prefix_entry) => prefix_entry.name.as_ref(),
        }
    }

    fn namespace(&self) -> &str {
        match self {
            PrefixLike::Prefix(prefix) => prefix.url.as_str(),
            PrefixLike::Local(local_prefix) => local_prefix.namespace.as_ref(),
            PrefixLike::Entry(prefix_entry) => prefix_entry.namespace.as_ref(),
        }
    }
}

/// How many undeclared prefixes (from the built-in vocabularies and
/// prefix.cc) are suggested at most.
pub const MAX_UNDECLARED_PREFIXES: usize = 30;

/// Prefix suggestions for the word at the cursor.
///
/// Prefixes the document declares are always suggested. Undeclared prefixes
/// (built-in vocabularies, then prefix.cc in popularity order) only once
/// something was typed, and at most [`MAX_UNDECLARED_PREFIXES`]: with an empty
/// word every one of the ~3,400 known prefixes would match, burying everything
/// else.
pub fn prefix_completion_helper<'a>(
    word: &TokenComponent,
    prefixes: &'a Prefixes,
    completions: &mut Vec<SimpleCompletion>,
    mut extra_edits: impl FnMut(&str, &str) -> Option<Vec<TextEdit>>,
    lovs: impl Iterator<Item = &'a LocalPrefix>,
    prefix_cc: impl Iterator<Item = &'a PrefixEntry>,
    lang: &DynLang,
) {
    let mut suggested = HashSet::new();
    let text = lang.unquote(&word.text);
    tracing::debug!("completion helper {:?}", word);

    let mut undeclared = 0;
    let candidates = prefixes
        .iter()
        .map(PrefixLike::Prefix)
        .chain(lovs.map(PrefixLike::Local))
        .chain(prefix_cc.map(PrefixLike::Entry));
    for prefix in candidates {
        let declared = matches!(prefix, PrefixLike::Prefix(_));
        if !declared && (text.is_empty() || undeclared >= MAX_UNDECLARED_PREFIXES) {
            continue;
        }
        if !prefix.name().starts_with(text)
            || !(prefix.namespace().ends_with('#') || prefix.namespace().ends_with('/'))
            || !suggested.insert(prefix.name().to_string())
        {
            continue;
        }
        if !declared {
            undeclared += 1;
        }
        let edits = extra_edits(prefix.name(), prefix.namespace());
        completions.push(prefix.as_completion(lang, word.range, edits));
    }
}

/// Generic prefix completion for the text RDF languages (Turtle / TriG / SPARQL).
///
/// Suggests prefixes from the local LOV data, the prefix.cc list, and the
/// prefixes already declared in the document.  When the chosen prefix is not yet
/// declared, the language's [`prefix_edits`](crate::lang::LangHelper::prefix_edits)
/// supplies an additional edit that inserts the declaration; when it is already
/// declared no such edit is added.
///
/// This is registered **once** (in the shared completion schedule) and dispatches
/// per-document through [`DynLang`], so it does not need a language marker filter.
/// Languages whose prefix model is incompatible (e.g. JSON-LD `@context`) opt out
/// via [`handles_prefix_completion`](crate::lang::LangHelper::handles_prefix_completion).
#[instrument(skip(query, lovs, prefix_cc, config))]
pub fn rdf_lov_undefined_prefix_completion(
    mut query: Query<(
        &TokenComponent,
        &Source,
        &RopeC,
        &Prefixes,
        &mut CompletionRequest,
        &DynLang,
    )>,
    lovs: Query<&LocalPrefix>,
    prefix_cc: Query<&PrefixEntry>,
    config: Res<ServerConfig>,
) {
    if config.config.local.is_disabled(Disabled::CompletionPrefix) {
        return;
    }
    let fmt = config.config.local.prefix_format.unwrap_or_default();
    for (word, source, rope, prefixes, mut req, lang) in &mut query {
        if lang.handles_prefix_completion() {
            continue;
        }
        prefix_completion_helper(
            word,
            prefixes,
            &mut req.0,
            |name, location| {
                if prefixes.iter().any(|p| p.prefix == name) {
                    None
                } else {
                    lang.prefix_edits(&source.0, &rope.0, name, location, fmt)
                }
            },
            lovs.iter(),
            prefix_cc.iter(),
            lang,
        );
    }
}

#[cfg(test)]
mod shorten_tests {
    use super::*;

    fn prefixes(decls: &[(&str, &str)]) -> Prefixes {
        Prefixes(
            decls
                .iter()
                .map(|(p, u)| Prefix {
                    prefix: p.to_string(),
                    url: crate::lsp_types::Url::parse(u).unwrap(),
                })
                .collect(),
            crate::lsp_types::Url::parse("file:///doc.ttl").unwrap(),
        )
    }

    #[test]
    fn local_names() {
        for ok in [
            "",
            "name",
            "Name_1",
            "a.b",
            "1abc",
            "a:b",
            "caf\u{e9}",
            "a%20b",
            "a-b",
        ] {
            assert!(is_local_name(ok), "{ok:?} should be valid");
        }
        for bad in [
            "a/b", "a#b", "a.", "-a", ".a", "a b", "a?b", "a%2", "a~b", "a,b",
        ] {
            assert!(!is_local_name(bad), "{bad:?} should be invalid");
        }
    }

    #[test]
    fn shorten_only_to_valid_prefixed_names() {
        let p = prefixes(&[("rml", "http://w3id.org/rml/")]);
        assert_eq!(
            p.shorten("http://w3id.org/rml/TriplesMap").as_deref(),
            Some("rml:TriplesMap")
        );
        assert_eq!(p.shorten("http://w3id.org/rml/shapes/X"), None);
        assert_eq!(
            p.shorten_compact("http://w3id.org/rml/shapes/X").as_deref(),
            Some("rml:shapes/X")
        );
        assert_eq!(p.shorten("http://other.test/x"), None);
    }

    #[test]
    fn ties_prefer_a_named_prefix_then_alphabetical() {
        let p = prefixes(&[
            ("", "http://w3id.org/rml/shapes/"),
            ("zz", "http://w3id.org/rml/shapes/"),
            ("rmlsh", "http://w3id.org/rml/shapes/"),
        ]);
        assert_eq!(
            p.shorten("http://w3id.org/rml/shapes/X").as_deref(),
            Some("rmlsh:X")
        );
    }

    #[test]
    fn longest_namespace_wins() {
        let p = prefixes(&[
            ("rml", "http://w3id.org/rml/"),
            ("rmlsh", "http://w3id.org/rml/shapes/"),
        ]);
        assert_eq!(
            p.shorten("http://w3id.org/rml/shapes/X").as_deref(),
            Some("rmlsh:X")
        );
        assert_eq!(
            p.shorten_compact("http://w3id.org/rml/shapes/X").as_deref(),
            Some("rmlsh:X")
        );
        assert_eq!(
            p.shorten("http://w3id.org/rml/source").as_deref(),
            Some("rml:source")
        );
    }
}
