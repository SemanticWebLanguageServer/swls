use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use crate::{
    lsp_types::{CompletionItemKind, Diagnostic, DiagnosticSeverity, TextDocumentItem, TextEdit},
    systems::PrefixEntry,
};
use bevy_ecs::prelude::*;
use lov::LocalPrefix;
use tower_lsp::lsp_types::DiagnosticTag;
use tracing::{debug, instrument};

use crate::prelude::*;

pub const PREFIX_CC: &'static str = include_str!("./prefix_cc.txt");

/// One defined prefix, maps prefix to url
#[derive(Debug, Clone)]
pub struct Prefix {
    pub prefix: String,
    pub url: crate::lsp_types::Url,
}

/// [`Component`] that containing defined prefixes and base URL.
///
/// [`lsp_core`](crate) uses [`Prefixes`] in different systems, for example
/// - to check for undefined prefixes diagnostics with
/// [`undefined_prefix`](crate::prelude::systems::undefined_prefix)
/// - derive linked documents [`DocumentLinks`] with
/// [`derive_prefix_links`](crate::prelude::systems::derive_prefix_links)
#[derive(Component, Debug)]
pub struct Prefixes(pub Vec<Prefix>, pub crate::lsp_types::Url);
impl Deref for Prefixes {
    type Target = Vec<Prefix>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl Prefixes {
    pub fn shorten(&self, value: &str) -> Option<String> {
        let try_shorten = |prefix: &Prefix| {
            let short = value.strip_prefix(prefix.url.as_str())?;
            Some(format!("{}:{}", prefix.prefix, short))
        };

        self.0.iter().flat_map(try_shorten).next()
    }

    pub fn expand(&self, token: &Token) -> Option<String> {
        match token {
            Token::PNameLN(pref, x) => {
                let pref = pref.as_ref().map(|x| x.as_str()).unwrap_or("");
                let prefix = self.0.iter().find(|x| &x.prefix == pref)?;
                Some(format!("{}{}", prefix.url, x))
            }
            Token::IRIRef(x) => {
                return self.1.join(&x).ok().map(|x| x.to_string());
            }
            _ => None,
        }
    }

    pub fn expand_json(&self, token: &Token) -> Option<String> {
        match token {
            Token::Str(pref, _) => {
                if let Some(x) = pref.find(':') {
                    let prefix = &pref[..x];
                    if let Some(exp) = self.0.iter().find(|x| &x.prefix == prefix) {
                        return Some(format!("{}{}", exp.url.as_str(), &pref[x + 1..]));
                    }
                } else {
                    if let Some(exp) = self.0.iter().find(|x| &x.prefix == pref) {
                        return Some(exp.url.as_str().to_string());
                    }
                }

                return Some(
                    self.1
                        .join(&pref)
                        .ok()
                        .map(|x| x.to_string())
                        .unwrap_or(pref.to_string()),
                );
            }
            _ => None,
        }
    }
}

pub fn prefix_completion_helper<'a>(
    word: &TokenComponent,
    prefixes: &Prefixes,
    completions: &mut Vec<SimpleCompletion>,
    mut extra_edits: impl FnMut(&str, &str) -> Option<Vec<TextEdit>>,
    lovs: impl Iterator<Item = &'a LocalPrefix>,
    prefix_cc: impl Iterator<Item = &'a PrefixEntry>,
    config: &LocalConfig,
    // known: &KnownPrefixes,
) {
    match word.token.value() {
        Token::Invalid(_) => {}
        _ => return,
    }

    let mut defined = HashSet::new();
    for p in prefixes.0.iter() {
        defined.insert(p.url.as_str());
    }

    let mut suggested = HashSet::new();
    completions.extend(
        lovs.filter(|lov| lov.name.starts_with(&word.text))
            .filter(|lov| !defined.contains(lov.namespace.as_ref()))
            .flat_map(|lov| {
                if suggested.contains(&lov.namespace) {
                    return None;
                }
                let new_text = format!("{}:", lov.name);
                // let sort_text = format!("{}", lov.rank);
                let filter_text = new_text.clone();
                if new_text != word.text {
                    let extra_edit = extra_edits(&lov.name, &lov.namespace)?;
                    let completion = SimpleCompletion::new(
                        CompletionItemKind::MODULE,
                        format!("{}", lov.name),
                        crate::lsp_types::TextEdit {
                            new_text,
                            range: word.range.clone(),
                        },
                    )
                    .label_description(lov.title.as_ref())
                    .documentation(lov.namespace.as_ref())
                    // .sort_text(sort_text)
                    .filter_text(filter_text);

                    let completion = extra_edit
                        .into_iter()
                        .fold(completion, |completion: SimpleCompletion, edit| {
                            completion.text_edit(edit)
                        });
                    suggested.insert(&lov.namespace);
                    Some(completion)
                } else {
                    None
                }
            }),
    );
    completions.extend(
        prefix_cc
            .filter(|pref| pref.name.starts_with(&word.text))
            .filter(|pref| !defined.contains(pref.namespace.as_ref()))
            .filter(|lov| {
                !config
                    .prefix_disabled
                    .iter()
                    .any(|x| lov.name.starts_with(x.as_str()))
            })
            .flat_map(|lov| {
                if suggested.contains(&lov.namespace) {
                    return None;
                }
                let new_text = format!("{}:", lov.name);
                // let sort_text = format!("{}", lov.rank);
                let filter_text = new_text.clone();
                if new_text != word.text {
                    let extra_edit = extra_edits(&lov.name, &lov.namespace)?;
                    let completion = SimpleCompletion::new(
                        CompletionItemKind::MODULE,
                        format!("{}", lov.name),
                        crate::lsp_types::TextEdit {
                            new_text,
                            range: word.range.clone(),
                        },
                    )
                    .documentation(lov.namespace.as_ref())
                    // .sort_text(sort_text)
                    .filter_text(filter_text);

                    let completion = extra_edit
                        .into_iter()
                        .fold(completion, |completion: SimpleCompletion, edit| {
                            completion.text_edit(edit)
                        });
                    suggested.insert(&lov.namespace);
                    Some(completion)
                } else {
                    None
                }
            }),
    );
}

pub fn undefined_prefix(
    query: Query<
        (&Tokens, &Prefixes, &Wrapped<TextDocumentItem>, &RopeC),
        (Or<(Changed<Prefixes>, Changed<Tokens>)>, With<Open>),
    >,
    mut client: ResMut<DiagnosticPublisher>,
) {
    for (tokens, prefixes, item, rope) in &query {
        let mut diagnostics: Vec<Diagnostic> = Vec::new();
        for t in &tokens.0 {
            match t.value() {
                Token::PNameLN(x, _) => {
                    let pref = x.as_ref().map(|x| x.as_str()).unwrap_or("");
                    let found = prefixes.0.iter().find(|x| x.prefix == pref).is_some();
                    if !found {
                        if let Some(range) = range_to_range(t.span(), &rope) {
                            diagnostics.push(Diagnostic {
                                range,
                                severity: Some(DiagnosticSeverity::ERROR),
                                source: Some(String::from("SWLS")),
                                message: format!("Undefined prefix {}", pref),
                                related_information: None,
                                ..Default::default()
                            })
                        }
                    }
                }
                _ => {}
            }
        }
        let _ = client.publish(&item.0, diagnostics, "undefined_prefix");
    }
}

/// Diagnostic system that warns about prefix declarations that are never used.
///
/// For example, `@prefix foaf: <http://xmlns.com/foaf/0.1/> .` without any `foaf:` usage
/// in the document produces a `Warning` diagnostic.
pub fn unused_prefix(
    query: Query<
        (&Tokens, &Prefixes, &Wrapped<TextDocumentItem>, &RopeC),
        (Or<(Changed<Prefixes>, Changed<Tokens>)>, With<Open>),
    >,
    mut client: ResMut<DiagnosticPublisher>,
) {
    for (tokens, prefixes, item, rope) in &query {
        let mut diagnostics: Vec<Diagnostic> = Vec::new();

        // Collect which prefixes are used (non-declaration PNameLN tokens)
        // and where each prefix is declared (the PNameLN span right after @prefix/PREFIX).
        let mut used_prefixes: HashSet<&str> = HashSet::new();
        let mut declaration_spans: HashMap<&str, std::ops::Range<usize>> = HashMap::new();

        for (i, t) in tokens.0.iter().enumerate() {
            if let Token::PNameLN(Some(pref), _) = t.value() {
                // A prefix name token that is immediately preceded (ignoring comments)
                // by a PrefixTag or SparqlPrefix is a declaration, not a use.
                let is_declaration = tokens.0[..i]
                    .iter()
                    .rev()
                    .find(|tok| !matches!(tok.value(), Token::Comment(_)))
                    .map(|tok| matches!(tok.value(), Token::PrefixTag | Token::SparqlPrefix))
                    .unwrap_or(false);

                if is_declaration {
                    declaration_spans.insert(pref.as_str(), t.span().clone());
                } else {
                    used_prefixes.insert(pref.as_str());
                }
            }
        }

        // Any declared prefix that was not used gets a warning
        for prefix in prefixes.0.iter() {
            if !used_prefixes.contains(prefix.prefix.as_str()) {
                if let Some(span) = declaration_spans.get(prefix.prefix.as_str()) {
                    if let Some(range) = range_to_range(span, rope) {
                        diagnostics.push(Diagnostic {
                            range,
                            tags: Some(vec![DiagnosticTag::UNNECESSARY]),
                            severity: Some(DiagnosticSeverity::INFORMATION),
                            source: Some(String::from("SWLS")),
                            message: format!(
                                "Prefix '{}' is declared but never used",
                                prefix.prefix
                            ),
                            related_information: None,
                            ..Default::default()
                        });
                    }
                }
            }
        }

        let _ = client.publish(&item.0, diagnostics, "unused_prefix");
    }
}

#[instrument(skip(query))]
pub fn defined_prefix_completion(
    mut query: Query<(&TokenComponent, &Prefixes, &mut CompletionRequest)>,
) {
    for (word, prefixes, mut req) in &mut query {
        let st = &word.text;
        let pref = if let Some(idx) = st.find(':') {
            &st[..idx]
        } else {
            &st
        };

        debug!("matching {}", pref);

        let completions = prefixes
            .0
            .iter()
            .filter(|p| p.prefix.as_str().starts_with(pref))
            .flat_map(|x| {
                let new_text = format!("{}:", x.prefix.as_str());
                if new_text != word.text {
                    Some(
                        SimpleCompletion::new(
                            CompletionItemKind::MODULE,
                            format!("{}", x.prefix.as_str()),
                            crate::lsp_types::TextEdit {
                                new_text,
                                range: word.range.clone(),
                            },
                        )
                        .documentation(x.url.as_str()),
                    )
                } else {
                    None
                }
            });

        req.0.extend(completions);
    }
}
