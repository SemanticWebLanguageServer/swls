use std::{collections::HashSet, ops::Deref};

use crate::{
    lsp_types::{CompletionItemKind, TextEdit},
    systems::PrefixEntry,
};
use bevy_ecs::prelude::*;
use swls_lov::LocalPrefix;
use tracing::{debug, instrument};

use crate::prelude::*;

pub const PREFIX_CC: &'static str = include_str!("./prefix_cc.txt");

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
    pub fn shorten(&self, value: &str) -> Option<String> {
        let try_shorten = |prefix: &Prefix| {
            let short = value.strip_prefix(prefix.url.as_str())?;
            Some(format!("{}:{}", prefix.prefix, short))
        };
        self.0.iter().flat_map(try_shorten).next()
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
) {
    // Only suggest when the typed text could be a prefix name (remove gate for Token::Invalid).
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
