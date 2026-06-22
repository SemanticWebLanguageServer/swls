use std::{collections::HashSet, ops::Deref};

use bevy_ecs::prelude::*;
use swls_lov::LocalPrefix;
use crate::lsp_types::Range;
use tracing::instrument;

use crate::{
    lsp_types::{CompletionItemKind, TextEdit},
    prelude::*,
    systems::PrefixEntry,
};

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
            format!("{}", name),
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

pub fn prefix_completion_helper<'a>(
    word: &TokenComponent,
    prefixes: &Prefixes,
    completions: &mut Vec<SimpleCompletion>,
    mut extra_edits: impl FnMut(&str, &str) -> Option<Vec<TextEdit>>,
    lovs: impl Iterator<Item = &'a LocalPrefix>,
    prefix_cc: impl Iterator<Item = &'a PrefixEntry>,
    lang: &DynLang,
) {
    let mut suggested = HashSet::new();
    let text = lang.unquote(&word.text);
    tracing::debug!("completion helper {:?}", word);

    completions.extend(
        lovs.map(|x| PrefixLike::Local(&x))
            .chain(prefix_cc.map(|x| PrefixLike::Entry(x)))
            .chain(prefixes.iter().map(PrefixLike::Prefix))
            .filter(|p| p.name().starts_with(text))
            .filter(|p| p.namespace().ends_with('#') || p.namespace().ends_with('/'))
            .flat_map(|lov| {
                if suggested.contains(lov.name()) {
                    return None;
                }

                let completion = lov.as_completion(
                    lang,
                    word.range.clone(),
                    extra_edits(&lov.name(), &lov.namespace()),
                );

                suggested.insert(lov.name().to_string());
                Some(completion)
            }),
    );

    // completions.extend(
    //     lovs.filter(|lov| lov.name.starts_with(text))
    //         .flat_map(|lov| {
    //             if suggested.contains(&lov.namespace) {
    //                 tracing::info!("suggested contains it {:?}", lov);
    //                 return None;
    //             }
    //
    //             let completion = PrefixLike::Local(lov).as_completion(
    //                 lang,
    //                 word.range.clone(),
    //                 extra_edits(&lov.name, &lov.namespace),
    //             );
    //
    //             suggested.insert(&lov.namespace);
    //             Some(completion)
    //         }),
    // );
    //
    // completions.extend(
    //     prefix_cc
    //         .filter(|pref| pref.name.starts_with(text))
    //         // .filter(|pref| !defined.contains(pref.namespace.as_ref()))
    //         // .filter(|lov| {
    //         //     !config
    //         //         .prefix_disabled
    //         //         .iter()
    //         //         .any(|x| lov.name.starts_with(x.as_str()))
    //         // })
    //         .flat_map(|lov| {
    //             if suggested.contains(&lov.namespace) {
    //                 return None;
    //             }
    //             let mut new_text = format!("{}:", lov.name);
    //             let filter_text = new_text.clone();
    //             if new_text != text {
    //                 new_text += "$0";
    //                 let extra_edit = extra_edits(&lov.name, &lov.namespace);
    //                 let completion = SimpleCompletion::new(
    //                     CompletionItemKind::MODULE,
    //                     format!("{}", lov.name),
    //                     crate::lsp_types::TextEdit {
    //                         new_text: lang.quote(&new_text),
    //                         range: word.range.clone(),
    //                     },
    //                 )
    //                 .documentation(lov.namespace.as_ref())
    //                 .filter_text(filter_text)
    //                 .as_snippet();
    //
    //                 let completion = extra_edit
    //                     .into_iter()
    //                     .flatten()
    //                     .fold(completion, |completion: SimpleCompletion, edit| {
    //                         completion.text_edit(edit)
    //                     });
    //
    //                 suggested.insert(&lov.namespace);
    //                 Some(completion)
    //             } else {
    //                 tracing::info!("new_text is word.text");
    //                 None
    //             }
    //         }),
    // );
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
