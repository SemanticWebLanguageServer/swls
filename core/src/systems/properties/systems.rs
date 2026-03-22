use std::{borrow::Cow, collections::HashSet};

use bevy_ecs::prelude::*;
use completion::{CompletionRequest, SimpleCompletion};
use hover::HoverRequest;
use tracing::{instrument, trace};

use crate::{
    lsp_types::{CompletionItemKind, TextEdit},
    prelude::*,
};

#[instrument(skip(query, resource))]
pub fn complete_class(
    mut query: Query<(
        &TokenComponent,
        &TripleComponent,
        &Prefixes,
        &Types,
        &mut CompletionRequest,
    )>,
    hierarchy: Res<TypeHierarchy<'static>>,
    resource: Res<Ontologies>,
) {
    for (token, triple, prefixes, types, mut request) in &mut query {
        if triple.triple.predicate.value == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
            && triple.target == TripleTarget::Object
        {
            let tts = types.get(&triple.triple.subject.value);

            let superclasses: HashSet<_> = tts
                .iter()
                .flat_map(|x| x.iter())
                .flat_map(|t| hierarchy.iter_superclass(*t))
                .collect();

            for class in resource.classes.values() {
                let to_beat = prefixes
                    .shorten(&class.term.value)
                    .map(|x| Cow::Owned(x))
                    .unwrap_or(class.term.value.clone());

                if to_beat.starts_with(&token.text) {
                    let mut completion = SimpleCompletion::new(
                        CompletionItemKind::CLASS,
                        format!("{}", to_beat),
                        TextEdit {
                            range: token.range.clone(),
                            new_text: to_beat.to_string(),
                        },
                    )
                    .label_description(class.full_title())
                    .documentation(class.full_docs(&hierarchy, &prefixes));

                    if superclasses.contains(class.term.as_str()) {
                        completion.kind = CompletionItemKind::INTERFACE;
                        request.push(completion.sort_text(format!("0{}", to_beat)));
                    } else {
                        request.push(completion.sort_text(format!("1{}", to_beat)));
                    }
                }
            }
        }
    }
}

pub fn hover_class(
    mut query: Query<(&TokenComponent, &Prefixes, &mut HoverRequest)>,
    hierarchy: Res<TypeHierarchy<'static>>,
    resource: Res<Ontologies>,
) {
    for (token, prefixes, mut request) in &mut query {
        if let Some(target) = prefixes.expand(token.token.value()) {
            for class in resource.classes.values() {
                if class.term.value == target {
                    request.0.push(format!(
                        "## {}\n\n{}",
                        class.full_title(),
                        class.full_docs(&hierarchy, &prefixes)
                    ));
                }
            }
        }
    }
}

#[instrument(skip(query, hierarchy, resource, config))]
pub fn complete_properties(
    mut query: Query<(
        &TokenComponent,
        &TripleComponent,
        &Prefixes,
        &Label,
        &Types,
        &mut CompletionRequest,
    )>,
    hierarchy: Res<TypeHierarchy<'static>>,
    resource: Res<Ontologies>,
    config: Res<ServerConfig>,
) {
    for (token, triple, prefixes, _this_label, types, mut request) in &mut query {
        if triple.target == TripleTarget::Predicate {
            let tts = types.get(&triple.triple.subject.value);

            if let Some(tts) = tts.as_ref() {
                trace!("Types for {}", triple.triple.subject.value);
                for t in *tts {
                    trace!(
                        "{} {}",
                        triple.triple.subject.value,
                        hierarchy.type_name(*t)
                    );
                }
            } else {
                trace!("No types for {}", triple.triple.subject.value);
            }

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
                    .map(|x| Cow::Owned(x))
                    .unwrap_or(property.term.value.clone());

                if to_beat.starts_with(&token.text) {
                    let mut completion = SimpleCompletion::new(
                        CompletionItemKind::ENUM_MEMBER,
                        format!("{}", to_beat),
                        TextEdit {
                            range: token.range.clone(),
                            new_text: to_beat.to_string(),
                        },
                    )
                    .label_description(&property.full_title())
                    .documentation(&property.full_docs(&prefixes));

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
}

#[instrument(skip(query, resource))]
pub fn hover_property(
    mut query: Query<(
        &TokenComponent,
        &Prefixes,
        &DocumentLinks,
        &mut HoverRequest,
    )>,
    resource: Res<Ontologies>,
) {
    for (token, prefixes, _links, mut request) in &mut query {
        if let Some(target) = prefixes.expand(token.token.value()) {
            for c in resource
                .properties
                .values()
                .filter(|c| c.term.value == target)
            {
                request
                    .0
                    .push(format!("## {}\n\n{}", c.full_title(), c.full_docs(&prefixes)));
            }
        }
    }
}
