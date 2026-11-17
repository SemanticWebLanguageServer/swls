use std::collections::HashSet;

use bevy_ecs::prelude::*;
use completion::{CompletionRequest, SimpleCompletion};
use hover::HoverRequest;
use sophia_api::term::{Term as _, TermKind};
use tracing::{instrument, trace};

use crate::{
    feature::completion::CursorContext,
    lsp_types::{CompletionItemKind, TextEdit},
    prelude::*,
};

#[instrument(skip(query, resource, config))]
pub fn complete_class(
    mut query: Query<(
        &CursorContext,
        &TripleComponent,
        &Prefixes,
        &Types,
        &mut CompletionRequest,
        &DynLang,
    )>,
    hierarchy: Res<TypeHierarchy>,
    resource: Res<Ontologies>,
    config: Res<ServerConfig>,
) {
    if config.config.local.is_disabled(Disabled::CompletionClass) {
        return;
    }
    for (cursor, triple, prefixes, types, mut request, lang) in &mut query {
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
                let to_beat = lang.write_iri(prefixes, &class.term.value);

                if to_beat.starts_with(&cursor.typed) {
                    let mut completion = SimpleCompletion::new(
                        CompletionItemKind::CLASS,
                        to_beat.to_string(),
                        TextEdit {
                            range: cursor.range,
                            new_text: cursor.spaced(&to_beat),
                        },
                    )
                    .label_description(class.full_title())
                    .documentation(class.full_docs(&hierarchy, prefixes));

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
    mut query: Query<(&TripleComponent, &Prefixes, &mut HoverRequest)>,
    hierarchy: Res<TypeHierarchy>,
    resource: Res<Ontologies>,
    config: Res<ServerConfig>,
) {
    if config.config.local.is_disabled(Disabled::HoverClass) {
        return;
    }
    for (triple, prefixes, mut request) in &mut query {
        let Some(term) = triple.term() else { continue };
        if term.kind() != TermKind::Iri {
            continue;
        }
        let target = term.as_str();
        for class in resource.classes.values() {
            if class.term.value == target {
                request.0.push(format!(
                    "## {}\n\n{}",
                    class.full_title(),
                    class.full_docs(&hierarchy, prefixes)
                ));
            }
        }
    }
}

#[instrument(skip(query, hierarchy, resource, config))]
pub fn complete_properties(
    mut query: Query<(
        &CursorContext,
        &Prefixes,
        &Label,
        &Types,
        &mut CompletionRequest,
        &DynLang,
    )>,
    hierarchy: Res<TypeHierarchy>,
    resource: Res<Ontologies>,
    config: Res<ServerConfig>,
) {
    if config
        .config
        .local
        .is_disabled(Disabled::CompletionProperty)
    {
        return;
    }
    for (cursor, prefixes, _this_label, types, mut request, lang) in &mut query {
        let Some(subject) = cursor.subject.as_ref().filter(|_| cursor.at_predicate()) else {
            continue;
        };
        {
            let tts = types.get(&subject.value);
            let bare_text = lang.unquote(&cursor.typed);

            if let Some(tts) = tts.as_ref() {
                trace!("Types for {}", subject.value);
                for t in *tts {
                    trace!("{} {}", subject.value, hierarchy.type_name(*t));
                }
            } else {
                trace!("No types for {}", subject.value);
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

                let to_beat = lang.write_iri(prefixes, &property.term.value);

                if to_beat.starts_with(bare_text) {
                    let mut completion = SimpleCompletion::new(
                        CompletionItemKind::ENUM_MEMBER,
                        to_beat.to_string(),
                        TextEdit {
                            range: cursor.range,
                            new_text: cursor.spaced(&lang.quote(&to_beat)),
                        },
                    )
                    .label_description(property.full_title())
                    .documentation(property.full_docs(prefixes));

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

#[instrument(skip(query, resource, config))]
pub fn hover_property(
    mut query: Query<(
        &TripleComponent,
        &Prefixes,
        &DocumentLinks,
        &mut HoverRequest,
    )>,
    resource: Res<Ontologies>,
    config: Res<ServerConfig>,
) {
    if config.config.local.is_disabled(Disabled::HoverProperty) {
        return;
    }
    for (triple, prefixes, _links, mut request) in &mut query {
        let Some(term) = triple.term() else { continue };
        if term.kind() != TermKind::Iri {
            continue;
        }
        let target = term.as_str();
        for c in resource
            .properties
            .values()
            .filter(|c| c.term.value == target)
        {
            request.0.push(format!(
                "## {}\n\n{}",
                c.full_title(),
                c.full_docs(prefixes)
            ));
        }
    }
}

/// Hover system: when the cursor sits on an IRI that the user has explicitly
/// allow-listed via `allowed_properties`, explain that the property is *not*
/// part of the ontology and is only accepted because the user excluded it from
/// validation — including how to undo that.
pub fn hover_excluded_property(
    mut query: Query<(&TripleComponent, &mut HoverRequest)>,
    config: Res<ServerConfig>,
) {
    let allowed = &config.config.local.allowed_properties;
    if allowed.is_empty()
        || config
            .config
            .local
            .is_disabled(Disabled::HoverExcludedProperty)
    {
        return;
    }
    for (triple, mut request) in &mut query {
        let Some(term) = triple.term() else { continue };
        if term.kind() != TermKind::Iri {
            continue;
        }
        let target = term.as_str();
        if !allowed.contains(target) {
            continue;
        }
        request.0.push(format!(
            "⚠️ **`{}`** is not defined in its ontology.\n\n\
             It is accepted because you added it to the `allowed_properties` list. \
             To restore the \"unknown property\" warning, remove `\"{}\"` from the \
             `allowed_properties` array in your SWLS config (`config.json`).",
            target, target
        ));
    }
}
