use std::{borrow::Cow, collections::HashSet};

use bevy_ecs::prelude::*;
use completion::{CompletionRequest, SimpleCompletion};
use hover::HoverRequest;
use lsp_types::{CompletionItemKind, TextEdit};
use sophia_api::{
    ns::rdfs,
    prelude::{Any, Dataset},
    quad::Quad,
    term::Term,
};
use systems::OntologyExtractor;
use tracing::{debug, info, instrument};

use crate::{
    prelude::*,
    util::{ns::*, triple::MyTerm},
};

#[derive(PartialEq, Eq, Hash)]
pub struct DefinedClass {
    pub term: MyTerm<'static>,
    pub label: String,
    pub comment: String,
    pub reason: &'static str,
    pub location: std::ops::Range<usize>,
}

pub type DefinedClasses = HashSet<DefinedClass>;

fn derive_class(
    subject: <MyTerm<'_> as Term>::BorrowTerm<'_>,
    triples: &Triples,
    source: &'static str,
) -> Option<DefinedClass> {
    let label = triples
        .object([subject], [rdfs::label, dc::title])?
        .to_owned()
        .as_str()
        .to_string();
    let comment = triples
        .object([subject], [rdfs::comment, dc::description])?
        .to_owned()
        .as_str()
        .to_string();
    Some(DefinedClass {
        label,
        comment,
        term: subject.to_owned(),
        reason: source,
        location: subject.span.clone(),
    })
}

pub fn derive_classes(
    query: Query<(Entity, &Triples, &Label), (Changed<Triples>, Without<Dirty>)>,
    mut commands: Commands,
    extractor: Res<OntologyExtractor>,
) {
    for (e, triples, label) in &query {
        let classes: HashSet<_> = triples
            .0
            .quads_matching(Any, [rdf::type_], extractor.classes(), Any)
            .flatten()
            .flat_map(|x| derive_class(x.s(), &triples, "owl_class"))
            .collect();

        info!(
            "({} classes) Found {} classes for {} ({} triples)",
            extractor.classes().len(),
            classes.len(),
            label.0,
            triples.0.len()
        );
        commands.entity(e).insert(Wrapped(classes));
    }
}

#[instrument(skip(query, other))]
pub fn complete_class(
    mut query: Query<(
        &TokenComponent,
        &TripleComponent,
        &Prefixes,
        &DocumentLinks,
        &Label,
        &mut CompletionRequest,
    )>,
    other: Query<(&Label, &Wrapped<DefinedClasses>)>,
) {
    for (token, triple, prefixes, links, this_label, mut request) in &mut query {
        if triple.triple.predicate.value == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
            && triple.target == TripleTarget::Object
        {
            for (label, classes) in &other {
                // Check if this thing is actually linked
                if links
                    .iter()
                    .find(|link| link.0.as_str().starts_with(label.0.as_str()))
                    .is_none()
                    && label.0 != this_label.0
                {
                    debug!(
                        "Not looking for defined classes in {} (not linked)",
                        label.0
                    );
                    continue;
                }

                let st = classes.0.iter().take(5).fold(String::new(), |mut st, b| {
                    if let Some(short) = prefixes.shorten(&b.term.value) {
                        st += &short;
                    } else {
                        st += &b.term.value;
                    }
                    st += ", ";
                    st
                });
                debug!(
                    "Looking for defined classes in {} (found {} {})",
                    label.0,
                    classes.0.len(),
                    st
                );

                for class in classes.0.iter() {
                    let to_beat = prefixes
                        .shorten(&class.term.value)
                        .map(|x| Cow::Owned(x))
                        .unwrap_or(class.term.value.clone());

                    if to_beat.starts_with(&token.text) {
                        request.push(
                            SimpleCompletion::new(
                                CompletionItemKind::CLASS,
                                format!("{}", to_beat),
                                TextEdit {
                                    range: token.range.clone(),
                                    new_text: to_beat.to_string(),
                                },
                            )
                            .documentation(&class.comment),
                        );
                    }
                }
            }
        }
    }
}

pub fn hover_class(
    mut query: Query<(
        &TokenComponent,
        &Prefixes,
        &DocumentLinks,
        &mut HoverRequest,
    )>,
    other: Query<(&Label, &Wrapped<DefinedClasses>)>,
) {
    for (token, prefixes, links, mut request) in &mut query {
        if let Some(target) = prefixes.expand(token.token.value()) {
            for (label, classes) in &other {
                // Check if this thing is actually linked
                if links.iter().find(|link| link.0 == label.0).is_none() {
                    continue;
                }

                for c in classes.iter().filter(|c| c.term.value == target) {
                    request.0.push(format!("{}: {}", c.label, c.comment));
                }
            }
        }
    }
}

#[derive(PartialEq, Eq, Hash)]
pub struct DefinedProperty {
    pub predicate: MyTerm<'static>,
    pub comment: String,
    pub label: String,
    pub range: Vec<String>,
    pub domain: Vec<String>,
    pub reason: &'static str,
}
pub type DefinedProperties = HashSet<DefinedProperty>;

fn derive_property(
    subject: <MyTerm<'_> as Term>::BorrowTerm<'_>,
    triples: &Triples,
    source: &'static str,
) -> Option<DefinedProperty> {
    let label = triples
        .object([subject], [rdfs::label, dc::title])?
        .to_owned()
        .as_str()
        .to_string();
    let comment = triples
        .object([subject], [rdfs::comment, dc::description])?
        .to_owned()
        .as_str()
        .to_string();
    let domain: Vec<_> = triples
        .objects([subject], [rdfs::domain])
        .map(|x| x.as_str().to_string())
        .collect();

    let range: Vec<_> = triples
        .objects([subject], [rdfs::range])
        .map(|x| x.as_str().to_string())
        .collect();

    Some(DefinedProperty {
        predicate: subject.to_owned(),
        range,
        domain,
        label,
        comment,
        reason: source,
    })
}

pub fn derive_properties(
    query: Query<(Entity, &Triples, &Label), (Changed<Triples>, Without<Dirty>)>,
    mut commands: Commands,
    extractor: Res<OntologyExtractor>,
) {
    for (e, triples, label) in &query {
        let classes: HashSet<_> = triples
            .0
            .quads_matching(Any, [rdf::type_], extractor.properties(), Any)
            .flatten()
            .flat_map(|x| derive_property(x.s(), &triples, "owl_property"))
            .collect();

        info!(
            "({} properties) Found {} properties for {} ({} triples)",
            extractor.properties().len(),
            classes.len(),
            label.0.as_str(),
            triples.0.len()
        );
        commands.entity(e).insert(Wrapped(classes));
    }
}

#[instrument(skip(query, other, hierarchy))]
pub fn complete_properties(
    mut query: Query<(
        &TokenComponent,
        &TripleComponent,
        &Prefixes,
        &DocumentLinks,
        &Label,
        &Types,
        &mut CompletionRequest,
    )>,
    other: Query<(&Label, &Wrapped<DefinedProperties>)>,
    hierarchy: Res<TypeHierarchy<'static>>,
) {
    debug!("Complete properties");
    for (token, triple, prefixes, links, this_label, types, mut request) in &mut query {
        debug!("target {:?} text {}", triple.target, token.text);
        debug!("links {:?}", links);
        if triple.target == TripleTarget::Predicate {
            let tts = types.get(&triple.triple.subject.value);
            for (label, properties) in &other {
                // Check if this thing is actually linked
                if links
                    .iter()
                    .find(|link| link.0.as_str().starts_with(label.0.as_str()))
                    .is_none()
                    && label.0 != this_label.0
                {
                    debug!("This link is ignored {}", label.as_str());
                    continue;
                }

                for class in properties.0.iter() {
                    let to_beat = prefixes
                        .shorten(&class.predicate.value)
                        .map(|x| Cow::Owned(x))
                        .unwrap_or(class.predicate.value.clone());

                    debug!(
                        "{} starts with {} = {}",
                        to_beat,
                        token.text,
                        to_beat.starts_with(&token.text)
                    );

                    if to_beat.starts_with(&token.text) {
                        let correct_domain = class.domain.iter().any(|domain| {
                            if let Some(domain_id) = hierarchy.get_id_ref(&domain) {
                                if let Some(tts) = tts {
                                    tts.iter().any(|tt| *tt == domain_id)
                                } else {
                                    false
                                }
                            } else {
                                false
                            }
                        });

                        let mut completion = SimpleCompletion::new(
                            CompletionItemKind::PROPERTY,
                            format!("{}", to_beat),
                            TextEdit {
                                range: token.range.clone(),
                                new_text: to_beat.to_string(),
                            },
                        )
                        .label_description(&class.comment);

                        if correct_domain {
                            completion.kind = CompletionItemKind::FIELD;
                            debug!("Property has correct domain {}", to_beat);
                            request.push(completion.sort_text("1"));
                        } else {
                            request.push(completion);
                        }
                    }
                }
            }
        }
    }
}

#[instrument(skip(query, other))]
pub fn hover_property(
    mut query: Query<(
        &TokenComponent,
        &Prefixes,
        &DocumentLinks,
        &mut HoverRequest,
    )>,
    other: Query<(&Label, Option<&Prefixes>, &Wrapped<DefinedProperties>)>,
) {
    for (token, prefixes, links, mut request) in &mut query {
        if let Some(target) = prefixes.expand(token.token.value()) {
            for (label, p2, classes) in &other {
                // Check if this thing is actually linked
                if links.iter().find(|link| link.0 == label.0).is_none() {
                    continue;
                }

                let shorten = |from: &str| {
                    if let Some(x) = prefixes.shorten(from) {
                        return Some(x);
                    }

                    if let Some(p) = p2 {
                        return p.shorten(from);
                    }

                    None
                };

                for c in classes.iter().filter(|c| c.predicate.value == target) {
                    request.0.push(format!("{}: {}", c.label, c.comment));
                    for r in &c.range {
                        let range = shorten(&r);
                        request.0.push(format!(
                            "Range {}",
                            range.as_ref().map(|x| x.as_str()).unwrap_or(r.as_str())
                        ));
                    }

                    for d in &c.domain {
                        let domain = shorten(&d);
                        request.0.push(format!(
                            "Domain {}",
                            domain.as_ref().map(|x| x.as_str()).unwrap_or(d.as_str())
                        ));
                    }
                }
            }
        }
    }
}
