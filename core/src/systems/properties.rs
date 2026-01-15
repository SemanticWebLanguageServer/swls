use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

use bevy_ecs::prelude::*;
use completion::{CompletionRequest, SimpleCompletion};
use hover::HoverRequest;
use tracing::{debug, instrument};

use crate::{
    lsp_types::{CompletionItemKind, TextEdit},
    prelude::*,
    store::Store,
    util::triple::MyTerm,
};

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct DefinedClass {
    term: MyTerm<'static>,
    locations: HashSet<MyTerm<'static>>,
    titles: HashSet<String>,
    descriptions: HashSet<String>,
}
impl DefinedClass {
    pub fn full_title(&self) -> String {
        let mut title = String::new();

        for t in &self.titles {
            if !title.is_empty() {
                title += ", "
            }
            title += t;
        }
        title
    }

    pub fn full_docs(&self) -> String {
        let mut docs = String::new();
        for d in &self.descriptions {
            if !docs.is_empty() {
                docs += "\n"
            }
            docs += d;
        }

        for l in &self.locations {
            if !docs.is_empty() {
                docs += "\n"
            }
            docs += "from: ";
            docs += l.as_str();
        }
        docs
    }
}

pub type DefinedClasses = HashMap<oxigraph::model::NamedNode, DefinedClass>;

fn find_classes(
    store: &oxigraph::store::Store,
) -> Option<HashMap<oxigraph::model::NamedNode, DefinedClass>> {
    use oxigraph::model::{Literal, NamedNode};
    use oxigraph::sparql::{QueryResults, SparqlEvaluator};

    let mut definitions = HashMap::new();
    // SPARQL query
    if let QueryResults::Solutions(solutions) = SparqlEvaluator::new()
        .parse_query(include_str!("../queries/class_definitions.sparql"))
        .ok()?
        .on_store(&store)
        .execute()
        .ok()?
    {
        // ?location ?title ?description ?class
        for solution in solutions {
            if let Ok(solution) = solution {
                let class = solution.get("class").unwrap();
                // let location = solution.get("location").unwrap();
                let title = solution.get("title");
                let description = solution.get("description");

                match (
                    NamedNode::try_from(class.clone()),
                    // NamedNode::try_from(location.clone()),
                ) {
                    (
                        Ok(class),
                        // Ok(location)
                    ) => {
                        let v = definitions
                            .entry(class.clone())
                            .or_insert_with(|| DefinedClass {
                                term: MyTerm::named_node(class.as_str(), 0..0).to_owned(),
                                locations: HashSet::new(),
                                titles: HashSet::new(),
                                descriptions: HashSet::new(),
                            });

                        // v.locations
                        //     .insert(MyTerm::named_node(location.as_str(), 0..0).to_owned());
                        if let Some(title) = title.and_then(|x| Literal::try_from(x.clone()).ok()) {
                            v.titles.insert(title.value().to_string());
                        }

                        if let Some(description) =
                            description.and_then(|x| Literal::try_from(x.clone()).ok())
                        {
                            v.descriptions.insert(description.value().to_string());
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    Some(definitions)
}

pub fn derive_ontologies(
    query: Query<(), Added<Triples>>,
    store: Res<Store>,
    mut resource: ResMut<Ontologies>,
) {
    if !query.is_empty() {
        if let Some(classes) = find_classes(&store.0) {
            resource.classes = classes;
        }

        if let Some(properties) = find_properties(&store.0) {
            resource.properties = properties;
        }

        // if let Some(properties) = find_properties(&store.0) {
        //     for p in properties.into_values() {
        //         let title = p.full_title();
        //         let docs = p.full_docs();
        //         let domain = p.domains.into_iter().fold(String::new(), |mut acc, b| {
        //             if !acc.is_empty() {
        //                 acc += ", ";
        //             }
        //             acc += b.as_str();
        //             acc
        //         });
        //         let range = p.ranges.into_iter().fold(String::new(), |mut acc, b| {
        //             if !acc.is_empty() {
        //                 acc += ", ";
        //             }
        //             acc += b.as_str();
        //             acc
        //         });
        //         tracing::info!(
        //             "Found property {} ({} -> {}) {}\n{}",
        //             p.term,
        //             domain,
        //             range,
        //             title,
        //             docs
        //         )
        //     }
        // }
    }
}

#[instrument(skip(query, resource))]
pub fn complete_class(
    mut query: Query<(
        &TokenComponent,
        &TripleComponent,
        &Prefixes,
        &mut CompletionRequest,
    )>,
    resource: Res<Ontologies>,
) {
    for (token, triple, prefixes, mut request) in &mut query {
        if triple.triple.predicate.value == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
            && triple.target == TripleTarget::Object
        {
            for class in resource.classes.values() {
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
                        .label_description(class.full_title())
                        .documentation(class.full_docs()),
                    );
                }
            }
        }
    }
}

pub fn hover_class(
    mut query: Query<(&TokenComponent, &Prefixes, &mut HoverRequest)>,
    resource: Res<Ontologies>,
) {
    for (token, prefixes, mut request) in &mut query {
        if let Some(target) = prefixes.expand(token.token.value()) {
            // if let Some(class) = resource.classes.get(&target) {
            //
            // }

            for class in resource.classes.values() {
                if class.term.value == target {
                    request
                        .0
                        .push(format!("{}: {}", class.full_title(), class.full_docs()));
                }
            }
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct DefinedProperty {
    term: MyTerm<'static>,
    locations: HashSet<MyTerm<'static>>,
    titles: HashSet<String>,
    descriptions: HashSet<String>,
    domains: HashSet<MyTerm<'static>>,
    ranges: HashSet<MyTerm<'static>>,
}

impl DefinedProperty {
    pub fn full_title(&self) -> String {
        let mut title = String::new();

        for t in &self.titles {
            if !title.is_empty() {
                title += ", "
            }
            title += t;
        }
        title
    }

    pub fn full_docs(&self) -> String {
        let mut docs = String::new();
        for d in &self.descriptions {
            if !docs.is_empty() {
                docs += "\n"
            }
            docs += d;
        }

        for l in &self.locations {
            if !docs.is_empty() {
                docs += "\n"
            }
            docs += "from: ";
            docs += l.as_str();
        }
        docs
    }
}

fn find_properties(
    store: &oxigraph::store::Store,
) -> Option<HashMap<oxigraph::model::NamedNode, DefinedProperty>> {
    use oxigraph::model::{Literal, NamedNode};
    use oxigraph::sparql::{QueryResults, SparqlEvaluator};

    let mut definitions = HashMap::new();
    // SPARQL query
    if let QueryResults::Solutions(solutions) = SparqlEvaluator::new()
        .parse_query(include_str!("../queries/property_definitions.sparql"))
        .ok()?
        .on_store(&store)
        .execute()
        .ok()?
    {
        // ?location ?title ?description ?class
        for solution in solutions {
            if let Ok(solution) = solution {
                let class = solution.get("property").unwrap();
                // let location = solution.get("location").unwrap();
                let title = solution.get("title");
                let description = solution.get("description");

                let range = solution
                    .get("range")
                    .and_then(|r| NamedNode::try_from(r.clone()).ok())
                    .map(|r| MyTerm::named_node(r.as_str(), 0..0).to_owned());
                let domain = solution
                    .get("domain")
                    .and_then(|r| NamedNode::try_from(r.clone()).ok())
                    .map(|r| MyTerm::named_node(r.as_str(), 0..0).to_owned());

                match (
                    NamedNode::try_from(class.clone()),
                    // NamedNode::try_from(location.clone()),
                ) {
                    (
                        Ok(class),
                        // Ok(location)
                    ) => {
                        let v =
                            definitions
                                .entry(class.clone())
                                .or_insert_with(|| DefinedProperty {
                                    term: MyTerm::named_node(class.as_str(), 0..0).to_owned(),
                                    locations: HashSet::new(),
                                    titles: HashSet::new(),
                                    descriptions: HashSet::new(),
                                    ranges: HashSet::new(),
                                    domains: HashSet::new(),
                                });

                        // v.locations
                        //     .insert(MyTerm::named_node(location.as_str(), 0..0).to_owned());
                        if let Some(title) = title.and_then(|x| Literal::try_from(x.clone()).ok()) {
                            v.titles.insert(title.value().to_string());
                        }

                        if let Some(description) =
                            description.and_then(|x| Literal::try_from(x.clone()).ok())
                        {
                            v.descriptions.insert(description.value().to_string());
                        }
                        if let Some(domain) = domain {
                            v.domains.insert(domain);
                        }

                        if let Some(range) = range {
                            v.domains.insert(range);
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    Some(definitions)
}

pub type DefinedProperties = HashMap<oxigraph::model::NamedNode, DefinedProperty>;

#[instrument(skip(query, hierarchy, resource))]
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
    hierarchy: Res<TypeHierarchy<'static>>,
    resource: Res<Ontologies>,
) {
    debug!("Complete properties");
    for (token, triple, prefixes, links, _this_label, types, mut request) in &mut query {
        debug!("target {:?} text {}", triple.target, token.text);
        debug!("links {:?}", links);
        if triple.target == TripleTarget::Predicate {
            let tts = types.get(&triple.triple.subject.value);

            for property in resource.properties.values() {
                let to_beat = prefixes
                    .shorten(&property.term.value)
                    .map(|x| Cow::Owned(x))
                    .unwrap_or(property.term.value.clone());

                debug!(
                    "{} starts with {} = {}",
                    to_beat,
                    token.text,
                    to_beat.starts_with(&token.text)
                );

                if to_beat.starts_with(&token.text) {
                    let correct_domain = property.domains.iter().any(|domain| {
                        if let Some(domain_id) = hierarchy.get_id_ref(domain.as_str()) {
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
                    .label_description(&property.full_title())
                    .documentation(&property.full_docs());

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
                    .push(format!("{}\n{}", c.full_title(), c.full_docs()));
                for r in &c.ranges {
                    let range = prefixes.shorten(r.as_str());
                    request.0.push(format!(
                        "Range {}",
                        range.as_ref().map(|x| x.as_str()).unwrap_or(r.as_str())
                    ));
                }

                for d in &c.domains {
                    let domain = prefixes.shorten(d.as_str());
                    request.0.push(format!(
                        "Domain {}",
                        domain.as_ref().map(|x| x.as_str()).unwrap_or(d.as_str())
                    ));
                }
            }
        }
    }
}
