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

    pub fn full_docs(&self, hierarchy: &TypeHierarchy<'_>, pref: &Prefixes) -> String {
        let mut docs = String::new();
        for d in &self.descriptions {
            if !docs.is_empty() {
                docs += "\n"
            }
            docs += d;
        }

        if let Some(id) = hierarchy.get_id_ref(self.term.as_str()) {
            let superclasses = group_per_prefix(hierarchy.iter_superclass(id), pref);
            if !superclasses.is_empty() {
                if !docs.is_empty() {
                    docs += "\n\n"
                }
                docs += "Superclass of:";
                docs += superclasses.as_str();
            }
            let subclasses = group_per_prefix(hierarchy.iter_subclass(id), pref);
            if !subclasses.is_empty() {
                if !docs.is_empty() {
                    docs += "\n\n"
                }
                docs += "Subclass of:";
                docs += subclasses.as_str();
            }
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

fn group_per_prefix<'a>(subclasses: impl Iterator<Item = Cow<'a, str>>, pref: &Prefixes) -> String {
    let mut subclass_str = String::new();
    let subclasses: Vec<_> = subclasses
        .skip(1)
        .flat_map(|sub| pref.shorten(&sub))
        .collect();

    let mut per_prefix: HashMap<&'_ str, String> = HashMap::new();

    for sub in &subclasses {
        if let Some((first, snd)) = sub.split_once(':') {
            let value = per_prefix.entry(first).or_default();
            if !value.is_empty() {
                value.push_str(", ");
            }
            value.push_str(snd);
        }
    }

    let mut ordered: Vec<_> = per_prefix.into_iter().collect();
    ordered.sort();

    for (k, v) in ordered {
        subclass_str += "\n  ";
        subclass_str += k;
        subclass_str += ":{";
        subclass_str += &v;
        subclass_str += "}";
    }
    subclass_str
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
    mut hierarchy: ResMut<TypeHierarchy<'static>>,
) {
    if !query.is_empty() {
        if let Some(classes) = find_classes(&store.0) {
            resource.classes = classes;
        }

        if let Some(properties) = find_properties(&store.0) {
            for p in properties.values() {
                for domain in &p.domains {
                    let _ = hierarchy.get_id(domain.as_str());
                }

                for range in &p.ranges {
                    let _ = hierarchy.get_id(range.as_str());
                }
            }

            resource.properties = properties;
        }
    }
}

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
            // if let Some(class) = resource.classes.get(&target) {
            //
            // }

            for class in resource.classes.values() {
                if class.term.value == target {
                    request.0.push(format!(
                        "{}: {}",
                        class.full_title(),
                        class.full_docs(&hierarchy, &prefixes)
                    ));
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
    pub domains: HashSet<MyTerm<'static>>,
    pub ranges: HashSet<MyTerm<'static>>,
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

fn find_properties(store: &oxigraph::store::Store) -> Option<HashMap<String, DefinedProperty>> {
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
                        let v = definitions
                            // class.to_string() adds < > to the property
                            .entry(class.as_str().to_string())
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
                            v.ranges.insert(range);
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
        &Label,
        &Types,
        &mut CompletionRequest,
    )>,
    hierarchy: Res<TypeHierarchy<'static>>,
    resource: Res<Ontologies>,
) {
    for (token, triple, prefixes, _this_label, types, mut request) in &mut query {
        if triple.target == TripleTarget::Predicate {
            let tts = types.get(&triple.triple.subject.value);

            let subclasses: HashSet<_> = tts
                .iter()
                .flat_map(|x| x.iter())
                .flat_map(|t| hierarchy.iter_subclass(*t))
                .collect();

            for property in resource.properties.values() {
                let to_beat = prefixes
                    .shorten(&property.term.value)
                    .map(|x| Cow::Owned(x))
                    .unwrap_or(property.term.value.clone());

                if to_beat.starts_with(&token.text) {
                    let correct_domain = property
                        .domains
                        .iter()
                        .any(|domain| subclasses.contains(domain.as_str()));

                    let mut completion = SimpleCompletion::new(
                        CompletionItemKind::ENUM_MEMBER,
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
