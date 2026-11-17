use std::collections::HashMap;

use bevy_ecs::prelude::*;

use super::types::{DefinedClass, DefinedProperty};
use crate::{prelude::*, store::Store, util::triple::MyTerm};

pub fn find_classes(
    store: &oxigraph::store::Store,
) -> Option<HashMap<oxigraph::model::NamedNode, DefinedClass>> {
    use oxigraph::{
        model::{Literal, NamedNode},
        sparql::QueryResults,
    };

    let mut definitions = HashMap::new();
    if let QueryResults::Solutions(solutions) =
        crate::store::union_query(include_str!("../../queries/class_definitions.sparql"))?
            .on_store(store)
            .execute()
            .ok()?
    {
        for solution in solutions.flatten() {
            let class = solution.get("class").unwrap();
            let title = solution.get("title");
            let description = solution.get("description");

            if let (Ok(class),) = (NamedNode::try_from(class.clone()),) {
                let v = definitions
                    .entry(class.clone())
                    .or_insert_with(|| DefinedClass {
                        term: MyTerm::named_node(class.as_str(), 0..0).to_owned(),
                        locations: std::collections::HashSet::new(),
                        titles: Default::default(),
                        descriptions: Default::default(),
                    });

                if let Some(title) = title.and_then(|x| Literal::try_from(x.clone()).ok()) {
                    v.titles.insert(title.value(), title.language());
                }

                if let Some(description) =
                    description.and_then(|x| Literal::try_from(x.clone()).ok())
                {
                    v.descriptions
                        .insert(description.value(), description.language());
                }
            }
        }
    }

    Some(definitions)
}

pub fn find_properties(store: &oxigraph::store::Store) -> Option<HashMap<String, DefinedProperty>> {
    use oxigraph::{
        model::{Literal, NamedNode},
        sparql::QueryResults,
    };

    let mut definitions = HashMap::new();
    if let QueryResults::Solutions(solutions) =
        crate::store::union_query(include_str!("../../queries/property_definitions.sparql"))?
            .on_store(store)
            .execute()
            .ok()?
    {
        for solution in solutions.flatten() {
            let class = solution.get("property").unwrap();
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

            if let (Ok(class),) = (NamedNode::try_from(class.clone()),) {
                let v = definitions
                    .entry(class.as_str().to_string())
                    .or_insert_with(|| DefinedProperty {
                        term: MyTerm::named_node(class.as_str(), 0..0).to_owned(),
                        locations: std::collections::HashSet::new(),
                        titles: Default::default(),
                        descriptions: Default::default(),
                        ranges: std::collections::HashSet::new(),
                        domains: std::collections::HashSet::new(),
                    });

                if let Some(title) = title.and_then(|x| Literal::try_from(x.clone()).ok()) {
                    v.titles.insert(title.value(), title.language());
                }

                if let Some(description) =
                    description.and_then(|x| Literal::try_from(x.clone()).ok())
                {
                    v.descriptions
                        .insert(description.value(), description.language());
                }
                if let Some(domain) = domain {
                    v.domains.insert(domain);
                }

                if let Some(range) = range {
                    v.ranges.insert(range);
                }
            }
        }
    }

    Some(definitions)
}

/// Recompute [`Ontologies`] whenever the store contents changed, i.e. whenever a
/// background document was added or its triples changed (see [`load_store`]).
///
/// [`load_store`]: crate::store::load_store
pub fn derive_ontologies(
    query: Query<(), (Changed<Triples>, Without<Open>)>,
    store: Res<Store>,
    mut resource: ResMut<Ontologies>,
    mut hierarchy: ResMut<TypeHierarchy>,
) {
    if !query.is_empty() {
        if let Some(classes) = find_classes(&store.0) {
            tracing::debug!("Derive {} classes", classes.len());
            resource.classes = classes;
        }

        if let Some(properties) = find_properties(&store.0) {
            tracing::debug!("Derive {} properties", properties.len());
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
