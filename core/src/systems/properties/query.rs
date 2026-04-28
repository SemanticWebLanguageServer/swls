use std::collections::HashMap;

use bevy_ecs::prelude::*;

use super::types::{DefinedClass, DefinedProperty};
use crate::{prelude::*, store::Store, util::triple::MyTerm};

pub fn find_classes(
    store: &oxigraph::store::Store,
) -> Option<HashMap<oxigraph::model::NamedNode, DefinedClass>> {
    use oxigraph::{
        model::{Literal, NamedNode},
        sparql::{QueryResults, SparqlEvaluator},
    };

    let mut definitions = HashMap::new();
    if let QueryResults::Solutions(solutions) = SparqlEvaluator::new()
        .parse_query(include_str!("../../queries/class_definitions.sparql"))
        .ok()?
        .on_store(&store)
        .execute()
        .ok()?
    {
        for solution in solutions {
            if let Ok(solution) = solution {
                let class = solution.get("class").unwrap();
                let title = solution.get("title");
                let description = solution.get("description");

                match (NamedNode::try_from(class.clone()),) {
                    (Ok(class),) => {
                        let v = definitions
                            .entry(class.clone())
                            .or_insert_with(|| DefinedClass {
                                term: MyTerm::named_node(class.as_str(), 0..0).to_owned(),
                                locations: std::collections::HashSet::new(),
                                titles: std::collections::HashSet::new(),
                                descriptions: std::collections::HashSet::new(),
                            });

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

pub fn find_properties(store: &oxigraph::store::Store) -> Option<HashMap<String, DefinedProperty>> {
    use oxigraph::{
        model::{Literal, NamedNode},
        sparql::{QueryResults, SparqlEvaluator},
    };

    let mut definitions = HashMap::new();
    if let QueryResults::Solutions(solutions) = SparqlEvaluator::new()
        .parse_query(include_str!("../../queries/property_definitions.sparql"))
        .ok()?
        .on_store(&store)
        .execute()
        .ok()?
    {
        for solution in solutions {
            if let Ok(solution) = solution {
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

                match (NamedNode::try_from(class.clone()),) {
                    (Ok(class),) => {
                        let v = definitions
                            .entry(class.as_str().to_string())
                            .or_insert_with(|| DefinedProperty {
                                term: MyTerm::named_node(class.as_str(), 0..0).to_owned(),
                                locations: std::collections::HashSet::new(),
                                titles: std::collections::HashSet::new(),
                                descriptions: std::collections::HashSet::new(),
                                ranges: std::collections::HashSet::new(),
                                domains: std::collections::HashSet::new(),
                            });

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

pub fn derive_ontologies(
    query: Query<(), Added<Triples>>,
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
