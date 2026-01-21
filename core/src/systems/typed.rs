use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

use bevy_ecs::prelude::*;
use sophia_api::{
    ns::rdf,
    prelude::{Any, Dataset},
    quad::Quad as _,
};

use crate::{prelude::*, util::ns::rdfs};

#[derive(Default, Debug, Clone, Copy, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct TypeId(pub usize);

#[tracing::instrument(skip(query, hierarchy))]
pub fn extract_type_hierarchy(
    query: Query<&Triples, (Changed<Triples>, Without<Dirty>)>,
    mut hierarchy: ResMut<TypeHierarchy<'static>>,
) {
    for triples in &query {
        let class_terms = triples
            .quads_matching(Any, [rdf::type_], [rdfs::Class], Any)
            .flatten()
            .map(|x| x.s());

        let subclass = triples
            .quads_matching(Any, [rdfs::subClassOf], Any, Any)
            .flatten()
            .map(|x| x.s());

        let set: HashSet<_> = class_terms.chain(subclass).collect();

        for s in set {
            let id = hierarchy.get_id(s.as_str());

            for sub_class_of in triples
                .quads_matching([s], [rdfs::subClassOf], Any, Any)
                .flatten()
            {
                let object = hierarchy.get_id(sub_class_of.o().as_str());
                hierarchy.set_subclass_of(id, object);
            }

            for is_sub_class_of in triples
                .quads_matching(Any, [rdfs::subClassOf], [s], Any)
                .flatten()
            {
                let subject = hierarchy.get_id(is_sub_class_of.o().as_str());
                hierarchy.set_subclass_of(subject, id);
            }
        }
    }
}

pub fn infer_types(
    mut query: Query<(&Triples, &mut Types), (Changed<Triples>, With<Open>)>,
    hierarchy: Res<TypeHierarchy<'static>>,
    ontologies: Res<Ontologies>,
) {
    for (triples, mut types) in &mut query {
        types.clear();

        for t in triples
            .quads_matching(Any, [rdf::type_], Any, Any)
            .flatten()
        {
            if let Some(id) = hierarchy.get_id_ref(t.o().as_str()) {
                let vec = types.0.entry(t.s().value.clone()).or_default();
                vec.insert(id);
            }
        }

        let mut done: HashSet<(&MyTerm<'_>, &MyTerm<'_>)> = HashSet::new();

        for t in &triples.0 {
            let s = t.s();
            let p = t.p();
            let o = t.o();
            if p.as_str() == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" {
                continue;
            }

            if !done.contains(&(s, p)) {
                done.insert((s, p));

                if let Some(found_predicate) = ontologies.properties.get(p.as_str()) {
                    for d in &found_predicate.domains {
                        if let Some(id) = hierarchy.get_id_ref(d.as_str()) {
                            let vec = types.0.entry(s.value.clone()).or_default();
                            vec.insert(id);
                        } else {
                            tracing::error!(
                                "Tried to assign type (domain) {} but failed, not present in hierarchy",
                                d.as_str()
                            );
                        }
                    }
                }
            }

            if !done.contains(&(p, o)) {
                done.insert((p, o));
                if let Some(found_predicate) = ontologies.properties.get(p.as_str()) {
                    for d in &found_predicate.ranges {
                        if let Some(id) = hierarchy.get_id_ref(d.as_str()) {
                            let vec = types.0.entry(o.value.clone()).or_default();
                            vec.insert(id);
                        } else {
                            tracing::error!(
                                "Tried to assign type (range) {} but failed, not present in hierarchy",
                                d.as_str()
                            );
                        }
                    }
                }
            }
        }

        types.clean_up(&hierarchy);
    }
}

pub fn infer_current_type(
    query: Query<(Entity, &TripleComponent, &Types)>,
    mut commands: Commands,
) {
    for (e, tc, types) in &query {
        commands.entity(e).remove::<CurrentType>();
        if let Some(types) = types.get(tc.triple.s().as_str()) {
            tracing::debug!(
                "Found current type for {} {} {:?}",
                e,
                tc.triple.s().as_str(),
                types
            );
            commands.entity(e).insert(CurrentType(types.clone()));
        } else {
            tracing::debug!("No type found for {}", tc.triple.s().as_str(),);
        }
    }
}

#[tracing::instrument(skip(query, hierarchy))]
pub fn hover_types(
    mut query: Query<(&TokenComponent, &Types, &Prefixes, &mut HoverRequest)>,
    hierarchy: Res<TypeHierarchy<'static>>,
) {
    for (token, types, pref, mut hover) in &mut query {
        let Some(expaned) = pref.expand(&token.token) else {
            continue;
        };

        let Some(types) = types.get(expaned.as_str()) else {
            continue;
        };

        let mut type_string = String::new();
        for id in types {
            let type_name = hierarchy.type_name(*id);
            let type_name = pref
                .shorten(&type_name)
                .map(Cow::Owned)
                .unwrap_or(type_name.clone());
            if !type_string.is_empty() {
                type_string += ", ";
            }
            type_string += type_name.as_ref();
        }
        if !type_string.is_empty() {
            hover.0.push(format!("Type: {}", type_string));
        }
    }
}
