use std::{borrow::Cow, collections::HashSet};

use bevy_ecs::{
    component::Component,
    schedule::ScheduleLabel,
    system::{Query, Res},
    world::World,
};
use derive_more::{AsMut, AsRef, Deref, DerefMut};
use sophia_api::{ns::rdf, prelude::Dataset};

use crate::prelude::{DynLang, Prefixes, RopeC, Triples, TypeHierarchy, Types};

/// [`Component`] indicating that the current document is currently handling a Inlay request.
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug, Default)]
pub struct InlayRequest(pub Vec<crate::lsp_types::InlayHint>);

/// [`ScheduleLabel`] related to the Inlay schedule
#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Label;
pub fn setup_schedule(world: &mut World) {
    let mut inlay = bevy_ecs::schedule::Schedule::new(Label);
    inlay.add_systems(inlay_types);
    world.add_schedule(inlay);
}

#[tracing::instrument(skip(query, hierarchy))]
fn inlay_types(
    query: Query<(
        &Triples,
        &Types,
        &RopeC,
        &Prefixes,
        &DynLang,
        &mut InlayRequest,
    )>,
    hierarchy: Res<TypeHierarchy>,
) {
    for (triples, types, rope, prefixes, lang, mut request) in query {
        let subjects: HashSet<_> = triples.subjects().flatten().collect();

        let t = &mut request.0;

        for s in subjects {
            if let Some(types) = types.get(s.as_str()) {
                let defined: HashSet<_> = triples.objects([s], [rdf::type_]).collect();
                let defined_strings: HashSet<String> =
                    defined.iter().map(|x| x.value.to_string()).collect();

                let types: Vec<_> = types
                    .iter()
                    .map(|e| hierarchy.type_name(*e))
                    .filter(|t| !defined_strings.contains(t.as_ref()))
                    .map(|t| prefixes.shorten(t.as_ref()).map(Cow::Owned).unwrap_or(t))
                    .collect();

                if types.is_empty() {
                    continue;
                }

                if let Some(hint) = lang.inlay_types_hint(
                    &s.span,
                    rope,
                    defined.iter().max_by_key(|x| x.span.end).map(|x| &x.span),
                    types,
                ) {
                    t.push(hint);
                }
            }
        }
    }
}
