use std::collections::HashSet;

use bevy_ecs::{
    component::Component,
    schedule::ScheduleLabel,
    system::{Query, Res},
    world::World,
};
use derive_more::{AsMut, AsRef, Deref, DerefMut};
use sophia_api::{ns::rdf, prelude::Dataset};

use crate::{
    prelude::{Prefixes, RopeC, Triples, TypeHierarchy, Types},
    util::offset_to_position,
};

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
    query: Query<(&Triples, &Types, &RopeC, &Prefixes, &mut InlayRequest)>,
    hierarchy: Res<TypeHierarchy<'static>>,
) {
    for (triples, types, rope, prefixes, mut request) in query {
        let subjects: HashSet<_> = triples.subjects().flatten().collect();

        let t = &mut request.0;

        for s in subjects {
            if let Some(types) = types.get(s.as_str()) {
                let types: Vec<_> = types.iter().map(|e| hierarchy.type_name(*e)).collect();
                let defined: HashSet<_> = triples.objects([s], [rdf::type_]).collect();
                let defined_strings: HashSet<String> =
                    defined.iter().map(|x| x.value.to_string()).collect();

                let mut inlay_str = String::new();
                for t in &types {
                    if defined_strings.contains(t.as_ref()) {
                        continue;
                    }
                    if !inlay_str.is_empty() {
                        inlay_str += ", ";
                    }
                    if let Some(short) = prefixes.shorten(t.as_ref()) {
                        inlay_str += short.as_str();
                    } else {
                        inlay_str += t.as_ref();
                    }
                }

                if inlay_str.is_empty() {
                    continue;
                }
                if let Some(pos) = defined.iter().max_by_key(|x| x.span.end) {
                    if let Some(pos) = offset_to_position(pos.span.end, &rope) {
                        t.push(crate::lsp_types::InlayHint {
                            position: pos,
                            label: crate::lsp_types::InlayHintLabel::String(format!(
                                ", {}",
                                inlay_str
                            )),
                            kind: None,
                            text_edits: None,
                            tooltip: None,
                            padding_left: None,
                            padding_right: None,
                            data: None,
                        });
                    } else {
                        tracing::info!("Failed to convert pos to position {}", pos.span.end);
                    }
                } else {
                    let offset = if rope.get_char(s.span.start) == Some('[') {
                        s.span.start + 1
                    } else {
                        s.span.end
                    };

                    if let Some(pos) = offset_to_position(offset, &rope) {
                        t.push(crate::lsp_types::InlayHint {
                            position: pos,
                            label: crate::lsp_types::InlayHintLabel::String(format!(
                                " a {};",
                                inlay_str
                            )),
                            kind: None,
                            text_edits: None,
                            tooltip: None,
                            padding_left: None,
                            padding_right: None,
                            data: None,
                        });
                    } else {
                        tracing::info!("Failed to convert pos to position {}", s.span.end);
                    }
                }
            }
        }
    }
}
