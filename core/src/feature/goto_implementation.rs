use bevy_ecs::{
    component::Component,
    schedule::{IntoSystemConfigs, Schedule, ScheduleLabel},
    world::World,
};

pub use crate::{
    systems::{hover_class, hover_property, hover_types, infer_types},
    util::{token::get_current_token, triple::get_current_triple},
};

/// [`Component`] indicating that the current document is currently handling a GotoImplementation request.
#[derive(Component, Debug, Default)]
pub struct GotoImplementationRequest(pub Vec<lsp_types::Location>);

/// [`ScheduleLabel`] related to the GotoImplementation schedule
#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Label;

pub fn setup_schedule(world: &mut World) {
    let mut references = Schedule::new(Label);
    references.add_systems((
        get_current_token,
        get_current_triple.after(get_current_token),
        system::goto_implementation.after(get_current_triple),
    ));
    world.add_schedule(references);
}

mod system {
    use std::collections::HashSet;

    use bevy_ecs::prelude::*;
    use goto_implementation::GotoImplementationRequest;
    use sophia_api::{quad::Quad as _, term::TermKind};

    use crate::{prelude::*, util::token_to_location};

    pub fn goto_implementation(
        mut query: Query<(
            &TripleComponent,
            &Triples,
            &Label,
            &RopeC,
            &mut GotoImplementationRequest,
        )>,
        project: Query<(&Triples, &RopeC, &Label)>,
    ) {
        for (triple, triples, label, rope, mut req) in &mut query {
            let target = triple.kind();
            let Some(term) = triple.term() else {
                continue;
            };

            tracing::debug!("Found {} with kind {:?}", term.value, target);
            if target == TermKind::Iri {
                // This is a named node, we should look project wide
                for (triples, rope, label) in &project {
                    let subs: HashSet<_> = triples
                        .iter()
                        .map(|x| x.s())
                        .filter(|x| &x.value == &term.value)
                        .collect();

                    req.0.extend(
                        subs.into_iter()
                            .flat_map(|t| token_to_location(&t.span, label, &rope)),
                    );
                }
            } else if target == TermKind::BlankNode {
                // Blank node is constrained to current document
                let subs: HashSet<_> = triples
                    .iter()
                    .map(|x| x.s())
                    .filter(|x| &x.value == &term.value)
                    .collect();
                req.0.extend(
                    subs.into_iter()
                        .flat_map(|t| token_to_location(&t.span, label, &rope)),
                );
            }
        }
    }
}
