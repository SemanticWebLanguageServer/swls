use bevy_ecs::{
    component::Component,
    schedule::{IntoScheduleConfigs, Schedule, ScheduleLabel},
    world::World,
};

use crate::lsp_types::Location;
pub use crate::util::triple::get_current_triple;

/// [`Component`] indicating that the current document is handling a References request.
#[derive(Component, Debug, Default)]
pub struct ReferencesRequest(pub Vec<Location>);

/// [`ScheduleLabel`] related to the References schedule
#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Label;

pub fn setup_schedule(world: &mut World) {
    let mut references = Schedule::new(Label);
    references.add_systems((
        get_current_triple,
        system::add_references.after(get_current_triple),
    ));
    world.add_schedule(references);
}

mod system {
    use bevy_ecs::prelude::*;
    use references::ReferencesRequest;
    use sophia_api::{quad::Quad as _, term::TermKind};

    use crate::{prelude::*, util::token_to_location};

    pub fn add_references(
        mut query: Query<(
            &TripleComponent,
            &Triples,
            &Label,
            &RopeC,
            &mut ReferencesRequest,
        )>,
        project: Query<(&Triples, &RopeC, &Label)>,
    ) {
        for (triple, triples, label, rope, mut req) in &mut query {
            let target = triple.kind();
            let Some(term) = triple.term() else { continue };
            tracing::debug!("Found term '{}' with kind {:?}", term.value, target);

            if target == TermKind::Iri {
                for (proj_triples, proj_rope, proj_label) in &project {
                    req.0.extend(
                        proj_triples
                            .0
                            .iter()
                            .flat_map(|q| [q.s(), q.p(), q.o()])
                            .filter(|t| t == &term)
                            .flat_map(|t| token_to_location(&t.span, proj_label, proj_rope)),
                    );
                }
            } else if target == TermKind::BlankNode || target == TermKind::Variable {
                // Blank nodes and variables are document-local
                req.0.extend(
                    triples
                        .0
                        .iter()
                        .flat_map(|q| [q.s(), q.p(), q.o()])
                        .filter(|t| t == &term)
                        .flat_map(|t| token_to_location(&t.span, label, rope)),
                );
            }
        }
    }
}
