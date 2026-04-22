use bevy_ecs::{
    component::Component,
    schedule::{IntoScheduleConfigs, Schedule, ScheduleLabel},
    world::World,
};

use crate::prelude::get_current_cst_token;
pub use crate::util::triple::get_current_triple;

/// [`Component`] indicating that the current document is handling a GotoDefinition request.
#[derive(Component, Debug, Default)]
pub struct GotoDefinitionRequest(pub Vec<crate::lsp_types::Location>);

/// [`ScheduleLabel`] related to the GotoDefinition schedule
#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Label;

pub fn setup_schedule(world: &mut World) {
    let mut references = Schedule::new(Label);
    references.add_systems((
        get_current_cst_token.before(get_current_triple),
        get_current_triple,
        system::goto_definition.after(get_current_triple),
    ));
    world.add_schedule(references);
}

mod system {
    use std::collections::HashSet;

    use bevy_ecs::prelude::*;
    use goto_definition::GotoDefinitionRequest;
    use sophia_api::{quad::Quad as _, term::TermKind};

    use crate::{prelude::*, util::token_to_location};

    pub fn goto_definition(
        mut query: Query<(
            &TripleComponent,
            &Triples,
            &Label,
            &RopeC,
            &PositionComponent,
            &mut GotoDefinitionRequest,
        )>,
        project: Query<(&Triples, &RopeC, &Label)>,
    ) {
        for (triple, triples, label, rope, pos, mut req) in &mut query {
            let target = triple.kind();
            let Some(term) = triple.term() else {
                continue;
            };
            let _span = tracing::debug_span!("goto_definition", term = %term.value).entered();
            let Some(idx) = position_to_offset(pos.0, rope) else {
                continue;
            };
            if !term.span.contains(&idx) {
                continue;
            }

            tracing::debug!("kind {:?}", target);
            if target == TermKind::Iri {
                for (triples, rope, label) in &project {
                    tracing::info!("Looking into buffer {}", label.as_str());
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
