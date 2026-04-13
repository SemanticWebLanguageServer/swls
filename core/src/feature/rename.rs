use bevy_ecs::{
    prelude::*,
    schedule::{IntoScheduleConfigs, ScheduleLabel},
};
use sophia_api::quad::Quad as _;
use tracing::instrument;

pub use crate::util::triple::get_current_triple;
use crate::{lsp_types::TextEdit, prelude::*};

/// [`Component`] indicating that the current document is handling a PrepareRename request.
#[derive(Component, Debug)]
pub struct PrepareRenameRequest {
    pub range: crate::lsp_types::Range,
    pub placeholder: String,
}

/// [`ScheduleLabel`] related to the PrepareRename schedule
#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct PrepareRename;

/// [`Component`] for collecting rename [`TextEdit`]s.
#[derive(Component, Debug)]
pub struct RenameEdits(
    pub Vec<(crate::lsp_types::Url, crate::lsp_types::TextEdit)>,
    pub String,
);

/// [`ScheduleLabel`] related to the Rename schedule
#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Rename;

pub fn setup_schedules(world: &mut World) {
    let mut prepare_rename_schedule = Schedule::new(PrepareRename);
    prepare_rename_schedule
        .add_systems((get_current_triple, prepare_rename.after(get_current_triple)));
    world.add_schedule(prepare_rename_schedule);

    let mut rename_schedule = Schedule::new(Rename);
    rename_schedule.add_systems((get_current_triple, rename.after(get_current_triple)));
    world.add_schedule(rename_schedule);
}

#[instrument(skip(query, commands))]
pub fn prepare_rename(
    query: Query<(Entity, &RopeC, Option<&TripleComponent>)>,
    mut commands: Commands,
) {
    for (e, rope, m_triple) in &query {
        commands.entity(e).remove::<PrepareRenameRequest>();
        if let Some(triple) = m_triple {
            use sophia_api::term::TermKind;
            let renameable = matches!(
                triple.kind(),
                TermKind::Iri | TermKind::BlankNode | TermKind::Variable
            );
            if renameable {
                let span = match triple.target {
                    TripleTarget::Subject => &triple.triple.subject.span,
                    TripleTarget::Predicate => &triple.triple.predicate.span,
                    TripleTarget::Object => &triple.triple.object.span,
                    TripleTarget::Graph => continue,
                };
                if let Some(range) = range_to_range(span, &rope.0) {
                    let placeholder = match triple.term() {
                        Some(t) => t.value.to_string(),
                        None => continue,
                    };
                    commands
                        .entity(e)
                        .insert(PrepareRenameRequest { range, placeholder });
                    continue;
                }
            }
        }
        tracing::debug!("Didn't find a renameable triple");
    }
}

#[instrument(skip(query))]
pub fn rename(mut query: Query<(&TripleComponent, &Triples, &RopeC, &Label, &mut RenameEdits)>) {
    for (triple, triples, rope, label, mut edits) in &mut query {
        let Some(target) = triple.term() else {
            continue;
        };
        let new_text = edits.1.clone();
        for quad in triples.0.iter() {
            for term in [quad.s(), quad.p(), quad.o()] {
                if term == target {
                    if let Some(range) = range_to_range(&term.span, &rope.0) {
                        edits.0.push((
                            label.0.clone(),
                            TextEdit {
                                range,
                                new_text: new_text.clone(),
                            },
                        ));
                    }
                }
            }
        }
    }
}
