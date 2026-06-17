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
    query: Query<(Entity, &RopeC, &DynLang, Option<&TripleComponent>)>,
    mut commands: Commands,
) {
    for (e, rope, lang, m_triple) in &query {
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

                let raw: String = rope.0.slice(span.start..span.end).to_string();
                let inner = lang.0.rename_placeholder(&raw);

                // Guard: inner must be non-empty
                if inner.is_empty() {
                    continue;
                }

                // Compute how many chars are stripped from the front.
                let prefix_offset = inner.as_ptr() as usize - raw.as_ptr() as usize;
                let prefix_chars = raw[..prefix_offset].chars().count();
                let inner_char_len = inner.chars().count();

                let inner_start = span.start + prefix_chars;
                let inner_end = inner_start + inner_char_len;

                if inner_start >= inner_end || inner_end > span.end {
                    continue;
                }

                if let Some(range) = range_to_range(&(inner_start..inner_end), &rope.0) {
                    let placeholder = inner.to_string();
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
pub fn rename(mut query: Query<(&TripleComponent, &Triples, &RopeC, &Label, &DynLang, &mut RenameEdits)>) {
    for (triple, triples, rope, label, lang, mut edits) in &mut query {
        let Some(target) = triple.term() else {
            continue;
        };
        let new_text = lang.0.rename_wrap(&edits.1);

        // Collect unique byte-span ranges to avoid duplicate edits when the same
        // term appears as subject/predicate/object across multiple triples.
        let mut seen_spans: std::collections::HashSet<(usize, usize)> = std::collections::HashSet::new();

        for quad in triples.0.iter() {
            for term in [quad.s(), quad.p(), quad.o()] {
                if term == target {
                    let key = (term.span.start, term.span.end);
                    if seen_spans.insert(key) {
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
}
