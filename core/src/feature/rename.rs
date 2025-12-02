use bevy_ecs::{
    prelude::*,
    schedule::{IntoScheduleConfigs, ScheduleLabel},
};
use tracing::instrument;

pub use crate::util::token::get_current_token;
use crate::{lsp_types::TextEdit, prelude::*};

/// [`Component`] indicating that the current document is currently handling a PrepareRename request.
#[derive(Component, Debug)]
pub struct PrepareRenameRequest {
    pub range: crate::lsp_types::Range,
    pub placeholder: String,
}

/// [`ScheduleLabel`] related to the PrepareRename schedule
#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct PrepareRename;

/// [`Component`] indicating that the current document is currently handling a Rename request,
/// collecting [TextEdits](`lsp_types::TextEdit`).
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
        .add_systems((get_current_token, prepare_rename.after(get_current_token)));
    world.add_schedule(prepare_rename_schedule);

    let mut rename_schedule = Schedule::new(Rename);
    rename_schedule.add_systems((get_current_token, rename.after(get_current_token)));
    world.add_schedule(rename_schedule);
}

#[instrument(skip(query, commands,))]
pub fn prepare_rename(query: Query<(Entity, Option<&TokenComponent>)>, mut commands: Commands) {
    for (e, m_token) in &query {
        commands.entity(e).remove::<(PrepareRenameRequest,)>();
        if let Some(token) = m_token {
            let renameable = match token.token.value() {
                Token::Variable(_) => true,
                Token::IRIRef(_) => true,
                Token::PNameLN(_, _) => true,
                Token::BlankNodeLabel(_) => true,
                _ => false,
            };

            if renameable {
                commands.entity(e).insert(PrepareRenameRequest {
                    range: token.range.clone(),
                    placeholder: token.text.clone(),
                });
                continue;
            }
        }
        tracing::debug!("Didn't find a good token");
    }
}

#[instrument(skip(query,))]
pub fn rename(mut query: Query<(&TokenComponent, &Tokens, &RopeC, &Label, &mut RenameEdits)>) {
    for (token, tokens, rope, label, mut edits) in &mut query {
        tracing::debug!("Token {:?}", token);
        let new_text = edits.1.clone();
        for t in tokens.0.iter().filter(|x| x.value() == token.token.value()) {
            tracing::debug!("Changing {:?}", t);
            if let Some(range) = range_to_range(t.span(), &rope.0) {
                edits.0.push((
                    label.0.clone(),
                    TextEdit {
                        range,
                        new_text: new_text.clone(),
                    },
                ))
            }
        }
    }
}
