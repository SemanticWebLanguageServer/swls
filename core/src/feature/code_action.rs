use bevy_ecs::{component::Component, schedule::ScheduleLabel, world::World};

/// [`Component`] indicating that the current document is handling a CodeAction request.
#[derive(Component, Debug, Default)]
pub struct CodeActionRequest(pub Vec<crate::lsp_types::CodeAction>);

/// [`ScheduleLabel`] related to the CodeAction schedule, this is language specific
#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Label;

pub fn setup_schedule(world: &mut World) {
    let schedule = bevy_ecs::schedule::Schedule::new(Label);
    world.add_schedule(schedule);
}
