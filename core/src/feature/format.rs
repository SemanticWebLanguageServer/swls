use bevy_ecs::{component::Component, schedule::ScheduleLabel, world::World};
use derive_more::{AsMut, AsRef, Deref, DerefMut};

/// [`Component`] indicating that the current document is currently handling a Format request.
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct FormatRequest(pub Option<Vec<crate::lsp_types::TextEdit>>);

/// [`ScheduleLabel`] related to the Format schedule, this is language specific
#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Label;

pub fn setup_schedule(world: &mut World) {
    let format = bevy_ecs::schedule::Schedule::new(Label);
    // inlay.add_systems(inlay_triples);
    world.add_schedule(format);
}
