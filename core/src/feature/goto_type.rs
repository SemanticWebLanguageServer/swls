use bevy_ecs::{
    component::Component,
    schedule::{IntoScheduleConfigs, Schedule, ScheduleLabel},
    world::World,
};

use crate::systems::infer_current_type;
pub use crate::{
    systems::infer_types,
    util::{token::get_current_token, triple::get_current_triple},
};

/// [`Component`] indicating that the current document is currently handling a GotoType request.
#[derive(Component, Debug, Default)]
pub struct GotoTypeRequest(pub Vec<crate::lsp_types::Location>);

/// [`ScheduleLabel`] related to the GotoImplementation schedule
#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Label;

pub fn setup_schedule(world: &mut World) {
    let mut references = Schedule::new(Label);
    references.add_systems((
        get_current_token,
        get_current_triple.after(get_current_token),
        infer_types,
        infer_current_type
            .after(get_current_triple)
            .after(infer_types),
        // system::goto_class_type.after(infer_current_type),
    ));
    world.add_schedule(references);
}

// TODO: goto_class_type is not implemented yet
