use bevy_ecs::{
    component::Component,
    schedule::{IntoScheduleConfigs, Schedule, ScheduleLabel},
    world::World,
};

pub use crate::{
    systems::{hover_class, hover_property, hover_types, infer_types},
    util::{token::get_current_token, triple::get_current_triple},
};

/// [`Component`] indicating that the current document is currently handling a Hover request.
#[derive(Component, Debug, Default)]
pub struct HoverRequest(pub Vec<String>, pub Option<crate::lsp_types::Range>);

/// [`ScheduleLabel`] related to the Parse schedule
#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Label;

pub fn setup_schedule(world: &mut World) {
    let mut hover = Schedule::new(Label);
    hover.add_systems((
        infer_types,
        get_current_token,
        get_current_triple.after(get_current_token),
        hover_types
            .before(hover_class)
            .before(hover_property)
            .after(get_current_token)
            .after(infer_types),
        hover_class.after(get_current_token),
        hover_property.after(get_current_token),
    ));
    world.add_schedule(hover);
}
