use bevy_ecs::{
    component::Component,
    schedule::{IntoScheduleConfigs, Schedule, ScheduleLabel},
    world::World,
};

pub use crate::{
    systems::{
        get_current_prefix, hover_class, hover_excluded_property, hover_prefix, hover_property,
        hover_types, infer_types,
    },
    util::triple::get_current_triple,
};

/// [`Component`] indicating that the current document is handling a Hover request.
#[derive(Component, Debug, Default)]
pub struct HoverRequest(pub Vec<String>, pub Option<crate::lsp_types::Range>);

/// [`ScheduleLabel`] related to the Hover schedule
#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Label;

pub fn setup_schedule(world: &mut World) {
    let mut hover = Schedule::new(Label);
    hover.add_systems((
        infer_types,
        get_current_triple,
        // Runs after the triple lookup and drops its (wrong) result when the
        // cursor is on a prefix declaration, so the triple-based hovers below
        // no-op and `hover_prefix` describes the namespace instead.
        get_current_prefix.after(get_current_triple),
        hover_prefix.after(get_current_prefix),
        hover_types
            .before(hover_class)
            .before(hover_property)
            .after(get_current_prefix)
            .after(infer_types),
        hover_class.after(get_current_prefix),
        hover_property.after(get_current_prefix),
        hover_excluded_property.after(get_current_prefix),
    ));
    world.add_schedule(hover);
}
