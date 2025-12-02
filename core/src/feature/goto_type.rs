use bevy_ecs::{
    component::Component,
    schedule::{IntoScheduleConfigs, Schedule, ScheduleLabel},
    world::World,
};

use crate::systems::infer_current_type;
pub use crate::{
    systems::{hover_class, hover_property, hover_types, infer_types},
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
        system::goto_class_type.after(infer_current_type),
    ));
    world.add_schedule(references);
}

mod system {
    use std::{borrow::Cow, collections::HashSet};

    use bevy_ecs::prelude::*;
    use goto_type::GotoTypeRequest;

    use crate::{prelude::*, systems::DefinedClasses, util::token_to_location};

    pub fn goto_class_type(
        mut query: Query<(&CurrentType, &mut GotoTypeRequest)>,
        project: Query<(&Wrapped<DefinedClasses>, &RopeC, &Label)>,
        her: Res<TypeHierarchy<'static>>,
    ) {
        for (ty, mut req) in &mut query {
            let mut targets = HashSet::<Cow<'_, str>>::new();
            for t in &ty.0 {
                targets.insert(her.type_name(*t).clone());
            }
            tracing::debug!("Finding types for types {:?}", targets);

            for (classes, rope, label) in &project {
                for clazz in &classes.0 {
                    if targets.contains(clazz.term.as_str()) {
                        if let Some(location) = token_to_location(&clazz.location, &label, &rope) {
                            req.0.push(location);
                        }
                    }
                }
            }
        }
    }
}
