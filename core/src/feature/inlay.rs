use bevy_ecs::{component::Component, schedule::ScheduleLabel, system::Query, world::World};
use derive_more::{AsMut, AsRef, Deref, DerefMut};

use crate::prelude::{RopeC, Triples};

/// [`Component`] indicating that the current document is currently handling a Inlay request.
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct InlayRequest(pub Option<Vec<crate::lsp_types::InlayHint>>);

/// [`ScheduleLabel`] related to the Inlay schedule
#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Label;
pub fn setup_schedule(world: &mut World) {
    let mut inlay = bevy_ecs::schedule::Schedule::new(Label);
    inlay.add_systems(inlay_test);
    world.add_schedule(inlay);
}

fn inlay_test(mut query: Query<(&Triples, &RopeC, &mut InlayRequest)>) {
    for (_triples, _rope, mut req) in &mut query {
        let out = Vec::new();
        // out.push(crate::lsp_types::InlayHint {
        //     position: Position::new(0, 0),
        //     label: crate::lsp_types::InlayHintLabel::String("Hello world".to_string()),
        //     kind: None,
        //     text_edits: None,
        //     tooltip: None,
        //     padding_left: None,
        //     padding_right: None,
        //     data: None,
        // });

        req.0 = Some(out);
    }
}
