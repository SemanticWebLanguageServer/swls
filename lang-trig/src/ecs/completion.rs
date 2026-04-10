use bevy_ecs::prelude::*;
use completion::CompletionRequest;
use swls_core::systems::PrefixEntry;
use swls_core::{components::*, prelude::*, systems::prefix::prefix_completion_helper};
use swls_lov::LocalPrefix;

use crate::TriGLang;

pub fn trig_lov_undefined_prefix_completion(
    mut query: Query<(
        &TokenComponent,
        &Element<TriGLang>,
        &Prefixes,
        &mut CompletionRequest,
    )>,
    lovs: Query<&LocalPrefix>,
    prefix_cc: Query<&PrefixEntry>,
    config: Res<ServerConfig>,
) {
    for (word, turtle, prefixes, mut req) in &mut query {
        let mut start = swls_core::lsp_types::Position::new(0, 0);

        if turtle.base.is_some() {
            start = swls_core::lsp_types::Position::new(1, 0);
        }

        use swls_core::lsp_types::Range;
        prefix_completion_helper(
            word,
            prefixes,
            &mut req.0,
            |name, location| {
                Some(vec![swls_core::lsp_types::TextEdit {
                    range: Range::new(start.clone(), start),
                    new_text: format!("@prefix {}: <{}>.\n", name, location),
                }])
            },
            lovs.iter(),
            prefix_cc.iter(),
            &config.config.local,
        );
    }
}
