use bevy_ecs::prelude::*;
use swls_core::{
    components::*,
    lsp_types::{Position, Range},
    prelude::*,
};
use tracing::debug;

use crate::{lang::formatter::format_turtle, TurtleLang};

pub fn format_turtle_system(
    mut query: Query<(&RopeC, &Element<TurtleLang>, &Comments, &mut FormatRequest), Without<Dirty>>,
) {
    debug!("Format turtle system");

    for (source, turtle, comments, mut request) in &mut query {
        if request.0.is_some() {
            debug!("Didn't format with the turtle format system, already formatted");
            continue;
        }
        debug!("Formatting with turtle format system");

        let formatted = format_turtle(
            &turtle.0,
            swls_core::lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments.0,
            &source.0,
        );

        request.0 = formatted.map(|x| {
            vec![swls_core::lsp_types::TextEdit::new(
                Range::new(
                    Position::new(0, 0),
                    Position::new(source.0.len_lines() as u32 + 1, 0),
                ),
                x,
            )]
        });
    }
}

#[cfg(test)]
mod test {
    use swls_core::prelude::FormatLabel;
    use swls_test_utils::{create_file, setup_world, TestClient};

    use super::*;

    #[test]
    fn format_does_it() {
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        let entity = create_file(
            &mut world,
            "@prefix foaf: <>.",
            "http://example.com/ns#",
            "turtle",
            Open,
        );

        world.entity_mut(entity).insert(FormatRequest(None));
        world.run_schedule(FormatLabel);
        let m_formatted: Option<FormatRequest> = world.entity_mut(entity).take();
        let m_formatted = m_formatted.and_then(|x| x.0);

        assert!(m_formatted.is_some());
        let formatted = &m_formatted.unwrap()[0].new_text;
        assert_eq!(formatted, "@prefix foaf: <>.\n\n");
    }
}
