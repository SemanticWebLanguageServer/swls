use bevy_ecs::prelude::*;
use lsp_core::{components::*, prelude::*};
use lsp_core::lsp_types::{Position, Range};
use tracing::info;

use crate::{lang::formatter::format_turtle, TurtleLang};

pub fn format_turtle_system(
    mut query: Query<(&RopeC, &Element<TurtleLang>, &Tokens, &mut FormatRequest), Without<Dirty>>,
) {
    info!("Format turtle system");

    for (source, turtle, tokens, mut request) in &mut query {
        if request.0.is_some() {
            info!("Didn't format with the turtle format system, already formatted");
            continue;
        }
        info!("Formatting with turtle format system");
        let comments: Vec<_> = tokens
            .iter()
            .flat_map(|x| {
                x.try_map_ref(|inner| match inner {
                    Token::Comment(c) => Some(c.to_string()),
                    _ => None,
                })
            })
            .collect();

        let formatted = format_turtle(
            &turtle.0,
            lsp_core::lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments,
            &source.0,
        );

        request.0 = formatted.map(|x| {
            vec![lsp_core::lsp_types::TextEdit::new(
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
    use lsp_core::prelude::FormatLabel;
    use test_utils::{create_file, setup_world, TestClient};

    use super::*;

    // crate::setup_world::<TestClient>(&mut world);
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
