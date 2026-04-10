use std::ops::Range;

use bevy_ecs::prelude::*;
use derive_more::{AsMut, AsRef, Deref, DerefMut};
use tracing::{debug, instrument};

use crate::{components::*, prelude::*};

/// [`Component`] used to indicate the currently targeted token during a request.
#[derive(Component, Debug)]
pub struct TokenComponent {
    pub range: crate::lsp_types::Range,
    pub text: String,
    pub source_span: Range<usize>,
    /// `true` when the cursor is on an error/invalid token.
    pub is_error: bool,
}

/// [`Component`] that contains CST leaf tokens from the rowan syntax tree.
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct CstTokens(pub Vec<Spanned<rowan::SyntaxKind>>);

#[instrument(skip(query, commands))]
pub fn get_current_cst_token(
    query: Query<(Entity, &CstTokens, &PositionComponent, &RopeC, &Source)>,
    mut commands: Commands,
) {
    for (entity, cst_tokens, position, rope, source) in &query {
        commands.entity(entity).remove::<TokenComponent>();
        let Some(offset) = position_to_offset(position.0, &rope.0) else {
            debug!("Couldn't transform to an offset ({:?})", position.0);
            continue;
        };

        let token = cst_tokens
            .0
            .iter()
            .filter(|x| x.span().contains(&offset))
            .min_by_key(|x| x.span().end - x.span().start);

        let Some(token) = token else {
            let closest = cst_tokens.0.iter().min_by_key(|x| {
                let d_start = offset.abs_diff(x.span().start);
                let d_end = offset.abs_diff(x.span().end);
                d_start.min(d_end)
            });
            let Some(token) = closest else {
                debug!("No CST tokens found");
                continue;
            };
            let span = token.span().clone();
            if span.start >= source.0.len() || span.end > source.0.len() {
                continue;
            }
            // Cursor is strictly past the token's end: it is in whitespace between tokens.
            // Use an empty-text placeholder at the cursor position so completion filters
            // (e.g. `to_beat.starts_with(&token.text)`) pass for all candidates.
            if offset > span.end {
                let empty_span = offset..offset;
                let Some(range) = range_to_range(&empty_span, &rope.0) else {
                    debug!("Failed to transform offset to range");
                    continue;
                };
                debug!("Cursor in whitespace after token, inserting empty TokenComponent");
                commands.entity(entity).insert(TokenComponent {
                    text: String::new(),
                    range,
                    source_span: empty_span,
                    is_error: false,
                });
                continue;
            }
            let text = source.0[span.clone()].to_string();
            let Some(range) = range_to_range(&span, &rope.0) else {
                debug!("Failed to transform span to range");
                continue;
            };
            debug!("Closest CST token {:?} '{}'", token.value(), text);
            commands.entity(entity).insert(TokenComponent {
                text,
                range,
                source_span: span,
                is_error: false,
            });
            continue;
        };

        let span = token.span().clone();
        if span.start >= source.0.len() || span.end > source.0.len() {
            continue;
        }
        let text = source.0[span.clone()].to_string();
        let Some(range) = range_to_range(&span, &rope.0) else {
            debug!("Failed to transform span to range");
            continue;
        };
        debug!("CST token at cursor {:?} '{}'", token.value(), text);
        commands.entity(entity).insert(TokenComponent {
            text,
            range,
            source_span: span,
            is_error: false,
        });
    }
}

pub mod semantic_token {
    use crate::lsp_types::SemanticTokenType as STT;
    pub const BOOLEAN: STT = STT::new("boolean");
    pub const LANG_TAG: STT = STT::new("langTag");
}
