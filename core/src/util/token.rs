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

/// Whether a token is punctuation (`;`, `.`, `[`, …) rather than a word
/// (an IRI, prefixed name, literal, keyword, …).
fn is_punctuation(text: &str) -> bool {
    !text.is_empty() && text.chars().all(|c| ";.,[](){}".contains(c))
}

/// The token a cursor at byte `offset` belongs to, given the tokens' spans.
///
/// A cursor sits *between* characters, so for `ex:fo|;` it belongs to `ex:fo`
/// (the word being typed), and for `|ex:name` to `ex:name`. In order:
///
/// 1. a word token that contains the cursor,
/// 2. a word token that ends at the cursor,
/// 3. a punctuation token that contains the cursor or ends at it,
/// 4. none: the cursor is in whitespace.
pub fn token_at_cursor<'a>(
    spans: impl Iterator<Item = &'a Range<usize>> + Clone,
    source: &str,
    offset: usize,
) -> Option<Range<usize>> {
    let text = |r: &Range<usize>| source.get(r.clone()).unwrap_or_default();
    let narrowest = |it: &mut dyn Iterator<Item = &'a Range<usize>>| {
        it.min_by_key(|r| r.end - r.start).cloned()
    };
    let words = spans.clone().filter(|r| !is_punctuation(text(r)));
    let puncts = spans.filter(|r| is_punctuation(text(r)));

    narrowest(
        &mut words
            .clone()
            .filter(|r| r.start <= offset && offset < r.end),
    )
    .or_else(|| narrowest(&mut words.filter(|r| r.end == offset && r.start < r.end)))
    .or_else(|| narrowest(&mut puncts.filter(|r| r.start <= offset && offset <= r.end)))
}

/// Resolve the token at the request's cursor (see [`token_at_cursor`]) into a
/// [`TokenComponent`]. In whitespace, the token is empty and sits at the
/// cursor, so completion text is inserted there and filters accept anything.
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

        let span = token_at_cursor(cst_tokens.0.iter().map(|t| t.span()), &source.0, offset)
            .filter(|span| span.end <= source.0.len())
            .unwrap_or(offset..offset);

        let text = source.0[span.clone()].to_string();
        let Some(range) = range_to_range(&span, &rope.0) else {
            debug!("Failed to transform span to range");
            continue;
        };
        debug!("Token at cursor {:?} '{}'", span, text);
        commands.entity(entity).insert(TokenComponent {
            text,
            range,
            source_span: span,
            is_error: false,
        });
    }
}

#[cfg(test)]
mod cursor_tests {
    use super::token_at_cursor;

    /// Spans of the whitespace-separated tokens of `src`, with `;` `.` `[` `]`
    /// split off as their own tokens.
    fn spans(src: &str) -> Vec<std::ops::Range<usize>> {
        let mut out = Vec::new();
        let mut start = None;
        for (i, c) in src.char_indices() {
            let punct = ";.[]".contains(c);
            if c.is_whitespace() || punct {
                if let Some(s) = start.take() {
                    out.push(s..i);
                }
                if punct {
                    out.push(i..i + 1);
                }
            } else if start.is_none() {
                start = Some(i);
            }
        }
        if let Some(s) = start {
            out.push(s..src.len());
        }
        out
    }

    fn at(src: &str) -> Option<String> {
        let offset = src.find('|').unwrap();
        let src = src.replace('|', "");
        let spans = spans(&src);
        token_at_cursor(spans.iter(), &src, offset).map(|r| src[r].to_string())
    }

    #[test]
    fn cursor_belongs_to_the_word_it_touches() {
        assert_eq!(at("<a> ex:fo|;").as_deref(), Some("ex:fo"));
        assert_eq!(at("<a> ex:fo|").as_deref(), Some("ex:fo"));
        assert_eq!(at("<a> |ex:name .").as_deref(), Some("ex:name"));
        assert_eq!(at("<a> ex:na|me .").as_deref(), Some("ex:name"));
    }

    #[test]
    fn punctuation_only_when_no_word_touches_the_cursor() {
        assert_eq!(at("<a> a ex:C ;|").as_deref(), Some(";"));
        assert_eq!(at("<a> ex:p [|").as_deref(), Some("["));
    }

    #[test]
    fn whitespace_has_no_token() {
        assert_eq!(at("<a> a ex:C ;\n  |\n."), None);
        assert_eq!(at("<a> ex:p [\n    |\n] ."), None);
    }
}

pub mod semantic_token {
    use crate::lsp_types::SemanticTokenType as STT;
    pub const BOOLEAN: STT = STT::new("boolean");
    pub const LANG_TAG: STT = STT::new("langTag");
}
