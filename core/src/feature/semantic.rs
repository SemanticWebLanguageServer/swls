use bevy_ecs::{prelude::*, schedule::ScheduleLabel};
use derive_more::{AsMut, AsRef, Deref, DerefMut};

use crate::{
    lsp_types::{SemanticToken, SemanticTokenType},
    prelude::*,
};

/// [`Resource`] mapping a [`SemanticTokenType`] to their used index.
#[derive(Resource, AsRef, Deref, AsMut, DerefMut, Debug, Default)]
pub struct SemanticTokensDict(pub std::collections::HashMap<SemanticTokenType, usize>);

/// [`Component`] indicating that the current document is handling a Highlight request.
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct HighlightRequest(pub Vec<SemanticToken>);

#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Label;

pub fn setup_world(world: &mut World) {
    // Each language registers its own basic_semantic_tokens::<L> in its own semantic schedule.
    let mut semantic_tokens = bevy_ecs::schedule::Schedule::new(Label);
    semantic_tokens.add_systems(semantic_tokens_system);
    world.add_schedule(semantic_tokens);
}

struct TokenHelper {
    start: usize,
    length: usize,
    ty: usize,
}

pub type TokenTypesComponent = Wrapped<Vec<Spanned<SemanticTokenType>>>;

/// Generic CST-based semantic token extraction.  Register this for each language via:
/// `world.schedule_scope(semantic::Label, |_, sched| { sched.add_systems(basic_semantic_tokens::<L>); })`
pub fn basic_semantic_tokens<L: Lang + Component>(
    mut query: Query<(Entity, &CstTokens, &Source), (With<HighlightRequest>, With<L>)>,
    mut commands: Commands,
) {
    for (e, cst_tokens, text) in &mut query {
        let types: TokenTypesComponent = Wrapped(
            cst_tokens
                .0
                .iter()
                .flat_map(|Spanned(kind, span)| {
                    L::semantic_token_spans(*kind, span.clone(), text)
                        .into_iter()
                        .map(|(t, s)| spanned(t, s))
                })
                .collect(),
        );
        commands.entity(e).insert(types);
    }
}

pub fn semantic_tokens_system(
    mut query: Query<(&RopeC, &TokenTypesComponent, &mut HighlightRequest)>,
    res: Res<SemanticTokensDict>,
) {
    tracing::debug!("semantic_tokens_system called");
    for (rope, types, mut req) in &mut query {
        let rope = &rope.0;
        // `ts` is indexed by *byte* offset; spans (`r`) are byte ranges.
        let mut ts: Vec<Option<SemanticTokenType>> = Vec::with_capacity(rope.len_bytes());
        ts.resize(rope.len_bytes(), None);
        types.iter().for_each(|Spanned(ty, r)| {
            r.clone().for_each(|j| {
                if j < ts.len() {
                    ts[j] = Some(ty.clone())
                } else {
                    tracing::error!(
                        "Semantic tokens type {} (index={}) falls outside of document size ({} bytes)",
                        ty.as_str(),
                        j,
                        rope.len_bytes()
                    );
                }
            });
        });

        let mut last = None;
        let mut start = 0;
        let mut out_tokens = Vec::new();
        for (i, ty) in ts.into_iter().enumerate() {
            if last != ty {
                if let Some(t) = last {
                    out_tokens.push(TokenHelper {
                        start,
                        length: i - start,
                        ty: res.get(&t).cloned().unwrap_or(0),
                    });
                }

                last = ty;
                start = i;
            }
        }

        if let Some(t) = last {
            out_tokens.push(TokenHelper {
                start,
                length: rope.len_bytes() - start, // byte length, like the branch above
                ty: res.get(&t).cloned().unwrap_or(0),
            });
        }

        // Emit LSP semantic tokens. `start`/`length` on each `TokenHelper` are byte
        // offsets/lengths; LSP wants UTF-16 columns and lengths. We walk the source
        // once with a monotonically advancing cursor (tokens are sorted, non-
        // overlapping), so this is O(n) overall — crucially avoiding an O(line²)
        // blow-up on minified single-line documents that a per-token byte→UTF-16
        // conversion would incur.
        let text = rope.as_str();
        let mut cur_byte = 0usize;
        let mut cur_line = 0u32;
        let mut cur_col = 0u32; // UTF-16 column within the current line
        let mut pre_line = 0u32;
        let mut pre_col = 0u32;
        req.0 = out_tokens
            .into_iter()
            .map(|token| {
                let advance = |from: usize, to: usize, line: &mut u32, col: &mut u32| -> u32 {
                    let mut utf16 = 0u32;
                    for ch in text.get(from..to).unwrap_or("").chars() {
                        let w = ch.len_utf16() as u32;
                        utf16 += w;
                        if ch == '\n' {
                            *line += 1;
                            *col = 0;
                        } else {
                            *col += w;
                        }
                    }
                    utf16
                };

                advance(cur_byte, token.start, &mut cur_line, &mut cur_col);
                let start_line = cur_line;
                let start_col = cur_col;
                let length = advance(token.start, token.start + token.length, &mut cur_line, &mut cur_col);
                cur_byte = token.start + token.length;

                let delta_line = start_line - pre_line;
                let delta_start = if delta_line == 0 {
                    start_col - pre_col
                } else {
                    start_col
                };
                pre_line = start_line;
                pre_col = start_col;

                SemanticToken {
                    delta_line,
                    delta_start,
                    length,
                    token_type: token.ty as u32,
                    token_modifiers_bitset: 0,
                }
            })
            .collect();
    }
}
