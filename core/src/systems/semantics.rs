use crate::{
    components::{HighlightRequest, RopeC, SemanticTokensDict, Wrapped},
    lang::TokenTrait,
    prelude::{spanned, Spanned},
    util::token::{Token, Tokens},
};
use bevy_ecs::prelude::*;
use bevy_ecs::schedule::ScheduleLabel;
use crate::lsp_types::{SemanticToken, SemanticTokenType};

struct TokenHelper {
    start: usize,
    length: usize,
    ty: usize,
}

pub type TokenTypesComponent = Wrapped<Vec<Spanned<SemanticTokenType>>>;
pub fn basic_semantic_tokens(
    mut query: Query<(Entity, &Tokens), With<HighlightRequest>>,
    mut commands: Commands,
) {
    for (e, tokens) in &mut query {
        let types: TokenTypesComponent = Wrapped(
            tokens
                .iter()
                .flat_map(|token| {
                    Token::span_tokens(token)
                        .into_iter()
                        .map(|(x, y)| spanned(x, y))
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
    for (rope, types, mut req) in &mut query {
        let rope = &rope.0;
        let mut ts: Vec<Option<SemanticTokenType>> = Vec::with_capacity(rope.len_chars());
        ts.resize(rope.len_chars(), None);
        types.iter().for_each(|Spanned(ty, r)| {
            r.clone().for_each(|j| ts[j] = Some(ty.clone()));
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
                length: rope.len_chars() - start,
                ty: res.get(&t).cloned().unwrap_or(0),
            });
        }

        let mut pre_line = 0;
        let mut pre_start = 0;
        req.0 = out_tokens
            .into_iter()
            .flat_map(|token| {
                let line = rope.try_byte_to_line(token.start as usize).ok()? as u32;
                let first = rope.try_line_to_char(line as usize).ok()? as u32;
                let start = rope.try_byte_to_char(token.start as usize).ok()? as u32 - first;
                let delta_line = line - pre_line;
                let delta_start = if delta_line == 0 {
                    start - pre_start
                } else {
                    start
                };
                let ret = Some(SemanticToken {
                    delta_line,
                    delta_start,
                    length: token.length as u32,
                    token_type: token.ty as u32,
                    token_modifiers_bitset: 0,
                });
                pre_line = line;
                pre_start = start;
                ret
            })
            .collect();
    }
}
