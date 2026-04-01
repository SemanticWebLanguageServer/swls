use std::{collections::HashMap, sync::Arc};

use bevy_ecs::prelude::*;
use rowan::NodeOrToken;
use swls_core::prelude::*;
use tracing::{info, instrument};
use turtle::{PrevParseInfo, turtle::SyntaxKind};

use crate::{
    lang::parser::{parse_new, TurtleNode},
    TurtleLang,
};

/// Extract CST leaf tokens (skipping whitespace) and comments from the syntax tree.
fn extract_cst_tokens(
    syntax: &TurtleNode,
    source: &str,
) -> (Vec<Spanned<rowan::SyntaxKind>>, Vec<Spanned<String>>) {
    let mut tokens = Vec::new();
    let mut comments = Vec::new();

    for node_or_token in syntax.descendants_with_tokens() {
        if let NodeOrToken::Token(t) = node_or_token {
            let kind = t.kind();
            if kind == SyntaxKind::WhiteSpace {
                continue;
            }
            let range = t.text_range();
            let span = usize::from(range.start())..usize::from(range.end());
            let rowan_kind = rowan::SyntaxKind(kind as u16);

            if kind == SyntaxKind::Comment {
                let text = t.text().to_string();
                comments.push(spanned(text, span.clone()));
            }

            tokens.push(spanned(rowan_kind, span));
        }
    }

    // Validate spans against source length
    let src_len = source.len();
    tokens.retain(|t| t.span().start <= src_len && t.span().end <= src_len);
    comments.retain(|c| c.span().start <= src_len && c.span().end <= src_len);

    (tokens, comments)
}

#[instrument(skip(query, commands, prev_infos, config), name = "parse_turtle")]
pub fn parse_turtle_system(
    query: Query<
        (Entity, &Source, &Label, Option<&Open>),
        (Changed<Source>, With<TurtleLang>),
    >,
    mut commands: Commands,
    mut prev_infos: Local<HashMap<String, PrevParseInfo>>,
    config: Res<ServerConfig>,
) {
    if !config.config.turtle.unwrap_or(true) {
        return;
    }
    for (entity, source, label, open) in &query {
        let span = tracing::info_span!("parse_turtle", label = label.to_string());
        let _enter = span.enter();

        // For open (user-edited) documents, don't reuse incremental state.
        let prev = if open.is_none() {
            prev_infos.get(label.as_str())
        } else {
            None
        };
        let (turtle, errors, new_prev, syntax) =
            parse_new(source.0.as_str(), label.as_str(), prev);

        info!(
            "{} triples ({} parse errors)",
            turtle.triples.len(),
            errors.len()
        );
        if open.is_some() {
            for e in &errors {
                info!("Parse error {:?}: {}", e.range, e.msg);
            }
        }

        prev_infos.insert(label.to_string(), new_prev);

        let (cst_tokens, cst_comments) = extract_cst_tokens(&syntax, source.0.as_str());

        let span = 0..source.0.len();
        let element = Element::<TurtleLang>(swls_core::prelude::spanned(turtle, span));
        if errors.is_empty() {
            commands
                .entity(entity)
                .insert((
                    element,
                    Errors(errors),
                    CstTokens(cst_tokens),
                    Comments(cst_comments),
                ))
                .remove::<Dirty>();
        } else {
            commands.entity(entity).insert((
                element,
                Errors(errors),
                CstTokens(cst_tokens),
                Comments(cst_comments),
                Dirty,
            ));
        }
    }
}

pub fn derive_triples(
    query: Query<
        (Entity, &Label, &Element<TurtleLang>),
        (Changed<Element<TurtleLang>>, With<TurtleLang>),
    >,
    mut commands: Commands,
) {
    for (entity, label, turtle) in &query {
        use crate::lang::model::TurtleExt;
        match turtle.0.get_simple_triples() {
            Ok(tripl) => {
                let triples: Vec<_> = tripl.iter().map(|x| x.to_owned()).collect();
                commands.entity(entity).insert(Triples(Arc::new(triples)));
            }
            Err(e) => {
                info!("derive_triples: error for {}: {:?}", label.as_str(), e);
            }
        }
    }
}
