use std::{collections::HashMap, sync::Arc};

use bevy_ecs::prelude::*;
use swls_core::prelude::*;
use tracing::{info, instrument};
use turtle::PrevParseInfo;

use crate::{
    lang::{
        parser::parse_new,
        tokenizer::parse_tokens_str,
    },
    TurtleLang,
};

#[instrument(skip(query, commands), name = "parse_source")]
pub fn parse_source(
    query: Query<(Entity, &Source), (Changed<Source>, With<TurtleLang>)>,
    mut commands: Commands,
) {
    for (entity, source) in &query {
        let (tok, es) = parse_tokens_str(source.0.as_str());
        commands.entity(entity).insert(Tokens(tok));
        commands.entity(entity).insert(Errors(es));
    }
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
        // Incremental bias can cause worse error recovery on incomplete documents,
        // producing incorrect triple structures and breaking completion detection.
        let prev = if open.is_none() { prev_infos.get(label.as_str()) } else { None };
        let (turtle, errors, new_prev) = parse_new(source.0.as_str(), label.as_str(), prev);

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

        let span = 0..source.0.len();
        let element = Element::<TurtleLang>(swls_core::prelude::spanned(turtle, span));
        if errors.is_empty() {
            commands
                .entity(entity)
                .insert((element, Errors(errors)))
                .remove::<Dirty>();
        } else {
            commands.entity(entity).insert((element, Errors(errors), Dirty));
        }
    }
}

pub fn derive_triples(
    query: Query<(Entity, &Label, &Element<TurtleLang>), (Changed<Element<TurtleLang>>, With<TurtleLang>)>,
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
