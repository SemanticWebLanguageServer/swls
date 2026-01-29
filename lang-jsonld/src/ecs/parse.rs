use std::sync::Arc;

use bevy_ecs::prelude::*;
use lsp_core::{
    components::*,
    prelude::{Tokens, Triples},
};
use tracing::{info, instrument};

use crate::{
    lang::{parser::parse, tokenizer::tokenize, triples},
    JsonLd,
};

#[instrument(skip(query, commands))]
pub fn parse_source(
    query: Query<(Entity, &Source), (Changed<Source>, With<JsonLd>)>,
    mut commands: Commands,
) {
    for (entity, source) in &query {
        let (tok, es) = tokenize(source.0.as_str());
        info!("tokenized  {} tokens ({} errors)", tok.len(), es.len());
        commands.entity(entity).insert((Tokens(tok), Errors(es)));
    }
}

#[instrument(skip(query, commands))]
pub fn parse_jsonld_system(
    query: Query<(Entity, &Source, &Tokens, &Label), (Changed<Tokens>, With<JsonLd>)>,
    mut commands: Commands,
    config: Res<ServerConfig>,
) {
    if !config.config.jsonld.unwrap_or(true) {
        return;
    }
    for (entity, source, tokens, label) in &query {
        let (jsonld, es) = parse(source.as_str(), tokens.0.clone());
        info!("{} triples ({} errors)", label.0, es.len());
        if es.is_empty() {
            let element = Element::<JsonLd>(jsonld);
            commands
                .entity(entity)
                .insert((element, Errors(es)))
                .remove::<Dirty>();
        } else {
            let element = Element::<JsonLd>(jsonld);
            commands.entity(entity).insert((Errors(es), element, Dirty));
        }
    }
}

#[instrument(skip(query, commands))]
pub fn derive_triples(
    query: Query<(Entity, &Label, &Element<JsonLd>), Changed<Element<JsonLd>>>,
    mut commands: Commands,
) {
    for (e, l, el) in &query {
        let prefix = triples::derive_prefixes(&el, &l.0);
        let triples = triples::derive_triples(&el, &prefix);
        commands
            .entity(e)
            .insert((Triples(Arc::new(triples)), prefix));
    }
}
