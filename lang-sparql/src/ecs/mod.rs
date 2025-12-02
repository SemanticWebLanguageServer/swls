use std::collections::{HashMap, HashSet};

use bevy_ecs::{prelude::*, world::World};
use completion::{CompletionRequest, SimpleCompletion};
use lang_turtle::lang::{
    context::{Context, TokenIdx},
    model::TriplesBuilder,
};
use lov::LocalPrefix;
use lsp_core::{components::*, prelude::*, systems::prefix::prefix_completion_helper};
use lsp_core::lsp_types::CompletionItemKind;
use sophia_iri::resolve::BaseIri;

use crate::{
    lang::{parsing::parse, tokenizer::parse_tokens_str},
    Sparql,
};

pub fn setup_parse(world: &mut World) {
    use lsp_core::feature::parse::*;
    world.schedule_scope(Label, |_, schedule| {
        schedule.add_systems((
            parse_source,
            parse_sparql_system.after(parse_source),
            derive_triples
                .after(parse_sparql_system)
                .before(prefixes)
                .before(triples),
        ));
    });
}

pub fn setup_completion(world: &mut World) {
    use lsp_core::feature::completion::*;
    world.schedule_scope(Label, |_, schedule| {
        schedule.add_systems((
            sparql_lov_undefined_prefix_completion.after(get_current_token),
            variable_completion.after(get_current_token),
        ));
    });
}

#[instrument(skip(query, commands))]
fn parse_source(
    query: Query<(Entity, &Source), (Changed<Source>, With<Sparql>)>,
    mut commands: Commands,
) {
    for (entity, source) in &query {
        let (tok, es) = parse_tokens_str(source.0.as_str());
        info!("tokenized  {} tokens ({} errors)", tok.len(), es.len());
        commands.entity(entity).insert((Tokens(tok), Errors(es)));
    }
}

#[instrument(skip(query, commands))]
fn parse_sparql_system(
    query: Query<(Entity, &Source, &Tokens, &Label), (Changed<Tokens>, With<Sparql>)>,
    mut commands: Commands,
    mut old: Local<HashMap<String, (Vec<Spanned<Token>>, Context)>>,
    config: Res<ServerConfig>,
) {
    if !config.config.sparql.unwrap_or(true) {
        return;
    }
    for (entity, source, tokens, label) in &query {
        let (ref mut old_tokens, ref mut context) = old.entry(label.to_string()).or_default();

        context.setup_current_to_prev(
            TokenIdx { tokens: &tokens },
            tokens.len(),
            TokenIdx {
                tokens: &old_tokens,
            },
            old_tokens.len(),
        );
        let ctx = context.ctx();

        let (jsonld, es) = parse(source.as_str(), label.0.clone(), tokens.0.clone(), ctx);

        *old_tokens = tokens.0.clone();
        context.clear();

        jsonld.add_to_context(context);

        // turtle.set_context(context);
        info!("{} triples ({} errors)", label.0, es.len());

        if es.is_empty() {
            let element = Element::<Sparql>(jsonld);
            commands
                .entity(entity)
                .insert((element, Errors(es)))
                .remove::<Dirty>();
        } else {
            let element = Element::<Sparql>(jsonld);
            commands.entity(entity).insert((Errors(es), element, Dirty));
        }
    }
}

#[instrument(skip(query, commands))]
fn derive_triples(
    query: Query<(Entity, &Label, &Element<Sparql>), Changed<Element<Sparql>>>,
    mut commands: Commands,
) {
    for (e, l, el) in &query {
        let query = el.0.value();

        let prefixes: Vec<_> = query
            .prefixes
            .iter()
            .flat_map(|prefix| {
                let url = prefix.value.expand(query)?;
                let url = lsp_core::lsp_types::Url::parse(&url).ok()?;
                Some(Prefix {
                    url,
                    prefix: prefix.prefix.value().clone(),
                })
            })
            .collect();

        commands.entity(e).insert(Prefixes(prefixes, l.0.clone()));

        if let Ok(base) = BaseIri::new(query.base.to_string()) {
            let mut builder = TriplesBuilder::new(query, base);
            let _ = query.ingest_triples(&mut builder);
            let triples: Vec<_> = builder.triples.into_iter().map(|x| x.to_owned()).collect();

            commands.entity(e).insert(Triples(triples));
        }
    }
}

#[instrument(skip(query,))]
pub fn variable_completion(
    mut query: Query<(&Tokens, &TokenComponent, &mut CompletionRequest), With<Sparql>>,
) {
    for (tokens, token, mut req) in &mut query {
        if token.text.starts_with('?') {
            let token_set: HashSet<&str> = tokens
                .0
                .iter()
                .flat_map(|x| match x.value() {
                    Token::Variable(x) => Some(x.as_str()),
                    _ => None,
                })
                .collect();

            for x in token_set {
                let t = format!("?{}", x);
                let completion = SimpleCompletion::new(
                    CompletionItemKind::VARIABLE,
                    t.clone(),
                    lsp_core::lsp_types::TextEdit {
                        range: token.range.clone(),
                        new_text: t,
                    },
                );
                req.push(completion);
            }
        }
    }
}

pub fn sparql_lov_undefined_prefix_completion(
    mut query: Query<(
        &TokenComponent,
        &Element<Sparql>,
        &Prefixes,
        &mut CompletionRequest,
    )>,
    lovs: Query<&LocalPrefix>,
) {
    for (word, turtle, prefixes, mut req) in &mut query {
        let mut start = Position::new(0, 0);

        if turtle.base_statement.is_some() {
            start = Position::new(1, 0);
        }

        use lsp_core::lsp_types::{Position, Range};
        prefix_completion_helper(
            word,
            prefixes,
            &mut req.0,
            |name, location| {
                Some(vec![lsp_core::lsp_types::TextEdit {
                    range: Range::new(start.clone(), start),
                    new_text: format!("PREFIX {}: <{}>\n", name, location),
                }])
            },
            lovs.iter(),
        );
    }
}
