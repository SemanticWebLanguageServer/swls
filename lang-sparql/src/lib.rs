#[macro_use]
extern crate tracing;

use bevy_ecs::prelude::*;
use chumsky::error::Simple;
use lsp_core::{lsp_types::SemanticTokenType, prelude::*};

pub mod ecs;
use crate::ecs::{setup_completion, setup_parse};
pub mod lang;
// pub mod model;
// use crate::model::Query;
// pub mod parsing;
// pub mod tokenizer;

pub fn setup_world(world: &mut World) {
    let mut semantic_token_dict = world.resource_mut::<SemanticTokensDict>();
    [SemanticTokenType::VARIABLE].iter().for_each(|lt| {
        if !semantic_token_dict.contains_key(lt) {
            let l = semantic_token_dict.0.len();
            semantic_token_dict.insert(lt.clone(), l);
        }
    });
    world.add_observer(|trigger: On<CreateEvent>, mut commands: Commands| {
        let e = trigger.event();
        match &e.language_id {
            Some(x) if x == "sparql" => {
                info!("Found sparql documnet!");
                commands
                    .entity(e.event_target())
                    .insert(Sparql)
                    .insert(DynLang(Box::new(SparqlHelper)));
                return;
            }
            _ => {}
        }

        if trigger.event().url.as_str().ends_with(".sq") {
            info!("Found sparql documnet!");
            commands
                .entity(e.event_target())
                .insert(Sparql)
                .insert(DynLang(Box::new(SparqlHelper)));
            return;
        }
    });

    world.schedule_scope(DiagnosticsLabel, |_, schedule| {
        schedule.add_systems(diagnostics::publish_diagnostics::<Sparql>);
    });

    setup_parse(world);

    setup_completion(world);
}

#[derive(Debug, Component)]
pub struct Sparql;

impl Lang for Sparql {
    type Token = Token;

    type TokenError = Simple<char>;

    type Element = crate::lang::model::Query;

    type ElementError = Simple<Token>;

    const PATTERN: Option<&'static str> = None;

    const LANG: &'static str = "sparql";
    const CODE_ACTION: bool = false;
    const HOVER: bool = true;

    const TRIGGERS: &'static [&'static str] = &[];
    const LEGEND_TYPES: &'static [SemanticTokenType] = &[
        SemanticTokenType::VARIABLE,
        SemanticTokenType::STRING,
        SemanticTokenType::NUMBER,
        SemanticTokenType::KEYWORD,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::ENUM_MEMBER,
    ];
}

lazy_static::lazy_static! {
    static ref KWDS: Vec<&'static str> = {
        let mut m = Vec::new();

        // lsp_core::token::SparqlCall::ITEMS.iter().for_each(|x| m.push(x.complete()));
        SparqlKeyword::ITEMS.iter().for_each(|x| m.push(x.complete()));
        SparqlAggregate::ITEMS.iter().for_each(|x| m.push(x.complete()));

        m
    };
}

#[derive(Debug)]
pub struct SparqlHelper;

impl LangHelper for SparqlHelper {
    fn keyword(&self) -> &[&'static str] {
        &KWDS
    }
}
