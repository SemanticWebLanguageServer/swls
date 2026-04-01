#[macro_use]
extern crate tracing;

use bevy_ecs::prelude::*;
use swls_core::{lsp_types::SemanticTokenType, prelude::*};
use swls_lang_turtle::lang::parser::TurtleParseError;

pub mod ecs;
use crate::ecs::{setup_completion, setup_parse};
pub mod lang;

pub fn setup_world(world: &mut World) {
    let mut semantic_token_dict = world.resource_mut::<SemanticTokensDict>();
    Sparql::LEGEND_TYPES.iter().for_each(|lt| {
        if !semantic_token_dict.contains_key(lt) {
            let l = semantic_token_dict.0.len();
            semantic_token_dict.insert(lt.clone(), l);
        }
    });
    world.add_observer(|trigger: On<CreateEvent>, mut commands: Commands| {
        let e = trigger.event();
        match &e.language_id {
            Some(x) if x == "sparql" => {
                info!("Found sparql document!");
                commands
                    .entity(e.event_target())
                    .insert(Sparql)
                    .insert(DynLang(Box::new(SparqlHelper)));
                return;
            }
            _ => {}
        }

        if trigger.event().url.as_str().ends_with(".sq") {
            info!("Found sparql document!");
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

    // Register CST-based semantic highlighting for SPARQL.
    world.schedule_scope(swls_core::feature::SemanticLabel, |_, schedule| {
        use bevy_ecs::schedule::IntoScheduleConfigs;
        schedule.add_systems(
            swls_core::feature::semantic::basic_semantic_tokens::<Sparql>
                .before(swls_core::feature::semantic::semantic_tokens_system),
        );
    });

    setup_parse(world);
    setup_completion(world);
}

#[derive(Debug, Component)]
pub struct Sparql;

impl Lang for Sparql {
    type Element = turtle::model::Turtle;
    type ElementError = TurtleParseError;

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

    fn semantic_token_type(kind: rowan::SyntaxKind) -> Option<SemanticTokenType> {
        use turtle::sparql::parser::SyntaxKind as SK;
        let k = kind.0;
        if k == SK::Var as u16 {
            Some(SemanticTokenType::VARIABLE)
        } else if k == SK::MyString as u16
            || k == SK::StringLiteral1 as u16
            || k == SK::StringLiteral2 as u16
            || k == SK::StringLiteralLong1 as u16
            || k == SK::StringLiteralLong2 as u16
        {
            Some(SemanticTokenType::STRING)
        } else if k == SK::Integer as u16
            || k == SK::Decimal as u16
            || k == SK::Double as u16
        {
            Some(SemanticTokenType::NUMBER)
        } else if k == SK::Iriref as u16 {
            Some(SemanticTokenType::PROPERTY)
        } else if k == SK::PnameLn as u16 {
            Some(SemanticTokenType::ENUM_MEMBER)
        } else {
            None
        }
    }
}

/// All SPARQL keywords for completion.
pub static SPARQL_KEYWORDS: &[&str] = &[
    "REGEX", "SUBSTR", "REPLACE", "EXISTS", "SELECT", "DISTINCT", "REDUCED",
    "OPTIONAL", "UNION", "AS", "CONSTRUCT", "WHERE", "DESCRIBE", "ASK", "FROM",
    "NAMED", "GROUP", "BY", "HAVING", "ORDER", "ASC", "DESC", "LIMIT", "OFFSET",
    "VALUES", "LOAD", "SILENT", "CLEAR", "DROP", "CREATE", "ADD", "MOVE", "COPY",
    "INSERT", "DATA", "DELETE", "WITH", "USING", "DEFAULT", "ALL", "GRAPH",
    "SERVICE", "BIND", "UNDEF", "MINUS", "FILTER",
    // Aggregates
    "COUNT", "SUM", "MIN", "MAX", "AVG", "SAMPLE", "GROUP_CONCAT",
];

#[derive(Debug)]
pub struct SparqlHelper;

impl LangHelper for SparqlHelper {
    fn keyword(&self) -> &[&'static str] {
        SPARQL_KEYWORDS
    }
}
