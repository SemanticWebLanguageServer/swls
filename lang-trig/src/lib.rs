use bevy_ecs::{component::Component, resource::Resource, world::World};
use swls_core::{
    lang::{Lang, LangHelper},
    lsp_types::SemanticTokenType,
    prelude::*,
};
use swls_lang_rdf_base::register_rdf_lang;
use swls_lang_turtle::lang::parser::TurtleParseError;

pub mod ecs;
use crate::ecs::{format_trig_system, setup_completion, setup_parsing};

#[derive(Component, Default)]
pub struct TriGLang;

#[derive(Debug, Default)]
pub struct TriGHelper;

impl LangHelper for TriGHelper {
    fn keyword(&self) -> &[&'static str] {
        &["@prefix", "@base", "a", "GRAPH"]
    }
}

pub fn setup_world<C: Client + ClientSync + Resource + Clone>(world: &mut World) {
    register_rdf_lang::<TriGLang, TriGHelper>(world, "trig", &[".trig"]);
    setup_parsing(world);
    setup_completion(world);

    world.schedule_scope(FormatLabel, |_, schedule| {
        schedule.add_systems(format_trig_system);
    });
}

impl Lang for TriGLang {
    type Element = rdf_parsers::model::Turtle;
    type ElementError = TurtleParseError;

    const LANG: &'static str = "trig";
    const TRIGGERS: &'static [&'static str] = &[":"];
    const CODE_ACTION: bool = false;
    const HOVER: bool = true;
    const PATTERN: Option<&'static str> = None;

    const LEGEND_TYPES: &'static [SemanticTokenType] = &[
        semantic_token::BOOLEAN,
        semantic_token::LANG_TAG,
        SemanticTokenType::COMMENT,
        SemanticTokenType::ENUM_MEMBER,
        SemanticTokenType::ENUM,
        SemanticTokenType::KEYWORD,
        SemanticTokenType::NAMESPACE,
        SemanticTokenType::NUMBER,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::STRING,
        SemanticTokenType::VARIABLE,
    ];

    fn semantic_token_type(kind: rowan::SyntaxKind) -> Option<SemanticTokenType> {
        use rdf_parsers::trig::parser::SyntaxKind as SK;
        let k = kind.0;
        if k == SK::Comment as u16 {
            Some(SemanticTokenType::COMMENT)
        } else if k == SK::Iriref as u16 {
            Some(SemanticTokenType::PROPERTY)
        } else if k == SK::Integer as u16 || k == SK::Decimal as u16 || k == SK::Double as u16 {
            Some(SemanticTokenType::NUMBER)
        } else if k == SK::StringLiteralQuote as u16
            || k == SK::StringLiteralSingleQuote as u16
            || k == SK::StringLiteralLongQuote as u16
            || k == SK::StringLiteralLongSingleQuote as u16
        {
            Some(SemanticTokenType::STRING)
        } else if k == SK::Langtag as u16 {
            Some(semantic_token::LANG_TAG)
        } else if k == SK::TrueLit as u16 || k == SK::FalseLit as u16 {
            Some(semantic_token::BOOLEAN)
        } else if k == SK::BaseToken as u16
            || k == SK::PrefixToken as u16
            || k == SK::SparqlBaseToken as u16
            || k == SK::SparqlPrefixToken as u16
            || k == SK::Alit as u16
            || k == SK::GraphToken as u16
        {
            Some(SemanticTokenType::KEYWORD)
        } else {
            None
        }
    }

    fn semantic_token_spans(
        kind: rowan::SyntaxKind,
        span: std::ops::Range<usize>,
        text: &str,
    ) -> Vec<(SemanticTokenType, std::ops::Range<usize>)> {
        use rdf_parsers::trig::parser::SyntaxKind as SK;
        let k = kind.0;
        if k == SK::PnameLn as u16 {
            // "prefix:local" → NAMESPACE for "prefix:" and PROPERTY for "local"
            if let Some((a, _)) = text.get(span.clone()).and_then(|s| s.split_once(':')) {
                let (start, end) = (span.start, span.end);
                vec![
                    (SemanticTokenType::NAMESPACE, start..start + a.len() + 1),
                    (SemanticTokenType::PROPERTY, start + 1 + a.len()..end),
                ]
            } else {
                vec![(SemanticTokenType::PROPERTY, span)]
            }
        } else if k == SK::PnameNs as u16 {
            vec![(SemanticTokenType::NAMESPACE, span)]
        } else if k == SK::BlankNodeLabel as u16 {
            // "_:label" → "_:" as NAMESPACE, label as PROPERTY
            if span.len() > 2 {
                vec![
                    (SemanticTokenType::NAMESPACE, span.start..span.start + 2),
                    (SemanticTokenType::PROPERTY, span.start + 2..span.end),
                ]
            } else {
                vec![(SemanticTokenType::NAMESPACE, span)]
            }
        } else {
            Self::semantic_token_type(kind)
                .map(|t| vec![(t, span)])
                .unwrap_or_default()
        }
    }
}
