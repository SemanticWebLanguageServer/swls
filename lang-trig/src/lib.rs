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
    register_rdf_lang::<TriGLang, TriGHelper>(world, &["trig"], &[".trig"]);
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
        SemanticTokenType::KEYWORD,
        SemanticTokenType::NAMESPACE,
        SemanticTokenType::NUMBER,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::STRING,
    ];

    fn semantic_token_type(kind: rowan::SyntaxKind) -> Option<SemanticTokenType> {
        use rdf_parsers::trig::parser::SyntaxKind as SK;
        match SK::from(kind) {
            SK::Comment => Some(SemanticTokenType::COMMENT),
            SK::Iriref => Some(SemanticTokenType::PROPERTY),
            SK::Integer | SK::Decimal | SK::Double => Some(SemanticTokenType::NUMBER),
            SK::StringLiteralQuote
            | SK::StringLiteralSingleQuote
            | SK::StringLiteralLongQuote
            | SK::StringLiteralLongSingleQuote => Some(SemanticTokenType::STRING),
            SK::Langtag => Some(semantic_token::LANG_TAG),
            SK::TrueLit | SK::FalseLit => Some(semantic_token::BOOLEAN),
            SK::BaseToken
            | SK::PrefixToken
            | SK::SparqlBaseToken
            | SK::SparqlPrefixToken
            | SK::GraphToken
            | SK::Alit => Some(SemanticTokenType::KEYWORD),
            _ => None,
        }
    }

    fn semantic_token_spans(
        kind: rowan::SyntaxKind,
        span: std::ops::Range<usize>,
        text: &str,
    ) -> Vec<(SemanticTokenType, std::ops::Range<usize>)> {
        use rdf_parsers::trig::parser::SyntaxKind as SK;
        match SK::from(kind) {
            SK::PnameLn => {
                // "prefix:local" → NAMESPACE for "prefix:" and PROPERTY for "local"
                if let Some((a, _)) = text.get(span.clone()).and_then(|s| s.split_once(':')) {
                    let (start, end) = (span.start, span.end);
                    vec![
                        (SemanticTokenType::NAMESPACE, start..start + a.len() + 1),
                        (SemanticTokenType::PROPERTY, start + a.len() + 1..end),
                    ]
                } else {
                    vec![(SemanticTokenType::PROPERTY, span)]
                }
            }
            SK::PnameNs => vec![(SemanticTokenType::NAMESPACE, span)],
            SK::BlankNodeLabel => {
                // "_:label" → "_:" as NAMESPACE, label as PROPERTY
                if span.len() > 2 {
                    vec![
                        (SemanticTokenType::NAMESPACE, span.start..span.start + 2),
                        (SemanticTokenType::PROPERTY, span.start + 2..span.end),
                    ]
                } else {
                    vec![(SemanticTokenType::NAMESPACE, span)]
                }
            }
            _ => Self::semantic_token_type(kind)
                .map(|t| vec![(t, span)])
                .unwrap_or_default(),
        }
    }
}
