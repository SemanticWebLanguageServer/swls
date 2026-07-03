use bevy_ecs::{component::Component, resource::Resource, world::World};
use swls_core::{
    lang::{Lang, LangHelper},
    lsp_types::SemanticTokenType,
    prelude::*,
};
use swls_lang_rdf_base::register_rdf_lang;
use swls_lang_turtle::lang::parser::TurtleParseError;

pub mod ecs;
use crate::ecs::{format_n3_system, setup_parsing};

#[derive(Component, Default)]
pub struct N3Lang;

#[derive(Debug, Default)]
pub struct N3Helper;

impl LangHelper for N3Helper {
    fn keyword(&self) -> &[&'static str] {
        &["@prefix", "@base", "a", "has", "is", "of"]
    }
    fn model_based_rename(&self) -> bool {
        true
    }
    fn blank_node_code_actions(&self) -> bool {
        true
    }
}

pub fn setup_world<C: Client + ClientSync + Resource + Clone>(world: &mut World) {
    register_rdf_lang::<N3Lang, N3Helper>(world, &["n3"], &[".n3"]);
    setup_parsing(world);

    world.schedule_scope(FormatLabel, |_, schedule| {
        schedule.add_systems(format_n3_system);
    });
}

impl Lang for N3Lang {
    type ElementError = TurtleParseError;

    const LANG: &'static str = "n3";
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
        SemanticTokenType::VARIABLE,
    ];

    fn semantic_token_type(kind: rowan::SyntaxKind) -> Option<SemanticTokenType> {
        use rdf_parsers::n3::parser::SyntaxKind as SK;
        match SK::from(kind) {
            SK::Comment => Some(SemanticTokenType::COMMENT),
            SK::Iriref => Some(SemanticTokenType::PROPERTY),
            SK::Integer | SK::Decimal | SK::Double => Some(SemanticTokenType::NUMBER),
            SK::String => Some(SemanticTokenType::STRING),
            SK::Langtag => Some(semantic_token::LANG_TAG),
            SK::BooleanLiteral => Some(semantic_token::BOOLEAN),
            SK::QuickVarName => Some(SemanticTokenType::VARIABLE),
            // N3 keywords: directives plus the verb keywords (a / has / is / of)
            // and the SPARQL-style BASE / PREFIX directives.
            SK::BaseToken
            | SK::PrefixToken
            | SK::BaseLit
            | SK::PrefixLit
            | SK::Alit
            | SK::HasLit
            | SK::IsLit
            | SK::OfLit => Some(SemanticTokenType::KEYWORD),
            _ => None,
        }
    }

    fn semantic_token_spans(
        kind: rowan::SyntaxKind,
        span: std::ops::Range<usize>,
        text: &str,
    ) -> Vec<(SemanticTokenType, std::ops::Range<usize>)> {
        use rdf_parsers::n3::parser::SyntaxKind as SK;
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
