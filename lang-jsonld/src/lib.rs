#![doc(
    html_logo_url = "https://ajuvercr.github.io/semantic-web-lsp/assets/icons/favicon.png",
    html_favicon_url = "https://ajuvercr.github.io/semantic-web-lsp/assets/icons/favicon.ico"
)]
use bevy_ecs::{component::Component, resource::Resource, world::World};
use swls_core::{
    lang::{Lang, LangHelper},
    lsp_types::SemanticTokenType,
    prelude::*,
};
use swls_lang_rdf_base::register_rdf_lang;
use swls_lang_turtle::lang::parser::TurtleParseError;

pub mod ecs;
use crate::ecs::{setup_parsing, ContextCache};

#[derive(Component, Default)]
pub struct JsonLdLang;

#[derive(Debug, Default)]
pub struct JsonLdHelper;

impl LangHelper for JsonLdHelper {
    fn keyword(&self) -> &[&'static str] {
        &[
            "@context",
            "@id",
            "@type",
            "@graph",
            "@base",
            "@vocab",
            "@language",
            "@value",
            "@list",
            "@set",
            "@reverse",
            "@index",
            "@container",
        ]
    }
}

pub fn setup_world<C: Client + ClientSync + Resource + Clone>(world: &mut World) {
    register_rdf_lang::<JsonLdLang, JsonLdHelper>(world, "jsonld", &[".jsonld"]);
    world.insert_resource(ContextCache::default());
    setup_parsing::<C>(world);
}

impl Lang for JsonLdLang {
    type Element = rdf_parsers::model::Turtle;
    type ElementError = TurtleParseError;

    const LANG: &'static str = "jsonld";
    const TRIGGERS: &'static [&'static str] = &["\"@", "\""];
    const CODE_ACTION: bool = false;
    const HOVER: bool = true;
    const PATTERN: Option<&'static str> = None;

    const LEGEND_TYPES: &'static [SemanticTokenType] = &[
        semantic_token::BOOLEAN,
        SemanticTokenType::COMMENT,
        SemanticTokenType::KEYWORD,
        SemanticTokenType::NAMESPACE,
        SemanticTokenType::NUMBER,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::STRING,
    ];

    fn semantic_token_type(kind: rowan::SyntaxKind) -> Option<SemanticTokenType> {
        use rdf_parsers::jsonld::parser::SyntaxKind as SK;
        let k = kind.0;
        if k == SK::Comment as u16 {
            Some(SemanticTokenType::COMMENT)
        } else if k == SK::StringToken as u16 {
            Some(SemanticTokenType::STRING)
        } else if k == SK::JsonNumber as u16 {
            Some(SemanticTokenType::NUMBER)
        } else if k == SK::TrueLit as u16 || k == SK::FalseLit as u16 || k == SK::NullLit as u16 {
            Some(semantic_token::BOOLEAN)
        } else {
            None
        }
    }

    fn semantic_token_spans(
        kind: rowan::SyntaxKind,
        span: std::ops::Range<usize>,
        text: &str,
    ) -> Vec<(SemanticTokenType, std::ops::Range<usize>)> {
        if text.get(span.start + 1..span.start + 2) == Some("@") {
            return vec![(SemanticTokenType::KEYWORD, span)];
        }
        if text.get(span.end..span.end + 1) == Some(":") {
            return vec![(SemanticTokenType::NAMESPACE, span)];
        }
        Self::semantic_token_type(kind)
            .map(|t| vec![(t, span)])
            .unwrap_or_default()
    }
}
