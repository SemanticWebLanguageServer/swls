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

pub mod config;
pub mod ecs;
pub mod lang;
pub mod prefix;

use crate::{
    config::{extract_known_prefixes_from_config, extract_known_shapes_from_config},
    ecs::{setup_code_action, setup_completion, setup_formatting, setup_parsing},
};

#[derive(Component, Default)]
pub struct TurtleLang;

#[derive(Debug, Default)]
pub struct TurtleHelper;
impl LangHelper for TurtleHelper {
    fn keyword(&self) -> &[&'static str] {
        &["@prefix", "@base", "a"]
    }
}

pub fn setup_world<C: Client + ClientSync + Resource + Clone>(world: &mut World) {
    register_rdf_lang::<TurtleLang, TurtleHelper>(world, "turtle", &[".ttl"]);

    world.schedule_scope(swls_core::Startup, |_, schedule| {
        schedule.add_systems((
            extract_known_prefixes_from_config::<C>,
            extract_known_shapes_from_config::<C>,
        ));
    });

    setup_parsing(world);
    setup_completion(world);
    setup_formatting(world);
    setup_code_action(world);
}

impl Lang for TurtleLang {
    type Element = rdf_parsers::model::Turtle;
    type ElementError = crate::lang::parser::TurtleParseError;

    const LANG: &'static str = "turtle";
    const TRIGGERS: &'static [&'static str] = &[":"];
    const CODE_ACTION: bool = true;
    const HOVER: bool = true;

    const LEGEND_TYPES: &'static [swls_core::lsp_types::SemanticTokenType] = &[
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

    const PATTERN: Option<&'static str> = None;

    fn semantic_token_type(kind: rowan::SyntaxKind) -> Option<SemanticTokenType> {
        use rdf_parsers::turtle::SyntaxKind as SK;

        // Convert rowan::SyntaxKind(u16) to turtle SyntaxKind via the raw discriminant.
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
        use rdf_parsers::turtle::SyntaxKind as SK;
        let k = kind.0;
        let this_kind = SK::from(kind);
        if k == SK::PnameLn as u16 {
            // PnameLn = "prefix:local" — emit NAMESPACE for "prefix:" and PROPERTY for "local".
            // Slice to just the token text so split_once finds the colon within the token,
            // not the first colon in the whole document.
            let token_text = text.get(span.clone()).unwrap_or("");
            if let Some((a, _)) = token_text.split_once(':') {
                let (start, end) = (span.start, span.end);
                vec![
                    (SemanticTokenType::NAMESPACE, start..start + a.len() + 1),
                    (SemanticTokenType::PROPERTY, start + a.len() + 1..end),
                ]
            } else {
                vec![(SemanticTokenType::PROPERTY, span)]
            }
        } else if k == SK::PnameNs as u16 {
            vec![(SemanticTokenType::NAMESPACE, span)]
        } else if k == SK::BlankNodeLabel as u16 {
            // "_:label" — "_:" is NAMESPACE, label is PROPERTY
            if span.len() > 2 {
                vec![
                    (SemanticTokenType::NAMESPACE, span.start..span.start + 2),
                    (SemanticTokenType::PROPERTY, span.start + 2..span.end),
                ]
            } else {
                vec![(SemanticTokenType::NAMESPACE, span)]
            }
        } else {
            let output = Self::semantic_token_type(kind)
                .map(|t| vec![(t, span.clone())])
                .unwrap_or_default();
            tracing::trace!("token kind {:?} {:?} {:?}", this_kind, span, output);
            output
        }
    }
}
