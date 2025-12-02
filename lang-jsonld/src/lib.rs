#![doc(
    html_logo_url = "https://ajuvercr.github.io/semantic-web-lsp/assets/icons/favicon.png",
    html_favicon_url = "https://ajuvercr.github.io/semantic-web-lsp/assets/icons/favicon.ico"
)]
use bevy_ecs::prelude::*;
use chumsky::prelude::Simple;
use lsp_core::{
    components::DynLang,
    lang::{Lang, LangHelper},
    lsp_types::SemanticTokenType,
    prelude::*,
    CreateEvent,
};
use ropey::Rope;

pub mod ecs;
pub mod lang;
use crate::{
    ecs::{highlight_named_nodes, keyword_highlight, setup_parse},
    lang::parser::Json,
};

pub fn setup_world(world: &mut World) {
    let mut semantic_token_dict = world.resource_mut::<SemanticTokensDict>();
    JsonLd::LEGEND_TYPES.iter().for_each(|lt| {
        if !semantic_token_dict.contains_key(lt) {
            let l = semantic_token_dict.0.len();
            semantic_token_dict.insert(lt.clone(), l);
        }
    });
    world.add_observer(|trigger: On<CreateEvent>, mut commands: Commands| {
        let e = trigger.event();
        match &e.language_id {
            Some(x) if x == "jsonld" => {
                commands
                    .entity(e.event_target())
                    .insert(JsonLd)
                    .insert(DynLang(Box::new(JsonLdHelper)));
                return;
            }
            _ => {}
        }
        // pass
        if trigger.event().url.as_str().ends_with(".jsonld") {
            commands
                .entity(e.event_target())
                .insert(JsonLd)
                .insert(DynLang(Box::new(JsonLdHelper)));
            return;
        }
    });

    world.schedule_scope(SemanticLabel, |_, schedule| {
        use semantic::*;
        schedule.add_systems((
            highlight_named_nodes
                .before(keyword_highlight)
                .after(basic_semantic_tokens),
            keyword_highlight
                .before(semantic_tokens_system)
                .after(basic_semantic_tokens),
        ));
    });

    world.schedule_scope(DiagnosticsLabel, |_, schedule| {
        use diagnostics::*;
        schedule.add_systems(publish_diagnostics::<JsonLd>);
    });

    setup_parse(world);
}

#[derive(Debug, Component)]
pub struct JsonLd;

impl Lang for JsonLd {
    type Token = Token;

    type TokenError = Simple<char>;

    type Element = Json;

    type ElementError = Simple<Token>;

    const PATTERN: Option<&'static str> = None;

    const LANG: &'static str = "jsonld";
    const CODE_ACTION: bool = false;
    const HOVER: bool = true;

    const TRIGGERS: &'static [&'static str] = &["@", "\""];
    const LEGEND_TYPES: &'static [SemanticTokenType] = &[
        SemanticTokenType::VARIABLE,
        SemanticTokenType::STRING,
        SemanticTokenType::NUMBER,
        SemanticTokenType::KEYWORD,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::ENUM_MEMBER,
    ];
}

#[derive(Debug)]
pub struct JsonLdHelper;
impl LangHelper for JsonLdHelper {
    fn get_relevant_text(
        &self,
        token: &Spanned<Token>,
        rope: &Rope,
    ) -> (String, std::ops::Range<usize>) {
        let r = token.span();
        match token.value() {
            Token::Str(st, _) => (st.clone(), r.start + 1..r.end - 1),
            _ => (self._get_relevant_text(token, rope), r.clone()),
        }
    }

    fn keyword(&self) -> &[&'static str] {
        &[]
    }
}
