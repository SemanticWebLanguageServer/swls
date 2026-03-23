#![doc(
    html_logo_url = "https://ajuvercr.github.io/semantic-web-lsp/assets/icons/favicon.png",
    html_favicon_url = "https://ajuvercr.github.io/semantic-web-lsp/assets/icons/favicon.ico"
)]
use bevy_ecs::{
    component::Component,
    event::EntityEvent,
    observer::On,
    resource::Resource,
    system::Commands,
    world::World,
};
use chumsky::prelude::Simple;
use swls_core::{
    feature::diagnostics::publish_diagnostics,
    lang::{Lang, LangHelper},
    lsp_types::SemanticTokenType,
    prelude::*,
    CreateEvent,
};

pub mod config;
pub mod ecs;
pub mod lang;
pub mod prefix;

use crate::config::{extract_known_prefixes_from_config, extract_known_shapes_from_config};
use crate::ecs::{setup_code_action, setup_completion, setup_formatting, setup_parsing};

#[derive(Component)]
pub struct TurtleLang;

#[derive(Debug)]
pub struct TurtleHelper;
impl LangHelper for TurtleHelper {
    fn keyword(&self) -> &[&'static str] {
        &["@prefix", "@base", "a"]
    }
}

pub fn setup_world<C: Client + ClientSync + Resource + Clone>(world: &mut World) {
    let mut semantic_token_dict = world.resource_mut::<SemanticTokensDict>();
    TurtleLang::LEGEND_TYPES.iter().for_each(|lt| {
        if !semantic_token_dict.contains_key(lt) {
            let l = semantic_token_dict.0.len();
            semantic_token_dict.insert(lt.clone(), l);
        }
    });

    world.add_observer(|trigger: On<CreateEvent>, mut commands: Commands| {
        let e = &trigger.event();
        match &e.language_id {
            Some(x) if x == "turtle" => {
                commands
                    .entity(e.event_target())
                    .insert((TurtleLang, DynLang(Box::new(TurtleHelper))));
                return;
            }
            _ => {}
        }
        if trigger.event().url.as_str().ends_with(".ttl") {
            commands
                .entity(e.event_target())
                .insert((TurtleLang, DynLang(Box::new(TurtleHelper))));
            return;
        }
    });

    world.schedule_scope(swls_core::feature::DiagnosticsLabel, |_, schedule| {
        schedule.add_systems(publish_diagnostics::<TurtleLang>);
    });

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
    type Token = Token;

    type TokenError = Simple<char>;

    type Element = crate::lang::model::Turtle;

    type ElementError = Simple<Token>;

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
}
