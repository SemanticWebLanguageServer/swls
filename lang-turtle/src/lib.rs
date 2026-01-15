#![doc(
    html_logo_url = "https://ajuvercr.github.io/semantic-web-lsp/assets/icons/favicon.png",
    html_favicon_url = "https://ajuvercr.github.io/semantic-web-lsp/assets/icons/favicon.ico"
)]
use std::{borrow::Cow, collections::HashMap};

use bevy_ecs::{
    component::Component,
    event::EntityEvent,
    observer::On,
    resource::Resource,
    system::{Commands, Res},
    world::{CommandQueue, World},
};
use chumsky::prelude::Simple;
use lang::{context::Context, model::Turtle, parser::parse_turtle, tokenizer::parse_tokens_str};
use lov::LocalPrefix;
use lsp_core::{
    feature::diagnostics::publish_diagnostics,
    lang::{Lang, LangHelper},
    lsp_types::{SemanticTokenType, Url},
    prelude::*,
    CreateEvent,
};

pub mod ecs;
pub mod lang;

use crate::ecs::{setup_completion, setup_formatting, setup_parsing};

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
        // pass
        if trigger.event().url.as_str().ends_with(".ttl") {
            commands
                .entity(e.event_target())
                .insert((TurtleLang, DynLang(Box::new(TurtleHelper))));
            return;
        }
    });

    world.schedule_scope(lsp_core::feature::DiagnosticsLabel, |_, schedule| {
        schedule.add_systems(publish_diagnostics::<TurtleLang>);
    });

    world.schedule_scope(lsp_core::Startup, |_, schedule| {
        schedule.add_systems(extract_known_prefixes_from_config::<C>);
    });

    setup_parsing(world);
    setup_completion(world);
    setup_formatting(world);
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

    const LEGEND_TYPES: &'static [lsp_core::lsp_types::SemanticTokenType] = &[
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

/// Finds a resource
/// if the location starts with http, it is a remote resource
/// if the location fails to parse as a url, it is a file path
///     anyway, try to read the file
async fn find(location: &str, fs: &Fs, client: &impl Client) -> Option<Vec<(String, Url)>> {
    if location.starts_with("http") {
        let url = Url::parse(location).ok()?;
        let content = client
            .fetch(
                &location,
                &HashMap::from([(String::from("Accept"), String::from("text/turtle"))]),
            )
            .await
            .ok()?
            .body;
        Some(vec![(content, url)])
    } else {
        if let Ok(url) = Url::parse(&location) {
            let content = fs.0.read_file(&url).await?;
            Some(vec![(content, url)])
        } else {
            let files = fs.0.glob_read(&location).await?;
            Some(
                files
                    .into_iter()
                    .flat_map(|File { content, name }| {
                        if let Some(url) = file_name_to_url(&name) {
                            Some((content, url))
                        } else {
                            None
                        }
                    })
                    .collect(),
            )
        }
    }
}
#[cfg(not(target_arch = "wasm32"))]
fn file_name_to_url(name: &str) -> Option<Url> {
    lsp_core::lsp_types::Url::from_file_path(name).ok()
}

#[cfg(target_arch = "wasm32")]
fn file_name_to_url(name: &str) -> Option<Url> {
    None
}

fn prefix_from_url(url: &Url) -> Option<(Cow<'static, str>, Cow<'static, str>)> {
    let segments = url.path_segments()?;
    let last = segments.last()?;

    let prefix = last.rsplit_once('.').map(|(x, _)| x).unwrap_or(last);
    let prefix: Cow<'static, str> = Cow::Owned(prefix.to_string());

    let url = Cow::Owned(url.to_string());

    Some((prefix, url))
}

fn prefix_from_declaration(
    turtle: &Triples2<'_>,
) -> Option<(Cow<'static, str>, Cow<'static, str>)> {
    let subject = turtle
        .iter()
        .find(|t| {
            t.predicate.as_str() == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                && t.object.as_str() == "http://www.w3.org/2002/07/owl#Ontology"
        })?
        .subject
        .to_owned();

    let prefix = turtle
        .iter()
        .find(|t| {
            t.subject.eq(&subject)
                && t.predicate.as_str() == "http://purl.org/vocab/vann/preferredNamespacePrefix"
        })?
        .object
        .as_str();
    let prefix = Cow::Owned(String::from(prefix));

    let url = turtle
        .iter()
        .find(|t| {
            t.subject.eq(&subject)
                && t.predicate.as_str() == "http://purl.org/vocab/vann/preferredNamespaceUri"
        })?
        .object
        .as_str();
    let url = Cow::Owned(String::from(url));

    Some((prefix, url))
}

fn prefix_from_prefixes(
    model: &Turtle,
    turtle: &Triples2<'_>,
) -> Option<(Cow<'static, str>, Cow<'static, str>)> {
    let shortened: Vec<_> = turtle
        .iter()
        .filter(|x| x.predicate.as_str() == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
        .flat_map(|t| model.shorten(t.subject.as_str()))
        .collect();

    let mut counts = HashMap::new();
    for short in &shortened {
        if let Some((prefix, _)) = short.split_once(':') {
            let key: &mut usize = counts.entry(prefix).or_default();
            *key += 1;
        }
    }

    let (prefix, _) = counts.into_iter().max_by_key(|(_, e)| *e)?;

    let prefix = model
        .prefixes
        .iter()
        .find(|p| p.prefix.as_str() == prefix)?;

    Some((
        Cow::Owned(String::from(prefix.prefix.as_str())),
        Cow::Owned(prefix.value.0.expand(model)?),
    ))
}

fn prefix_from_source(url: &Url, source: &str) -> Option<(Cow<'static, str>, Cow<'static, str>)> {
    let (tok, _) = parse_tokens_str(source);
    let empty = Context::new();
    let (turtle, _) = parse_turtle(&url, tok, source.len(), empty.ctx());

    let triples = turtle.get_simple_triples().ok()?;

    prefix_from_declaration(&triples).or_else(|| prefix_from_prefixes(&turtle, &triples))
}

pub fn extract_known_prefixes_from_config<C: Client + ClientSync + Resource + Clone>(
    config: Res<ServerConfig>,
    client: Res<C>,
    fs: Res<Fs>,
    sender: Res<CommandSender>,
) {
    for on in config.config.local.ontologies.iter().cloned() {
        let c = client.clone();
        let fs = fs.clone();

        let sender = sender.0.clone();

        let fut = async move {
            let Some(files) = find(&on, &fs, &c).await else {
                return;
            };

            let mut queue = CommandQueue::default();
            for (content, url) in files {
                let Some((prefix, url)) =
                    prefix_from_source(&url, &content).or_else(|| prefix_from_url(&url))
                else {
                    continue;
                };

                tracing::info!(
                    "Adding local prefix from ontologies config location {} prefix {}",
                    url,
                    prefix
                );

                let lov = LocalPrefix {
                    location: Cow::Owned(url.to_string()),
                    content: Cow::Owned(content),
                    name: prefix.clone(),
                    title: prefix,
                    rank: 0,
                };

                queue.push(move |world: &mut World| {
                    world.spawn(lov);
                });
            }

            let _ = sender.unbounded_send(queue);

            // This is the plan of approach: - add these ontologies to the predefined LOV things - need prefered prefix:
            //          - filename.ttl -> filename
            //          - parse and look for things?
            //   - let the lov things also add prefixes to the prefix thing
            //   - Profit

            // spawn_or_insert(url, bundle, language_id, extra)
            // we have the turtle files!
            // we have 2 choices now:
            //  add it to the ecs without links
        };

        client.spawn(fut);
    }
}
