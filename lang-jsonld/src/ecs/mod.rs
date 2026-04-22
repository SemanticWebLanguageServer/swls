use std::{collections::HashMap, future::Future, pin::Pin};

use bevy_ecs::{
    prelude::*, schedule::ScheduleLabel, system::RunSystemOnce as _, world::CommandQueue,
};
use futures::{channel, SinkExt};
use rdf_parsers::{
    jsonld::{
        convert::{
            convert_with_loader, parse_jsonld_for_context, ActiveContext, ContextLoader, JsonLdVal,
        },
        parser::{Lang, Rule, SyntaxKind},
    },
    IncrementalBias, PrevParseInfo,
};
use rowan::{GreenNode, NodeOrToken};
use swls_core::{
    lsp_types::{request::InlayHintRefreshRequest, TextDocumentItem, Url},
    prelude::*,
};
use swls_lang_turtle::{ecs::parse::derive_triples_system, lang::parser::TurtleParseError};
use tracing::{info, instrument};

use crate::{cjs, JsonLdLang};

pub mod completion;
pub use completion::setup_completion;

/// Caches remotely-fetched `@context` documents so they survive across parse ticks.
#[derive(Resource, Default)]
pub struct ContextCache(pub HashMap<String, String>);

/// ECS component that carries the accumulated JSON-LD active context for a document.
///
/// Populated by [`parse_jsonld_system`] and consumed by JSON-LD-specific
/// completion systems to offer term-alias and prefix-based completions.
#[derive(Component, Debug, Default, Clone)]
pub struct JsonLdActiveContext(pub ActiveContext);

/// Resolves `@context` URLs by first checking open documents in the ECS world,
/// then falling back to the [`ContextCache`] of previously-fetched remote contexts.
/// Missing URLs are recorded so the caller can fetch them asynchronously.
struct WorldContextLoader {
    sender: CommandSender,
}

impl ContextLoader for WorldContextLoader {
    fn load_val<'a>(
        &'a mut self,
        url: &'a str,
    ) -> Pin<Box<dyn Future<Output = Option<rdf_parsers::jsonld::convert::JsonLdVal>> + 'a>> {
        info!("context loader load val {}", url);
        let cs = self.sender.clone();

        let url = url.to_string();
        Box::pin(async move {
            let (sender, receiver) = futures::channel::oneshot::channel::<Result<String, _>>();
            let mut command_queue = CommandQueue::default();
            let url2 = url.clone();
            tracing::info!("Trying to find context {}", url.as_str());
            command_queue.push(move |world: &mut World| {
                world.spawn(ContextRequest {
                    url: url2,
                    on_ready: Some(sender),
                });
                world.run_schedule(FetchLabel);
            });

            let _ = cs.unbounded_send(command_queue);

            let v = receiver.await.ok()?;
            info!("context loader got response for {}", url);
            match v {
                Ok(s) => parse_jsonld_for_context(&s),
                Err(x) => {
                    if let JsonLdVal::Object(members, _) = &x {
                        if let Some((_, _, _, ctx)) =
                            members.iter().find(|(k, _, _, _)| k == "@context")
                        {
                            return Some(ctx.clone());
                        }
                    }

                    Some(x)
                }
            }
        })
    }
    fn load<'a>(&'a mut self, url: &'a str) -> Pin<Box<dyn Future<Output = Option<String>> + 'a>> {
        info!("context loader load string {}", url);
        Box::pin(std::future::ready(None))
    }
}

pub fn setup_parsing<C: Client + Resource + Clone>(world: &mut World) {
    use swls_core::feature::parse::*;
    world.schedule_scope(ParseLabel, |_, schedule| {
        schedule.add_systems((
            parse_jsonld_system::<C>,
            derive_jsonld_prefixes
                .after(parse_jsonld_system::<C>)
                .before(prefixes),
            derive_triples_system::<JsonLdLang>
                .after(parse_jsonld_system::<C>)
                .before(triples),
        ));
    });

    let mut schedule = bevy_ecs::schedule::Schedule::new(FetchLabel);
    schedule.add_systems((
        cjs::cjs_loader.before(fetch_from_world),
        fetch_from_world,
        fetch_from_web::<C>.after(fetch_from_world),
        return_empty.after(fetch_from_web::<C>),
    ));
    world.add_schedule(schedule);
}

/// Indicates that something should be fetched
#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct FetchLabel;

fn return_empty(requests: Query<(Entity, &mut ContextRequest)>, mut commander: Commands) {
    for (e, mut r) in requests {
        if let Some(sender) = r.on_ready.take() {
            tracing::info!("loader sending empty {}", r.url);
            let _ = sender.send(Result::Ok(String::new()));
        } else {
            tracing::info!("loader cannot send empty {}", r.url);
        }
        commander.entity(e).despawn();
    }
}

fn fetch_from_world(
    mut requests: Query<(Entity, &mut ContextRequest)>,
    documents: Query<(&Label, Option<&Wrapped<JsonLdVal>>, Option<&Source>)>,
    mut commander: Commands,
) {
    let mut should_reparse = false;
    for (entity, mut request) in requests.iter_mut() {
        for (labels, jsonld_val, s) in documents {
            if labels.as_str() == request.url.as_str() {
                if let Some(sender) = request.on_ready.take() {
                    let o = match (jsonld_val, s) {
                        (Some(val), _) => {
                            let char_count = format!("{:?}", val).len();
                            tracing::info!(
                                "loader fetch_from_world sent {} (found val {} {})",
                                request.url,
                                jsonld_val.is_some(),
                                char_count
                            );
                            should_reparse = true;
                            sender.send(Result::Err(val.as_ref().clone()))
                        }
                        (_, Some(source)) => {
                            tracing::info!(
                                "loader fetch_from_world sent {} (found val false {})",
                                request.url,
                                source.len()
                            );
                            sender.send(Result::Ok(source.to_string()))
                        }
                        _ => {
                            tracing::info!(
                                "loader fetch_from_world sent {} (empty string)",
                                request.url,
                            );
                            sender.send(Result::Ok("".to_string()))
                        }
                    };
                    tracing::info!("Sent succesful {:?}", o);
                }
                if let Ok(mut e) = commander.get_entity(entity) {
                    e.despawn();
                }
            }
        }
    }
    if should_reparse {
        commander.run_schedule(ParseLabel);
    }
}

fn fetch_from_web<C: Client + Resource + Clone>(
    mut requests: Query<(&mut ContextRequest,), Added<ContextRequest>>,
    sender: Res<CommandSender>,
    client: Res<C>,
    fs: Res<Fs>,
) {
    for (r,) in requests.iter_mut() {
        let client_clone = client.as_ref().clone();
        let url = r.url.to_string();

        let Ok(label) = Url::parse(&url) else {
            continue;
        };
        let sender = sender.clone();
        if label.scheme().starts_with("http") {
            client.spawn(async move {
                if let Ok(resp) = client_clone.fetch(&url, &HashMap::new()).await {
                    let mut cq = CommandQueue::default();
                    cq.push(move |world: &mut World| {
                        if world
                            .query::<(Entity, &Label)>()
                            .iter(&world)
                            .find(|x| x.1.as_str() == &url)
                            .is_none()
                        {
                            world.spawn((Source(resp.body), Label(label)));
                            let _ = world.run_system_once(derive_jsonld_triples::<C>);
                        }
                    });
                    let _ = sender.unbounded_send(cq);
                }
            });
        } else {
            let fs = fs.clone();
            client.spawn(async move {
                if let Some(resp) = fs.0.read_file(&label).await {
                    let mut cq = CommandQueue::default();
                    cq.push(move |world: &mut World| {
                        if world
                            .query::<(Entity, &Label)>()
                            .iter(&world)
                            .find(|x| x.1.as_str() == &url)
                            .is_none()
                        {
                            let item = TextDocumentItem {
                                version: 1,
                                uri: label.clone(),
                                language_id: String::from("turtle"),
                                text: String::new(),
                            };
                            let e = world
                                .spawn((
                                    RopeC(ropey::Rope::from_str(&resp)),
                                    Source(resp),
                                    Label(label.clone()),
                                    Wrapped(item),
                                    Types(HashMap::new()),
                                ))
                                .id();

                            world.trigger(CreateEvent {
                                url: label,
                                language_id: Some("jsonld".to_string()),
                                entity: e,
                            });
                            tracing::info!("Found local file, rerunning the deriving {}", url);
                            let _ = world.run_system_once(derive_jsonld_triples::<C>);
                        }
                    });
                    let _ = sender.unbounded_send(cq);
                }
            });
        }
    }
}

#[derive(bevy_ecs::prelude::Component)]
pub struct ContextRequest {
    pub url: String,
    pub on_ready: Option<channel::oneshot::Sender<Result<String, JsonLdVal>>>,
}

fn extract_jsonld_cst_tokens(node: &rowan::SyntaxNode<Lang>) -> Vec<Spanned<rowan::SyntaxKind>> {
    let mut tokens = Vec::new();
    for not in node.descendants_with_tokens() {
        if let NodeOrToken::Token(t) = not {
            if t.kind() == SyntaxKind::WhiteSpace {
                continue;
            }
            let range = t.text_range();
            let span = usize::from(range.start())..usize::from(range.end());
            tokens.push(spanned(rowan::SyntaxKind(t.kind() as u16), span));
        }
    }
    tokens
}

fn collect_errors(node: &rowan::SyntaxNode<Lang>) -> Vec<TurtleParseError> {
    let mut errors = Vec::new();
    let mut stack = vec![node.clone()];
    while let Some(current) = stack.pop() {
        for child in current.children_with_tokens() {
            match child {
                NodeOrToken::Node(n) => {
                    if n.kind() == SyntaxKind::Error {
                        let range = rdf_parsers::effective_error_span::<Lang>(&n);
                        let msg = n
                            .parent()
                            .map(|p| format!("Expected: {:?}", p.kind()))
                            .unwrap_or_else(|| format!("Unexpected: {}", n.text()));
                        errors.push(TurtleParseError { range, msg });
                    } else {
                        stack.push(n);
                    }
                }
                NodeOrToken::Token(t) => {
                    if t.kind() == SyntaxKind::Error {
                        let r = t.text_range();
                        errors.push(TurtleParseError {
                            range: r.start().into()..r.end().into(),
                            msg: format!("Unexpected: {}", t.text()),
                        });
                    }
                }
            }
        }
    }
    errors
}

pub fn derive_jsonld_triples<C: Client + Resource + Clone>(
    query: Query<(Entity, &Wrapped<GreenNode>, &Label, &Source), With<JsonLdLang>>,
    sender: Res<CommandSender>,
    client: Res<C>,
) {
    for (e, gn, label, source) in query {
        let base = label.0.to_string();
        let span = 0..source.0.len();
        let mut sender = sender.clone();
        let gn = gn.0.clone();
        let c2 = client.clone();
        client.spawn_local(async move {
            let mut loader = WorldContextLoader {
                sender: sender.clone(),
            };
            let syntax = rowan::SyntaxNode::new_root(gn);
            let (jsonld_model, ctx) = convert_with_loader(&syntax, &mut loader, Some(base)).await;

            info!("{} triples", jsonld_model.triples.len(),);

            for t in &jsonld_model.triples {
                info!("{}", t.value());
            }

            let element = Element::<JsonLdLang>(spanned(jsonld_model, span));

            let mut command_queue = CommandQueue::default();
            command_queue.push(move |world: &mut World| {
                world
                    .entity_mut(e)
                    .insert((element, JsonLdActiveContext(ctx)));
                // Re-run ParseLabel so that derive_triples_system and
                // derive_jsonld_prefixes see the newly-inserted element
                // (they react to Changed<Element<JsonLdLang>>).  Without
                // this, those systems only fire on the *next* user edit.
                world.run_schedule(ParseLabel);
            });
            let _ = sender.0.send(command_queue).await;
            c2.send_request::<InlayHintRefreshRequest>(()).await;
        });
    }
}

#[instrument(skip(query, sender, commands, client))]
fn parse_jsonld_system<C: Client + Resource + Clone>(
    query: Query<
        (Entity, &Source, &Label, Option<&Wrapped<PrevParseInfo>>),
        (Changed<Source>, With<JsonLdLang>),
    >,
    mut commands: Commands,
    sender: Res<CommandSender>,
    client: Res<C>,
) {
    for (entity, source, label, prev) in &query {
        let (parse, new_prev) = rdf_parsers::parse_incremental(
            Rule::new(SyntaxKind::JsonldDoc),
            source.0.as_str(),
            prev.map(|x| &x.0),
            IncrementalBias::default(),
        );
        commands.entity(entity).insert(Wrapped(new_prev));

        let syntax = parse.syntax::<Lang>();
        let errors = collect_errors(&syntax);
        let cst_tokens = extract_jsonld_cst_tokens(&syntax);

        let mut loader = WorldContextLoader {
            sender: sender.clone(),
        };

        let gn = parse.green_node.clone();
        let base = label.0.to_string();
        let span = 0..source.0.len();
        let e = entity.clone();
        let mut sender = sender.clone();
        client.spawn_local(async move {
            let syntax = rowan::SyntaxNode::new_root(gn.clone());
            let (jsonld_model, ctx) = convert_with_loader(&syntax, &mut loader, Some(base)).await;

            info!("{} triples", jsonld_model.triples.len(),);

            for t in &jsonld_model.triples {
                info!("{}", t.value());
            }

            let element = Element::<JsonLdLang>(spanned(jsonld_model, span));

            let mut command_queue = CommandQueue::default();
            command_queue.push(move |world: &mut World| {
                world
                    .entity_mut(e)
                    .insert((element, JsonLdActiveContext(ctx)));
                // Re-run ParseLabel so that derive_triples_system and
                // derive_jsonld_prefixes see the newly-inserted element
                // (they react to Changed<Element<JsonLdLang>>).  Without
                // this, those systems only fire on the *next* user edit.
                world.run_schedule(ParseLabel);
            });
            let _ = sender.0.send(command_queue).await;
        });

        if errors.is_empty() {
            commands
                .entity(entity)
                .insert((
                    Errors(errors),
                    CstTokens(cst_tokens),
                    Wrapped(parse.green_node),
                ))
                .remove::<Dirty>();
        } else {
            commands.entity(entity).insert((
                Errors(errors),
                CstTokens(cst_tokens),
                Wrapped(parse.green_node),
                Dirty,
            ));
        }
    }
}

/// Derives a [`Prefixes`] component for a JSON-LD document from the prefixes
/// declared in its `@context` (e.g. `"foaf": "http://xmlns.com/foaf/0.1/"`).
///
/// This mirrors the equivalent system in `lang-turtle` and is required so that
/// generic completion and hover systems that rely on [`Prefixes`] work for
/// JSON-LD documents.
fn derive_jsonld_prefixes(
    query: Query<(Entity, &Label, &Element<JsonLdLang>), Changed<Element<JsonLdLang>>>,
    mut commands: Commands,
) {
    use swls_lang_turtle::lang::model::NamedNodeExt;

    for (entity, url, turtle) in &query {
        let prefixes: Vec<_> = turtle
            .prefixes
            .iter()
            .flat_map(|prefix| {
                let expanded = prefix.value.value().expand(turtle.value())?;
                let parsed_url = swls_core::lsp_types::Url::parse(&expanded).ok()?;
                Some(Prefix {
                    url: parsed_url,
                    prefix: prefix.prefix.value().clone(),
                })
            })
            .collect();

        let base = turtle
            .base
            .as_ref()
            .and_then(|b| {
                b.0 .1
                    .value()
                    .expand(turtle.value())
                    .and_then(|x| swls_core::lsp_types::Url::parse(&x).ok())
            })
            .unwrap_or_else(|| url.0.clone());

        commands.entity(entity).insert(Prefixes(prefixes, base));
    }
}

pub(crate) fn format_jsonld_system(
    mut query: Query<(&RopeC, &Wrapped<GreenNode>, &mut FormatRequest), With<JsonLdLang>>,
) {
    use swls_core::lsp_types::{Position, Range};
    for (source, node, mut request) in &mut query {
        if request.0.is_some() {
            tracing::debug!("Didn't format with the jsonld format system, already formatted");
            continue;
        }
        tracing::debug!("Formatting with turtle format system");

        let root = rowan::SyntaxNode::new_root(node.0.clone());

        let formatted = rdf_parsers::jsonld::format::format(&root, 80);

        request.0 = Some(vec![swls_core::lsp_types::TextEdit::new(
            Range::new(
                Position::new(0, 0),
                Position::new(source.0.len_lines() as u32 + 1, 0),
            ),
            formatted,
        )]);
    }
}
