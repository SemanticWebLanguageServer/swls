use std::{
    collections::HashMap,
    future::Future,
    pin::Pin,
    task::{Context, Poll, RawWaker, RawWakerVTable, Waker},
};

use bevy_ecs::{error::info, prelude::*, world::CommandQueue};
use rdf_parsers::{
    jsonld::{
        convert::{convert_with_loader, ActiveContext, ContextLoader},
        parser::{Lang, Rule, SyntaxKind},
    },
    IncrementalBias, PrevParseInfo,
};
use rowan::{GreenNode, NodeOrToken};
use swls_core::prelude::*;
use swls_lang_turtle::{ecs::parse::derive_triples_system, lang::parser::TurtleParseError};
use tracing::{info, instrument};

use crate::JsonLdLang;

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

/// Drives a `future::ready`-only async future synchronously using a noop waker.
///
/// This mirrors the approach used inside `rdf_parsers::jsonld::convert::convert`.
/// Panics if the future does not resolve in a single poll — which can only happen
/// if a [`ContextLoader`] implementation does real async I/O (ours never does).
fn poll_sync<F: Future>(fut: F) -> F::Output {
    fn noop_clone(p: *const ()) -> RawWaker {
        RawWaker::new(p, &NOOP_VTABLE)
    }
    fn noop(_: *const ()) {}
    static NOOP_VTABLE: RawWakerVTable = RawWakerVTable::new(noop_clone, noop, noop, noop);
    // SAFETY: The waker is never used to wake anything; the future must resolve
    // in one poll (all ContextLoader::load calls return future::ready).
    let waker = unsafe { Waker::from_raw(RawWaker::new(std::ptr::null(), &NOOP_VTABLE)) };
    let mut cx = Context::from_waker(&waker);
    match std::pin::pin!(fut).poll(&mut cx) {
        Poll::Ready(v) => v,
        Poll::Pending => panic!("WorldContextLoader must only return future::ready"),
    }
}

/// Resolves `@context` URLs by first checking open documents in the ECS world,
/// then falling back to the [`ContextCache`] of previously-fetched remote contexts.
/// Missing URLs are recorded so the caller can fetch them asynchronously.
struct WorldContextLoader<'a> {
    world_sources: &'a HashMap<String, String>,
    fetched: &'a HashMap<String, String>,
    pub missing: Vec<String>,
}

impl ContextLoader for WorldContextLoader<'_> {
    fn load<'a>(&'a mut self, url: &'a str) -> Pin<Box<dyn Future<Output = Option<String>> + 'a>> {
        info!("context loader load {}", url);
        let result = self
            .world_sources
            .get(url)
            .or_else(|| self.fetched.get(url))
            .cloned();
        if result.is_none() {
            self.missing.push(url.to_string());
        }
        Box::pin(std::future::ready(result))
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

#[instrument(skip(
    query,
    all_sources,
    prev_infos,
    context_cache,
    sender,
    client,
    commands
))]
fn parse_jsonld_system<C: Client + Resource + Clone>(
    query: Query<(Entity, &Source, &Label), (Changed<Source>, With<JsonLdLang>)>,
    all_sources: Query<(&Label, &Source)>,
    mut commands: Commands,
    mut prev_infos: Local<HashMap<String, PrevParseInfo>>,
    context_cache: Res<ContextCache>,
    sender: Res<CommandSender>,
    client: Res<C>,
) {
    // Snapshot of all currently-open documents for context resolution.
    let world_sources: HashMap<String, String> = all_sources
        .iter()
        .map(|(l, s)| (l.to_string(), s.0.clone()))
        .collect();

    for (entity, source, label) in &query {
        let prev = prev_infos.get(label.as_str());
        let (parse, new_prev) = rdf_parsers::parse_incremental(
            Rule::new(SyntaxKind::JsonldDoc),
            source.0.as_str(),
            prev,
            IncrementalBias::default(),
        );
        prev_infos.insert(label.to_string(), new_prev);

        let syntax = parse.syntax::<Lang>();
        let errors = collect_errors(&syntax);
        let cst_tokens = extract_jsonld_cst_tokens(&syntax);

        let mut loader = WorldContextLoader {
            world_sources: &world_sources,
            fetched: &context_cache.0,
            missing: Vec::new(),
        };

        let (jsonld_model, ctx) = poll_sync(convert_with_loader(
            &syntax,
            &mut loader,
            Some(label.0.to_string()),
        ));

        info!(
            "{} triples ({} parse errors)",
            jsonld_model.triples.len(),
            errors.len()
        );

        info!("active context {:?}", ctx);
        for p in &jsonld_model.prefixes {
            info!("prefix {:?}", p.value());
        }

        let span = 0..source.0.len();
        let element = Element::<JsonLdLang>(spanned(jsonld_model, span));

        if errors.is_empty() {
            commands
                .entity(entity)
                .insert((
                    element,
                    Errors(errors),
                    CstTokens(cst_tokens),
                    JsonLdActiveContext(ctx),
                    Wrapped(parse.green_node),
                ))
                .remove::<Dirty>();
        } else {
            commands.entity(entity).insert((
                element,
                Errors(errors),
                CstTokens(cst_tokens),
                JsonLdActiveContext(ctx),
                Wrapped(parse.green_node),
                Dirty,
            ));
        }

        // For any @context URLs not yet available, fetch them asynchronously.
        // When a fetch succeeds the source is re-inserted to trigger a re-parse.
        for missing_url in loader.missing {
            let sender = sender.0.clone();
            let client_clone = client.as_ref().clone();
            let source_text = source.0.clone();
            client.spawn(async move {
                if let Ok(resp) = client_clone.fetch(&missing_url, &HashMap::new()).await {
                    let mut cq = CommandQueue::default();
                    cq.push(move |world: &mut World| {
                        world
                            .resource_mut::<ContextCache>()
                            .0
                            .insert(missing_url, resp.body);
                        if let Ok(mut em) = world.get_entity_mut(entity) {
                            em.insert(Source(source_text));
                        }
                    });
                    let _ = sender.unbounded_send(cq);
                }
            });
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
