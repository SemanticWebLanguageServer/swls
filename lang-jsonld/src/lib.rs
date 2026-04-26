#![doc(
    html_logo_url = "https://ajuvercr.github.io/semantic-web-lsp/assets/icons/favicon.png",
    html_favicon_url = "https://ajuvercr.github.io/semantic-web-lsp/assets/icons/favicon.ico"
)]
pub mod cjs;

use bevy_ecs::{
    component::Component,
    query::With,
    resource::Resource,
    schedule::IntoScheduleConfigs,
    system::{Query, Res, RunSystemOnce},
    world::{CommandQueue, World},
};
use components_rs::{
    components::registry::{resolve_iri_to_url, ComponentRegistry},
    module_state::ModuleState,
};
use oxigraph::model::{GraphName, Literal, NamedNode, Quad};
use swls_core::{
    lang::{Lang, LangHelper},
    lsp_types::{SemanticTokenType, Url},
    prelude::{goto_definition::GotoDefinitionRequest, *},
    util::resolve_iri,
    Started,
};
use swls_lang_rdf_base::register_rdf_lang;
use swls_lang_turtle::lang::parser::TurtleParseError;

pub mod ecs;
use crate::{
    ecs::{
        derive_jsonld_triples, format_jsonld_system, setup_completion, setup_parsing, ContextCache,
        JsonLdActiveContext,
    },
    fs::start_testing,
};

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

    fn default_position(&self) -> TripleTarget {
        TripleTarget::Predicate
    }

    fn unquote<'a>(&self, text: &'a str) -> &'a str {
        let s = text.strip_prefix('"').unwrap_or(text);
        s.strip_suffix('"').unwrap_or(s)
    }
    fn quote(&self, inp: &str) -> String {
        format!("\"{}\"", inp)
    }
    fn handles_prefix_completion(&self) -> bool {
        true
    }
}

pub fn setup_world<C: Client + ClientSync + Resource + Clone>(world: &mut World) {
    register_rdf_lang::<JsonLdLang, JsonLdHelper>(world, "jsonld", &[".jsonld"]);
    register_rdf_lang::<JsonLdLang, JsonLdHelper>(world, "json", &[".json"]);
    world.insert_resource(ContextCache::default());
    world.insert_resource(Registry::empty());
    setup_parsing::<C>(world);
    setup_completion(world);

    world.schedule_scope(FormatLabel, |_, schedule| {
        schedule.add_systems(format_jsonld_system);
    });

    world.schedule_scope(Started, |_, schedule| {
        schedule.add_systems((start_jsonld::<C>,));
    });

    world.schedule_scope(GotoDefinitionLabel, |_, schedule| {
        schedule.add_systems(goto_cjs.after(get_current_triple));
    });
}

/// Convert a byte offset to an LSP `Position` (line + character).
fn byte_offset_to_position(source: &str, offset: usize) -> swls_core::lsp_types::Position {
    let offset = offset.min(source.len());
    let before = &source[..offset];
    let line = before.matches('\n').count() as u32;
    let col = before.rfind('\n').map(|p| offset - p - 1).unwrap_or(offset) as u32;
    swls_core::lsp_types::Position::new(line, col)
}

/// Convert a byte-range span to an LSP `Range` using the file's source text.
fn span_to_lsp_range(source: &str, span: &std::ops::Range<usize>) -> swls_core::lsp_types::Range {
    swls_core::lsp_types::Range::new(
        byte_offset_to_position(source, span.start),
        byte_offset_to_position(source, span.end),
    )
}

/// Expand a JSON-LD compact IRI or bare term name using the document's active context.
///
/// Handles chains like `css:dist/...` → `npmd:@solid/...dist/...` → `https://...dist/...`
/// and bare term names like `BearerWebIdExtractor` → `css:dist/...BearerWebIdExtractor`.
fn expand_iri_with_context(
    active: &rdf_parsers::jsonld::convert::ActiveContext,
    value: &str,
) -> String {
    expand_iri_inner(active, value, 0)
}

fn expand_iri_inner(
    active: &rdf_parsers::jsonld::convert::ActiveContext,
    value: &str,
    depth: usize,
) -> String {
    if depth > 10 || value.is_empty() || value.starts_with('@') {
        return value.to_string();
    }
    // Already absolute — well-known schemes only, so we don't mistake `css:` for absolute.
    if value.starts_with("https://")
        || value.starts_with("http://")
        || value.starts_with("file://")
        || value.starts_with("urn:")
    {
        return value.to_string();
    }
    // Bare term lookup (e.g. "BearerWebIdExtractor")
    if let Some(def) = active.terms.get(value) {
        if let Some(iri) = &def.iri {
            if iri != value {
                return expand_iri_inner(active, iri, depth + 1);
            }
        }
    }
    // Compact IRI like "prefix:suffix"
    if let Some(colon_pos) = value.find(':') {
        if colon_pos > 0 {
            let prefix = &value[..colon_pos];
            let suffix = &value[colon_pos + 1..];
            if let Some(def) = active.terms.get(prefix) {
                if let Some(iri) = &def.iri {
                    let expanded_prefix = expand_iri_inner(active, iri, depth + 1);
                    return format!("{}{}", expanded_prefix, suffix);
                }
            }
        }
    }
    value.to_string()
}

#[tracing::instrument(skip(query, res))]
fn goto_cjs(
    mut query: Query<
        (
            &TokenComponent,
            Option<&TripleComponent>,
            &Label,
            &mut GotoDefinitionRequest,
            Option<&JsonLdActiveContext>,
        ),
        With<JsonLdLang>,
    >,
    res: Res<Registry>,
) {
    use swls_core::lsp_types::{Location, Range};

    for (token, triple, label, mut req, active_ctx) in &mut query {
        // Only use the expanded IRI from the TripleComponent if the cursor token
        // actually overlaps the matched term's span.  get_current_triple is lenient
        // and may fall back to a nearby triple (e.g. the first triple in the
        // document) when the cursor is on @context or other non-triple content.
        let triple_term_str = triple.and_then(|tc| {
            let term_span = match tc.target {
                TripleTarget::Subject => &tc.triple.subject.span,
                TripleTarget::Predicate => &tc.triple.predicate.span,
                TripleTarget::Object => &tc.triple.object.span,
                TripleTarget::Graph => return None,
            };
            let cursor = token.source_span.start;
            if term_span.start <= cursor && cursor <= term_span.end {
                tc.term().map(|t| t.as_str())
            } else {
                None
            }
        });
        let raw_token = token.text.as_str().trim_matches('"');
        // When no triple term is available (e.g. cursor in @context), expand compact
        // IRIs like `css:dist/...` using the document's active context so that
        // resolve_iri_to_url can match them against import_paths.
        let context_expanded = if triple_term_str.is_none() {
            active_ctx.map(|ctx| expand_iri_with_context(&ctx.0, raw_token))
        } else {
            None
        };
        let st: &str = triple_term_str
            .as_deref()
            .or(context_expanded.as_deref())
            .unwrap_or(raw_token);

        tracing::info!("Goto definition {:?} {}", triple_term_str, st,);

        // Components: navigate to the component's own source file at the exact @id span.
        let found_target = if let Some(component) = res.0.components.get(st) {
            Some((component.source_file.as_str(), component.iri_span.clone()))
        } else if let Some(module) = res.0.modules.get(st) {
            Some((module.source_file.as_str(), module.iri_span.clone()))
        } else if let Some((file, span)) = res.0.parameters.get(st) {
            Some((file.as_str(), span.clone()))
        } else {
            None
        };

        tracing::info!(
            "CJS from {:?} {:?}",
            found_target,
            resolve_iri_to_url(st, &res.1.import_paths),
        );
        if let Some((file, span)) = found_target {
            if let Ok(uri) = swls_core::lsp_types::Url::parse(file) {
                let range = res
                    .0
                    .file_sources
                    .get(file)
                    .map(|src| span_to_lsp_range(src, &span))
                    .unwrap_or_default();
                req.0.push(Location { uri, range });
                continue;
            }
        }

        let iri_no_fragment = st.split('#').next().unwrap_or(st);
        let resolved = resolve_iri_to_url(iri_no_fragment, &res.1.import_paths)
            .or_else(|| res.1.context_urls.get(iri_no_fragment).cloned());
        if let Some(t) = resolved {
            tracing::info!("target {}", t.as_str());
            req.0.push(Location {
                uri: t,
                range: Range::default(),
            });
            continue;
        }

        if triple_term_str.is_none() {
            // Import IRIs: strip any fragment, then resolve to a local file path.
            let target = resolve_iri(&label.as_str(), st);
            if let Ok(uri) = swls_core::lsp_types::Url::parse(&target) {
                req.0.push(Location {
                    uri,
                    range: Range::default(),
                });
                continue;
            }
        }

        tracing::debug!("goto_cjs: no definition found for '{}'", st);
    }
}

mod fs {
    use components_rs::{
        error::{ComponentsJsError, Result},
        fs::FsDirEntry,
    };
    use swls_core::{lsp_types::Url, prelude::Fs};

    use crate::Registry;

    pub struct LocalFs(Fs);

    #[async_trait::async_trait]
    impl components_rs::fs::Fs for LocalFs {
        /// Read the entire contents of a file as a UTF-8 string.
        async fn read_to_string(&self, url: &Url) -> Result<String> {
            self.0
                 .0
                .read_file(&url)
                .await
                .ok_or(ComponentsJsError::General(format!(
                    "Failed to read file {}",
                    url.as_str()
                )))
        }

        /// List the immediate children of a directory.
        async fn read_dir(&self, path: &Url) -> Result<Vec<components_rs::fs::FsDirEntry>> {
            let entries = self
                .0
                 .0
                .read_dir(path)
                .await
                .ok_or(ComponentsJsError::General(format!(
                    "Failed to read dir {:?}",
                    path.as_str()
                )))?;
            Ok(entries
                .into_iter()
                .map(|entry| components_rs::fs::FsDirEntry {
                    name: entry.name,
                    path: entry.path,
                    is_dir: entry.is_dir,
                })
                .collect())
        }

        /// Check whether `path` is a file.
        async fn is_file(&self, path: &Url) -> bool {
            self.0 .0.is_file(path).await
        }

        /// Check whether `path` is a directory.
        async fn is_dir(&self, path: &Url) -> bool {
            self.0 .0.is_dir(path).await
        }

        async fn glob(&self, base: &Url, pattern: &str) -> Result<Vec<FsDirEntry>> {
            let entries = self
                .0
                 .0
                .glob(base, pattern)
                .await
                .ok_or(ComponentsJsError::General(format!(
                    "Failed to read dir {:?} {}",
                    base.as_str(),
                    pattern
                )))?;
            Ok(entries
                .into_iter()
                .map(|entry| components_rs::fs::FsDirEntry {
                    name: entry.name,
                    path: entry.path,
                    is_dir: entry.is_dir,
                })
                .collect())
        }
    }

    pub async fn start_testing(fs: &Fs, path: &Url) -> Result<Registry> {
        use components_rs::components::registry::ComponentRegistry;
        use components_rs::module_state::ModuleState;

        let fs = LocalFs(fs.clone());
        let state = ModuleState::build(&fs, path).await?;

        let mut registry = ComponentRegistry::new();
        registry.register_available_modules(&fs, &state).await?;
        registry.finalize();

        Ok(Registry(registry, state))
    }
}

fn build_cjs_quads(registry: &ComponentRegistry) -> Vec<Quad> {
    let rdf_type = NamedNode::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#type");
    let rdfs_class = NamedNode::new_unchecked("http://www.w3.org/2000/01/rdf-schema#Class");
    let rdfs_subclass_of =
        NamedNode::new_unchecked("http://www.w3.org/2000/01/rdf-schema#subClassOf");
    let rdfs_comment = NamedNode::new_unchecked("http://www.w3.org/2000/01/rdf-schema#comment");
    let rdf_property =
        NamedNode::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#Property");
    let rdfs_domain = NamedNode::new_unchecked("http://www.w3.org/2000/01/rdf-schema#domain");
    let rdfs_range = NamedNode::new_unchecked("http://www.w3.org/2000/01/rdf-schema#range");

    let graph = GraphName::DefaultGraph;
    let mut quads = Vec::new();

    for comp in registry.components.values() {
        let iri = NamedNode::new_unchecked(&comp.iri);

        quads.push(Quad::new(
            iri.clone(),
            rdf_type.clone(),
            rdfs_class.clone(),
            graph.clone(),
        ));

        if let Some(comment) = &comp.comment {
            quads.push(Quad::new(
                iri.clone(),
                rdfs_comment.clone(),
                Literal::new_simple_literal(comment.as_str()),
                graph.clone(),
            ));
        }

        for parent in &comp.extends {
            let parent_node = NamedNode::new_unchecked(parent);
            quads.push(Quad::new(
                iri.clone(),
                rdfs_subclass_of.clone(),
                parent_node,
                graph.clone(),
            ));
        }

        for param in &comp.parameters {
            let param_iri = NamedNode::new_unchecked(&param.iri);

            quads.push(Quad::new(
                param_iri.clone(),
                rdf_type.clone(),
                rdf_property.clone(),
                graph.clone(),
            ));
            quads.push(Quad::new(
                param_iri.clone(),
                rdfs_domain.clone(),
                iri.clone(),
                graph.clone(),
            ));

            if let Some(range) = &param.range {
                let range_node = NamedNode::new_unchecked(range);
                quads.push(Quad::new(
                    param_iri.clone(),
                    rdfs_range.clone(),
                    range_node,
                    graph.clone(),
                ));
            }

            if let Some(comment) = &param.comment {
                quads.push(Quad::new(
                    param_iri.clone(),
                    rdfs_comment.clone(),
                    Literal::new_simple_literal(comment.as_str()),
                    graph.clone(),
                ));
            }
        }
    }

    quads
}

#[derive(Resource)]
pub struct Registry(pub ComponentRegistry, pub ModuleState);
impl Registry {
    pub fn empty() -> Self {
        Self(ComponentRegistry::new(), ModuleState::empty())
    }
}

#[tracing::instrument(skip(fs, client, config, commands))]
fn start_jsonld<C: Client + Resource + Clone>(
    fs: Res<Fs>,
    client: Res<C>,
    config: Res<ServerConfig>,
    commands: Res<CommandSender>,
) {
    let fs = fs.clone();
    tracing::info!("conffig {:?}", config);
    if let Some(ws) = config.workspaces.first().and_then(|x| {
        if x.uri.as_str().ends_with('/') {
            Some(x.uri.clone())
        } else {
            Url::parse(&format!("{}/", x.uri.as_str())).ok()
        }
    }) {
        tracing::info!("HERE 2 {:?}", ws.as_str());
        let commands = commands.clone();
        let thing = async move {
            tracing::info!("HERE 3 {:?}", ws.as_str());
            if let Ok(reg) = start_testing(&fs, &ws).await {
                let mut command_queue = CommandQueue::default();
                command_queue.push(move |world: &mut World| {
                    let quads = build_cjs_quads(&reg.0);
                    world.insert_resource(reg);

                    let store_clone = world.get_resource::<swls_core::store::Store>();
                    //
                    if let Some(store) = store_clone {
                        tracing::info!("Derive store found adding {} triples", quads.len());
                        let mut loader = store.0.bulk_loader();
                        let _ = loader.load_quads(quads.into_iter());
                        let _ = loader.commit();
                    }

                    let _ = world.run_system_once(derive_jsonld_triples::<C>);
                });
                let _ = commands.unbounded_send(command_queue);
            }
            ()
        };
        client.spawn(thing);
    } else {
        tracing::info!("Restiry thing failed");
    }
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
