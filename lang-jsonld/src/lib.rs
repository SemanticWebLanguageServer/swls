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
    components::registry::{resolve_iri_to_path, ComponentRegistry},
    module_state::ModuleState,
};
use oxigraph::model::{GraphName, Literal, NamedNode, Quad};
use swls_core::{
    lang::{Lang, LangHelper},
    lsp_types::SemanticTokenType,
    prelude::{goto_definition::GotoDefinitionRequest, *},
    util::resolve_iri,
    Startup,
};
use swls_lang_rdf_base::register_rdf_lang;
use swls_lang_turtle::lang::parser::TurtleParseError;

pub mod ecs;
use crate::{
    ecs::{
        derive_jsonld_triples, format_jsonld_system, setup_completion, setup_parsing, ContextCache,
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

    world.schedule_scope(Startup, |_, schedule| {
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

fn goto_cjs(
    mut query: Query<
        (
            &TokenComponent,
            Option<&TripleComponent>,
            &Label,
            &mut GotoDefinitionRequest,
        ),
        With<JsonLdLang>,
    >,
    res: Res<Registry>,
) {
    use swls_core::lsp_types::{Location, Range};

    for (token, triple, label, mut req) in &mut query {
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
        let st: &str = triple_term_str
            .as_deref()
            .unwrap_or_else(|| token.text.as_str().trim_matches('"'));

        tracing::info!(
            "Goto definition {:?} {} {:?} {:?}",
            triple_term_str,
            st,
            token,
            triple
        );

        // Components: navigate to the component's own source file at the exact @id span.
        if let Some(component) = res.0.components.get(st) {
            tracing::info!("Component from {}", component.source_file);

            if let Ok(uri) = swls_core::lsp_types::Url::from_file_path(std::path::Path::new(
                &component.source_file,
            )) {
                let range = res
                    .0
                    .file_sources
                    .get(&component.source_file)
                    .map(|src| span_to_lsp_range(src, &component.iri_span))
                    .unwrap_or_default();
                req.0.push(Location { uri, range });
                continue;
            }
        }

        // Modules: navigate to the module source file at the exact @id span.
        if let Some(module) = res.0.modules.get(st) {
            tracing::info!("Module from {}", module.source_file);
            if let Ok(uri) =
                swls_core::lsp_types::Url::from_file_path(std::path::Path::new(&module.source_file))
            {
                let range = res
                    .0
                    .file_sources
                    .get(&module.source_file)
                    .map(|src| span_to_lsp_range(src, &module.iri_span))
                    .unwrap_or_default();
                req.0.push(Location { uri, range });
                continue;
            }
        }

        // Parameters: navigate to the parameter's defining file at the exact @id span.
        if let Some((source_file, iri_span)) = res.0.parameters.get(st) {
            tracing::info!("Parameter from {}", source_file);
            if let Ok(uri) =
                swls_core::lsp_types::Url::from_file_path(std::path::Path::new(source_file))
            {
                let range = res
                    .0
                    .file_sources
                    .get(source_file)
                    .map(|src| span_to_lsp_range(src, iri_span))
                    .unwrap_or_default();
                req.0.push(Location { uri, range });
                continue;
            }
        }

        if triple_term_str.is_none() {
            // Import IRIs: strip any fragment, then resolve to a local file path.
            let iri_no_fragment = st.split('#').next().unwrap_or(st);
            if let Some(path) = resolve_iri_to_path(iri_no_fragment, &res.1.import_paths) {
                tracing::debug!("goto_cjs import IRI resolved to {:?}", path);
                if let Ok(uri) = swls_core::lsp_types::Url::from_file_path(&path) {
                    req.0.push(Location {
                        uri,
                        range: Range::default(),
                    });
                    continue;
                }
            }

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
    use std::path::{Path, PathBuf};

    use components_rs::error::{ComponentsJsError, Result};
    use swls_core::prelude::Fs;

    use crate::Registry;

    pub struct LocalFs(Fs);

    #[async_trait::async_trait]
    impl components_rs::fs::Fs for LocalFs {
        /// Read the entire contents of a file as a UTF-8 string.
        async fn read_to_string(&self, path: &Path) -> Result<String> {
            if let Ok(url) = swls_core::lsp_types::Url::from_file_path(path) {
                self.0
                     .0
                    .read_file(&url)
                    .await
                    .ok_or(ComponentsJsError::General(format!(
                        "Failed to read file {}",
                        url.as_str()
                    )))
            } else {
                Result::Err(ComponentsJsError::General(
                    "Failed to convert to url".to_string(),
                ))
            }
        }

        /// List the immediate children of a directory.
        async fn read_dir(&self, path: &Path) -> Result<Vec<components_rs::fs::FsDirEntry>> {
            let entries = self
                .0
                 .0
                .read_dir(path)
                .await
                .ok_or(ComponentsJsError::General(format!(
                    "Failed to read dir {:?}",
                    path.as_os_str()
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
        async fn is_file(&self, path: &Path) -> bool {
            self.0 .0.is_file(path).await
        }

        /// Check whether `path` is a directory.
        async fn is_dir(&self, path: &Path) -> bool {
            self.0 .0.is_dir(path).await
        }

        /// Resolve symlinks and produce the canonical, absolute path.
        /// In environments without symlinks (e.g., WASM), returning the
        /// input path unchanged is acceptable.
        async fn canonicalize(&self, path: &Path) -> Result<PathBuf> {
            self.0
                 .0
                .canonicalize(path)
                .await
                .ok_or(ComponentsJsError::General(
                    "Failed to canonicalize".to_string(),
                ))
        }
    }

    pub async fn start_testing(fs: &Fs, path: &Path) -> Result<Registry> {
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
        Self(ComponentRegistry::new(), ModuleState::default())
    }
}

#[tracing::instrument(skip(fs, client, config))]
fn start_jsonld<C: Client + Resource + Clone>(
    fs: Res<Fs>,
    client: Res<C>,
    config: Res<ServerConfig>,
    commands: Res<CommandSender>,
) {
    let fs = fs.clone();
    tracing::info!("conffig {:?}", config);
    if let Some(ws) = config
        .workspaces
        .first()
        .and_then(|x| x.uri.to_file_path().ok())
    {
        tracing::info!("HERE 2 {:?}", ws.as_os_str());
        let commands = commands.clone();
        let thing = async move {
            tracing::info!("HERE 3 {:?}", ws.as_os_str());
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
