#![doc(
    html_logo_url = "https://ajuvercr.github.io/semantic-web-lsp/assets/icons/favicon.png",
    html_favicon_url = "https://ajuvercr.github.io/semantic-web-lsp/assets/icons/favicon.ico"
)]
pub mod cjs;
use std::path::{Path, PathBuf};

use bevy_ecs::{
    component::Component,
    resource::Resource,
    schedule::IntoScheduleConfigs,
    system::{Commands, Query, Res},
    world::{CommandQueue, World},
};
use components_rs::{
    components::registry::{resolve_iri_to_path, ComponentRegistry},
    module_state::ModuleState,
};
use swls_core::{
    lang::{Lang, LangHelper},
    lsp_types::SemanticTokenType,
    prelude::{goto_definition::GotoDefinitionRequest, *},
    Started, Startup,
};
use swls_lang_rdf_base::register_rdf_lang;
use swls_lang_turtle::lang::parser::TurtleParseError;

pub mod ecs;
use crate::{
    ecs::{format_jsonld_system, setup_completion, setup_parsing, ContextCache},
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

fn goto_cjs(
    mut query: Query<(
        &TokenComponent,
        Option<&TripleComponent>,
        &mut GotoDefinitionRequest,
    )>,
    res: Res<Registry>,
) {
    use swls_core::lsp_types::{Location, Range};

    for (token, triple, mut req) in &mut query {
        let st = match triple.and_then(|x| x.term()) {
            Some(x) => x.as_str(),
            None => token.text.as_str().trim_matches('"'),
        };

        // Fall back to treating the token as a literal IRI.
        if let Some(path) = resolve_iri_to_path(st, &res.1.import_paths) {
            tracing::debug!("goto_cjs literal IRI resolved to {:?}", path);
            if let Ok(uri) = swls_core::lsp_types::Url::from_file_path(&path) {
                req.0.push(Location {
                    uri,
                    range: Range::default(),
                });
                continue;
            }
        }

        // Try modules and components by IRI.
        if let Some(module) = res.0.modules.get(st) {
            if let Ok(uri) =
                swls_core::lsp_types::Url::from_file_path(std::path::Path::new(&module.source_file))
            {
                req.0.push(Location {
                    uri,
                    range: Range::default(),
                });
                continue;
            }
        }

        if let Some(component) = res.0.components.get(st) {
            if let Some(module_iri) = &component.module_iri {
                if let Some(module) = res.0.modules.get(module_iri.as_str()) {
                    if let Ok(uri) = swls_core::lsp_types::Url::from_file_path(
                        std::path::Path::new(&module.source_file),
                    ) {
                        req.0.push(Location {
                            uri,
                            range: Range::default(),
                        });
                        continue;
                    }
                }
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
                    world.insert_resource(reg);
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
