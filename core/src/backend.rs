use std::collections::HashMap;

use crate::text::LineIndex;
use bevy_ecs::{
    bundle::Bundle,
    component::Component,
    entity::Entity,
    query::With,
    schedule::ScheduleLabel,
    world::{CommandQueue, World},
};
use completion::CompletionRequest;
use goto_type::GotoTypeRequest;
use references::ReferencesRequest;
use request::{GotoTypeDefinitionParams, GotoTypeDefinitionResponse};
use tracing::{debug, error, info, instrument, warn};

use crate::{
    client::Client,
    feature::{
        code_action::{CodeActionRequest, Label as CodeActionLabel},
        goto_definition::GotoDefinitionRequest,
    },
    lsp_types::{request::SemanticTokensRefresh, *},
    prelude::*,
    Started, Startup,
};

/// Error returned by [`Backend`] request handlers.
///
/// No handler currently fails at the protocol level, so this type is uninhabited.
/// It exists so handlers can return a transport-agnostic [`Result`] that front-ends
/// map onto their own error type (e.g. `tower_lsp::jsonrpc::Error`) without `swls-core`
/// depending on any transport crate.
#[derive(Debug)]
pub enum ServerError {}

/// Transport-agnostic result type for [`Backend`] request handlers.
pub type Result<T> = std::result::Result<T, ServerError>;

/// Transport-agnostic LSP request handler.
///
/// `Backend` exposes inherent `async` methods mirroring the LSP requests. Front-ends
/// (the `swls` binary's `tower_lsp` adapter, or a future wasm dispatcher) call these
/// directly. It is generic over the [`Client`] used to send server-to-client messages.
pub struct Backend<C> {
    sender: CommandSender,
    client: C,
    semantic_tokens: Vec<SemanticTokenType>,
    /// Whether the client lets the server register file watchers.
    watch_files: std::sync::atomic::AtomicBool,
}

/// Files the server asks the client to watch: the RDF files it may have
/// loaded in the background (workspace index, `owl:imports`, configuration).
const WATCHED_FILES: &str = "**/*.{ttl,nt,owl,trig,n3}";

impl<C> std::fmt::Debug for Backend<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Backend")
            .field("semantic_tokens", &self.semantic_tokens)
            .finish_non_exhaustive()
    }
}

impl<C: Client> Backend<C> {
    pub fn new(sender: CommandSender, client: C, tokens: Vec<SemanticTokenType>) -> Self {
        Self {
            sender,
            client,
            semantic_tokens: tokens,
            watch_files: std::sync::atomic::AtomicBool::new(false),
        }
    }

    /// `config` combined with the global and the workspace config files.
    async fn with_config_files(&self, mut config: Config, root: Option<&Url>) -> Config {
        // Without a file system (the world is gone) there are no config files
        // to read; keep the options the client sent.
        if let Some(fs) = self.run(|w| w.resource::<Fs>().clone()).await {
            if let Some(global) = LocalConfig::global(&fs).await {
                config.local.combine(global);
            }
            if let Some(root) = root {
                if let Some(local) = LocalConfig::local(&fs, root).await {
                    config.local.combine(local);
                }
            }
        }
        config
    }

    async fn run<T: Send + Sync + 'static>(
        &self,
        f: impl FnOnce(&mut World) -> T + Send + Sync + 'static,
    ) -> Option<T> {
        let (tx, rx) = futures::channel::oneshot::channel();
        let mut commands = CommandQueue::default();
        commands.push(move |world: &mut World| {
            let o = f(world);
            if tx.send(o).is_err() {
                error!("Failed to run schedule for {}", stringify!(T));
            };
        });

        if let Err(e) = self.sender.0.unbounded_send(commands) {
            error!("Failed to send commands {}", e);
            return None;
        }

        rx.await.ok()
    }

    /// Run `schedule` for the document at `url` with the request component(s)
    /// `param`, and take the response component `T`.
    ///
    /// The document is looked up inside the world, in the same step that runs
    /// the schedule, so a document closed in between cannot be used after it
    /// is gone. `None` when there is no such document.
    async fn run_schedule<T: Component>(
        &self,
        url: Url,
        schedule: impl ScheduleLabel + Clone,
        param: impl Bundle,
    ) -> Option<T> {
        let (tx, rx) = futures::channel::oneshot::channel();

        let mut commands = CommandQueue::default();
        commands.push(move |world: &mut World| {
            let Some(entity) = find_document(world, &url) else {
                debug!("No document {}", url);
                let _ = tx.send(None);
                return;
            };
            world.entity_mut(entity).insert(param);
            world.run_schedule(schedule.clone());
            if tx.send(world.entity_mut(entity).take::<T>()).is_err() {
                error!(name: "Failed to run schedule", "Failed to run schedule {:?}", schedule);
            };
        });

        if let Err(e) = self.sender.0.unbounded_send(commands) {
            error!("Failed to send commands {}", e);
            return None;
        }

        rx.await.unwrap_or_default()
    }

    /// The document a request is about. Requests refer to documents by URL;
    /// the entity is looked up when the request runs (see [`Self::run_schedule`]).
    async fn document(&self, uri: &str) -> Option<Url> {
        Url::parse(uri).ok()
    }

    /// Whether the given LSP feature has been disabled via [`ServerConfig`].
    async fn is_disabled(&self, feature: Disabled) -> bool {
        self.run(move |world| {
            world
                .resource::<ServerConfig>()
                .config
                .local
                .is_disabled(feature)
        })
        .await
        .unwrap_or(false)
    }
}

impl<C: Client> Backend<C> {
    #[instrument(skip(self, init))]
    pub async fn initialize(&self, init: InitializeParams) -> Result<InitializeResult> {
        info!("Initialize");

        // Clients that only send a root get that root as their workspace.
        let workspaces = init
            .workspace_folders
            .clone()
            .filter(|w| !w.is_empty())
            .or_else(|| {
                init.root_uri.clone().map(|uri| {
                    vec![WorkspaceFolder {
                        name: uri.path().to_string(),
                        uri,
                    }]
                })
            })
            .unwrap_or_default();
        let config: Config = match init.initialization_options.clone() {
            None | Some(serde_json::Value::Null) => Config::default(),
            Some(options) => match serde_json::from_value(options) {
                Ok(config) => config,
                Err(e) => {
                    // A single wrong value would otherwise reset every setting
                    // without a trace.
                    let msg = format!("Invalid swls initializationOptions, using defaults: {e}");
                    warn!("{msg}");
                    self.client.log_message(MessageType::WARNING, msg).await;
                    Config::default()
                }
            },
        };

        let config = self.with_config_files(config, init.root_uri.as_ref()).await;
        let server_config = ServerConfig { config, workspaces };

        let watch_files = init
            .capabilities
            .workspace
            .as_ref()
            .and_then(|w| w.did_change_watched_files.as_ref())
            .and_then(|w| w.dynamic_registration)
            .unwrap_or(false);
        self.watch_files
            .store(watch_files, std::sync::atomic::Ordering::Relaxed);

        info!("Initialize {:?}", server_config);
        let document_selectors: Vec<_> = [
            ("sparql", server_config.config.sparql.unwrap_or(true)),
            ("turtle", server_config.config.turtle.unwrap_or(true)),
            ("trig", server_config.config.trig.unwrap_or(true)),
            ("n3", server_config.config.n3.unwrap_or(false)),
            ("jsonld", server_config.config.jsonld.unwrap_or(true)),
        ]
        .into_iter()
        .filter(|(_, x)| *x)
        .map(|(x, _)| DocumentFilter {
            language: Some(String::from(x)),
            scheme: None,
            pattern: None,
        })
        .collect();

        let disabled = server_config.config.local.disabled.clone();
        let is_enabled = |d: Disabled| !disabled.contains(&d);

        self.run(|world| {
            world.insert_resource(server_config);
            world.run_schedule(Startup);
        })
        .await;

        // let triggers = L::TRIGGERS.iter().copied().map(String::from).collect();
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                inlay_hint_provider: is_enabled(Disabled::InlayHint).then_some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                code_action_provider: is_enabled(Disabled::CodeAction)
                    .then_some(CodeActionProviderCapability::Simple(true)),
                completion_provider: is_enabled(Disabled::Completion).then_some(
                    CompletionOptions {
                        resolve_provider: Some(false),
                        trigger_characters: Some(vec![String::from(":")]),
                        work_done_progress_options: Default::default(),
                        all_commit_characters: None,
                        completion_item: None,
                    },
                ),
                // implementation_provider: Some(ImplementationProviderCapability::Simple(true)),
                type_definition_provider: is_enabled(Disabled::GotoTypeDefinition)
                    .then_some(TypeDefinitionProviderCapability::Simple(true)),
                references_provider: is_enabled(Disabled::References).then_some(OneOf::Left(true)),
                hover_provider: is_enabled(Disabled::Hover)
                    .then_some(HoverProviderCapability::Simple(true)),
                definition_provider: is_enabled(Disabled::GotoDefinition)
                    .then_some(OneOf::Left(true)),
                document_formatting_provider: is_enabled(Disabled::Format)
                    .then_some(OneOf::Left(true)),
                document_on_type_formatting_provider: is_enabled(Disabled::PrefixAutoInsert)
                    .then_some(DocumentOnTypeFormattingOptions {
                        first_trigger_character: ":".to_string(),
                        more_trigger_character: None,
                    }),
                semantic_tokens_provider: is_enabled(Disabled::SemanticTokens).then_some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(document_selectors),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: self.semantic_tokens.clone(),
                                    token_modifiers: vec![],
                                },
                                range: Some(false),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                rename_provider: is_enabled(Disabled::Rename).then_some(OneOf::Right(
                    RenameOptions {
                        prepare_provider: Some(true),
                        work_done_progress_options: Default::default(),
                    },
                )),
                document_link_provider: is_enabled(Disabled::DocumentLink).then_some(
                    DocumentLinkOptions {
                        resolve_provider: Some(false),
                        work_done_progress_options: Default::default(),
                    },
                ),
                document_symbol_provider: is_enabled(Disabled::DocumentSymbol)
                    .then_some(OneOf::Left(true)),
                folding_range_provider: is_enabled(Disabled::FoldingRange)
                    .then_some(FoldingRangeProviderCapability::Simple(true)),
                workspace_symbol_provider: is_enabled(Disabled::WorkspaceSymbol)
                    .then_some(OneOf::Left(true)),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec![crate::systems::ALLOW_PROPERTY_COMMAND.to_string()],
                    work_done_progress_options: Default::default(),
                }),
                ..ServerCapabilities::default()
            },
        })
    }

    pub async fn initialized(&self, _params: InitializedParams) {
        self.run(|world| {
            tracing::info!("initialized");
            world.run_schedule(Started);
        })
        .await;

        // Ask the client to report changes to RDF files on disk, so background
        // documents (workspace files, imports) stay current.
        if self.watch_files.load(std::sync::atomic::Ordering::Relaxed) {
            let options = DidChangeWatchedFilesRegistrationOptions {
                watchers: vec![FileSystemWatcher {
                    glob_pattern: GlobPattern::String(WATCHED_FILES.to_string()),
                    kind: None,
                }],
            };
            let registration = Registration {
                id: String::from("swls-watched-files"),
                method: String::from("workspace/didChangeWatchedFiles"),
                register_options: serde_json::to_value(options).ok(),
            };
            let _ = self
                .client
                .send_request::<request::RegisterCapability>(RegistrationParams {
                    registrations: vec![registration],
                })
                .await;
        }
    }

    /// Files changed on disk. Background documents are reloaded (or dropped
    /// when deleted), and new RDF files in the workspace are indexed. A file
    /// open in the editor keeps the editor's text.
    #[instrument(skip(self, params))]
    pub async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        enum Action {
            Reload(Entity),
            Load,
            Nothing,
        }
        let Some(fs) = self.run(|w| w.resource::<Fs>().clone()).await else {
            return;
        };
        for change in params.changes {
            let url = change.uri;
            if change.typ == FileChangeType::DELETED {
                let url = url.clone();
                self.run(move |world| crate::document::remove_background(world, &url))
                    .await;
                continue;
            }
            let action = {
                let url = url.clone();
                self.run(move |world| match find_document(world, &url) {
                    Some(e) if world.entity(e).contains::<Open>() => Action::Nothing,
                    Some(e) => Action::Reload(e),
                    None => {
                        let config = world.resource::<ServerConfig>();
                        let index = !config.config.local.is_disabled(Disabled::WorkspaceIndex)
                            && crate::workspace::is_workspace_file(&url)
                            && crate::workspace::in_workspace(config, &url);
                        if index {
                            Action::Load
                        } else {
                            Action::Nothing
                        }
                    }
                })
                .await
                .unwrap_or(Action::Nothing)
            };
            match action {
                Action::Reload(entity) => {
                    if let Some(content) = fs.0.read_file(&url).await {
                        self.run(move |world| {
                            crate::document::restore_background(world, entity, content)
                        })
                        .await;
                    }
                }
                Action::Load => {
                    crate::workspace::load_workspace_file(&fs, url, &self.sender).await;
                }
                Action::Nothing => {}
            }
        }
    }

    /// The client's settings changed: apply them like `initializationOptions`
    /// (combined with the config files), load newly configured ontologies and
    /// shapes, and re-check open documents. Capabilities announced at
    /// `initialize` cannot change.
    #[instrument(skip(self, params))]
    pub async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        // Clients send either the settings themselves or `{ "swls": … }`.
        let settings = match params.settings {
            serde_json::Value::Object(mut map) if map.contains_key("swls") => {
                map.remove("swls").unwrap_or_default()
            }
            other => other,
        };
        if settings.is_null() {
            return;
        }
        let config: Config = match serde_json::from_value(settings) {
            Ok(config) => config,
            Err(e) => {
                let msg = format!("Invalid swls settings, keeping the current ones: {e}");
                warn!("{msg}");
                self.client.log_message(MessageType::WARNING, msg).await;
                return;
            }
        };
        let root = self
            .run(|w| {
                w.resource::<ServerConfig>()
                    .workspaces
                    .first()
                    .map(|w| w.uri.clone())
            })
            .await
            .flatten();
        let config = self.with_config_files(config, root.as_ref()).await;

        self.run(move |world| {
            world.resource_mut::<ServerConfig>().config = config;
            world.run_schedule(crate::ConfigChanged);
            // Re-check every open document with the new settings.
            let open: Vec<Entity> = world
                .query_filtered::<Entity, With<Open>>()
                .iter(world)
                .collect();
            for e in open {
                if let Some(mut source) = world.get_mut::<Source>(e) {
                    bevy_ecs::change_detection::DetectChangesMut::set_changed(&mut source);
                }
            }
            world.run_schedule(ParseLabel);
            world.flush();
        })
        .await;
    }

    pub async fn did_change_workspace_folders(
        &self,
        params: DidChangeWorkspaceFoldersParams,
    ) -> () {
        self.run(move |world| {
            let mut config = world.resource_mut::<ServerConfig>();
            let WorkspaceFoldersChangeEvent { added, removed } = params.event;

            for r in removed {
                if let Some(idx) = config.workspaces.iter().position(|x| x == &r) {
                    config.workspaces.remove(idx);
                }
            }

            // This is nice and all, but we don't bubble this event up in the world
            config.workspaces.extend(added);
        })
        .await;
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    pub async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        debug!("semantic tokens full");
        if self.is_disabled(Disabled::SemanticTokens).await {
            return Ok(None);
        }
        let uri = params.text_document.uri.as_str();
        let Some(doc) = self.document(uri).await else {
            debug!("No document {}", uri);
            return Ok(None);
        };

        if let Some(res) = self
            .run_schedule::<HighlightRequest>(doc, SemanticLabel, HighlightRequest(vec![]))
            .await
        {
            Ok(Some(SemanticTokensResult::Tokens(
                crate::lsp_types::SemanticTokens {
                    result_id: None,
                    data: res.0,
                },
            )))
        } else {
            debug!("resulting in no tokens");
            Ok(None)
        }
    }

    #[instrument(skip(self))]
    pub async fn shutdown(&self) -> Result<()> {
        info!("Shutting down!");

        Ok(())
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document_position.text_document.uri.as_str()))]
    pub async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        if self.is_disabled(Disabled::References).await {
            return Ok(None);
        }
        let Some(doc) = self
            .document(params.text_document_position.text_document.uri.as_str())
            .await
        else {
            return Ok(None);
        };

        let pos = params.text_document_position.position;

        let arr = self
            .run_schedule::<ReferencesRequest>(
                doc,
                ReferencesLabel,
                (PositionComponent(pos), ReferencesRequest(Vec::new())),
            )
            .await
            .map(|x| x.0);

        Ok(arr)
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    pub async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        if self.is_disabled(Disabled::Rename).await {
            return Ok(None);
        }
        let Some(doc) = self.document(params.text_document.uri.as_str()).await else {
            return Ok(None);
        };

        let pos = params.position;

        let resp = self
            .run_schedule::<PrepareRenameRequest>(doc, PrepareRenameLabel, PositionComponent(pos))
            .await
            .map(|x| PrepareRenameResponse::RangeWithPlaceholder {
                range: x.range,
                placeholder: x.placeholder,
            });

        Ok(resp)
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document_position.text_document.uri.as_str()))]
    pub async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        if self.is_disabled(Disabled::Rename).await {
            return Ok(Some(WorkspaceEdit::new(HashMap::new())));
        }
        let Some(doc) = self
            .document(params.text_document_position.text_document.uri.as_str())
            .await
        else {
            return Ok(None);
        };

        let pos = params.text_document_position.position;

        let mut change_map: HashMap<crate::lsp_types::Url, Vec<TextEdit>> = HashMap::new();
        if let Some(changes) = self
            .run_schedule::<RenameEdits>(
                doc,
                RenameLabel,
                (
                    PositionComponent(pos),
                    RenameEdits(Vec::new(), params.new_name),
                ),
            )
            .await
        {
            for (url, change) in changes.0 {
                let entry = change_map.entry(url);
                entry.or_default().push(change);
            }
        }
        Ok(Some(WorkspaceEdit::new(change_map)))
    }

    pub async fn hover(&self, params: HoverParams) -> Result<Option<crate::lsp_types::Hover>> {
        if self.is_disabled(Disabled::Hover).await {
            return Ok(None);
        }
        let request: HoverRequest = HoverRequest::default();

        let Some(doc) = self
            .document(
                params
                    .text_document_position_params
                    .text_document
                    .uri
                    .as_str(),
            )
            .await
        else {
            return Ok(None);
        };

        let pos = params.text_document_position_params.position;

        if let Some(hover) = self
            .run_schedule::<HoverRequest>(doc, HoverLabel, (request, PositionComponent(pos)))
            .await
        {
            tracing::debug!("hover returned {} items", hover.0.len());
            if !hover.0.is_empty() {
                return Ok(Some(crate::lsp_types::Hover {
                    contents: crate::lsp_types::HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: hover.0.join("\n\n---\n\n"),
                    }),
                    range: hover.1,
                }));
            }
        }

        Ok(None)
    }

    pub async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        debug!("Inlay hints called");
        if self.is_disabled(Disabled::InlayHint).await {
            return Ok(None);
        }
        let uri = params.text_document.uri.as_str();
        let Some(doc) = self.document(uri).await else {
            debug!("No document {}", uri);
            return Ok(None);
        };

        let request = self
            .run_schedule::<InlayRequest>(doc, InlayLabel, InlayRequest::default())
            .await;

        debug!(
            "Inlay hints resolved {} hints",
            request.as_ref().map(|x| x.0.len()).unwrap_or(0)
        );

        Ok(request.map(|x| x.0))
    }

    #[instrument(skip(self))]
    pub async fn formatting(
        &self,
        params: DocumentFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        if self.is_disabled(Disabled::Format).await {
            return Ok(None);
        }
        let uri = params.text_document.uri.as_str();
        let Some(doc) = self.document(uri).await else {
            debug!("No document {}", uri);
            return Ok(None);
        };

        let request = self
            .run_schedule::<FormatRequest>(doc, FormatLabel, FormatRequest(None))
            .await;
        Ok(request.and_then(|x| x.0))
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document_position.text_document.uri.as_str()))]
    pub async fn on_type_formatting(
        &self,
        params: DocumentOnTypeFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        if self.is_disabled(Disabled::PrefixAutoInsert).await {
            return Ok(None);
        }
        let uri = params.text_document_position.text_document.uri.as_str();
        let Some(doc) = self.document(uri).await else {
            return Ok(None);
        };

        // The exact position after the typed character: the scan expects the
        // cursor right after `:`.
        let pos = params.text_document_position.position;
        let request = self
            .run_schedule::<on_type_format::OnTypeFormatRequest>(
                doc,
                OnTypeFormatLabel,
                (
                    PositionComponent(pos),
                    on_type_format::OnTypeFormatRequest(None),
                ),
            )
            .await;
        Ok(request.and_then(|x| x.0))
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    pub async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let item = params.text_document;
        let url = item.uri.as_str().to_string();

        self.run(|world| {
            crate::document::open_document(world, item);
        })
        .await;
        debug!("Opened {}", url);

        debug!("Requesting tokens refresh");
        let _ = self.client.send_request::<SemanticTokensRefresh>(()).await;
        debug!("Semantic tokens refresh");
    }

    /// The editor closed a document.
    ///
    /// A document the server also loads by itself (e.g. a cached ontology the
    /// user opened through goto-definition) gets the server's copy back, so
    /// unsaved edits never stick. See [`crate::document`].
    #[instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    pub async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let url = params.text_document.uri;

        let closed = {
            let url = url.clone();
            self.run(move |world| crate::document::close_document(world, &url))
                .await
        };

        if let Some(crate::document::Closed::Background { entity, snapshot }) = closed {
            let fs = self.run(|w| w.resource::<Fs>().clone()).await;
            let on_disk = match fs {
                Some(fs) => fs.0.read_file(&url).await,
                None => None,
            };
            if let Some(content) = on_disk.or(snapshot) {
                self.run(move |world| crate::document::restore_background(world, entity, content))
                    .await;
            }
        }
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    pub async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let Some(doc) = self.document(params.text_document.uri.as_str()).await else {
            debug!("No document {}", params.text_document.uri.as_str());
            return;
        };

        let change = {
            if let Some(c) = params.content_changes.into_iter().next() {
                c
            } else {
                return;
            }
        };

        self.run(move |world| {
            let Some(entity) = find_document(world, &doc) else {
                debug!("No document {}", doc);
                return;
            };
            let rope_c = RopeC(LineIndex::new(&change.text));
            world
                .entity_mut(entity)
                .insert((Source(change.text), rope_c));
            world.run_schedule(ParseLabel);
            world.flush();
        })
        .await;
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    pub async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let url = params.text_document.uri;

        self.run(move |world| {
            // A saved workspace file now feeds the other documents.
            crate::document::save_document(world, &url);
            world.run_schedule(SaveLabel);

            debug!("Ran OnSave Schedule");
        })
        .await;
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document_position_params.text_document.uri.as_str()))]
    pub async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        if self.is_disabled(Disabled::GotoDefinition).await {
            return Ok(None);
        }
        let Some(doc) = self
            .document(
                params
                    .text_document_position_params
                    .text_document
                    .uri
                    .as_str(),
            )
            .await
        else {
            return Ok(None);
        };

        let pos = params.text_document_position_params.position;

        let arr = self
            .run_schedule::<GotoDefinitionRequest>(
                doc,
                GotoDefinitionLabel,
                (PositionComponent(pos), GotoDefinitionRequest(Vec::new())),
            )
            .await
            .map(|x| {
                tracing::debug!("goto definition: {} locations", x.0.len());
                GotoDefinitionResponse::Array(x.0)
            });

        Ok(arr)
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document_position_params.text_document.uri.as_str()))]
    pub async fn goto_type_definition(
        &self,
        params: GotoTypeDefinitionParams,
    ) -> Result<Option<GotoTypeDefinitionResponse>> {
        if self.is_disabled(Disabled::GotoTypeDefinition).await {
            return Ok(None);
        }
        let Some(doc) = self
            .document(
                params
                    .text_document_position_params
                    .text_document
                    .uri
                    .as_str(),
            )
            .await
        else {
            return Ok(None);
        };

        let pos = params.text_document_position_params.position;

        let arr = self
            .run_schedule::<GotoTypeRequest>(
                doc,
                GotoTypeLabel,
                (PositionComponent(pos), GotoTypeRequest(Vec::new())),
            )
            .await
            .map(|x| GotoTypeDefinitionResponse::Array(x.0));

        Ok(arr)
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    pub async fn code_action(
        &self,
        params: CodeActionParams,
    ) -> Result<Option<CodeActionResponse>> {
        if self.is_disabled(Disabled::CodeAction).await {
            return Ok(None);
        }
        let uri = params.text_document.uri.as_str();
        let Some(doc) = self.document(uri).await else {
            return Ok(None);
        };

        let pos = params.range.start;
        let request = self
            .run_schedule::<CodeActionRequest>(
                doc,
                CodeActionLabel,
                (CodeActionRequest::default(), PositionComponent(pos)),
            )
            .await;

        Ok(request.map(|r| {
            r.0.into_iter()
                .map(CodeActionOrCommand::CodeAction)
                .collect()
        }))
    }

    /// Handle `workspace/executeCommand`.
    ///
    /// Currently only `swls.allowProperty`: adds the given property IRI to the
    /// user's `allowed_properties`, both at runtime (clearing the warning) and on
    /// disk (so the choice survives restarts), then refreshes diagnostics.
    #[instrument(skip(self, params), fields(command = %params.command))]
    pub async fn execute_command(
        &self,
        params: ExecuteCommandParams,
    ) -> Result<Option<serde_json::Value>> {
        if params.command == crate::systems::ALLOW_PROPERTY_COMMAND {
            let Some(iri) = params
                .arguments
                .first()
                .and_then(|v| v.as_str())
                .map(String::from)
            else {
                return Ok(None);
            };

            // 1. Update the in-memory config so the warning clears immediately.
            self.run({
                let iri = iri.clone();
                move |world| {
                    world
                        .resource_mut::<ServerConfig>()
                        .config
                        .local
                        .allowed_properties
                        .insert(iri);
                }
            })
            .await;

            // 2. Persist to the global config file.
            if let Some(fs) = self.run(|w| w.resource::<Fs>().clone()).await {
                LocalConfig::persist_allowed_property(&fs, &iri).await;
            }

            // 3. Re-run diagnostics for all open documents.
            self.run(|world| {
                world.run_schedule(ParseLabel);
                world.flush();
            })
            .await;
        }

        Ok(None)
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document_position.text_document.uri.as_str()))]
    pub async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        if self.is_disabled(Disabled::Completion).await {
            return Ok(None);
        }
        let Some(doc) = self
            .document(params.text_document_position.text_document.uri.as_str())
            .await
        else {
            return Ok(None);
        };

        let pos = params.text_document_position.position;

        let completions: Option<Vec<crate::lsp_types::CompletionItem>> = self
            .run_schedule::<CompletionRequest>(
                doc,
                CompletionLabel,
                (CompletionRequest(vec![]), PositionComponent(pos)),
            )
            .await
            .map(|mut x| {
                crate::feature::completion::sort_completions(&mut x.0);
                x.0.into_iter().map(|x| x.into()).collect()
            });

        Ok(completions.map(CompletionResponse::Array))
    }

    /// `textDocument/documentLink`.
    pub async fn document_link(
        &self,
        params: DocumentLinkParams,
    ) -> Result<Option<Vec<DocumentLink>>> {
        if self.is_disabled(Disabled::DocumentLink).await {
            return Ok(None);
        }
        let url = params.text_document.uri;
        Ok(self
            .run(move |world| crate::navigation::document_links(world, &url))
            .await)
    }

    /// `textDocument/documentSymbol`.
    pub async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        if self.is_disabled(Disabled::DocumentSymbol).await {
            return Ok(None);
        }
        let url = params.text_document.uri;
        Ok(self
            .run(move |world| crate::navigation::document_symbols(world, &url))
            .await
            .map(DocumentSymbolResponse::Nested))
    }

    /// `textDocument/foldingRange`.
    pub async fn folding_range(
        &self,
        params: FoldingRangeParams,
    ) -> Result<Option<Vec<FoldingRange>>> {
        if self.is_disabled(Disabled::FoldingRange).await {
            return Ok(None);
        }
        let url = params.text_document.uri;
        Ok(self
            .run(move |world| crate::navigation::folding_ranges(world, &url))
            .await)
    }

    /// `workspace/symbol`.
    pub async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        if self.is_disabled(Disabled::WorkspaceSymbol).await {
            return Ok(None);
        }
        let query = params.query;
        Ok(self
            .run(move |world| crate::navigation::workspace_symbols(world, &query))
            .await)
    }
}
