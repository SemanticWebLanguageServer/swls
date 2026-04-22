use std::{collections::HashMap, sync::Arc};

use bevy_ecs::{
    bundle::Bundle,
    component::Component,
    entity::Entity,
    schedule::ScheduleLabel,
    world::{CommandQueue, World},
};
use completion::CompletionRequest;
use futures::lock::Mutex;
use goto_type::GotoTypeRequest;
use references::ReferencesRequest;
use request::{GotoTypeDefinitionParams, GotoTypeDefinitionResponse};
use ropey::Rope;
use tower_lsp::{jsonrpc::Result, LanguageServer};
use tracing::{debug, error, info, instrument};

use crate::{
    feature::{
        code_action::{CodeActionRequest, Label as CodeActionLabel},
        goto_definition::GotoDefinitionRequest,
    },
    lsp_types::{request::SemanticTokensRefresh, *},
    prelude::*,
    Started, Startup,
};

#[derive(Debug)]
pub struct Backend {
    entities: Arc<Mutex<HashMap<String, Entity>>>,
    sender: CommandSender,
    #[allow(unused)]
    client: tower_lsp::Client,
    semantic_tokens: Vec<SemanticTokenType>,
}

impl Backend {
    pub fn new(
        sender: CommandSender,
        client: tower_lsp::Client,
        tokens: Vec<SemanticTokenType>,
    ) -> Self {
        Self {
            entities: Default::default(),
            sender,
            client,
            semantic_tokens: tokens,
        }
    }

    async fn run<T: Send + Sync + 'static>(
        &self,
        f: impl FnOnce(&mut World) -> T + Send + Sync + 'static,
    ) -> Option<T> {
        let (tx, rx) = futures::channel::oneshot::channel();
        let mut commands = CommandQueue::default();
        commands.push(move |world: &mut World| {
            let o = f(world);
            if let Err(_) = tx.send(o) {
                error!("Failed to run schedule for {}", stringify!(T));
            };
        });

        if let Err(e) = self.sender.0.unbounded_send(commands) {
            error!("Failed to send commands {}", e);
            return None;
        }

        rx.await.ok()
    }

    async fn run_schedule<T: Component>(
        &self,
        entity: Entity,
        schedule: impl ScheduleLabel + Clone,
        param: impl Bundle,
    ) -> Option<T> {
        let (tx, rx) = futures::channel::oneshot::channel();

        let mut commands = CommandQueue::default();
        commands.push(move |world: &mut World| {
            world.entity_mut(entity).insert(param);
            world.run_schedule(schedule.clone());
            if let Err(_) = tx.send(world.entity_mut(entity).take::<T>()) {
                error!(name: "Failed to run schedule", "Failed to run schedule {:?}", schedule);
            };
        });

        if let Err(e) = self.sender.0.unbounded_send(commands) {
            error!("Failed to send commands {}", e);
            return None;
        }

        rx.await.unwrap_or_default()
    }

    async fn get_entity(&self, uri: &str) -> Option<Entity> {
        let map = self.entities.lock().await;
        map.get(uri).copied()
    }

    fn adjust_position(pos: &mut Position) {
        pos.character = pos.character.saturating_sub(1);
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    #[instrument(skip(self, init))]
    async fn initialize(&self, init: InitializeParams) -> Result<InitializeResult> {
        info!("Initialize");

        let workspaces = init.workspace_folders.clone().unwrap_or_default();
        let config: Config =
            serde_json::from_value(init.initialization_options.clone().unwrap_or_default())
                .unwrap_or_default();

        let mut server_config = ServerConfig { config, workspaces };

        let fs = self.run(|w| w.resource::<Fs>().clone()).await.unwrap();
        if let Some(global) = LocalConfig::global(&fs).await {
            server_config.config.local.combine(global);
        }

        if let Some(root) = init.root_uri.as_ref() {
            if let Some(local) = LocalConfig::local(&fs, root).await {
                server_config.config.local.combine(local);
            }
        }

        info!("Initialize {:?}", server_config);
        let document_selectors: Vec<_> = [
            ("sparql", server_config.config.sparql.unwrap_or(true)),
            ("turtle", server_config.config.turtle.unwrap_or(true)),
            ("trig", server_config.config.trig.unwrap_or(true)),
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

        self.run(|world| {
            world.insert_resource(server_config);
            world.run_schedule(Startup);
        })
        .await;

        // let triggers = L::TRIGGERS.iter().copied().map(String::from).collect();
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                inlay_hint_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![String::from(":")]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                // implementation_provider: Some(ImplementationProviderCapability::Simple(true)),
                type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
                references_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                semantic_tokens_provider: Some(
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
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: Default::default(),
                })),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        self.run(|world| {
            tracing::info!("initialized");
            world.run_schedule(Started);
        })
        .await;
    }

    async fn did_change_workspace_folders(&self, params: DidChangeWorkspaceFoldersParams) -> () {
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
        ()
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        debug!("semantic tokens full");
        let uri = params.text_document.uri.as_str();
        let Some(entity) = self.get_entity(uri).await else {
            info!("Didn't find entity {} stopping", uri);
            return Ok(None);
        };

        if let Some(res) = self
            .run_schedule::<HighlightRequest>(entity, SemanticLabel, HighlightRequest(vec![]))
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
    async fn shutdown(&self) -> Result<()> {
        info!("Shutting down!");

        Ok(())
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document_position.text_document.uri.as_str()))]
    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let Some(entity) = self
            .get_entity(params.text_document_position.text_document.uri.as_str())
            .await
        else {
            return Ok(None);
        };

        let mut pos = params.text_document_position.position;
        Self::adjust_position(&mut pos);

        let arr = self
            .run_schedule::<ReferencesRequest>(
                entity,
                ReferencesLabel,
                (PositionComponent(pos), ReferencesRequest(Vec::new())),
            )
            .await
            .map(|x| x.0);

        Ok(arr)
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let Some(entity) = self.get_entity(params.text_document.uri.as_str()).await else {
            return Ok(None);
        };

        let mut pos = params.position;
        Self::adjust_position(&mut pos);

        let resp = self
            .run_schedule::<PrepareRenameRequest>(
                entity,
                PrepareRenameLabel,
                PositionComponent(pos),
            )
            .await
            .map(|x| PrepareRenameResponse::RangeWithPlaceholder {
                range: x.range,
                placeholder: x.placeholder,
            });

        Ok(resp)
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document_position.text_document.uri.as_str()))]
    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let Some(entity) = self
            .get_entity(params.text_document_position.text_document.uri.as_str())
            .await
        else {
            return Ok(None);
        };

        let mut pos = params.text_document_position.position;
        Self::adjust_position(&mut pos);

        let mut change_map: HashMap<crate::lsp_types::Url, Vec<TextEdit>> = HashMap::new();
        if let Some(changes) = self
            .run_schedule::<RenameEdits>(
                entity,
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

    async fn hover(&self, params: HoverParams) -> Result<Option<crate::lsp_types::Hover>> {
        let request: HoverRequest = HoverRequest::default();

        let Some(entity) = self
            .get_entity(
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

        let mut pos = params.text_document_position_params.position;
        Self::adjust_position(&mut pos);

        if let Some(hover) = self
            .run_schedule::<HoverRequest>(entity, HoverLabel, (request, PositionComponent(pos)))
            .await
        {
            tracing::info!("hover\n{:?}", hover.0);
            if hover.0.len() > 0 {
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

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        debug!("Inlay hints called");
        let uri = params.text_document.uri.as_str();
        let Some(entity) = self.get_entity(uri).await else {
            info!("Didn't find entity {}", uri);
            return Ok(None);
        };

        let request = self
            .run_schedule::<InlayRequest>(entity, InlayLabel, InlayRequest::default())
            .await;

        debug!(
            "Inlay hints resolved {} hints",
            request.as_ref().map(|x| x.0.len()).unwrap_or(0)
        );

        Ok(request.and_then(|x| Some(x.0)))
    }

    #[instrument(skip(self))]
    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri.as_str();
        let Some(entity) = self.get_entity(uri).await else {
            info!("Didn't find entity {}", uri);
            return Ok(None);
        };

        let request = self
            .run_schedule::<FormatRequest>(entity, FormatLabel, FormatRequest(None))
            .await;
        Ok(request.and_then(|x| x.0))
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let item = params.text_document;
        let url = item.uri.as_str().to_string();

        let lang_id = Some(item.language_id.clone());
        let spawn = spawn_or_insert(
            item.uri.clone(),
            (
                Source(item.text.clone()),
                Label(item.uri.clone()),
                RopeC(Rope::from_str(&item.text)),
                Wrapped(item),
                DocumentLinks(Vec::new()),
                Open,
                Types(HashMap::new()),
            ),
            lang_id,
            (),
        );

        let entity = self
            .run(|world| {
                let id = spawn(world);
                world.run_schedule(ParseLabel);
                world.flush();
                world.run_schedule(DiagnosticsLabel);
                id
            })
            .await;

        if let Some(entity) = entity {
            self.entities.lock().await.insert(url, entity);
        }

        debug!("Requesting tokens refresh");
        let _ = self.client.send_request::<SemanticTokensRefresh>(()).await;
        debug!("Semantic tokens refresh");
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let Some(entity) = self.get_entity(params.text_document.uri.as_str()).await else {
            info!("Didn't find entity {}", params.text_document.uri.as_str());
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
            let rope_c = RopeC(Rope::from_str(&change.text));
            world
                .entity_mut(entity)
                .insert((Source(change.text), rope_c));
            world.run_schedule(ParseLabel);
            world.flush();
            world.run_schedule(DiagnosticsLabel);
        })
        .await;
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let _ = params;

        self.run(move |world| {
            world.run_schedule(SaveLabel);

            debug!("Ran OnSave Schedule");
        })
        .await;
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document_position_params.text_document.uri.as_str()))]
    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let Some(entity) = self
            .get_entity(
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

        let mut pos = params.text_document_position_params.position;
        Self::adjust_position(&mut pos);

        let arr = self
            .run_schedule::<GotoDefinitionRequest>(
                entity,
                GotoDefinitionLabel,
                (PositionComponent(pos), GotoDefinitionRequest(Vec::new())),
            )
            .await
            .map(|x| {
                tracing::info!("response {:?}", x.0);
                GotoDefinitionResponse::Array(x.0)
            });

        Ok(arr)
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document_position_params.text_document.uri.as_str()))]
    async fn goto_type_definition(
        &self,
        params: GotoTypeDefinitionParams,
    ) -> Result<Option<GotoTypeDefinitionResponse>> {
        let Some(entity) = self
            .get_entity(
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

        let mut pos = params.text_document_position_params.position;
        Self::adjust_position(&mut pos);

        let arr = self
            .run_schedule::<GotoTypeRequest>(
                entity,
                GotoTypeLabel,
                (PositionComponent(pos), GotoTypeRequest(Vec::new())),
            )
            .await
            .map(|x| GotoTypeDefinitionResponse::Array(x.0));

        Ok(arr)
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = params.text_document.uri.as_str();
        let Some(entity) = self.get_entity(uri).await else {
            return Ok(None);
        };

        let request = self
            .run_schedule::<CodeActionRequest>(
                entity,
                CodeActionLabel,
                CodeActionRequest::default(),
            )
            .await;

        Ok(request.map(|r| {
            r.0.into_iter()
                .map(CodeActionOrCommand::CodeAction)
                .collect()
        }))
    }

    #[instrument(skip(self, params), fields(uri = %params.text_document_position.text_document.uri.as_str()))]
    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let Some(entity) = self
            .get_entity(params.text_document_position.text_document.uri.as_str())
            .await
        else {
            return Ok(None);
        };

        // Problem: when the cursor is at the end of an ident, that ident is not in range of the
        // cursor
        let mut pos = params.text_document_position.position;
        Self::adjust_position(&mut pos);

        let completions: Option<Vec<crate::lsp_types::CompletionItem>> = self
            .run_schedule::<CompletionRequest>(
                entity,
                CompletionLabel,
                (CompletionRequest(vec![]), PositionComponent(pos)),
            )
            .await
            .map(|x| x.0.into_iter().map(|x| x.into()).collect());

        Ok(completions.map(|mut c| {
            c.sort_by(|a, b| a.sort_text.cmp(&b.sort_text));

            CompletionResponse::Array(c)
        }))
    }
}
