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
use goto_definition::GotoDefinitionRequest;
use goto_type::GotoTypeRequest;
use lsp_types::*;
use references::ReferencesRequest;
use request::{GotoTypeDefinitionParams, GotoTypeDefinitionResponse};
use ropey::Rope;
use systems::LovHelper;
use tower_lsp::{jsonrpc::Result, LanguageServer};
use tracing::info;

use crate::{prelude::*, Startup};

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
                tracing::error!("Failed to run schedule for {}", stringify!(T));
            };
        });

        if let Err(e) = self.sender.0.unbounded_send(commands) {
            tracing::error!("Failed to send commands {}", e);
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
                tracing::error!("Failed to run schedule {:?}", schedule);
            };
        });

        if let Err(e) = self.sender.0.unbounded_send(commands) {
            tracing::error!("Failed to send commands {}", e);
            return None;
        }

        rx.await.unwrap_or_default()
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    #[tracing::instrument(skip(self, _init))]
    async fn initialize(&self, _init: InitializeParams) -> Result<InitializeResult> {
        info!("Initialize");
        // iew
        let cache = Cache::from_client(&self.client).await;
        let helper = LovHelper::from_cache(&cache);

        self.run(|world| {
            world.insert_resource(cache);
            world.insert_resource(helper);
            world.run_schedule(Startup)
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
                code_action_provider: None,
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
                                    document_selector: Some(vec![
                                        DocumentFilter {
                                            language: Some(String::from("turtle")),
                                            scheme: None,
                                            pattern: None,
                                        },
                                        DocumentFilter {
                                            language: Some(String::from("jsonld")),
                                            scheme: None,
                                            pattern: None,
                                        },
                                        DocumentFilter {
                                            language: Some(String::from("sparql")),
                                            scheme: None,
                                            pattern: None,
                                        },
                                        // DocumentFilter {
                                        //     language: Some(String::from("sparql")),
                                        //     scheme: None,
                                        //     pattern: Some(String::from("*.rq")),
                                        // },
                                    ]),
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

    #[tracing::instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        info!("semantic tokens full");
        let uri = params.text_document.uri.as_str();
        let entity = {
            let e = {
                let map = self.entities.lock().await;
                if let Some(entity) = map.get(uri) {
                    Some(entity.clone())
                } else {
                    info!("Didn't find entity {} retrying", uri);
                    None
                }
            };

            if let Some(e) = e {
                e
            } else {
                let map = self.entities.lock().await;
                if let Some(entity) = map.get(uri) {
                    entity.clone()
                } else {
                    info!("Didn't find entty {} stopping", uri);
                    return Ok(None);
                }
            }
        };

        if let Some(res) = self
            .run_schedule::<HighlightRequest>(entity, SemanticLabel, HighlightRequest(vec![]))
            .await
        {
            Ok(Some(SemanticTokensResult::Tokens(
                lsp_types::SemanticTokens {
                    result_id: None,
                    data: res.0,
                },
            )))
        } else {
            info!("resulitng in no tokens");
            Ok(None)
        }
    }

    #[tracing::instrument(skip(self))]
    async fn shutdown(&self) -> Result<()> {
        info!("Shutting down!");

        self.run(|world| {
            match (
                world.remove_resource::<Cache>(),
                world.remove_resource::<LovHelper>(),
            ) {
                (Some(cache), Some(helper)) => {
                    helper.save(&cache);
                }
                _ => {}
            }
        })
        .await;

        Ok(())
    }

    #[tracing::instrument(skip(self, params), fields(uri = %params.text_document_position.text_document.uri.as_str()))]
    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let entity = {
            let map = self.entities.lock().await;
            if let Some(entity) = map.get(params.text_document_position.text_document.uri.as_str())
            {
                entity.clone()
            } else {
                return Ok(None);
            }
        };

        let mut pos = params.text_document_position.position;
        pos.character = if pos.character > 0 {
            pos.character - 1
        } else {
            pos.character
        };

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

    #[tracing::instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let entity = {
            let map = self.entities.lock().await;
            if let Some(entity) = map.get(params.text_document.uri.as_str()) {
                entity.clone()
            } else {
                return Ok(None);
            }
        };

        let mut pos = params.position;
        pos.character = if pos.character > 0 {
            pos.character - 1
        } else {
            pos.character
        };

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

    #[tracing::instrument(skip(self, params), fields(uri = %params.text_document_position.text_document.uri.as_str()))]
    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let entity = {
            let map = self.entities.lock().await;
            if let Some(entity) = map.get(params.text_document_position.text_document.uri.as_str())
            {
                entity.clone()
            } else {
                return Ok(None);
            }
        };

        let mut pos = params.text_document_position.position;
        pos.character = if pos.character > 0 {
            pos.character - 1
        } else {
            pos.character
        };

        let mut change_map: HashMap<lsp_types::Url, Vec<TextEdit>> = HashMap::new();
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

    async fn hover(&self, params: HoverParams) -> Result<Option<lsp_types::Hover>> {
        let request: HoverRequest = HoverRequest::default();

        let entity = {
            let map = self.entities.lock().await;
            if let Some(entity) = map.get(
                params
                    .text_document_position_params
                    .text_document
                    .uri
                    .as_str(),
            ) {
                entity.clone()
            } else {
                return Ok(None);
            }
        };

        let mut pos = params.text_document_position_params.position;
        pos.character = if pos.character > 0 {
            pos.character - 1
        } else {
            pos.character
        };

        if let Some(hover) = self
            .run_schedule::<HoverRequest>(entity, HoverLabel, (request, PositionComponent(pos)))
            .await
        {
            if hover.0.len() > 0 {
                return Ok(Some(lsp_types::Hover {
                    contents: lsp_types::HoverContents::Array(
                        hover.0.into_iter().map(MarkedString::String).collect(),
                    ),
                    range: hover.1,
                }));
            }
        }

        Ok(None)
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        info!("Inlay hints called");
        let uri = params.text_document.uri.as_str();
        let entity = {
            let map = self.entities.lock().await;
            if let Some(entity) = map.get(uri) {
                entity.clone()
            } else {
                info!("Didn't find entity {}", uri);
                return Ok(None);
            }
        };

        let request = self
            .run_schedule::<InlayRequest>(entity, InlayLabel, InlayRequest(None))
            .await;

        Ok(request.and_then(|x| x.0))
    }

    #[tracing::instrument(skip(self))]
    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri.as_str();
        let entity = {
            let map = self.entities.lock().await;
            if let Some(entity) = map.get(uri) {
                entity.clone()
            } else {
                info!("Didn't find entity {}", uri);
                return Ok(None);
            }
        };

        let request = self
            .run_schedule::<FormatRequest>(entity, FormatLabel, FormatRequest(None))
            .await;
        Ok(request.and_then(|x| x.0))
    }

    #[tracing::instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let item = params.text_document;
        let url = item.uri.as_str().to_string();

        tracing::info!("Did open");

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
                info!("Running diagnostics");
                world.run_schedule(DiagnosticsLabel);
                id
            })
            .await;

        if let Some(entity) = entity {
            self.entities.lock().await.insert(url, entity);
        }
    }

    #[tracing::instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let entity = {
            let map = self.entities.lock().await;
            if let Some(entity) = map.get(params.text_document.uri.as_str()) {
                entity.clone()
            } else {
                info!("Didn't find entity {}", params.text_document.uri.as_str());
                return;
            }
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
            info!("Running diagnostics");
            world.run_schedule(DiagnosticsLabel);
        })
        .await;
    }

    #[tracing::instrument(skip(self, params), fields(uri = %params.text_document.uri.as_str()))]
    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let _ = params;

        info!("Did save");
        self.run(move |world| {
            world.run_schedule(SaveLabel);

            info!("Ran OnSave Schedule");
        })
        .await;
    }

    #[tracing::instrument(skip(self, params), fields(uri = %params.text_document_position_params.text_document.uri.as_str()))]
    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let entity = {
            let map = self.entities.lock().await;
            if let Some(entity) = map.get(
                params
                    .text_document_position_params
                    .text_document
                    .uri
                    .as_str(),
            ) {
                entity.clone()
            } else {
                return Ok(None);
            }
        };

        let mut pos = params.text_document_position_params.position;
        pos.character = if pos.character > 0 {
            pos.character - 1
        } else {
            pos.character
        };

        let arr = self
            .run_schedule::<GotoDefinitionRequest>(
                entity,
                GotoDefinitionLabel,
                (PositionComponent(pos), GotoDefinitionRequest(Vec::new())),
            )
            .await
            .map(|x| GotoDefinitionResponse::Array(x.0));

        Ok(arr)
    }

    #[tracing::instrument(skip(self, params), fields(uri = %params.text_document_position_params.text_document.uri.as_str()))]
    async fn goto_type_definition(
        &self,
        params: GotoTypeDefinitionParams,
    ) -> Result<Option<GotoTypeDefinitionResponse>> {
        let entity = {
            let map = self.entities.lock().await;
            if let Some(entity) = map.get(
                params
                    .text_document_position_params
                    .text_document
                    .uri
                    .as_str(),
            ) {
                entity.clone()
            } else {
                return Ok(None);
            }
        };

        let mut pos = params.text_document_position_params.position;
        pos.character = if pos.character > 0 {
            pos.character - 1
        } else {
            pos.character
        };

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

    #[tracing::instrument(skip(self, params), fields(uri = %params.text_document_position.text_document.uri.as_str()))]
    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let entity = {
            let map = self.entities.lock().await;
            if let Some(entity) = map.get(params.text_document_position.text_document.uri.as_str())
            {
                entity.clone()
            } else {
                return Ok(None);
            }
        };

        // Problem: whne the cursor is at the end of en ident, that ident is not in range of the
        // cursor
        let mut pos = params.text_document_position.position;
        pos.character = if pos.character > 0 {
            pos.character - 1
        } else {
            pos.character
        };

        let completions: Option<Vec<lsp_types::CompletionItem>> = self
            .run_schedule::<CompletionRequest>(
                entity,
                CompletionLabel,
                (CompletionRequest(vec![]), PositionComponent(pos)),
            )
            .await
            .map(|x| x.0.into_iter().map(|x| x.into()).collect());

        Ok(completions.map(|c| CompletionResponse::Array(c)))
    }
}
