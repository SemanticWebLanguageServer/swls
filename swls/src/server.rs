//! `tower_lsp` front-end for the transport-agnostic [`Backend`].
//!
//! Since the `feat: move tower_lsp to swls binary` change, `swls-core` no longer
//! depends on any transport crate: [`Backend`] exposes plain inherent `async`
//! handlers that return a [`swls_core::backend::Result`]. This module adapts those
//! handlers to [`tower_lsp::LanguageServer`] by delegating each trait method to its
//! `Backend` counterpart.
//!
//! The adapter is intentionally mechanical — it adds no behaviour, only wiring.

use swls_core::backend::Backend;
use swls_core::prelude::CommandSender;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::request::{GotoTypeDefinitionParams, GotoTypeDefinitionResponse};
use tower_lsp::lsp_types::*;
use tower_lsp::LanguageServer;

use crate::client::TowerClient;

/// `tower_lsp` adapter around [`Backend`] driven by a [`TowerClient`].
#[derive(Debug)]
pub struct LspBackend {
    backend: Backend<TowerClient>,
}

impl LspBackend {
    pub fn new(
        sender: CommandSender,
        client: TowerClient,
        tokens: Vec<SemanticTokenType>,
    ) -> Self {
        Self {
            backend: Backend::new(sender, client, tokens),
        }
    }
}

/// Lift a [`Backend`] result into a `tower_lsp` JSON-RPC result.
///
/// [`swls_core::backend::ServerError`] is uninhabited, so the error arm is
/// statically unreachable; the empty `match` discharges it for any target type.
fn lift<T>(result: swls_core::backend::Result<T>) -> Result<T> {
    match result {
        Ok(value) => Ok(value),
        Err(err) => match err {},
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for LspBackend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        lift(self.backend.initialize(params).await)
    }

    async fn initialized(&self, params: InitializedParams) {
        self.backend.initialized(params).await;
    }

    async fn shutdown(&self) -> Result<()> {
        lift(self.backend.shutdown().await)
    }

    async fn did_change_workspace_folders(&self, params: DidChangeWorkspaceFoldersParams) {
        self.backend.did_change_workspace_folders(params).await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.backend.did_open(params).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.backend.did_change(params).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.backend.did_save(params).await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        lift(self.backend.completion(params).await)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        lift(self.backend.hover(params).await)
    }

    async fn formatting(
        &self,
        params: DocumentFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        lift(self.backend.formatting(params).await)
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        lift(self.backend.inlay_hint(params).await)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        lift(self.backend.semantic_tokens_full(params).await)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        lift(self.backend.references(params).await)
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        lift(self.backend.prepare_rename(params).await)
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        lift(self.backend.rename(params).await)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        lift(self.backend.goto_definition(params).await)
    }

    async fn goto_type_definition(
        &self,
        params: GotoTypeDefinitionParams,
    ) -> Result<Option<GotoTypeDefinitionResponse>> {
        lift(self.backend.goto_type_definition(params).await)
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        lift(self.backend.code_action(params).await)
    }
}
