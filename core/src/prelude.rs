#[doc(inline)]
pub use crate::{
    backend::Backend,
    client::{Client, ClientSync},
    components::*,
    feature::{
        self,
        completion::{CompletionRequest, SimpleCompletion},
        diagnostics::{DiagnosticItem, DiagnosticPublisher, DiagnosticSender, SimpleDiagnostic},
        format::FormatRequest,
        hover::HoverRequest,
        inlay::InlayRequest,
        rename::PrepareRenameRequest,
        rename::RenameEdits,
        semantic::{HighlightRequest, SemanticTokensDict},
        *,
    },
    lang::{Lang, LangHelper, TokenTrait},
    setup_schedule_labels, systems,
    systems::prefix::{Prefix, Prefixes},
    systems::spawn_or_insert,
    util::{
        fs::*, lsp_range_to_range, offset_to_position, offsets_to_range, position_to_offset,
        range_to_range, spanned, token::*, triple::*, Spanned,
    },
    CreateEvent,
};
