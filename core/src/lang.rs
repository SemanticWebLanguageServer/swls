use crate::lsp_types::SemanticTokenType;

pub fn head() -> crate::lsp_types::Range {
    let start = crate::lsp_types::Position {
        line: 0,
        character: 0,
    };
    crate::lsp_types::Range {
        end: start.clone(),
        start,
    }
}

pub trait Lang: 'static {
    /// Type of the parsed element.
    type Element: Send + Sync;
    type ElementError: Into<crate::feature::diagnostics::SimpleDiagnostic>
        + Send
        + Sync
        + std::fmt::Debug;

    const CODE_ACTION: bool;
    const HOVER: bool;
    const LANG: &'static str;
    const TRIGGERS: &'static [&'static str];
    const LEGEND_TYPES: &'static [SemanticTokenType];
    const PATTERN: Option<&'static str>;

    /// Map a CST syntax kind to a semantic token type for highlighting.
    fn semantic_token_type(_kind: rowan::SyntaxKind) -> Option<SemanticTokenType> {
        None
    }

    /// Map a CST syntax kind + byte span to semantic token spans for highlighting.
    fn semantic_token_spans(
        kind: rowan::SyntaxKind,
        span: std::ops::Range<usize>,
    ) -> Vec<(SemanticTokenType, std::ops::Range<usize>)> {
        Self::semantic_token_type(kind)
            .map(|t| vec![(t, span)])
            .unwrap_or_default()
    }
}

pub trait LangHelper: std::fmt::Debug {
    fn keyword(&self) -> &[&'static str];
}
