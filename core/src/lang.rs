use std::{hash::Hash, ops::Range};

use crate::lsp_types::SemanticTokenType;
use ropey::Rope;

use crate::prelude::*;

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

pub trait TokenTrait: Sized {
    fn token(&self) -> Option<SemanticTokenType>;

    fn span_tokens(Spanned(this, span): &Spanned<Self>) -> Vec<(SemanticTokenType, Range<usize>)> {
        if let Some(x) = this.token() {
            vec![(x, span.clone())]
        } else {
            Vec::new()
        }
    }
}

pub trait Lang: 'static {
    /// Type of tokens after tokenization
    type Token: PartialEq + Hash + Clone + Send + Sync + TokenTrait;
    type TokenError: Into<SimpleDiagnostic> + Send + Sync + std::fmt::Debug;

    /// Type of Element inside a ParentingSystem
    type Element: Send + Sync;
    type ElementError: Into<SimpleDiagnostic> + Send + Sync + std::fmt::Debug;

    const CODE_ACTION: bool;
    const HOVER: bool;
    const LANG: &'static str;
    const TRIGGERS: &'static [&'static str];
    const LEGEND_TYPES: &'static [SemanticTokenType];
    const PATTERN: Option<&'static str>;
}

pub trait LangHelper: std::fmt::Debug {
    fn _get_relevant_text(&self, token: &Spanned<Token>, rope: &Rope) -> String {
        rope.slice(token.span().clone()).to_string()
    }
    fn get_relevant_text(
        &self,
        token: &Spanned<Token>,
        rope: &Rope,
    ) -> (String, std::ops::Range<usize>) {
        (self._get_relevant_text(token, rope), token.span().clone())
    }
    fn keyword(&self) -> &[&'static str];
}
