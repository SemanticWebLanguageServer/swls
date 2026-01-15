use chumsky::{Parser as _, Stream};
use context::Context;
use lsp_core::lsp_types::Url;
use lsp_core::{prelude::PToken, util::Spanned};
use model::Turtle;
use tokenizer::parse_tokens_str_safe;

pub mod context;
pub mod formatter;
pub mod model;
// pub mod model2;
pub mod parser;
// pub mod parser2;
pub mod tokenizer;

pub fn parse_source(url: &Url, string: &str) -> (Option<Turtle>, Vec<String>) {
    let context = Context::new();
    let ctx = context.ctx();

    let parser = parser::turtle(url, ctx);

    let tokens = match parse_tokens_str_safe(string) {
        Ok(t) => t,
        Err(e) => {
            return (None, e.into_iter().map(|x| x.to_string()).collect());
        }
    };

    let end = string.len()..string.len();
    let stream = Stream::from_iter(
        end,
        tokens
            .into_iter()
            .enumerate()
            .filter(|(_, x)| !x.is_comment())
            .map(|(i, t)| t.map(|x| PToken(x, i)))
            // .rev()
            .map(|Spanned(x, y)| (x, y)),
    );

    let (t, es) = parser.parse_recovery(stream);
    (t, es.into_iter().map(|x| x.to_string()).collect())
}
