pub mod formatter;
pub mod model;
pub mod parser;

use swls_core::lsp_types::Url;
use model::Turtle;

/// Compatibility shim for conformance tests and other callers expecting the old parse_source API.
/// Returns `(Some(Turtle), errors)` — the new parser always recovers so Turtle is always Some.
pub fn parse_source(url: &Url, string: &str) -> (Option<Turtle>, Vec<String>) {
    let (turtle, errors, _prev, _node) = parser::parse_new(string, url.as_str(), None);
    let error_strings: Vec<String> = errors.iter().map(|e| e.msg.clone()).collect();
    (Some(turtle), error_strings)
}
