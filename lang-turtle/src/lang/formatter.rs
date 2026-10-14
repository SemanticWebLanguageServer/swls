//! Turtle formatting.
//!
//! Layout comes from the generated formatter in `rdf_parsers` (grammar hints
//! plus `rdf_parsers::turtle::format::TurtleHook`). The only rule added here
//! is rewriting prefix and base declarations to the configured style.

use rdf_parsers::{
    format::{Doc, FormatHook, Formatter},
    turtle::{
        format::TurtleHook,
        parser::{format::format_with, Lang, SyntaxKind, SyntaxNode},
    },
};

#[derive(Default, Clone, Copy)]
pub enum PrefixStyle {
    #[default]
    OldSchool,
    Sparql,
}

/// Format a Turtle document from its syntax tree.
pub fn format_turtle(root: &SyntaxNode, prefix_style: impl Into<PrefixStyle>) -> String {
    let hook = PrefixStyleHook {
        style: prefix_style.into(),
    };
    format_with(root, 80, &hook)
}

/// [`TurtleHook`] that also writes prefix and base declarations in one style.
struct PrefixStyleHook {
    style: PrefixStyle,
}

impl FormatHook<Lang> for PrefixStyleHook {
    fn keep(&self, element: &rowan::SyntaxElement<Lang>) -> bool {
        TurtleHook.keep(element)
    }

    fn node(&self, node: &SyntaxNode, fmt: &Formatter<'_, Lang>) -> Option<Doc> {
        self.directive(node).or_else(|| TurtleHook.node(node, fmt))
    }
}

impl PrefixStyleHook {
    fn directive(&self, node: &SyntaxNode) -> Option<Doc> {
        let text = |kind| {
            node.descendants_with_tokens()
                .find(|e| e.kind() == kind && e.as_token().is_some())
                .map(|e| e.as_token().unwrap().text().to_string())
        };
        // Leave declarations with comments inside as they are.
        if text(SyntaxKind::Comment).is_some() {
            return None;
        }
        let iri = text(SyntaxKind::Iriref)?;
        let out = match (node.kind(), self.style) {
            (SyntaxKind::PrefixId | SyntaxKind::SparqlPrefix, style) => {
                let prefix = text(SyntaxKind::PnameNs)?;
                match style {
                    PrefixStyle::OldSchool => format!("@prefix {prefix} {iri}."),
                    PrefixStyle::Sparql => format!("PREFIX {prefix} {iri}"),
                }
            }
            (SyntaxKind::Base | SyntaxKind::SparqlBase, PrefixStyle::OldSchool) => {
                format!("@base {iri}.")
            }
            (SyntaxKind::Base | SyntaxKind::SparqlBase, PrefixStyle::Sparql) => {
                format!("BASE {iri}")
            }
            _ => return None,
        };
        Some(Doc::text(out))
    }
}

#[cfg(test)]
mod tests {
    use super::{format_turtle, PrefixStyle};
    use crate::lang::parser::parse_new;

    fn format(input: &str, style: PrefixStyle) -> String {
        let (_, _, _, syntax, _) = parse_new(input, "http://example.com/ns#", None);
        format_turtle(&syntax, style)
    }

    #[test]
    fn writes_prefixes_in_turtle_style() {
        let input =
            "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\nBASE <http://ex/>\n<a> foaf:knows <b>.";
        let expected =
            "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n@base <http://ex/>.\n\n<a> foaf:knows <b>.\n";
        assert_eq!(format(input, PrefixStyle::OldSchool), expected);
    }

    #[test]
    fn writes_prefixes_in_sparql_style() {
        let input =
            "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n@base <http://ex/>.\n<a> foaf:knows <b>.";
        let expected =
            "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\nBASE <http://ex/>\n\n<a> foaf:knows <b>.\n";
        assert_eq!(format(input, PrefixStyle::Sparql), expected);
    }

    #[test]
    fn keeps_layout_rules() {
        // Layout itself is tested in rdf-parsers; this checks the hook chain.
        let input = "[] a <x>; <p> [ <q> <r>; <s> <t> ], <u>;. # done";
        let expected = "[ ] a <x>;\n  <p> [\n    <q> <r>;\n    <s> <t>;\n  ], <u>. # done\n";
        assert_eq!(format(input, PrefixStyle::OldSchool), expected);
    }
}
