use std::{borrow::Cow, ops::Range};

use crate::{lsp_types::SemanticTokenType, prelude::TripleTarget, util::offset_to_position};

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
        _text: &str,
    ) -> Vec<(SemanticTokenType, std::ops::Range<usize>)> {
        Self::semantic_token_type(kind)
            .map(|t| vec![(t, span)])
            .unwrap_or_default()
    }
}

pub trait LangHelper: std::fmt::Debug {
    fn keyword(&self) -> &[&'static str];
    fn default_position(&self) -> TripleTarget {
        TripleTarget::Subject
    }
    fn unquote<'a>(&self, inp: &'a str) -> &'a str {
        inp
    }
    fn quote(&self, inp: &str) -> String {
        format!("{}", inp)
    }
    /// Return the source keyword used to introduce a prefix declaration
    /// (used only for display / span-finding purposes).
    fn prefix_keyword(&self) -> &str {
        "@prefix"
    }
    /// Format a prefix declaration string to be inserted into a document.
    ///
    /// Default (Turtle / TriG): honors the user's [`PrefixFormat`] preference —
    /// `@prefix {name}: <{url}>.\n` (Turtle) or `PREFIX {name}: <{url}>\n` (SPARQL-style).
    fn format_prefix_declaration(
        &self,
        name: &str,
        url: &str,
        format: crate::components::PrefixFormat,
    ) -> String {
        match format {
            crate::components::PrefixFormat::Sparql => format!("PREFIX {}: <{}>\n", name, url),
            crate::components::PrefixFormat::Turtle => format!("@prefix {}: <{}>.\n", name, url),
        }
    }
    /// Produce the text edit(s) that declare prefix `name` → `namespace` in this
    /// document, or `None` when it cannot / should not be inserted (e.g. the
    /// prefix is already present).
    ///
    /// This is the single source of truth for "how do I add a prefix to a
    /// document of this language", shared by prefix completion and the
    /// "add missing prefix" diagnostic quick-fix so the two paths stay
    /// consistent.  The default (Turtle / SPARQL / TriG) inserts a declaration
    /// line at the very top of the file; JSON-LD overrides this to splice the
    /// prefix into the `@context` object.
    fn prefix_edits(
        &self,
        _source: &str,
        _rope: &ropey::Rope,
        name: &str,
        namespace: &str,
        format: crate::components::PrefixFormat,
    ) -> Option<Vec<crate::lsp_types::TextEdit>> {
        let pos = crate::lsp_types::Position::new(0, 0);
        Some(vec![crate::lsp_types::TextEdit {
            range: crate::lsp_types::Range::new(pos, pos),
            new_text: self.format_prefix_declaration(name, namespace, format),
        }])
    }
    /// Return `true` if the generic prefix-diagnostics system should analyse this
    /// language's documents.
    ///
    /// JSON-LD uses `@context` semantics where terms are pre-expanded before being
    /// stored as `Triples`; its prefix model is not compatible with the span-based
    /// detection used by the generic system.  Override to return `false` to opt out.
    fn supports_prefix_diagnostics(&self) -> bool {
        true
    }
    /// Extract the prefix name of a prefixed term whose `:` was just typed at byte
    /// `offset` (the cursor sits immediately after the `:`), or `None` when the
    /// context is not a bare prefixed name.  Used by on-type formatting to decide
    /// whether to auto-declare a prefix.
    ///
    /// Default (Turtle / TriG / SPARQL): scan back over prefix-name characters and
    /// require a term boundary before them, skipping comments and
    /// `@prefix`/`@base` (or `PREFIX`/`BASE`) declaration lines.  JSON-LD overrides
    /// this to scan inside a JSON string instead.
    fn prefix_name_at<'a>(&self, source: &'a str, offset: usize) -> Option<&'a str> {
        let bytes = source.as_bytes();
        if offset == 0 || offset > source.len() || bytes[offset - 1] != b':' {
            return None;
        }
        let colon = offset - 1;

        let mut start = colon;
        while start > 0 {
            let c = bytes[start - 1];
            if c.is_ascii_alphanumeric() || matches!(c, b'_' | b'-' | b'.') {
                start -= 1;
            } else {
                break;
            }
        }
        if start == colon {
            return None;
        }

        // The character before the name must be a term boundary; keeps us from
        // matching `:` inside an IRI (`<…:…>`), a string literal, or a pname.
        let boundary_ok = start == 0
            || matches!(
                bytes[start - 1],
                b' ' | b'\t' | b'\n' | b'\r' | b';' | b',' | b'[' | b'(' | b'{'
            );
        if !boundary_ok {
            return None;
        }

        // Skip comments and prefix/base declaration lines.
        let line_start = source[..start].rfind('\n').map(|i| i + 1).unwrap_or(0);
        let before = &source[line_start..start];
        if before.contains('#') {
            return None;
        }
        let keyword = before.trim();
        if keyword.eq_ignore_ascii_case("@prefix")
            || keyword.eq_ignore_ascii_case("prefix")
            || keyword.eq_ignore_ascii_case("@base")
            || keyword.eq_ignore_ascii_case("base")
        {
            return None;
        }

        Some(&source[start..colon])
    }
    /// Given a raw token string from the document (e.g. `<http://ex.org/foo>` or `ex:foo`),
    /// return the bare text that should be pre-filled in the editor's rename input box.
    fn rename_placeholder<'a>(&self, raw: &'a str) -> &'a str {
        // Default (Turtle / SPARQL / TriG): strip surrounding `< >`
        let s = raw.strip_prefix('<').unwrap_or(raw);
        s.strip_suffix('>').unwrap_or(s)
    }
    /// Wrap the user-supplied rename text so that it is valid in the current language.
    ///
    /// Default (Turtle / SPARQL / TriG) smart rules:
    /// - already has `< >` → keep as-is
    /// - starts with `_:` → blank node, keep as-is
    /// - contains `://` → full IRI with scheme (e.g. `http://`), wrap in `< >`
    /// - contains `:` but no `://` → prefixed name (e.g. `ex:foo`), keep as-is
    /// - otherwise → bare label, wrap in `< >` to be safe
    fn rename_wrap(&self, new_text: &str) -> String {
        if new_text.starts_with('<') && new_text.ends_with('>') {
            new_text.to_string()
        } else if new_text.starts_with("_:") {
            new_text.to_string()
        } else if new_text.contains("://") {
            format!("<{}>", new_text)
        } else if new_text.contains(':') {
            // Prefixed name like ex:foo
            new_text.to_string()
        } else {
            format!("<{}>", new_text)
        }
    }
    /// Return `true` if this language provides its own prefix completion and
    /// the generic [`defined_prefix_completion`] system should be skipped.
    fn handles_prefix_completion(&self) -> bool {
        false
    }
    /// Return `true` if this language renames via the model-based systems in
    /// `swls-lang-rdf-base` (registered through `setup_rename`).  When `true`,
    /// the core language-agnostic `prepare_rename`/`rename` systems skip this
    /// language's documents to avoid producing duplicate edits.
    ///
    /// Text RDF syntaxes (Turtle / TriG / SPARQL) override this to `true`;
    /// JSON-LD keeps the default and stays on the agnostic path.
    fn model_based_rename(&self) -> bool {
        false
    }
    fn supports_shape_validation(&self) -> bool {
        true
    }

    fn inlay_types_hint(
        &self,
        subject: &Range<usize>,
        rope: &ropey::Rope,
        last_type: Option<&Range<usize>>,
        types: Vec<Cow<'_, str>>,
    ) -> Option<crate::lsp_types::InlayHint> {
        let (label, position) = if let Some(lt) = last_type {
            if let Some(pos) = offset_to_position(lt.end, &rope) {
                let label = format!(", {}", types.join(", "));
                (label, pos)
            } else {
                return None;
            }
        } else {
            let offset = if rope.get_char(subject.start) == Some('[') {
                subject.start + 1
            } else {
                subject.end
            };

            if let Some(pos) = offset_to_position(offset, &rope) {
                let label = format!(" a {};", types.join(", "));
                (label, pos)
            } else {
                return None;
            }
        };

        return Some(crate::lsp_types::InlayHint {
            position,
            label: crate::lsp_types::InlayHintLabel::String(label),
            kind: None,
            text_edits: None,
            tooltip: None,
            padding_left: None,
            padding_right: None,
            data: None,
        });
    }
}
