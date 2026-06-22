use std::collections::{HashMap, HashSet};

use bevy_ecs::prelude::*;
use sophia_api::{quad::Quad as _, term::TermKind};
use swls_lov::LocalPrefix;
use tracing::instrument;

use crate::{
    feature::{code_action::CodeActionRequest, diagnostics::DiagnosticPublisher},
    lsp_types::{
        CodeAction, CodeActionKind, Diagnostic, DiagnosticSeverity, Position, Range, TextEdit,
        WorkspaceEdit,
    },
    prelude::*,
    systems::PrefixEntry,
    util::offset_to_position,
};

// ─── Token-level helper ───────────────────────────────────────────────────────

/// Given the raw source text of a single term token, return the prefix name if the
/// term is written as a prefixed name (`prefix:local`).
///
/// Returns `None` for full IRIs (`<...>` or containing `://`), blank nodes (`_:`),
/// variables (`?`), and anonymous default prefix (empty string before `:`).
pub fn extract_prefix_from_token(raw: &str) -> Option<&str> {
    let text = raw.trim_matches('"'); // JSON-LD wraps IRIs in quotes
    if text.starts_with('<') || text.starts_with("_:") || text.starts_with('?') {
        return None;
    }
    if text.contains("://") {
        return None;
    }
    let colon = text.find(':')?;
    let prefix = &text[..colon];
    if prefix.is_empty() {
        None // default prefix ":" — not really a declared prefix
    } else {
        Some(prefix)
    }
}

// ─── Core helper (mirrors prefix_completion_helper) ───────────────────────────

/// Analyse a document's triples against its declared prefixes and return
/// (diagnostics, code_actions).
///
/// The caller supplies `make_fix_edit(prefix_name, suggested_url) -> Vec<TextEdit>` —
/// identical in spirit to the `extra_edits` callback in `prefix_completion_helper`.
/// This callback is responsible for generating the language-specific text edit that
/// adds a prefix declaration (typically via [`LangHelper::prefix_edits`]).  It may
/// return an empty Vec if the language does not support auto-insertion.
///
/// LOV / prefix.cc iterators are used to suggest a URL for undefined prefixes.
/// Because there are usually only a handful of undefined prefixes, we collect the
/// undefined set first and then make a single pass over the LOV / prefix.cc data
/// to resolve just those — rather than materialising the entire prefix universe
/// into a lookup map.
pub fn prefix_diagnostic_helper<'a>(
    triples: &Triples,
    prefixes: &Prefixes,
    rope: &RopeC,
    label: &Label,
    lovs: impl Iterator<Item = &'a LocalPrefix>,
    prefix_cc: impl Iterator<Item = &'a PrefixEntry>,
    mut make_fix_edit: impl FnMut(&str, &str) -> Vec<TextEdit>,
    report_undefined: bool,
    report_unused: bool,
) -> (Vec<Diagnostic>, Vec<CodeAction>) {
    let declared: HashSet<&str> = prefixes.iter().map(|p| p.prefix.as_str()).collect();

    // Walk all IRI terms to find used prefix names + first occurrence spans.
    let mut used: HashSet<String> = HashSet::new();
    let mut undefined_spans: HashMap<String, Range> = HashMap::new();
    // Resolved IRI values of all IRI terms — used to detect JSON-LD term-alias
    // usage (an alias is used as a bare term, not as `prefix:local`).
    let mut iri_values: HashSet<String> = HashSet::new();

    for quad in triples.0.iter() {
        for term in [quad.s(), quad.p(), quad.o()] {
            if term.ty != Some(TermKind::Iri) {
                continue;
            }
            iri_values.insert(term.value.to_string());
            let span = &term.span;
            if span.is_empty() {
                continue;
            }
            let raw = match rope.0.get_slice(span.start..span.end) {
                Some(s) => s.to_string(),
                None => continue,
            };
            let Some(prefix_name) = extract_prefix_from_token(&raw) else {
                continue;
            };
            used.insert(prefix_name.to_string());

            if !declared.contains(prefix_name) && !undefined_spans.contains_key(prefix_name) {
                if let (Some(start), Some(end)) = (
                    offset_to_position(span.start, &rope.0),
                    offset_to_position(span.end, &rope.0),
                ) {
                    undefined_spans.insert(prefix_name.to_string(), Range::new(start, end));
                }
            }
        }
    }

    // Resolve suggested URLs for only the undefined prefixes in a single pass.
    let mut url_lookup: HashMap<&str, String> = HashMap::new();
    if !undefined_spans.is_empty() {
        for lp in lovs {
            if undefined_spans.contains_key(lp.name.as_ref()) {
                url_lookup
                    .entry(lp.name.as_ref())
                    .or_insert_with(|| lp.namespace.to_string());
            }
        }
        for pe in prefix_cc {
            if undefined_spans.contains_key(pe.name.as_ref()) {
                url_lookup
                    .entry(pe.name.as_ref())
                    .or_insert_with(|| pe.namespace.to_string());
            }
        }
    }

    let mut diagnostics: Vec<Diagnostic> = Vec::new();
    let mut code_actions: Vec<CodeAction> = Vec::new();

    // ── ERROR: used but not declared ─────────────────────────────────────────
    for (prefix_name, range) in report_undefined.then_some(&undefined_spans).into_iter().flatten() {
        let suggested_url = url_lookup
            .get(prefix_name.as_str())
            .map(|s| s.as_str())
            .unwrap_or("");

        let fix_edits = make_fix_edit(prefix_name, suggested_url);

        diagnostics.push(Diagnostic {
            range: range.clone(),
            severity: Some(DiagnosticSeverity::ERROR),
            message: format!("Undefined prefix \"{}\"", prefix_name),
            ..Default::default()
        });

        if !fix_edits.is_empty() {
            let mut changes = std::collections::HashMap::new();
            changes.insert(label.0.clone(), fix_edits);
            code_actions.push(CodeAction {
                title: format!("Add prefix declaration for \"{}\"", prefix_name),
                kind: Some(CodeActionKind::QUICKFIX),
                edit: Some(WorkspaceEdit {
                    changes: Some(changes),
                    ..Default::default()
                }),
                ..Default::default()
            });
        }
    }

    // ── WARNING: declared but not used ────────────────────────────────────────
    for prefix in report_unused.then(|| prefixes.iter()).into_iter().flatten() {
        if used.contains(prefix.prefix.as_str()) {
            continue;
        }

        // JSON-LD term aliases (e.g. `"name": "foaf:name"`) are not used as
        // `prefix:local`, but as a bare term whose resolved IRI is the alias
        // target. Such an entry's namespace does not end in `/` or `#`; treat it
        // as used if its target IRI appears among the document's IRI terms.
        let target = prefix.url.as_str();
        let is_alias = !target.ends_with('/') && !target.ends_with('#');
        if is_alias && iri_values.contains(target) {
            continue;
        }

        {
            let (start, end) = find_prefix_declaration_range(&rope.0, &prefix.prefix);
            diagnostics.push(Diagnostic {
                range: Range::new(start, end),
                severity: Some(DiagnosticSeverity::WARNING),
                message: format!(
                    "Prefix \"{}\" is declared but never used",
                    prefix.prefix
                ),
                ..Default::default()
            });
        }
    }

    (diagnostics, code_actions)
}

/// Scan the rope for the declaration line of `prefix_name`.
///
/// Handles three syntaxes:
/// - Turtle/TriG: `@prefix foaf: <…>.`
/// - SPARQL:       `PREFIX foaf: <…>`
/// - JSON-LD:      `"foaf": "http://…"` (inside `@context`)
fn find_prefix_declaration_range(
    rope: &ropey::Rope,
    prefix_name: &str,
) -> (Position, Position) {
    // Patterns that identify a declaration for this prefix, paired with the byte
    // offset (within the match) at which the highlighted key/name begins.
    let turtle_needle = format!("@prefix {}:", prefix_name);
    let sparql_needle = format!("PREFIX {}:", prefix_name);
    // JSON-LD: `"foaf":` — a context key. May appear mid-line (even on a
    // single-line document), so we search anywhere on the line, not just at the
    // start.
    let jsonld_needle = format!("\"{}\":", prefix_name);

    // (needle, offset of key start within needle, key length)
    let patterns = [
        (&turtle_needle, "@prefix ".len(), prefix_name.len()),
        (&sparql_needle, "PREFIX ".len(), prefix_name.len()),
        // Highlight the quoted key, including the surrounding quotes.
        (&jsonld_needle, 0, prefix_name.len() + 2),
    ];

    let candidate = rope.lines().enumerate().find_map(|(line_idx, line_slice)| {
        let line = line_slice.to_string();
        let line_start = rope.line_to_char(line_idx);

        for (needle, key_off, key_len) in patterns.iter() {
            if let Some(idx) = line.find(needle.as_str()) {
                let key_byte = idx + key_off;
                // Convert byte indices within the line to char offsets.
                let key_start_char = line[..key_byte].chars().count();
                let key_end_char = line[..key_byte + key_len].chars().count();
                let start = offset_to_position(line_start + key_start_char, rope)?;
                let end = offset_to_position(line_start + key_end_char, rope)?;
                return Some((start, end));
            }
        }
        None
    });

    candidate.unwrap_or((Position::new(0, 0), Position::new(0, 0)))
}

// ─── ECS systems ─────────────────────────────────────────────────────────────

/// ECS system: runs `prefix_diagnostic_helper` for every open document whose
/// triples or declared prefixes changed.
///
/// Skips languages that opt out via [`LangHelper::supports_prefix_diagnostics`]
/// (e.g. JSON-LD, which pre-expands all terms before storing them as triples).
#[instrument(skip(query, client, lovs, prefix_cc, config))]
pub fn prefix_diagnostics(
    query: Query<
        (
            &Triples,
            &Prefixes,
            &Source,
            &RopeC,
            &Label,
            &Wrapped<crate::lsp_types::TextDocumentItem>,
            &DynLang,
        ),
        (Or<(Changed<Triples>, Changed<Prefixes>)>, With<Open>),
    >,
    mut client: ResMut<DiagnosticPublisher>,
    lovs: Query<&LocalPrefix>,
    prefix_cc: Query<&PrefixEntry>,
    config: Res<ServerConfig>,
) {
    let fmt = config.config.local.prefix_format.unwrap_or_default();
    let report_undefined = !config.config.local.is_disabled(Disabled::UndefinedPrefix);
    let report_unused = !config.config.local.is_disabled(Disabled::UnusedPrefix);
    for (triples, prefixes, source, rope, label, params, lang) in &query {
        if !lang.0.supports_prefix_diagnostics() {
            // Clear any stale prefix diagnostics for this language and skip.
            let _ = client.publish(&params.0, vec![], "prefix");
            continue;
        }

        let (diagnostics, _) = prefix_diagnostic_helper(
            triples,
            prefixes,
            rope,
            label,
            lovs.iter(),
            prefix_cc.iter(),
            |name, url| {
                lang.0
                    .prefix_edits(&source.0, &rope.0, name, url, fmt)
                    .unwrap_or_default()
            },
            report_undefined,
            report_unused,
        );

        let _ = client.publish(&params.0, diagnostics, "prefix");
    }
}

/// ECS system: runs `prefix_diagnostic_helper` for every open document to populate
/// `CodeActionRequest` with "Add prefix declaration" quickfixes.
///
/// Skips languages that opt out via [`LangHelper::supports_prefix_diagnostics`].
#[instrument(skip(query, lovs, prefix_cc, config))]
pub fn add_missing_prefix_code_action(
    mut query: Query<
        (
            &Triples,
            &Prefixes,
            &Source,
            &RopeC,
            &Label,
            &DynLang,
            &mut CodeActionRequest,
        ),
        With<Open>,
    >,
    lovs: Query<&LocalPrefix>,
    prefix_cc: Query<&PrefixEntry>,
    config: Res<ServerConfig>,
) {
    let fmt = config.config.local.prefix_format.unwrap_or_default();
    if config.config.local.is_disabled(Disabled::UndefinedPrefix) {
        return;
    }
    for (triples, prefixes, source, rope, label, lang, mut req) in &mut query {
        if !lang.0.supports_prefix_diagnostics() {
            continue;
        }

        let (_, actions) = prefix_diagnostic_helper(
            triples,
            prefixes,
            rope,
            label,
            lovs.iter(),
            prefix_cc.iter(),
            |name, url| {
                lang.0
                    .prefix_edits(&source.0, &rope.0, name, url, fmt)
                    .unwrap_or_default()
            },
            true,
            false,
        );

        req.0.extend(actions);
    }
}
