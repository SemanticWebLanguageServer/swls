use std::collections::{HashMap, HashSet};

use bevy_ecs::prelude::*;
use rdf_parsers::{
    model::{BlankNode, Literal, NamedNode, Term, Turtle},
    Spanned,
};
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

// ─── Core helper (mirrors prefix_completion_helper) ───────────────────────────

/// Analyse a document's parsed [`Turtle`] model against its declared prefixes and
/// return (diagnostics, code_actions).
///
/// Walking the model (rather than the derived `Triples`) is what makes prefix
/// usage detection correct for prefixes that appear only in a literal's datatype
/// (`"5"^^xsd:integer`) — the triple form stores datatypes pre-expanded, dropping
/// the prefix entirely.
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
    turtle: &Turtle,
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

    // Collect every prefixed-name usage straight from the parsed model, as
    // (prefix name, byte span of the occurrence).  This includes datatypes, which
    // the derived triples cannot express.
    let mut uses: Vec<(String, std::ops::Range<usize>)> = Vec::new();
    // Resolved absolute IRIs of every named node.  Used to detect JSON-LD term
    // aliases, which are used as bare terms (not `prefix:local`) and so resolve
    // directly to their target IRI rather than showing up as a prefix usage.
    let mut resolved: HashSet<String> = HashSet::new();
    for triple in &turtle.triples {
        let triple = triple.value();
        collect_prefix_uses(&triple.subject, &mut uses, &mut resolved);
        for po in &triple.po {
            let po = po.value();
            collect_prefix_uses(&po.predicate, &mut uses, &mut resolved);
            for object in &po.object {
                collect_prefix_uses(object, &mut uses, &mut resolved);
            }
        }
    }

    let mut used: HashSet<String> = HashSet::new();
    // One entry per undefined-prefix *occurrence* (→ one diagnostic each), in
    // document order. Deduped by (name, byte span): a subject that heads a
    // predicate-object list (`:a :b :c; :d :e .`) yields one usage per object in
    // the model and must only be flagged once.
    let mut undefined_occurrences: Vec<(String, Range)> = Vec::new();
    // Distinct undefined prefix names (→ one quick-fix each, + URL lookup).
    let mut undefined_names: HashSet<String> = HashSet::new();
    let mut seen_spans: HashSet<(String, usize, usize)> = HashSet::new();

    for (prefix_name, span) in &uses {
        used.insert(prefix_name.clone());

        if declared.contains(prefix_name.as_str()) {
            continue;
        }
        // Record each distinct (prefix, span) occurrence once.
        if seen_spans.insert((prefix_name.clone(), span.start, span.end)) {
            if let (Some(start), Some(end)) = (
                offset_to_position(span.start, &rope.0),
                offset_to_position(span.end, &rope.0),
            ) {
                undefined_occurrences.push((prefix_name.clone(), Range::new(start, end)));
                undefined_names.insert(prefix_name.clone());
            }
        }
    }

    // Resolve suggested URLs for only the undefined prefixes in a single pass.
    let mut url_lookup: HashMap<&str, String> = HashMap::new();
    if !undefined_names.is_empty() {
        for lp in lovs {
            if undefined_names.contains(lp.name.as_ref()) {
                url_lookup
                    .entry(lp.name.as_ref())
                    .or_insert_with(|| lp.namespace.to_string());
            }
        }
        for pe in prefix_cc {
            if undefined_names.contains(pe.name.as_ref()) {
                url_lookup
                    .entry(pe.name.as_ref())
                    .or_insert_with(|| pe.namespace.to_string());
            }
        }
    }

    let mut diagnostics: Vec<Diagnostic> = Vec::new();
    let mut code_actions: Vec<CodeAction> = Vec::new();

    // ── ERROR: used but not declared ─────────────────────────────────────────
    if report_undefined {
        // One diagnostic per occurrence, so every offending token is underlined.
        // Keep them grouped by prefix so the per-prefix quick-fix can reference
        // every occurrence it resolves.
        let mut diags_by_prefix: HashMap<&str, Vec<Diagnostic>> = HashMap::new();
        for (prefix_name, range) in &undefined_occurrences {
            let diag = Diagnostic {
                range: range.clone(),
                severity: Some(DiagnosticSeverity::ERROR),
                message: format!("Undefined prefix \"{}\"", prefix_name),
                ..Default::default()
            };
            diagnostics.push(diag.clone());
            diags_by_prefix
                .entry(prefix_name.as_str())
                .or_default()
                .push(diag);
        }

        // One quick-fix per distinct prefix (in document order): the fix inserts
        // a single declaration at the top of the file, resolving *every*
        // occurrence at once — so per-occurrence quick-fixes would be duplicates.
        // The action links back to all of the prefix's diagnostics (via their
        // ranges) so editors associate the fix with each underlined occurrence.
        let mut emitted: HashSet<&str> = HashSet::new();
        for (prefix_name, _) in &undefined_occurrences {
            if !emitted.insert(prefix_name.as_str()) {
                continue;
            }
            let suggested_url = url_lookup
                .get(prefix_name.as_str())
                .map(|s| s.as_str())
                .unwrap_or("");

            let fix_edits = make_fix_edit(prefix_name, suggested_url);
            if fix_edits.is_empty() {
                continue;
            }

            let mut changes = std::collections::HashMap::new();
            changes.insert(label.0.clone(), fix_edits);
            code_actions.push(CodeAction {
                title: format!("Add prefix declaration for \"{}\"", prefix_name),
                kind: Some(CodeActionKind::QUICKFIX),
                diagnostics: diags_by_prefix.remove(prefix_name.as_str()),
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

        // JSON-LD term aliases (`"name": "foaf:name"`) are used as bare terms, not
        // as `prefix:local`; their target is a specific IRI rather than a namespace
        // (it does not end in `/` or `#`).  Treat the alias as used when that target
        // resolves somewhere in the document.
        let target = prefix.url.as_str();
        let is_alias = !target.ends_with('/') && !target.ends_with('#');
        if is_alias && resolved.contains(target) {
            continue;
        }

        let (start, end) = find_prefix_declaration_range(&rope.0, &prefix.prefix);
        diagnostics.push(Diagnostic {
            range: Range::new(start, end),
            severity: Some(DiagnosticSeverity::WARNING),
            message: format!("Prefix \"{}\" is declared but never used", prefix.prefix),
            ..Default::default()
        });
    }

    (diagnostics, code_actions)
}

/// Walk `term`, recursing into RDF collections and anonymous blank-node property
/// lists (and a literal's datatype `"x"^^prefix:local`), collecting:
/// - `uses`: every prefixed-name usage as `(prefix name, byte span)`, and
/// - `resolved`: the resolved absolute IRI of every named node.
fn collect_prefix_uses(
    term: &Spanned<Term>,
    uses: &mut Vec<(String, std::ops::Range<usize>)>,
    resolved: &mut HashSet<String>,
) {
    match term.value() {
        Term::NamedNode(nn) => record_named_node(nn, term.span(), uses, resolved),
        Term::Literal(Literal::RDF(lit)) => {
            // The datatype carries its own span (over the `^^<iri>` term), which is
            // what makes a prefix that only appears in a datatype visible here.
            if let Some(datatype) = &lit.ty {
                record_named_node(datatype.value(), datatype.span(), uses, resolved);
            }
        }
        Term::Collection(items) => {
            for item in items {
                collect_prefix_uses(item, uses, resolved);
            }
        }
        Term::BlankNode(BlankNode::Unnamed(pos, _, _)) => {
            for po in pos {
                let po = po.value();
                collect_prefix_uses(&po.predicate, uses, resolved);
                for object in &po.object {
                    collect_prefix_uses(object, uses, resolved);
                }
            }
        }
        _ => {}
    }
}

/// Record a named node's prefix usage (at `span`) and its resolved absolute IRI.
fn record_named_node(
    nn: &NamedNode,
    span: &std::ops::Range<usize>,
    uses: &mut Vec<(String, std::ops::Range<usize>)>,
    resolved: &mut HashSet<String>,
) {
    match nn {
        NamedNode::Prefixed {
            prefix, computed, ..
        } => {
            // JSON-LD stores a registered whole-term compact IRI as
            // `Prefixed(term, "")` where `prefix` is the whole term including the
            // ':'.  A real prefix name never contains ':', so that identifies the
            // whole-term case — it is not a `prefix:local` split and so not a
            // prefix usage.
            if !prefix.contains(':') {
                uses.push((prefix.clone(), span.clone()));
            }
            if let Some(computed) = computed {
                resolved.insert(computed.clone());
            }
        }
        NamedNode::Full(iri, _) => {
            resolved.insert(iri.clone());
        }
        _ => {}
    }
}

/// Scan the rope for the declaration line of `prefix_name`.
///
/// Handles three syntaxes:
/// - Turtle/TriG: `@prefix foaf: <…>.`
/// - SPARQL:       `PREFIX foaf: <…>`
/// - JSON-LD:      `"foaf": "http://…"` (inside `@context`)
fn find_prefix_declaration_range(rope: &LineIndex, prefix_name: &str) -> (Position, Position) {
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

    let candidate = (0..rope.len_lines()).find_map(|line_idx| {
        let line = rope.line_str(line_idx)?;
        let line_start = rope.line_start(line_idx)?;

        for (needle, key_off, key_len) in patterns.iter() {
            if let Some(idx) = line.find(needle.as_str()) {
                // Byte offsets within the whole document; `offset_to_position`
                // converts them (and the encoding) for us.
                let key_byte = line_start + idx + key_off;
                let start = offset_to_position(key_byte, rope)?;
                let end = offset_to_position(key_byte + key_len, rope)?;
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
            &Element,
            &Prefixes,
            &Source,
            &RopeC,
            &Label,
            &Wrapped<crate::lsp_types::TextDocumentItem>,
            &DynLang,
        ),
        (Or<(Changed<Element>, Changed<Prefixes>)>, With<Open>),
    >,
    mut client: ResMut<DiagnosticPublisher>,
    lovs: Query<&LocalPrefix>,
    prefix_cc: Query<&PrefixEntry>,
    config: Res<ServerConfig>,
) {
    let fmt = config.config.local.prefix_format.unwrap_or_default();
    let report_undefined = !config.config.local.is_disabled(Disabled::UndefinedPrefix);
    let report_unused = !config.config.local.is_disabled(Disabled::UnusedPrefix);
    for (element, prefixes, source, rope, label, params, lang) in &query {
        if !lang.0.supports_prefix_diagnostics() {
            // Clear any stale prefix diagnostics for this language and skip.
            let _ = client.publish(&params.0, vec![], "prefix");
            continue;
        }

        let (diagnostics, _) = prefix_diagnostic_helper(
            element.value(),
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
            &Element,
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
    for (element, prefixes, source, rope, label, lang, mut req) in &mut query {
        if !lang.0.supports_prefix_diagnostics() {
            continue;
        }

        let (_, actions) = prefix_diagnostic_helper(
            element.value(),
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
