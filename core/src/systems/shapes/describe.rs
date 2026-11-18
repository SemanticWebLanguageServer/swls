//! Documentation from SHACL property shapes, for the predicates of a subject.
//!
//! When a property shape applies to the subject (see [`super::model`]) and
//! constrains a predicate, hover and completion for that predicate show:
//!
//! - `sh:name` and `sh:description`,
//! - `rdfs:label` and `rdfs:comment` of the property shape,
//! - a summary of its constraints (cardinality, node kind, datatype, class,
//!   allowed values, nested shapes, patterns, ranges, severity).
//!
//! Texts are shown in the preferred language (see [`crate::util::lang_text`]).

use std::collections::HashMap;

use bevy_ecs::prelude::*;
use sophia_api::term::{Term as _, TermKind};

use super::model::*;
use crate::{
    feature::{
        completion::{CompletionRequest, CursorContext, SimpleCompletion},
        hover::HoverRequest,
    },
    lsp_types::{CompletionItemKind, TextEdit},
    prelude::*,
    systems::shapes::visible::{visible_shapes, ShapeDocs},
    util::lang_text::normalize_ws,
};

/// Shortens IRIs for display: with the document's prefixes, else with those
/// the shape documents declare (so `xsd:string` reads well even when the
/// document does not declare `xsd:`).
pub(crate) struct Names<'a> {
    document: &'a Prefixes,
    /// Every prefix the shape documents declare, as one set, so the choice
    /// between them does not depend on the order the documents are visited.
    shapes: Prefixes,
}

impl<'a> Names<'a> {
    pub(crate) fn new(document: &'a Prefixes, shapes: Vec<&'a Prefixes>) -> Self {
        let declared = shapes.iter().flat_map(|p| p.0.iter().cloned()).collect();
        Self {
            document,
            shapes: Prefixes(declared, document.1.clone()),
        }
    }

    /// `iri` as a prefixed name from the document's prefixes, else from the
    /// shape documents' prefixes, else in full.
    fn iri(&self, iri: &str) -> String {
        self.document
            .shorten(iri)
            .or_else(|| self.shapes.shorten(iri))
            .unwrap_or_else(|| format!("<{}>", iri))
    }
}

fn term(names: &Names<'_>, node: Node<'_>) -> String {
    match node.term.kind() {
        TermKind::Iri => names.iri(&node.term.value),
        TermKind::Literal => format!("\"{}\"", node.term.value),
        _ => String::from("[ … ]"),
    }
}

fn code(s: &str) -> String {
    format!("`{}`", s.replace('`', "'"))
}

/// Name of a shape for display: its IRI, or a description of an anonymous one.
fn shape_name(names: &Names<'_>, node: Node<'_>) -> String {
    match node.term.kind() {
        TermKind::Iri => code(&names.iri(&node.term.value)),
        _ => String::from("an anonymous shape"),
    }
}

fn number(graph: &ShapeGraph<'_>, node: Node<'_>, local: &str) -> Option<String> {
    let v = graph.object(node, &sh(local))?;
    Some(v.term.value.trim().to_string())
}

/// One-line summary of the constraints of property shape `prop`.
pub(crate) fn constraints(
    graph: &ShapeGraph<'_>,
    prop: Node<'_>,
    names: &Names<'_>,
) -> Vec<String> {
    let mut out = Vec::new();
    let list = |local: &str| -> Vec<String> {
        graph
            .objects(prop, &sh(local))
            .into_iter()
            .flat_map(|l| graph.list(l))
            .map(|t| code(&term(names, t)))
            .collect()
    };
    let values = |local: &str| -> Vec<String> {
        graph
            .objects(prop, &sh(local))
            .into_iter()
            .map(|t| code(&term(names, t)))
            .collect()
    };

    let min: f64 = graph.number(prop, &sh("minCount")).unwrap_or(0.0);
    match min as u64 {
        0 => {}
        1 => out.push(String::from("required")),
        n => out.push(format!("at least {n}")),
    }
    if let Some(max) = graph.number(prop, &sh("maxCount")) {
        out.push(format!("at most {}", max as u64));
    }

    for kind in graph.objects(prop, &sh("nodeKind")) {
        let kind = kind.term.value.strip_prefix(SH).unwrap_or(&kind.term.value);
        out.push(String::from(match kind {
            "IRI" => "IRI",
            "BlankNode" => "blank node",
            "Literal" => "literal",
            "BlankNodeOrIRI" => "IRI or blank node",
            "BlankNodeOrLiteral" => "blank node or literal",
            "IRIOrLiteral" => "IRI or literal",
            other => other,
        }));
    }
    for dt in values("datatype") {
        out.push(format!("datatype {dt}"));
    }
    for class in values("class") {
        out.push(format!("instance of {class}"));
    }
    let allowed = list("in");
    if !allowed.is_empty() {
        out.push(format!("one of {}", allowed.join(", ")));
    }
    for v in values("hasValue") {
        out.push(format!("value {v}"));
    }
    for node in graph.objects(prop, &sh("node")) {
        out.push(format!("conforms to {}", shape_name(names, node)));
    }
    for (local, word) in [("or", "one of"), ("xone", "exactly one of")] {
        for l in graph.objects(prop, &sh(local)) {
            let branches: Vec<String> = graph
                .list(l)
                .into_iter()
                .flat_map(|b| {
                    if b.term.kind() == TermKind::Iri {
                        return Some(shape_name(names, b));
                    }
                    graph
                        .object(b, &sh("node"))
                        .map(|n| shape_name(names, n))
                        .or_else(|| graph.object(b, &sh("class")).map(|c| code(&term(names, c))))
                        .or_else(|| {
                            graph
                                .object(b, &sh("datatype"))
                                .map(|d| code(&term(names, d)))
                        })
                })
                .collect();
            if !branches.is_empty() {
                out.push(format!("conforms to {} {}", word, branches.join(", ")));
            }
        }
    }
    if let Some(pattern) = graph.object(prop, &sh("pattern")) {
        let p = &pattern.term.value;
        let shown: String = p.chars().take(40).collect();
        let ellipsis = if p.chars().count() > 40 { "…" } else { "" };
        out.push(format!("matches {}{}", code(&shown), ellipsis));
    }
    for (local, sign) in [
        ("minLength", "length ≥"),
        ("maxLength", "length ≤"),
        ("minInclusive", "≥"),
        ("maxInclusive", "≤"),
        ("minExclusive", ">"),
        ("maxExclusive", "<"),
    ] {
        if let Some(v) = number(graph, prop, local) {
            out.push(format!("{sign} {v}"));
        }
    }
    let languages = list("languageIn");
    if !languages.is_empty() {
        out.push(format!("language one of {}", languages.join(", ")));
    }
    if graph.has(prop, &sh("uniqueLang"), "true") {
        out.push(String::from("one value per language"));
    }
    if let Some(sev) = graph.object(prop, &sh("severity")) {
        if sev.term.value != sh("Violation") {
            let sev = sev.term.value.strip_prefix(SH).unwrap_or(&sev.term.value);
            out.push(format!("severity {}", sev.to_lowercase()));
        }
    }
    out
}

/// Markdown documenting property shape `applied` for a hover or completion.
pub(crate) fn describe(
    graph: &ShapeGraph<'_>,
    applied: AppliedPropertyShape<'_>,
    names: &Names<'_>,
) -> String {
    let prop = applied.node;
    let texts = |p: &str| -> Vec<String> {
        graph
            .texts(prop, p)
            .preferred()
            .into_iter()
            .map(|t| normalize_ws(t).into_owned())
            .collect()
    };
    let shacl_names = texts(&sh("name"));
    let descriptions = texts(&sh("description"));
    // Shown as well, unless they repeat the SHACL text word for word.
    let labels: Vec<_> = texts(RDFS_LABEL)
        .into_iter()
        .filter(|l| !shacl_names.contains(l))
        .collect();
    let comments: Vec<_> = texts(RDFS_COMMENT)
        .into_iter()
        .filter(|c| !descriptions.contains(c))
        .collect();

    let mut md = format!("**Shape** {}", shape_name(names, applied.owner));
    if prop.term.kind() == TermKind::Iri {
        md.push_str(&format!(" · property shape {}", shape_name(names, prop)));
    }

    let mut section = |title: &str, items: &[String]| {
        if !items.is_empty() {
            md.push_str(&format!("\n\n**{}:** {}", title, items.join(" / ")));
        }
    };
    section("Name", &shacl_names);
    section("Description", &descriptions);
    section("Label", &labels);
    section("Comment", &comments);

    if let Some(paths) = property_paths(graph, prop).filter(|p| p.len() > 1) {
        let paths: Vec<_> = paths.iter().map(|p| code(&names.iri(p))).collect();
        md.push_str(&format!("\n\n**Any one of:** {}", paths.join(", ")));
    }
    let summary = constraints(graph, prop, names);
    if !summary.is_empty() {
        md.push_str(&format!("\n\n**Constraints:** {}", summary.join(" · ")));
    }
    md
}

/// Documentation of every property shape that applies to `focus`, per predicate.
/// How much the shapes need a predicate on the subject, ordered from least to
/// most urgent.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Need {
    /// Allowed, or already present often enough.
    Optional,
    /// One of the alternatives of a required `sh:alternativePath`, none of
    /// which the subject has yet.
    OneOfRequired,
    /// Required (`sh:minCount >= 1`) and the subject does not have it yet.
    Required,
}

impl Need {
    /// Sort key prefix: required predicates come before every other
    /// suggestion (`!` sorts before digits, letters and `@`).
    fn sort_prefix(self) -> Option<&'static str> {
        match self {
            Need::Required => Some("!0"),
            Need::OneOfRequired => Some("!1"),
            Need::Optional => None,
        }
    }

    fn label(self) -> Option<&'static str> {
        match self {
            Need::Required => Some("required by shape"),
            Need::OneOfRequired => Some("one of required by shape"),
            Need::Optional => None,
        }
    }
}

/// A predicate the shapes describe for the subject.
struct ShapePredicate {
    path: String,
    docs: Vec<String>,
    need: Need,
}

/// Documentation of every property shape that applies to `focus`, per
/// predicate, with how much the shapes still need it.
fn docs_per_predicate(
    graph: &ShapeGraph<'_>,
    focus: &Focus<'_>,
    names: &Names<'_>,
) -> Vec<ShapePredicate> {
    let mut out: Vec<ShapePredicate> = Vec::new();
    for applied in applied_property_shapes(graph, focus) {
        let Some(paths) = property_paths(graph, applied.node) else {
            continue;
        };
        let min = graph.number(applied.node, &sh("minCount")).unwrap_or(0.0);
        let missing = !paths.iter().any(|p| focus.has_predicate(p));
        let need = match (min >= 1.0 && missing, paths.len()) {
            (false, _) => Need::Optional,
            (true, 1) => Need::Required,
            (true, _) => Need::OneOfRequired,
        };
        let doc = describe(graph, applied, names);
        for path in paths {
            match out.iter_mut().find(|p| p.path == path) {
                Some(p) => {
                    p.docs.push(doc.clone());
                    p.need = p.need.max(need);
                }
                None => out.push(ShapePredicate {
                    path,
                    docs: vec![doc.clone()],
                    need,
                }),
            }
        }
    }
    out
}

/// Hover system: document a predicate with the property shapes that apply to
/// it for this subject.
pub fn hover_shape_property(
    mut query: Query<(
        &TripleComponent,
        &Triples,
        &Prefixes,
        &Label,
        &DocumentLinks,
        &mut HoverRequest,
    )>,
    shapes: ShapeDocs,
    hierarchy: Res<TypeHierarchy>,
    config: Res<ServerConfig>,
) {
    let local = &config.config.local;
    if local.is_disabled(Disabled::HoverShapeProperty) || local.is_disabled(Disabled::Shapes) {
        return;
    }
    for (triple, triples, prefixes, label, links, mut request) in &mut query {
        if triple.target != TripleTarget::Predicate {
            continue;
        }
        let predicate = &triple.triple.predicate;
        if predicate.kind() != TermKind::Iri {
            continue;
        }
        let (docs, shape_prefixes) = visible_shapes(&shapes, label, links);
        if docs.is_empty() {
            continue;
        }
        let graph = ShapeGraph::new(docs);
        let names = Names::new(prefixes, shape_prefixes);
        let focus = Focus::new(&triple.triple.subject, &triples.0, &hierarchy);
        for applied in applied_property_shapes(&graph, &focus) {
            let applies = property_paths(&graph, applied.node)
                .is_some_and(|paths| paths.iter().any(|p| *p == predicate.value));
            if applies {
                request.0.push(describe(&graph, applied, &names));
            }
        }
    }
}

/// Completion system: add shape documentation to property completions, and
/// offer predicates that only the shapes declare.
///
/// Runs after [`complete_properties`](crate::systems::complete_properties).
pub fn shape_property_completions(
    mut query: Query<(
        &CursorContext,
        &Prefixes,
        &Triples,
        &Label,
        &DocumentLinks,
        &DynLang,
        &mut CompletionRequest,
    )>,
    shapes: ShapeDocs,
    hierarchy: Res<TypeHierarchy>,
    config: Res<ServerConfig>,
) {
    let local = &config.config.local;
    if local.is_disabled(Disabled::CompletionProperty) || local.is_disabled(Disabled::Shapes) {
        return;
    }
    for (cursor, prefixes, triples, label, links, lang, mut request) in &mut query {
        if !cursor.at_predicate() {
            continue;
        }
        let Some(subject) = cursor.subject.as_ref() else {
            continue;
        };
        let (docs, shape_prefixes) = visible_shapes(&shapes, label, links);
        if docs.is_empty() {
            continue;
        }
        let graph = ShapeGraph::new(docs);
        let names = Names::new(prefixes, shape_prefixes);
        let focus = Focus::new(subject, &triples.0, &hierarchy);
        let per_predicate = docs_per_predicate(&graph, &focus, &names);
        if per_predicate.is_empty() {
            continue;
        }

        let shortened: HashMap<String, usize> = per_predicate
            .iter()
            .enumerate()
            .flat_map(|(i, p)| [(p.path.clone(), i), (names.iri(&p.path), i)])
            .chain(
                per_predicate
                    .iter()
                    .enumerate()
                    .flat_map(|(i, p)| prefixes.shorten(&p.path).map(|s| (s, i))),
            )
            .collect();

        // Document (and rank) the property suggestions that already exist.
        let mut documented = vec![false; per_predicate.len()];
        for item in request.0.iter_mut() {
            if item.kind != CompletionItemKind::FIELD
                && item.kind != CompletionItemKind::ENUM_MEMBER
            {
                continue;
            }
            let Some(&i) = shortened.get(&item.label) else {
                continue;
            };
            documented[i] = true;
            let predicate = &per_predicate[i];
            let shape_docs = predicate.docs.join("\n\n---\n\n");
            item._documentation = Some(match item._documentation.take() {
                Some(d) if !d.is_empty() => format!("{d}\n\n---\n\n{shape_docs}"),
                _ => shape_docs,
            });
            if let (Some(sort), Some(need)) = (predicate.need.sort_prefix(), predicate.need.label())
            {
                item._sort_text = Some(format!("{}{}", sort, item.label));
                let title = item
                    ._label_details
                    .as_ref()
                    .and_then(|d| d.description.clone())
                    .filter(|t| !t.is_empty());
                let description = match title {
                    Some(title) => format!("{need} · {title}"),
                    None => need.to_string(),
                };
                item.set_label_description(description);
                item.kind = CompletionItemKind::FIELD;
            }
        }

        // Predicates only the shapes know about.
        let typed = lang.unquote(&cursor.typed);
        for (i, predicate) in per_predicate.iter().enumerate() {
            if documented[i] || focus_has(subject, &triples.0, &predicate.path) {
                continue;
            }
            let short = lang.write_iri(prefixes, &predicate.path);
            if !short.starts_with(typed) {
                continue;
            }
            let sort = predicate.need.sort_prefix().unwrap_or("0");
            let mut item = SimpleCompletion::new(
                CompletionItemKind::FIELD,
                short.clone(),
                TextEdit {
                    range: cursor.range,
                    new_text: cursor.spaced(&lang.quote(&short)),
                },
            )
            .documentation(predicate.docs.join("\n\n---\n\n"))
            .sort_text(format!("{}{}", sort, short));
            if let Some(need) = predicate.need.label() {
                item = item.label_description(need);
            }
            request.0.push(item);
        }
    }
}

fn focus_has(subject: &MyTerm<'_>, triples: &[MyQuad<'static>], predicate: &str) -> bool {
    triples
        .iter()
        .any(|t| t.subject == *subject && t.predicate.value == predicate)
}
