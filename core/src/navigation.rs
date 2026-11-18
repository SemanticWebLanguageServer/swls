//! Document links, document symbols (outline), folding ranges and workspace
//! symbols. Read-only views of what the server already parsed.

use std::collections::HashSet;

use bevy_ecs::prelude::*;
use sophia_api::term::{Term as _, TermKind};

use crate::{
    lsp_types::{
        DocumentLink, DocumentSymbol, FoldingRange, FoldingRangeKind, Location, SymbolInformation,
        SymbolKind, Url,
    },
    prelude::*,
    systems::{import_url, namespace_sources, ConfiguredOntology, PreferredPrefixes},
    util::{ns::owl, range_to_range},
};

const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

/// At most this many workspace symbols are returned.
pub const MAX_WORKSPACE_SYMBOLS: usize = 256;

/// Anonymous blank nodes (`[ … ]`) are part of their parent's block; they get
/// no symbol of their own.
fn is_anonymous(term: &MyTerm<'_>) -> bool {
    term.kind() == TermKind::BlankNode && term.value.starts_with("internal_bnode")
}

fn name(term: &MyTerm<'_>, prefixes: Option<&Prefixes>) -> String {
    match term.kind() {
        TermKind::Iri => prefixes
            .and_then(|p| p.shorten(&term.value))
            .unwrap_or_else(|| format!("<{}>", term.value)),
        TermKind::BlankNode => format!("_:{}", term.value),
        TermKind::Literal => {
            let v: String = term.value.chars().take(60).collect();
            let more = if term.value.chars().count() > 60 {
                "…"
            } else {
                ""
            };
            format!("\"{v}{more}\"")
        }
        _ => term.value.to_string(),
    }
}

/// Symbol kind of a subject, from its types.
fn kind_of(types: &[&str]) -> SymbolKind {
    let has = |suffixes: &[&str]| {
        types
            .iter()
            .any(|t| suffixes.iter().any(|s| t.ends_with(s)))
    };
    if has(&["#NodeShape", "#PropertyShape"]) {
        SymbolKind::STRUCT
    } else if has(&["#Class"]) {
        SymbolKind::CLASS
    } else if has(&[
        "#Property",
        "ObjectProperty",
        "DatatypeProperty",
        "AnnotationProperty",
    ]) {
        SymbolKind::PROPERTY
    } else if has(&["#Ontology"]) {
        SymbolKind::MODULE
    } else {
        SymbolKind::OBJECT
    }
}

/// Consecutive triples about the same (non-anonymous) subject: one statement
/// block, e.g. `<s> a ex:C ; ex:p 1 .`.
fn blocks<'t>(triples: &'t [MyQuad<'static>]) -> Vec<Vec<&'t MyQuad<'static>>> {
    let mut sorted: Vec<&'t MyQuad<'static>> = triples
        .iter()
        .filter(|t| !is_anonymous(&t.subject))
        .collect();
    sorted.sort_by_key(|t| t.span.start);
    let mut out: Vec<Vec<&'t MyQuad<'static>>> = Vec::new();
    for t in sorted {
        match out.last_mut() {
            Some(block) if block[0].subject == t.subject => block.push(t),
            _ => out.push(vec![t]),
        }
    }
    out
}

/// Byte span covering a block: from its first triple to its furthest end.
fn block_span(block: &[&MyQuad<'static>]) -> std::ops::Range<usize> {
    let start = block
        .iter()
        .map(|t| t.span.start.min(t.subject.span.start))
        .min()
        .unwrap_or_default();
    let end = block
        .iter()
        .map(|t| t.span.end.max(t.object.span.end))
        .max()
        .unwrap_or_default();
    start..end
}

fn components(world: &World, entity: Entity) -> Option<(&Triples, &RopeC, Option<&Prefixes>)> {
    let e = world.get_entity(entity).ok()?;
    Some((e.get::<Triples>()?, e.get::<RopeC>()?, e.get::<Prefixes>()))
}

/// Links from a document to what it refers to: each `owl:imports` IRI to the
/// imported document, each `@prefix` IRI to the ontology loaded for it.
pub fn document_links(world: &mut World, url: &Url) -> Vec<DocumentLink> {
    let Some(entity) = find_document(world, url) else {
        return Vec::new();
    };
    let fs = world.resource::<Fs>().clone();
    // Everyone the prefix links may point to (read before borrowing the document).
    let locals: Vec<(swls_lov::LocalPrefix, Option<ConfiguredOntology>)> = world
        .query::<(&swls_lov::LocalPrefix, Option<&ConfiguredOntology>)>()
        .iter(world)
        .map(|(l, c)| (l.clone(), c.copied()))
        .collect();

    let Some((triples, rope, prefixes)) = components(world, entity) else {
        return Vec::new();
    };
    let mut out = Vec::new();
    for t in triples.iter() {
        if t.predicate.value != owl::imports.iriref().as_str() {
            continue;
        }
        let Some(target) = Url::parse(&t.object.value)
            .ok()
            .and_then(|iri| import_url(&fs, &iri))
        else {
            continue;
        };
        if let Some(range) = range_to_range(&t.object.span, &rope.0) {
            out.push(DocumentLink {
                range,
                target: Some(target),
                tooltip: Some(String::from("Open the imported document")),
                data: None,
            });
        }
    }

    // Prefix declarations carry spans in the parsed model.
    let Some(element) = world.entity(entity).get::<Element>() else {
        return out;
    };
    let preferred = world.resource::<PreferredPrefixes>();
    for decl in element.value().prefixes.iter() {
        let prefix = decl.value().prefix.value();
        let Some(namespace) = prefixes
            .and_then(|p| p.iter().find(|p| &p.prefix == prefix))
            .map(|p| p.url.to_string())
        else {
            continue;
        };
        let target = namespace_sources(
            &fs,
            &namespace,
            prefix,
            preferred,
            locals.iter().map(|(l, c)| (l, c.as_ref())),
        )
        .into_iter()
        .next()
        .map(|s| s.url)
        .or_else(|| Url::parse(&namespace).ok());
        if let (Some(target), Some(range)) =
            (target, range_to_range(decl.value().value.span(), &rope.0))
        {
            out.push(DocumentLink {
                range,
                target: Some(target),
                tooltip: Some(format!("Open the ontology for {prefix}:")),
                data: None,
            });
        }
    }
    out
}

/// The outline of a document: one symbol per subject block, with its
/// predicates as children.
#[allow(deprecated)] // `DocumentSymbol::deprecated` must be set
pub fn document_symbols(world: &mut World, url: &Url) -> Vec<DocumentSymbol> {
    let Some(entity) = find_document(world, url) else {
        return Vec::new();
    };
    let Some((triples, rope, prefixes)) = components(world, entity) else {
        return Vec::new();
    };
    let mut out = Vec::new();
    for block in blocks(triples) {
        let subject = &block[0].subject;
        let types: Vec<&str> = block
            .iter()
            .filter(|t| t.predicate.value == RDF_TYPE)
            .map(|t| t.object.value.as_ref())
            .collect();
        let detail = block
            .iter()
            .filter(|t| t.predicate.value == RDF_TYPE)
            .map(|t| name(&t.object, prefixes))
            .collect::<Vec<_>>()
            .join(", ");
        let (Some(range), Some(selection_range)) = (
            range_to_range(&block_span(&block), &rope.0),
            range_to_range(&subject.span, &rope.0),
        ) else {
            continue;
        };
        let children = block
            .iter()
            .filter(|t| t.predicate.value != RDF_TYPE)
            .filter_map(|t| {
                let span = t.predicate.span.start..t.object.span.end.max(t.predicate.span.end);
                Some(DocumentSymbol {
                    name: name(&t.predicate, prefixes),
                    detail: Some(name(&t.object, prefixes)),
                    kind: SymbolKind::FIELD,
                    tags: None,
                    deprecated: None,
                    range: range_to_range(&span, &rope.0)?,
                    selection_range: range_to_range(&t.predicate.span, &rope.0)?,
                    children: None,
                })
            })
            .collect::<Vec<_>>();
        out.push(DocumentSymbol {
            name: name(subject, prefixes),
            detail: (!detail.is_empty()).then_some(detail),
            kind: kind_of(&types),
            tags: None,
            deprecated: None,
            range,
            selection_range,
            children: (!children.is_empty()).then_some(children),
        });
    }
    out
}

/// Folding ranges: multi-line subject blocks, multi-line `[ … ]`, and the
/// run of prefix declarations.
pub fn folding_ranges(world: &mut World, url: &Url) -> Vec<FoldingRange> {
    let Some(entity) = find_document(world, url) else {
        return Vec::new();
    };
    let Some((triples, rope, _)) = components(world, entity) else {
        return Vec::new();
    };
    let lines = |span: &std::ops::Range<usize>| -> Option<(u32, u32)> {
        let r = range_to_range(span, &rope.0)?;
        (r.end.line > r.start.line).then_some((r.start.line, r.end.line))
    };
    let mut seen = HashSet::new();
    let mut out = Vec::new();
    let mut push = |(start, end): (u32, u32), kind: FoldingRangeKind| {
        if seen.insert((start, end)) {
            out.push(FoldingRange {
                start_line: start,
                start_character: None,
                end_line: end,
                end_character: None,
                kind: Some(kind),
                collapsed_text: None,
            });
        }
    };

    if let Some(element) = world.entity(entity).get::<Element>() {
        let spans: Vec<_> = element
            .value()
            .prefixes
            .iter()
            .map(|p| p.span().clone())
            .collect();
        if let (Some(first), Some(last)) = (
            spans.iter().map(|s| s.start).min(),
            spans.iter().map(|s| s.end).max(),
        ) {
            if let Some(l) = lines(&(first..last)) {
                push(l, FoldingRangeKind::Imports);
            }
        }
    }
    for block in blocks(triples) {
        if let Some(l) = lines(&block_span(&block)) {
            push(l, FoldingRangeKind::Region);
        }
    }
    for t in triples.iter() {
        if t.object.kind() == TermKind::BlankNode && t.object.span.start < t.object.span.end {
            if let Some(l) = lines(&t.object.span) {
                push(l, FoldingRangeKind::Region);
            }
        }
    }
    out.sort_by_key(|f| (f.start_line, f.end_line));
    out
}

/// Subjects across every document the server knows (open, imported, cached,
/// workspace) whose name contains `query` (case-insensitive), at most
/// [`MAX_WORKSPACE_SYMBOLS`].
#[allow(deprecated)] // `SymbolInformation::deprecated` must be set
pub fn workspace_symbols(world: &mut World, query: &str) -> Vec<SymbolInformation> {
    let query = query.to_lowercase();
    let docs: Vec<(Url, Entity)> = world
        .resource::<Documents>()
        .iter()
        .map(|(u, e)| (u.clone(), e))
        .collect();
    let mut out = Vec::new();
    for (url, entity) in docs {
        let Some((triples, rope, prefixes)) = components(world, entity) else {
            continue;
        };
        let container = url
            .path_segments()
            .and_then(|mut s| s.next_back())
            .map(str::to_string);
        let mut seen = HashSet::new();
        for block in blocks(triples) {
            let subject = &block[0].subject;
            if !seen.insert(subject.value.to_string()) {
                continue;
            }
            let label = name(subject, prefixes);
            if !query.is_empty()
                && !label.to_lowercase().contains(&query)
                && !subject.value.to_lowercase().contains(&query)
            {
                continue;
            }
            let types: Vec<&str> = block
                .iter()
                .filter(|t| t.predicate.value == RDF_TYPE)
                .map(|t| t.object.value.as_ref())
                .collect();
            let Some(range) = range_to_range(&subject.span, &rope.0) else {
                continue;
            };
            out.push(SymbolInformation {
                name: label,
                kind: kind_of(&types),
                tags: None,
                deprecated: None,
                location: Location {
                    uri: url.clone(),
                    range,
                },
                container_name: container.clone(),
            });
            if out.len() >= MAX_WORKSPACE_SYMBOLS {
                return out;
            }
        }
    }
    out
}
