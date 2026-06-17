//! Shared, model-based rename for the text RDF syntaxes (Turtle / TriG / SPARQL /
//! N3 — anything whose [`Lang::Element`] is the [`Turtle`] model).
//!
//! Unlike the language-agnostic rename in `swls-core` (which slices the rope at a
//! term span and classifies the raw string), these systems work off the parsed
//! [`Turtle`] model.  Walking the model gives us:
//!
//! * exact term boundaries (no `<>`/`_:` string heuristics for *finding* the
//!   token under the cursor), and
//! * natural de-duplication — a subject written once with the `;` shorthand
//!   appears exactly once in the model, so it yields exactly one edit.
//!
//! Each text RDF language opts in via [`setup_rename`].  JSON-LD deliberately
//! stays on the core agnostic path because its concrete syntax wraps IRIs in
//! quoted strings rather than `<>`.

use std::ops::Range as StdRange;

use bevy_ecs::prelude::*;
use rdf_parsers::model::{BlankNode, NamedNode, Term, Turtle, PO};
use swls_core::{
    feature::rename::{PrepareRename, PrepareRenameRequest, Rename, RenameEdits},
    lang::Lang,
    lsp_types::TextEdit,
    prelude::*,
    util::{offsets_to_range, position_to_offset},
};

use crate::traits::NamedNodeExt;

/// Canonical identity of a renameable term.  Two occurrences are renamed
/// together iff their keys are equal.
#[derive(Clone, PartialEq, Eq)]
enum RenameKey {
    /// A (possibly prefixed) IRI, compared by its fully-expanded absolute form.
    Iri(String),
    /// A labelled blank node, compared by its label.
    Blank(String),
    /// A SPARQL variable, compared by its name.
    Var(String),
}

/// Compute the canonical key of a term, or `None` if it is not renameable
/// (literals, `a`, anonymous blank nodes, collections, invalid terms).
fn term_key(term: &Term, turtle: &Turtle) -> Option<RenameKey> {
    match term {
        Term::NamedNode(nn) => match nn {
            NamedNode::A(_) | NamedNode::Invalid => None,
            _ => nn.expand(turtle).map(RenameKey::Iri),
        },
        Term::BlankNode(BlankNode::Named(label, _)) => Some(RenameKey::Blank(label.clone())),
        Term::Variable(v) => Some(RenameKey::Var(v.0.clone())),
        _ => None,
    }
}

/// The bare text shown to the user in the rename input box for a term.
fn term_placeholder(term: &Term) -> Option<String> {
    match term {
        Term::NamedNode(NamedNode::Full(iri, _)) => Some(iri.clone()),
        Term::NamedNode(NamedNode::Prefixed { prefix, value, .. }) => {
            Some(format!("{}:{}", prefix, value))
        }
        Term::BlankNode(BlankNode::Named(label, _)) => Some(format!("_:{}", label)),
        Term::Variable(v) => Some(format!("?{}", v.0)),
        _ => None,
    }
}

/// Visit every term in the model, recursing into collections and the
/// predicate/object positions of anonymous blank nodes.
fn walk_terms(turtle: &Turtle, mut f: impl FnMut(&Spanned<Term>)) {
    fn walk(term: &Spanned<Term>, f: &mut impl FnMut(&Spanned<Term>)) {
        f(term);
        match term.value() {
            Term::Collection(items) => {
                for item in items {
                    walk(item, f);
                }
            }
            Term::BlankNode(BlankNode::Unnamed(pos, _, _)) => {
                for po in pos {
                    walk_po(po, f);
                }
            }
            _ => {}
        }
    }
    fn walk_po(po: &Spanned<PO>, f: &mut impl FnMut(&Spanned<Term>)) {
        walk(&po.value().predicate, f);
        for object in &po.value().object {
            walk(object, f);
        }
    }
    for triple in &turtle.triples {
        let triple = triple.value();
        walk(&triple.subject, &mut f);
        for po in &triple.po {
            walk_po(po, &mut f);
        }
    }
}

/// Find the innermost (smallest-span) renameable term containing `offset`,
/// returning its span, canonical key and placeholder.
///
/// Matching is done in priority order so the cursor reliably targets the term it
/// sits on, even at token boundaries:
///
/// 1. a term that *strictly* contains `offset` (`start <= offset < end`);
/// 2. a term that strictly contains `offset + 1` — this recovers the editor's
///    cursor when `Backend::adjust_position` has decremented it onto the
///    *end-boundary* of a preceding token (e.g. the space before an empty
///    `<>`), which would otherwise wrongly select that preceding token;
/// 3. as a last resort, a term whose span *ends* exactly at `offset`.
fn find_renameable_at(
    turtle: &Turtle,
    offset: usize,
) -> Option<(StdRange<usize>, RenameKey, String)> {
    find_renameable_with(turtle, offset, false)
        .or_else(|| find_renameable_with(turtle, offset + 1, false))
        .or_else(|| find_renameable_with(turtle, offset, true))
}

/// Smallest-span renameable term containing `offset`.  When `inclusive` is
/// `false` the span is treated as half-open `[start, end)`; when `true` the end
/// boundary is allowed (`[start, end]`).
fn find_renameable_with(
    turtle: &Turtle,
    offset: usize,
    inclusive: bool,
) -> Option<(StdRange<usize>, RenameKey, String)> {
    let mut best: Option<(StdRange<usize>, RenameKey, String)> = None;
    walk_terms(turtle, |term| {
        let span = term.span();
        let contains = span.start <= offset && (offset < span.end || (inclusive && offset == span.end));
        if !contains {
            return;
        }
        let value = term.value();
        let (Some(key), Some(placeholder)) = (term_key(value, turtle), term_placeholder(value))
        else {
            return;
        };
        let width = span.end - span.start;
        let is_better = best
            .as_ref()
            .map(|(bspan, _, _)| width < bspan.end - bspan.start)
            .unwrap_or(true);
        if is_better {
            best = Some((span.clone(), key, placeholder));
        }
    });
    best
}

/// Model-based `prepare_rename`: report the range and placeholder of the term
/// under the cursor.
pub fn prepare_rename<L>(
    query: Query<(Entity, &Element<L>, &RopeC, &PositionComponent)>,
    mut commands: Commands,
) where
    L: Lang<Element = Turtle> + Send + Sync + 'static,
{
    for (entity, element, rope, position) in &query {
        commands.entity(entity).remove::<PrepareRenameRequest>();

        let Some(offset) = position_to_offset(position.0, &rope.0) else {
            continue;
        };
        let Some((span, _key, placeholder)) = find_renameable_at(element.value(), offset) else {
            continue;
        };
        let Some(range) = offsets_to_range(span.start, span.end, &rope.0) else {
            continue;
        };
        commands
            .entity(entity)
            .insert(PrepareRenameRequest { range, placeholder });
    }
}

/// Model-based `rename`: replace every occurrence of the term under the cursor
/// (matched by canonical key) with the wrapped new text.
pub fn rename<L>(
    mut query: Query<(
        &Element<L>,
        &RopeC,
        &Label,
        &PositionComponent,
        &DynLang,
        &mut RenameEdits,
    )>,
) where
    L: Lang<Element = Turtle> + Send + Sync + 'static,
{
    for (element, rope, label, position, lang, mut edits) in &mut query {
        let Some(offset) = position_to_offset(position.0, &rope.0) else {
            continue;
        };
        let turtle = element.value();
        let Some((_span, key, _placeholder)) = find_renameable_at(turtle, offset) else {
            continue;
        };
        let new_text = lang.0.rename_wrap(&edits.1);

        let mut collected: Vec<TextEdit> = Vec::new();
        walk_terms(turtle, |term| {
            if term_key(term.value(), turtle).as_ref() == Some(&key) {
                if let Some(range) = offsets_to_range(term.span().start, term.span().end, &rope.0) {
                    collected.push(TextEdit {
                        range,
                        new_text: new_text.clone(),
                    });
                }
            }
        });
        for edit in collected {
            edits.0.push((label.0.clone(), edit));
        }
    }
}

/// Register the model-based [`prepare_rename`] and [`rename`] systems for
/// language `L`.  Call this from a text RDF language's `setup_world`, and make
/// the language's [`LangHelper::model_based_rename`] return `true` so the core
/// agnostic rename systems skip its documents.
pub fn setup_rename<L>(world: &mut World)
where
    L: Lang<Element = Turtle> + Send + Sync + 'static,
{
    world.schedule_scope(PrepareRename, |_, schedule| {
        schedule.add_systems(prepare_rename::<L>);
    });
    world.schedule_scope(Rename, |_, schedule| {
        schedule.add_systems(rename::<L>);
    });
}
