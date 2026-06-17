//! Shared, language-agnostic code actions for the text RDF syntaxes
//! (Turtle / TriG / SPARQL / N3 — anything whose [`Lang::Element`] is the
//! [`Turtle`] model).
//!
//! These systems are generic over the language marker `L` and are opted into by
//! each language via [`setup_blank_node_code_action`].  JSON-LD deliberately does
//! **not** register them, because its concrete syntax is JSON rather than the
//! `[ … ]` / `_:bN` blank-node forms these actions produce.

use std::collections::HashMap;
use std::ops::Range as StdRange;

use bevy_ecs::prelude::*;
use rdf_parsers::model::{BlankNode, Term, Turtle, PO};
use swls_core::{
    feature::code_action::{CodeActionRequest, Label as CodeActionLabel},
    lang::Lang,
    lsp_types::{CodeAction, CodeActionKind, Range, TextEdit, WorkspaceEdit},
    prelude::*,
    util::{offset_to_position, offsets_to_range, position_to_offset},
};

/// Recursively search a term for the innermost *unnamed* blank node whose span
/// contains `offset`.  Records `(span, has_content)` of the best (deepest) match.
fn find_unnamed_in_term(
    term: &Spanned<Term>,
    offset: usize,
    best: &mut Option<(StdRange<usize>, bool)>,
) {
    let span = term.span();
    if offset < span.start || offset > span.end {
        return;
    }
    match term.value() {
        Term::BlankNode(BlankNode::Unnamed(pos, _, _)) => {
            // Deeper (nested) matches overwrite shallower ones → innermost wins.
            *best = Some((span.clone(), !pos.is_empty()));
            for po in pos {
                find_unnamed_in_po(po, offset, best);
            }
        }
        Term::Collection(items) => {
            for item in items {
                find_unnamed_in_term(item, offset, best);
            }
        }
        _ => {}
    }
}

fn find_unnamed_in_po(po: &Spanned<PO>, offset: usize, best: &mut Option<(StdRange<usize>, bool)>) {
    let po = po.value();
    find_unnamed_in_term(&po.predicate, offset, best);
    for object in &po.object {
        find_unnamed_in_term(object, offset, best);
    }
}

/// Pick a fresh `_:bN` blank-node label that does not already occur in `source`.
fn fresh_blank_label(source: &str) -> String {
    let mut n = 0;
    loop {
        let candidate = format!("_:b{}", n);
        if !source.contains(&candidate) {
            return candidate;
        }
        n += 1;
    }
}

/// Code action: when the cursor sits inside an anonymous blank node `[ … ]`,
/// offer to extract it into a labelled blank node (`_:bN`) declared as a separate
/// statement.  For example:
///
/// ```turtle
/// <#s> foaf:knows [ foaf:name "Alice" ] .
/// ```
///
/// becomes
///
/// ```turtle
/// <#s> foaf:knows _:b0 .
/// _:b0 foaf:name "Alice" .
/// ```
///
/// This is generic over the language marker `L`; every text RDF syntax whose
/// parsed model is [`Turtle`] (Turtle, TriG, SPARQL, N3, …) can reuse it.
pub fn extract_blank_node<L>(
    mut query: Query<(
        &Element<L>,
        &Source,
        &RopeC,
        &Label,
        &PositionComponent,
        &mut CodeActionRequest,
    )>,
) where
    L: Lang<Element = Turtle> + Send + Sync + 'static,
{
    for (element, source, rope, label, position, mut req) in &mut query {
        let Some(offset) = position_to_offset(position.0, &rope.0) else {
            continue;
        };
        let turtle: &Turtle = element.value();

        let mut best: Option<(StdRange<usize>, bool)> = None;
        for triple in &turtle.triples {
            let triple = triple.value();
            find_unnamed_in_term(&triple.subject, offset, &mut best);
            for po in &triple.po {
                find_unnamed_in_po(po, offset, &mut best);
            }
        }

        let Some((bspan, has_content)) = best else {
            continue;
        };
        // Extracting an empty `[]` would yield an invalid `_:bN .` statement.
        if !has_content {
            continue;
        }

        // Pull the predicate-object content out of the `[ … ]` source slice.
        let Some(raw) = source.0.get(bspan.start..bspan.end) else {
            continue;
        };
        let inner = raw.trim();
        let inner = inner.strip_prefix('[').unwrap_or(inner);
        let inner = inner.strip_suffix(']').unwrap_or(inner);
        let inner = inner.trim();
        if inner.is_empty() {
            continue;
        }

        let bnode = fresh_blank_label(&source.0);

        // Edit 1: replace the inline blank node with the new label.
        let Some(replace_range) = offsets_to_range(bspan.start, bspan.end, &rope.0) else {
            continue;
        };
        let replace_edit = TextEdit {
            range: replace_range,
            new_text: bnode.clone(),
        };

        // Edit 2: append the extracted statement at the end of the document.
        let end_offset = source.0.len();
        let Some(end_pos) = offset_to_position(end_offset, &rope.0) else {
            continue;
        };
        let needs_leading_newline = !source.0.ends_with('\n');
        let append_edit = TextEdit {
            range: Range::new(end_pos, end_pos),
            new_text: format!(
                "{}{} {} .\n",
                if needs_leading_newline { "\n" } else { "" },
                bnode,
                inner
            ),
        };

        let mut changes = HashMap::new();
        changes.insert(label.0.clone(), vec![replace_edit, append_edit]);

        req.0.push(CodeAction {
            title: String::from("Extract blank node into named blank node"),
            kind: Some(CodeActionKind::REFACTOR_EXTRACT),
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                ..Default::default()
            }),
            ..Default::default()
        });
    }
}

/// Find the innermost `BlankNode::Named` term whose span contains `offset`,
/// returning its label and span.  Searches subjects, predicates and objects
/// recursively (into nested `[ … ]` and collections).
fn find_named_at(turtle: &Turtle, offset: usize) -> Option<(String, StdRange<usize>)> {
    fn walk(
        term: &Spanned<Term>,
        offset: usize,
        best: &mut Option<(String, StdRange<usize>)>,
    ) {
        let span = term.span();
        if offset < span.start || offset > span.end {
            return;
        }
        match term.value() {
            Term::BlankNode(BlankNode::Named(name, _)) => {
                *best = Some((name.clone(), span.clone()));
            }
            Term::BlankNode(BlankNode::Unnamed(pos, _, _)) => {
                for po in pos {
                    walk(&po.value().predicate, offset, best);
                    for object in &po.value().object {
                        walk(object, offset, best);
                    }
                }
            }
            Term::Collection(items) => {
                for item in items {
                    walk(item, offset, best);
                }
            }
            _ => {}
        }
    }

    let mut best = None;
    for triple in &turtle.triples {
        let triple = triple.value();
        walk(&triple.subject, offset, &mut best);
        for po in &triple.po {
            walk(&po.value().predicate, offset, &mut best);
            for object in &po.value().object {
                walk(object, offset, &mut best);
            }
        }
    }
    best
}

/// Collect every reference (span) to the named blank node `name` reachable from
/// `term`, recursing into nested blank nodes and collections.
fn collect_named_refs(term: &Spanned<Term>, name: &str, out: &mut Vec<StdRange<usize>>) {
    match term.value() {
        Term::BlankNode(BlankNode::Named(n, _)) if n == name => out.push(term.span().clone()),
        Term::BlankNode(BlankNode::Unnamed(pos, _, _)) => {
            for po in pos {
                collect_named_refs(&po.value().predicate, name, out);
                for object in &po.value().object {
                    collect_named_refs(object, name, out);
                }
            }
        }
        Term::Collection(items) => {
            for item in items {
                collect_named_refs(item, name, out);
            }
        }
        _ => {}
    }
}

/// Code action: the inverse of [`extract_blank_node`].  When the cursor sits on a
/// labelled blank node `_:bN` that is *defined* by exactly one statement and
/// *referenced* in exactly one other place, inline the definition back into an
/// anonymous blank node `[ … ]`.  For example:
///
/// ```turtle
/// <#s> foaf:knows _:b0 .
/// _:b0 foaf:name "Alice" .
/// ```
///
/// becomes
///
/// ```turtle
/// <#s> foaf:knows [ foaf:name "Alice" ] .
/// ```
///
/// Generic over the language marker `L`, mirroring [`extract_blank_node`].
pub fn inline_blank_node<L>(
    mut query: Query<(
        &Element<L>,
        &Source,
        &RopeC,
        &Label,
        &PositionComponent,
        &mut CodeActionRequest,
    )>,
) where
    L: Lang<Element = Turtle> + Send + Sync + 'static,
{
    for (element, source, rope, label, position, mut req) in &mut query {
        let Some(offset) = position_to_offset(position.0, &rope.0) else {
            continue;
        };
        let turtle: &Turtle = element.value();

        let Some((name, _)) = find_named_at(turtle, offset) else {
            continue;
        };

        // Locate the single defining statement (`_:name <po> .`) and gather every
        // reference to `_:name` everywhere else.
        let mut def: Option<(usize, StdRange<usize>)> = None;
        let mut refs: Vec<StdRange<usize>> = Vec::new();
        for (idx, triple) in turtle.triples.iter().enumerate() {
            let tvalue = triple.value();
            let is_def = matches!(
                tvalue.subject.value(),
                Term::BlankNode(BlankNode::Named(n, _)) if *n == name
            );
            if is_def && def.is_none() {
                def = Some((idx, tvalue.subject.span().clone()));
            } else {
                collect_named_refs(&tvalue.subject, &name, &mut refs);
            }
            // Always scan predicate/object positions for references (including the
            // definition's own objects, so a self-reference disqualifies inlining).
            for po in &tvalue.po {
                collect_named_refs(&po.value().predicate, &name, &mut refs);
                for object in &po.value().object {
                    collect_named_refs(object, &name, &mut refs);
                }
            }
        }

        // Inlining only makes sense with exactly one definition and one reference.
        let Some((def_idx, _)) = def else {
            continue;
        };
        if refs.len() != 1 {
            continue;
        }
        let ref_span = refs.into_iter().next().unwrap();

        // Extract the predicate-object source of the definition (everything after
        // the subject up to, but excluding, the closing `.`).
        let def_triple = turtle.triples[def_idx].value();
        let def_span = turtle.triples[def_idx].span();
        let inner_start = def_triple.subject.span().end;
        let Some(inner_raw) = source.0.get(inner_start..def_span.end) else {
            continue;
        };
        let inner = inner_raw.trim();
        let inner = inner.strip_suffix('.').unwrap_or(inner).trim();
        if inner.is_empty() {
            continue;
        }

        // Compute the deletion range for the whole definition statement, consuming
        // surrounding line whitespace/newline for a clean removal.
        let mut del_start = def_span.start;
        let mut del_end = def_span.end;
        let bytes = source.0.as_bytes();
        while del_end < bytes.len() && matches!(bytes[del_end], b' ' | b'\t') {
            del_end += 1;
        }
        if del_end < bytes.len() && bytes[del_end] == b'\r' {
            del_end += 1;
        }
        if del_end < bytes.len() && bytes[del_end] == b'\n' {
            del_end += 1;
        }
        while del_start > 0 && matches!(bytes[del_start - 1], b' ' | b'\t') {
            del_start -= 1;
        }

        // Guard against overlapping edits (e.g. definition and reference share a
        // line); inlining would corrupt the document, so skip the action.
        if del_start < ref_span.end && ref_span.start < del_end {
            continue;
        }

        let Some(ref_range) = offsets_to_range(ref_span.start, ref_span.end, &rope.0) else {
            continue;
        };
        let Some(del_range) = offsets_to_range(del_start, del_end, &rope.0) else {
            continue;
        };

        let replace_edit = TextEdit {
            range: ref_range,
            new_text: format!("[ {} ]", inner),
        };
        let delete_edit = TextEdit {
            range: del_range,
            new_text: String::new(),
        };

        let mut changes = HashMap::new();
        changes.insert(label.0.clone(), vec![replace_edit, delete_edit]);

        req.0.push(CodeAction {
            title: String::from("Inline named blank node"),
            kind: Some(CodeActionKind::REFACTOR_INLINE),
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                ..Default::default()
            }),
            ..Default::default()
        });
    }
}

/// Register the [`extract_blank_node`] and [`inline_blank_node`] code actions for
/// language `L` in the shared `CodeAction` schedule.  Call this from a text RDF
/// language's `setup_world`.
pub fn setup_blank_node_code_action<L>(world: &mut World)
where
    L: Lang<Element = Turtle> + Send + Sync + 'static,
{
    world.schedule_scope(CodeActionLabel, |_, schedule| {
        schedule.add_systems((extract_blank_node::<L>, inline_blank_node::<L>));
    });
}
