//! Which SHACL shape documents apply to a document.
//!
//! One rule, used by validation, shape templates, hover and completion:
//! a shape document applies to a document when it is
//!
//! - **global**: configured in `shapes` (marked [`Global`]),
//! - **the document itself**, or
//! - **linked**: the document links to it, through a prefix it declares or an
//!   `owl:imports` ([`DocumentLinks`]).

use bevy_ecs::prelude::*;

use crate::{prelude::*, systems::shapes::ShaclShapes};

/// Whether the shape document labelled `shapes` applies to document `doc`.
pub fn shapes_apply(shapes: &Label, global: bool, doc: &Label, links: &DocumentLinks) -> bool {
    global || shapes.0 == doc.0 || links.iter().any(|link| link.0 == shapes.0)
}

/// Shape documents, with what [`visible_shapes`] needs.
pub type ShapeDocs<'w, 's> = Query<
    'w,
    's,
    (
        &'static Label,
        &'static Triples,
        Option<&'static Global>,
        Option<&'static Prefixes>,
    ),
    With<ShaclShapes>,
>;

/// Triples and prefixes of the shape documents that apply to `doc`.
pub fn visible_shapes<'a>(
    shapes: &'a ShapeDocs,
    doc: &Label,
    links: &DocumentLinks,
) -> (Vec<&'a [MyQuad<'static>]>, Vec<&'a Prefixes>) {
    let mut triples = Vec::new();
    let mut prefixes = Vec::new();
    for (label, t, global, p) in shapes.iter() {
        if shapes_apply(label, global.is_some(), doc, links) {
            triples.push(t.0.as_slice());
            prefixes.extend(p);
        }
    }
    (triples, prefixes)
}
