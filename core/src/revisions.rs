//! Change tracking for data shared by all documents.
//!
//! Most derived data belongs to one document and is recomputed when that
//! document's triples change (`Changed<Triples>`). Some data is shared:
//! ontologies loaded in the background, the class hierarchy, SHACL shapes,
//! configured sources. It usually arrives *after* the documents that use it
//! were opened, and a change to it can affect every open document.
//!
//! [`Revisions`] counts changes to each kind of shared data:
//!
//! - **producers** bump a counter when (and only when) their data changed,
//! - **consumers** remember the revision they last processed in a
//!   `Local<Seen>`; when it moved they process every open document, otherwise
//!   only documents whose own triples changed.
//!
//! ```ignore
//! fn consumer(
//!     docs: Query<(Ref<Triples>, …), With<Open>>,
//!     revisions: Res<Revisions>,
//!     mut seen: Local<Seen>,
//! ) {
//!     let all = seen.update(revisions.shapes);
//!     for (triples, …) in &docs {
//!         if !all && !triples.is_changed() { continue; }
//!         …
//!     }
//! }
//! ```

use bevy_ecs::prelude::*;

/// Revision counters of data shared by all documents. See the module docs.
#[derive(Resource, Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Revisions {
    /// Triples in the shared store (background documents) changed.
    pub store: u64,
    /// Known classes and properties ([`Ontologies`](crate::prelude::Ontologies)) changed.
    pub ontologies: u64,
    /// The class hierarchy ([`TypeHierarchy`](crate::prelude::TypeHierarchy)) changed.
    pub hierarchy: u64,
    /// SHACL shapes were added, changed or removed.
    pub shapes: u64,
    /// Configured ontology sources were added.
    pub sources: u64,
}

/// The revision a consumer last processed. Use as `Local<Seen>`, one per
/// counter the system depends on.
#[derive(Debug, Default)]
pub struct Seen(Option<u64>);

impl Seen {
    /// Whether `revision` is new to this consumer, and remember it.
    ///
    /// The first call returns `true` only when something already changed
    /// (`revision > 0`), so a consumer does not redo all documents at startup.
    pub fn update(&mut self, revision: u64) -> bool {
        let new = self.0.map_or(revision > 0, |seen| seen != revision);
        self.0 = Some(revision);
        new
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn seen_reports_each_new_revision_once() {
        let mut seen = Seen::default();
        assert!(!seen.update(0));
        assert!(!seen.update(0));
        assert!(seen.update(1));
        assert!(!seen.update(1));
        assert!(seen.update(3));

        let mut late = Seen::default();
        assert!(late.update(2), "a consumer that starts late catches up");
    }
}
