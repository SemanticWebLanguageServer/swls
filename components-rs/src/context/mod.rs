//! JSON-LD context resolution: expanding compact IRIs to full IRIs and back.
//!
//! Components.js files use a layered context system: a package's `lsd:contexts` map registers
//! context documents by IRI, and individual `components.jsonld` / `config/*.jsonld` files
//! reference those IRIs in their `@context` arrays.  This module resolves that chain so that
//! short terms like `"Class"` or `"oo:parameter"` can be expanded to their canonical IRIs.
//!
//! # Key types
//!
//! - [`expand::ContextResolver`] — per-file resolver built from a single document's `@context`.
//!   Used during component and config extraction to expand IRIs found in that file.
//! - [`expand::IriCompactor`] — project-wide, built by merging *all* known contexts.
//!   Used by the LSP to compact full IRIs back to their shortest display form for hover cards
//!   and completion labels.

pub mod expand;
