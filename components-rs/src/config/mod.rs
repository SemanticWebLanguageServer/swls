//! Configuration instance extraction — the concrete wirings in `config/*.jsonld` files.
//!
//! A configuration file instantiates one or more component classes by setting `@type` to a
//! component IRI and providing parameter values as JSON-LD properties.  Each such entry becomes
//! a [`registry::ConfigRegistry`] entry holding a [`crate::components::types::ConfigInstance`].
//!
//! # LSP uses
//!
//! - **Diagnostics**: cross-reference `ConfigInstance.component_type_iri` against
//!   [`crate::components::registry::ComponentRegistry`] to flag unknown `@type` values or
//!   missing required parameters.
//! - **Completion**: inside a config object, suggest the parameter IRIs of the matched
//!   component (compacted via the active context) as property keys.
//! - **Goto-definition**: `ConfigInstance.source_file` + `iri_span` lets the LSP jump to
//!   where an instance IRI is declared when it is referenced elsewhere.

pub mod registry;
