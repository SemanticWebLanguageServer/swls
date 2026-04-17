//! Component extraction — the classes and parameters defined in `components.jsonld` files.
//!
//! # Two-phase loading
//!
//! [`registry::ComponentRegistry::register_available_modules`] uses a two-phase approach to
//! handle cross-file merging correctly:
//!
//! 1. **Collect** — every reachable `components.jsonld` (following `rdfs:seeAlso` imports) is
//!    parsed with [`rdf_parsers::jsonld::convert::parse_json`].  Nodes are merged into a flat
//!    map keyed by their expanded IRI, and `@id` byte spans are harvested for goto-definition.
//! 2. **Process** — `oo:Module` nodes are turned into [`types::CjsModule`] values; their inline
//!    `oo:component` arrays become [`types::CjsComponent`] values with fully expanded IRIs.
//!
//! After loading, call [`registry::ComponentRegistry::finalize`] to walk each component's
//! `rdfs:subClassOf` chain and merge inherited parameters in — without this step, completion
//! only shows parameters declared directly on the component.
//!
//! # Key types for the LSP
//!
//! | Type | LSP feature |
//! |------|-------------|
//! | [`types::CjsComponent`] | `@type` completion, hover docs, goto-definition |
//! | [`types::CjsParameter`] | key completion inside config objects, type hints |
//! | [`types::CjsModule`] | module IRI completion in `@context` blocks |

pub mod registry;
pub mod types;
