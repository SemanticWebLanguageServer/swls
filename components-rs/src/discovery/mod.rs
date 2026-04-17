//! Low-level discovery of npm packages and their Components.js metadata.
//!
//! This module handles the filesystem traversal that happens *before* any component or config
//! parsing.  Its outputs feed directly into [`crate::module_state::ModuleState`].
//!
//! - [`node_modules`] — walks `node_modules/` trees (including scoped packages under `@scope/`)
//!   following Node.js resolution order, collecting every directory that contains a
//!   `package.json`.
//! - [`package_json`] — parses `package.json` files, extracting the `lsd:*` fields that
//!   Components.js uses: `lsd:module`, `lsd:components`, `lsd:contexts`, `lsd:importPaths`.
//!   Also handles auto-expansion of `lsd:module: true` into a full IRI.

pub mod node_modules;
pub mod package_json;
