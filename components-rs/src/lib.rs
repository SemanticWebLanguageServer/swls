//! Static analysis library for [Components.js](https://componentsjs.readthedocs.io/) projects.
//!
//! Components.js is a semantic dependency injection framework for TypeScript/JavaScript that
//! describes classes and their constructor parameters in JSON-LD files (`components.jsonld`) and
//! wires concrete instances together in configuration files (`config/*.jsonld`).
//!
//! This crate reads those files and produces typed Rust structs that a language server can use
//! to implement:
//!
//! - **Autocompletion** of `@type` values and parameter keys in config files
//! - **Hover documentation** from `rdfs:comment` strings
//! - **Goto-definition** via byte-range spans (`iri_span`) stored on every extracted type
//! - **Diagnostics** for missing required parameters or unknown component IRIs
//!
//! # Typical usage (in an LSP)
//!
//! ```ignore
//! use components_rs::{fs::OsFs, module_state::ModuleState,
//!                     components::registry::ComponentRegistry,
//!                     config::registry::ConfigRegistry};
//!
//! let fs = OsFs;
//! let state = ModuleState::build(&fs, &project_root).await?;
//!
//! let mut comp_reg = ComponentRegistry::new();
//! comp_reg.register_available_modules(&fs, &state).await?;
//! comp_reg.finalize(); // resolves inherited parameters
//!
//! let mut cfg_reg = ConfigRegistry::new();
//! cfg_reg.discover_configs(&fs, &state).await?;
//! ```
//!
//! # Module map
//!
//! | Module | Purpose |
//! |--------|---------|
//! | [`module_state`] | Entry point — discovers all packages and builds the shared lookup tables |
//! | [`components`] | Extracted component classes, parameters, and modules |
//! | [`config`] | Extracted configuration instances (concrete wirings) |
//! | [`context`] | JSON-LD context resolution and IRI expansion/compaction |
//! | [`discovery`] | Low-level `node_modules` traversal and `package.json` parsing |
//! | [`fs`] | Abstract filesystem trait (swap in-memory impl for WASM/tests) |
//! | [`error`] | Shared error type |

pub mod components;
pub mod config;
pub mod context;
pub mod discovery;
pub mod error;
pub mod fs;
pub mod module_state;
