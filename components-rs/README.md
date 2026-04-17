# components-js

A Rust library for analyzing [Components.js](https://github.com/LinkedSoftwareDependencies/Components.js/) projects — discovering modules, component classes, configurations, and their relationships.

## Overview

Components.js is a semantic dependency injection framework for TypeScript and JavaScript. This crate provides tooling to statically analyze Components.js projects: parsing component files, resolving JSON-LD contexts, and building registries of modules, classes, and configuration instances.

## Features

- **Module discovery** — scan `node_modules` for Components.js module definitions
- **Component registry** — build a typed registry of classes, abstract classes, and their parameters
- **Config resolution** — discover and parse configuration instances
- **Context expansion** — resolve JSON-LD `@context` references
- **Pluggable filesystem** — abstract `Fs` trait for custom backends (in-memory, WASM, etc.)

## Feature flags

| Feature | Default | Description |
|---------|---------|-------------|
| `tokio` | ✓ | Enables `OsFs`, an `Fs` implementation backed by `tokio::fs` |
| `cli` | | Enables the `components-js` binary (adds `clap`, `anyhow`, `tracing-subscriber`) |

## Library usage

```toml
[dependencies]
components-js = { version = "0.1", default-features = false, features = ["tokio"] }
```

```rust
use components_js::fs::OsFs;
use components_js::module_state::ModuleState;
use components_js::components::registry::ComponentRegistry;

let fs = OsFs;
let state = ModuleState::build(&fs, "path/to/project".as_ref()).await?;

let mut registry = ComponentRegistry::new();
registry.register_available_modules(&fs, &state).await?;
registry.finalize();
```

## CLI usage

```sh
cargo install components-js --features cli

components-js /path/to/project summary
components-js /path/to/project list-classes --json
```

## Acknowledgements

This project is a Rust reimplementation of analysis tooling for
[Components.js](https://github.com/LinkedSoftwareDependencies/Components.js/)
by [Linked Software Dependencies](https://github.com/LinkedSoftwareDependencies).

## License

[MIT](LICENSE)
