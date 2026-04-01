# AGENT.md — Semantic Web Language Server (swls)

> Quick reference for AI coding agents working in this repository.

---

## What Is This Project?

**swls** is a Language Server Protocol (LSP) server for Semantic Web languages, providing IDE-like features (completion, hover, diagnostics, formatting, rename, etc.) for **Turtle**, **JSON-LD**, and **SPARQL** in any LSP-compatible editor (VS Code, NeoVim, JetBrains).

The server binary communicates over stdio using the LSP protocol (via `tower-lsp`). It also compiles to WASM for browser/VS Code extension use.

- **Published crate / binary**: `swls`
- **Repository**: https://github.com/semanticweblanguageserver/swls
- **Paper citation**: ESWC 2025 – "The Semantic Web Language Server"

---

## Workspace Layout

```
swls/                        ← workspace root
├── core/                    ← swls-core: ECS framework, all shared types, backend, features
├── swls/                    ← swls: native binary (main.rs) and TowerClient
├── swls-token-helpers/      ← shared tokenizer/parser helpers (chumsky combinators, token defs)
├── lang-turtle/             ← swls-lang-turtle: Turtle (.ttl) language support
├── lang-sparql/             ← swls-lang-sparql: SPARQL (.sq/.rq/.sparql) language support
├── lang-jsonld/             ← swls-lang-jsonld: JSON-LD (.jsonld) language support
├── lov/                     ← swls-lov: bundled prefix/ontology metadata (LocalPrefix)
├── test-utils/              ← shared test harness (TestClient, TestFs, setup_world, create_file)
├── conformance/             ← W3C conformance tests for Turtle parser (generated via build.rs)
├── turtle/                  ← A* error-recovering parser (excluded from workspace, path dep)
├── prefixes/                ← (submodule/data) prefix data for lov
├── jetbrains/               ← JetBrains IDE plugin (separate Kotlin/Gradle project)
└── examples/                ← usage examples
```

All Rust crates share `[workspace.dependencies]` from the root `Cargo.toml`.

> **Note:** The `turtle/` directory is **excluded** from the workspace (`exclude = ["turtle"]` in root `Cargo.toml`) because it has the same package name as the upstream crate. It is consumed as a path dependency by `lang-turtle`, `lang-sparql`, and `core`. Do not add it to the workspace members list.

---

## The `turtle/` A* Parser Crate

The `turtle/` directory is a local copy of an A* error-recovering parser for Turtle and SPARQL. It is the **primary parsing engine** for both `lang-turtle` and `lang-sparql`.

### Key APIs

```rust
// Parse a Turtle document incrementally
let (parse, new_prev) = turtle::parse_incremental(
    Rule::new(SyntaxKind::Turtle),   // or SyntaxKind::QueryUnit for SPARQL
    source,
    prev,                            // Option<&PrevParseInfo> — None for fresh parse
    IncrementalBias::default(),
);

// Get the rowan CST
let syntax: SyntaxNode<Lang> = parse.syntax::<Lang>();

// Convert to model
let turtle_model: turtle::model::Turtle = turtle::convert(&syntax);        // Turtle
let turtle_model: turtle::model::Turtle = turtle::sparql::convert::convert(&syntax);  // SPARQL
```

### Model Types (`turtle::model::*`)

- `Turtle { base, set_base, prefixes, triples }` — document root
- `Triple { subject, predicate_objects }` — one subject with all its PO pairs
- `Term` — `NamedNode | BlankNode | Literal | Variable | Collection | ...`
- `TurtlePrefix { name, value }` — prefix declaration
- `Spanned<T>` — `(value: T, range: Range<usize>)` with `Deref<Target=T>` and utility methods

### Error Recovery

The A* parser always produces a complete CST even for invalid input. Errors are represented as:
- `SyntaxNode` with `kind == SyntaxKind::Error` (structural error recovery)
- `SyntaxToken` with `kind == SyntaxKind::Error` (lexer-level invalid token)

**Always use `children_with_tokens()` + `NodeOrToken` matching** when walking the CST to detect errors — `.children()` alone misses token-level errors.

### Stack Overflow Prevention

`turtle::List<T>` is a recursive cons-list. The default recursive `Drop` causes stack overflows on large files. Fixed by `turtle::list::drop_list<T>` (iterative walk via `Rc::try_unwrap`), called in `Parse::from_steps`.

---


## Architecture: ECS-Driven LSP

The core design is **Entity Component System (ECS)** using **Bevy ECS** (`bevy_ecs`). Each open document is an ECS **Entity**. Derived data (tokens, triples, prefixes, completions, hover info, diagnostics) are **Components** attached to that entity. LSP requests are modelled as **Schedules** that run ECS systems.

### Request Lifecycle

1. An LSP request arrives at `Backend` (in `core/src/backend.rs`), which implements `tower_lsp::LanguageServer`.
2. `Backend` sends a `CommandQueue` to a tokio channel that feeds the single-threaded `World`.
3. The command inserts request-specific components (e.g., `CompletionRequest`) on the entity, then runs the relevant **Schedule** (e.g., `CompletionLabel`).
4. Systems in that schedule read existing components and populate the request component.
5. The result is returned to the LSP client.

### Schedule Labels (features)

All defined in `core/src/feature/`:

| Label | LSP Feature |
|---|---|
| `ParseLabel` | Tokenize + parse document |
| `CompletionLabel` | Code completion |
| `HoverLabel` | Hover info |
| `DiagnosticsLabel` | Publish diagnostics |
| `FormatLabel` | Document formatting |
| `RenameLabel` / `PrepareRenameLabel` | Rename symbol |
| `ReferencesLabel` | Find references |
| `GotoDefinitionLabel` | Go to definition |
| `GotoTypeLabel` | Go to type definition |
| `CodeActionLabel` | Code actions |
| `SemanticLabel` | Semantic token highlighting |
| `InlayLabel` | Inlay hints |
| `SaveLabel` | On-save actions |
| `Tasks` | Async background tasks |
| `Startup` | One-time world initialization |

### Key Components (in `core/src/components/`)

- `Source(String)` — raw document text
- `RopeC(Rope)` — efficient rope representation for edits
- `Label(Url)` — document URL, used as entity identity key
- `Element<L>` — parsed semantic element (language-specific AST root)
- `Tokens` — tokenization output
- `Triples` — parsed RDF triples (sophia_api quads)
- `Prefixes` — prefix/namespace map for the document
- `Open` — marker: document is currently open in the editor
- `Dirty` — marker: document has unparsed changes
- `DocumentLinks` — referenced documents (via `owl:imports` or prefix imports)
- `PositionComponent` — cursor position injected during a request
- `DynLang(Box<dyn LangHelper>)` — language-specific helper (keywords, text extraction)
- `DefinedClass`, `DefinedClasses`, `DefinedProperty`, `DefinedProperties` — derived ontology info
- `CompletionRequest`, `HoverRequest`, `FormatRequest`, etc. — per-request response accumulators
- `TokenComponent`, `TripleComponent` — cursor-specific context injected during request handling

### Key Resources

- `SemanticTokensDict` — maps `SemanticTokenType` → index
- `Ontologies` — known ontology data loaded at startup
- `OntologyExtractor` — async ontology fetching with LOV API caching
- `TypeHierarchy` — RDF class hierarchy (subclass relationships)
- `DiagnosticPublisher` — sends diagnostics back to client via channel
- `CommandSender` / `CommandReceiver` — channel for sending `CommandQueue` to the world
- `Fs(Arc<dyn FsTrait>)` — file system abstraction (native vs. WASM)
- `ServerConfig` — top-level config (enabled languages, workspaces); contains `LocalConfig` and `CompletionConfig`

---

## Language Trait System

Each language implements two traits (in `core/src/lang.rs`):

### `Lang` trait

Implemented by `TurtleLang`, `JsonLd`, `Sparql`:

```rust
pub trait Lang: 'static {
    type Token: PartialEq + Hash + Clone + Send + Sync + TokenTrait;
    type TokenError: Into<SimpleDiagnostic> + ...;
    type Element: Send + Sync;        // parsed AST root
    type ElementError: Into<SimpleDiagnostic> + ...;

    const CODE_ACTION: bool;
    const HOVER: bool;
    const LANG: &'static str;        // e.g. "turtle", "sparql", "jsonld"
    const TRIGGERS: &'static [&'static str];  // completion trigger chars
    const LEGEND_TYPES: &'static [SemanticTokenType];
    const PATTERN: Option<&'static str>;
}
```

### `LangHelper` trait

Provides runtime language-specific behavior: keywords list and text extraction from tokens (e.g., JSON-LD strips quotes from string tokens).

### Adding a New Language

1. Create a new crate `lang-xxx/`.
2. Define a marker component (`struct MyLang`) and implement `Lang` and `LangHelper`.
3. Register a `CreateEvent` observer that matches by `language_id` or file extension.
4. Implement `setup_world(world: &mut World)` and call it from `swls/src/main.rs`.

---

## Crate Details

### `core` (`swls-core`)

The backbone. Contains:
- `backend.rs` — `Backend` struct, implements `tower_lsp::LanguageServer`, dispatches all requests.
- `components/` — all ECS components and resources.
- `feature/` — one module per LSP feature; each has a `setup_schedule()` fn and a `Label` schedule.
- `systems/` — generic systems: `spawn_or_insert`, `handle_tasks`, `derive_classes`, `derive_ontologies`, `complete_properties`, `complete_class`, `fetch_lov_properties`, `open_imports`, etc.
  - `shapes/` — **SHACL shape validation** (feature-gated with `shapes` feature flag); uses `shacl_validation`, `shacl_ir`, `shacl_ast` crates; supports multi-file shape compilation and global shapes
- `store.rs` — Oxigraph RDF 1.2 store wrapper (used for SPARQL queries and SHACL validation)
- `lang.rs` — `Lang` and `LangHelper` traits, `TokenTrait`.
- `client.rs` — `Client` (async) and `ClientSync` traits; platform abstraction.
- `util/` — range conversions, `Spanned<T>`, token types, triple utilities.
- `store.rs` — document store utilities.
- `prelude.rs` — everything commonly needed; use `swls_core::prelude::*`.

### `swls` (binary crate)

- `main.rs` — entry point; builds tokio runtime, sets up `World` with all language plugins, launches `tower_lsp::Server` over stdio.
- `client.rs` — `TowerClient` (wraps `tower_lsp::Client`), `BinFs` (native filesystem).
- `timings.rs` — tracing layer for request timing.
- Logs go to `$TMPDIR/turtle-lsp.txt` (falls back to stderr).

### `lang-turtle` (`swls-lang-turtle`)

Uses the **A\* error-recovering parser** from the `turtle/` crate (not chumsky).

- `lang/parser.rs` — wraps `turtle::parse_incremental` (Turtle grammar); defines `TurtleParseError`, `PrevParseInfo`; `parse_new(source, base_url, prev) -> (Turtle, Vec<TurtleParseError>, PrevParseInfo)`. `collect_errors` walks the CST via `children_with_tokens()` to catch both Error nodes and Error tokens.
- `lang/model.rs` — extension-trait layer over `turtle::model::*`: `TurtleExt` (get_simple_triples, get_prefixes, get_base), `TriplesBuilder` (subject/predicate/object expansion), `NamedNodeExt`, `TurtlePrefixExt`, `Based`. `get_simple_triples()` is the main entry point → `Vec<MyQuad>`.
- `lang/tokenizer.rs` — logos-based tokenizer (still used for semantic tokens/formatting).
- `lang/formatter.rs` — document formatter.
- `lang/context.rs` — cursor-context helpers (still present, used by SPARQL tokenizer).
- `lang/mod.rs` — exports all submodules; provides `parse_source` compatibility shim for conformance tests.
- `ecs/parse.rs` — `parse_turtle_system`: calls `parse_new`, open docs skip incremental state (`prev=None`) to avoid A* bias; `derive_triples` calls `TurtleExt::get_simple_triples()`.
- `ecs/` — completion, formatting, code actions, hover, semantic tokens systems.
- `config.rs`, `prefix.rs` — prefix/config helpers.
- Supports: diagnostics, completion (prefixes, properties, classes), formatting, code actions, hover, semantic tokens.

**Key type aliases used throughout:**
- `turtle::model::Turtle` — parsed document model (`triples`, `prefixes`, `base`, `set_base`)
- `turtle::Spanned<T>` — re-exported as `swls_core::prelude::Spanned<T>`; wraps a value with a byte range
- `MyQuad` — `sophia_api`-compatible quad extracted from triples
- `TurtleParseError { range: Range<usize>, msg: String }` — parser error

**Incremental parse note:** For open (user-edited) documents, always pass `prev=None` to `parse_new` to avoid A* error-recovery bias from stale state. LOV (read-only) documents may reuse `PrevParseInfo`.

**Placeholder triple:** When a predicate has no object (e.g., `foaf:` with cursor after colon), `TriplesBuilder::handle_po` pushes a `MyQuad` with `MyTerm::invalid` as object. This ensures `get_current_triple` can detect predicate position for autocompletion.

### `lang-sparql` (`swls-lang-sparql`)

Uses the **A\* SPARQL parser** from `turtle::sparql` and models SPARQL as `turtle::model::Turtle` (triples + prefixes). No rich SPARQL AST — sufficient for all current LSP features.

- `ecs/mod.rs` — `parse_sparql_system`: calls `turtle::parse_incremental` with `SyntaxKind::QueryUnit`, then `turtle::sparql::convert::convert()` to get a `Turtle` model. `collect_errors` walks CST via `children_with_tokens()`. `derive_triples` calls `TurtleExt::get_simple_triples()`.
- `lang/tokenizer.rs` — logos+chumsky tokenizer for variable completion (detects `Token::Variable`). Chumsky remains as a dep for this reason only.
- `lang/mod.rs` — only exposes `tokenizer`; old `model.rs` and `parsing.rs` are deleted.
- `type Element = turtle::model::Turtle`, `type ElementError = TurtleParseError`
- Note: `CODE_ACTION = false`, no formatting. Recognized by `language_id == "sparql"` or `.sq`/`.rq` extension.

### `lang-jsonld` (`swls-lang-jsonld`)

- `lang/` — JSON tokenizer and `Json` AST parser.
- `ecs/` — parse, keyword highlight, named-node highlight.
- Completion triggers: `@` and `"`.
- Recognized by `language_id == "jsonld"` or `.jsonld` extension.
- `JsonLdHelper` overrides `get_relevant_text` to strip string quotes.

### `swls-token-helpers`

Shared `chumsky` parser combinators for tokenizing:
- `tokens()` — common tokens (brackets, delimiters, `a`, booleans, etc.)
- `keywords()` — `@prefix`, `@base`, `PREFIX`, `BASE`
- `comment()` — `# ...` comments
- `tok()` — helper to map a literal string to a token
- `t!` macro — shorthand for `impl Parser<char, T, Error = Simple<char>>`

### `lov` (`swls-lov`)

Bundled prefix/ontology metadata. Provides `LocalPrefix` component with fields: `location`, `namespace`, `content`, `name`, `title`, `rank`. The `LOCAL_PREFIXES` static slice is generated from `min_prefixes` submodule.

### `test-utils`

Shared test harness:
- `TestClient` — implements `Client` + `ClientSync`; collects logs and diagnostics; runs futures via `async-executor`.
- `TestFs` — implements `FsTrait`; uses a temp directory.
- `setup_world(client, f)` — creates a `World`, runs `Startup`, returns `(World, DiagnosticReceiver)`.
- `create_file(world, content, url, lang, bundle)` — inserts a document entity and triggers parse.
- `debug_world(world)` — prints all entities and their components.

### `conformance`

W3C Turtle conformance test suite runner. `build.rs` generates test functions from the `w3c/` directory. `test_syntax(location, is_positive)` parses a `.ttl` file and checks whether parsing succeeds/fails as expected.

---

## Key Dependencies

| Crate | Role |
|---|---|
| `bevy_ecs` | ECS world, components, systems, schedules |
| `bevy_tasks` | Task spawning |
| `tower-lsp` | LSP protocol server implementation |
| `turtle` (path dep) | A* error-recovering parser for Turtle + SPARQL; provides `turtle::model::*`, `parse_incremental`, `Spanned<T>` |
| `rowan` | CST (concrete syntax tree) underlying the A* parser output |
| `chumsky` (0.9) | Parser combinator; still used for SPARQL variable tokenizer in lang-sparql |
| `logos` (0.15) | Lexer/tokenizer generator |
| `ropey` | Rope data structure for efficient text edits |
| `sophia_api` / `sophia_iri` / `sophia_turtle` | RDF data model and quad iterators |
| `oxigraph` (0.5.3) | RDF 1.2 store with embedded SPARQL engine (used for SHACL/store) |
| `shacl_validation` / `shacl_ir` / `shacl_ast` | SHACL shape validation (feature-gated) |
| `reqwest` | HTTP fetching for ontologies/LOV |
| `serde` / `serde_json` | JSON serialization |
| `tracing` | Structured logging and timing |
| `similar` | Text diffing (for formatting edits) |
| `futures` | Async utilities, channels |
| `tokio` | Async runtime (native binary) |

---

## Build & Dev

### Common Commands

```sh
# Build the full workspace
cargo build --workspace

# Build and install the native binary (debug)
cargo install --path swls

# Run all tests (including conformance)
cargo test --workspace

# Build docs for all crates
cargo doc --workspace --lib

# Run a specific crate's tests
cargo test -p swls-core
cargo test -p swls-lang-turtle
cargo test -p conformance
```

### `Makefile.toml` (cargo-make tasks)

Run with `cargo make <task>`:

| Task | Description |
|---|---|
| `deps` | Install `wasm-bindgen-cli` |
| `install-bin` | `cargo install --path ./lsp-bin/ --debug` |
| `build-server` | Build WASM target for browser/VS Code |
| `run-app` | Run the Monaco demo app locally |
| `run-web` | Run VS Code web extension locally |
| `build-prod` | Production build of web app |
| `build-docs` | Build all Rust docs |

### Nix

`flake.nix` provides a dev shell. Use `nix develop` to get a shell with all tools.

### CI (`.github/workflows/ci.yml`)

- **build-native**: `cargo build --all-targets --workspace` + `cargo test --workspace` on `ubuntu-latest`
- **doc**: Builds and deploys docs to GitHub Pages (runs after `build-native`)
- Triggers: push/PR to `main`

---

## Testing Patterns

Tests live alongside the code or in `conformance/`. Use `test-utils` for integration-style tests:

```rust
use test_utils::{TestClient, setup_world, create_file};
use swls_lang_turtle::setup_world as turtle_setup;

let client = TestClient::new();
let (mut world, _diag_rx) = setup_world(client, |world| {
    turtle_setup::<TestClient>(world);
});
let entity = create_file(&mut world, "@prefix ex: <http://example.org/> .\n", 
    "file:///test.ttl", "turtle", ());
world.run_schedule(CompletionLabel);
// inspect world components on entity
```

`TestClient::await_futures(|| world.run_schedule(Tasks))` drains async tasks (e.g., LOV fetches).

---

## LSP Configuration (Client Side)

The server accepts `initializationOptions`:

```json
{
  "turtle": true,
  "sparql": false,
  "jsonld": true
}
```

Each key enables/disables the corresponding language plugin.

NeoVim file type detection:
- `*.ttl` → `turtle`
- `*.sq`, `*.rq`, `*.sparql` → `sparql`
- `*.jsonld` → `jsonld`

---

## Important Conventions

- **`prelude::*`** — almost all code does `use swls_core::prelude::*`; check `core/src/prelude.rs` to see what's exported.
- **Feature schedules** — to add a new system to a feature, use `world.schedule_scope(FeatureLabel, |_, schedule| { schedule.add_systems(...) })`.
- **Entity = document** — never think of entities as anything else in this codebase.
- **`Changed<T>` queries** — systems typically use `Changed<Triples>` or `Changed<Tokens>` to avoid redundant recomputation.
- **`Without<Dirty>`** — always guard derived-data systems with `Without<Dirty>` to skip documents that haven't finished parsing.
- **`shapes` feature flag** — SHACL validation in `swls-core` is gated behind `features = ["shapes"]`; not enabled by default.
- **Error handling** — errors are `SimpleDiagnostic` converted from parser errors via `Into<SimpleDiagnostic>`.
- **Logging** — use `tracing::{debug!, info!, error!}`. Logs go to `$TMPDIR/turtle-lsp.txt` (falls back to stderr).
- **Temp files** — `BinFs` stores virtual files in `/tmp/swls/<N>/` (auto-incrementing N).
- **Formatting** — `rustfmt.toml` at workspace root; run `cargo fmt`.
- **No `prefixes/` Rust crate** — the `prefixes/` dir appears to be a data submodule, not a compiled crate.
