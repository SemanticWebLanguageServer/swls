# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 0.2.4 (2026-11-19)

### Chore

 - <csr-id-3abc6c04fab641feadce0524d5ac271ddbada6fa/> drop the redundant homepage from the manifests
   Cargo 1.100 warns that `package.homepage` equal to `package.repository` is
   redundant, once per crate. components-rs keeps its own, different homepage.

### New Features

 - <csr-id-3a2b949554225c4c7f6e8d651b2b46285ab9c053/> document links, outline, folding ranges, workspace symbols (4/4)
 - <csr-id-d72ae659eab3fdc89c5e07b21d853ceed1f4c5ec/> workspace index, watched files, saved edits, settings changes (3/4)
 - <csr-id-ba311f45d338b4428e492f03a3d46a284ed5a825/> skip git-ignored files in the workspace index
   The workspace walk now uses the `ignore` crate (ripgrep's), so it skips
   what git would ignore: `.gitignore` files at every level,
   `.git/info/exclude`, the global git excludes and `.ignore` files, also
   outside a git repository. Hidden entries, `node_modules` and `target` stay
   skipped regardless.
   
   Files created after startup follow the same rule: before indexing a new
   file, `FsTrait::is_ignored` checks the ignore files between the workspace
   folder and the file (`util::fs::is_ignored`; the global git excludes only
   apply to the startup walk).
 - <csr-id-16d398e42e117a6dde0aa406ca327310fa21870f/> document links, outline, folding ranges and workspace symbols
   Read-only views of what the server already parsed (`core::navigation`):
   
   - `textDocument/documentLink`: each `owl:imports` IRI links to the imported
     document, each `@prefix` IRI to the ontology loaded for it (the cached
     file, a configured ontology, or the namespace itself).
   - `textDocument/documentSymbol`: one symbol per subject block, with its
     types as detail and its predicates as children. The kind follows the
     types: shapes, classes, properties, ontologies, other resources.
   - `textDocument/foldingRange`: multi-line subject blocks, multi-line
     `[ … ]` blank nodes, and the run of prefix declarations (as imports).
   - `workspace/symbol`: subjects across every known document (open,
     imported, cached, workspace), matched case-insensitively, at most 256.
   
   Each can be disabled (`document_link`, `document_symbol`, `folding_range`,
   `workspace_symbol`). Anonymous blank nodes get no symbol of their own.
   
   Tests over stdio for each.
 - <csr-id-78ae1d8e1c867a5bb4343775235f6370f5668936/> workspace index, watched files, saved edits and settings changes
   Workspace index (`core::workspace`)
   - At startup, the RDF files of the workspace folders (`.ttl`, `.nt`,
     `.owl`, `.trig`, `.n3`) are loaded as background documents, so their
     classes, properties and shapes are known without opening them. Hidden
     folders, `node_modules` and `target` are skipped; at most 500 files of
     at most 5 MB per folder; RDF/XML and HTML are skipped. Clients that send
     only a `rootUri` get it as their workspace. Disable with
     `"disabled": ["workspace_index"]`. `FsTrait::walk` provides the bounded
     walk (nothing for file systems without one).
   
   Files changed on disk (`workspace/didChangeWatchedFiles`)
   - The server registers a watcher for RDF files when the client supports
     dynamic registration.
   - A changed background document is reloaded from disk, a deleted one is
     dropped (its triples and shapes too), and a new RDF file in the
     workspace is indexed. A file open in the editor keeps the editor's text.
   
   Saved edits (`textDocument/didSave`)
   - Saving one of the user's own background documents (not a cache file)
     loads its triples into the shared store, so other documents see the
     saved classes and properties; closing it restores the saved version.
     Unsaved edits still never reach the store.
   
   Settings changes (`workspace/didChangeConfiguration`)
   - The settings (either the settings object or `{ "swls": … }`) are applied
     like `initializationOptions`, combined with the config files. Newly
     configured ontologies and shapes are loaded (the new `ConfigChanged`
     schedule; loading is idempotent), and open documents are re-checked.
     Invalid settings are reported and the current ones kept.
   
   Tests over stdio: workspace files are indexed (skipping `node_modules`
   and hidden folders), watchers are registered, changed / deleted / created
   files update completions, a saved workspace file makes its changes known,
   and a settings change applies without a restart.

### Other

 - <csr-id-d0ad03704f3e76071292c0fbbffebea68fee8bba/> fewer completions, single parse, one diagnostics message per pass, shape cache (2/4)
 - <csr-id-63fddf143f877a45b1eb2eb75653432da294c879/> fetch timeouts, no panics on I/O, stdio integration tests (1/4)

### Performance

 - <csr-id-86bb35d96c579bd664660eff24dfd453d1bf50b0/> keep the shape index between requests
   Hover, completion and shape templates rebuilt the index of every visible
   shape triple on each request. `ShapeGraph` now holds shared references
   (`Arc`) to the shape documents' triples and indexes positions instead of
   borrowing, so it can outlive a request; `ShapeCache` (a `Local` of each
   system) reuses it while the visible documents and their triples are the
   same. Keyed by `Arc` identity: changed triples or another set of documents
   build a new graph.
 - <csr-id-0ae7ce3ee36354bbdd0a53ff64f6cdc2c7f550df/> send each document's diagnostics once per pass
   The publisher sends a document's full, merged diagnostics on every publish,
   once per reason (syntax, prefixes, namespaces, shapes), so one edit sent 8
   `publishDiagnostics` messages for the document. The server now lets a pass
   finish (10 ms), drains what is queued and sends each document's latest set
   once (`diagnostics::coalesce`).
 - <csr-id-ebfa4cf13e3a5555e1987387ce5ac23cb3ca59ad/> parse each new document once
   `spawn_or_insert` ran the parse schedule itself, and its callers (opening a
   document, loading a background document, configured shapes) ran it again
   right after, so every new document was parsed twice. It no longer parses;
   the callers do, once everything is added. The built-in ontologies at
   startup and the test helper `create_file`, which relied on the implicit
   parse, now parse explicitly.
 - <csr-id-d00bd89e0af70823598818095d9738379b9dadb0/> suggest undeclared prefixes only for a typed word, at most 30
   A completion at an empty predicate position returned about 3,400 items,
   nearly all of them prefixes from prefix.cc. That was slow to send and
   buried the useful suggestions.
   
   Prefixes the document declares are always suggested. Undeclared prefixes
   (built-in vocabularies, then prefix.cc in popularity order) are only
   suggested once something was typed, and at most 30
   (`MAX_UNDECLARED_PREFIXES`). Removes the commented-out previous version of
   the helper.

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 13 commits contributed to the release.
 - 12 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Document links, outline, folding ranges, workspace symbols (4/4) ([`3a2b949`](https://github.com/SemanticWebLanguageServer/swls/commit/3a2b949554225c4c7f6e8d651b2b46285ab9c053))
    - Workspace index, watched files, saved edits, settings changes (3/4) ([`d72ae65`](https://github.com/SemanticWebLanguageServer/swls/commit/d72ae659eab3fdc89c5e07b21d853ceed1f4c5ec))
    - Merge branch 'claude/correctness' into claude/cheap-features ([`0172a18`](https://github.com/SemanticWebLanguageServer/swls/commit/0172a18c294cedaabab69077e8b0b3967e27c9ae))
    - Skip git-ignored files in the workspace index ([`ba311f4`](https://github.com/SemanticWebLanguageServer/swls/commit/ba311f45d338b4428e492f03a3d46a284ed5a825))
    - Fewer completions, single parse, one diagnostics message per pass, shape cache (2/4) ([`d0ad037`](https://github.com/SemanticWebLanguageServer/swls/commit/d0ad03704f3e76071292c0fbbffebea68fee8bba))
    - Fetch timeouts, no panics on I/O, stdio integration tests (1/4) ([`63fddf1`](https://github.com/SemanticWebLanguageServer/swls/commit/63fddf143f877a45b1eb2eb75653432da294c879))
    - Document links, outline, folding ranges and workspace symbols ([`16d398e`](https://github.com/SemanticWebLanguageServer/swls/commit/16d398e42e117a6dde0aa406ca327310fa21870f))
    - Workspace index, watched files, saved edits and settings changes ([`78ae1d8`](https://github.com/SemanticWebLanguageServer/swls/commit/78ae1d8e1c867a5bb4343775235f6370f5668936))
    - Keep the shape index between requests ([`86bb35d`](https://github.com/SemanticWebLanguageServer/swls/commit/86bb35d96c579bd664660eff24dfd453d1bf50b0))
    - Send each document's diagnostics once per pass ([`0ae7ce3`](https://github.com/SemanticWebLanguageServer/swls/commit/0ae7ce3ee36354bbdd0a53ff64f6cdc2c7f550df))
    - Parse each new document once ([`ebfa4cf`](https://github.com/SemanticWebLanguageServer/swls/commit/ebfa4cf13e3a5555e1987387ce5ac23cb3ca59ad))
    - Suggest undeclared prefixes only for a typed word, at most 30 ([`d00bd89`](https://github.com/SemanticWebLanguageServer/swls/commit/d00bd89e0af70823598818095d9738379b9dadb0))
    - Drop the redundant homepage from the manifests ([`3abc6c0`](https://github.com/SemanticWebLanguageServer/swls/commit/3abc6c04fab641feadce0524d5ac271ddbada6fa))
</details>

## 0.2.3 (2026-11-18)

<csr-id-b566e5f3228a4578291186a1eaa985ade4c736ca/>
<csr-id-f0d9f31379a47547d665ee36ee7eb5a3704b5187/>
<csr-id-8f28aa0b47d503e5ea2a06a276f238850d737166/>
<csr-id-ef9b50600ab106ff4b3f6178c5f172269c9d9605/>
<csr-id-15493a8bfeea01bd5d813e13f68d9d52672081b5/>
<csr-id-8160444de14ceb8c86d3b22412e95a6b62974d46/>

### Chore

 - <csr-id-b566e5f3228a4578291186a1eaa985ade4c736ca/> fix clippy warnings and configure workspace lints
   Makes `cargo clippy --workspace --all-targets -- -D warnings` pass.
   
   - Workspace lints (`[workspace.lints.clippy]`, inherited by every crate):
     `type_complexity` and `too_many_arguments` are allowed, because Bevy
     systems take their data as parameters and nested `Query` types.
   - `cargo clippy --fix` for the mechanical lints (needless borrows, redundant
     closures and returns, `'static` on constants, …).
   - By hand:
     - `Option::is_none_or` (Rust 1.82) replaced in components-rs, whose MSRV
       is 1.75;
     - `try_next` (deprecated) replaced by `try_recv` on the command and
       diagnostic channels;
     - `MyQuad::into_oxi(_graph)` return `Option` instead of `Result<_, ()>`;
     - `PartialOrd` for `MyTerm` delegates to `Ord`;
     - `assert!(false, …)` → `panic!` in the conformance tests;
     - a few `sort_by_key`, `while let`, `is_err()` and merged `if` branches;
     - `TestFs: Default`;
     - kept the needed clone in the config loaders (clippy's suggestion does
       not compile, the value moves into a spawned future);
     - pinned a lifetime in `prefix_completion_helper` that the elided form
       could not express.
   - `wrong_self_convention` is allowed on the two public `into_triples` trait
     methods rather than renaming them across crates.

### New Features

 - <csr-id-a1dc471597db6b8c6b6ddc7d4971fa9f1c0e6a4e/> suggest predicates that shapes require first
   At a predicate position, a predicate that a SHACL property shape requires
   (`sh:minCount >= 1`) and that the subject does not have yet is now the first
   suggestion, followed by the alternatives of a required `sh:alternativePath`
   none of which the subject has. This holds whether the suggestion comes from
   the shapes or from an ontology. Such items are marked "required by shape" or
   "one of required by shape"; an ontology suggestion keeps its title after the
   marker. Predicates the subject already has are not promoted.
   
   Before, required predicates only got a "required by shape" label and were
   sorted alphabetically among the domain-matching properties, and required
   predicates that an ontology also defined were not ranked at all.
   
   Completions are now sorted the way LSP clients sort them, by sort text or
   else by label (`completion::sort_completions`, used by the server). Sorting
   by `Option<sort_text>` put every item without a sort text, such as prefix
   suggestions, first.
 - <csr-id-4ac42a280cbb4c56d40f8ad336cdb979eddd2680/> document predicates with SHACL property shapes, prefer one language
   Hover on a predicate now also shows every SHACL property shape that
   applies to it for this subject:
   
   - `sh:name` and `sh:description`, and also the property shape's
   `rdfs:label` and `rdfs:comment` (unless they repeat the SHACL text),
   - the alternatives of an `sh:alternativePath`,
   - a constraint summary: cardinality, node kind, datatype, class, `sh:in`,
   `sh:hasValue`, `sh:node` and `sh:or` / `sh:xone` shapes, pattern,
   lengths and ranges, languages, and a non-default severity.
   
   Shapes apply by the same targets as the shape templates. That code moved
   to `shapes::model`, so hover, completion and templates agree. Property
   completions get the same documentation, and predicates only the shapes
   declare are offered too, marked when a shape requires them. IRIs in this
   documentation fall back to the shape documents' prefixes and are only
   shortened to valid prefixed names. The hover can be disabled with
   `hover_shape_property`.
   
   Wherever text is shown, one language is chosen: untagged text, then
   English (`en`, `en-*`), then the first language (`util::lang_text`).
   This now applies to ontology class and property titles and
   descriptions (previously every language, in random order), SHACL
   validation messages (`sh:message`, else `rdfs:comment`), shape template
   placeholders and the new shape documentation.
   
   Fixes found on the way:
   - `MyTerm`'s sophia `Term` impl reported no datatype or language for
   literals, so every literal entered the store as a plain string. It now
   reports `xsd:string`, `rdf:langString` or the literal's datatype, and
   its language tag.
   - The conversions to oxigraph checked the datatype before the language,
   which would turn a language-tagged literal into an `rdf:langString`
   typed literal without a language.
   - Removed imports `open_imports` left behind in `lov::fetch`.
 - <csr-id-0bb608f3d344b59f4820c4ee2b3bffd43a783d41/> SHACL shape templates in completion
   At a predicate position, offer two snippet completions built from the
   SHACL shapes that apply to the subject:
   
   - "shape template: required properties": every property with
   `sh:minCount >= 1`,
   - "shape template: all properties": every property the shapes declare.
   
   Properties the subject already has are left out.
   
   Shapes apply through `sh:targetClass` (including superclasses from the
   ontologies, and implicit class targets), `sh:targetSubjectsOf`,
   `sh:targetObjectsOf` (so blank nodes such as RML predicate-object maps get
   templates) and `sh:targetNode`. A shape's properties include those of the
   shapes and property shapes it lists in `sh:node` / `sh:and`.
   
   Values are snippet placeholders shaped by the constraints:
   - `sh:hasValue` is filled in, and `sh:in` becomes a choice.
   - `sh:alternativePath` becomes a choice of predicates. Optional
   declarations it covers are folded into it, and it is satisfied by any
   one of its paths.
   - A nested value (`sh:node`, the first `sh:or` / `sh:xone` branch with a
   node shape, shapes targeting the objects of the predicate, or shapes
   for its `sh:class`) expands to `[ … ]` with its required properties,
   up to two levels deep.
   - `sh:nodeKind` / `sh:datatype` choose between `<…>`, `"…"`, typed
   literals and plain placeholders.
   
   The template is inserted at the cursor without eating a following `.` or
   `]`, replaces a partially typed predicate, and continuation lines follow
   the indentation. Templates are offered for Turtle-family languages with
   shape validation (`LangHelper::shape_templates`) and can be disabled with
   `completion_shape_template`.
 - <csr-id-3a5907c5cf5078ad1dc0cd7c13cdaf8fcff68292/> resolve ontologies by the namespace's preferred prefix name
   Ontologies were fetched and cached by the prefix name typed in the
   document. `@prefix f: <http://xmlns.com/foaf/0.1/>` fetched whatever
   vocabulary LOV calls `f` into `lov/f.ttl`, and documents using different
   names for one namespace each loaded their own copy.
   
   - `PreferredPrefixes` maps every known namespace to its preferred name:
   the built-in LOV vocabularies first, then prefix.cc, which lists the
   most popular name first. It is built once at startup.
   - `namespace_sources` uses that name for the cache file
   (`lov/<preferred>.ttl`) and the LOV lookup, so fetching, links and
   goto-definition agree and every document shares one ontology document.
   The typed name is only used for namespaces that are not known.
   - A vocabulary fetched from LOV by name must mention the namespace it is
   loaded for (either scheme). Otherwise it is ignored and not cached, so
   a guessed name can no longer load an unrelated vocabulary.
 - <csr-id-18f6bde08999c3f6bb77f64ce2f4e0a19a6ac20a/> load remote owl:imports and make imported documents navigable
   `open_imports` read every import through `Fs::read_file`, which only
   handles `file:` URLs, so remote imports never loaded. Import handling now
   lives in `systems::imports`:
   
   - `http(s):` imports are fetched with content negotiation (Turtle,
   N-Triples) and go through the ontology cache at
   `sources/<host>/<path>`, like configured remote ontologies.
   `file:` imports keep their own URL.
   - RDF/XML and HTML responses (common for `.owl` files and for IRIs that
   serve a web page) are skipped with a warning instead of being parsed
   as Turtle, and are not cached.
   - An import's language follows its file extension. Forcing `turtle`
   gave e.g. a `.jsonld` import both the Turtle and the JSON-LD language.
   - `import_url` is the single mapping from an imported IRI to its
   document, used for loading, document links and goto-definition, so
   goto-definition on an `owl:imports` IRI opens the imported document.
   - Imports are followed transitively, each IRI at most once.
   
   Related fixes:
   
   - `validate_shapes` also revalidates every open document when shapes
   change. Shapes usually arrive after the document was opened (imports,
   LOV, configuration), so before this their violations only appeared
   after the next edit.
   - Shape links are matched exactly instead of by prefix: a link to
   `http://x/ont2` matched a document labelled `http://x/ont`.
   - `spawn_or_insert` always creates `DocumentLinks`. When it was missing,
   the prefix and import link systems each inserted their own in the same
   pass and one overwrote the other.
 - <csr-id-945c7d3136db2c20ac1a90ee1dcb84c56cf0569a/> persistent read-only ontology cache, didClose and cached-copy rules
   Replace the per-process `/tmp/swls/<n>` folder with one cache in the user's
   cache directory (`~/.cache/swls`, `~/Library/Caches/swls`,
   `%LOCALAPPDATA%\swls`):
   
   - Readable layout: `lov/<prefix>.ttl` and `sources/<host>/<path>`.
   - Files are written atomically (temp file + rename), so servers running at
   the same time can share the cache, and are marked read-only on disk so
   editors warn before they are edited.
   - Entries are reused across restarts and refreshed after 7 days
   (`util::cache`). When a refresh fails the stale copy is used, so the
   server works offline with what it fetched before. Configured remote
   ontologies and shapes go through the cache too, and a non-200 response
   is no longer treated as an ontology.
   
   The LSP cannot make a document read-only, so the server enforces the
   snapshot semantics itself (`core::document`):
   
   - Documents the server loads itself are marked `Background`. When the
   editor opens one, its text is kept as a `Snapshot`, edits never reach
   the store, and a cache file shows an information notice.
   - Implement `textDocument/didClose`: drop `Open`, clear the document's
   diagnostics, and give a background document the server's copy back
   (from disk, else the snapshot).
   
   Tests get an isolated cache directory per instance instead of a shared
   `/tmp/swls/test` that each test deleted.
 - <csr-id-4e5fd5eae67472a6e1f2d240408aae549b409156/> persistent read-only ontology cache, didClose and cached-copy rules
   Replace the per-process `/tmp/swls/<n>` folder with one cache in the user's
   cache directory (`~/.cache/swls`, `~/Library/Caches/swls`,
   `%LOCALAPPDATA%\swls`):
   
   - Readable layout: `lov/<prefix>.ttl` and `sources/<host>/<path>`.
   - Files are written atomically (temp file + rename), so servers running at
   the same time can share the cache, and are marked read-only on disk so
   editors warn before they are edited.
   - Entries are reused across restarts and refreshed after 7 days
   (`util::cache`). When a refresh fails the stale copy is used, so the
   server works offline with what it fetched before. Configured remote
   ontologies and shapes go through the cache too, and a non-200 response
   is no longer treated as an ontology.
   
   The LSP cannot make a document read-only, so the server enforces the
   snapshot semantics itself (`core::document`):
   
   - Documents the server loads itself are marked `Background`. When the
   editor opens one, its text is kept as a `Snapshot`, edits never reach
   the store, and a cache file shows an information notice.
   - Implement `textDocument/didClose`: drop `Open`, clear the document's
   diagnostics, and give a background document the server's copy back
   (from disk, else the snapshot).
   
   Tests get an isolated cache directory per instance instead of a shared
   `/tmp/swls/test` that each test deleted.

### Bug Fixes

 - <csr-id-250fb5e5ba6737d3c773fc53281782f33d9b2b47/> only shorten IRIs to valid prefixed names, longest prefix first
 - <csr-id-f45bd539cf78d85895b56e6f5b1780d5ae4b25ed/> give each ontology document its own identity
   Configured ontologies that describe the same namespace (e.g. the RML
   modules, all `http://w3id.org/rml/`) were all labelled
   `<cache>/<prefix>.ttl`, so they collapsed onto one entity: each load
   replaced the previous one, and since the store only loaded `Added<Triples>`
   only the first module's classes and properties ever became known.
   
   - Resolve the documents for a namespace in one place
   (`namespace_sources`), used by fetching, document links and prefix
   goto-definition. Configured ontologies are identified by their location:
   local files keep their own URL, remote files are cached at
   `<cache>/<host>/<path>` (`FsTrait::cache_url`). They take precedence over
   the built-in / LOV ontology for that namespace.
   - Load every background document into its own named graph and reload it
   when its triples change, so replaced or edited documents no longer leave
   stale triples; queries use the union of all graphs.
   - Re-resolve open documents when configured ontologies arrive after the
   document was opened.
   - `spawn_background`: background loads (LOV, owl:imports, configured
   shapes) no longer overwrite the text of a document open in the editor.
   - Only write cache files for cache locations, never over a user's file.
 - <csr-id-2f8a0661a463c8666bad9b81ccd6c8dff9066069/> give each ontology document its own identity
   Configured ontologies that describe the same namespace (e.g. the RML
   modules, all `http://w3id.org/rml/`) were all labelled
   `<cache>/<prefix>.ttl`, so they collapsed onto one entity: each load
   replaced the previous one, and since the store only loaded `Added<Triples>`
   only the first module's classes and properties ever became known.
   
   - Resolve the documents for a namespace in one place
   (`namespace_sources`), used by fetching, document links and prefix
   goto-definition. Configured ontologies are identified by their location:
   local files keep their own URL, remote files are cached at
   `<cache>/<host>/<path>` (`FsTrait::cache_url`). They take precedence over
   the built-in / LOV ontology for that namespace.
   - Load every background document into its own named graph and reload it
   when its triples change, so replaced or edited documents no longer leave
   stale triples; queries use the union of all graphs.
   - Re-resolve open documents when configured ontologies arrive after the
   document was opened.
   - `spawn_background`: background loads (LOV, owl:imports, configured
   shapes) no longer overwrite the text of a document open in the editor.
   - Only write cache files for cache locations, never over a user's file.
 - <csr-id-ac42b779da1bb02e382eeefbbabd8057a3a13cba/> time out remote fetches and stop panicking on network and I/O errors
   - One shared HTTP client for all fetches, with a 10 s connect and a 30 s
     request timeout. Before, every fetch built a new client without timeouts,
     so a host that never answered kept that load pending forever.
   - A response whose body cannot be read is an error instead of a panic
     (`resp.text().await.unwrap()`).
   - `file:` URLs are read through `to_file_path`, which decodes
     percent-escapes; `Url::path` kept them, so a path with a space failed.
   - No panic when a built-in ontology has no cache location, when the world
     does not answer during `initialize`, or on an unbound SPARQL variable.

### Refactor

 - <csr-id-f0d9f31379a47547d665ee36ee7eb5a3704b5187/> name type hierarchy walks by what they return
 - <csr-id-8f28aa0b47d503e5ea2a06a276f238850d737166/> cursor semantics, a completion context, one shape visibility rule
   Positions
   - The token at a cursor is resolved with cursor semantics
     (`token_at_cursor`): a cursor sits between characters, so it belongs to a
     word it is inside of, else a word ending at it (`ex:fo|;` is `ex:fo`),
     else punctuation it touches, else no token (whitespace). The triple
     lookup prefers a term containing the cursor, then one ending at it.
   - `Backend` no longer shifts positions one character left. The shift made
     completion at the end of a word work, but hovering the first character of
     a term resolved the term before it. The server and the test harness now
     handle positions identically; the harness docs say so.
   
   Completion context
   - `CursorContext`, resolved once per completion request
     (`get_cursor_context`): the role at the cursor, the subject being
     described, the typed word, where to insert, and whether a space is needed
     after `;` / `,` / `[`. Inside `[ … ]`, not on a term, the cursor is at a
     predicate position of that blank node.
   - Class, property, shape-template and shape-property completion use it,
     replacing the templates' private cursor helpers. Property completion
     inside a blank node now works, with domain-aware ordering for its type.
   
   Shape visibility
   - `shapes::visible`: one rule for which shape documents apply to a document
     (global, the document itself, or linked), used by validation, templates,
     hover and completion.
   - Shape documentation shortens IRIs with all shape documents' prefixes as
     one set, and `Prefixes::shorten` breaks ties between equally long
     namespaces by preferring a named prefix, then alphabetical order, so the
     result no longer depends on the order documents are visited.
 - <csr-id-ef9b50600ab106ff4b3f6178c5f172269c9d9605/> revision counters for data shared between documents
   Shared data (the store, known ontologies, the class hierarchy, SHACL
   shapes, configured sources) usually arrives after the documents that use it
   were opened. Each consumer had its own trigger: `Changed<Triples>` on
   background documents, `Changed<ShaclShapes>`, `Added<ConfiguredOntology>`,
   running the save schedule after an import, or simply rerunning for every
   open document on every parse.
   
   `Revisions` (`core::revisions`) counts changes per kind of shared data.
   Producers bump a counter when their data changed; consumers keep a
   `Local<Seen>` and process every open document once after a bump, otherwise
   only documents whose own triples changed.
   
   - `load_store` → `store`; `derive_ontologies` recomputes on `store` and
     bumps `ontologies`; `extract_type_hierarchy` and `derive_ontologies` bump
     `hierarchy` when it grew; `derive_shapes` bumps `shapes` only when a
     document has or had actual shapes; configured ontologies bump `sources`.
   - `infer_types` now reruns when ontologies or the hierarchy change. Before,
     types inferred from property domains stayed wrong until the next edit
     when the ontology arrived after the document was opened. It is ordered
     after its producers, so the change is seen in the same pass.
   - `validate_namespace_properties` no longer reruns for every open document
     on every parse; it runs on edits, ontology changes and config changes.
   - `validate_shapes`, `derive_prefix_links` and `fetch_lov_properties` use
     the counters instead of their ad-hoc queries, and imports no longer run
     the save schedule to force revalidation.
 - <csr-id-15493a8bfeea01bd5d813e13f68d9d52672081b5/> a document registry instead of scanning labels
   Documents were found by scanning every `Label` (in `spawn_or_insert`,
   `spawn_background`, the document lifecycle, the JSON-LD loader and the test
   harness), while `Backend` kept its own URL → entity map, filled on
   `didOpen`. The two could disagree, and a request looked up its entity
   before running, so a document closed in between would make
   `entity_mut` panic.
   
   - `Documents`: a resource mapping URLs to entities, kept up to date by
     `Label`'s `on_insert` / `on_replace` hooks, so it is correct however a
     document is spawned, relabelled or despawned. `Label` is now an
     immutable component, so it cannot change without the hooks running.
   - `find_document(world, url)` is the one lookup, used everywhere a label
     scan was. Without the registry (a bare test world) it falls back to a
     scan.
   - `Backend` drops its map. Requests carry the URL, and `run_schedule`
     looks the document up inside the world, in the same step that runs the
     schedule.
   - The registry docs describe the kinds of documents: `Open`, `Background`,
     both, and cached (a background document in the ontology cache).

### Style

 - <csr-id-8160444de14ceb8c86d3b22412e95a6b62974d46/> cargo fmt --all
   Formatting only, no code changes. Listed in .git-blame-ignore-revs.

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 19 commits contributed to the release.
 - 17 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 1 unique issue was worked on: [#30](https://github.com/SemanticWebLanguageServer/swls/issues/30)

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **[#30](https://github.com/SemanticWebLanguageServer/swls/issues/30)**
    - Give each ontology document its own identity ([`f45bd53`](https://github.com/SemanticWebLanguageServer/swls/commit/f45bd539cf78d85895b56e6f5b1780d5ae4b25ed))
    - Give each ontology document its own identity ([`2f8a066`](https://github.com/SemanticWebLanguageServer/swls/commit/2f8a0661a463c8666bad9b81ccd6c8dff9066069))
 * **Uncategorized**
    - Release swls-lov v0.1.4, swls-core v0.2.3, components-rs v0.1.4, swls-lang-rdf-base v0.2.3, swls-lang-turtle v0.2.3, swls-lang-jsonld v0.2.3, swls-lang-n3 v0.1.3, swls-lang-sparql v0.2.3, swls-lang-trig v0.2.3, swls v0.6.0 ([`e3aa4bf`](https://github.com/SemanticWebLanguageServer/swls/commit/e3aa4bfb5c246842e24de7c37e58415a472fe291))
    - Time out remote fetches and stop panicking on network and I/O errors ([`ac42b77`](https://github.com/SemanticWebLanguageServer/swls/commit/ac42b779da1bb02e382eeefbbabd8057a3a13cba))
    - Suggest predicates that shapes require first ([`a1dc471`](https://github.com/SemanticWebLanguageServer/swls/commit/a1dc471597db6b8c6b6ddc7d4971fa9f1c0e6a4e))
    - Name type hierarchy walks by what they return ([`f0d9f31`](https://github.com/SemanticWebLanguageServer/swls/commit/f0d9f31379a47547d665ee36ee7eb5a3704b5187))
    - Cursor semantics, a completion context, one shape visibility rule ([`8f28aa0`](https://github.com/SemanticWebLanguageServer/swls/commit/8f28aa0b47d503e5ea2a06a276f238850d737166))
    - Revision counters for data shared between documents ([`ef9b506`](https://github.com/SemanticWebLanguageServer/swls/commit/ef9b50600ab106ff4b3f6178c5f172269c9d9605))
    - A document registry instead of scanning labels ([`15493a8`](https://github.com/SemanticWebLanguageServer/swls/commit/15493a8bfeea01bd5d813e13f68d9d52672081b5))
    - Fix clippy warnings and configure workspace lints ([`b566e5f`](https://github.com/SemanticWebLanguageServer/swls/commit/b566e5f3228a4578291186a1eaa985ade4c736ca))
    - Cargo fmt --all ([`8160444`](https://github.com/SemanticWebLanguageServer/swls/commit/8160444de14ceb8c86d3b22412e95a6b62974d46))
    - Only shorten IRIs to valid prefixed names, longest prefix first ([`250fb5e`](https://github.com/SemanticWebLanguageServer/swls/commit/250fb5e5ba6737d3c773fc53281782f33d9b2b47))
    - Document predicates with SHACL property shapes, prefer one language ([`4ac42a2`](https://github.com/SemanticWebLanguageServer/swls/commit/4ac42a280cbb4c56d40f8ad336cdb979eddd2680))
    - SHACL shape templates in completion ([`0bb608f`](https://github.com/SemanticWebLanguageServer/swls/commit/0bb608f3d344b59f4820c4ee2b3bffd43a783d41))
    - Load remote owl:imports and resolve ontologies by preferred prefix name ([`ee692c8`](https://github.com/SemanticWebLanguageServer/swls/commit/ee692c8307a2c597c158af25eddbdcb919e33bfd))
    - Resolve ontologies by the namespace's preferred prefix name ([`3a5907c`](https://github.com/SemanticWebLanguageServer/swls/commit/3a5907c5cf5078ad1dc0cd7c13cdaf8fcff68292))
    - Load remote owl:imports and make imported documents navigable ([`18f6bde`](https://github.com/SemanticWebLanguageServer/swls/commit/18f6bde08999c3f6bb77f64ce2f4e0a19a6ac20a))
    - Persistent read-only ontology cache, didClose and cached-copy rules ([`945c7d3`](https://github.com/SemanticWebLanguageServer/swls/commit/945c7d3136db2c20ac1a90ee1dcb84c56cf0569a))
    - Persistent read-only ontology cache, didClose and cached-copy rules ([`4e5fd5e`](https://github.com/SemanticWebLanguageServer/swls/commit/4e5fd5eae67472a6e1f2d240408aae549b409156))
</details>

## 0.2.2 (2026-10-14)

<csr-id-53cc4155da52b7054793c81009832fba55c3e2fb/>
<csr-id-c5042d79e63cac0596075c50ae18c9d15cb5d9cb/>
<csr-id-4debbca4c3e1839781b7728951f91f2ba729165b/>

### Chore

 - <csr-id-53cc4155da52b7054793c81009832fba55c3e2fb/> prune dead features, dependencies, and the swls-token-helpers crate
   - Remove the always-on `shapes` gate (SHACL validation was never actually
     toggleable since parse/save referenced it unconditionally) and the dead
     `tokio`/`agnostic` features. The `sparql_service`/`mie` deps the `tokio`
     feature gated are kept as version pins with `default-features = false`:
     shacl_validation pulls them in transitively, newer releases break against
     the =0.2.9 rudof/shacl stack, and their default `sparql` feature would chain
     into `rudof_rdf/sparql` and drag in tokio/reqwest/mio — which breaks the
     wasm build (swls-web). SHACL only uses NativeEngine, so `sparql` is never
     wanted.
   - Make `tower-lsp` a direct dependency of the `swls` binary instead of a
     shared workspace dependency (core only references it in docs).
   - Remove dead deps `similar`, `lazy_static`, `logos`, `chumsky`, plus a
     full `cargo machete` sweep across every crate.
   - Delete the orphaned `swls-token-helpers` crate (not a workspace member,
     no dependents).
   - Sync `[workspace.dependencies]` version constraints with the bumped crate
     versions so the workspace resolves again.

### Documentation

 - <csr-id-682af7ba71d4e99d0f9516494fcd7ef552232f4d/> point crates to main readme + update readme

### New Features

 - <csr-id-aa051911290ed08291c2d5738d5217cd10f12aa7/> enable formatting by default for all languages
   Turtle, TriG, SPARQL, N3 and JSON-LD now format unless disabled with
   `format.<language>: false`.
 - <csr-id-caa7a763f47817d920f5d2b99af882c9172da443/> format TriG by default
 - <csr-id-adedb5370c73559f7ae91bf335eed2e12d98c2fb/> use the rdf-parsers formatters for Turtle, TriG and SPARQL
 - <csr-id-43a8054c274bde6e63475dcdccd65af9d24b7613/> prefix hover/goto-definition + JSON-LD prefix-diagnostics opt-out
   Prefix handling across languages, plus model-authoritative JSON-LD
   semantic-token colouring and the datatype-span work this built on.
   
   Hover / goto-definition on prefixes (new core/src/systems/prefix_hover.rs):
   - get_current_prefix detects a prefix declaration / JSON-LD @context term at the
   cursor from the parsed model (which, unlike derived triples, carries spans) and
   drops the wrong nearest-triple fallback so the triple-based hovers no-op.
   - hover_prefix shows the prefix -> namespace mapping (+ LOV title when known).
   - goto_prefix jumps a real namespace to its ontology file; goto_cjs now steps
   aside for namespaces but still resolves term aliases to their CJS definition.
   - Wired into the Hover and GotoDefinition schedules; new `definition()` e2e
   harness helper; e2e/tests/prefix_hover.rs.
   
   JSON-LD prefix diagnostics:
   - supports_prefix_diagnostics() = false for JSON-LD; @context has no prefix/alias
   distinction and pulls in shared/remote terms, so the span-based detector emitted
   false positives. e2e/tests/prefix_diagnostics.rs updated to assert the opt-out.
   
   Prefix diagnostics core:
   - Walk the parsed Turtle model instead of the derived triples so prefixes that
   only appear in a datatype ("5"^^xsd:integer) are detected.
   
   JSON-LD semantic tokens (model-authoritative):
   - Colour every term from the parsed model: a compact IRI `"ex:obs1"` is the term
   colour (quotes + local = enumMember) with `prefix:` NAMESPACE on top, applied
   consistently to @id subjects and nested object references; @context prefix keys
   are NAMESPACE.
   - The lexer supplies only what the model can't see: keywords (`@id`, `@type`, …)
   and the STRING/NUMBER/BOOLEAN base for real literals. Its previous
   string-followed-by-`:` NAMESPACE guess is removed.
   - Keep KEYWORD colouring for `@type` even though it is the rdf:type predicate in
   the model. Adapt to the rdf-parsers Spanned lang/ty API and carry
   context-computed IRIs. Tests in lang-jsonld/src/ecs/mod.rs pin the byte types.
   
   Depends on rdf-parsers 0.1.16.
 - <csr-id-817cc6bfcc92911b6c6be7b491ab1b00b85e13b9/> better turtle formatting + consolidate language specific implementation (removing generic marker)
 - <csr-id-832b6ae7122bc4b071a1a68ffba7c9bc2956d7e3/> make N3 support opt-in (disabled by default)
   N3 stays compiled in but is now gated off at runtime unless the user sets
   `"n3": true` in config. Flips the two existing per-language gates from
   `unwrap_or(true)` to `unwrap_or(false)`:
   
   - parse gate (`parse_n3_system`): `.n3` documents aren't parsed by default.
   - document selector (`backend.rs`): the server no longer advertises handling
   the `n3` language, so clients don't route `.n3` files to it.
   
   The n3 unit tests set `config.n3 = Some(true)` so they still exercise the
   parser. Other languages remain enabled by default.
 - <csr-id-faad6d5d3e8023269a265c58352644e590bc47f2/> gate features with configuration
 - <csr-id-12b14f9f8b92f0163b3e0e25842df707bcf1e785/> add automatic insert of prefix statements when writing colon
 - <csr-id-71381b48710c992a6233aa080af53730d2657040/> inline and extract blank nodes
 - <csr-id-e76bb7f72a54acfdd3b6ed56687a51930f0c12e6/> add undefined iri's warnings
 - <csr-id-754ce139d2f65c2486ff027786bec722844c3779/> prefix diagnostics
 - <csr-id-91525d7d08b5f1d52f0ce4096e39bd844d082ded/> move tower_lsp to swls binary
 - <csr-id-a0d86b89724b070322f63e75ea0e27aacc062a28/> start rename
 - <csr-id-eb4144409025da4fba32892e10c207441c882de0/> use lov-mirror on github for better response times
 - <csr-id-9b17ed366f37598da0a9747dc51d552bee891ded/> better highlighting
 - <csr-id-2056a5f93ff4467478a4a72bf3f0c4c6691f78b0/> allow jsonld to show inlayed types in a json-ld manner
 - <csr-id-9e7d856a0991850c9fd51981a2555b4bdde9cb57/> move logging to lsp logging + actually spawn local
 - <csr-id-37ca76298f9b43001bdcc6f9096a8611b8559128/> bump bevy_ecs to 0.18!
 - <csr-id-35ff9925b3957c491e3798a62ae4e4e32f88d40e/> make cjs work for vscode
 - <csr-id-478139e705d9f72c1ad9e8df5228379afd2b5b7f/> remove PathBuf from read dir
 - <csr-id-44cb452ffa0417562a5d26c863981419b56df701/> move components-rs to Url
 - <csr-id-663af2958384584b8111e41a3131fb55984b71bf/> fix many minor mistakes
 - <csr-id-987907ef012c3d41a094a3b7bf853ed0a55d23a8/> bump
 - <csr-id-97441d4d9dc7778d3fcb6b0862ad0d61fa38b01f/> check if cjs to properties and classes is a good idea
 - <csr-id-5ec44b5d4c325bb6687bf70292c29b74d4770ded/> goto @context string
 - <csr-id-f2a1679be0c5261e2705335eb2c2c1a14015906d/> improve cjs goto definition
 - <csr-id-9a887c510c8740c9d9f01a27f88aeca533a69cf3/> incorporate cjs, but we cannot yet parse the IRIs
 - <csr-id-85ebb4941c245ecf65c0146b644eee371cb7b939/> try to render hover as markdown
 - <csr-id-ade12adaca2ba8bf197b3aaf59091ee9ec266687/> add better jsonld support
 - <csr-id-a47bca07a0b3d01fbd411868bb7308cf9802bcf5/> things
 - <csr-id-7c78bd43551a5afcdaaa8bef83fe9fd9ecd4d443/> make jsonld context resolve with local contexts
 - <csr-id-aa9acc8a6c0565ef86b54545222717e00760ac00/> fix many bugs like highlighting, json-ld, autocompletion when not in a token etc
 - <csr-id-5157d404a94386a17c74ea125a6d8809412d10ac/> improve error spans
 - <csr-id-dc45e8187ffa2cd0a6d6aec35e7477802b8bcd88/> use new parser for turtle and sparql
 - <csr-id-721d151c1dbcaccab3a81f81f7d59f2517fe323c/> rename before publish
 - <csr-id-c6d99d1cfab4ab20fec9a1b376f3fa7c443d8a52/> move to upstream rudof
 - <csr-id-cf54f0bfd210e1b01311683399a70f8d7ae157e7/> optimize logging
 - <csr-id-799356a263277b24507115aa1574b5d0c1f7303a/> claude refactor
 - <csr-id-16579c4da87b74784982930c3af0dea8e938b0d5/> add unused prefix diagnostic and organize imports code action
 - <csr-id-27b8c86e7d11ebf2faee236c03b360e37c024e28/> add file based configs + order prefix completions based on popularity

### Bug Fixes

 - <csr-id-a8632e1da167bb0d7b38513a8e0cefe6d59fdb5a/> incorrect json-ld alias marked as unused + fix spans on diagnostics
 - <csr-id-5085b23f3eb4309daf5d6b13853cfb89facae3d6/> improve rename robustness
 - <csr-id-3bbf1cd80df9ff26b109c7e30da844cbc262d66e/> remove unused systems
 - <csr-id-1292fb8ccbdb3b10f01a7eb9e6bc64ccfcbda9dc/> better timing on the validation to reduce race conditions
 - <csr-id-dc8482d1c433868c18ec54017d4bc2b12776592c/> diagnostics after on change things works way better
 - <csr-id-29ef7e453047b405fe76ff846b8a0bf230489bef/> make cargo release happy with version numbers
 - <csr-id-acd94a957d7ad001014eadef773bf59ee0fa6391/> remove unused imports warnings

### Other

 - <csr-id-c5042d79e63cac0596075c50ae18c9d15cb5d9cb/> properly disable json-ld when the language is disabled
 - <csr-id-4debbca4c3e1839781b7728951f91f2ba729165b/> fix jsonld goto definition for the last time!

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 167 commits contributed to the release.
 - 51 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 1 unique issue was worked on: [#32](https://github.com/SemanticWebLanguageServer/swls/issues/32)

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **[#32](https://github.com/SemanticWebLanguageServer/swls/issues/32)**
    - Refactor/replace ropey lineindex ([`81148b6`](https://github.com/SemanticWebLanguageServer/swls/commit/81148b64d51d9399a6d8d76b8f6c5114b90450af))
 * **Uncategorized**
    - Release swls-lov v0.1.3, swls-core v0.2.2, components-rs v0.1.3, swls-lang-rdf-base v0.2.2, swls-lang-turtle v0.2.2, swls-lang-jsonld v0.2.2, swls-lang-n3 v0.1.2, swls-lang-sparql v0.2.2, swls-lang-trig v0.2.2, swls v0.5.0 ([`8e33bcd`](https://github.com/SemanticWebLanguageServer/swls/commit/8e33bcd501862e31fb5edefbce63909abf26feff))
    - Adjusting changelogs prior to release of swls-lov v0.1.3, swls-core v0.2.2, components-rs v0.1.3, swls-lang-rdf-base v0.2.2, swls-lang-turtle v0.2.2, swls-lang-jsonld v0.2.2, swls-lang-n3 v0.1.2, swls-lang-sparql v0.2.2, swls-lang-trig v0.2.2, swls v0.5.0 ([`3e999f2`](https://github.com/SemanticWebLanguageServer/swls/commit/3e999f28fe47eadd3d7eaa7067d2d68025365485))
    - Enable formatting by default for all languages ([`aa05191`](https://github.com/SemanticWebLanguageServer/swls/commit/aa051911290ed08291c2d5738d5217cd10f12aa7))
    - Format TriG by default ([`caa7a76`](https://github.com/SemanticWebLanguageServer/swls/commit/caa7a763f47817d920f5d2b99af882c9172da443))
    - Use the rdf-parsers formatters for Turtle, TriG and SPARQL ([`adedb53`](https://github.com/SemanticWebLanguageServer/swls/commit/adedb5370c73559f7ae91bf335eed2e12d98c2fb))
    - Release swls-core v0.2.1, swls-lang-rdf-base v0.2.1, swls-lang-turtle v0.2.1, swls-lang-jsonld v0.2.1, swls-lang-n3 v0.1.1, swls-lang-sparql v0.2.1, swls-lang-trig v0.2.1, swls v0.4.1 ([`b5c3a82`](https://github.com/SemanticWebLanguageServer/swls/commit/b5c3a82f8cf84992f103c9e8b8b01a5303d6b92d))
    - Adjusting changelogs prior to release of swls-core v0.2.1, swls-lang-rdf-base v0.2.1, swls-lang-turtle v0.2.1, swls-lang-jsonld v0.2.1, swls-lang-n3 v0.1.1, swls-lang-sparql v0.2.1, swls-lang-trig v0.2.1, swls v0.4.1 ([`2f1e550`](https://github.com/SemanticWebLanguageServer/swls/commit/2f1e5503bae9428b76613ab8b110700234569e1e))
    - Prefix hover/goto-definition + JSON-LD prefix-diagnostics opt-out ([`43a8054`](https://github.com/SemanticWebLanguageServer/swls/commit/43a8054c274bde6e63475dcdccd65af9d24b7613))
    - Better turtle formatting + consolidate language specific implementation (removing generic marker) ([`817cc6b`](https://github.com/SemanticWebLanguageServer/swls/commit/817cc6bfcc92911b6c6be7b491ab1b00b85e13b9))
    - Release swls-core v0.2.0, swls-lang-rdf-base v0.2.0, swls-lang-turtle v0.2.0, swls-lang-jsonld v0.2.0, swls-lang-n3 v0.1.0, swls-lang-sparql v0.2.0, swls-lang-trig v0.2.0, swls v0.4.0 ([`9de2e61`](https://github.com/SemanticWebLanguageServer/swls/commit/9de2e6140c68f374c9dbc0c981647f71dd26d26d))
    - Make N3 support opt-in (disabled by default) ([`832b6ae`](https://github.com/SemanticWebLanguageServer/swls/commit/832b6ae7122bc4b071a1a68ffba7c9bc2956d7e3))
    - Prune dead features, dependencies, and the swls-token-helpers crate ([`53cc415`](https://github.com/SemanticWebLanguageServer/swls/commit/53cc4155da52b7054793c81009832fba55c3e2fb))
    - Feat: add lang n3 feat: update prefix diagnostics feat: add format settings ([`c0e3888`](https://github.com/SemanticWebLanguageServer/swls/commit/c0e3888d726463bffd360a3758a33e4df7aa9b02))
    - Release swls-core v0.1.4, swls-lang-rdf-base v0.1.4, swls-lang-turtle v0.1.4, swls-lang-jsonld v0.1.6, swls-lang-sparql v0.1.5, swls-lang-trig v0.1.4, swls v0.3.0 ([`df8a0b7`](https://github.com/SemanticWebLanguageServer/swls/commit/df8a0b7b8223d33cc7547692f7cb9040636584ab))
    - Gate features with configuration ([`faad6d5`](https://github.com/SemanticWebLanguageServer/swls/commit/faad6d5d3e8023269a265c58352644e590bc47f2))
    - Incorrect json-ld alias marked as unused + fix spans on diagnostics ([`a8632e1`](https://github.com/SemanticWebLanguageServer/swls/commit/a8632e1da167bb0d7b38513a8e0cefe6d59fdb5a))
    - Add automatic insert of prefix statements when writing colon ([`12b14f9`](https://github.com/SemanticWebLanguageServer/swls/commit/12b14f9f8b92f0163b3e0e25842df707bcf1e785))
    - Improve rename robustness ([`5085b23`](https://github.com/SemanticWebLanguageServer/swls/commit/5085b23f3eb4309daf5d6b13853cfb89facae3d6))
    - Remove unused systems ([`3bbf1cd`](https://github.com/SemanticWebLanguageServer/swls/commit/3bbf1cd80df9ff26b109c7e30da844cbc262d66e))
    - Inline and extract blank nodes ([`71381b4`](https://github.com/SemanticWebLanguageServer/swls/commit/71381b48710c992a6233aa080af53730d2657040))
    - Add undefined iri's warnings ([`e76bb7f`](https://github.com/SemanticWebLanguageServer/swls/commit/e76bb7f72a54acfdd3b6ed56687a51930f0c12e6))
    - Better timing on the validation to reduce race conditions ([`1292fb8`](https://github.com/SemanticWebLanguageServer/swls/commit/1292fb8ccbdb3b10f01a7eb9e6bc64ccfcbda9dc))
    - Diagnostics after on change things works way better ([`dc8482d`](https://github.com/SemanticWebLanguageServer/swls/commit/dc8482d1c433868c18ec54017d4bc2b12776592c))
    - Prefix diagnostics ([`754ce13`](https://github.com/SemanticWebLanguageServer/swls/commit/754ce139d2f65c2486ff027786bec722844c3779))
    - Move tower_lsp to swls binary ([`91525d7`](https://github.com/SemanticWebLanguageServer/swls/commit/91525d7d08b5f1d52f0ce4096e39bd844d082ded))
    - Start rename ([`a0d86b8`](https://github.com/SemanticWebLanguageServer/swls/commit/a0d86b89724b070322f63e75ea0e27aacc062a28))
    - Release swls-core v0.1.3, swls-lang-rdf-base v0.1.3, swls-lang-jsonld v0.1.5, swls-lang-sparql v0.1.4, swls-lang-trig v0.1.3, swls v0.2.2 ([`0d1c8c5`](https://github.com/SemanticWebLanguageServer/swls/commit/0d1c8c52d0b7741321109ad22f1f16d53e4f8dc6))
    - Adjusting changelogs prior to release of swls-core v0.1.3, swls-lang-rdf-base v0.1.3, swls-lang-jsonld v0.1.5, swls-lang-sparql v0.1.4, swls-lang-trig v0.1.3, swls v0.2.2 ([`4f3e731`](https://github.com/SemanticWebLanguageServer/swls/commit/4f3e731b0301e0b689bfe15e790ad4706a3c84e1))
    - Use lov-mirror on github for better response times ([`eb41444`](https://github.com/SemanticWebLanguageServer/swls/commit/eb4144409025da4fba32892e10c207441c882de0))
    - Better highlighting ([`9b17ed3`](https://github.com/SemanticWebLanguageServer/swls/commit/9b17ed366f37598da0a9747dc51d552bee891ded))
    - Release swls-lov v0.1.2, swls-core v0.1.2, swls-lang-rdf-base v0.1.2, swls-lang-turtle v0.1.3, swls-lang-jsonld v0.1.3, swls-lang-sparql v0.1.3, swls-lang-trig v0.1.2, swls v0.2.0 ([`bfde48f`](https://github.com/SemanticWebLanguageServer/swls/commit/bfde48f836e70a9b3f08230e2d84c957eb5a72b0))
    - Release swls-lov v0.1.2, swls-core v0.1.2, swls-lang-rdf-base v0.1.2, swls-lang-turtle v0.1.3, swls-lang-jsonld v0.1.3, swls-lang-sparql v0.1.3, swls-lang-trig v0.1.2, swls v0.2.0 ([`eb52296`](https://github.com/SemanticWebLanguageServer/swls/commit/eb52296d24ca7c04061acc584a3423b4213cb2ee))
    - Point crates to main readme + update readme ([`682af7b`](https://github.com/SemanticWebLanguageServer/swls/commit/682af7ba71d4e99d0f9516494fcd7ef552232f4d))
    - Allow jsonld to show inlayed types in a json-ld manner ([`2056a5f`](https://github.com/SemanticWebLanguageServer/swls/commit/2056a5f93ff4467478a4a72bf3f0c4c6691f78b0))
    - Properly disable json-ld when the language is disabled ([`c5042d7`](https://github.com/SemanticWebLanguageServer/swls/commit/c5042d79e63cac0596075c50ae18c9d15cb5d9cb))
    - Move logging to lsp logging + actually spawn local ([`9e7d856`](https://github.com/SemanticWebLanguageServer/swls/commit/9e7d856a0991850c9fd51981a2555b4bdde9cb57))
    - Release swls-lov v0.1.1, swls-core v0.1.1, components-rs v0.1.2, swls-lang-rdf-base v0.1.0, swls-lang-turtle v0.1.1, swls-lang-jsonld v0.1.1, swls-lang-sparql v0.1.1, swls-lang-trig v0.1.0, swls v0.1.1 ([`612f47d`](https://github.com/SemanticWebLanguageServer/swls/commit/612f47d34fbde7ad5025165735b0c19eb6d6b203))
    - Bump bevy_ecs to 0.18! ([`37ca762`](https://github.com/SemanticWebLanguageServer/swls/commit/37ca76298f9b43001bdcc6f9096a8611b8559128))
    - Make cjs work for vscode ([`35ff992`](https://github.com/SemanticWebLanguageServer/swls/commit/35ff9925b3957c491e3798a62ae4e4e32f88d40e))
    - Remove PathBuf from read dir ([`478139e`](https://github.com/SemanticWebLanguageServer/swls/commit/478139e705d9f72c1ad9e8df5228379afd2b5b7f))
    - Fix jsonld goto definition for the last time! ([`4debbca`](https://github.com/SemanticWebLanguageServer/swls/commit/4debbca4c3e1839781b7728951f91f2ba729165b))
    - Move components-rs to Url ([`44cb452`](https://github.com/SemanticWebLanguageServer/swls/commit/44cb452ffa0417562a5d26c863981419b56df701))
    - Fix many minor mistakes ([`663af29`](https://github.com/SemanticWebLanguageServer/swls/commit/663af2958384584b8111e41a3131fb55984b71bf))
    - Bump ([`987907e`](https://github.com/SemanticWebLanguageServer/swls/commit/987907ef012c3d41a094a3b7bf853ed0a55d23a8))
    - Check if cjs to properties and classes is a good idea ([`97441d4`](https://github.com/SemanticWebLanguageServer/swls/commit/97441d4d9dc7778d3fcb6b0862ad0d61fa38b01f))
    - Goto @context string ([`5ec44b5`](https://github.com/SemanticWebLanguageServer/swls/commit/5ec44b5d4c325bb6687bf70292c29b74d4770ded))
    - Improve cjs goto definition ([`f2a1679`](https://github.com/SemanticWebLanguageServer/swls/commit/f2a1679be0c5261e2705335eb2c2c1a14015906d))
    - Incorporate cjs, but we cannot yet parse the IRIs ([`9a887c5`](https://github.com/SemanticWebLanguageServer/swls/commit/9a887c510c8740c9d9f01a27f88aeca533a69cf3))
    - Try to render hover as markdown ([`85ebb49`](https://github.com/SemanticWebLanguageServer/swls/commit/85ebb4941c245ecf65c0146b644eee371cb7b939))
    - Add better jsonld support ([`ade12ad`](https://github.com/SemanticWebLanguageServer/swls/commit/ade12adaca2ba8bf197b3aaf59091ee9ec266687))
    - Things ([`a47bca0`](https://github.com/SemanticWebLanguageServer/swls/commit/a47bca07a0b3d01fbd411868bb7308cf9802bcf5))
    - Make jsonld context resolve with local contexts ([`7c78bd4`](https://github.com/SemanticWebLanguageServer/swls/commit/7c78bd43551a5afcdaaa8bef83fe9fd9ecd4d443))
    - Fix many bugs like highlighting, json-ld, autocompletion when not in a token etc ([`aa9acc8`](https://github.com/SemanticWebLanguageServer/swls/commit/aa9acc8a6c0565ef86b54545222717e00760ac00))
    - Improve error spans ([`5157d40`](https://github.com/SemanticWebLanguageServer/swls/commit/5157d404a94386a17c74ea125a6d8809412d10ac))
    - Remove logos/chumsky tokenizers; replace with CST-based token production ([`da53ead`](https://github.com/SemanticWebLanguageServer/swls/commit/da53eadb2b042a3d9c8b4d1d28d190e2ef1462f2))
    - Use new parser for turtle and sparql ([`dc45e81`](https://github.com/SemanticWebLanguageServer/swls/commit/dc45e8187ffa2cd0a6d6aec35e7477802b8bcd88))
    - Make cargo release happy with version numbers ([`29ef7e4`](https://github.com/SemanticWebLanguageServer/swls/commit/29ef7e453047b405fe76ff846b8a0bf230489bef))
    - Remove unused imports warnings ([`acd94a9`](https://github.com/SemanticWebLanguageServer/swls/commit/acd94a957d7ad001014eadef773bf59ee0fa6391))
    - Rename before publish ([`721d151`](https://github.com/SemanticWebLanguageServer/swls/commit/721d151c1dbcaccab3a81f81f7d59f2517fe323c))
    - Move to upstream rudof ([`c6d99d1`](https://github.com/SemanticWebLanguageServer/swls/commit/c6d99d1cfab4ab20fec9a1b376f3fa7c443d8a52))
    - Optimize logging ([`cf54f0b`](https://github.com/SemanticWebLanguageServer/swls/commit/cf54f0bfd210e1b01311683399a70f8d7ae157e7))
    - Claude refactor ([`799356a`](https://github.com/SemanticWebLanguageServer/swls/commit/799356a263277b24507115aa1574b5d0c1f7303a))
    - Add unused prefix diagnostic and organize imports code action ([`16579c4`](https://github.com/SemanticWebLanguageServer/swls/commit/16579c4da87b74784982930c3af0dea8e938b0d5))
    - Try to find and fix broken blank node parsing ([`0cc6538`](https://github.com/SemanticWebLanguageServer/swls/commit/0cc65382a738f79d3c673aa451feea229836bfdc))
    - Yolo move rudof to github repository ([`cc71e61`](https://github.com/SemanticWebLanguageServer/swls/commit/cc71e61e97eafcc9a57f5f92c6166a09c5118b64))
    - Bring shapes back :party: ([`75763fc`](https://github.com/SemanticWebLanguageServer/swls/commit/75763fc1d8c23ca2d5d54f01730a27c2a910f440))
    - Add global shapes ([`3ea0c89`](https://github.com/SemanticWebLanguageServer/swls/commit/3ea0c89dc67e3cbad528be66bd3da74ba123bd51))
    - Readd shapes ([`c5dd419`](https://github.com/SemanticWebLanguageServer/swls/commit/c5dd41900a6acac67e5be88966291f24781a44cf))
    - Add a bit of logging ([`f0c3911`](https://github.com/SemanticWebLanguageServer/swls/commit/f0c3911a94c31ab90d679c038542d65b1694b516))
    - Improve property extraction + show domain and range in suggestions ([`777e1ab`](https://github.com/SemanticWebLanguageServer/swls/commit/777e1ab90df9a4a86408c5db41f63b5032c004cd))
    - Update prefix.cc prefixes ([`d21bbcc`](https://github.com/SemanticWebLanguageServer/swls/commit/d21bbcc79e0b8fd8924aae4672aa96d93da313aa))
    - Improve prefix.cc completion ([`20151a1`](https://github.com/SemanticWebLanguageServer/swls/commit/20151a12f96cc9432ee567ce4c9c01ef195bea33))
    - Show sub and superclasses on type hover ([`418ae2f`](https://github.com/SemanticWebLanguageServer/swls/commit/418ae2fa9a7d66201f27da70b13e13483e604419))
    - Only inlay super type, not subtypes ([`f20efd9`](https://github.com/SemanticWebLanguageServer/swls/commit/f20efd9224a916a19f1d2b84b317fd2783fc4f9d))
    - Complete properties based on types ([`14bfbf3`](https://github.com/SemanticWebLanguageServer/swls/commit/14bfbf300af28e7cdb392e911f47fcdf2bb4097e))
    - Clearify subclassof and superclassof + fix domain and range hover issues ([`5a04274`](https://github.com/SemanticWebLanguageServer/swls/commit/5a042740f8f5016a4a5c4ee9010a6a14e01591af))
    - Prefer configured ontologies before lov ontologies ([`6706681`](https://github.com/SemanticWebLanguageServer/swls/commit/670668102fb80b4ca571e4cd728707ed6169fc0c))
    - Derive properties and classes with sparql queries ([`482f1d8`](https://github.com/SemanticWebLanguageServer/swls/commit/482f1d8a9ebb5bbdf0ef628edf7cfa5b8f160971))
    - Fix tests ([`27e906f`](https://github.com/SemanticWebLanguageServer/swls/commit/27e906fb1bd395ebb3a66bd5d9067f187208cda0))
    - Fix update dependencies ([`d1ad1bc`](https://github.com/SemanticWebLanguageServer/swls/commit/d1ad1bc26e3ae25dcfaff4913c521835b91d0068))
    - Update dependencies ([`cec99b9`](https://github.com/SemanticWebLanguageServer/swls/commit/cec99b9c7028b3556e68017c8f67c2cb2af75057))
    - Let's just save this for now ([`6a7b5bb`](https://github.com/SemanticWebLanguageServer/swls/commit/6a7b5bb4a88e9268e67bac53da58258911a5ebe4))
    - Allow glob ontologies ([`8933499`](https://github.com/SemanticWebLanguageServer/swls/commit/8933499ce8cb504a29e5fd8864998c9292655a8f))
    - Read extra ontology files as ontologies ([`675a559`](https://github.com/SemanticWebLanguageServer/swls/commit/675a5591d85cbac7a84bdb172b79e62fb362bc0c))
    - Add file based configs + order prefix completions based on popularity ([`27b8c86`](https://github.com/SemanticWebLanguageServer/swls/commit/27b8c86e7d11ebf2faee236c03b360e37c024e28))
    - Remove unused dependencies ([`033dd71`](https://github.com/SemanticWebLanguageServer/swls/commit/033dd718488003e1149f6af6182799d99e8bf5b9))
    - Fix off by a few error for cogs ontology ([`1253c95`](https://github.com/SemanticWebLanguageServer/swls/commit/1253c951106a68b5c7d563dc51449f0e58b42387))
    - Configure which languages to activate ([`5fe3e6f`](https://github.com/SemanticWebLanguageServer/swls/commit/5fe3e6fe4dd91948bfc2bc6584eb6716e41946a4))
    - Move lsp-bin to swls ([`2c3ccc0`](https://github.com/SemanticWebLanguageServer/swls/commit/2c3ccc07ea7e71459e960c65c809c99b538d9569))
    - Remove logs + add context to sparql + fix race condition in app ([`6770fbf`](https://github.com/SemanticWebLanguageServer/swls/commit/6770fbf9b1ddcc044072c176e763a8f671d71e09))
    - Parse turtle with previous token information (subject or predicate or object) ([`02a28f0`](https://github.com/SemanticWebLanguageServer/swls/commit/02a28f0215c81cdef7bb1798c34398fbdf56b74b))
    - Remember turtle diffs ([`0087222`](https://github.com/SemanticWebLanguageServer/swls/commit/00872223ae341724739c12853a452719bb6f284c))
    - Move readFile to fs Resource + implement custom/readFile in vscode extension ([`2fdcd85`](https://github.com/SemanticWebLanguageServer/swls/commit/2fdcd85e82a36afef4110e6c353e84e55040f8fd))
    - Remove compiler warnings ([`973a7af`](https://github.com/SemanticWebLanguageServer/swls/commit/973a7afa89501d737db1a96bfeefadd9dc2b5070))
    - Goto definition resolves to local files in binary and does applyEdits for web lsps ([`36dbe24`](https://github.com/SemanticWebLanguageServer/swls/commit/36dbe2430f7f07c16237cf3b61400ec181ac10a6))
    - Fix semantic tokens issue ([`d418613`](https://github.com/SemanticWebLanguageServer/swls/commit/d4186135bd6bd8a986736b1246cff61b4d30f431))
    - Figure out how to apply edits for goto definitions ([`9281436`](https://github.com/SemanticWebLanguageServer/swls/commit/928143664e71f5f7654e96407d75568a84748caf))
    - Add conformance tests and move turtle tokenization to logos ([`0656e22`](https://github.com/SemanticWebLanguageServer/swls/commit/0656e2291020f2480173388e8b934c935826c1f2))
    - Add wasm guard statements ([`27a1f4c`](https://github.com/SemanticWebLanguageServer/swls/commit/27a1f4c2b68009d7efc1081c063c9a73bd4f7436))
    - Fix tests ([`35e616d`](https://github.com/SemanticWebLanguageServer/swls/commit/35e616d027cf5ead6ca4466dcf7402da8fe1a77e))
    - Add timing information in lgos ([`10307c5`](https://github.com/SemanticWebLanguageServer/swls/commit/10307c5994b75fc75344585ce09048b69bddbbcd))
    - Check expanded term equality for references ([`20ff0c0`](https://github.com/SemanticWebLanguageServer/swls/commit/20ff0c0e70ea58ef21b267293eb26f499f826870))
    - Goto implementation is actually goto definition ([`89a70d2`](https://github.com/SemanticWebLanguageServer/swls/commit/89a70d2b7fe55b80782db17938451148173be323))
    - Add cached lov fetching ([`aeea720`](https://github.com/SemanticWebLanguageServer/swls/commit/aeea7201a0f24373b52138df9e4d315fac0f65e7))
    - Goto implementation ([`ee00eba`](https://github.com/SemanticWebLanguageServer/swls/commit/ee00eba3671d7f6847eb367f58e2f6a0467ebc2b))
    - Add goto references ([`735508d`](https://github.com/SemanticWebLanguageServer/swls/commit/735508d850c83f82647063f86974229e82f92077))
    - Cargo fmt ([`464a42a`](https://github.com/SemanticWebLanguageServer/swls/commit/464a42a93db9044bd7e661721875e33c2c57f525))
    - Derive all property and class types ([`4607c51`](https://github.com/SemanticWebLanguageServer/swls/commit/4607c5154dcb1b42d2e052fe5277f247794e1d44))
    - Add documentation icons ([`b34ad57`](https://github.com/SemanticWebLanguageServer/swls/commit/b34ad5790074e5d61d909b086f2157809f1d7d36))
    - Add favicon ([`6c8ba15`](https://github.com/SemanticWebLanguageServer/swls/commit/6c8ba154c9a7000435931b8e1c3d77fadf243714))
    - Make app work again + refactor links ([`03e187e`](https://github.com/SemanticWebLanguageServer/swls/commit/03e187e04492b42b18d85558d33018438087e9b3))
    - Fix tests ([`87af94f`](https://github.com/SemanticWebLanguageServer/swls/commit/87af94fc0a10aa789a75cf7b44673951c4bb0a57))
    - Refactor + add documentation ([`f45a56d`](https://github.com/SemanticWebLanguageServer/swls/commit/f45a56d88c5dabca1889b1609ea5d49bb8675869))
    - Cargo +nightly fmt ([`d4b3573`](https://github.com/SemanticWebLanguageServer/swls/commit/d4b357369b476ba6c55ce4e934900f5cac43f4dd))
    - Enable semantic highlighting in vscode and publish ([`fbe1ecf`](https://github.com/SemanticWebLanguageServer/swls/commit/fbe1ecfa1bb567e012421f25407b5deb240241d4))
    - Cargo fix ([`b1249be`](https://github.com/SemanticWebLanguageServer/swls/commit/b1249be37853eea36ae35b76a0b79ed0b4d34136))
    - Use refactor in lang-turtle ([`634f221`](https://github.com/SemanticWebLanguageServer/swls/commit/634f221ce1353f5579ac49f3490bd6bd86e58068))
    - Cargo fmt ([`92ff670`](https://github.com/SemanticWebLanguageServer/swls/commit/92ff67014c3461231042156fff1fe2c8617ecf07))
    - Refactor continues ([`ab76f4b`](https://github.com/SemanticWebLanguageServer/swls/commit/ab76f4b7c100289e9f0adbce21336e36702637b1))
    - Document components ([`147b22a`](https://github.com/SemanticWebLanguageServer/swls/commit/147b22a8bae8bdcebde20f2da762c8c86331756f))
    - Fix tests ([`56ca3e6`](https://github.com/SemanticWebLanguageServer/swls/commit/56ca3e689db15f86f58a541425adf3c7be9492fe))
    - Remove warnings ([`f5045f5`](https://github.com/SemanticWebLanguageServer/swls/commit/f5045f5dad25335c1f4bc96aea189a8a2332c15d))
    - Cargo fix --lib ([`ebcef05`](https://github.com/SemanticWebLanguageServer/swls/commit/ebcef058efc5674a904d4edd5b2699e5e45232c0))
    - Refactor Spanned ([`d35e695`](https://github.com/SemanticWebLanguageServer/swls/commit/d35e695b22a2e71e4219600d0739ae51b5cdd70b))
    - Refactor completion ([`ff89762`](https://github.com/SemanticWebLanguageServer/swls/commit/ff89762c2fe89eb3ed947cd20d59c0e246eef7de))
    - Start add docs and refactor ([`2798feb`](https://github.com/SemanticWebLanguageServer/swls/commit/2798feb2d43106eebb00e1054f6931cda06c22a7))
    - Rewrite vscode extension ([`084ee88`](https://github.com/SemanticWebLanguageServer/swls/commit/084ee888f9a923666eed8c6c620ca30638491d21))
    - Fix rename ([`f0b2087`](https://github.com/SemanticWebLanguageServer/swls/commit/f0b2087cfd50d5b834c9829d60373734bfcb974b))
    - Add rename ([`18937ec`](https://github.com/SemanticWebLanguageServer/swls/commit/18937ec14cbd1186ab4e585b9f398c548371aef9))
    - Add keyboard shortcuts to demo ([`3da8dcc`](https://github.com/SemanticWebLanguageServer/swls/commit/3da8dcc5c28de14aa1818bb464b223c8dca96563))
    - Shape validation on save ([`814309a`](https://github.com/SemanticWebLanguageServer/swls/commit/814309aeab9bff1b8998249681cc8247278f4387))
    - Add shacl validations ([`c22f73e`](https://github.com/SemanticWebLanguageServer/swls/commit/c22f73ec5023485bf467e968d25609a0b95301a7))
    - Add prefix.cc completion ([`d5f46bc`](https://github.com/SemanticWebLanguageServer/swls/commit/d5f46bc03e7f8c0c05239eb39151e71991a2634b))
    - Enable lsp on monaco ([`f0a8e89`](https://github.com/SemanticWebLanguageServer/swls/commit/f0a8e89611937a7f374871f1eb3ad59292c66472))
    - Move to cargo make, move lsp-bin/src/backend.rs to core/src/backend.rs, add wasm compile ([`5ef8c77`](https://github.com/SemanticWebLanguageServer/swls/commit/5ef8c777c59bf4eb78f809ae8dcbe818942f9882))
    - Add undefined prefixes diagnostic ([`98585a2`](https://github.com/SemanticWebLanguageServer/swls/commit/98585a2a2b668f547018b0bdcd1ac9a2f20bfb85))
    - Add types on hover ([`d3cf042`](https://github.com/SemanticWebLanguageServer/swls/commit/d3cf04201447f2202cc4401b333de62eb0f29f35))
    - Add types on hover ([`112ea76`](https://github.com/SemanticWebLanguageServer/swls/commit/112ea76a807d22c0dfc77ca75d6c7926238e33cc))
    - Add things ([`e54f7bc`](https://github.com/SemanticWebLanguageServer/swls/commit/e54f7bc5e7470a9c506dd8b263d12d63329a8a1b))
    - Fix some bugs and better derive triples ([`e04ab58`](https://github.com/SemanticWebLanguageServer/swls/commit/e04ab588d0f2cc90b8b2bd003b050fd52b7cb08b))
    - Add sparql-lang triples ([`1718322`](https://github.com/SemanticWebLanguageServer/swls/commit/171832265be634eb7b239b8a8ba6786b9bca57d8))
    - Start sparql ([`865fcdc`](https://github.com/SemanticWebLanguageServer/swls/commit/865fcdcb3c6247613585316e0ad849b8a08eb229))
    - Better parse json ([`da2f271`](https://github.com/SemanticWebLanguageServer/swls/commit/da2f271cdc3c010894679b323eb0c1328a52aa7b))
    - Add jsonld completion ([`34037c0`](https://github.com/SemanticWebLanguageServer/swls/commit/34037c066029c5d72b9cb2ebccd5db0e96937777))
    - Parse files with CreateEvent ([`6cdff13`](https://github.com/SemanticWebLanguageServer/swls/commit/6cdff13a4e383ca3f1fb816ced4bbe044584d539))
    - Parse jsonld triples ([`dd4d115`](https://github.com/SemanticWebLanguageServer/swls/commit/dd4d115b746bdd302f3a80805529b1d5683524f6))
    - Add property completion ([`8aa55a9`](https://github.com/SemanticWebLanguageServer/swls/commit/8aa55a93ca2cb995c60ab4adcd5bbcc894c73f9a))
    - Cleanup unused implementations ([`a14bcba`](https://github.com/SemanticWebLanguageServer/swls/commit/a14bcbafda4f4a3beff84561b28c21eb2ca41368))
    - Better simple completions ([`9ac70f8`](https://github.com/SemanticWebLanguageServer/swls/commit/9ac70f81993e4e96b7deb48dca56bbaf3636ae89))
    - Add turtle prefix document links ([`441b52d`](https://github.com/SemanticWebLanguageServer/swls/commit/441b52d5d0c67f2ec3d31e5c877041a73fb464c0))
    - Add lov prefix completion ([`40c922f`](https://github.com/SemanticWebLanguageServer/swls/commit/40c922f1ab74f606ae024909220cb986e996c6e2))
    - Derive more asref, asmut ([`6e73321`](https://github.com/SemanticWebLanguageServer/swls/commit/6e7332179cd074815b16e188238b9d35038a5bf3))
    - Work on completions ([`7a9d351`](https://github.com/SemanticWebLanguageServer/swls/commit/7a9d35178d954503131136f4c139a79b641669df))
    - Cargo fmt ([`60d4190`](https://github.com/SemanticWebLanguageServer/swls/commit/60d4190d37e1045a613264962f57061369b9bfcf))
    - Cleanup ([`f0208b9`](https://github.com/SemanticWebLanguageServer/swls/commit/f0208b9a0119fcb6b69d26801b7c9e6cc51975b2))
    - Remove mutexed world, use channels ([`a2c1ac9`](https://github.com/SemanticWebLanguageServer/swls/commit/a2c1ac93f6056a33677e6da6f85d5772d7e9b594))
    - Setup formatting ([`3c3054d`](https://github.com/SemanticWebLanguageServer/swls/commit/3c3054dcace0fdd4457978427a1098f6be2cc3c7))
    - Start using the ecs in the backend ([`ef97317`](https://github.com/SemanticWebLanguageServer/swls/commit/ef973170bb66b2cf4854826d6c1ddca39a0a75c9))
    - Start migrate to bevy ecs ([`32a931e`](https://github.com/SemanticWebLanguageServer/swls/commit/32a931eb641c715871b06307a94457ed0e2d22e7))
    - Format Cargo tomls ([`8aa4380`](https://github.com/SemanticWebLanguageServer/swls/commit/8aa4380fe90b36e63b174032bcc0e4fc9df9adab))
    - Move fetch to client ([`48ce642`](https://github.com/SemanticWebLanguageServer/swls/commit/48ce64280a4e407c637df0ca223b87c717259e61))
    - Split jsonld ([`9eee2f2`](https://github.com/SemanticWebLanguageServer/swls/commit/9eee2f23e6b0ff8b32f31ec407fbff5bf1caad11))
    - Split lang-turtle ([`50df638`](https://github.com/SemanticWebLanguageServer/swls/commit/50df638d5687a9a165745066c9fe937ee57cd447))
    - Nexter steps ([`8f5a01d`](https://github.com/SemanticWebLanguageServer/swls/commit/8f5a01de5b88d0ce108a1d6ca8487060c395a982))
    - Next steps ([`cf866ec`](https://github.com/SemanticWebLanguageServer/swls/commit/cf866ec0b80f6da699c0007fd4e51c526d30c3cf))
    - Start adding web and core workspace packages ([`fd37f28`](https://github.com/SemanticWebLanguageServer/swls/commit/fd37f2876fc6b911f0fe467eccb0838c8fcc9aa0))
</details>

## 0.2.1 (2026-07-07)

### New Features

 - <csr-id-43a8054c274bde6e63475dcdccd65af9d24b7613/> prefix hover/goto-definition + JSON-LD prefix-diagnostics opt-out
   Prefix handling across languages, plus model-authoritative JSON-LD
   semantic-token colouring and the datatype-span work this built on.
   
   Hover / goto-definition on prefixes (new core/src/systems/prefix_hover.rs):
   - get_current_prefix detects a prefix declaration / JSON-LD @context term at the
   cursor from the parsed model (which, unlike derived triples, carries spans) and
   drops the wrong nearest-triple fallback so the triple-based hovers no-op.
   - hover_prefix shows the prefix -> namespace mapping (+ LOV title when known).
   - goto_prefix jumps a real namespace to its ontology file; goto_cjs now steps
   aside for namespaces but still resolves term aliases to their CJS definition.
   - Wired into the Hover and GotoDefinition schedules; new `definition()` e2e
   harness helper; e2e/tests/prefix_hover.rs.
   
   JSON-LD prefix diagnostics:
   - supports_prefix_diagnostics() = false for JSON-LD; @context has no prefix/alias
   distinction and pulls in shared/remote terms, so the span-based detector emitted
   false positives. e2e/tests/prefix_diagnostics.rs updated to assert the opt-out.
   
   Prefix diagnostics core:
   - Walk the parsed Turtle model instead of the derived triples so prefixes that
   only appear in a datatype ("5"^^xsd:integer) are detected.
   
   JSON-LD semantic tokens (model-authoritative):
   - Colour every term from the parsed model: a compact IRI `"ex:obs1"` is the term
   colour (quotes + local = enumMember) with `prefix:` NAMESPACE on top, applied
   consistently to @id subjects and nested object references; @context prefix keys
   are NAMESPACE.
   - The lexer supplies only what the model can't see: keywords (`@id`, `@type`, …)
   and the STRING/NUMBER/BOOLEAN base for real literals. Its previous
   string-followed-by-`:` NAMESPACE guess is removed.
   - Keep KEYWORD colouring for `@type` even though it is the rdf:type predicate in
   the model. Adapt to the rdf-parsers Spanned lang/ty API and carry
   context-computed IRIs. Tests in lang-jsonld/src/ecs/mod.rs pin the byte types.
   
   Depends on rdf-parsers 0.1.16.
 - <csr-id-817cc6bfcc92911b6c6be7b491ab1b00b85e13b9/> better turtle formatting + consolidate language specific implementation (removing generic marker)

## v0.2.0 (2026-06-29)

<csr-id-53cc4155da52b7054793c81009832fba55c3e2fb/>

### Chore

 - <csr-id-53cc4155da52b7054793c81009832fba55c3e2fb/> prune dead features, dependencies, and the swls-token-helpers crate
   - Remove the always-on `shapes` gate (SHACL validation was never actually
     toggleable since parse/save referenced it unconditionally) and the dead
     `tokio`/`agnostic` features. The `sparql_service`/`mie` deps the `tokio`
     feature gated are kept as version pins with `default-features = false`:
     shacl_validation pulls them in transitively, newer releases break against
     the =0.2.9 rudof/shacl stack, and their default `sparql` feature would chain
     into `rudof_rdf/sparql` and drag in tokio/reqwest/mio — which breaks the
     wasm build (swls-web). SHACL only uses NativeEngine, so `sparql` is never
     wanted.
   - Make `tower-lsp` a direct dependency of the `swls` binary instead of a
     shared workspace dependency (core only references it in docs).
   - Remove dead deps `similar`, `lazy_static`, `logos`, `chumsky`, plus a
     full `cargo machete` sweep across every crate.
   - Delete the orphaned `swls-token-helpers` crate (not a workspace member,
     no dependents).
   - Sync `[workspace.dependencies]` version constraints with the bumped crate
     versions so the workspace resolves again.

### New Features

 - <csr-id-832b6ae7122bc4b071a1a68ffba7c9bc2956d7e3/> make N3 support opt-in (disabled by default)
   N3 stays compiled in but is now gated off at runtime unless the user sets
   `"n3": true` in config. Flips the two existing per-language gates from
   `unwrap_or(true)` to `unwrap_or(false)`:
   
   - parse gate (`parse_n3_system`): `.n3` documents aren't parsed by default.
   - document selector (`backend.rs`): the server no longer advertises handling
   the `n3` language, so clients don't route `.n3` files to it.
   
   The n3 unit tests set `config.n3 = Some(true)` so they still exercise the
   parser. Other languages remain enabled by default.
 - <csr-id-faad6d5d3e8023269a265c58352644e590bc47f2/> gate features with configuration
 - <csr-id-12b14f9f8b92f0163b3e0e25842df707bcf1e785/> add automatic insert of prefix statements when writing colon
 - <csr-id-71381b48710c992a6233aa080af53730d2657040/> inline and extract blank nodes
 - <csr-id-e76bb7f72a54acfdd3b6ed56687a51930f0c12e6/> add undefined iri's warnings
 - <csr-id-754ce139d2f65c2486ff027786bec722844c3779/> prefix diagnostics
 - <csr-id-91525d7d08b5f1d52f0ce4096e39bd844d082ded/> move tower_lsp to swls binary
 - <csr-id-a0d86b89724b070322f63e75ea0e27aacc062a28/> start rename

### Bug Fixes

 - <csr-id-a8632e1da167bb0d7b38513a8e0cefe6d59fdb5a/> incorrect json-ld alias marked as unused + fix spans on diagnostics
 - <csr-id-5085b23f3eb4309daf5d6b13853cfb89facae3d6/> improve rename robustness
 - <csr-id-3bbf1cd80df9ff26b109c7e30da844cbc262d66e/> remove unused systems
 - <csr-id-1292fb8ccbdb3b10f01a7eb9e6bc64ccfcbda9dc/> better timing on the validation to reduce race conditions
 - <csr-id-dc8482d1c433868c18ec54017d4bc2b12776592c/> diagnostics after on change things works way better

## v0.1.4 (2026-06-22)

### New Features

 - <csr-id-bb4f08bed28af56f776d32f787a459c7325ec47e/> gate features with configuration
 - <csr-id-4bad0c7033f9029315e831d5801922f959a64165/> add automatic insert of prefix statements when writing colon
 - <csr-id-ece087810771449d6e3e9badcc21b123af613879/> inline and extract blank nodes
 - <csr-id-5daeb7fab3c033983ddb34cb6c0518eafcd0cbc1/> add undefined iri's warnings
 - <csr-id-b55d2807364d9521378e9d5433ee53d3d6bc8109/> prefix diagnostics
 - <csr-id-91c1ad0150e44713d3589bb264d704a4656e4a8a/> move tower_lsp to swls binary
 - <csr-id-251a00396498ebe11c4c9632afb72b6dd91dafda/> start rename

### Bug Fixes

 - <csr-id-e93ae6e2e86c1cdb634820f09c120663cf898fee/> incorrect json-ld alias marked as unused + fix spans on diagnostics
 - <csr-id-3604321f63b609c0095e507c094925d0d49894e5/> improve rename robustness
 - <csr-id-91a3739936dfb66c688d161c5264c997020abc86/> remove unused systems
 - <csr-id-aeb99acaba4869abdf8c7b8608b48c6ff91e0149/> better timing on the validation to reduce race conditions
 - <csr-id-e52d0f65cef812ffeab54b7d110045ce4c74f741/> diagnostics after on change things works way better

## v0.1.3 (2026-05-20)

### New Features

 - <csr-id-eb4144409025da4fba32892e10c207441c882de0/> use lov-mirror on github for better response times
 - <csr-id-9b17ed366f37598da0a9747dc51d552bee891ded/> better highlighting

## v0.1.2 (2026-04-30)

<csr-id-c5042d79e63cac0596075c50ae18c9d15cb5d9cb/>

### Documentation

 - <csr-id-682af7ba71d4e99d0f9516494fcd7ef552232f4d/> point crates to main readme + update readme

### New Features

 - <csr-id-2056a5f93ff4467478a4a72bf3f0c4c6691f78b0/> allow jsonld to show inlayed types in a json-ld manner
 - <csr-id-9e7d856a0991850c9fd51981a2555b4bdde9cb57/> move logging to lsp logging + actually spawn local

### Other

 - <csr-id-c5042d79e63cac0596075c50ae18c9d15cb5d9cb/> properly disable json-ld when the language is disabled

## v0.1.1 (2026-04-28)

<csr-id-4debbca4c3e1839781b7728951f91f2ba729165b/>

### New Features

 - <csr-id-37ca76298f9b43001bdcc6f9096a8611b8559128/> bump bevy_ecs to 0.18!
 - <csr-id-35ff9925b3957c491e3798a62ae4e4e32f88d40e/> make cjs work for vscode
 - <csr-id-478139e705d9f72c1ad9e8df5228379afd2b5b7f/> remove PathBuf from read dir
 - <csr-id-44cb452ffa0417562a5d26c863981419b56df701/> move components-rs to Url
 - <csr-id-663af2958384584b8111e41a3131fb55984b71bf/> fix many minor mistakes
 - <csr-id-987907ef012c3d41a094a3b7bf853ed0a55d23a8/> bump
 - <csr-id-97441d4d9dc7778d3fcb6b0862ad0d61fa38b01f/> check if cjs to properties and classes is a good idea
 - <csr-id-5ec44b5d4c325bb6687bf70292c29b74d4770ded/> goto @context string
 - <csr-id-f2a1679be0c5261e2705335eb2c2c1a14015906d/> improve cjs goto definition
 - <csr-id-9a887c510c8740c9d9f01a27f88aeca533a69cf3/> incorporate cjs, but we cannot yet parse the IRIs
 - <csr-id-85ebb4941c245ecf65c0146b644eee371cb7b939/> try to render hover as markdown
 - <csr-id-ade12adaca2ba8bf197b3aaf59091ee9ec266687/> add better jsonld support
 - <csr-id-a47bca07a0b3d01fbd411868bb7308cf9802bcf5/> things
 - <csr-id-7c78bd43551a5afcdaaa8bef83fe9fd9ecd4d443/> make jsonld context resolve with local contexts
 - <csr-id-aa9acc8a6c0565ef86b54545222717e00760ac00/> fix many bugs like highlighting, json-ld, autocompletion when not in a token etc
 - <csr-id-5157d404a94386a17c74ea125a6d8809412d10ac/> improve error spans
 - <csr-id-dc45e8187ffa2cd0a6d6aec35e7477802b8bcd88/> use new parser for turtle and sparql
 - <csr-id-721d151c1dbcaccab3a81f81f7d59f2517fe323c/> rename before publish
 - <csr-id-c6d99d1cfab4ab20fec9a1b376f3fa7c443d8a52/> move to upstream rudof
 - <csr-id-cf54f0bfd210e1b01311683399a70f8d7ae157e7/> optimize logging
 - <csr-id-799356a263277b24507115aa1574b5d0c1f7303a/> claude refactor
 - <csr-id-16579c4da87b74784982930c3af0dea8e938b0d5/> add unused prefix diagnostic and organize imports code action
 - <csr-id-27b8c86e7d11ebf2faee236c03b360e37c024e28/> add file based configs + order prefix completions based on popularity

### Bug Fixes

 - <csr-id-29ef7e453047b405fe76ff846b8a0bf230489bef/> make cargo release happy with version numbers
 - <csr-id-acd94a957d7ad001014eadef773bf59ee0fa6391/> remove unused imports warnings

### Other

 - <csr-id-4debbca4c3e1839781b7728951f91f2ba729165b/> fix jsonld goto definition for the last time!

