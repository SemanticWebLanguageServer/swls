//! # SWLS End-to-End Test Harness
//!
//! This crate provides [`LspHarness`], a synchronous wrapper around the ECS world that makes
//! it easy to write E2E tests for LSP features (completion, hover, diagnostics, formatting).
//!
//! ## Quick start
//!
//! ```rust,no_run
//! use swls_e2e_tests::LspHarness;
//!
//! let mut h = LspHarness::new();
//! let file = h.open_file("file:///test.ttl", "turtle",
//!     "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n<> a foaf:");
//!
//! // drain async tasks so LOV / ontology fetches complete
//! h.drain_tasks();
//!
//! let completions = h.completions(&file, 1, 7);
//! h.assert_completions(&completions)
//!     .count_at_least(1);
//! ```
//!
//! ## Position convention
//!
//! Positions are 0-indexed `(line, character)` matching the LSP spec.  The harness works at
//! the ECS level and does **not** apply the `Backend::adjust_position` -1 offset.  Pass the
//! character index of the **first character of the token** you want resolved (not the cursor
//! position after the last typed character).  This is consistent with the position values used
//! in the existing unit tests inside `lang-turtle`.

#![allow(dead_code)]

use std::collections::HashMap;

use bevy_ecs::{prelude::*, world::World};
use futures::{channel::mpsc::UnboundedReceiver, executor::block_on};
use ropey::Rope;
use swls_core::{
    feature::{
        code_action::{CodeActionRequest, Label as CodeActionLabel},
        completion::{CompletionRequest, Label as CompletionLabel, SimpleCompletion},
        diagnostics::DiagnosticItem,
        format::{FormatRequest, Label as FormatLabel},
        hover::{HoverRequest, Label as HoverLabel},
        parse::Label as ParseLabel,
        rename::{PrepareRenameRequest, PrepareRename as PrepareRenameLabel, Rename as RenameLabel, RenameEdits},
    },
    lsp_types::{CodeAction, CompletionItemKind, Diagnostic, Position, Range, TextEdit, Url},
    prelude::*,
    Tasks,
};
use swls_test_utils::{create_file, setup_world, TestClient};

// ─── Harness ─────────────────────────────────────────────────────────────────

/// The central test fixture.  Create one per test (or reuse across related tests that share
/// documents).  All operations are synchronous — async tasks are drained on demand via
/// [`drain_tasks`](LspHarness::drain_tasks).
pub struct LspHarness {
    world: World,
    entities: HashMap<String, Entity>,
    diag_rx: UnboundedReceiver<DiagnosticItem>,
}

impl LspHarness {
    /// Create a harness with **all** SWLS language plugins registered (Turtle, SPARQL, TriG,
    /// JSON-LD) and the task queue initialised.
    pub fn new() -> Self {
        Self::new_with(|_| {})
    }

    /// Like [`new`](Self::new), but also lets you register mock HTTP resources that will be
    /// returned by the `TestClient` when the server fetches URLs (e.g. LOV, ontologies, imports).
    ///
    /// ```rust,no_run
    /// use swls_e2e_tests::LspHarness;
    ///
    /// let mut h = LspHarness::with_resources([
    ///     ("http://xmlns.com/foaf/0.1/", "@prefix foaf: <http://xmlns.com/foaf/0.1/> ."),
    /// ]);
    /// ```
    pub fn with_resources<'a>(resources: impl IntoIterator<Item = (&'a str, &'a str)>) -> Self {
        let mut client = TestClient::new();
        for (url, content) in resources {
            client.add_res(url, content);
        }
        Self::new_with_client(client, |_| {})
    }

    /// Internal: build with a custom `TestClient` and a post-setup hook.
    fn new_with(extra_setup: impl FnOnce(&mut World)) -> Self {
        Self::new_with_client(TestClient::new(), extra_setup)
    }

    fn new_with_client(client: TestClient, extra_setup: impl FnOnce(&mut World)) -> Self {
        let (world, diag_rx) = setup_world(client, |world| {
            swls_lang_turtle::setup_world::<TestClient>(world);
            swls_lang_sparql::setup_world(world);
            swls_lang_trig::setup_world::<TestClient>(world);
            swls_lang_jsonld::setup_world::<TestClient>(world);
            extra_setup(world);
        });
        Self {
            world,
            entities: HashMap::new(),
            diag_rx,
        }
    }
}

// ─── File management ──────────────────────────────────────────────────────────

impl LspHarness {
    /// Open a new file in the LSP world and run the initial parse pass.
    ///
    /// Returns a [`FileHandle`] that can be passed to request methods.
    pub fn open_file(&mut self, url: &str, lang: &str, content: &str) -> FileHandle {
        let entity = create_file(&mut self.world, content, url, lang, Open);
        self.world.run_schedule(ParseLabel);
        self.entities.insert(url.to_string(), entity);
        FileHandle {
            entity,
            url: url.to_string(),
        }
    }

    /// Update the source of an already-open file and re-parse.
    pub fn update_file(&mut self, handle: &FileHandle, new_content: &str) {
        self.world
            .entity_mut(handle.entity)
            .insert((Source(new_content.to_string()), RopeC(Rope::from_str(new_content))));
        self.world.run_schedule(ParseLabel);
    }

    /// Open a secondary file (not `Open`) so it is available as a linked document but does not
    /// have completion / hover run on it directly.  Useful for simulating imported ontologies.
    pub fn open_linked_file(&mut self, url: &str, lang: &str, content: &str) -> FileHandle {
        let entity = create_file(&mut self.world, content, url, lang, ());
        self.world.run_schedule(ParseLabel);
        self.entities.insert(url.to_string(), entity);
        FileHandle {
            entity,
            url: url.to_string(),
        }
    }
}

// ─── Async task management ────────────────────────────────────────────────────

impl LspHarness {
    /// Drain all pending async tasks (e.g. LOV prefix fetches, ontology loads, import
    /// resolution).  Call this after opening files when tests depend on data that is loaded
    /// asynchronously.
    ///
    /// Blocks the current thread until the task counter reaches zero and one final `Tasks`
    /// schedule tick confirms no new tasks were spawned.
    pub fn drain_tasks(&mut self) {
        let c = self.world.resource::<TestClient>().clone();
        block_on(c.await_futures(|| self.world.run_schedule(Tasks)));
    }

    /// Convenience: open a file **and** drain tasks in one step.
    pub fn open_file_and_drain(&mut self, url: &str, lang: &str, content: &str) -> FileHandle {
        let handle = self.open_file(url, lang, content);
        self.drain_tasks();
        handle
    }
}

// ─── LSP feature methods ──────────────────────────────────────────────────────

impl LspHarness {
    /// Request completion items at `(line, character)`.
    ///
    /// Positions are 0-indexed as in the LSP spec.  The character value is the index of the
    /// **first character of the token** being completed (consistent with the existing unit tests).
    pub fn completions(&mut self, handle: &FileHandle, line: u32, character: u32) -> Vec<SimpleCompletion> {
        self.world.entity_mut(handle.entity).insert((
            CompletionRequest(Vec::new()),
            PositionComponent(Position { line, character }),
        ));
        self.world.run_schedule(CompletionLabel);
        self.world
            .entity_mut(handle.entity)
            .take::<CompletionRequest>()
            .map(|r| r.0)
            .unwrap_or_default()
    }

    /// Request hover information at `(line, character)`.
    pub fn hover(&mut self, handle: &FileHandle, line: u32, character: u32) -> Vec<String> {
        self.world.entity_mut(handle.entity).insert((
            HoverRequest::default(),
            PositionComponent(Position { line, character }),
        ));
        self.world.run_schedule(HoverLabel);
        self.world
            .entity_mut(handle.entity)
            .take::<HoverRequest>()
            .map(|r| r.0)
            .unwrap_or_default()
    }

    /// Request document formatting.  Returns the list of `TextEdit`s the server would send,
    /// or `None` if no formatting was produced.
    pub fn format(&mut self, handle: &FileHandle) -> Option<Vec<TextEdit>> {
        self.world
            .entity_mut(handle.entity)
            .insert(FormatRequest(None));
        self.world.run_schedule(FormatLabel);
        self.world
            .entity_mut(handle.entity)
            .take::<FormatRequest>()
            .and_then(|r| r.0)
    }

    /// Check whether the document currently has the `Dirty` marker (i.e. has parse errors).
    pub fn is_dirty(&self, handle: &FileHandle) -> bool {
        self.world.entity(handle.entity).contains::<Dirty>()
    }

    /// Read the `Triples` component of a file, returning the number of parsed RDF triples.
    /// Returns 0 if no triples have been derived yet (e.g. the file is dirty).
    pub fn triple_count(&self, handle: &FileHandle) -> usize {
        self.world
            .entity(handle.entity)
            .get::<Triples>()
            .map(|t| t.0.len())
            .unwrap_or(0)
    }

    /// Re-run the `ParseLabel` schedule (which now also publishes diagnostics) and
    /// return the current diagnostics for all open files.
    ///
    /// Because `DiagnosticPublisher` re-sends the full merged set for a URI on every
    /// `publish()` call, we keep only the **last** item per URI — that is always the
    /// most up-to-date merged state (all reasons combined).
    pub fn run_diagnostics(&mut self) -> Vec<(Url, Diagnostic)> {
        self.world.run_schedule(ParseLabel);
        // Drain the channel, keeping only the last item per URI.
        let mut latest: HashMap<Url, Vec<Diagnostic>> = HashMap::new();
        while let Ok(item) = self.diag_rx.try_recv() {
            latest.insert(item.uri.clone(), item.diagnostics);
        }
        latest
            .into_iter()
            .flat_map(|(url, diags)| diags.into_iter().map(move |d| (url.clone(), d)))
            .collect()
    }

    /// Run the `CodeActionLabel` schedule and return the list of code actions.
    pub fn code_actions(&mut self, handle: &FileHandle) -> Vec<CodeAction> {
        self.world
            .entity_mut(handle.entity)
            .insert(CodeActionRequest::default());
        self.world.run_schedule(CodeActionLabel);
        self.world
            .entity_mut(handle.entity)
            .take::<CodeActionRequest>()
            .map(|r| r.0)
            .unwrap_or_default()
    }

    /// Run the `PrepareRename` schedule at `(line, character)` and return the result.
    ///
    /// Returns `Some(PrepareRenameResult)` when the position is over a renameable term,
    /// `None` when rename is not available at that position.
    pub fn prepare_rename(
        &mut self,
        handle: &FileHandle,
        line: u32,
        character: u32,
    ) -> Option<PrepareRenameResult> {
        self.world
            .entity_mut(handle.entity)
            .insert(PositionComponent(Position { line, character }));
        self.world.run_schedule(PrepareRenameLabel);
        self.world
            .entity_mut(handle.entity)
            .take::<PrepareRenameRequest>()
            .map(|r| PrepareRenameResult {
                range: r.range,
                placeholder: r.placeholder,
            })
    }

    /// Run the `Rename` schedule at `(line, character)` with `new_name` as the replacement.
    ///
    /// Returns all `(file_url, TextEdit)` pairs that should be applied to the workspace.
    /// `TextEdit.new_text` contains the fully-wrapped replacement (e.g. `<http://new>` for a
    /// Turtle IRI rename where `new_name = "http://new"`).
    pub fn rename(
        &mut self,
        handle: &FileHandle,
        line: u32,
        character: u32,
        new_name: &str,
    ) -> Vec<(Url, TextEdit)> {
        self.world.entity_mut(handle.entity).insert((
            PositionComponent(Position { line, character }),
            RenameEdits(Vec::new(), new_name.to_string()),
        ));
        self.world.run_schedule(RenameLabel);
        self.world
            .entity_mut(handle.entity)
            .take::<RenameEdits>()
            .map(|r| r.0)
            .unwrap_or_default()
    }
}

// ─── Assertion helpers ────────────────────────────────────────────────────────

impl LspHarness {
    /// Start a fluent assertion chain on a completion result.
    ///
    /// ```rust,no_run
    /// # use swls_e2e_tests::LspHarness;
    /// # let mut h = LspHarness::new();
    /// # let file = h.open_file("file:///t.ttl", "turtle", "");
    /// let completions = h.completions(&file, 0, 0);
    /// h.assert_completions(&completions)
    ///     .contains_label("@prefix")
    ///     .count_at_least(1);
    /// ```
    pub fn assert_completions<'a>(&self, completions: &'a [SimpleCompletion]) -> CompletionAssert<'a> {
        CompletionAssert { completions }
    }

    /// Convenience: assert that a hover result is non-empty.
    pub fn assert_hover_non_empty(&self, result: &[String], context: &str) {
        assert!(
            !result.is_empty(),
            "Expected hover to return content for {context}, got empty"
        );
    }
}

// ─── FileHandle ───────────────────────────────────────────────────────────────

/// A lightweight handle to a document opened in the [`LspHarness`].
///
/// Obtained from [`LspHarness::open_file`].  Cheap to clone.
#[derive(Debug, Clone)]
pub struct FileHandle {
    pub entity: Entity,
    pub url: String,
}

/// Result of a `prepare_rename` call.
#[derive(Debug, Clone)]
pub struct PrepareRenameResult {
    /// The range in the document that will be replaced by the rename.
    /// This is the *inner* range (without surrounding delimiters like `<>` or `""`).
    pub range: Range,
    /// The pre-filled text shown to the user in the rename input box.
    pub placeholder: String,
}

// ─── CompletionAssert ─────────────────────────────────────────────────────────

/// Fluent assertion builder for completion results.
///
/// Panics with a descriptive message on the first failing assertion.
pub struct CompletionAssert<'a> {
    completions: &'a [SimpleCompletion],
}

impl<'a> CompletionAssert<'a> {
    /// Assert that at least one completion has the given label.
    pub fn contains_label(self, label: &str) -> Self {
        let found = self.completions.iter().any(|c| c.label == label);
        assert!(
            found,
            "Expected completion list to contain label {:?}, but got: [{}]",
            label,
            self.completions
                .iter()
                .map(|c| c.label.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        );
        self
    }

    /// Assert that no completion has the given label.
    pub fn does_not_contain_label(self, label: &str) -> Self {
        let found = self.completions.iter().any(|c| c.label == label);
        assert!(
            !found,
            "Expected completion list NOT to contain label {:?}, but it was present",
            label
        );
        self
    }

    /// Assert that the completion list has at least `n` items.
    pub fn count_at_least(self, n: usize) -> Self {
        assert!(
            self.completions.len() >= n,
            "Expected at least {n} completions, got {}",
            self.completions.len()
        );
        self
    }

    /// Assert that the completion list has exactly `n` items.
    pub fn count_exactly(self, n: usize) -> Self {
        assert_eq!(
            self.completions.len(),
            n,
            "Expected exactly {n} completions, got {}",
            self.completions.len()
        );
        self
    }

    /// Assert that a completion with the given label has the expected kind.
    pub fn label_has_kind(self, label: &str, expected_kind: CompletionItemKind) -> Self {
        let item = self.completions.iter().find(|c| c.label == label);
        match item {
            None => panic!(
                "Completion label {:?} not found; available: [{}]",
                label,
                self.completions
                    .iter()
                    .map(|c| c.label.as_str())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Some(c) => assert_eq!(
                c.kind, expected_kind,
                "Completion {:?} has kind {:?}, expected {:?}",
                label, c.kind, expected_kind
            ),
        }
        self
    }

    /// Assert that at least one completion has a label matching the given prefix.
    pub fn contains_label_starting_with(self, prefix: &str) -> Self {
        let found = self.completions.iter().any(|c| c.label.starts_with(prefix));
        assert!(
            found,
            "Expected a completion label starting with {:?}, but got: [{}]",
            prefix,
            self.completions
                .iter()
                .map(|c| c.label.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        );
        self
    }

    /// Return the underlying completions for custom assertions.
    pub fn into_inner(self) -> &'a [SimpleCompletion] {
        self.completions
    }

    /// Print all completion labels to stdout (useful while debugging a test).
    pub fn debug_print(self) -> Self {
        println!("Completions ({}):", self.completions.len());
        for c in self.completions {
            println!("  {:?} ({:?})", c.label, c.kind);
        }
        self
    }
}

// ─── Default impl ─────────────────────────────────────────────────────────────

impl Default for LspHarness {
    fn default() -> Self {
        Self::new()
    }
}
