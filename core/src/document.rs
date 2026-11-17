//! Editor lifecycle of documents: `didOpen` / `didClose`.
//!
//! A URL can be both a document the server loaded by itself ([`Background`]:
//! LOV ontologies, `owl:imports`, configured ontologies and shapes) and one the
//! user opens in the editor, e.g. after goto-definition into the ontology cache.
//! They share one entity, with these rules:
//!
//! - While the editor has it [`Open`], the editor buffer is the document's text.
//!   Edits are analysed like any open document, but never reach the shared store
//!   ([`load_store`](crate::store::load_store) skips open documents), and a late
//!   background load does not overwrite the buffer
//!   ([`spawn_background`](crate::systems::spawn_background)).
//! - On close, a background document gets the server's copy back: the file on
//!   disk (for the cache, the server's snapshot; for the user's own file, what
//!   they saved), or the [`Snapshot`] taken on open.
//!
//! The LSP has no way to make a document read-only in the editor. Cache files
//! are read-only on disk, and opening one shows a notice explaining that edits
//! are not used.

use std::collections::HashMap;

use bevy_ecs::prelude::*;

use crate::{
    feature::{diagnostics::DiagnosticPublisher, parse::Label as ParseLabel},
    lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range, TextDocumentItem, Url},
    prelude::*,
};

/// Diagnostic reason of the notice shown on an open cache file.
const CACHED_COPY: &str = "cached_copy";

/// Open `item` in the editor and parse it. Returns its entity.
pub fn open_document(world: &mut World, item: TextDocumentItem) -> Entity {
    let url = item.uri.clone();

    // Keep the server's copy of a background document before the editor buffer
    // replaces it.
    let mut background = false;
    if let Some(e) = find_document(world, &url) {
        let entity = world.entity(e);
        background = entity.contains::<Background>();
        if background && !entity.contains::<Open>() && !entity.contains::<Snapshot>() {
            if let Some(source) = entity.get::<Source>() {
                let snapshot = Snapshot(source.0.clone());
                world.entity_mut(e).insert(snapshot);
            }
        }
    }

    // Publish the notice before spawning: spawning already runs the parse
    // schedule, whose diagnostics are merged with it.
    let is_cache = world
        .get_resource::<Fs>()
        .is_some_and(|fs| fs.0.is_cache_url(&url));
    if background && is_cache {
        if let Some(mut publisher) = world.get_resource_mut::<DiagnosticPublisher>() {
            let _ = publisher.publish(&item, vec![cached_copy_notice()], CACHED_COPY);
        }
    }

    let lang_id = Some(item.language_id.clone());
    let spawn = spawn_or_insert(
        url.clone(),
        (
            Source(item.text.clone()),
            Label(url.clone()),
            RopeC(LineIndex::new(&item.text)),
            Wrapped(item.clone()),
            DocumentLinks(Vec::new()),
            Open,
            Types(HashMap::new()),
        ),
        lang_id,
        (),
    );
    let entity = spawn(world);

    world.run_schedule(ParseLabel);
    world.flush();
    entity
}

fn cached_copy_notice() -> Diagnostic {
    Diagnostic {
        range: Range::new(Position::new(0, 0), Position::new(0, 0)),
        severity: Some(DiagnosticSeverity::INFORMATION),
        source: Some(String::from("SWLS")),
        message: String::from(
            "This is a copy cached by swls. Edits are not used by the language server \
             and are reverted when the file is closed.",
        ),
        ..Default::default()
    }
}

/// Outcome of [`close_document`].
#[derive(Debug)]
pub enum Closed {
    /// No document with that URL.
    Unknown,
    /// A document only the editor had; it no longer publishes diagnostics.
    Editor,
    /// A background document: give it back the server's copy with
    /// [`restore_background`], from disk or else from `snapshot`.
    Background {
        entity: Entity,
        snapshot: Option<String>,
    },
}

/// The editor closed `url`: stop treating it as open and clear its diagnostics.
pub fn close_document(world: &mut World, url: &Url) -> Closed {
    let Some(entity) = find_document(world, url) else {
        return Closed::Unknown;
    };

    let mut e = world.entity_mut(entity);
    e.remove::<Open>();
    let background = e.contains::<Background>();
    let snapshot = e.take::<Snapshot>().map(|s| s.0);

    if let Some(mut publisher) = world.get_resource_mut::<DiagnosticPublisher>() {
        let _ = publisher.clear(url);
    }

    if background {
        Closed::Background { entity, snapshot }
    } else {
        Closed::Editor
    }
}

/// Give a closed background document the server's `content` back and reparse
/// it, which reloads its triples in the store.
pub fn restore_background(world: &mut World, entity: Entity, content: String) {
    if world.get_entity(entity).is_err() {
        return;
    }
    world
        .entity_mut(entity)
        .insert((RopeC(LineIndex::new(&content)), Source(content)));
    world.run_schedule(ParseLabel);
    world.flush();
}
