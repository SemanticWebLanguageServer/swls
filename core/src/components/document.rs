use std::collections::HashSet;

use bevy_ecs::prelude::*;
use derive_more::{AsMut, AsRef, Deref, DerefMut};

use crate::{lang::LangHelper, lsp_types::Position, prelude::*, systems::TypeId};

#[derive(Component, Default, Debug, Clone, Eq, PartialEq)]
pub struct CurrentType(pub HashSet<TypeId>);

/// [`Component`] that contains the parsed semantic element.
///
/// Every RDF language (Turtle, TriG, SPARQL, N3, JSON-LD) parses into the shared
/// [`Turtle`](rdf_parsers::model::Turtle) model, so this is a single concrete
/// component rather than being generic over the language.  Systems that must run
/// for one language only pair a `&Element` query with a `With<LangMarker>` filter
/// (e.g. `With<TurtleLang>`).
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct Element(pub Spanned<rdf_parsers::model::Turtle>);

/// Simple wrapper structure that derives [`Component`]
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct Wrapped<E>(pub E);

/// Simple wrapper for errors that derives [`Component`]
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct Errors<E>(pub Vec<E>);

/// [`Component`] containing the current source code as [`String`]
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct Source(pub String);

/// [`Component`] containing the current source code as a [`LineIndex`], used for
/// LSP byte ↔ position conversions.
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug, Default)]
pub struct RopeC(pub LineIndex);

/// [`Component`] that allows for language specific implementation for certain things, reducing
/// code duplication.
#[derive(Component, Debug, AsRef, Deref)]
pub struct DynLang(pub Box<dyn LangHelper + 'static + Send + Sync>);

/// [`Component`] indicating whether or not the document is actually open.
///
/// Documents that are not [`Open`] don't publish diagnostics for example
#[derive(Component, Debug)]
pub struct Open;

/// [`Component`] indicating whether or not the document is dirty, a dirty document parsed with
/// errors.
///
/// A document is often Dirty, computational intens calculation can be done on documents that are
/// not dirty, like [`derive_classes`](crate::prelude::systems::derive_classes) and [`derive_properties`](crate::prelude::systems::derive_properties).
#[derive(Component, Debug)]
pub struct Dirty;

/// A document the server loaded by itself (an LOV ontology, an `owl:imports`
/// target, a configured ontology or shape), as opposed to one the editor opened.
///
/// Its triples feed the shared store. When the user opens it in the editor the
/// buffer is shown and analysed, but edits never reach the store, and closing it
/// restores the server's copy (see [`crate::document`]).
#[derive(Component, Debug)]
pub struct Background;

/// The server's copy of a [`Background`] document, kept while the editor has it
/// [`Open`] so it can be restored when the editor closes it.
#[derive(Component, Debug)]
pub struct Snapshot(pub String);

/// Indicates that this should be a global entity, linked to all other documents
#[derive(Component, Debug)]
pub struct Global;

/// [`Component`] containing the [`lsp_types::Url`] of the current document.
///
/// Immutable: every document is indexed by its label in [`Documents`], which
/// component hooks keep up to date. Insert a new `Label` to relabel a document.
#[derive(Component, AsRef, Deref, Debug)]
#[component(immutable, on_insert = register_document, on_replace = unregister_document)]
pub struct Label(pub crate::lsp_types::Url);

/// Every document in the world, by URL.
///
/// Maintained by [`Label`]'s hooks, so it is correct however a document is
/// spawned, relabelled or despawned. Look documents up with
/// [`find_document`] rather than scanning labels.
///
/// What kind of document an entity is follows from its markers: [`Open`] (the
/// editor has it), [`Background`] (the server loaded it), both (a background
/// document the user opened), and for a background document whether its URL
/// is in the ontology cache (`FsTrait::is_cache_url`).
#[derive(Resource, Debug, Default)]
pub struct Documents(std::collections::HashMap<crate::lsp_types::Url, Entity>);

impl Documents {
    /// The document labelled `url`.
    pub fn get(&self, url: &crate::lsp_types::Url) -> Option<Entity> {
        self.0.get(url).copied()
    }

    /// Every document, by URL.
    pub fn iter(&self) -> impl Iterator<Item = (&crate::lsp_types::Url, Entity)> {
        self.0.iter().map(|(u, e)| (u, *e))
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

fn register_document(
    mut world: bevy_ecs::world::DeferredWorld,
    ctx: bevy_ecs::lifecycle::HookContext,
) {
    let Some(url) = world.get::<Label>(ctx.entity).map(|l| l.0.clone()) else {
        return;
    };
    if let Some(mut docs) = world.get_resource_mut::<Documents>() {
        if let Some(previous) = docs.0.insert(url.clone(), ctx.entity) {
            if previous != ctx.entity {
                tracing::warn!("Two documents labelled {}, keeping the newest", url);
            }
        }
    }
}

fn unregister_document(
    mut world: bevy_ecs::world::DeferredWorld,
    ctx: bevy_ecs::lifecycle::HookContext,
) {
    let Some(url) = world.get::<Label>(ctx.entity).map(|l| l.0.clone()) else {
        return;
    };
    if let Some(mut docs) = world.get_resource_mut::<Documents>() {
        if docs.0.get(&url) == Some(&ctx.entity) {
            docs.0.remove(&url);
        }
    }
}

/// The document labelled `url`, from the [`Documents`] registry (or, in a
/// world set up without it, by scanning labels).
pub fn find_document(world: &mut World, url: &crate::lsp_types::Url) -> Option<Entity> {
    if let Some(docs) = world.get_resource::<Documents>() {
        return docs.get(url);
    }
    world
        .query::<(Entity, &Label)>()
        .iter(world)
        .find(|(_, l)| &l.0 == url)
        .map(|(e, _)| e)
}

/// [`Component`] used to remember the linked documents.
///
/// This is used, for example, to only suggest properties defined in a linked document.
/// Or only validate with shapes found in linked documents.
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug, Clone)]
pub struct DocumentLinks(pub Vec<(crate::lsp_types::Url, &'static str)>);

/// [`Component`] used to wrap an incoming [`lsp_types::Position`].
///
/// This component is translated into [`TokenComponent`] and [`TripleComponent`]
/// with [`get_current_token`]
/// and [get_current_triple] respectively.
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct PositionComponent(pub Position);

/// [`Component`] containing the typical keywords for the current language.
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct KeyWords(pub Vec<&'static str>);

/// [`Component`] containing comment text extracted from the CST (stripped of `#` prefix).
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug, Default)]
pub struct Comments(pub Vec<crate::util::Spanned<String>>);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lsp_types::Url;

    fn url(s: &str) -> Url {
        Url::parse(s).unwrap()
    }

    fn world() -> World {
        let mut world = World::new();
        world.init_resource::<Documents>();
        world
    }

    #[test]
    fn registry_follows_spawn_relabel_and_despawn() {
        let mut world = world();
        let a = world.spawn(Label(url("file:///a.ttl"))).id();
        let b = world.spawn(Label(url("file:///b.ttl"))).id();
        assert_eq!(find_document(&mut world, &url("file:///a.ttl")), Some(a));
        assert_eq!(world.resource::<Documents>().len(), 2);

        world
            .entity_mut(a)
            .insert(Label(url("file:///renamed.ttl")));
        assert_eq!(find_document(&mut world, &url("file:///a.ttl")), None);
        assert_eq!(
            find_document(&mut world, &url("file:///renamed.ttl")),
            Some(a)
        );

        world.despawn(b);
        assert_eq!(find_document(&mut world, &url("file:///b.ttl")), None);
        assert_eq!(world.resource::<Documents>().len(), 1);

        world.entity_mut(a).remove::<Label>();
        assert!(world.resource::<Documents>().is_empty());
    }

    #[test]
    fn removing_a_stale_duplicate_keeps_the_current_document() {
        let mut world = world();
        let first = world.spawn(Label(url("file:///x.ttl"))).id();
        let second = world.spawn(Label(url("file:///x.ttl"))).id();
        assert_eq!(
            find_document(&mut world, &url("file:///x.ttl")),
            Some(second)
        );
        world.despawn(first);
        assert_eq!(
            find_document(&mut world, &url("file:///x.ttl")),
            Some(second)
        );
    }

    #[test]
    fn lookup_without_the_registry_scans_labels() {
        let mut world = World::new();
        let a = world.spawn(Label(url("file:///a.ttl"))).id();
        assert_eq!(find_document(&mut world, &url("file:///a.ttl")), Some(a));
    }
}
