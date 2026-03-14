use std::{borrow::Cow, collections::HashSet};

use bevy_ecs::prelude::*;
use derive_more::{AsMut, AsRef, Deref, DerefMut};

use crate::{
    lang::{Lang, LangHelper},
    lsp_types::Position,
    prelude::*,
    systems::TypeId,
};

#[derive(Component, Default, Debug, Clone, Eq, PartialEq)]
pub struct CurrentType(pub HashSet<TypeId>);

/// [`Component`] that contains the parsed semantic element (i.e. Turtle, JSONLD).
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct Element<L: Lang>(pub Spanned<L::Element>);

/// Simple wrapper structure that derives [`Component`]
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct Wrapped<E>(pub E);

/// Simple wrapper for errors that derives [`Component`]
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct Errors<E>(pub Vec<E>);

/// [`Component`] containing the current source code as [`String`]
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct Source(pub String);

/// [`Component`] containing the current source code as [`ropey::Rope`]
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct RopeC(pub ropey::Rope);

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

/// Indicates that this should be a global entity, linked to all other documents
#[derive(Component, Debug)]
pub struct Global;

/// [`Component`] containing the [`lsp_types::Url`] of the current document.
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct Label(pub crate::lsp_types::Url);

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
