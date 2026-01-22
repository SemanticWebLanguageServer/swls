use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    fmt::Debug,
    path::PathBuf,
};

use bevy_ecs::{prelude::*, world::CommandQueue};
use derive_more::{AsMut, AsRef, Deref, DerefMut};
use futures::channel::mpsc::{UnboundedReceiver, UnboundedSender};
use serde::Deserialize;

use crate::{
    lang::{Lang, LangHelper},
    lsp_types::{Position, Url, WorkspaceFolder},
    prelude::*,
    systems::{DefinedClass, DefinedProperty, TypeId},
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

/// [`Component`] containing the [`lsp_types::Url`] of the current document.
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct Label(pub crate::lsp_types::Url);

/// [`Resource`] used to receive command queues. These command queues are handled with [`handle_tasks`](crate::prelude::systems::handle_tasks).
#[derive(Resource, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct CommandReceiver(pub UnboundedReceiver<CommandQueue>);

/// [`Resource`] used to send command queues, allowing for async operations.
#[derive(Resource, AsRef, Deref, AsMut, DerefMut, Debug, Clone)]
pub struct CommandSender(pub UnboundedSender<CommandQueue>);

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

/// maps terms to all known correct types.
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct Types(pub HashMap<Cow<'static, str>, HashSet<TypeId>>);
impl Types {
    #[tracing::instrument(skip(self, hierarchy))]
    pub fn clean_up(&mut self, hierarchy: &TypeHierarchy<'_>) {
        for sets in self.0.values_mut() {
            let new_set = sets
                .iter()
                .filter(|&trying| {
                    for maybe_presnt in hierarchy.iter_superclass_ids(*trying) {
                        if maybe_presnt == *trying {
                            continue;
                        }
                        if sets.contains(&maybe_presnt) {
                            return false;
                        }
                    }
                    true
                })
                .copied()
                .collect();

            *sets = new_set;
        }

        tracing::info!(
            "found {} typed entities",
            self.iter().filter(|x| !x.1.is_empty()).count()
        );

        for (k, v) in self.iter() {
            for id in v {
                tracing::debug!("{} a {}", k, hierarchy.type_name(*id));
            }
        }
    }
}

/// [`Resource`] used to set and get all super and subtypes starting from a [`TypeId`]
///
/// Example
/// ```
/// use lsp_core::components::TypeHierarchy;
///
/// let mut hierarchy = TypeHierarchy::default();
/// let image_id = hierarchy.get_id("http://xmlns.com/foaf/0.1/Image");
/// let document_id = hierarchy.get_id("http://xmlns.com/foaf/0.1/Document");
/// hierarchy.set_subclass_of(image_id, document_id);
///
/// for ty in hierarchy.iter_superclass(document_id) {
///     // first "http://xmlns.com/foaf/0.1/Document"
///     // then "http://xmlns.com/foaf/0.1/Image"
///     println!("Type {}", ty);
/// }
/// ```
#[derive(Resource, Debug, Default)]
pub struct TypeHierarchy<'a> {
    numbers: HashMap<Cow<'a, str>, TypeId>,
    nodes: Vec<Cow<'a, str>>,
    subclass: Vec<HashSet<TypeId>>,
    superclass: Vec<HashSet<TypeId>>,
}

#[derive(Resource, Default)]
pub struct Ontologies {
    pub classes: HashMap<oxigraph::model::NamedNode, DefinedClass>,
    pub properties: HashMap<String, DefinedProperty>,
}

impl<'a> TypeHierarchy<'a> {
    pub fn get_id(&mut self, class: &str) -> TypeId {
        if let Some(id) = self.numbers.get(class) {
            *id
        } else {
            let new_id = TypeId(self.nodes.len());
            let class_cow: Cow<'a, str> = Cow::Owned(class.to_string());
            self.nodes.push(class_cow.clone());
            self.numbers.insert(class_cow, new_id);
            self.subclass.push(HashSet::new());
            self.superclass.push(HashSet::new());
            new_id
        }
    }

    pub fn get_id_ref(&self, class: &str) -> Option<TypeId> {
        self.numbers.get(class).copied()
    }

    pub fn set_subclass_of(&mut self, class: TypeId, to: TypeId) {
        self.subclass[class.0].insert(to);
        self.superclass[to.0].insert(class);
    }

    pub fn iter_subclass<'b>(&'b self, id: TypeId) -> impl Iterator<Item = Cow<'a, str>> + 'b {
        self.iter_subclass_ids(id)
            .map(|id| self.nodes[id.0].clone())
    }

    pub fn iter_subclass_ids<'b>(&'b self, id: TypeId) -> impl Iterator<Item = TypeId> + 'b {
        let mut stack = std::collections::VecDeque::new();
        stack.push_back(id);
        let mut done = HashSet::new();
        std::iter::from_fn(move || {
            while let Some(id) = stack.pop_front() {
                if done.contains(&id) {
                    continue;
                }
                done.insert(id);

                self.subclass[id.0].iter().for_each(|i| stack.push_back(*i));
                return Some(id);
            }

            None
        })
    }

    pub fn type_name(&self, id: TypeId) -> Cow<'a, str> {
        self.nodes[id.0].clone()
    }

    pub fn iter_superclass<'b>(&'b self, id: TypeId) -> impl Iterator<Item = Cow<'a, str>> + 'b {
        self.iter_superclass_ids(id)
            .map(|id| self.nodes[id.0].clone())
    }

    pub fn iter_superclass_ids<'b>(&'b self, id: TypeId) -> impl Iterator<Item = TypeId> + 'b {
        let mut stack = std::collections::VecDeque::new();
        stack.push_back(id);
        let mut done = HashSet::new();
        std::iter::from_fn(move || {
            while let Some(id) = stack.pop_front() {
                if done.contains(&id) {
                    continue;
                }
                done.insert(id);

                self.superclass[id.0]
                    .iter()
                    .for_each(|i| stack.push_back(*i));
                return Some(id);
            }

            None
        })
    }
}

#[derive(Debug, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Disabled {
    #[serde(alias = "SHAPES", alias = "shapes")]
    Shapes,
}

#[derive(Resource, Debug, Default)]
pub struct ServerConfig {
    pub workspaces: Vec<WorkspaceFolder>,
    pub config: Config,
}

#[derive(Debug, Deserialize)]
pub struct Config {
    /// Log level
    #[serde(default = "debug")]
    pub log: String,
    /// Enable turtle
    pub turtle: Option<bool>,
    /// Enable jsonld
    pub jsonld: Option<bool>,
    /// Enable sparql
    pub sparql: Option<bool>,
    /// Extra local configuration
    #[serde(flatten)]
    pub local: LocalConfig,
}

#[derive(Debug, Deserialize, Default)]
#[serde(default)]
pub struct LocalConfig {
    /// Extra ontologies to import
    pub ontologies: HashSet<String>,
    /// Extra shapes to import
    pub shapes: HashSet<String>,
    /// Features to disable
    pub disabled: HashSet<Disabled>,
    pub prefix_disabled: HashSet<String>,
}

impl LocalConfig {
    /// Combines this config with another config, giving precedence to the other config
    pub fn combine(&mut self, other: LocalConfig) {
        self.ontologies.extend(other.ontologies);
        self.shapes.extend(other.shapes);
        self.disabled.extend(other.disabled);
        self.prefix_disabled.extend(other.prefix_disabled);
    }
    #[cfg(target_arch = "wasm32")]
    pub async fn global(_: &Fs) -> Option<Self> {
        None
    }
    #[cfg(not(target_arch = "wasm32"))]
    pub async fn global(fs: &Fs) -> Option<Self> {
        let global_path = dirs::config_dir()
            .unwrap_or_else(|| PathBuf::from("."))
            .join("swls/config.json");
        let url = crate::lsp_types::Url::from_file_path(global_path).ok()?;

        tracing::debug!("Found global config url {}", url.as_str());
        let content = fs.0.read_file(&url).await?;
        tracing::debug!("Read global config content");

        match serde_json::from_str(&content) {
            Ok(x) => Some(x),
            Err(e) => {
                tracing::error!("Deserialize failed\n{:?}", e);
                None
            }
        }
    }

    pub async fn local(fs: &Fs, url: &Url) -> Option<Self> {
        let url = Url::parse(&format!("{}/.swls/config.json", url.as_str())).ok()?;
        tracing::debug!("Found local config url {}", url.as_str());
        let content = fs.0.read_file(&url).await?;
        tracing::debug!("Read local config content");
        match serde_json::from_str(&content) {
            Ok(x) => Some(x),
            Err(e) => {
                tracing::error!("Deserialize failed\n{:?}", e);
                None
            }
        }
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            log: "debug".to_string(),
            turtle: None,
            jsonld: None,
            sparql: None,
            local: LocalConfig::default(),
        }
    }
}

fn debug() -> String {
    String::from("debug")
}
