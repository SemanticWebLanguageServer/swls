use std::collections::HashMap;

use bevy_ecs::prelude::*;
use derive_more::{AsMut, AsRef, Deref, DerefMut};
use futures::channel::mpsc::{UnboundedReceiver, UnboundedSender};

use crate::systems::{DefinedClass, DefinedProperty};

/// [`Resource`] used to receive command queues. These command queues are handled with [`handle_tasks`](crate::prelude::systems::handle_tasks).
#[derive(Resource, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct CommandReceiver(pub UnboundedReceiver<bevy_ecs::world::CommandQueue>);

/// [`Resource`] used to send command queues, allowing for async operations.
#[derive(Resource, AsRef, Deref, AsMut, DerefMut, Debug, Clone)]
pub struct CommandSender(pub UnboundedSender<bevy_ecs::world::CommandQueue>);

/// [`Resource`] used to set and get all known ontology classes and properties.
#[derive(Resource, Default)]
pub struct Ontologies {
    pub classes: HashMap<oxigraph::model::NamedNode, DefinedClass>,
    pub properties: HashMap<String, DefinedProperty>,
}
