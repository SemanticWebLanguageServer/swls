use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

use bevy_ecs::prelude::*;
use derive_more::{AsMut, AsRef, Deref, DerefMut};

use crate::systems::TypeId;

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
