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
    pub fn clean_up(&mut self, hierarchy: &TypeHierarchy) {
        for sets in self.0.values_mut() {
            let new_set = sets
                .iter()
                // Keep only the most specific types: drop a type when one of
                // its subclasses is also present.
                .filter(|&trying| {
                    for maybe_presnt in hierarchy.iter_descendant_ids(*trying) {
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

        tracing::debug!(
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
/// use swls_core::components::TypeHierarchy;
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
pub struct TypeHierarchy {
    numbers: HashMap<Cow<'static, str>, TypeId>,
    nodes: Vec<Cow<'static, str>>,
    /// Direct superclasses of each class.
    parents: Vec<HashSet<TypeId>>,
    /// Direct subclasses of each class.
    children: Vec<HashSet<TypeId>>,
}

impl TypeHierarchy {
    /// Number of classes plus subclass edges. Grows whenever the hierarchy
    /// learns something, so producers can tell whether it changed.
    pub fn size(&self) -> usize {
        self.nodes.len() + self.parents.iter().map(|s| s.len()).sum::<usize>()
    }

    pub fn get_id(&mut self, class: &str) -> TypeId {
        if let Some(id) = self.numbers.get(class) {
            *id
        } else {
            let new_id = TypeId(self.nodes.len());
            let class_cow: Cow<'static, str> = Cow::Owned(class.to_string());
            self.nodes.push(class_cow.clone());
            self.numbers.insert(class_cow, new_id);
            self.parents.push(HashSet::new());
            self.children.push(HashSet::new());
            new_id
        }
    }

    pub fn get_id_ref(&self, class: &str) -> Option<TypeId> {
        self.numbers.get(class).copied()
    }

    /// Record that `class` is a subclass of `of`.
    pub fn set_subclass_of(&mut self, class: TypeId, of: TypeId) {
        self.parents[class.0].insert(of);
        self.children[of.0].insert(class);
    }

    pub fn type_name(&self, id: TypeId) -> Cow<'static, str> {
        self.nodes[id.0].clone()
    }

    /// `id` and all its superclasses (transitively), by name.
    pub fn iter_ancestors<'b>(
        &'b self,
        id: TypeId,
    ) -> impl Iterator<Item = Cow<'static, str>> + 'b {
        self.iter_ancestor_ids(id)
            .map(|id| self.nodes[id.0].clone())
    }

    /// `id` and all its superclasses (transitively).
    pub fn iter_ancestor_ids<'b>(&'b self, id: TypeId) -> impl Iterator<Item = TypeId> + 'b {
        self.walk(id, &self.parents)
    }

    /// `id` and all its subclasses (transitively), by name.
    pub fn iter_descendants<'b>(
        &'b self,
        id: TypeId,
    ) -> impl Iterator<Item = Cow<'static, str>> + 'b {
        self.iter_descendant_ids(id)
            .map(|id| self.nodes[id.0].clone())
    }

    /// `id` and all its subclasses (transitively).
    pub fn iter_descendant_ids<'b>(&'b self, id: TypeId) -> impl Iterator<Item = TypeId> + 'b {
        self.walk(id, &self.children)
    }

    /// Breadth-first walk from `id` along `edges`, each class once.
    fn walk<'b>(
        &'b self,
        id: TypeId,
        edges: &'b [HashSet<TypeId>],
    ) -> impl Iterator<Item = TypeId> + 'b {
        let mut queue = std::collections::VecDeque::from([id]);
        let mut done = HashSet::new();
        std::iter::from_fn(move || {
            while let Some(id) = queue.pop_front() {
                if !done.insert(id) {
                    continue;
                }
                queue.extend(edges[id.0].iter().copied());
                return Some(id);
            }
            None
        })
    }

    #[deprecated(note = "walks superclasses; use `iter_ancestors`")]
    pub fn iter_subclass<'b>(&'b self, id: TypeId) -> impl Iterator<Item = Cow<'static, str>> + 'b {
        self.iter_ancestors(id)
    }

    #[deprecated(note = "walks superclasses; use `iter_ancestor_ids`")]
    pub fn iter_subclass_ids<'b>(&'b self, id: TypeId) -> impl Iterator<Item = TypeId> + 'b {
        self.iter_ancestor_ids(id)
    }

    #[deprecated(note = "walks subclasses; use `iter_descendants`")]
    pub fn iter_superclass<'b>(
        &'b self,
        id: TypeId,
    ) -> impl Iterator<Item = Cow<'static, str>> + 'b {
        self.iter_descendants(id)
    }

    #[deprecated(note = "walks subclasses; use `iter_descendant_ids`")]
    pub fn iter_superclass_ids<'b>(&'b self, id: TypeId) -> impl Iterator<Item = TypeId> + 'b {
        self.iter_descendant_ids(id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ancestors_and_descendants_follow_the_subclass_relation() {
        let mut h = TypeHierarchy::default();
        let animal = h.get_id("Animal");
        let dog = h.get_id("Dog");
        let puppy = h.get_id("Puppy");
        h.set_subclass_of(dog, animal);
        h.set_subclass_of(puppy, dog);

        let names = |it: &mut dyn Iterator<Item = Cow<'static, str>>| {
            let mut v: Vec<String> = it.map(|c| c.to_string()).collect();
            v.sort();
            v
        };
        assert_eq!(
            names(&mut h.iter_ancestors(puppy)),
            ["Animal", "Dog", "Puppy"]
        );
        assert_eq!(
            names(&mut h.iter_descendants(animal)),
            ["Animal", "Dog", "Puppy"]
        );
        assert_eq!(names(&mut h.iter_ancestors(animal)), ["Animal"]);
    }
}
