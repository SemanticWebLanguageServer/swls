use std::{
    collections::{HashMap, HashSet},
    ops::Index,
};

use bevy_ecs::component::Component;
use swls_core::{prelude::Token, util::Spanned};
use similar::ChangeTag;

pub struct TokenIdx<'a> {
    pub tokens: &'a Vec<Spanned<Token>>,
}
impl<'a> Index<usize> for TokenIdx<'a> {
    type Output = Token;

    fn index(&self, index: usize) -> &Self::Output {
        &self.tokens[index].value()
    }
}
#[derive(Clone, Copy)]
pub struct Ctx<'a> {
    context: &'a Context,
}

impl<'a> Ctx<'a> {
    pub fn find_was(&self, idx: usize) -> Option<ContextKind> {
        if let Some(idx) = self.context.current_to_prev.get(&idx) {
            if self.context.subjects.contains(idx) {
                return Some(ContextKind::Subject);
            }

            if self.context.predicates.contains(idx) {
                return Some(ContextKind::Predicate);
            }

            if self.context.objects.contains(idx) {
                return Some(ContextKind::Object);
            }
        }
        None
    }
    pub fn was(&self, idx: usize, kind: ContextKind) -> bool {
        match kind {
            ContextKind::Subject => self.was_subject(idx),
            ContextKind::Predicate => self.was_predicate(idx),
            ContextKind::Object => self.was_object(idx),
        }
    }

    pub fn was_subject(&self, idx: usize) -> bool {
        self.context
            .current_to_prev
            .get(&idx)
            .map(|old| self.context.subjects.contains(old))
            .unwrap_or_default()
    }

    pub fn was_object(&self, idx: usize) -> bool {
        self.context
            .current_to_prev
            .get(&idx)
            .map(|old| self.context.objects.contains(old))
            .unwrap_or_default()
    }

    pub fn was_predicate(&self, idx: usize) -> bool {
        self.context
            .current_to_prev
            .get(&idx)
            .map(|old| self.context.predicates.contains(old))
            .unwrap_or_default()
    }
}

#[derive(Copy, Debug, Clone)]
pub enum ContextKind {
    Subject,
    Predicate,
    Object,
}
impl std::fmt::Display for ContextKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ContextKind::Subject => write!(f, "subject"),
            ContextKind::Predicate => write!(f, "predicate"),
            ContextKind::Object => write!(f, "object"),
        }
    }
}

#[derive(Debug, Default, Component)]
pub struct Context {
    subjects: HashSet<usize>,
    predicates: HashSet<usize>,
    objects: HashSet<usize>,

    // This might also be an array, but we keep things simple
    current_to_prev: HashMap<usize, usize>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn add_subject(&mut self, idx: usize) {
        self.subjects.insert(idx);
    }

    pub fn add_predicate(&mut self, idx: usize) {
        self.predicates.insert(idx);
    }

    pub fn add_object(&mut self, idx: usize) {
        self.objects.insert(idx);
    }

    pub fn add(&mut self, idx: usize, kind: ContextKind) {
        match kind {
            ContextKind::Subject => self.add_subject(idx),
            ContextKind::Predicate => self.add_predicate(idx),
            ContextKind::Object => self.add_object(idx),
        }
    }

    pub fn clear(&mut self) {
        self.subjects.clear();
        self.predicates.clear();
        self.objects.clear();
        self.current_to_prev.clear();
    }

    pub fn setup_current_to_prev<Arr>(
        &mut self,
        current: Arr,
        current_length: usize,
        prev: Arr,
        prev_length: usize,
    ) where
        Arr: Index<usize>,
        Arr::Output: PartialEq<Arr::Output> + Ord + std::hash::Hash + Eq + Sized + Clone,
    {
        let diffs = similar::capture_diff(
            similar::Algorithm::Myers,
            &prev,
            0..prev_length,
            &current,
            0..current_length,
        );
        for changes in diffs.iter() {
            for change in changes.iter_changes(&prev, &current) {
                if change.tag() == ChangeTag::Equal {
                    if let (Some(new), Some(old)) = (change.new_index(), change.old_index()) {
                        self.current_to_prev.insert(new, old);
                    }
                }
            }
        }
    }

    pub fn ctx<'a>(&'a self) -> Ctx<'a> {
        Ctx { context: self }
    }
}
