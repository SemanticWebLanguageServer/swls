use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

use crate::{prelude::*, util::triple::MyTerm};

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct DefinedClass {
    pub term: MyTerm<'static>,
    pub locations: HashSet<MyTerm<'static>>,
    pub titles: HashSet<String>,
    pub descriptions: HashSet<String>,
}
impl DefinedClass {
    pub fn full_title(&self) -> String {
        let mut title = String::new();

        for t in &self.titles {
            if !title.is_empty() {
                title += ", "
            }
            title += t;
        }
        title
    }

    pub fn full_docs(&self, hierarchy: &TypeHierarchy<'_>, pref: &Prefixes) -> String {
        let mut docs = String::new();
        for d in &self.descriptions {
            if !docs.is_empty() {
                docs += "\n\n"
            }
            docs += d;
        }

        if let Some(id) = hierarchy.get_id_ref(self.term.as_str()) {
            let superclasses = group_per_prefix(hierarchy.iter_superclass(id), pref);
            if !superclasses.is_empty() {
                if !docs.is_empty() {
                    docs += "\n\n"
                }
                docs += "**Superclass of:**";
                docs += superclasses.as_str();
            }
            let subclasses = group_per_prefix(hierarchy.iter_subclass(id), pref);
            if !subclasses.is_empty() {
                if !docs.is_empty() {
                    docs += "\n\n"
                }
                docs += "**Subclass of:**";
                docs += subclasses.as_str();
            }
        }

        for l in &self.locations {
            if !docs.is_empty() {
                docs += "\n\n"
            }
            docs += "*from: ";
            docs += l.as_str();
            docs += "*";
        }
        docs
    }
}

pub fn group_per_prefix<'a>(subclasses: impl Iterator<Item = Cow<'a, str>>, pref: &Prefixes) -> String {
    let mut subclass_str = String::new();
    let subclasses: Vec<_> = subclasses
        .skip(1)
        .flat_map(|sub| pref.shorten(&sub))
        .collect();

    let mut per_prefix: HashMap<&'_ str, String> = HashMap::new();

    for sub in &subclasses {
        if let Some((first, snd)) = sub.split_once(':') {
            let value = per_prefix.entry(first).or_default();
            if !value.is_empty() {
                value.push_str(", ");
            }
            value.push_str(snd);
        }
    }

    let mut ordered: Vec<_> = per_prefix.into_iter().collect();
    ordered.sort();

    for (k, v) in ordered {
        subclass_str += "\n- ";
        subclass_str += k;
        subclass_str += ":{";
        subclass_str += &v;
        subclass_str += "}";
    }
    subclass_str
}

pub type DefinedClasses = HashMap<oxigraph::model::NamedNode, DefinedClass>;

#[derive(PartialEq, Eq)]
pub struct DefinedProperty {
    pub term: MyTerm<'static>,
    pub locations: HashSet<MyTerm<'static>>,
    pub titles: HashSet<String>,
    pub descriptions: HashSet<String>,
    pub domains: HashSet<MyTerm<'static>>,
    pub ranges: HashSet<MyTerm<'static>>,
}

impl DefinedProperty {
    pub fn full_title(&self) -> String {
        let mut title = String::new();

        for t in &self.titles {
            if !title.is_empty() {
                title += ", "
            }
            title += t;
        }
        title
    }

    pub fn full_docs(&self, prefixes: &Prefixes) -> String {
        let mut docs = String::new();
        for d in &self.descriptions {
            if !docs.is_empty() {
                docs += "\n"
            }
            docs += d;
        }

        if !self.domains.is_empty() {
            if !docs.is_empty() {
                docs += "\n\n"
            }
            docs += "**Domain:** ";
            let mut first = true;
            for r in &self.domains {
                let range = prefixes.shorten(r.as_str());
                if !first {
                    docs += ", "
                }
                first = false;
                docs += range.as_ref().map(|x| x.as_str()).unwrap_or(r.as_str());
            }
        }

        if !self.ranges.is_empty() {
            if !docs.is_empty() {
                docs += "\n\n"
            }
            docs += "**Range:** ";
            let mut first = true;
            for r in &self.ranges {
                let range = prefixes.shorten(r.as_str());
                if !first {
                    docs += ", "
                }
                first = false;
                docs += range.as_ref().map(|x| x.as_str()).unwrap_or(r.as_str());
            }
        }

        for l in &self.locations {
            if !docs.is_empty() {
                docs += "\n\n"
            }
            docs += "*from: ";
            docs += l.as_str();
            docs += "*";
        }
        docs
    }
}

pub type DefinedProperties = HashMap<oxigraph::model::NamedNode, DefinedProperty>;
