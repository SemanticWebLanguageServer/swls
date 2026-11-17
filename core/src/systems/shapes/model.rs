//! A model of the SHACL shapes visible to a document, evaluated for one focus
//! node (the subject at the cursor).
//!
//! - [`ShapeGraph`]: the triples of the visible shape documents, indexed.
//! - [`Focus`]: what the document says about the subject (its classes, the
//!   predicates it uses, the triples it is the object of).
//! - Which node shapes apply follows SHACL targets: `sh:targetClass`
//!   (including superclasses known from the ontologies, and implicit class
//!   targets), `sh:targetSubjectsOf`, `sh:targetObjectsOf` and `sh:targetNode`.
//!   A shape includes the shapes and property shapes it lists in `sh:node` /
//!   `sh:and`. Branches of `sh:or` / `sh:xone` are alternatives, so none of them
//!   is assumed.
//!
//! Used by shape templates ([`super::template`]) and by hover and completion
//! documentation for predicates ([`super::describe`]).

use std::collections::{HashMap, HashSet};

use sophia_api::term::{Term as _, TermKind};

use crate::{
    prelude::*,
    util::lang_text::{normalize_ws, LangTexts},
};

pub(crate) const SH: &str = "http://www.w3.org/ns/shacl#";
pub(crate) const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
pub(crate) const RDF_FIRST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first";
pub(crate) const RDF_REST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";
pub(crate) const RDF_NIL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";
pub(crate) const RDFS_CLASS: &str = "http://www.w3.org/2000/01/rdf-schema#Class";
pub(crate) const OWL_CLASS: &str = "http://www.w3.org/2002/07/owl#Class";
pub(crate) const RDFS_LABEL: &str = "http://www.w3.org/2000/01/rdf-schema#label";
pub(crate) const RDFS_COMMENT: &str = "http://www.w3.org/2000/01/rdf-schema#comment";
pub(crate) const XSD: &str = "http://www.w3.org/2001/XMLSchema#";

/// How deep nested `sh:node` shapes are expanded inline.
pub(crate) const MAX_NESTING: usize = 2;

pub(crate) fn sh(local: &str) -> String {
    format!("{SH}{local}")
}

// ─── Shape graph ─────────────────────────────────────────────────────────────

/// A term in one of the shape documents. Blank nodes are only meaningful in
/// the document they come from; IRIs are looked up in every document.
#[derive(Clone, Copy, Debug)]
pub(crate) struct Node<'a> {
    pub(crate) doc: usize,
    pub(crate) term: &'a MyTerm<'static>,
}

impl Node<'_> {
    pub(crate) fn key(&self) -> (usize, String) {
        if self.term.kind() == TermKind::BlankNode {
            (self.doc, self.term.value.to_string())
        } else {
            (usize::MAX, self.term.value.to_string())
        }
    }
}

/// The triples of every shape document visible to a document, indexed by
/// subject.
pub struct ShapeGraph<'a> {
    /// Subject key → (predicate, object) pairs. See [`Node::key`].
    index: HashMap<(usize, String), Vec<(&'a str, Node<'a>)>>,
    node_shapes: Vec<Node<'a>>,
}

impl<'a> ShapeGraph<'a> {
    pub fn new(docs: Vec<&'a [MyQuad<'static>]>) -> Self {
        let mut index: HashMap<(usize, String), Vec<(&'a str, Node<'a>)>> = HashMap::new();
        for (doc, triples) in docs.iter().enumerate() {
            for t in triples.iter() {
                let subject = Node {
                    doc,
                    term: &t.subject,
                };
                let object = Node {
                    doc,
                    term: &t.object,
                };
                index
                    .entry(subject.key())
                    .or_default()
                    .push((t.predicate.value.as_ref(), object));
            }
        }
        let mut graph = Self {
            index,
            node_shapes: Vec::new(),
        };
        graph.node_shapes = graph.find_node_shapes(&docs);
        graph
    }

    pub(crate) fn objects(&self, node: Node<'a>, predicate: &str) -> Vec<Node<'a>> {
        self.index
            .get(&node.key())
            .map(|pairs| {
                pairs
                    .iter()
                    .filter(|(p, _)| *p == predicate)
                    .map(|(_, o)| *o)
                    .collect()
            })
            .unwrap_or_default()
    }

    pub(crate) fn object(&self, node: Node<'a>, predicate: &str) -> Option<Node<'a>> {
        self.objects(node, predicate).into_iter().next()
    }

    pub(crate) fn has(&self, node: Node<'a>, predicate: &str, object: &str) -> bool {
        self.objects(node, predicate)
            .iter()
            .any(|o| o.term.value == object)
    }

    /// Members of the RDF list starting at `head`.
    pub(crate) fn list(&self, head: Node<'a>) -> Vec<Node<'a>> {
        let mut out = Vec::new();
        let mut seen = HashSet::new();
        let mut current = Some(head);
        while let Some(node) = current {
            if node.term.value == RDF_NIL || !seen.insert(node.key()) {
                break;
            }
            out.extend(self.object(node, RDF_FIRST));
            current = self.object(node, RDF_REST);
        }
        out
    }

    /// Node shapes: subjects that declare shape-level things but no `sh:path`.
    pub(crate) fn find_node_shapes(&self, docs: &[&'a [MyQuad<'static>]]) -> Vec<Node<'a>> {
        let markers = [
            sh("property"),
            sh("targetClass"),
            sh("targetSubjectsOf"),
            sh("targetObjectsOf"),
            sh("targetNode"),
        ];
        let mut seen = HashSet::new();
        let mut out = Vec::new();
        for (doc, triples) in docs.iter().enumerate() {
            for t in triples.iter() {
                let is_marker = markers.iter().any(|m| t.predicate.value == m.as_str())
                    || (t.predicate.value == RDF_TYPE && t.object.value == sh("NodeShape"));
                if !is_marker {
                    continue;
                }
                let node = Node {
                    doc,
                    term: &t.subject,
                };
                if self.object(node, &sh("path")).is_some() {
                    continue;
                }
                if seen.insert(node.key()) {
                    out.push(node);
                }
            }
        }
        out
    }

    pub(crate) fn node_shapes(&self) -> &[Node<'a>] {
        &self.node_shapes
    }

    /// Active node shapes with a `target` (e.g. `sh:targetClass`) of `value`.
    pub(crate) fn targeting(&self, target: &str, value: &str) -> Vec<Node<'a>> {
        let target = sh(target);
        self.node_shapes
            .iter()
            .copied()
            .filter(|&s| self.has(s, &target, value) && !self.has(s, &sh("deactivated"), "true"))
            .collect()
    }

    /// Literal values of `predicate` on `node`, with their languages.
    pub(crate) fn texts(&self, node: Node<'a>, predicate: &str) -> LangTexts {
        self.objects(node, predicate)
            .into_iter()
            .map(|o| o.term)
            .collect()
    }

    /// The first of `predicates` that has a literal, in the preferred language.
    pub(crate) fn literal(&self, node: Node<'a>, predicates: &[&str]) -> Option<String> {
        predicates.iter().find_map(|p| {
            self.texts(node, p)
                .first()
                .map(|t| normalize_ws(t).into_owned())
        })
    }

    pub(crate) fn number(&self, node: Node<'a>, predicate: &str) -> Option<f64> {
        self.object(node, predicate)?.term.value.trim().parse().ok()
    }
}

// ─── Focus node: what is known about the subject in the document ─────────────

/// What the document says about the subject being completed.
pub struct Focus<'a> {
    /// The subject term (IRI or blank node value).
    pub subject: &'a MyTerm<'static>,
    /// The document's triples.
    pub triples: &'a [MyQuad<'static>],
    /// The subject's classes and their superclasses.
    pub classes: HashSet<String>,
}

impl<'a> Focus<'a> {
    pub fn new(
        subject: &'a MyTerm<'static>,
        triples: &'a [MyQuad<'static>],
        hierarchy: &TypeHierarchy,
    ) -> Self {
        let mut classes = HashSet::new();
        for t in triples.iter() {
            if t.subject == *subject && t.predicate.value == RDF_TYPE {
                classes.insert(t.object.value.to_string());
                if let Some(id) = hierarchy.get_id_ref(&t.object.value) {
                    classes.extend(hierarchy.iter_ancestors(id).map(|c| c.to_string()));
                }
            }
        }
        Self {
            subject,
            triples,
            classes,
        }
    }

    pub(crate) fn has_predicate(&self, predicate: &str) -> bool {
        self.triples
            .iter()
            .any(|t| t.subject == *self.subject && t.predicate.value == predicate)
    }

    pub(crate) fn is_object_of(&self, predicate: &str) -> bool {
        self.triples
            .iter()
            .any(|t| t.object == *self.subject && t.predicate.value == predicate)
    }
}

// ─── Property model ──────────────────────────────────────────────────────────

/// What a template puts in place of a property's value.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// `sh:hasValue`.
    Fixed(Term),
    /// `sh:in`.
    OneOf(Vec<Term>),
    /// `sh:node`: a nested resource described by these properties.
    Nested(Vec<Property>),
    /// An IRI (`sh:nodeKind sh:IRI`, or `sh:class`).
    Iri,
    /// A literal, with its datatype when not `xsd:string`.
    Literal(Option<String>),
    /// No constraint on the value.
    Any,
}

/// A term as it appears in a template.
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Iri(String),
    Literal(String),
}

/// A property a shape declares for its focus nodes.
#[derive(Debug, Clone, PartialEq)]
pub struct Property {
    /// The predicate, or the first of `alternatives`.
    pub path: String,
    /// For `sh:alternativePath`: every predicate that satisfies the property
    /// (any one of them). Empty for a plain predicate path.
    pub alternatives: Vec<String>,
    pub required: bool,
    pub name: String,
    pub description: Option<String>,
    pub value: Value,
    pub(crate) order: Option<f64>,
}

impl Property {
    pub(crate) fn is_group(&self) -> bool {
        self.alternatives.len() > 1
    }

    /// Identity used to merge declarations of the same property.
    pub(crate) fn key(&self) -> String {
        if self.is_group() {
            let mut alts = self.alternatives.clone();
            alts.sort();
            alts.join("|")
        } else {
            self.path.clone()
        }
    }

    pub(crate) fn paths(&self) -> impl Iterator<Item = &str> {
        std::iter::once(self.path.as_str()).chain(self.alternatives.iter().map(String::as_str))
    }
}

/// The node shapes that apply to `focus`.
pub(crate) fn applicable_shapes<'a>(graph: &ShapeGraph<'a>, focus: &Focus<'_>) -> Vec<Node<'a>> {
    graph
        .node_shapes()
        .iter()
        .copied()
        .filter(|&shape| !graph.has(shape, &sh("deactivated"), "true"))
        .filter(|&shape| {
            let applies = |p: &str, f: &dyn Fn(&str) -> bool| {
                graph
                    .objects(shape, &sh(p))
                    .iter()
                    .any(|o| f(&o.term.value))
            };
            applies("targetClass", &|c| focus.classes.contains(c))
                || applies("targetSubjectsOf", &|p| focus.has_predicate(p))
                || applies("targetObjectsOf", &|p| focus.is_object_of(p))
                || applies("targetNode", &|n| n == focus.subject.value.as_ref())
                || (focus.classes.contains(shape.term.value.as_ref())
                    && (graph.has(shape, RDF_TYPE, RDFS_CLASS)
                        || graph.has(shape, RDF_TYPE, OWL_CLASS)))
        })
        .collect()
}

/// Properties declared by `shape` and the shapes it includes (`sh:node`,
/// `sh:and`), merged by path.
pub(crate) fn shape_properties<'a>(
    graph: &ShapeGraph<'a>,
    shape: Node<'a>,
    depth: usize,
    visiting: &mut HashSet<(usize, String)>,
) -> Vec<Property> {
    if !visiting.insert(shape.key()) {
        return Vec::new();
    }

    let mut out: Vec<Property> = Vec::new();
    for prop in graph.objects(shape, &sh("property")) {
        if let Some(p) = property(graph, prop, depth, visiting) {
            merge(&mut out, p);
        }
    }

    let mut included = graph.objects(shape, &sh("node"));
    for list in graph.objects(shape, &sh("and")) {
        included.extend(graph.list(list));
    }
    for inc in included {
        // `sh:and` members are often property shapes themselves.
        if graph.object(inc, &sh("path")).is_some() {
            if let Some(p) = property(graph, inc, depth, visiting) {
                merge(&mut out, p);
            }
            continue;
        }
        for p in shape_properties(graph, inc, depth, visiting) {
            merge(&mut out, p);
        }
    }

    visiting.remove(&shape.key());
    consolidate(&mut out);
    out
}

/// Fold plain declarations into the alternative groups that cover them.
///
/// RML for example declares `[ sh:path [ sh:alternativePath (rml:subjectMap
/// rml:subject) ] ; sh:minCount 1 ]` next to optional shapes for
/// `rml:subjectMap` and `rml:subject` that only constrain the value. The
/// group takes the value constraints of its first alternative, and optional
/// declarations it covers are dropped so a template names them once.
pub(crate) fn consolidate(props: &mut Vec<Property>) {
    let groups: Vec<Vec<String>> = props
        .iter()
        .filter(|p| p.is_group())
        .map(|p| p.alternatives.clone())
        .collect();
    for alts in &groups {
        let first_value = props
            .iter()
            .find(|p| !p.is_group() && p.path == alts[0])
            .map(|p| p.value.clone());
        if let (Some(value), Some(group)) = (
            first_value,
            props.iter_mut().find(|p| &p.alternatives == alts),
        ) {
            if group.value == Value::Any {
                group.value = value;
            }
        }
        props.retain(|p| p.is_group() || p.required || !alts.contains(&p.path));
    }
}

pub(crate) fn merge(into: &mut Vec<Property>, p: Property) {
    match into.iter_mut().find(|x| x.key() == p.key()) {
        Some(existing) => {
            existing.required |= p.required;
            if existing.value == Value::Any {
                existing.value = p.value;
            }
            if existing.description.is_none() {
                existing.description = p.description;
            }
            if existing.order.is_none() {
                existing.order = p.order;
            }
        }
        None => into.push(p),
    }
}

pub(crate) fn local_name(iri: &str) -> &str {
    iri.rsplit(['#', '/', ':'])
        .find(|s| !s.is_empty())
        .unwrap_or(iri)
}

pub(crate) fn to_term(node: Node<'_>) -> Term {
    match node.term.kind() {
        TermKind::Literal => Term::Literal(node.term.value.to_string()),
        _ => Term::Iri(node.term.value.to_string()),
    }
}

pub(crate) fn property<'a>(
    graph: &ShapeGraph<'a>,
    prop: Node<'a>,
    depth: usize,
    visiting: &mut HashSet<(usize, String)>,
) -> Option<Property> {
    if graph.has(prop, &sh("deactivated"), "true") {
        return None;
    }
    let paths = property_paths(graph, prop)?;
    let path = paths[0].clone();
    let alternatives = if paths.len() > 1 { paths } else { Vec::new() };

    let required = graph.number(prop, &sh("minCount")).unwrap_or(0.0) >= 1.0;
    let name = graph
        .literal(prop, &[&sh("name"), RDFS_LABEL])
        .map(|l| {
            // Some shapes label a property with its CURIE (`rml:iterator`).
            if l.contains(':') && !l.contains(' ') {
                local_name(&l).to_string()
            } else {
                l
            }
        })
        .unwrap_or_else(|| local_name(&path).to_string());
    let description = graph.literal(prop, &[&sh("description"), RDFS_COMMENT]);

    let value = if let Some(v) = graph.object(prop, &sh("hasValue")) {
        Value::Fixed(to_term(v))
    } else if let Some(list) = graph.object(prop, &sh("in")) {
        Value::OneOf(graph.list(list).into_iter().map(to_term).collect())
    } else if let Some(nested) = (depth < MAX_NESTING)
        .then(|| nested_properties(graph, prop, &path, depth, visiting))
        .filter(|ps| !ps.is_empty())
    {
        Value::Nested(nested)
    } else if let Some(dt) = graph.object(prop, &sh("datatype")) {
        let dt = dt.term.value.to_string();
        Value::Literal((dt != format!("{XSD}string")).then_some(dt))
    } else {
        match graph
            .object(prop, &sh("nodeKind"))
            .map(|k| k.term.value.to_string())
        {
            Some(k) if k == sh("IRI") => Value::Iri,
            Some(k) if k == sh("Literal") => Value::Literal(None),
            _ if graph.object(prop, &sh("class")).is_some() => Value::Iri,
            _ => Value::Any,
        }
    };

    Some(Property {
        path,
        alternatives,
        required,
        name,
        description,
        value,
        order: graph.number(prop, &sh("order")),
    })
}

/// Required properties of the value of `prop` (reached through `path`), from
/// the node shapes that describe it:
///
/// - its `sh:node`, or else the first alternative of `sh:or` / `sh:xone` that
///   names one,
/// - shapes with `sh:targetObjectsOf path`, which is how e.g. RML describes
///   the value of `rml:subjectMap`,
/// - shapes with `sh:targetClass` of its `sh:class`.
pub(crate) fn nested_properties<'a>(
    graph: &ShapeGraph<'a>,
    prop: Node<'a>,
    path: &str,
    depth: usize,
    visiting: &mut HashSet<(usize, String)>,
) -> Vec<Property> {
    let mut shapes: Vec<Node<'a>> = graph.objects(prop, &sh("node"));
    if shapes.is_empty() {
        shapes.extend(
            ["or", "xone"]
                .iter()
                .flat_map(|k| graph.objects(prop, &sh(k)))
                .flat_map(|list| graph.list(list))
                .find_map(|branch| graph.object(branch, &sh("node"))),
        );
    }
    shapes.extend(graph.targeting("targetObjectsOf", path));
    for class in graph.objects(prop, &sh("class")) {
        shapes.extend(graph.targeting("targetClass", &class.term.value));
    }

    let mut out = Vec::new();
    for shape in shapes {
        for p in shape_properties(graph, shape, depth + 1, visiting) {
            merge(&mut out, p);
        }
    }
    consolidate(&mut out);
    out.retain(|p| p.required);
    out
}

/// Properties the shapes applying to `focus` declare and the subject does not
/// have yet, in template order (`sh:order`, then required first).
pub fn missing_properties(graph: &ShapeGraph<'_>, focus: &Focus<'_>) -> Vec<Property> {
    let mut props: Vec<Property> = Vec::new();
    for shape in applicable_shapes(graph, focus) {
        for p in shape_properties(graph, shape, 0, &mut HashSet::new()) {
            merge(&mut props, p);
        }
    }
    consolidate(&mut props);
    props.retain(|p| !p.paths().any(|path| focus.has_predicate(path)));
    // A stable sort keeps declaration order among equals.
    props.sort_by(|a, b| {
        let order = |p: &Property| p.order.unwrap_or(f64::MAX);
        order(a)
            .partial_cmp(&order(b))
            .unwrap_or(std::cmp::Ordering::Equal)
            .then(Ord::cmp(&b.required, &a.required))
    });
    props
}

/// The predicates a property shape constrains: its `sh:path`, or every member
/// of an `sh:alternativePath` (any one of them). `None` for other paths
/// (inverse, repeated, sequences), which cannot be written as a predicate.
pub(crate) fn property_paths<'a>(graph: &ShapeGraph<'a>, prop: Node<'a>) -> Option<Vec<String>> {
    let path_node = graph.object(prop, &sh("path"))?;
    if path_node.term.kind() == TermKind::Iri {
        return Some(vec![path_node.term.value.to_string()]);
    }
    let list = graph.object(path_node, &sh("alternativePath"))?;
    let members = graph.list(list);
    if members.is_empty() || members.iter().any(|m| m.term.kind() != TermKind::Iri) {
        return None;
    }
    Some(members.iter().map(|m| m.term.value.to_string()).collect())
}

/// A property shape that applies to a focus node.
#[derive(Clone, Copy, Debug)]
pub(crate) struct AppliedPropertyShape<'a> {
    /// The property shape itself.
    pub(crate) node: Node<'a>,
    /// The node shape that applies to the focus node and (directly or through
    /// `sh:node` / `sh:and`) declares this property shape.
    pub(crate) owner: Node<'a>,
}

/// Every active property shape that applies to `focus`, in declaration order.
pub(crate) fn applied_property_shapes<'a>(
    graph: &ShapeGraph<'a>,
    focus: &Focus<'_>,
) -> Vec<AppliedPropertyShape<'a>> {
    fn walk<'a>(
        graph: &ShapeGraph<'a>,
        shape: Node<'a>,
        owner: Node<'a>,
        seen: &mut HashSet<(usize, String)>,
        out: &mut Vec<AppliedPropertyShape<'a>>,
    ) {
        if !seen.insert(shape.key()) {
            return;
        }
        let push = |node: Node<'a>, out: &mut Vec<AppliedPropertyShape<'a>>| {
            if !graph.has(node, &sh("deactivated"), "true") {
                out.push(AppliedPropertyShape { node, owner });
            }
        };
        for prop in graph.objects(shape, &sh("property")) {
            push(prop, out);
        }
        let mut included = graph.objects(shape, &sh("node"));
        for list in graph.objects(shape, &sh("and")) {
            included.extend(graph.list(list));
        }
        for inc in included {
            if graph.object(inc, &sh("path")).is_some() {
                push(inc, out);
            } else {
                walk(graph, inc, owner, seen, out);
            }
        }
    }

    let mut seen = HashSet::new();
    let mut out = Vec::new();
    for shape in applicable_shapes(graph, focus) {
        walk(graph, shape, shape, &mut seen, &mut out);
    }
    let mut unique = HashSet::new();
    out.retain(|p| unique.insert(p.node.key()));
    out
}
