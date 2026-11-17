//! Completion templates generated from SHACL shapes.
//!
//! At a predicate position, the node shapes that apply to the subject are
//! collected and turned into two snippet completions:
//!
//! - **required properties**: every property with `sh:minCount >= 1`,
//! - **all properties**: every property the shapes declare.
//!
//! Properties the subject already has are left out, so a template can also be
//! used to complete a partially written resource.
//!
//! Which shapes apply follows SHACL targets, evaluated on the document:
//! `sh:targetClass` (including superclasses known from the ontologies, and
//! implicit class targets), `sh:targetSubjectsOf`, `sh:targetObjectsOf` and
//! `sh:targetNode`. The properties of a shape include those of the shapes it
//! refers to with `sh:node` and `sh:and`. Branches of `sh:or` / `sh:xone` are
//! alternatives, so none of them is assumed.
//!
//! Values are placeholders shaped by the constraints of the property:
//! `sh:hasValue` is filled in, `sh:in` becomes a choice, a nested `sh:node`
//! becomes `[ … ]` with that shape's required properties, and `sh:nodeKind` /
//! `sh:datatype` decide between `<…>`, `"…"` and plain placeholders.

use std::collections::{HashMap, HashSet};

use bevy_ecs::prelude::*;
use sophia_api::term::{Term as _, TermKind};

use crate::{
    feature::completion::{CompletionRequest, SimpleCompletion},
    lsp_types::{CompletionItemKind, Position, Range, TextEdit},
    prelude::*,
    systems::shapes::ShaclShapes,
};

const SH: &str = "http://www.w3.org/ns/shacl#";
const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
const RDF_FIRST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first";
const RDF_REST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";
const RDF_NIL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";
const RDFS_CLASS: &str = "http://www.w3.org/2000/01/rdf-schema#Class";
const OWL_CLASS: &str = "http://www.w3.org/2002/07/owl#Class";
const RDFS_LABEL: &str = "http://www.w3.org/2000/01/rdf-schema#label";
const RDFS_COMMENT: &str = "http://www.w3.org/2000/01/rdf-schema#comment";
const XSD: &str = "http://www.w3.org/2001/XMLSchema#";

/// How deep nested `sh:node` shapes are expanded inline.
const MAX_NESTING: usize = 2;

fn sh(local: &str) -> String {
    format!("{SH}{local}")
}

// ─── Shape graph ─────────────────────────────────────────────────────────────

/// A term in one of the shape documents. Blank nodes are only meaningful in
/// the document they come from; IRIs are looked up in every document.
#[derive(Clone, Copy, Debug)]
struct Node<'a> {
    doc: usize,
    term: &'a MyTerm<'static>,
}

impl Node<'_> {
    fn key(&self) -> (usize, String) {
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

    fn objects(&self, node: Node<'a>, predicate: &str) -> Vec<Node<'a>> {
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

    fn object(&self, node: Node<'a>, predicate: &str) -> Option<Node<'a>> {
        self.objects(node, predicate).into_iter().next()
    }

    fn has(&self, node: Node<'a>, predicate: &str, object: &str) -> bool {
        self.objects(node, predicate)
            .iter()
            .any(|o| o.term.value == object)
    }

    /// Members of the RDF list starting at `head`.
    fn list(&self, head: Node<'a>) -> Vec<Node<'a>> {
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
    fn find_node_shapes(&self, docs: &[&'a [MyQuad<'static>]]) -> Vec<Node<'a>> {
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

    fn node_shapes(&self) -> &[Node<'a>] {
        &self.node_shapes
    }

    /// Active node shapes with a `target` (e.g. `sh:targetClass`) of `value`.
    fn targeting(&self, target: &str, value: &str) -> Vec<Node<'a>> {
        let target = sh(target);
        self.node_shapes
            .iter()
            .copied()
            .filter(|&s| self.has(s, &target, value) && !self.has(s, &sh("deactivated"), "true"))
            .collect()
    }

    fn literal(&self, node: Node<'a>, predicates: &[&str]) -> Option<String> {
        predicates.iter().find_map(|p| {
            self.object(node, p)
                .filter(|o| o.term.kind() == TermKind::Literal)
                .map(|o| normalize_ws(&o.term.value))
        })
    }

    fn number(&self, node: Node<'a>, predicate: &str) -> Option<f64> {
        self.object(node, predicate)?.term.value.trim().parse().ok()
    }
}

fn normalize_ws(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
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
                    // `iter_subclass` walks from a class to the classes it is a
                    // subclass of, i.e. its superclasses.
                    classes.extend(hierarchy.iter_subclass(id).map(|c| c.to_string()));
                }
            }
        }
        Self {
            subject,
            triples,
            classes,
        }
    }

    fn has_predicate(&self, predicate: &str) -> bool {
        self.triples
            .iter()
            .any(|t| t.subject == *self.subject && t.predicate.value == predicate)
    }

    fn is_object_of(&self, predicate: &str) -> bool {
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
    order: Option<f64>,
}

impl Property {
    fn is_group(&self) -> bool {
        self.alternatives.len() > 1
    }

    /// Identity used to merge declarations of the same property.
    fn key(&self) -> String {
        if self.is_group() {
            let mut alts = self.alternatives.clone();
            alts.sort();
            alts.join("|")
        } else {
            self.path.clone()
        }
    }

    fn paths(&self) -> impl Iterator<Item = &str> {
        std::iter::once(self.path.as_str()).chain(self.alternatives.iter().map(String::as_str))
    }
}

/// The node shapes that apply to `focus`.
fn applicable_shapes<'a>(graph: &ShapeGraph<'a>, focus: &Focus<'_>) -> Vec<Node<'a>> {
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
fn shape_properties<'a>(
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
fn consolidate(props: &mut Vec<Property>) {
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

fn merge(into: &mut Vec<Property>, p: Property) {
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

fn local_name(iri: &str) -> &str {
    iri.rsplit(|c| c == '#' || c == '/' || c == ':')
        .find(|s| !s.is_empty())
        .unwrap_or(iri)
}

fn to_term(node: Node<'_>) -> Term {
    match node.term.kind() {
        TermKind::Literal => Term::Literal(node.term.value.to_string()),
        _ => Term::Iri(node.term.value.to_string()),
    }
}

fn property<'a>(
    graph: &ShapeGraph<'a>,
    prop: Node<'a>,
    depth: usize,
    visiting: &mut HashSet<(usize, String)>,
) -> Option<Property> {
    if graph.has(prop, &sh("deactivated"), "true") {
        return None;
    }
    // A template can write a predicate, or a choice between predicates
    // (`sh:alternativePath`). Inverse and repeated paths are left out.
    let path_node = graph.object(prop, &sh("path"))?;
    let alternatives: Vec<String> = if path_node.term.kind() == TermKind::Iri {
        Vec::new()
    } else {
        let list = graph.object(path_node, &sh("alternativePath"))?;
        let members = graph.list(list);
        if members.is_empty() || members.iter().any(|m| m.term.kind() != TermKind::Iri) {
            return None;
        }
        members.iter().map(|m| m.term.value.to_string()).collect()
    };
    let path = match alternatives.first() {
        Some(first) => first.clone(),
        None => path_node.term.value.to_string(),
    };

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
fn nested_properties<'a>(
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

// ─── Rendering ───────────────────────────────────────────────────────────────

/// Escape text for use inside an LSP snippet.
fn escape(text: &str) -> String {
    text.replace('\\', "\\\\")
        .replace('$', "\\$")
        .replace('}', "\\}")
}

/// Escape a choice of an LSP snippet `${1|a,b|}`.
fn escape_choice(text: &str) -> String {
    escape(text).replace(',', "\\,").replace('|', "\\|")
}

struct Renderer<'a> {
    prefixes: &'a Prefixes,
    tabstop: usize,
}

impl Renderer<'_> {
    fn iri(&self, iri: &str) -> String {
        self.prefixes
            .shorten(iri)
            .unwrap_or_else(|| format!("<{}>", iri))
    }

    fn term(&self, term: &Term) -> String {
        match term {
            Term::Iri(iri) => self.iri(iri),
            Term::Literal(l) => format!("\"{}\"", l.replace('"', "\\\"")),
        }
    }

    fn next(&mut self) -> usize {
        self.tabstop += 1;
        self.tabstop
    }

    /// The predicate of `p`: a choice for an alternative path.
    fn predicate(&mut self, p: &Property) -> String {
        if p.is_group() {
            let n = self.next();
            let choices: Vec<_> = p
                .alternatives
                .iter()
                .map(|a| escape_choice(&self.iri(a)))
                .collect();
            format!("${{{}|{}|}}", n, choices.join(","))
        } else {
            escape(&self.iri(&p.path))
        }
    }

    /// `predicate value` for each property, values laid out for `indent`.
    fn lines(&mut self, props: &[Property], indent: &str) -> Vec<String> {
        props
            .iter()
            .map(|p| {
                let predicate = self.predicate(p);
                format!("{} {}", predicate, self.value(p, indent))
            })
            .collect()
    }

    fn placeholder(&mut self, name: &str) -> String {
        let n = self.next();
        format!("${{{}:{}}}", n, escape_choice(name))
    }

    /// The value of `p`, for a property written on a line indented by `indent`.
    fn value(&mut self, p: &Property, indent: &str) -> String {
        match &p.value {
            Value::Fixed(t) => escape(&self.term(t)),
            Value::OneOf(options) if !options.is_empty() => {
                let n = self.next();
                let choices: Vec<_> = options
                    .iter()
                    .map(|t| escape_choice(&self.term(t)))
                    .collect();
                format!("${{{}|{}|}}", n, choices.join(","))
            }
            Value::Nested(props) if props.len() == 1 => {
                format!("[ {} ]", self.lines(props, indent).join(""))
            }
            Value::Nested(props) => {
                // One property per line, indented one level deeper.
                let inner = format!("{}    ", indent);
                let lines = self.lines(props, &inner);
                format!(
                    "[\n{inner}{}\n{indent}]",
                    lines.join(&format!(" ;\n{inner}"))
                )
            }
            Value::Iri => format!("<{}>", self.placeholder(&p.name)),
            Value::Literal(None) => format!("\"{}\"", self.placeholder(&p.name)),
            Value::Literal(Some(dt)) => match dt.strip_prefix(XSD) {
                Some("boolean") => {
                    let n = self.next();
                    format!("${{{}|true,false|}}", n)
                }
                Some("integer" | "int" | "long" | "decimal" | "double" | "float") => {
                    self.placeholder(&p.name)
                }
                _ => format!(
                    "\"{}\"^^{}",
                    self.placeholder(&p.name),
                    escape(&self.iri(dt))
                ),
            },
            _ => self.placeholder(&p.name),
        }
    }
}

/// Snippet text for `props`: one `predicate value` per line, separated by `;`,
/// with continuation lines indented by `indent`.
pub fn render(props: &[Property], prefixes: &Prefixes, indent: &str) -> String {
    let mut r = Renderer {
        prefixes,
        tabstop: 0,
    };
    let lines = r.lines(props, indent);
    let mut out = lines.join(&format!(" ;\n{}", indent));
    out.push_str("$0");
    out
}

fn documentation(props: &[Property], prefixes: &Prefixes, preview: &str) -> String {
    let r = Renderer {
        prefixes,
        tabstop: 0,
    };
    let mut md = String::new();
    for p in props {
        let names: Vec<_> = if p.is_group() {
            p.alternatives
                .iter()
                .map(|a| format!("`{}`", r.iri(a)))
                .collect()
        } else {
            vec![format!("`{}`", r.iri(&p.path))]
        };
        md.push_str(&format!(
            "- {}{}",
            names.join(" or "),
            if p.required { " (required)" } else { "" }
        ));
        if let Some(d) = &p.description {
            md.push_str(&format!(": {}", d));
        }
        md.push('\n');
    }
    md.push_str(&format!("\n```turtle\n{}\n```", preview));
    md
}

/// Strip snippet syntax to show a template as plain text.
fn plain(snippet: &str) -> String {
    let mut out = String::new();
    let mut chars = snippet.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '\\' => out.extend(chars.next()),
            '$' if chars.peek() == Some(&'{') => {
                chars.next();
                let mut body = String::new();
                let mut depth = 1;
                while let Some(c) = chars.next() {
                    match c {
                        '\\' => body.extend(chars.next()),
                        '{' => {
                            depth += 1;
                            body.push(c)
                        }
                        '}' => {
                            depth -= 1;
                            if depth == 0 {
                                break;
                            }
                            body.push(c)
                        }
                        _ => body.push(c),
                    }
                }
                let body = body.trim_start_matches(|c: char| c.is_ascii_digit());
                if let Some(name) = body.strip_prefix(':') {
                    out.push_str(name);
                } else if let Some(choices) = body.strip_prefix('|') {
                    out.push_str(
                        choices
                            .trim_end_matches('|')
                            .split(',')
                            .next()
                            .unwrap_or(""),
                    );
                }
            }
            '$' if chars.peek() == Some(&'0') => {
                chars.next();
            }
            _ => out.push(c),
        }
    }
    out
}

// ─── Completion system ───────────────────────────────────────────────────────

const PUNCTUATION: &[&str] = &[";", ".", ",", "[", "]", "(", ")"];

/// Where to insert a template, and whether it needs a separating space.
fn insert_range(token: &TokenComponent, pos: Position) -> (Range, bool) {
    let is_punct = PUNCTUATION.contains(&token.text.as_str());
    let contains = token.range.start <= pos && pos <= token.range.end;
    if !is_punct && contains {
        // Replace what the user typed.
        (token.range, false)
    } else if is_punct && contains && token.range.start < pos {
        // Right after `;` / `[`: insert behind it.
        (Range::new(token.range.end, token.range.end), true)
    } else {
        // The token is further on (e.g. a `.` on the next line): insert here.
        (Range::new(pos, pos), false)
    }
}

/// Move an empty insertion point to the end of its line when only whitespace
/// follows it, so a template typed on a blank line keeps the line's indentation
/// (the backend reports completion positions one character early).
fn settle(rope: &RopeC, range: Range) -> Range {
    if range.start != range.end {
        return range;
    }
    let Some(line) = rope.0.line_str(range.start.line as usize) else {
        return range;
    };
    let line = line.trim_end_matches(['\n', '\r']);
    let col = range.start.character as usize;
    let chars: Vec<char> = line.chars().collect();
    if col >= chars.len() || !chars[col..].iter().all(|c| *c == ' ' || *c == '\t') {
        return range;
    }
    let end = Position::new(range.start.line, chars.len() as u32);
    Range::new(end, end)
}

/// Indentation for continuation lines of a template inserted at `at`.
fn indentation(rope: &RopeC, at: Position) -> String {
    let line = rope.0.line_str(at.line as usize).unwrap_or_default();
    let leading: String = line
        .chars()
        .take_while(|c| *c == ' ' || *c == '\t')
        .collect();
    let before: String = line.chars().take(at.character as usize).collect();
    if before.trim().is_empty() {
        leading
    } else {
        format!("{}    ", leading)
    }
}

/// Completion system offering "required" and "all properties" templates from
/// the SHACL shapes that apply to the subject at the cursor.
pub fn complete_shape_templates(
    mut query: Query<(
        &TokenComponent,
        &TripleComponent,
        &PositionComponent,
        &Prefixes,
        &Triples,
        &Label,
        &DocumentLinks,
        &RopeC,
        &DynLang,
        &mut CompletionRequest,
    )>,
    shapes: Query<(&Label, &Triples, Option<&Global>), With<ShaclShapes>>,
    hierarchy: Res<TypeHierarchy>,
    config: Res<ServerConfig>,
) {
    let local = &config.config.local;
    if local.is_disabled(Disabled::CompletionShapeTemplate) || local.is_disabled(Disabled::Shapes) {
        return;
    }

    for (token, triple, pos, prefixes, triples, label, links, rope, lang, mut request) in &mut query
    {
        if !lang.shape_templates() {
            continue;
        }
        let subject = &triple.triple.subject;
        let in_blank_node_body = triple.target == TripleTarget::Subject
            && subject.kind() == TermKind::BlankNode
            && token.text == "]";
        if triple.target != TripleTarget::Predicate && !in_blank_node_body {
            continue;
        }

        let docs: Vec<&[MyQuad<'static>]> = shapes
            .iter()
            .filter(|(l, _, global)| {
                global.is_some() || l.0 == label.0 || links.iter().any(|link| link.0 == l.0)
            })
            .map(|(_, t, _)| t.0.as_slice())
            .collect();
        if docs.is_empty() {
            continue;
        }
        let graph = ShapeGraph::new(docs);
        let focus = Focus::new(subject, &triples.0, &hierarchy);
        let all = missing_properties(&graph, &focus);
        if all.is_empty() {
            continue;
        }
        let required: Vec<_> = all.iter().filter(|p| p.required).cloned().collect();

        let (range, leading_space) = insert_range(token, pos.0);
        let range = settle(rope, range);
        let indent = indentation(rope, range.start);
        let filter = if range == token.range {
            token.text.clone()
        } else {
            String::new()
        };

        let mut offer = |props: &[Property], label: &str, sort: &str| {
            let snippet = render(props, prefixes, &indent);
            let preview = plain(&snippet);
            let new_text = if leading_space {
                format!(" {}", snippet)
            } else {
                snippet
            };
            let names: Vec<_> = props.iter().map(|p| p.name.as_str()).collect();
            request.push(
                SimpleCompletion::new(
                    CompletionItemKind::SNIPPET,
                    label.to_string(),
                    TextEdit { range, new_text },
                )
                .as_snippet()
                .label_description(names.join(", "))
                .documentation(documentation(props, prefixes, &preview))
                .sort_text(sort)
                .filter_text(format!("{}{}", filter, label)),
            );
        };

        if !required.is_empty() {
            offer(&required, "shape template: required properties", "00");
        }
        if all.len() > required.len() {
            offer(&all, "shape template: all properties", "01");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn plain_strips_snippet_syntax() {
        assert_eq!(
            plain("ex:a ${1:name} ;\n  ex:b ${2|ex:x,ex:y|} ;\n  ex:c \"\\$5\"$0"),
            "ex:a name ;\n  ex:b ex:x ;\n  ex:c \"$5\""
        );
    }

    #[test]
    fn escapes_snippet_specials() {
        assert_eq!(escape("a$b}c\\"), "a\\$b\\}c\\\\");
        assert_eq!(escape_choice("a,b|c"), "a\\,b\\|c");
    }

    #[test]
    fn local_names() {
        assert_eq!(
            local_name("http://w3id.org/rml/logicalSource"),
            "logicalSource"
        );
        assert_eq!(local_name("http://ex.test/onto#name"), "name");
    }
}
