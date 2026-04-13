use std::{borrow::Cow, collections::HashSet};

// Re-export canonical model types from the turtle crate.
// NOTE: TurtleSimpleError is intentionally NOT re-exported — we define our own
// below, adding a Parse(IriParseError) variant needed by TriplesBuilder.
pub use rdf_parsers::model::{
    Base, BlankNode, Literal, NamedNode, RDFLiteral, StringStyle, Term, Triple, Turtle,
    TurtlePrefix, Variable, PO,
};
use sophia_iri::resolve::{BaseIri, IriParseError};
use swls_core::{
    lsp_types::Url,
    prelude::{MyQuad, MyTerm, Spanned, TermContext, Triples2},
};
use tracing::warn;

// ── Based trait ──────────────────────────────────────────────────────────────

pub trait Based {
    fn get_base(&self) -> &str;
    fn prefixes(&self) -> &[Spanned<TurtlePrefix>];
}

impl Based for Turtle {
    fn get_base(&self) -> &str {
        self.set_base.as_deref().unwrap_or("")
    }
    fn prefixes(&self) -> &[Spanned<TurtlePrefix>] {
        &self.prefixes
    }
}

// ── Error type ───────────────────────────────────────────────────────────────

#[derive(Debug)]
pub enum TurtleSimpleError {
    Parse(IriParseError),
    UnexpectedBase(&'static str),
    UnexpectedBaseString(String),
}

// ── NamedNode extension trait ─────────────────────────────────────────────────

pub trait NamedNodeExt {
    /// Fully expand a (possibly prefixed) named node to an absolute IRI string.
    fn expand<T: Based>(&self, turtle: &T) -> Option<String>;
    fn expand_step<T: Based>(&self, turtle: &T, done: HashSet<String>) -> Option<String>;
}

impl NamedNodeExt for NamedNode {
    fn expand<T: Based>(&self, turtle: &T) -> Option<String> {
        let out = self.expand_step(turtle, HashSet::new())?;
        let base = Url::parse(turtle.get_base()).ok()?;
        let url = base.join(&out).ok()?;
        Some(url.to_string())
    }

    fn expand_step<T: Based>(&self, turtle: &T, mut done: HashSet<String>) -> Option<String> {
        match self {
            Self::Full(s, _) => Some(s.clone()),
            Self::Prefixed { prefix, value, .. } => {
                if done.contains(prefix.as_str()) {
                    return None;
                }
                done.insert(prefix.clone());
                let prefix_entry = turtle
                    .prefixes()
                    .iter()
                    .find(|x| x.prefix.as_str() == prefix.as_str())?;
                let expanded = NamedNodeExt::expand_step(prefix_entry.value.value(), turtle, done)?;
                Some(format!("{}{}", expanded, value))
            }
            Self::A(_) => Some("http://www.w3.org/1999/02/22-rdf-syntax-ns#type".to_string()),
            Self::Invalid => None,
        }
    }
}

// ── TurtlePrefix extension trait ──────────────────────────────────────────────

pub trait TurtlePrefixExt {
    fn shorten<T: Based>(&self, turtle: &T, url: &str) -> Option<String>;
}

impl TurtlePrefixExt for TurtlePrefix {
    fn shorten<T: Based>(&self, turtle: &T, url: &str) -> Option<String> {
        let prefix_url = NamedNodeExt::expand(self.value.value(), turtle)?;
        let short = url.strip_prefix(&prefix_url)?;
        Some(format!("{}:{}", self.prefix.as_str(), short))
    }
}

// ── Turtle extension trait ────────────────────────────────────────────────────

pub trait TurtleExt {
    fn get_simple_triples<'a>(&'a self) -> Result<Triples2<'a>, TurtleSimpleError>;
    fn into_triples<'a>(&self, triples: Vec<MyQuad<'a>>) -> Triples2<'a>;
    fn shorten(&self, url: &str) -> Option<String>;
}

impl TurtleExt for Turtle {
    fn get_simple_triples<'a>(&'a self) -> Result<Triples2<'a>, TurtleSimpleError> {
        let base = match &self.base {
            Some(Spanned(Base(_, Spanned(named_node, _)), _)) => {
                let nn = NamedNodeExt::expand_step(named_node, self, HashSet::new()).ok_or(
                    TurtleSimpleError::UnexpectedBase("Expected valid named node base"),
                )?;
                BaseIri::new(nn).map_err(TurtleSimpleError::Parse)?
            }
            None => BaseIri::new(self.set_base.clone().unwrap_or_default())
                .map_err(TurtleSimpleError::Parse)?,
        };

        let mut builder = TriplesBuilder::new(self, base);

        for t in &self.triples {
            let _ = builder.ingest(t);
        }

        Ok(self.into_triples(builder.triples))
    }

    fn into_triples<'a>(&self, triples: Vec<MyQuad<'a>>) -> Triples2<'a> {
        let base = match &self.base {
            Some(Spanned(Base(_, Spanned(named_node, span)), _)) => {
                NamedNodeExt::expand_step(named_node, self, HashSet::new())
                    .map(|st| MyTerm::named_node(st, span.clone()))
            }
            None => Some(MyTerm::named_node(
                self.set_base.clone().unwrap_or_default(),
                0..0,
            )),
        };
        let base_url = self.set_base.clone().unwrap_or_default();
        Triples2 {
            triples,
            base,
            base_url,
        }
    }

    fn shorten(&self, url: &str) -> Option<String> {
        self.prefixes
            .iter()
            .find_map(|p| TurtlePrefixExt::shorten(p.value(), self, url))
    }
}

// ── TriplesBuilder ────────────────────────────────────────────────────────────

pub struct TriplesBuilder<'a, T> {
    pub triples: Vec<MyQuad<'a>>,
    blank_node: Box<dyn FnMut(std::ops::Range<usize>) -> MyTerm<'a>>,
    base: BaseIri<String>,
    based: &'a T,
}

impl<'a, T: Based> TriplesBuilder<'a, T> {
    pub fn new(based: &'a T, base: BaseIri<String>) -> Self {
        let mut count = 0;
        let blank_node = Box::new(move |span: std::ops::Range<usize>| {
            count += 1;
            MyTerm::blank_node(format!("internal_bnode_{}", count), span)
        });
        Self {
            triples: vec![],
            blank_node,
            base,
            based,
        }
    }

    fn handle_po(
        &mut self,
        pos: &'a [Spanned<PO>],
        span: std::ops::Range<usize>,
        subject: MyTerm<'a>,
    ) -> Result<(), TurtleSimpleError> {
        if pos.is_empty() {
            // Subject exists but has no predicate-object pairs (e.g. an incomplete statement).
            // Push a placeholder triple so that TripleTarget::Subject is returned for any
            // cursor position within this statement's span.
            self.triples.push(MyQuad {
                subject: subject.clone(),
                predicate: MyTerm::invalid(0..0),
                object: MyTerm::invalid(0..0),
                span: span.clone(),
            });
        }
        let mut first = true;
        for Spanned(PO { predicate, object }, span2) in pos.iter() {
            let this_span = if first {
                first = false;
                span.clone()
            } else {
                span2.clone()
            };

            let predicate_term = match predicate.value() {
                Term::NamedNode(NamedNode::Invalid) => MyTerm::invalid(predicate.span().clone()),
                Term::NamedNode(nn) => {
                    if let Ok(node) = NamedNodeExt::expand_step(nn, self.based, HashSet::new())
                        .ok_or(TurtleSimpleError::UnexpectedBase(
                            "Expected valid named node for predicate",
                        ))
                        .and_then(|n| {
                            self.base
                                .resolve(n.as_str())
                                .map_err(TurtleSimpleError::Parse)
                        })
                        .map(|x| x.unwrap())
                    {
                        MyTerm::named_node(node, predicate.span().clone())
                    } else {
                        MyTerm::invalid(predicate.span().clone())
                    }
                }
                _ => MyTerm::invalid(predicate.span().clone()),
            };

            if object.is_empty() {
                // Incomplete statement (e.g. `<> foaf:` with no object yet).
                // Push a placeholder triple so get_current_triple can still detect
                // the predicate position and return property completions.
                let end = predicate.span().end;
                self.triples.push(MyQuad {
                    subject: subject.clone(),
                    predicate: predicate_term.clone(),
                    object: MyTerm::invalid(end..end),
                    span: this_span.clone(),
                });
            }
            let mut first_object = true;
            for o in object.iter() {
                let this_span = if first_object {
                    first_object = false;
                    this_span.clone()
                } else {
                    o.span().clone()
                };
                let obj = self.term_to_my_term(Ok(o.as_ref()))?;
                self.triples.push(MyQuad {
                    subject: subject.clone(),
                    predicate: predicate_term.clone(),
                    object: obj,
                    span: this_span,
                });
            }
        }
        Ok(())
    }

    fn term_to_my_term(
        &mut self,
        term: Result<Spanned<&'a Term>, MyTerm<'a>>,
    ) -> Result<MyTerm<'a>, TurtleSimpleError> {
        let object = match term {
            Ok(Spanned(Term::Variable(Variable(var, _)), span)) => MyTerm::variable(var, span),
            Ok(Spanned(Term::NamedNode(NamedNode::Invalid), span)) => MyTerm::invalid(span),
            Ok(Spanned(Term::NamedNode(nn), span)) => MyTerm::named_node(
                NamedNodeExt::expand_step(nn, self.based, HashSet::new())
                    .ok_or(TurtleSimpleError::UnexpectedBase(
                        "Expected valid named node for object",
                    ))
                    .and_then(|n| {
                        self.base
                            .resolve(n.as_str())
                            .map_err(TurtleSimpleError::Parse)
                    })
                    .map(|x| x.unwrap())?,
                span,
            ),
            Ok(Spanned(Term::Literal(Literal::RDF(lit)), span)) => {
                let term_context = match (&lit.lang, &lit.ty) {
                    (Some(l), _) => TermContext::LangTag(Cow::Owned(l.to_string())),
                    (_, Some(dt)) => {
                        if let Some(dt) = NamedNodeExt::expand_step(dt, self.based, HashSet::new())
                        {
                            TermContext::DataType(dt.into())
                        } else {
                            TermContext::None
                        }
                    }
                    _ => TermContext::None,
                };
                MyTerm::literal(lit.value.to_string(), span, term_context)
            }
            Ok(Spanned(Term::Literal(Literal::Boolean(bool)), span)) => {
                let term_context =
                    TermContext::DataType("http://www.w3.org/2001/XMLSchema#boolean".into());
                MyTerm::literal(bool.to_string(), span, term_context)
            }
            Ok(Spanned(Term::Literal(Literal::Numeric(num)), span)) => {
                let term_context =
                    TermContext::DataType("http://www.w3.org/2001/XMLSchema#integer".into());
                MyTerm::literal(num.to_string(), span, term_context)
            }
            Ok(Spanned(Term::BlankNode(bn), span)) => match bn {
                BlankNode::Named(v, _) => MyTerm::blank_node(v, span),
                BlankNode::Unnamed(v, _, _) => {
                    let out = (self.blank_node)(span.clone());
                    self.handle_po(v, span, out.clone())?;
                    out
                }
                BlankNode::Invalid => {
                    return Err(TurtleSimpleError::UnexpectedBase(
                        "Unexpected invalid blank for object",
                    ))
                }
            },
            Ok(Spanned(Term::Collection(terms), span)) => self.handle_collection(terms, span)?,
            Ok(Spanned(Term::Invalid, span)) => MyTerm::invalid(span),
            Err(x) => x,
        };
        Ok(object)
    }

    fn handle_collection(
        &mut self,
        collection: &'a [Spanned<Term>],
        span: std::ops::Range<usize>,
    ) -> Result<MyTerm<'a>, TurtleSimpleError> {
        let mut prev = MyTerm::named_node(
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil",
            span.end..span.end,
        );
        for Spanned(term, s) in collection.iter().rev() {
            let next = (self.blank_node)(s.clone());
            let obj = self.term_to_my_term(Ok(Spanned(term, s.clone())))?;
            self.triples.push(MyQuad {
                subject: next.clone(),
                predicate: MyTerm::named_node(
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#first",
                    prev.span.start..prev.span.start,
                ),
                object: obj,
                span: span.clone(),
            });
            self.triples.push(MyQuad {
                subject: next.clone(),
                predicate: MyTerm::named_node(
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest",
                    s.start..s.start,
                ),
                object: prev,
                span: s.clone(),
            });
            prev = next;
        }
        Ok(prev)
    }

    pub fn ingest(
        &mut self,
        Spanned(ref triple, span): &'a Spanned<Triple>,
    ) -> Result<(), TurtleSimpleError> {
        let sub = match triple.subject.value() {
            Term::BlankNode(BlankNode::Named(vs, _)) => {
                MyTerm::blank_node(vs, triple.subject.span().clone())
            }
            Term::BlankNode(BlankNode::Unnamed(vs, _, _)) => {
                let out = (self.blank_node)(triple.subject.span().clone());
                self.handle_po(vs, triple.subject.span().clone(), out.clone())?;
                out
            }
            Term::NamedNode(NamedNode::Invalid) => MyTerm::invalid(triple.subject.span().clone()),
            Term::NamedNode(nn) => MyTerm::named_node(
                NamedNodeExt::expand_step(nn, self.based, HashSet::new())
                    .ok_or(TurtleSimpleError::UnexpectedBase(
                        "Expected valid named node for subject",
                    ))
                    .and_then(|n| {
                        self.base
                            .resolve(n.as_str())
                            .map_err(TurtleSimpleError::Parse)
                    })
                    .map(|x| x.unwrap())?,
                triple.subject.span().clone(),
            ),
            Term::Invalid => MyTerm::invalid(triple.subject.span().clone()),
            Term::Variable(var) => MyTerm::variable(&var.0, triple.subject.span().clone()),
            x => {
                warn!("Failed, unexpected {}", x.ty());
                return Err(TurtleSimpleError::UnexpectedBaseString(format!(
                    "Unexpected {}",
                    x.ty()
                )));
            }
        };
        self.handle_po(&triple.po, span.clone(), sub)?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use std::{collections::HashSet, str::FromStr};

    use swls_core::prelude::MyQuad;

    use super::{Turtle, TurtleExt};

    fn parse_turtle(inp: &str, base_url: &str) -> Turtle {
        use rdf_parsers::turtle::{convert::convert, Lang, Rule, SyntaxKind};
        let (parse, _) = rdf_parsers::parse_incremental(
            Rule::new(SyntaxKind::TurtleDoc),
            inp,
            None,
            rdf_parsers::IncrementalBias::default(),
        );
        let mut t = convert(&parse.syntax::<Lang>());
        t.set_base = Some(base_url.to_string());
        t
    }

    #[test]
    fn easy_triples() {
        let txt = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

[] a foaf:Name;
   foaf:knows <abc>;.
"#;
        let output = parse_turtle(txt, "http://example.com/ns#");
        let triples = output.get_simple_triples().expect("Triples found");
        assert_eq!(triples.triples.len(), 3);
        println!("{:?}", triples);
    }

    #[test]
    fn easy_triples_2() {
        let txt = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

[ foaf:knows <abc>; ]
    a foaf:Name;
    foaf:knows [
        a foaf:Name;
        foaf:knows [
            a foaf:Name; ] ].
"#;
        let output = parse_turtle(txt, "http://example.com/ns#");
        let triples = output.get_simple_triples().expect("Triples found");
        assert_eq!(triples.triples.len(), 6);
    }

    #[test]
    fn triples_collection() {
        let txt = r#"
<e> <pred> (<a> <b> <c>).
"#;
        let output = parse_turtle(txt, "http://example.com/");
        let triples = output
            .get_simple_triples()
            .expect("Triples found collection");
        let a: &Vec<MyQuad<'_>> = &triples;
        let quads: HashSet<String> = a
            .iter()
            .map(|triple| format!("{} {} {}.", triple.subject, triple.predicate, triple.object))
            .collect();

        let expected_quads: HashSet<String> = [
            "<http://example.com/e> <http://example.com/pred> _:internal_bnode_3.",
            "_:internal_bnode_3 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:internal_bnode_2.",
            "_:internal_bnode_3 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.com/a>.",
            "_:internal_bnode_2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:internal_bnode_1.",
            "_:internal_bnode_2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.com/b>.",
            "_:internal_bnode_1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> <http://www.w3.org/1999/02/22-rdf-syntax-ns#nil>.",
            "_:internal_bnode_1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.com/c>.",
        ].iter().map(|x| x.trim().to_string()).collect();

        for t in &quads {
            println!("{}", t);
        }
        assert_eq!(quads, expected_quads);
        assert_eq!(triples.triples.len(), 7);
    }

    #[test]
    fn triple_spans_are_correct() {
        // "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n<> foaf:name \"Arthur\"."
        // Byte layout after the newline (offset 44):
        //   44..46  "<>"
        //   47..56  "foaf:name"
        //   57..65  "\"Arthur\""
        let txt = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n<> foaf:name \"Arthur\".";
        let output = parse_turtle(txt, "http://example.com/");
        let triples = output.get_simple_triples().expect("Triples found");
        assert_eq!(
            triples.triples.len(),
            1,
            "expected exactly one triple, got: {:#?}",
            triples.triples
        );
        let t = &triples.triples[0];
        println!("triple span:    {:?}", t.span);
        println!("subject span:   {:?}", t.subject.span);
        println!("predicate span: {:?}", t.predicate.span);
        println!("object span:    {:?}", t.object.span);

        // Subject "<>" is at bytes 44..46
        assert!(
            t.subject.span.contains(&44),
            "cursor at start of subject should be in subject span, span={:?}",
            t.subject.span
        );
        // Predicate "foaf:name" is at bytes 47..56
        assert!(
            t.predicate.span.contains(&50),
            "cursor in middle of predicate should be in predicate span, span={:?}",
            t.predicate.span
        );
        // Object "\"Arthur\"" is at bytes 57..65
        assert!(
            t.object.span.contains(&60),
            "cursor in middle of object should be in object span, span={:?}",
            t.object.span
        );

        // The triple outer span must contain all positions
        assert!(t.span.contains(&44));
        assert!(t.span.contains(&50));
        assert!(t.span.contains(&60));
    }

    #[test]
    fn owl_is_valid() {
        let txt = include_str!("../../../lov/prefixes/owl.ttl");
        let output = parse_turtle(txt, "http://example.com/ns#");
        output.get_simple_triples().expect("Triples found");
    }

    #[test]
    fn owl_is_valid_2() {
        let txt = r#"
@prefix dc:    <http://purl.org/dc/elements/1.1/> .
@prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl:   <http://www.w3.org/2002/07/owl#> .
@prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
@prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xml:   <http://www.w3.org/XML/1998/namespace> .
@prefix grddl: <http://www.w3.org/2003/g/data-view#> .

<http://www.w3.org/2002/07/owl>
        a                              owl:Ontology ;
        rdfs:comment                   "
  This ontology partially describes the built-in " ; .
            "#;
        let output = parse_turtle(txt, "http://example.com/ns#");
        output.get_simple_triples().expect("Triples found");
    }
}
