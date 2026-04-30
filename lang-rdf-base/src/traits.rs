use std::{borrow::Cow, collections::HashSet};

// Re-export canonical model types from the turtle crate.
// NOTE: TurtleSimpleError is intentionally NOT re-exported — we define our own
// below, adding a Parse(IriParseError) variant needed by TriplesBuilder.
pub use rdf_parsers::model::{
    Base, BlankNode, Literal, NamedNode, RDFLiteral, StringStyle, Term, Triple, Turtle,
    TurtlePrefix, Variable, PO,
};
use swls_core::{
    lsp_types::Url,
    prelude::{MyQuad, MyTerm, Spanned, TermContext, Triples2},
    util::resolve_iri,
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

// ── TriplesBuilder ────────────────────────────────────────────────────────────

pub struct TriplesBuilder<'a, T> {
    pub triples: Vec<MyQuad<'a>>,
    blank_node: Box<dyn FnMut(std::ops::Range<usize>) -> MyTerm<'a>>,
    base: String,
    based: &'a T,
}

impl<'a, T: Based> TriplesBuilder<'a, T> {
    pub fn new(based: &'a T, base: String) -> Self {
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
                    if let Some(node) = NamedNodeExt::expand_step(nn, self.based, HashSet::new())
                        .map(|n| resolve_iri(&self.base, &n))
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
            Ok(Spanned(Term::NamedNode(nn), span)) => {
                match NamedNodeExt::expand_step(nn, self.based, HashSet::new()) {
                    Some(n) => MyTerm::named_node(resolve_iri(&self.base, &n), span),
                    None => MyTerm::invalid(span),
                }
            }
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
            Term::NamedNode(nn) => {
                match NamedNodeExt::expand_step(nn, self.based, HashSet::new()) {
                    Some(n) => MyTerm::named_node(
                        resolve_iri(&self.base, &n),
                        triple.subject.span().clone(),
                    ),
                    None => MyTerm::invalid(triple.subject.span().clone()),
                }
            }
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

#[derive(Debug)]
pub enum TurtleSimpleError {
    UnexpectedBase(&'static str),
    UnexpectedBaseString(String),
}

// ── Turtle extension trait ────────────────────────────────────────────────────
pub trait TurtleExt {
    fn get_simple_triples<'a>(&'a self) -> Result<Triples2<'a>, TurtleSimpleError>;
    fn into_triples<'a>(&self, triples: Vec<MyQuad<'a>>) -> Triples2<'a>;
    fn shorten(&self, url: &str) -> Option<String>;
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

impl TurtleExt for Turtle {
    fn get_simple_triples<'a>(&'a self) -> Result<Triples2<'a>, TurtleSimpleError> {
        let base = match &self.base {
            Some(Spanned(Base(_, Spanned(named_node, _)), _)) => {
                NamedNodeExt::expand_step(named_node, self, HashSet::new()).ok_or(
                    TurtleSimpleError::UnexpectedBase("Expected valid named node base"),
                )?
            }
            None => self.set_base.clone().unwrap_or_default(),
        };

        let mut builder = TriplesBuilder::new(self, base);

        for t in &self.triples {
            if let Err(e) = builder.ingest(t) {
                tracing::error!("Failed parsing triple {}: {:?}", t.value(), e);
            }
        }

        Ok(self.into_triples(builder.triples))
    }

    fn into_triples<'a>(&self, triples: Vec<MyQuad<'a>>) -> Triples2<'a> {
        tracing::debug!("Results in {} triples", triples.len());
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
