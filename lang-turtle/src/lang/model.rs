use std::{collections::HashSet, fmt::Display, ops::Range};

use lsp_core::prelude::{MyQuad, MyTerm, Spanned, StringStyle, Triples2};
use sophia_iri::resolve::{BaseIri, IriParseError};
use tracing::info;

use super::context::{Context, ContextKind};

pub trait Based {
    fn get_base(&self) -> &lsp_types::Url;
    fn prefixes(&self) -> &[Spanned<TurtlePrefix>];
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable(pub String, pub usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    RDF(RDFLiteral),
    Boolean(bool),
    Numeric(String),
}

impl Literal {
    pub fn plain_string(&self) -> String {
        match self {
            Literal::RDF(s) => s.plain_string(),
            Literal::Boolean(x) => x.to_string(),
            Literal::Numeric(x) => x.clone(),
        }
    }
}
impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::RDF(x) => x.fmt(f),
            Literal::Boolean(x) => write!(f, "{}", x),
            Literal::Numeric(x) => write!(f, "{}", x),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RDFLiteral {
    pub value: String,
    pub quote_style: StringStyle,
    pub lang: Option<String>,
    pub ty: Option<NamedNode>,
    // Span of tokens
    pub idx: usize,
    pub len: usize,
}

impl RDFLiteral {
    pub fn plain_string(&self) -> String {
        self.value.to_string()
    }
}

impl Display for RDFLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let quote = match self.quote_style {
            StringStyle::DoubleLong => "\"\"\"",
            StringStyle::Double => "\"",
            StringStyle::SingleLong => "'''",
            StringStyle::Single => "'",
        };
        match (&self.lang, &self.ty) {
            (None, None) => write!(f, "{}{}{}", quote, self.value, quote),
            (None, Some(t)) => write!(f, "{}{}{}^^{}", quote, self.value, quote, t),
            (Some(l), None) => write!(f, "{}{}{}@{}", quote, self.value, quote, l),
            (Some(l), Some(t)) => write!(f, "{}{}{}@{}^^{}", quote, self.value, quote, l, t),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NamedNode {
    Full(String, usize),
    Prefixed {
        prefix: String,
        value: String,
        idx: usize,
    },
    A(usize),
    Invalid,
}

impl NamedNode {
    pub fn expand<T: Based>(&self, turtle: &T) -> Option<String> {
        let base = turtle.get_base();
        let out = self.expand_step(turtle, HashSet::new())?;

        let url = base.join(&out).ok()?;

        Some(url.to_string())
    }

    pub fn expand_step<'a, T: Based>(
        &'a self,
        turtle: &T,
        mut done: HashSet<&'a str>,
    ) -> Option<String> {
        match self {
            Self::Full(s, _) => s.clone().into(),
            Self::Prefixed {
                prefix,
                value,
                idx: _,
            } => {
                if done.contains(prefix.as_str()) {
                    return None;
                }
                done.insert(prefix);
                let prefix = turtle
                    .prefixes()
                    .iter()
                    .find(|x| x.prefix.as_str() == prefix.as_str())?;

                let expaned = prefix.value.expand_step(turtle, done)?;
                Some(format!("{}{}", expaned, value))
            }
            Self::A(_) => Some("http://www.w3.org/1999/02/22-rdf-syntax-ns#type".to_string()),
            Self::Invalid => None,
        }
    }
}

impl Display for NamedNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NamedNode::Full(x, _) => write!(f, "<{}>", x),
            NamedNode::Prefixed {
                prefix,
                value,
                idx: _,
            } => write!(f, "{}:{}", prefix, value),
            NamedNode::A(_) => write!(f, "a"),
            NamedNode::Invalid => write!(f, "invalid"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BlankNode {
    Named(String, usize),
    Unnamed(Vec<Spanned<PO>>, usize, usize),
    Invalid,
}

fn rev_range(range: &std::ops::Range<usize>, len: usize) -> std::ops::Range<usize> {
    (len - range.end)..(len - range.start)
}

impl BlankNode {
    pub fn fix_spans(&mut self, len: usize) {
        match self {
            BlankNode::Unnamed(ref mut pos, _, _) => {
                pos.iter_mut().for_each(|span| {
                    span.1 = rev_range(&span.1, len);
                    span.0.fix_spans(len);
                });
            }
            _ => {}
        }
    }
}

impl Display for BlankNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BlankNode::Named(x, _) => write!(f, "_:{}", x),
            BlankNode::Unnamed(pos, _, _) => {
                if pos.len() == 0 {
                    write!(f, "[ ]")
                } else {
                    write!(f, "[ ")?;

                    for po in pos {
                        write!(f, "{} ;", po.value())?;
                    }

                    write!(f, " ]")
                }
            }
            BlankNode::Invalid => write!(f, "invalid"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Literal(Literal),
    BlankNode(BlankNode),
    NamedNode(NamedNode),
    Collection(Vec<Spanned<Term>>),
    Variable(Variable),
    Invalid,
}

impl Term {
    pub fn set_context(&self, ctx: &mut Context, kind: ContextKind) {
        match self {
            Term::Literal(Literal::RDF(RDFLiteral { idx, len, .. })) => {
                for i in *idx..*idx + *len {
                    ctx.add(i, kind);
                }
            }

            Term::BlankNode(BlankNode::Unnamed(pos, start, end)) => {
                ctx.add(*start, kind);
                ctx.add(*end, kind);
                for po in pos {
                    po.set_context(ctx);
                }
            }
            Term::BlankNode(BlankNode::Named(_, idx))
            | Term::Variable(Variable(_, idx))
            | Term::NamedNode(NamedNode::Full(_, idx))
            | Term::NamedNode(NamedNode::A(idx))
            | Term::NamedNode(NamedNode::Prefixed { idx, .. }) => ctx.add(*idx, kind),
            _ => {}
        }
    }
    pub fn fix_spans(&mut self, len: usize) {
        match self {
            Term::BlankNode(bn) => bn.fix_spans(len),
            Term::Collection(pos) => {
                pos.iter_mut().for_each(|span| {
                    span.1 = rev_range(&span.1, len);
                    span.0.fix_spans(len);
                });
            }
            _ => {}
        }
    }

    pub fn named_node(&self) -> Option<&NamedNode> {
        match self {
            Term::NamedNode(nn) => Some(&nn),
            _ => None,
        }
    }

    pub fn is_subject(&self) -> bool {
        match self {
            Term::BlankNode(_) => true,
            Term::Variable(_) => true,
            Term::NamedNode(NamedNode::A(_)) => false,
            Term::NamedNode(_) => true,
            Term::Invalid => true,
            Term::Collection(_) => true,
            _ => false,
        }
    }
    pub fn is_predicate(&self) -> bool {
        match self {
            Term::NamedNode(_) => true,
            Term::Variable(_) => true,
            Term::Invalid => true,
            _ => false,
        }
    }

    pub fn is_object(&self) -> bool {
        match self {
            Term::NamedNode(NamedNode::A(_)) => false,
            Term::Variable(_) => true,
            Term::Invalid => true,
            Term::Collection(_) => true,
            _ => true,
        }
    }
    pub fn is_variable(&self) -> bool {
        match self {
            Term::Variable(_) => true,
            Term::Invalid => true,
            _ => false,
        }
    }
    pub fn ty(&self) -> &'static str {
        match self {
            Term::Literal(_) => "literal",
            Term::BlankNode(_) => "blank node",
            Term::NamedNode(_) => "named node",
            Term::Collection(_) => "collection",
            Term::Invalid => "invalid",
            Term::Variable(_) => "variable",
        }
    }
    pub fn expand<T: Based>(&self, turtle: &T) -> Option<String> {
        self.named_node()?.expand(turtle)
    }
    pub fn expand_step<'a, T: Based>(
        &'a self,
        turtle: &T,
        done: HashSet<&'a str>,
    ) -> Option<String> {
        self.named_node()?.expand_step(turtle, done)
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Literal(l) => l.fmt(f),
            Term::BlankNode(b) => b.fmt(f),
            Term::NamedNode(n) => n.fmt(f),
            Term::Collection(n) => {
                write!(f, "( ")?;
                for l in n {
                    l.fmt(f)?;
                }
                write!(f, "  )")?;
                Ok(())
            }
            Term::Invalid => write!(f, "invalid"),
            Term::Variable(x) => write!(f, "{}", x.0),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Triple {
    pub subject: Spanned<Term>,
    pub po: Vec<Spanned<PO>>,
}

impl Triple {
    pub fn fix_spans(&mut self, len: usize) {
        self.subject.1 = rev_range(&self.subject.1, len);
        self.subject.0.fix_spans(len);

        self.po.iter_mut().for_each(|span| {
            span.1 = rev_range(&span.1, len);
            span.0.fix_spans(len);
        });
    }

    pub fn set_context(&self, ctx: &mut Context) {
        self.subject.set_context(ctx, ContextKind::Subject);

        for po in &self.po {
            po.set_context(ctx);
        }
    }
}

impl Display for Triple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.subject.value(), self.po[0].value())?;

        for po in &self.po[1..] {
            write!(f, "; {}", po.value())?;
        }

        write!(f, ".")
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PO {
    pub predicate: Spanned<Term>,
    pub object: Vec<Spanned<Term>>,
}
impl PO {
    pub fn fix_spans(&mut self, len: usize) {
        self.predicate.1 = rev_range(&self.predicate.1, len);
        self.predicate.0.fix_spans(len);

        self.object.iter_mut().for_each(|span| {
            span.1 = rev_range(&span.1, len);
            span.0.fix_spans(len);
        });
    }

    pub fn set_context(&self, ctx: &mut Context) {
        self.predicate.set_context(ctx, ContextKind::Predicate);

        for o in &self.object {
            o.set_context(ctx, ContextKind::Object);
        }
    }
}

impl Display for PO {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.predicate.value(), self.object[0].value())?;

        for po in &self.object[1..] {
            write!(f, ", {}", po.value())?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Base(pub Range<usize>, pub Spanned<NamedNode>);
impl Display for Base {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@base {} .", self.1.value())
    }
}
impl Base {
    pub fn fix_spans(&mut self, len: usize) {
        self.1 .1 = rev_range(&self.1 .1, len);
    }
    pub fn resolve_location(&mut self, location: &lsp_types::Url) {
        match self.1.value_mut() {
            NamedNode::Full(s, _) => {
                if let Some(ns) = location.join(&s).ok() {
                    *s = ns.to_string();
                }
            }
            _ => {}
        }
    }
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TurtlePrefix {
    pub span: Range<usize>,
    pub prefix: Spanned<String>,
    pub value: Spanned<NamedNode>,
}
impl TurtlePrefix {
    fn shorten<T: Based>(&self, turtle: &T, url: &str) -> Option<String> {
        let prefix_url = self.value.expand(turtle)?;
        let short = url.strip_prefix(&prefix_url)?;
        Some(format!("{}:{}", self.prefix.value(), short))
    }

    pub fn fix_spans(&mut self, len: usize) {
        self.span = rev_range(&self.span, len);
        self.prefix.1 = rev_range(&self.prefix.1, len);
        self.value.1 = rev_range(&self.value.1, len);
    }
}
impl Display for TurtlePrefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "@prefix {}: {} .",
            self.prefix.value(),
            self.value.value()
        )
    }
}

#[derive(Debug)]
pub enum TurtleSimpleError {
    Parse(IriParseError),
    UnexpectedBase(&'static str),
    UnexpectedBaseString(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Turtle {
    pub base: Option<Spanned<Base>>,
    pub set_base: lsp_types::Url,
    pub prefixes: Vec<Spanned<TurtlePrefix>>,
    pub triples: Vec<Spanned<Triple>>,
}
impl Based for Turtle {
    fn get_base(&self) -> &lsp_types::Url {
        &self.set_base
    }

    fn prefixes(&self) -> &[Spanned<TurtlePrefix>] {
        &self.prefixes
    }
}

impl Turtle {
    pub fn fix_spans(&mut self, len: usize) {
        self.base.iter_mut().for_each(|base| {
            base.1 = rev_range(&base.1, len);
            base.0.fix_spans(len);
        });
        self.prefixes.iter_mut().for_each(|base| {
            base.1 = rev_range(&base.1, len);
            base.0.fix_spans(len);
        });
        self.triples.iter_mut().for_each(|base| {
            base.1 = rev_range(&base.1, len);
            base.0.fix_spans(len);
        });
    }

    pub fn into_triples<'a>(&self, triples: Vec<MyQuad<'a>>) -> Triples2<'a> {
        let base = match &self.base {
            Some(Spanned(Base(_, Spanned(named_node, span)), _)) => named_node
                .expand_step(self, HashSet::new())
                .map(|st| MyTerm::named_node(st, span.clone())),
            None => Some(MyTerm::named_node(self.set_base.as_str().to_string(), 0..0)),
        };

        let base_url = self.set_base.to_string();
        Triples2 {
            triples,
            base,
            base_url,
        }
    }
}

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
            let predicate = MyTerm::named_node("TestPredicate", 0..0);
            let object = MyTerm::named_node("TestObject", 0..0);

            let quad = MyQuad {
                subject: subject.clone(),
                predicate: predicate.clone(),
                object,
                span: span.clone(),
            };

            self.triples.push(quad);
        }
        let mut first = true;

        for Spanned(PO { predicate, object }, span2) in pos.iter() {
            let this_span = if first {
                first = false;
                span.clone()
            } else {
                span2.clone()
            };

            let predicate = if let Ok(node) = predicate
                .value()
                .expand_step(self.based, HashSet::new())
                .ok_or(TurtleSimpleError::UnexpectedBase(
                    "Expected valid named node for object",
                ))
                .and_then(|n| {
                    self.base
                        .resolve(n.as_str())
                        .map_err(|e| TurtleSimpleError::Parse(e))
                })
                .map(|x| x.unwrap())
            {
                MyTerm::named_node(node, predicate.span().clone())
            } else {
                MyTerm::invalid(predicate.span().clone())
            };

            let mut first_object = true;
            for o in object.iter() {
                let this_span = if first_object {
                    first_object = false;
                    this_span.clone()
                } else {
                    o.span().clone()
                };
                let object = self.term_to_my_term(Ok(o.as_ref()))?;

                let quad = MyQuad {
                    subject: subject.clone(),
                    predicate: predicate.clone(),
                    object,
                    span: this_span,
                };

                self.triples.push(quad);
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
                nn.expand_step(self.based, HashSet::new())
                    .ok_or(TurtleSimpleError::UnexpectedBase(
                        "Expected valid named node for object",
                    ))
                    .and_then(|n| {
                        self.base
                            .resolve(n.as_str())
                            .map_err(|e| TurtleSimpleError::Parse(e))
                    })
                    .map(|x| x.unwrap())?,
                span,
            ),
            Ok(Spanned(Term::Literal(literal), span)) => {
                MyTerm::literal(literal.plain_string(), span)
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
            Ok(Spanned(Term::Collection(terms), span)) => self.handle_collection(&terms, span)?,
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

            let quad = MyQuad {
                subject: next.clone(),
                predicate: MyTerm::named_node(
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#first",
                    prev.span.start..prev.span.start,
                ),
                object: self.term_to_my_term(Ok(Spanned(term, s.clone())))?,
                span: span.clone(),
            };

            self.triples.push(quad);

            let quad = MyQuad {
                subject: next.clone(),
                predicate: MyTerm::named_node(
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest",
                    s.start..s.start,
                ),
                object: prev,
                span: s.clone(),
            };

            self.triples.push(quad);
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
                self.handle_po(&vs, triple.subject.span().clone(), out.clone())?;
                out
            }
            Term::NamedNode(NamedNode::Invalid) => MyTerm::invalid(triple.subject.span().clone()),
            Term::NamedNode(nn) => MyTerm::named_node(
                nn.expand_step(self.based, HashSet::new())
                    .ok_or(TurtleSimpleError::UnexpectedBase(
                        "Expected valid named node for object",
                    ))
                    .and_then(|n| {
                        self.base
                            .resolve(n.as_str())
                            .map_err(|e| TurtleSimpleError::Parse(e))
                    })
                    .map(|x| x.unwrap())?,
                triple.subject.span().clone(),
            ),
            Term::Invalid => MyTerm::invalid(triple.subject.span().clone()),
            Term::Variable(var) => MyTerm::variable(&var.0, triple.subject.span().clone()),

            x => {
                info!("Failed, unexpected {}", x.ty());
                return Err(TurtleSimpleError::UnexpectedBaseString(format!(
                    "Unexpected {}",
                    x.ty()
                )));
            }
        };

        self.handle_po(&triple.po, span.clone(), sub.clone())?;

        Ok(())
    }
}

impl Turtle {
    pub fn empty(location: &lsp_types::Url) -> Self {
        Self::new(None, Vec::new(), Vec::new(), location)
    }

    pub fn get_simple_triples<'a>(&'a self) -> Result<Triples2<'a>, TurtleSimpleError> {
        let base = match &self.base {
            Some(Spanned(Base(_, Spanned(named_node, _)), _)) => {
                let nn = named_node.expand_step(self, HashSet::new()).ok_or(
                    TurtleSimpleError::UnexpectedBase("Expected valid named node base"),
                )?;
                BaseIri::new(nn).map_err(TurtleSimpleError::Parse)?
            }
            None => BaseIri::new(self.set_base.as_str().to_string())
                .map_err(TurtleSimpleError::Parse)?,
        };

        let mut builder = TriplesBuilder::new(self, base);

        for t in &self.triples {
            builder.ingest(&t)?;
        }

        Ok(self.into_triples(builder.triples))
    }
}

impl Turtle {
    pub fn new(
        mut base: Option<Spanned<Base>>,
        prefixes: Vec<Spanned<TurtlePrefix>>,
        triples: Vec<Spanned<Triple>>,
        location: &lsp_types::Url,
    ) -> Self {
        if let Some(b) = base.as_mut() {
            b.resolve_location(location);
        }
        Self {
            base,
            prefixes,
            triples,
            set_base: location.clone(),
        }
    }

    pub fn set_context(&self, ctx: &mut Context) {
        for t in &self.triples {
            t.set_context(ctx);
        }
    }

    pub fn get_base(&self) -> &lsp_types::Url {
        &self.set_base
    }

    pub fn shorten(&self, url: &str) -> Option<String> {
        self.prefixes
            .iter()
            .flat_map(|pref| pref.shorten(self, url))
            .next()
    }
}

impl Display for Turtle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(b) = &self.base {
            writeln!(f, "{}", b.value())?;
        }

        self.prefixes
            .iter()
            .map(|x| x.value())
            .try_for_each(|x| writeln!(f, "{}", x))?;

        self.triples
            .iter()
            .map(|x| x.value())
            .try_for_each(|x| writeln!(f, "{}", x))?;

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use std::{collections::HashSet, str::FromStr};

    use lsp_core::prelude::{MyQuad, Spanned, Token};

    use super::Turtle;
    use crate::lang::{context::Context, parser as parser2, tokenizer::parse_tokens_str_safe};

    #[derive(Debug)]
    pub enum Err {
        Tokenizing,
    }

    fn parse_turtle(
        inp: &str,
        url: &lsp_types::Url,
    ) -> Result<(Turtle, Vec<Spanned<String>>), Err> {
        let context = Context::new();
        let ctx = context.ctx();
        let tokens = parse_tokens_str_safe(inp).map_err(|e| Err::Tokenizing)?;

        let mut comments: Vec<_> = tokens
            .iter()
            .flat_map(|x| {
                x.try_map_ref(|t| match t {
                    Token::Comment(x) => Some(x.clone()),
                    _ => None,
                })
            })
            .collect();
        comments.sort_by_key(|x| x.1.start);

        let (turtle, errs) = parser2::parse_turtle(&url, tokens, inp.len(), ctx);
        for e in errs {
            println!("Error {:?}", e);
        }

        Ok((turtle.into_value(), comments))
    }

    #[test]
    fn easy_triples() {
        let txt = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
# @base <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

[] a foaf:Name;
   foaf:knows <abc>;.
"#;

        let url = lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let (output, _) = parse_turtle(txt, &url).expect("Simple");
        let triples = output.get_simple_triples().expect("Triples found");

        assert_eq!(triples.triples.len(), 3);
        println!("{:?}", triples);
    }

    #[test]
    fn easy_triples_2() {
        let txt = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
# @base <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

[ foaf:knows <abc>; ] 
    a foaf:Name;
    foaf:knows [
        a foaf:Name;
        foaf:knows [
            a foaf:Name; ] ].
"#;

        let url = lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let (output, _) = parse_turtle(txt, &url).expect("Simple");
        let triples = output.get_simple_triples().expect("Triples found");

        assert_eq!(triples.triples.len(), 6);
    }

    #[test]
    fn triples_collection() {
        let txt = r#"
<e> <pred> (<a> <b> <c>).
"#;

        let url = lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let (output, _) = parse_turtle(txt, &url).expect("Simple collection");
        let triples = output.get_simple_triples().expect("Triples found");

        let a: &Vec<MyQuad<'_>> = &triples;

        let quads: HashSet<String> = a
            .iter()
            .map(|triple| format!("{} {} {}.", triple.subject, triple.predicate, triple.object))
            .collect();

        let expected_quads: HashSet<String> = "<http://example.com/e> <http://example.com/pred> _:internal_bnode_3.
_:internal_bnode_3 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:internal_bnode_2.
_:internal_bnode_3 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.com/a>.
_:internal_bnode_2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:internal_bnode_1.
_:internal_bnode_2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.com/b>.
_:internal_bnode_1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> <http://www.w3.org/1999/02/22-rdf-syntax-ns#nil>.
_:internal_bnode_1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.com/c>.".split("\n").map(|x|x.trim()).map(String::from).collect();

        for t in &quads {
            println!("{}", t);
        }
        assert_eq!(quads, expected_quads);

        println!("triples {:?}", triples);

        assert_eq!(triples.triples.len(), 7);
    }

    #[test]
    fn owl_is_valid() {
        let txt = include_str!("../../../lov/prefixes/owl.ttl");

        let url = lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let (output, _) = parse_turtle(txt, &url).expect("Simple collection");
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
        rdfs:comment                   "\r\n  This ontology partially describes the built-in " ; .
            "#;

        let url = lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let (output, _) = parse_turtle(txt, &url).expect("Simple collection");
        output.get_simple_triples().expect("Triples found");
    }
}
