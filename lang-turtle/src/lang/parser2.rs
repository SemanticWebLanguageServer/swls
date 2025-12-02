use std::ops::Range;

use lsp_core::{prelude::Token, util::Spanned};
use lsp_core::lsp_types::Url;

use super::{model::*, tokenizer::parse_tokens_str};

struct Ctx<'a> {
    last_commit: usize,
    tokens: &'a [Spanned<Token>],
    idx: usize,
}

#[allow(unused)]
impl<'a> Ctx<'a> {
    pub fn new(tokens: &'a [Spanned<Token>]) -> Self {
        Self {
            last_commit: 0,
            idx: 0,
            tokens,
        }
    }

    fn finished(&self) -> Option<()> {
        if self.idx == self.tokens.len() {
            return None;
        }
        Some(())
    }
    fn current(&mut self) -> Option<(&Spanned<Token>, usize)> {
        self.skip_comments();
        self.finished()?;

        Some((&self.tokens[self.idx], self.idx))
    }

    fn skip_comments(&mut self) {
        while self.idx < self.tokens.len() && self.tokens[self.idx].is_comment() {
            self.idx += 1;
        }
    }

    fn parse_token<T>(&mut self, token: Token, t: T) -> Option<Spanned<T>> {
        let (c, idx) = self.current()?;
        if c.value() == &token {
            let span = self.tokens[self.idx].span().clone();
            self.inc();
            return Some(Spanned(t, span));
        }
        None
    }

    fn or<T>(&mut self, xs: &[fn(&mut Self) -> Option<T>]) -> Option<T> {
        let at = self.at();
        for &x in xs {
            if let Some(t) = x(self) {
                return Some(t);
            }
            self.reset(at);
        }
        None
    }

    fn or_state<T, C>(
        &mut self,
        xs: &[fn(&mut Self, &mut C) -> Option<T>],
        ctx: &mut C,
    ) -> Option<T> {
        let at = self.at();
        for &x in xs {
            if let Some(t) = x(self, ctx) {
                return Some(t);
            }
            self.reset(at);
        }
        None
    }

    fn commit(&mut self) {
        self.last_commit = self.idx;
    }

    fn at(&self) -> usize {
        self.idx
    }

    fn reset(&mut self, at: usize) {
        self.idx = at;
    }

    fn span(&self, start: usize, end: usize) -> Range<usize> {
        let start = self.tokens[start].span().start;
        let end = self.tokens[end - 1].span().end;
        start..end
    }
    fn spanned<T>(&self, item: T) -> Spanned<T> {
        Spanned(item, self.tokens[self.idx].span().clone())
    }
    fn inc(&mut self) {
        self.idx += 1;
    }

    fn parse_named_node(&mut self) -> Option<Spanned<Term>> {
        self.finished()?;
        let (c, idx) = self.current()?;

        if c.is_i_r_i_ref() {
            let o =
                c.map_ref(|t| Term::NamedNode(NamedNode::Full(t.clone().into_i_r_i_ref(), idx)));
            self.inc();
            return Some(o);
        }

        if c.is_p_name_l_n() {
            let o = c.map_ref(|t| {
                let (prefix, value) = t.clone().into_p_name_l_n();
                Term::NamedNode(NamedNode::Prefixed {
                    prefix: prefix.unwrap_or_default(),
                    value,
                    idx,
                })
            });
            self.inc();
            return Some(o);
        }

        None
    }

    fn parse_literal(&mut self) -> Option<Spanned<Term>> {
        self.finished()?;
        let start = self.at();
        let (c, idx) = self.current()?;

        if c.is_str() {
            let (value, style) = c.value().clone().into_str();
            let mut literal = RDFLiteral {
                value,
                quote_style: style,
                lang: None,
                ty: None,
                idx,
                len: 1,
            };
            self.inc();

            if let Some((c, _)) = self.current() {
                match c.value() {
                    Token::LangTag(tag) => {
                        literal.lang = Some(tag.clone());
                        literal.len += 1;
                        self.inc();
                    }

                    Token::DataTypeDelim => {
                        self.inc();
                        if let Term::NamedNode(nn) = self.parse_named_node()?.into_value() {
                            literal.ty = Some(nn);
                            literal.len += 2;
                        } else {
                            return None;
                        }
                    }
                    _ => {}
                }
            }

            let end = self.at();
            let span = self.span(start, end);

            let o = Spanned(Term::Literal(Literal::RDF(literal)), span);

            return Some(o);
        }

        if c.is_number() {
            let o = c.map_ref(|t| {
                let v = t.clone().into_number();
                Term::Literal(Literal::Numeric(v))
            });
            self.inc();
            return Some(o);
        }

        self.parse_token(Token::True, Term::Literal(Literal::Boolean(true)))
            .or_else(|| self.parse_token(Token::False, Term::Literal(Literal::Boolean(false))))
    }

    fn parse_unnamed_bn(&mut self) -> Option<Spanned<Term>> {
        let start = self.at();

        self.parse_token(Token::SqOpen, ())?;
        let po = self.parse_pos();
        self.parse_token(Token::SqClose, ())?;

        let end = self.at();
        let span = self.span(start, end);

        Some(Spanned(
            Term::BlankNode(BlankNode::Unnamed(po, start, end - 1)),
            span,
        ))
    }

    fn parse_blank_node(&mut self) -> Option<Spanned<Term>> {
        self.finished()?;
        let (c, idx) = self.current()?;
        if c.is_blank_node_label() {
            let o = c.map_ref(|t| {
                Term::BlankNode(BlankNode::Named(t.clone().into_blank_node_label(), idx))
            });
            self.inc();
            return Some(o);
        }

        if c.is_sq_open() {
            return self.parse_unnamed_bn();
        }

        if c.is_a_n_o_n() {
            let o = c.map_ref(|t| Term::BlankNode(BlankNode::Unnamed(Vec::new(), idx, idx)));
            self.inc();
            return Some(o);
        }

        None
    }

    fn parse_collection(&mut self) -> Option<Spanned<Term>> {
        let start = self.at();
        self.parse_token(Token::BracketOpen, ())?;

        let mut terms = Vec::new();
        while let Some(term) = self.parse_object() {
            terms.push(term);
        }

        self.parse_token(Token::BracketClose, ())?;
        let end = self.at();
        let span = self.span(start, end);

        Some(Spanned(Term::Collection(terms), span))
    }

    fn parse_variable(&mut self) -> Option<Spanned<Term>> {
        let (c, idx) = self.current()?;
        if c.is_variable() {
            let o = c.map_ref(|t| Term::Variable(Variable(t.clone().into_variable(), idx)));
            self.inc();
            return Some(o);
        }
        None
    }

    fn parse_subject(&mut self) -> Option<Spanned<Term>> {
        self.or(&[
            Self::parse_named_node,
            Self::parse_blank_node,
            Self::parse_collection,
            Self::parse_variable,
        ])
    }

    fn parse_predtype(&mut self) -> Option<Spanned<Term>> {
        let (c, idx) = self.current()?;
        if c.is_pred_type() {
            let o = c.map_ref(|t| Term::NamedNode(NamedNode::A(idx)));
            self.inc();
            return Some(o);
        }
        None
    }

    fn parse_predicate(&mut self) -> Option<Spanned<Term>> {
        self.or(&[
            Self::parse_predtype,
            Self::parse_named_node,
            Self::parse_variable,
        ])
    }

    fn parse_object(&mut self) -> Option<Spanned<Term>> {
        self.or(&[
            Self::parse_named_node,
            Self::parse_blank_node,
            Self::parse_literal,
            Self::parse_collection,
            Self::parse_variable,
        ])
    }

    fn parse_po(&mut self) -> Option<Spanned<PO>> {
        let start = self.at();
        let predicate = self.parse_predicate()?;
        let mut object = Vec::new();

        loop {
            if let Some(o) = self.parse_object() {
                object.push(o);
            }

            if self.parse_token(Token::Comma, ()).is_none() {
                break;
            }
        }
        let end = self.at();
        let span = self.span(start, end);

        Some(Spanned(PO { predicate, object }, span))
    }

    fn parse_pos(&mut self) -> Vec<Spanned<PO>> {
        let mut po = Vec::new();
        loop {
            if let Some(o) = self.parse_po() {
                po.push(o);
            }
            let mut split = false;

            while self.parse_token(Token::PredicateSplit, ()).is_some() {
                split = true;
            }
            if !split {
                return po;
            }
        }

        po
    }

    fn parse_triple(&mut self) -> Option<Spanned<Triple>> {
        let start = self.at();
        let subject = self.parse_subject()?;

        let po = self.parse_pos();
        if po.is_empty() {
            return None;
        }

        self.parse_token(Token::Stop, ())?;

        let end = self.at();
        let span = self.span(start, end);

        Some(Spanned(Triple { subject, po }, span))
    }

    fn bn_triple(&mut self) -> Option<Spanned<Triple>> {
        let start = self.at();
        let subject = self.parse_unnamed_bn()?;

        let po = Vec::new();

        self.parse_token(Token::Stop, ())?;

        let end = self.at();
        let span = self.span(start, end);

        Some(Spanned(Triple { subject, po }, span))
    }

    fn parse_base(&mut self) -> Option<Spanned<Base>> {
        let start = self.at();
        let (c, idx) = self.current()?;

        let needs_stop = match c.value() {
            Token::SparqlBase => false,
            Token::BaseTag => true,
            _ => return None,
        };
        let kwd_span = c.span().clone();
        self.inc();

        let nn = if let Some(Spanned(Term::NamedNode(nn), span)) = self.parse_named_node() {
            Spanned(nn, span)
        } else {
            return None;
        };

        if needs_stop {
            self.parse_token(Token::Stop, ())?;
        }

        let end = self.at();
        let span = self.span(start, end);

        Some(Spanned(Base(kwd_span, nn), span))
    }

    fn parse_prefix(&mut self) -> Option<Spanned<TurtlePrefix>> {
        let start = self.at();
        let (c, idx) = self.current()?;

        let needs_stop = match c.value() {
            Token::SparqlPrefix => false,
            Token::PrefixTag => true,
            _ => return None,
        };
        let kwd_span = c.span().clone();
        self.inc();

        let (c, idx) = self.current()?;
        let prefix = match c.value() {
            Token::PNameLN(x, _) => {
                let v = x.clone().unwrap_or_default();
                Spanned(v, c.span().clone())
            }
            _ => return None,
        };
        self.inc();

        let nn = if let Some(Spanned(Term::NamedNode(nn), span)) = self.parse_named_node() {
            Spanned(nn, span)
        } else {
            return None;
        };

        if needs_stop {
            self.parse_token(Token::Stop, ())?;
        }

        let end = self.at();
        let span = self.span(start, end);

        Some(Spanned(
            TurtlePrefix {
                span: kwd_span,
                prefix,
                value: nn,
            },
            span,
        ))
    }

    fn add_triple(&mut self, turtle: &mut Turtle) -> Option<()> {
        let triple = self.or(&[Self::parse_triple, Self::bn_triple])?;
        turtle.triples.push(triple);
        Some(())
    }

    fn add_base(&mut self, turtle: &mut Turtle) -> Option<()> {
        let base = self.parse_base()?;
        turtle.base = Some(base);
        Some(())
    }

    fn add_prefix(&mut self, turtle: &mut Turtle) -> Option<()> {
        let prefix = self.parse_prefix()?;
        turtle.prefixes.push(prefix);
        Some(())
    }

    fn parse_turtle(&mut self, turtle: &mut Turtle) {
        while self
            .or_state(
                &[Self::add_prefix, Self::add_base, Self::add_triple],
                turtle,
            )
            .is_some()
        {}
    }

    fn parse_turtle_retry(&mut self, turtle: &mut Turtle) -> Vec<Range<usize>> {
        let mut failed = Vec::new();
        while self.idx < self.tokens.len() {
            // Try to add something to turtle
            while self
                .or_state(
                    &[Self::add_prefix, Self::add_base, Self::add_triple],
                    turtle,
                )
                .is_some()
            {}

            if self.current().is_none() {
                break;
            }

            let sdx = self.at();
            while self.idx < self.tokens.len() && self.tokens[self.idx].value() != &Token::Stop {
                self.idx += 1;
            }
            self.idx += 1;
            failed.push(sdx..self.at());

            self.idx += 1;
        }
        failed
    }
}

pub fn parse_source(url: &Url, source: &str) -> Option<Turtle> {
    let mut output = Turtle::empty(url);
    let tokens = parse_tokens_str(source).0;
    let mut ctx = Ctx::new(&tokens);
    ctx.parse_turtle(&mut output);
    if ctx.current().is_none() {
        Some(output)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::Ctx;
    use crate::lang::{
        model::{Literal, Turtle},
        tokenizer::parse_tokens_str,
    };

    #[test]
    fn parse_triple_1() {
        let text = "<a> <b> <c> .";
        let tokens = parse_tokens_str(text).0;
        let triple = Ctx::new(&tokens).parse_triple();

        assert!(triple.is_some());
    }

    #[test]
    fn parse_triple_2() {
        let text = "[ ] <b> <c> .";
        let tokens = parse_tokens_str(text).0;
        let triple = Ctx::new(&tokens).parse_triple();

        assert!(triple.is_some());
    }

    #[test]
    fn parse_triple_3() {
        let text = "[ <a> [] ] <b> <c> .";
        let tokens = parse_tokens_str(text).0;
        let triple = Ctx::new(&tokens).parse_triple();

        assert!(triple.is_some());
    }

    #[test]
    fn parse_triple_4() {
        let text = "[ <a> [] ; ] <b> <c> ; .";
        let tokens = parse_tokens_str(text).0;
        let triple = Ctx::new(&tokens).parse_triple();

        assert!(triple.is_some());
    }

    #[test]
    fn parse_triple_5() {
        let text = "<a> <b> 'tetten'^^xsd:string .";
        let tokens = parse_tokens_str(text).0;
        let triple = Ctx::new(&tokens).parse_triple();

        assert!(triple.is_some());

        match triple.unwrap().po[0].object[0].value() {
            crate::lang::model::Term::Literal(Literal::RDF(literal)) => {
                assert!(literal.ty.is_some())
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn parse_triple_wrong_1() {
        let text = "a <b> <c> .";
        let tokens = parse_tokens_str(text).0;
        let triple = Ctx::new(&tokens).parse_triple();

        assert!(triple.is_none());
    }

    #[test]
    fn parse_triple_wrong_2() {
        let text = "a <b> 'tetten'^^ .";
        let tokens = parse_tokens_str(text).0;
        let triple = Ctx::new(&tokens).parse_triple();

        assert!(triple.is_none());
    }

    #[test]
    fn parse_turtle_1() {
        let txt = r#"
        @base <>. #This is a very nice comment!
#This is a very nice comment!
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
<a> <b> <c>.
#This is a very nice comment!
            "#;
        let url = lsp_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let mut output = Turtle::empty(&url);
        let tokens = parse_tokens_str(txt).0;
        Ctx::new(&tokens).parse_turtle(&mut output);
        assert_eq!(output.prefixes.len(), 1, "prefixes are parsed");
        assert_eq!(output.triples.len(), 1, "triples are parsed");
        assert!(output.base.is_some(), "base is parsed");
    }

    #[test]
    fn parse_turtle_2() {
        let txt = r#"
<a> <b> <c>.
#This is a very nice comment!
            "#;
        let url = lsp_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let mut output = Turtle::empty(&url);
        let tokens = parse_tokens_str(txt).0;
        Ctx::new(&tokens).parse_turtle(&mut output);
        assert_eq!(output.prefixes.len(), 0, "prefixes are parsed");
        assert_eq!(output.triples.len(), 1, "triples are parsed");
        assert!(output.base.is_none(), "base is not parsed");
    }

    #[test]
    fn parse_turtle_3() {
        let txt = r#"
<a> <b> <c>.
<d> <e> <f> <g>.
<h> <i> <j>.
#This is a very nice comment!
            "#;

        let url = lsp_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let mut output = Turtle::empty(&url);
        let tokens = parse_tokens_str(txt).0;
        let failed = Ctx::new(&tokens).parse_turtle_retry(&mut output);
        for fail in &failed {
            println!("Fail {:?}", fail);
            for t in &tokens[fail.clone()] {
                print!("{:?} {} - ", t.value(), &txt[t.span().clone()]);
            }
            println!("");
        }
        assert_eq!(output.prefixes.len(), 0, "prefixes are parsed");
        assert_eq!(output.triples.len(), 2, "triples are parsed");
        assert!(output.base.is_none(), "base is not parsed");
        assert_eq!(failed.len(), 1, "one mistake");
    }
}
