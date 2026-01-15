use chumsky::prelude::*;
use lsp_core::{prelude::*, util::token::PToken};
use tracing::info;

use super::context::{ContextKind, Ctx};
use crate::lang::model::{
    Base, BlankNode, Literal, NamedNode, RDFLiteral, Term, Triple, Turtle, TurtlePrefix, Variable,
    PO,
};

type S = std::ops::Range<usize>;

// pub fn just(token: Token) -> impl Parser<PToken, Token, Error = Simple<PToken, S>> + Clone {
//     let t2 = token.clone();
//     select! {
//         PToken(t, _) if t == t2 => {
//             t
//         },
//     }
//     .or(any::<PToken>()
//         .rewind()
//         .or(empty())
//         .try_map(move |t, span| {
//             Err(Simple::<PToken, S>::expected_input_found(
//                 span,
//                 [Some(PToken(token.clone(), 0))],
//                 t,
//             ))
//         }))
//     // any()
//     //     .map(|PToken(t, _)| t)
//     //     .or_not()
//     //     .try_map(move |t, span| match t {
//     //         Some(t) if t == token => Ok(t),
//     //         _ => {
//     //             println!("Not found Token {:?} found {:?}", token, t);
//     //             Err(Simple::<PToken, S>::expected_input_found(
//     //                 span,
//     //                 [Some(PToken(token.clone(), 0))],
//     //                 t.map(|t| PToken(t, 0)),
//     //             ))
//     //         }
//     //     })
//     // filter(move |PToken(ref t, _)| t == &token).map(|t| t.0)
// }
//
pub fn not(token: Token) -> impl Parser<PToken, PToken, Error = Simple<PToken, S>> + Clone {
    filter(move |PToken(ref t, _)| t != &token)
}

pub fn one_of<const C: usize>(
    tokens: [Token; C],
) -> impl Parser<PToken, PToken, Error = Simple<PToken, S>> + Clone {
    filter(move |PToken(ref t, _)| tokens.iter().any(|t2| t == t2))
}

#[derive(Clone)]
pub enum LiteralHelper {
    LangTag(String),
    DataType(NamedNode),
    None,
}

impl LiteralHelper {
    pub fn to_lit(self, (value, quote_style): (String, StringStyle), idx: usize) -> RDFLiteral {
        match self {
            LiteralHelper::LangTag(lang) => RDFLiteral {
                value,
                quote_style,
                lang: Some(lang),
                ty: None,
                idx,
                len: 2,
            },
            LiteralHelper::DataType(dt) => RDFLiteral {
                value,
                quote_style,
                lang: None,
                ty: Some(dt),
                idx,
                len: 3,
            },
            LiteralHelper::None => RDFLiteral {
                value,
                quote_style,
                lang: None,
                ty: None,
                idx,
                len: 1,
            },
        }
    }
}

fn literal() -> impl Parser<PToken, Literal, Error = Simple<PToken, S>> + Clone {
    let lt = select! { PToken(Token::LangTag(x), _) => LiteralHelper::LangTag(x)};

    let dt = just(PToken(Token::DataTypeDelim, 0))
        .ignore_then(named_node())
        .map(|x| LiteralHelper::DataType(x));

    let literal = select! {
        PToken(Token::Str(x, style), idx) => (x, style, idx)
    }
    .then(lt.or(dt).or(empty().to(LiteralHelper::None)))
    .map(|((x, y, idx), h)| Literal::RDF(h.to_lit((x, y), idx)));

    literal.or(select! {
        PToken(Token::Number(x), _) => Literal::Numeric(x),
        PToken(Token::True, _) => Literal::Boolean(true),
        PToken(Token::False, _) => Literal::Boolean(false),
    })
}

pub fn named_node() -> impl Parser<PToken, NamedNode, Error = Simple<PToken, S>> + Clone {
    let invalid = select! {
        PToken(Token::Invalid(_), _) => NamedNode::Invalid,
    }
    .validate(move |x, span: S, emit| {
        emit(Simple::custom(
            span.end..span.end,
            format!("Expected a named node."),
        ));
        x
    });

    select! {
        PToken(Token::PredType, idx) => NamedNode::A(idx),
        PToken(Token::IRIRef(x), idx) => NamedNode::Full(x, idx),
        PToken(Token::PNameLN(x, b), idx) => NamedNode::Prefixed { prefix: x.unwrap_or_default(), value: b, idx },
    }
    .or(invalid)
}

pub fn is_term_like(token: &Token) -> bool {
    match token {
        // Token::True => true,
        // Token::False => true,
        // Token::IRIRef(_) => true,
        // Token::PNameLN(_, _) => true,
        // Token::BlankNodeLabel(_) => true,
        // Token::Number(_) => true,
        // Token::Str(_, _) => true,
        // Token::ANON => true,
        // Token::Null => true,
        Token::Invalid(_) => true,
        // Token::Variable(_) => true,
        // Token::PrefixTag => true,
        // Token::BaseTag => true,
        // Token::SparqlPrefix => true,
        // Token::SparqlBase => true,
        _ => false,
    }
}

pub fn expect_token(
    token: Token,
    valid: impl Fn(Option<&PToken>) -> bool + Clone,
) -> impl Parser<PToken, Token, Error = Simple<PToken, S>> + Clone {
    let inner_token = token.clone();
    just::<PToken, _, _>(token.clone().into())
        .map(|PToken(t, _)| t)
        .or(not(token.clone())
            .map(|x| Some(x))
            .or(end().map(|_| None))
            .rewind()
            .try_map(move |t, span| {
                if valid(t.as_ref()) {
                    Ok(t)
                } else {
                    Err(Simple::expected_input_found(
                        span,
                        [Some(PToken(inner_token.clone(), 0))],
                        t.map(|x| x.into()),
                    ))
                }
            })
            .validate(move |_, span: S, emit| {
                emit(Simple::expected_input_found(
                    span,
                    [Some(PToken(token.clone(), 0))],
                    None,
                ));
                token.clone()
            }))
}

fn blank_node<'a>(
    ctx: Ctx<'a>,
) -> impl Parser<PToken, BlankNode, Error = Simple<PToken>> + Clone + use<'a> + 'a {
    recursive(|bn| {
        let start = select! {
            PToken(Token::SqOpen, idx) => idx
        };

        let end = select! {
            PToken(Token::SqClose, idx) => idx
        }
        .recover_with(skip_parser(empty().map(|_| 0)));

        start
            .then(po_list(bn, ctx))
            .then(end)
            .map(|((end, x), start)| BlankNode::Unnamed(x, start, end))
            .or(select! {
                PToken(Token::BlankNodeLabel(x), idx) => BlankNode::Named(x, idx),
            })
    })
}

fn subject<'a>(
    ctx: Ctx<'a>,
) -> impl Parser<PToken, Term, Error = Simple<PToken, S>> + Clone + use<'a> + 'a {
    term(blank_node(ctx), ctx, [])
    // let nn = named_node().map(|x| Term::NamedNode(x));
    // let bn = blank_node().map(|x| Term::BlankNode(x));
    // let var = variable().map(|x| Term::Variable(x));
    //
    // nn.or(bn).or(var)
}

fn variable() -> impl Parser<PToken, Variable, Error = Simple<PToken, S>> + Clone {
    select! {
        PToken(Token::Variable(x), idx) => Variable(x, idx),
    }
}

fn term<'a, const C: usize, T: 'a + Clone + Parser<PToken, BlankNode, Error = Simple<PToken>>>(
    bn: T,
    ctx: Ctx<'a>,
    not: [ContextKind; C],
) -> impl Parser<PToken, Term, Error = Simple<PToken>> + Clone + use<'a, C, T> {
    let ctx = ctx;
    select! {
        PToken(_, idx) => idx
    }
    .try_map(move |idx, span: S| {
        let mut err: Option<Simple<PToken>> = None;
        for kind in not {
            if ctx.was(idx, kind) {
                let new_error = Simple::custom(
                    span.clone(),
                    format!("Didn't expect {} here, found {:?}", kind, ctx.find_was(idx)),
                );
                info!("No term emitted {:?}", new_error);
                if let Some(old) = err.take() {
                    err = Some(old.merge(new_error));
                } else {
                    err = Some(new_error);
                }
            }
        }

        if let Some(e) = err {
            Err(e)
        } else {
            Ok(())
        }
    })
    .rewind()
    // .recover_with(skip_parser(empty().map(|_| Term::Invalid)))
    .ignore_then(recursive(|term| {
        let collection = term
            .map_with_span(spanned)
            .repeated()
            .delimited_by(
                just(PToken(Token::BracketOpen, 0)),
                just(PToken(Token::BracketClose, 0)),
            )
            .map(|x| Term::Collection(x));

        let nn = named_node().map(|x| Term::NamedNode(x));
        let blank = bn.map(|x| Term::BlankNode(x));
        let literal = literal().map(|x| Term::Literal(x));
        let variable = variable().map(|x| Term::Variable(x));

        choice((collection, literal, nn, blank, variable))
    }))
}

fn po<'a, T: Clone + Parser<PToken, BlankNode, Error = Simple<PToken>> + 'a>(
    bn: T,
    ctx: Ctx<'a>,
) -> impl Parser<PToken, PO, Error = Simple<PToken>> + Clone + use<'a, T> {
    term(bn.clone(), ctx, [ContextKind::Subject])
        .labelled("predicate")
        .map_with_span(spanned)
        .then(
            term(
                bn.clone(),
                ctx,
                [ContextKind::Subject, ContextKind::Predicate],
            )
            .recover_with(skip_parser(empty().map(|_| Term::Invalid)))
            .labelled("object")
            .map_with_span(spanned)
            .separated_by(just(PToken(Token::Comma, 0)).labelled("comma")), // .at_least(1),
        )
        .map(|(predicate, object)| PO { predicate, object })
}

fn po_list<'a, T: Clone + Parser<PToken, BlankNode, Error = Simple<PToken>> + 'a>(
    bn: T,
    ctx: Ctx<'a>,
) -> impl Parser<PToken, Vec<Spanned<PO>>, Error = Simple<PToken>> + Clone + use<'a, T> {
    po(bn, ctx)
        .labelled("po")
        .map_with_span(spanned)
        .separated_by(
            expect_token(Token::PredicateSplit, move |t| {
                t.map(|t| ctx.was_predicate(t.1) || t.0.is_invalid())
                    .unwrap_or_default()
            })
            .ignore_then(just([Token::PredicateSplit.into()]).repeated()),
        )
        .allow_trailing()
}

fn po_list_recovery<'a>(
    ctx: Ctx<'a>,
) -> impl Parser<PToken, Vec<Spanned<PO>>, Error = Simple<PToken>> + Clone + use<'a> {
    po_list(blank_node(ctx), ctx).recover_with(skip_parser(empty().map_with_span(|_, span: S| {
        vec![spanned(
            PO {
                predicate: spanned(Term::Invalid, span.clone()),
                object: vec![spanned(Term::Invalid, span.clone())],
            },
            span,
        )]
    })))
}

fn bn_triple(
    ctx: Ctx<'_>,
) -> impl Parser<PToken, Triple, Error = Simple<PToken>> + Clone + use<'_> {
    let pos = po_list_recovery(ctx).validate(|po, span, emit| {
        if po.is_empty() {
            emit(Simple::custom(
                span.clone(),
                format!("Expected at least one predicate object."),
            ));
            vec![spanned(
                PO {
                    predicate: spanned(Term::Invalid, span.clone()),
                    object: vec![spanned(Term::Invalid, span.clone())],
                },
                span,
            )]
        } else {
            po
        }
    });
    just([Token::SqOpen.into()])
        .ignore_then(pos)
        .then_ignore(just([Token::SqClose.into()]))
        .then_ignore(expect_token(Token::Stop, |_| true))
        .map_with_span(|pos, span| Triple {
            subject: spanned(Term::BlankNode(BlankNode::Unnamed(pos, 0, 0)), span),
            po: Vec::new(),
        })
}

pub fn triple(
    ctx: Ctx<'_>,
) -> impl Parser<PToken, Triple, Error = Simple<PToken>> + Clone + use<'_> {
    let pos = po_list_recovery(ctx)
        .validate(|po, span, emit| {
            if po.is_empty() {
                emit(Simple::custom(
                    span.clone(),
                    format!("Expected at least one predicate object."),
                ));
                vec![spanned(
                    PO {
                        predicate: spanned(Term::Invalid, span.clone()),
                        object: vec![spanned(Term::Invalid, span.clone())],
                    },
                    span,
                )]
            } else {
                po
            }
        })
        .labelled("po_list");

    subject(ctx)
        .labelled("subject")
        .map_with_span(spanned)
        .then(pos)
        .then_ignore(expect_token(Token::Stop, |_| true))
        .map(|(subject, po)| Triple { subject, po })
        .validate(|this: Triple, _, emit| {
            for po in &this.po {
                if !po.predicate.is_predicate() {
                    emit(Simple::custom(
                        po.predicate.span().clone(),
                        "predicate should be a named node",
                    ));
                }

                for o in &po.object {
                    if !o.is_object() {
                        emit(Simple::custom(
                            o.span().clone(),
                            "object should be an object",
                        ));
                    }
                }
            }

            if !this.subject.is_subject() {
                emit(Simple::custom(
                    this.subject.span().clone(),
                    "subject should be a subject",
                ));
            }

            this
        })
        .or(bn_triple(ctx))

    // expect_token(Token::Stop, |_| true)
    //     .ignore_then(po_list())
    //     .then_with(move |(po, succesful)| {
    //         let po2 = po.clone();
    //         let basic_subj = subject()
    //             .map_with_span(spanned)
    //             .map(move |subj| (po2.clone(), subj));
    //
    //         let end = po[0].span().end;
    //         let start = if succesful {
    //             empty().boxed()
    //         } else {
    //             any().map(|_| ()).boxed()
    //         };
    //         let alt_subj = start.validate(move |_, _: S, emit| {
    //             emit(Simple::custom(end..end, format!("Expected a predicate.")));
    //
    //             let mut po = po.clone();
    //             let first = po[0].value_mut();
    //
    //             let subj = first.predicate.clone();
    //             first.predicate = first.object.pop().unwrap();
    //
    //             first.object.push(Spanned(
    //                 Term::NamedNode(NamedNode::Invalid),
    //                 first.predicate.span().clone(),
    //             ));
    //
    //             // Subject::NamedNode(NamedNode::Invalid)
    //             (po, subj)
    //         });
    //
    //         basic_subj.or(alt_subj)
    //     })
    //     .map(|(po, subject)| Triple { subject, po })
    //     .validate(|this: Triple, _, emit| {
    //         for po in &this.po {
    //             if !po.predicate.is_predicate() {
    //                 emit(Simple::custom(
    //                     po.predicate.span().clone(),
    //                     "predicate should be a named node",
    //                 ));
    //             }
    //
    //             for o in &po.object {
    //                 if !o.is_object() {
    //                     emit(Simple::custom(
    //                         o.span().clone(),
    //                         "object should be an object",
    //                     ));
    //                 }
    //             }
    //         }
    //
    //         if !this.subject.is_subject() {
    //             emit(Simple::custom(
    //                 this.subject.span().clone(),
    //                 "subject should be a subject",
    //             ));
    //         }
    //
    //         this
    //     })
    //     .or(bn_triple())
}

fn base() -> impl Parser<PToken, Base, Error = Simple<PToken>> + Clone {
    let turtle_base = just([Token::BaseTag.into()])
        .map_with_span(|_, s| s)
        .then(named_node().map_with_span(spanned))
        .then_ignore(expect_token(Token::Stop, |_| true))
        .map(|(s, x)| Base(s, x));

    let sparql_base = just([Token::SparqlBase.into()])
        .map_with_span(|_, s| s)
        .then(named_node().map_with_span(spanned))
        .map(|(s, x)| Base(s, x));

    turtle_base.or(sparql_base)
}

fn prefix() -> impl Parser<PToken, TurtlePrefix, Error = Simple<PToken>> {
    let turtle_prefix = just([Token::PrefixTag.into()])
        .map_with_span(|_, s| s)
        .then(select! { |span| PToken(Token::PNameLN(x, _), _) => Spanned(x.unwrap_or_default(), span)})
        .then(named_node().map_with_span(spanned))
        .then_ignore(expect_token(Token::Stop, |_| true))
        .map(|((span, prefix), value)| TurtlePrefix {
            span,
            prefix,
            value,
        });
    let sparql_prefix = just([Token::SparqlPrefix.into()])

        .map_with_span(|_, s| s)
        .then(select! { |span| PToken(Token::PNameLN(x, _), _) => Spanned(x.unwrap_or_default(), span)})
        .then(named_node().map_with_span(spanned))
        .map(|((span, prefix), value)| TurtlePrefix {
            span,
            prefix,
            value,
        });

    turtle_prefix.or(sparql_prefix)
}

// Makes it easier to handle parts that are not ordered
enum Statement {
    Base(Spanned<Base>),
    Prefix(Spanned<TurtlePrefix>),
    Triple(Spanned<Triple>),
}

pub fn turtle<'a>(
    location: &'a lsp_core::lsp_types::Url,
    ctx: Ctx<'a>,
) -> impl Parser<PToken, Turtle, Error = Simple<PToken>> + 'a {
    let base = base().map_with_span(spanned).map(|b| Statement::Base(b));
    let prefix = prefix()
        .map_with_span(spanned)
        .map(|b| Statement::Prefix(b));
    let triple = triple(ctx)
        .map_with_span(spanned)
        .map(|b| Statement::Triple(b));

    let statement = base.or(prefix).or(triple);
    statement
        .repeated()
        .map(|statements| {
            let mut base = None;
            let mut prefixes = Vec::new();
            let mut triples = Vec::new();
            for statement in statements {
                match statement {
                    Statement::Base(b) => base = Some(b),
                    Statement::Prefix(p) => prefixes.push(p),
                    Statement::Triple(t) => triples.push(t),
                }
            }

            Turtle::new(base, prefixes, triples, location)
        })
        .then_ignore(end())
}

pub fn parse_turtle(
    location: &lsp_core::lsp_types::Url,
    tokens: Vec<Spanned<Token>>,
    len: usize,
    ctx: Ctx<'_>,
) -> (Spanned<Turtle>, Vec<Simple<PToken>>) {
    let stream = chumsky::Stream::from_iter(
        0..len,
        tokens
            .into_iter()
            .enumerate()
            .filter(|(_, x)| !x.is_comment())
            .map(|(i, t)| t.map(|x| PToken(x, i)))
            // .rev()
            .map(|Spanned(x, s)| (x, s)),
    );

    let parser = turtle(location, ctx)
        .map_with_span(spanned)
        .then_ignore(end().recover_with(skip_then_retry_until([])));

    info!("Parsing {}", location.as_str());
    let (json, json_errors) = parser.parse_recovery(stream);

    // let json_errors: Vec<_> = json_errors.into_iter().map(|error| (len, error)).collect();

    (
        json.unwrap_or(Spanned(Turtle::empty(location), 0..len)),
        json_errors,
    )
}

#[cfg(test)]
pub mod turtle_tests {
    use std::str::FromStr;

    use chumsky::{prelude::Simple, Parser, Stream};
    use lsp_core::prelude::{PToken, Spanned};

    use super::literal;
    use crate::lang::{
        context::{Context, TokenIdx},
        parser::{blank_node, named_node, prefix, triple, turtle, BlankNode},
        tokenizer::{parse_tokens_str, parse_tokens_str_safe},
    };

    pub fn parse_it<T, P: Parser<PToken, T, Error = Simple<PToken>>>(
        turtle: &str,
        parser: P,
    ) -> (Option<T>, Vec<Simple<PToken>>) {
        let tokens = parse_tokens_str_safe(turtle).unwrap();
        let end = turtle.len()..turtle.len();
        let stream = Stream::from_iter(
            end,
            tokens
                .into_iter()
                .enumerate()
                .filter(|(_, x)| !x.is_comment())
                .map(|(i, t)| t.map(|x| PToken(x, i)))
                .map(|Spanned(x, y)| (x, y)), // .rev(),
        );

        parser.parse_recovery(stream)
    }

    pub fn parse_it_recovery<T, P: Parser<PToken, T, Error = Simple<PToken>>>(
        turtle: &str,
        parser: P,
    ) -> (Option<T>, Vec<Simple<PToken>>) {
        let (tokens, _) = parse_tokens_str(turtle);
        let end = turtle.len()..turtle.len();
        let stream = Stream::from_iter(
            end,
            tokens
                .into_iter()
                .enumerate()
                .filter(|(_, x)| !x.is_comment())
                .map(|(i, t)| t.map(|x| PToken(x, i)))
                .map(|Spanned(x, y)| (x, y)), // .rev(),
        );

        parser.parse_recovery(stream)
    }

    #[test]
    fn parse_literal() {
        let turtle = "42";
        let output = parse_it(turtle, literal()).0.expect("number");
        assert_eq!(output.to_string(), "42");

        let turtle = "\"42\"@en";
        let output = parse_it(turtle, literal()).0.expect("lang string");
        assert_eq!(output.to_string(), turtle);

        let turtle = "\"42\"^^xsd:int";
        let output = parse_it(turtle, literal()).0.expect("double quotes");
        assert_eq!(output.to_string(), turtle);

        let turtle = "\'42\'";
        let output = parse_it(turtle, literal()).0.expect("single quotes");
        assert_eq!(output.to_string(), turtle);
        let turtle = "\"\"\"42\"\"\"";
        let output = parse_it(turtle, literal()).0.expect("long double quotes");
        assert_eq!(output.to_string(), turtle);

        let turtle = "\'\'\'42\'\'\'";
        let output = parse_it(turtle, literal()).0.expect("long single quotes");
        assert_eq!(output.to_string(), turtle);
    }

    #[test]
    fn parse_prefix() {
        let turtle = "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .";
        let output = parse_it(turtle, prefix()).0.expect("Simple");
        assert_eq!(
            output.to_string(),
            "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> ."
        );
    }

    #[test]
    fn parse_namednode() {
        let turtle = "<abc>";
        let output = parse_it(turtle, named_node()).0.expect("Simple");
        assert_eq!(output.to_string(), "<abc>");

        let turtle = "a";
        let output = parse_it(turtle, named_node()).0.expect("a");
        assert_eq!(output.to_string(), "a");

        let turtle = ":";
        let output = parse_it(turtle, named_node()).0.expect(":");
        assert_eq!(output.to_string(), ":");

        let turtle = "foo:bar";
        let output = parse_it(turtle, named_node()).0.expect("foo:bar");
        assert_eq!(output.to_string(), "foo:bar");
    }

    #[test]
    fn parse_blanknode() {
        let turtle = "[]";
        let ctx = Context::new();
        let output = parse_it(turtle, blank_node(ctx.ctx())).0.expect("anon");
        let is_unamed = match output {
            BlankNode::Unnamed(_, _, _) => true,
            _ => false,
        };
        assert!(is_unamed);

        let turtle = "_:foobar";
        let output = parse_it(turtle, blank_node(ctx.ctx())).0.expect("other bn");
        let is_named = match output {
            BlankNode::Named(_, _) => true,
            _ => false,
        };
        assert!(is_named);
    }

    #[test]
    fn parse_triple() {
        let context = Context::new();
        let ctx = context.ctx();

        let turtle = "<a> <b> <c> .";
        let output = parse_it(turtle, triple(ctx)).0.expect("simple");
        assert_eq!(output.to_string(), "<a> <b> <c>.");

        let turtle = "<a> <b> <c> , <d> .";
        let output = parse_it(turtle, triple(ctx)).0.expect("object list");
        assert_eq!(output.to_string(), "<a> <b> <c>, <d>.");

        let turtle = "[ <d> <e> ] <b> <c> .";
        let output = parse_it(turtle, triple(ctx)).0.expect("blank node list");
        assert_eq!(output.to_string(), "[ <d> <e> ; ] <b> <c>.");

        let turtle = "[ <d> <e> ; <f> <g> ;  ] <b> <c> .";
        let output = parse_it(turtle, triple(ctx)).0.expect("blank node po list");
        println!("Triple {:?}", output);
        assert_eq!(output.to_string(), "[ <d> <e> ;<f> <g> ; ] <b> <c>.");

        println!("Four");
        let turtle = "<a> <b> [ ] .";
        let output = parse_it(turtle, triple(ctx)).0.expect("bnode object");
        assert_eq!(output.to_string(), "<a> <b> [ ].");
    }

    #[test]
    fn parse_triple_with_recovery_no_end() {
        let context = Context::new();
        let ctx = context.ctx();

        let url = lsp_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let txt = "<a> <b> <c>";
        let (output, errors) = parse_it(txt, turtle(&url, ctx));

        println!("Errors {:?}", errors);
        println!("B {:?}", output);

        assert_eq!(errors.len(), 1);
        assert_eq!(output.unwrap().to_string(), "<a> <b> <c>.\n");
        assert!(errors[0].span().start != 0);
    }

    #[test]
    fn parse_triple_with_recovery_no_object() {
        let context = Context::new();
        let ctx = context.ctx();

        let url = lsp_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let txt = "<b> <c> .";
        let (output, errors) = parse_it(txt, turtle(&url, ctx));

        println!("output {:?}", output);
        println!("errors {:?}", errors);

        assert_eq!(errors.len(), 1);
        assert_eq!(output.unwrap().to_string(), "<b> <c> invalid.\n");
        assert!(errors[0].span().start != 0);
    }

    #[test]
    fn parse_triple_with_recovery_unfinished_object() {
        let context = Context::new();
        let ctx = context.ctx();
        let url = lsp_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let txt = "<a> <b> <c>; <d> .";
        let (output, errors) = parse_it(txt, turtle(&url, ctx));

        println!("output {:?}", output);
        println!("errors {:?}", errors);

        assert_eq!(errors.len(), 1);
        assert_eq!(output.unwrap().to_string(), "<a> <b> <c>; <d> invalid.\n");
        assert!(errors[0].span().start != 0);
    }

    #[test]
    fn parse_triple_with_invalid_token_predicate() {
        let context = Context::new();
        let ctx = context.ctx();
        let url = lsp_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let txt = "<a> foa";
        let (output, errors) = parse_it_recovery(txt, turtle(&url, ctx));

        println!("output {:?}", output);
        println!(
            "output {:?}",
            output.as_ref().map(|x| x.to_string()).unwrap_or_default()
        );
        println!("errors {:?}", errors);

        assert_eq!(errors.len(), 3);
        assert_eq!(output.unwrap().to_string(), "<a> invalid invalid.\n");
    }

    #[test]
    fn parse_triple_with_invalid_token_subject() {
        let context = Context::new();
        let ctx = context.ctx();
        let url = lsp_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let txt = "foa";
        let (output, errors) = parse_it_recovery(txt, turtle(&url, ctx));

        println!("output {:?}", output);
        println!(
            "output {:?}",
            output.as_ref().map(|x| x.to_string()).unwrap_or_default()
        );
        println!("errors {:?}", errors);
        for error in &errors {
            println!("  {:?}", error);
        }

        assert_eq!(output.unwrap().to_string(), "invalid invalid invalid.\n");
        assert_eq!(errors.len(), 3);
    }

    #[test]
    fn parse_turtle() {
        let context = Context::new();
        let ctx = context.ctx();
        let txt = r#"
        @base <>. #This is a very nice comment!
#This is a very nice comment!
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
<a> <b> <c>.
#This is a very nice comment!
            "#;
        let url = lsp_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let output = parse_it(txt, turtle(&url, ctx)).0.expect("simple");
        assert_eq!(output.prefixes.len(), 1, "prefixes are parsed");
        assert_eq!(output.triples.len(), 1, "triples are parsed");
        assert!(output.base.is_some(), "base is parsed");
    }

    #[test]
    fn turtle_shouldnt_panic() {
        let context = Context::new();
        let ctx = context.ctx();
        let txt = r#"
[
            "#;

        let url = lsp_core::lsp_types::Url::from_str(
            "file:///home/silvius/Projects/jsonld-lsp/examples/test.ttl",
        )
        .unwrap();

        let output = parse_it_recovery(txt, turtle(&url, ctx));
        println!("output {:?}", output.1);
        // assert_eq!(output.prefixes.len(), 1, "prefixes are parsed");
        assert_eq!(
            output.0.expect("simple").triples.len(),
            1,
            "triples are parsed"
        );
    }

    #[test]
    fn turtle_invalid_predicate_in_object() {
        let context = Context::new();
        let ctx = context.ctx();
        // I don't see what this test does :(
        let txt = r#"
@prefix foaf: <http://xmlns.com/foaf/0.1/>.
<> a foaf:Person.
foaf: foaf:name "Arthur".

<a> a foaf:Person;
        foaf:  <invalid>;
        foaf:name "Arthur".

<a> a foaf:Person;.
<c> foaf:name "Arthur".

<a> foaf: foaf:Person;
    foaf:name "Arthur".
            "#;
        let url = lsp_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let output = parse_it(txt, turtle(&url, ctx)).0.expect("simple");
        let triples = output.get_simple_triples().expect("triples");
        for t in &triples.triples {
            println!("t: {}", t);
        }
        assert_eq!(output.prefixes.len(), 1, "prefixes are parsed");
        assert_eq!(triples.len(), 9, "triples are parsed");
    }

    #[test]
    fn turtle_remembers_subject_context_for_triples() {
        let url = lsp_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let mut context = Context::new();
        let t1 = r#"
<a> <b> <c>.
        "#;
        let t2 = r#"
<x>
<a> <b> <c>.
        "#;

        let tokens_1 = parse_tokens_str_safe(t1).unwrap();
        let stream = Stream::from_iter(
            t1.len()..t1.len(),
            tokens_1
                .iter()
                .cloned()
                .enumerate()
                .filter(|(_, x)| !x.is_comment())
                .map(|(i, t)| t.map(|x| PToken(x, i)))
                .map(|Spanned(x, y)| (x, y)), // .rev(),
        );

        let turtle_1 = turtle(&url, context.ctx())
            .parse_recovery(stream)
            .0
            .unwrap();
        turtle_1.set_context(&mut context);

        let tokens_2 = parse_tokens_str_safe(t2).unwrap();
        let stream = Stream::from_iter(
            t2.len()..t2.len(),
            tokens_2
                .iter()
                .cloned()
                .enumerate()
                .filter(|(_, x)| !x.is_comment())
                .map(|(i, t)| t.map(|x| PToken(x, i)))
                .map(|Spanned(x, y)| (x, y)), // .rev(),
        );

        context.setup_current_to_prev(
            TokenIdx { tokens: &tokens_2 },
            tokens_2.len(),
            TokenIdx { tokens: &tokens_1 },
            tokens_1.len(),
        );

        let turtle_2 = turtle(&url, context.ctx())
            .parse_recovery(stream)
            .0
            .unwrap();

        println!("Turtle 1 {:?}", turtle_1);
        println!("Turtle 2 {:?}", turtle_2);
        assert_eq!(turtle_1.triples.len(), 1);
        assert_eq!(turtle_2.triples.len(), 2);
    }

    #[test]
    fn turtle_remembers_subject_context_in_triple() {
        let url = lsp_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let mut context = Context::new();
        let t1 = r#"<a> <b> <c>."#;
        let t2 = r#"<a> <x> <b> <c>."#;

        let tokens_1 = parse_tokens_str_safe(t1).unwrap();
        let stream = Stream::from_iter(
            t1.len()..t1.len(),
            tokens_1
                .iter()
                .cloned()
                .enumerate()
                .filter(|(_, x)| !x.is_comment())
                .map(|(i, t)| t.map(|x| PToken(x, i)))
                .map(|Spanned(x, y)| (x, y)), // .rev(),
        );

        let turtle_1 = turtle(&url, context.ctx())
            .parse_recovery(stream)
            .0
            .unwrap();
        turtle_1.set_context(&mut context);

        let tokens_2 = parse_tokens_str_safe(t2).unwrap();
        let stream = Stream::from_iter(
            t2.len()..t2.len(),
            tokens_2
                .iter()
                .cloned()
                .enumerate()
                .filter(|(_, x)| !x.is_comment())
                .map(|(i, t)| t.map(|x| PToken(x, i)))
                .map(|Spanned(x, y)| (x, y)), // .rev(),
        );

        context.setup_current_to_prev(
            TokenIdx { tokens: &tokens_2 },
            tokens_2.len(),
            TokenIdx { tokens: &tokens_1 },
            tokens_1.len(),
        );

        let (turtle_2, e2) = turtle(&url, context.ctx()).parse_recovery(stream);
        let turtle_2 = turtle_2.unwrap();

        println!("Turtle 1");
        for t in &turtle_1.triples {
            println!("  {}", t.value());
        }
        println!("Source {:?}", t2);
        println!("Turtle 2");
        for t in &turtle_2.triples {
            println!("  {}", t.value());
        }
        for e in e2 {
            println!("e {:?}", e);
        }
        assert_eq!(turtle_1.triples.len(), 1);
        assert_eq!(turtle_2.triples.len(), 1);
    }

    #[test]
    fn context_works_in_blanknodes() {
        let mut context = Context::new();
        let ctx = context.ctx();
        // I don't see what this test does :(
        let txt = r#"
@prefix foaf: <http://xmlns.com/foaf/0.1/>.
[ ] foaf:knows [ foaf:name "Arthur" ];
  foaf:name "Arthur".
            "#;
        let url = lsp_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();

        let tokens_2 = parse_tokens_str_safe(txt).unwrap();

        let output = parse_it(txt, turtle(&url, ctx)).0.expect("simple");
        output.set_context(&mut context);

        context.setup_current_to_prev(
            TokenIdx { tokens: &tokens_2 },
            tokens_2.len(),
            TokenIdx { tokens: &tokens_2 },
            tokens_2.len(),
        );

        let ctx = context.ctx();
        for (i, t) in tokens_2.iter().enumerate() {
            println!(
                "t ({} {} {}) {}",
                ctx.was_subject(i),
                ctx.was_predicate(i),
                ctx.was_object(i),
                &txt[t.span().clone()]
            )
        }
    }
}
