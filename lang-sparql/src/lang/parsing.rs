use chumsky::{prelude::*, Error};
use lang_turtle::lang::{
    context::Ctx,
    model::TurtlePrefix,
    parser::{named_node, not, triple},
};
use lsp_core::prelude::{spanned, PToken, Spanned, SparqlExpr, SparqlKeyword, Token};

use crate::lang::model::{
    Base, Bind, DatasetClause, Expression, GroupGraphPattern, GroupGraphPatternSub, Modifier,
    Prologue, Query, QueryClause, SelectClause, Solution, SubSelect, Variable, WhereClause,
};

fn j(token: Token) -> impl Parser<PToken, Token, Error = Simple<PToken>> + Clone {
    just(PToken(token, 0)).map(|x| x.0)
}

fn sparql_kwd(
    kwd: SparqlKeyword,
) -> impl Parser<PToken, Spanned<SparqlKeyword>, Error = Simple<PToken>> + Clone {
    just(PToken(Token::SparqlKeyword(kwd.clone()), 0))
        .to(kwd)
        .map_with_span(spanned)
}

// DONE
fn prologue() -> impl Parser<PToken, Prologue, Error = Simple<PToken>> + Clone {
    let base = j(Token::SparqlBase)
        .map_with_span(spanned)
        .then(named_node().map_with_span(spanned))
        .map(|(token, iri)| Prologue::Base { token, iri });

    let prefix = j(Token::SparqlPrefix).map_with_span(|_, s| s)
        .then(select! { |span| PToken(Token::PNameLN(x, _), _) => Spanned(x.unwrap_or_default(), span)})
        .then(named_node()
        .map_with_span(spanned))
        .map(|((span, prefix), value)| {
            Prologue::Prefix(TurtlePrefix {
                span,
                prefix,
                value,
            })
        });

    base.or(prefix)
}

// DONE
fn dataset_clause() -> impl Parser<PToken, DatasetClause, Error = Simple<PToken>> + Clone {
    sparql_kwd(SparqlKeyword::From)
        .then(sparql_kwd(SparqlKeyword::Named).or_not())
        .then(named_node().map_with_span(spanned))
        .map(|((from, named), iri)| DatasetClause { from, named, iri })
}

fn expression() -> impl Parser<PToken, Expression, Error = Simple<PToken>> + Clone {
    todo()
}

// DONE
fn bind() -> impl Parser<PToken, Bind, Error = Simple<PToken>> + Clone {
    sparql_kwd(SparqlKeyword::Bind)
        .ignore_then(expression())
        .map_with_span(spanned)
        .then(sparql_kwd(SparqlKeyword::As))
        .then(variable().map_with_span(spanned))
        .map(|((expr, kwd), var)| Bind { var, kwd, expr })
}

fn variable() -> impl Parser<PToken, Variable, Error = Simple<PToken>> + Clone {
    select! {
        PToken(Token::Variable(s), _) => Variable(s),
    }
}

// DONE
fn select_clause() -> impl Parser<PToken, SelectClause, Error = Simple<PToken>> + Clone {
    let star = j(Token::SparqlExpr(SparqlExpr::Times))
        .to(Solution::All)
        .map_with_span(spanned)
        .map(|x| vec![x]);

    let others = bind()
        .delimited_by(j(Token::CurlOpen), j(Token::CurlClose))
        .map(Solution::VarAs)
        .or(variable().map(Solution::Var))
        .map_with_span(spanned)
        .repeated();

    // This might be different to support ask and describe

    sparql_kwd(SparqlKeyword::Select)
        .then(
            sparql_kwd(SparqlKeyword::Distinct)
                .or(sparql_kwd(SparqlKeyword::Reduced))
                .or_not(),
        )
        .then(star.or(others))
        .map(|((kwd, modifier), solutions)| SelectClause {
            kwd,
            modifier,
            solutions,
        })
}

// DONE
fn sub_select<'a>(
    ctx: Ctx<'a>,
) -> impl Parser<PToken, SubSelect, Error = Simple<PToken>> + Clone + use<'a> {
    recursive(|sub_select| {
        let modi = modifier().map_with_span(spanned).repeated();
        select_clause()
            .then(where_clause(sub_select, ctx))
            .then(modi)
            .map(|((select, where_clause), modifier)| SubSelect {
                modifier,
                where_clause,
                select,
            })
    })
}

// DONE
fn group_graph_pattern_sub<
    'a,
    T: Parser<PToken, GroupGraphPattern, Error = Simple<PToken>> + Clone,
>(
    ggp: T,
    ctx: Ctx<'a>,
) -> impl Parser<PToken, GroupGraphPatternSub, Error = Simple<PToken>> + Clone + use<'a, T> {
    let next_check = not(Token::CurlClose).rewind();

    let trip = triple(ctx)
        .map_with_span(spanned)
        .map(GroupGraphPatternSub::Triple)
        .labelled("triple");

    let kwd = sparql_kwd(SparqlKeyword::Minus)
        .or(sparql_kwd(SparqlKeyword::Optional))
        .clone()
        .then_ignore(j(Token::CurlOpen).rewind())
        .then(ggp.clone().map_with_span(spanned))
        .map(|(kwd, ggp)| GroupGraphPatternSub::Kwd(kwd, ggp));

    let union = j(Token::CurlOpen)
        .rewind()
        .ignore_then(ggp.clone())
        .map_with_span(spanned)
        .then(
            sparql_kwd(SparqlKeyword::Union)
                .then(ggp.map_with_span(spanned))
                .repeated(),
        )
        .map(|(start, rest)| GroupGraphPatternSub::Union(start, rest));

    // TODO add the others
    //

    next_check.ignore_then(trip.or(kwd).or(union).labelled("group_graph_pattern_sub"))
}

fn expect_it(
    token: Token,
    st: &'static str,
) -> impl Parser<PToken, Token, Error = Simple<PToken>> + Clone {
    j(token.clone()).or(not(token.clone())
        .map(|x| x.0)
        .try_map(move |x: Token, span| {
            println!("{} didn't expect {}", st, x);
            Err(Simple::expected_input_found(
                span,
                [Some(PToken(token.clone(), 0))],
                Some(PToken(x.clone(), 0)),
            ))
        }))
}

// DONE
fn group_graph_pattern<'a, T: Parser<PToken, SubSelect, Error = Simple<PToken>> + Clone + 'a>(
    select: T,
    ctx: Ctx<'a>,
) -> impl Parser<PToken, GroupGraphPattern, Error = Simple<PToken>> + Clone + use<'a, T> {
    let s = select.clone();
    recursive(|ggp| {
        let select = s
            .clone()
            .map(Box::from)
            .map(GroupGraphPattern::SubSelect)
            .labelled("sub_select");

        let gg = group_graph_pattern_sub(ggp, ctx)
            .map_with_span(spanned)
            .repeated()
            .map(GroupGraphPattern::GroupGraph);

        let close = expect_it(Token::CurlClose, "close").labelled("CurlClose");
        let open = expect_it(Token::CurlOpen, "open").labelled("CurlOpen");

        open.ignore_then(gg.or(select)).then_ignore(close)
    })
}

// DONE
fn where_clause<'a, T: Parser<PToken, SubSelect, Error = Simple<PToken>> + Clone + 'a>(
    select: T,
    ctx: Ctx<'a>,
) -> impl Parser<PToken, WhereClause, Error = Simple<PToken>> + Clone + use<'a, T> {
    sparql_kwd(SparqlKeyword::Where)
        .or_not()
        .then(group_graph_pattern(select, ctx).map_with_span(spanned))
        .map(|(kwd, ggp)| WhereClause { ggp, kwd })
}

// DONE
fn modifier() -> impl Parser<PToken, Modifier, Error = Simple<PToken>> + Clone {
    let num = select!(
        PToken(Token::Number(x), _) => x,
    )
    .map_with_span(spanned);

    let limit_offset = sparql_kwd(SparqlKeyword::Limit)
        .or(sparql_kwd(SparqlKeyword::Offset))
        .then(num)
        .map(|(kwd, num)| Modifier::LimitOffset(kwd, num));
    limit_offset
}

pub fn query<'a>(
    base: lsp_core::lsp_types::Url,
    ctx: Ctx<'a>,
) -> impl Parser<PToken, Query, Error = Simple<PToken>> + Clone + use<'a> {
    let prologues = prologue().map_with_span(spanned).repeated().map(|xs| {
        let mut base = None;
        let mut prefixes = vec![];
        xs.into_iter().for_each(|Spanned(x, span)| match x {
            Prologue::Base { token, iri } => base = Some(Spanned(Base { token, iri }, span)),
            Prologue::Prefix(prefix) => prefixes.push(Spanned(prefix, span)),
        });
        (base, prefixes)
    });
    let kwds = select_clause().map(QueryClause::Select);
    let datasets = dataset_clause().map_with_span(spanned).repeated();
    let where_clause = where_clause(sub_select(ctx), ctx).map_with_span(spanned);
    let modifiers = modifier().map_with_span(spanned).repeated();

    prologues
        .then(kwds)
        .then(datasets)
        .then(where_clause)
        .then(modifiers)
        .map(
            move |(((((base_statement, prefixes), kwds), datasets), where_clause), modifier)| {
                Query {
                    base_statement,
                    prefixes,
                    base: base.clone(),
                    modifier,
                    where_clause,
                    datasets,
                    kwds,
                }
            },
        )
}

pub fn parse(
    source: &str,
    base: lsp_core::lsp_types::Url,
    tokens: Vec<Spanned<Token>>,
    ctx: Ctx<'_>,
) -> (Spanned<Query>, Vec<Simple<PToken>>) {
    let len = source.len();
    let stream = chumsky::Stream::from_iter(
        0..len,
        tokens
            .into_iter()
            .enumerate()
            .filter(|(_, x)| !x.is_comment())
            .map(|(i, t)| t.map(|x| PToken(x, i)))
            .map(|Spanned(x, s)| (x, s)),
    );

    let parser = query(base, ctx)
        .map_with_span(spanned)
        .then_ignore(end().recover_with(skip_then_retry_until([])));
    let (json, json_errors) = parser.parse_recovery(stream);

    (
        json.unwrap_or(Spanned(Query::default(), 0..source.len())),
        json_errors,
    )
}

#[cfg(test)]
mod tests {
    use chumsky::Stream;
    use lang_turtle::lang::context::Context;

    use super::*;
    use crate::lang::{
        parsing::select_clause,
        tokenizer::{parse_tokens_str},
    };
    pub fn parse_it<T, P: Parser<PToken, T, Error = Simple<PToken>>>(
        turtle: &str,
        parser: P,
    ) -> (Option<T>, Vec<Simple<PToken>>) {
        let (tokens, _) = parse_tokens_str(turtle);
        for token in &tokens {
            println!("token {:?}", token);
        }
        let end = turtle.len()..turtle.len();
        let stream = Stream::from_iter(
            end,
            tokens
                .into_iter()
                .enumerate()
                .filter(|x| !x.1.is_comment())
                .map(|(i, t)| t.map(|x| PToken(x, i)))
                .map(|Spanned(x, y)| (x, y)),
        );

        parser
            .then_ignore(chumsky::prelude::end())
            .parse_recovery(stream)
    }
    #[test]
    fn parse_prologue() {
        let inp = r#"
PREFIX  dc:  <http://purl.org/dc/elements/1.1/>
        "#;

        let (q, tok) = parse_it(inp, prologue());

        assert_eq!(tok, vec![]);
        assert!(q.is_some());
    }

    #[test]
    fn parse_select_clause() {
        let inp = r#"
SELECT  ?title ?price
        "#;

        let (q, tok) = parse_it(inp, select_clause());

        println!("q {:?}", q);

        for t in &tok {
            println!("t {:?}", t);
        }

        assert_eq!(tok, vec![]);
        assert!(q.is_some());
    }

    #[test]
    fn parse_triple() {
        let context = Context::new();
        let ctx = context.ctx();
        let inp = r#"
  ?x ns:discount ?discount .
        "#;

        let (q, tok) = parse_it(inp, triple(ctx));

        assert_eq!(tok, vec![]);
        assert!(q.is_some());
    }

    #[test]
    fn parse_group_graph_pattern_sub() {
        let context = Context::new();
        let ctx = context.ctx();
        let inp = r#"
 ?x ns:price ?p .
        "#;

        let (q, tok) = parse_it(
            inp,
            group_graph_pattern_sub(group_graph_pattern(sub_select(ctx), ctx), ctx),
        );

        assert_eq!(tok, vec![]);
        assert!(q.is_some());
    }

    #[test]
    fn parse_group_graph_pattern() {
        let context = Context::new();
        let ctx = context.ctx();
        let inp = r#"{
    ?x ns:price ?p .
}"#;

        let (q, tok) = parse_it(inp, group_graph_pattern(sub_select(ctx), ctx));

        assert_eq!(tok, vec![]);
        assert!(q.is_some());
    }

    #[test]
    fn simple_test() {
        let context = Context::new();
        let ctx = context.ctx();
        let inp = r#"PREFIX  dc:  <http://purl.org/dc/elements/1.1/>
PREFIX  ns:  <http://example.org/ns#>
SELECT  ?title ?price
{ ?x ns:price ?p .
  ?x dc:title ?title . 
  ?x ns:discount ?discount .
}
        "#;

        let (q, tok) = parse_it(
            inp,
            query(lsp_core::lsp_types::Url::parse("memory://myFile.sq").unwrap(), ctx),
        );

        assert_eq!(tok, vec![]);
        assert!(q.is_some());
    }
}
