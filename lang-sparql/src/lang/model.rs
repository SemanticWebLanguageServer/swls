use lang_turtle::lang::{
    context::Context,
    model::{Based, NamedNode, Triple, TriplesBuilder, TurtlePrefix, TurtleSimpleError},
};
use lsp_core::prelude::{Spanned, SparqlKeyword, Token};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Base {
    pub token: Spanned<Token>,
    pub iri: Spanned<NamedNode>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Prologue {
    Base {
        token: Spanned<Token>,
        iri: Spanned<NamedNode>,
    },
    Prefix(TurtlePrefix),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DatasetClause {
    pub from: Spanned<SparqlKeyword>,
    pub named: Option<Spanned<SparqlKeyword>>,
    pub iri: Spanned<NamedNode>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GroupGraphPattern {
    SubSelect(Box<SubSelect>),
    GroupGraph(Vec<Spanned<GroupGraphPatternSub>>),
    Invalid,
}
impl GroupGraphPattern {
    fn add_to_context(&self, ctx: &mut Context) {
        match self {
            GroupGraphPattern::SubSelect(sub_select) => {
                sub_select.add_to_context(ctx);
            }
            GroupGraphPattern::GroupGraph(spanneds) => {
                for s in spanneds {
                    s.value().add_to_context(ctx);
                }
            }
            _ => {}
        }
    }

    pub fn ingest_triples<'a>(
        &'a self,
        builder: &mut TriplesBuilder<'a, Query>,
    ) -> Result<(), TurtleSimpleError> {
        match self {
            GroupGraphPattern::SubSelect(sub_select) => sub_select.ingest_triples(builder)?,
            GroupGraphPattern::GroupGraph(xs) => {
                for x in xs {
                    x.ingest_triples(builder)?;
                }
            }
            GroupGraphPattern::Invalid => {}
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WhereClause {
    pub kwd: Option<Spanned<SparqlKeyword>>,
    pub ggp: Spanned<GroupGraphPattern>,
}
impl WhereClause {
    pub fn ingest_triples<'a>(
        &'a self,
        builder: &mut TriplesBuilder<'a, Query>,
    ) -> Result<(), TurtleSimpleError> {
        self.ggp.ingest_triples(builder)?;
        Ok(())
    }

    fn add_to_context(&self, ctx: &mut Context) {
        self.ggp.value().add_to_context(ctx);
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable(pub String);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expression {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Bind {
    pub expr: Spanned<Expression>,
    pub kwd: Spanned<SparqlKeyword>,
    pub var: Spanned<Variable>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Solution {
    All,
    Var(Variable),
    VarAs(Bind),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SelectClause {
    pub kwd: Spanned<SparqlKeyword>,
    pub modifier: Option<Spanned<SparqlKeyword>>,
    pub solutions: Vec<Spanned<Solution>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConstructClause {
    kwd: Spanned<SparqlKeyword>,
    template: Option<(
        Spanned<Token>,
        Vec<Spanned<GroupGraphPatternSub>>,
        Spanned<Token>,
    )>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum QueryClause {
    Select(SelectClause),
    Construct(ConstructClause),
    Invalid,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SubSelect {
    pub select: SelectClause,
    pub where_clause: WhereClause,
    pub modifier: Vec<Spanned<Modifier>>,
    // TODO values
}
impl SubSelect {
    fn add_to_context(&self, ctx: &mut Context) {
        self.where_clause.add_to_context(ctx);
    }

    pub fn ingest_triples<'a>(
        &'a self,
        builder: &mut TriplesBuilder<'a, Query>,
    ) -> Result<(), TurtleSimpleError> {
        self.where_clause.ingest_triples(builder)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GraphPatternNotTriples {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GroupGraphPatternSub {
    Triple(Spanned<Triple>),
    Kwd(Spanned<SparqlKeyword>, Spanned<GroupGraphPattern>),
    Filter(Spanned<SparqlKeyword>, ()),
    Union(
        Spanned<GroupGraphPattern>,
        Vec<(Spanned<SparqlKeyword>, Spanned<GroupGraphPattern>)>,
    ),
    GraphOrService(
        Spanned<SparqlKeyword>,
        Option<Spanned<SparqlKeyword>>,
        Spanned<NamedNode>,
        Spanned<GroupGraphPattern>,
    ),
    Bind(
        Spanned<SparqlKeyword>,
        Spanned<Token>,
        Spanned<Bind>,
        Spanned<Token>,
    ),
    Inline(Spanned<()>),
}
impl GroupGraphPatternSub {
    fn add_to_context(&self, ctx: &mut Context) {
        match self {
            GroupGraphPatternSub::Triple(t) => t.value().set_context(ctx),
            GroupGraphPatternSub::Kwd(_, beta) => beta.add_to_context(ctx),
            GroupGraphPatternSub::Union(a, b) => {
                a.add_to_context(ctx);
                for (_, b) in b {
                    b.add_to_context(ctx);
                }
            }
            GroupGraphPatternSub::GraphOrService(_, _, _, a) => a.add_to_context(ctx),
            _ => {}
        }
    }

    pub fn ingest_triples<'a>(
        &'a self,
        builder: &mut TriplesBuilder<'a, Query>,
    ) -> Result<(), TurtleSimpleError> {
        match self {
            GroupGraphPatternSub::Triple(triple) => builder.ingest(&triple)?,
            GroupGraphPatternSub::Kwd(_, x) => x.ingest_triples(builder)?,
            GroupGraphPatternSub::Union(x, xs) => {
                x.ingest_triples(builder)?;
                for (_, x) in xs {
                    x.ingest_triples(builder)?;
                }
            }
            GroupGraphPatternSub::GraphOrService(_, _, _, x) => x.ingest_triples(builder)?,
            _ => {}
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Modifier {
    GroupBy(Spanned<SparqlKeyword>, Spanned<SparqlKeyword>, Spanned<()>),
    Having(Spanned<SparqlKeyword>, Spanned<()>),
    OrderBy(Spanned<SparqlKeyword>, Spanned<SparqlKeyword>, Spanned<()>),
    LimitOffset(Spanned<SparqlKeyword>, Spanned<String>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Query {
    pub base: lsp_core::lsp_types::Url,
    pub prefixes: Vec<Spanned<TurtlePrefix>>,
    pub base_statement: Option<Spanned<Base>>,
    pub kwds: QueryClause,
    pub datasets: Vec<Spanned<DatasetClause>>,
    pub where_clause: Spanned<WhereClause>,
    pub modifier: Vec<Spanned<Modifier>>,
}
impl Query {
    pub fn add_to_context(&self, ctx: &mut Context) {
        self.where_clause.add_to_context(ctx);
    }

    pub fn ingest_triples<'a>(
        &'a self,
        builder: &mut TriplesBuilder<'a, Query>,
    ) -> Result<(), TurtleSimpleError> {
        self.where_clause.ingest_triples(builder)?;
        Ok(())
    }

    pub fn set_base(&mut self, base: lsp_core::lsp_types::Url) {
        self.base = self
            .base_statement
            .as_ref()
            .iter()
            .find_map(|x| match x.iri.value() {
                NamedNode::Full(x, _) => lsp_core::lsp_types::Url::parse(&x).ok(),
                _ => None,
            })
            .unwrap_or_else(|| base.clone());
    }

    pub fn get_base(&self) -> &lsp_core::lsp_types::Url {
        &self.base
    }
}

impl Based for Query {
    fn get_base(&self) -> &lsp_core::lsp_types::Url {
        &self.base
    }

    fn prefixes(&self) -> &[Spanned<TurtlePrefix>] {
        &self.prefixes
    }
}

impl Default for Query {
    fn default() -> Self {
        Query {
            base: lsp_core::lsp_types::Url::parse("memory://somefile.sq").unwrap(),
            base_statement: None,
            prefixes: vec![],
            kwds: QueryClause::Invalid,
            datasets: vec![],
            where_clause: Spanned(
                WhereClause {
                    kwd: None,
                    ggp: Spanned(GroupGraphPattern::Invalid, 0..0),
                },
                0..0,
            ),
            modifier: vec![],
        }
    }
}

#[cfg(test)]
mod tests {
    use lang_turtle::lang::context::Context;
    use sophia_iri::resolve::BaseIri;

    use super::*;
    use crate::lang::{parsing::parse, tokenizer::parse_tokens_str};

    fn parse_sparql(inp: &str) -> Query {
        let context = Context::new();
        let ctx = context.ctx();
        let (tokens, errors) = parse_tokens_str(inp);
        println!("Tokens");
        for t in &tokens {
            println!("t {:?}", t);
        }

        println!("errors");
        for t in &errors {
            println!("e {:?}", t);
        }

        let (jsonld, errors) = parse(
            inp,
            lsp_core::lsp_types::Url::parse("memory::myFile.sq").unwrap(),
            tokens,
            ctx,
        );
        println!("errors");
        for t in &errors {
            println!("e {:?}", t);
        }

        jsonld.0
    }

    #[test]
    fn simple_sparql_1() {
        let st = r#"
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX  dc:  <http://purl.org/dc/elements/1.1/>
PREFIX  ns:  <http://example.org/ns#>

SELECT  ?title ?price
{ ?x ns:price ?p .
  ?x dc:title ?title . 
  ?x ns:discount ?discount .
}
        "#;

        let query = parse_sparql(st);
        let base = BaseIri::new(query.base.to_string()).unwrap();
        let mut builder = TriplesBuilder::new(&query, base);
        query.ingest_triples(&mut builder).expect("builds fine");

        assert_eq!(builder.triples.len(), 3);
    }

    #[test]
    fn simple_sparql_2() {
        let st = r#"
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX  dc:  <http://purl.org/dc/elements/1.1/>
PREFIX  ns:  <http://example.org/ns#>

SELECT  ?title ?price
{ ?x ns:price ?p .
  ?x dc:title ?title . 
  [ # this is one triple as well
  ] ns:discount ?discount .
}
        "#;

        let query = parse_sparql(st);
        let base = BaseIri::new(query.base.to_string()).unwrap();
        let mut builder = TriplesBuilder::new(&query, base);
        query.ingest_triples(&mut builder).expect("builds fine");

        for t in &builder.triples {
            println!("t {}", t);
        }

        assert_eq!(builder.triples.len(), 4);
    }
}
