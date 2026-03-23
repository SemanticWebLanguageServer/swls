use std::{borrow::Cow, cmp::Ordering, fmt::Display, sync::Arc};

use bevy_ecs::prelude::*;
use iri_s::IriS;
use prefixmap::{error::PrefixMapError, PrefixMap};

use rudof_rdf::rdf_core::{
    term::{
        self as r_term, literal::Literal, BlankNode, Iri, IriOrBlankNode, Term as Rudof_Term,
        TermKind, Triple,
    },
    FocusRDF, Matcher, NeighsRDF, RDFError, SHACLPath,
};
use sophia_api::term::Term;

use crate::prelude::*;
use shacl_ir::compiled::schema_ir::SchemaIR as ShaclSchemaIR;

#[derive(Component)]
pub struct ShaclShapes {
    shacl: ShaclSchemaIR,
}
impl ShaclShapes {
    pub fn new(shacl: ShaclSchemaIR) -> Self {
        Self { shacl }
    }

    pub fn ir(&self) -> &ShaclSchemaIR {
        &self.shacl
    }
}
unsafe impl Send for ShaclShapes {}

#[derive(Clone, Debug)]
pub struct Rdf {
    focus: Option<MyTerm<'static>>,
    triples: Arc<Vec<MyQuad<'static>>>,
}
impl Rdf {
    pub fn new(triples: &Triples) -> Self {
        Self {
            focus: None,
            triples: triples.0.clone(),
        }
    }
}

impl FocusRDF for Rdf {
    fn set_focus(&mut self, focus: &Self::Term) {
        self.focus = Some(focus.clone());
    }

    fn get_focus(&self) -> Option<&Self::Term> {
        self.focus.as_ref()
    }
}

impl NeighsRDF for Rdf {
    fn triples(&self) -> std::result::Result<impl Iterator<Item = Self::Triple>, Self::Err> {
        Ok(self.triples.iter().cloned())
    }

    fn triples_matching<S, P, O>(
        &self,
        subject: &S,
        predicate: &P,
        object: &O,
    ) -> Result<impl Iterator<Item = Self::Triple> + '_, Self::Err>
    where
        S: Matcher<Self::Subject>,
        P: Matcher<Self::IRI>,
        O: Matcher<Self::Term>,
    {
        let subject: Option<MyTerm<'static>> = subject.value().cloned();
        let predicate: Option<MyTerm<'static>> = predicate.value().cloned();
        let object: Option<MyTerm<'static>> = object.value().cloned();
        let triples = self.triples()?.filter_map(move |triple| {
            let subject_matches = subject
                .as_ref()
                .map(|s| s == &triple.subject)
                .unwrap_or(true);

            let predicate_matches = predicate
                .as_ref()
                .map(|s| s == &triple.predicate)
                .unwrap_or(true);

            let object_matches = object.as_ref().map(|s| s == &triple.object).unwrap_or(true);

            if subject_matches && predicate_matches && object_matches {
                Some(triple)
            } else {
                None
            }
        });
        Ok(triples)
    }
}

impl<'b> TryInto<oxrdf::Term> for MyTerm<'static> {
    type Error = ();

    fn try_into(self) -> std::result::Result<oxrdf::Term, Self::Error> {
        use oxigraph::model as M;
        use oxigraph::model::Term as T;
        let output = match &self.ty {
            Some(sophia_api::prelude::TermKind::Iri) => {
                T::NamedNode(M::NamedNode::new(self.as_str()).map_err(|_| ())?)
            }
            Some(sophia_api::prelude::TermKind::Literal) => {
                if let Some(dt) = sophia_api::prelude::Term::datatype(&self) {
                    let dt = M::NamedNode::new(dt.as_str()).map_err(|_| ())?;
                    T::Literal(M::Literal::new_typed_literal(self.value.as_ref(), dt))
                } else if let Some(lang) = self.language_tag() {
                    T::Literal(
                        M::Literal::new_language_tagged_literal(self.value.as_ref(), lang.as_str())
                            .map_err(|_| ())?,
                    )
                } else {
                    T::Literal(M::Literal::new_simple_literal(self.value.as_ref()))
                }
            }
            Some(sophia_api::prelude::TermKind::BlankNode) => {
                T::BlankNode(M::BlankNode::new(self.value.as_ref()).map_err(|_| ())?)
            }
            _ => {
                return Err(());
            }
        };
        return Ok(output);
    }
}

impl TryFrom<MyTerm<'static>> for IriOrBlankNode {
    type Error = ();

    fn try_from(term: MyTerm<'static>) -> std::result::Result<Self, Self::Error> {
        let MyTerm { ty, value, .. } = term;
        if let Some(t) = ty {
            match t {
                sophia_api::term::TermKind::Iri => {
                    return Ok(IriOrBlankNode::Iri(IriS::new_unchecked(value.as_ref())))
                }
                sophia_api::term::TermKind::BlankNode => {
                    return Ok(IriOrBlankNode::BlankNode(value.to_string()))
                }
                _ => {}
            }
        }
        Err(())
    }
}

impl From<IriS> for MyTerm<'static> {
    fn from(value: IriS) -> Self {
        MyTerm::named_node(value.as_str(), 0..0).to_owned()
    }
}

impl From<IriOrBlankNode> for MyTerm<'static> {
    fn from(value: IriOrBlankNode) -> Self {
        match value {
            IriOrBlankNode::BlankNode(id) => MyTerm::blank_node(id, 0..0),
            IriOrBlankNode::Iri(iri_s) => MyTerm::named_node(iri_s.as_str(), 0..0).to_owned(),
        }
    }
}

impl From<MyTerm<'static>> for IriS {
    fn from(value: MyTerm<'static>) -> Self {
        IriS::new_unchecked(value.as_str())
    }
}

impl Iri for MyTerm<'static> {
    fn as_str(&self) -> &str {
        self.as_str()
    }
}

impl From<MyQuad<'static>> for MyTerm<'static> {
    fn from(value: MyQuad<'static>) -> Self {
        tracing::error!(
            "From<MyQuad<'static>> for MyTerm<'static> is not supported, using subject"
        );
        value.subject
    }
}

impl Rudof_Term for MyTerm<'static> {
    fn kind(&self) -> TermKind {
        if let Some(ty) = self.ty {
            map(ty)
        } else {
            TermKind::BlankNode
        }
    }

    fn lexical_form(&self) -> String {
        self.value.to_string()
    }
}

impl BlankNode for MyTerm<'static> {
    fn new(id: impl Into<String>) -> Self {
        MyTerm::blank_node(id.into(), 0..0)
    }

    fn id(&self) -> &str {
        &self.value
    }
}

impl Literal for MyTerm<'static> {
    fn lexical_form(&self) -> &str {
        self.value.as_ref()
    }

    fn lang(&self) -> Option<r_term::literal::Lang> {
        match &self.context {
            TermContext::LangTag(cow) => return r_term::literal::Lang::new(cow.as_ref()).ok(),
            _ => {}
        }
        None
    }

    fn datatype(&self) -> prefixmap::IriRef {
        let str = match &self.context {
            TermContext::DataType(cow) => cow.as_ref(),
            _ => "http://www.w3.org/2001/XMLSchema#string",
        };

        prefixmap::IriRef::Iri(IriS::new_unchecked(str))
    }

    fn to_concrete_literal(&self) -> Option<r_term::literal::ConcreteLiteral> {
        if self.ty == Some(sophia_api::term::TermKind::Literal) {
            Some(self.clone().into())
        } else {
            None
        }
    }
}

impl From<r_term::Object> for MyTerm<'static> {
    fn from(value: r_term::Object) -> Self {
        use r_term::Object;
        match value {
            Object::Iri(iri_s) => MyTerm::named_node(iri_s.as_str(), 0..0).to_owned(),
            Object::BlankNode(b) => MyTerm::blank_node(b, 0..0),
            Object::Literal(sliteral) => sliteral.into(),
            Object::Triple { subject, .. } => (*subject).into(),
        }
    }
}

fn dt_to_slit(value: &str, dt: &str) -> r_term::literal::ConcreteLiteral {
    use r_term::literal::ConcreteLiteral as SLiteral;
    if "http://www.w3.org/2001/XMLSchema#boolean" == dt {
        if let Ok(b) = SLiteral::parse_bool(value) {
            return SLiteral::BooleanLiteral(b);
        }
    }

    if "http://www.w3.org/2001/XMLSchema#integer" == dt {
        if let Ok(b) = SLiteral::parse_integer(value) {
            return SLiteral::NumericLiteral(r_term::literal::NumericLiteral::Integer(b));
        }
    }

    if "http://www.w3.org/2001/XMLSchema#double" == dt {
        if let Ok(b) = SLiteral::parse_double(value) {
            return SLiteral::NumericLiteral(r_term::literal::NumericLiteral::Double(b));
        }
    }

    if "http://www.w3.org/2001/XMLSchema#decimal" == dt {
        if let Ok(b) = SLiteral::parse_decimal(value) {
            return SLiteral::NumericLiteral(r_term::literal::NumericLiteral::Decimal(b));
        }
    }

    SLiteral::DatatypeLiteral {
        lexical_form: value.to_string(),
        datatype: prefixmap::IriRef::Iri(IriS::new_unchecked(&dt)),
    }
}

impl From<MyTerm<'static>> for r_term::Object {
    fn from(term: MyTerm<'static>) -> Self {
        let MyTerm { ty, value, .. } = &term;
        match ty.unwrap_or(sophia_api::prelude::TermKind::BlankNode) {
            sophia_api::term::TermKind::Iri => {
                r_term::Object::Iri(IriS::new_unchecked(value.as_ref()))
            }
            sophia_api::term::TermKind::BlankNode => r_term::Object::BlankNode(value.to_string()),
            sophia_api::term::TermKind::Literal => r_term::Object::Literal(term.into()),
            _ => {
                tracing::error!(
                    "Unsupported into srdf::Object for MyTerm with type {:?}",
                    ty
                );

                r_term::Object::Iri(IriS::default())
            }
        }
    }
}

impl From<r_term::literal::ConcreteLiteral> for MyTerm<'static> {
    fn from(value: r_term::literal::ConcreteLiteral) -> Self {
        let context = match value.lang() {
            Some(dt) => TermContext::LangTag(Cow::Owned(dt.to_string())),
            None => TermContext::DataType(Cow::Owned(value.datatype().to_string())),
        };
        MyTerm::literal(value.lexical_form(), 0..0, context)
    }
}

impl From<bool> for MyTerm<'static> {
    fn from(value: bool) -> Self {
        MyTerm::literal(
            value.to_string(),
            0..0,
            TermContext::DataType(Cow::Borrowed("http://www.w3.org/2001/XMLSchema#boolean")),
        )
    }
}

impl From<String> for MyTerm<'static> {
    fn from(value: String) -> Self {
        MyTerm::literal(
            value,
            0..0,
            TermContext::DataType(Cow::Borrowed("http://www.w3.org/2001/XMLSchema#string")),
        )
    }
}

impl From<f64> for MyTerm<'static> {
    fn from(value: f64) -> Self {
        MyTerm::literal(
            value.to_string(),
            0..0,
            TermContext::DataType(Cow::Borrowed("http://www.w3.org/2001/XMLSchema#double")),
        )
    }
}

impl From<i128> for MyTerm<'static> {
    fn from(value: i128) -> Self {
        MyTerm::literal(
            value.to_string(),
            0..0,
            TermContext::DataType(Cow::Borrowed("http://www.w3.org/2001/XMLSchema#integer")),
        )
    }
}

impl From<MyTerm<'static>> for r_term::literal::ConcreteLiteral {
    fn from(t: MyTerm<'static>) -> Self {
        let value = t.value;
        let context = t.context;
        match context {
            TermContext::None => r_term::literal::ConcreteLiteral::StringLiteral {
                lexical_form: value.to_string(),
                lang: None,
            },
            TermContext::DataType(dt) => dt_to_slit(value.as_ref(), dt.as_ref()),
            TermContext::LangTag(cow) => r_term::literal::ConcreteLiteral::StringLiteral {
                lexical_form: value.to_string(),
                lang: r_term::literal::Lang::new(cow).ok(),
            },
        }
    }
}
impl r_term::Triple<MyTerm<'static>, MyTerm<'static>, MyTerm<'static>> for MyQuad<'static> {
    fn new(
        subject: impl Into<MyTerm<'static>>,
        predicate: impl Into<MyTerm<'static>>,
        object: impl Into<MyTerm<'static>>,
    ) -> Self {
        MyQuad {
            subject: subject.into(),
            predicate: predicate.into(),
            object: object.into(),
            span: 0..0,
        }
    }

    fn subj(&self) -> &MyTerm<'static> {
        &self.subject
    }

    fn pred(&self) -> &MyTerm<'static> {
        &self.predicate
    }

    fn obj(&self) -> &MyTerm<'static> {
        &self.object
    }

    fn into_components(self) -> (MyTerm<'static>, MyTerm<'static>, MyTerm<'static>) {
        let MyQuad {
            subject,
            predicate,
            object,
            ..
        } = self;
        (subject, predicate, object)
    }
}

// impl From<IriOrBlankNode> for MyTerm<'static> {
//     fn from(value: IriOrBlankNode) -> Self {
//         todo!()
//     }
// }

impl rudof_rdf::rdf_core::Rdf for Rdf {
    type Subject = MyTerm<'static>;

    type IRI = MyTerm<'static>;

    type Term = MyTerm<'static>;

    type BNode = MyTerm<'static>;

    type Literal = MyTerm<'static>;

    type Triple = MyQuad<'static>;

    type Err = &'static str;

    fn qualify_iri(&self, iri: &Self::IRI) -> String {
        iri.value.to_string()
    }

    fn qualify_subject(&self, subj: &Self::Subject) -> String {
        subj.value.to_string()
    }

    fn qualify_term(&self, term: &Self::Term) -> String {
        term.value.to_string()
    }

    fn prefixmap(&self) -> Option<PrefixMap> {
        None
    }

    fn resolve_prefix_local(
        &self,
        prefix: &str,
        _local: &str,
    ) -> std::result::Result<IriS, PrefixMapError> {
        Err(PrefixMapError::PrefixNotFound {
            prefix: prefix.to_string(),
            prefixmap: PrefixMap::new(),
        })
    }

    fn term_as_literal(term: &Self::Term) -> Result<Self::Literal, RDFError> {
        if term.ty == Some(sophia_api::term::TermKind::Literal) {
            <Self::Term as TryInto<Self::Literal>>::try_into(term.clone()).map_err(|_| {
                RDFError::TermAsLiteral {
                    term: term.to_string(),
                }
            })
        } else {
            Err(RDFError::TermAsLiteral {
                term: term.to_string(),
            })
        }
    }

    fn term_as_sliteral(term: &Self::Term) -> Result<r_term::literal::ConcreteLiteral, RDFError> {
        let lit = Self::term_as_literal(term)?;
        let slit =
            <Self::Literal as TryInto<r_term::literal::ConcreteLiteral>>::try_into(lit.clone())
                .map_err(|_| RDFError::LiteralAsSLiteral {
                    literal: lit.to_string(),
                })?;
        Ok(slit)
    }

    fn term_as_subject(term: &Self::Term) -> Result<Self::Subject, RDFError> {
        if !(term.ty == Some(sophia_api::term::TermKind::BlankNode)
            || term.ty == Some(sophia_api::term::TermKind::Iri))
        {
            return Err(RDFError::TermAsSubject {
                term: term.to_string(),
            });
        }
        <Self::Term as TryInto<Self::Subject>>::try_into(term.clone()).map_err(|_e| {
            RDFError::TermAsSubject {
                term: term.to_string(),
            }
        })
    }

    // Cannot use the default implementation because the types are the same between term and iri
    // Which makes mapping impossible to fail
    fn term_as_iri(term: &Self::Term) -> Result<Self::IRI, RDFError> {
        if term.ty == Some(sophia_api::term::TermKind::Iri) {
            <Self::Term as TryInto<Self::IRI>>::try_into(term.clone()).map_err(|_| {
                RDFError::TermAsIri {
                    term: term.to_string(),
                }
            })
        } else {
            Err(RDFError::TermAsIri {
                term: term.to_string(),
            })
        }
    }

    fn term_as_iris(term: &Self::Term) -> Result<IriS, RDFError> {
        if term.ty != Some(sophia_api::term::TermKind::Iri) {
            return Err(RDFError::TermAsIriS {
                term: term.to_string(),
            });
        }

        let iri = <Self::Term as TryInto<Self::IRI>>::try_into(term.clone()).map_err(|_| {
            RDFError::TermAsIriS {
                term: term.to_string(),
            }
        })?;
        let iri_s: IriS = iri.into();
        Ok(iri_s)
    }

    fn term_as_bnode(term: &Self::Term) -> Result<Self::BNode, RDFError> {
        if term.ty != Some(sophia_api::term::TermKind::BlankNode) {
            return Err(RDFError::TermAsBNode {
                term: term.to_string(),
            });
        }
        <Self::Term as TryInto<Self::BNode>>::try_into(term.clone()).map_err(|_| {
            RDFError::TermAsBNode {
                term: term.to_string(),
            }
        })
    }

    fn compare(&self, term1: &Self::Term, term2: &Self::Term) -> Result<Ordering, RDFError> {
        term1
            .partial_cmp(&term2)
            .ok_or_else(|| RDFError::ComparisonError {
                term1: term1.to_string(),
                term2: term2.to_string(),
            })
    }
}

fn map(term: sophia_api::term::TermKind) -> r_term::TermKind {
    match term {
        sophia_api::term::TermKind::Iri => TermKind::Iri,
        sophia_api::term::TermKind::Literal => TermKind::Literal,
        sophia_api::term::TermKind::BlankNode => TermKind::BlankNode,
        sophia_api::term::TermKind::Triple => TermKind::Triple,
        sophia_api::term::TermKind::Variable => TermKind::BlankNode,
    }
}

impl r_term::Subject for MyTerm<'_> {
    fn kind(&self) -> r_term::TermKind {
        if let Some(ty) = self.ty {
            map(ty)
        } else {
            TermKind::BlankNode
        }
    }
}
impl Matcher<MyTerm<'static>> for MyTerm<'static> {
    fn value(&self) -> Option<&MyTerm<'static>> {
        Some(self)
    }
}

pub struct PrefixedPath<'a> {
    pub path: &'a SHACLPath,
    pub prefixes: &'a Prefixes,
}
impl<'a> PrefixedPath<'a> {
    pub fn new(path: &'a SHACLPath, prefixes: &'a Prefixes) -> Self {
        Self { path, prefixes }
    }
}

impl<'a> Display for PrefixedPath<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.path {
            SHACLPath::Predicate { pred } => {
                if let Some(shorten) = self.prefixes.shorten(pred.as_str()) {
                    write!(f, "{shorten}")
                } else {
                    write!(f, "{pred}")
                }
            }
            SHACLPath::Alternative { paths } => {
                write!(f, "(")?;
                let mut first = true;
                for p in paths {
                    if !first {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", Self::new(p, self.prefixes))?;
                    first = false;
                }
                write!(f, ")")
            }
            SHACLPath::Sequence { paths } => {
                write!(f, "(")?;
                let mut first = true;
                for p in paths {
                    if !first {
                        write!(f, " / ")?;
                    }
                    write!(f, "{}", Self::new(p, self.prefixes))?;
                    first = false;
                }
                write!(f, ")")
            }
            SHACLPath::Inverse { path } => {
                write!(f, "^({})", Self::new(path, self.prefixes))
            }
            SHACLPath::ZeroOrMore { path } => {
                write!(f, "({})*", Self::new(path, self.prefixes))
            }
            SHACLPath::OneOrMore { path } => {
                write!(f, "({})+", Self::new(path, self.prefixes))
            }
            SHACLPath::ZeroOrOne { path } => {
                write!(f, "({})?", Self::new(path, self.prefixes))
            }
        }
    }
}
