use std::{
    borrow::{Borrow, Cow},
    hash::Hash,
    sync::Arc,
    usize,
};

use bevy_ecs::prelude::*;
use derive_more::{AsMut, AsRef, Deref, DerefMut};
use sophia_api::{
    prelude::{Any, Dataset},
    quad::Quad,
    term::{matcher::TermMatcher, BnodeId, GraphName, IriRef, Term, TermKind},
    MownStr,
};
use tracing::{debug, instrument};

use crate::{
    components::{PositionComponent, RopeC},
    util::{
        ns::{owl, rdfs},
        position_to_offset,
    },
};

/// [`Component`] used to indicate the term type of currently targeted
/// [`Token`](`crate::prelude::Token`) in the Triple.
#[derive(Debug, PartialEq)]
pub enum TripleTarget {
    Subject,
    Predicate,
    Object,
    Graph,
}

/// [`Component`] used to indicate the currently targeted [`MyQuad<'static>`] during a request.
#[derive(Component, Debug)]
pub struct TripleComponent {
    pub triple: MyQuad<'static>,
    pub target: TripleTarget,
}

impl TripleComponent {
    pub fn kind(&self) -> TermKind {
        let target = match self.target {
            TripleTarget::Subject => self.triple.s().kind(),
            TripleTarget::Predicate => self.triple.p().kind(),
            TripleTarget::Object => self.triple.o().kind(),
            TripleTarget::Graph => self
                .triple
                .g()
                .map(|x| x.kind())
                .unwrap_or(sophia_api::term::TermKind::Triple),
        };
        target
    }

    pub fn term(&self) -> Option<&MyTerm<'static>> {
        let target = match self.target {
            TripleTarget::Subject => self.triple.s(),
            TripleTarget::Predicate => self.triple.p(),
            TripleTarget::Object => self.triple.o(),
            TripleTarget::Graph => return None,
        };
        Some(target)
    }
}

/// [`Component`] containing all derived Triples from the documents.
///
/// These triples are used to derive properties and classes and other things.
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct Triples(pub Arc<Vec<MyQuad<'static>>>);

impl Triples {
    pub fn object<'s, S, P>(&'s self, subj: S, pred: P) -> Option<&'s MyTerm<'s>>
    where
        S: TermMatcher + 's,
        P: TermMatcher + 's,
    {
        self.0
            .quads_matching(
                subj,
                pred,
                sophia_api::prelude::Any,
                sophia_api::prelude::Any,
            )
            .flatten()
            .next()
            .map(|x| x.o())
    }

    pub fn objects<'s, S, P>(&'s self, subj: S, pred: P) -> impl Iterator<Item = &'s MyTerm<'s>>
    where
        S: TermMatcher + 's,
        P: TermMatcher + 's,
    {
        self.0
            .quads_matching(
                subj,
                pred,
                sophia_api::prelude::Any,
                sophia_api::prelude::Any,
            )
            .flatten()
            .map(|x| x.o())
    }
}

#[instrument(skip(query, commands))]
pub fn get_current_triple(
    query: Query<(Entity, &PositionComponent, &Triples, &RopeC)>,
    mut commands: Commands,
) {
    for (e, position, triples, rope) in &query {
        commands.entity(e).remove::<TripleComponent>();

        let Some(offset) = position_to_offset(position.0, &rope.0) else {
            debug!("Couldn't transform to an offset");
            continue;
        };

        if let Some(t) = triples
            .0
            .iter()
            .filter(|triple| triple.span.contains(&offset))
            .min_by_key(|x| x.span.end - x.span.start)
        {
            let target = [
                (TripleTarget::Subject, &t.subject.span),
                (TripleTarget::Predicate, &t.predicate.span),
                (TripleTarget::Object, &t.object.span),
            ]
            .into_iter()
            .filter(|x| x.1.contains(&offset))
            .min_by_key(|x| x.1.end - x.1.start)
            .map(|x| x.0)
            .unwrap_or(TripleTarget::Subject);

            debug!("Current triple {} {:?}", t, target);
            commands.entity(e).insert(TripleComponent {
                triple: t.clone(),
                target,
            });
        } else {
            debug!("No current triple found");
        }
    }
}

#[derive(Debug, Clone)]
pub struct MyQuad<'a> {
    pub subject: MyTerm<'a>,
    pub predicate: MyTerm<'a>,
    pub object: MyTerm<'a>,
    pub span: std::ops::Range<usize>,
}
impl<'b, 'a> TryFrom<&'b MyQuad<'a>> for oxigraph::model::Quad {
    type Error = ();

    fn try_from(value: &'b MyQuad<'a>) -> std::result::Result<Self, Self::Error> {
        let subject = oxigraph::model::Term::try_from(&value.subject)?;
        let predicate = oxigraph::model::Term::try_from(&value.predicate)?;
        let object = oxigraph::model::Term::try_from(&value.object)?;

        let subject = oxigraph::model::NamedOrBlankNode::try_from(subject).map_err(|_| ())?;
        let predicate = oxigraph::model::NamedNode::try_from(predicate).map_err(|_| ())?;

        Ok(oxigraph::model::Quad::new(
            subject,
            predicate,
            object,
            oxigraph::model::GraphName::default(),
        ))
    }
}

impl<'a> MyQuad<'a> {
    pub fn into_oxi_graph(
        &self,
        graph: impl TryInto<oxigraph::model::Term>,
    ) -> Result<oxigraph::model::Quad, ()> {
        let graph = graph.try_into().map_err(|_| ())?;
        let graph = oxigraph::model::NamedOrBlankNode::try_from(graph).map_err(|_| ())?;
        let graph = oxigraph::model::GraphName::from(graph);

        self.into_oxi(Some(graph))
    }

    pub fn into_oxi(
        &self,
        graph: Option<oxigraph::model::GraphName>,
    ) -> Result<oxigraph::model::Quad, ()> {
        let subject = oxigraph::model::Term::try_from(&self.subject)?;
        let predicate = oxigraph::model::Term::try_from(&self.predicate)?;
        let object = oxigraph::model::Term::try_from(&self.object)?;
        let graph = graph.unwrap_or_default();

        let subject = oxigraph::model::NamedOrBlankNode::try_from(subject).map_err(|_| ())?;
        let predicate = oxigraph::model::NamedNode::try_from(predicate).map_err(|_| ())?;

        Ok(oxigraph::model::Quad::new(
            subject, predicate, object, graph,
        ))
    }
}

impl<'a> std::fmt::Display for MyQuad<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {}. # {:?}",
            self.subject, self.predicate, self.object, self.span
        )
    }
}

impl<'a> MyQuad<'a> {
    pub fn to_owned(&self) -> MyQuad<'static> {
        MyQuad {
            subject: self.subject.to_owned(),
            predicate: self.predicate.to_owned(),
            object: self.object.to_owned(),
            span: self.span.clone(),
        }
    }
}
impl<'a, 'b> TryFrom<&'b MyTerm<'a>> for oxigraph::model::Term {
    type Error = ();

    fn try_from(value: &'b MyTerm<'a>) -> std::result::Result<Self, Self::Error> {
        use oxigraph::model as M;
        use oxigraph::model::Term as T;
        let output = match &value.ty {
            Some(TermKind::Iri) => T::NamedNode(M::NamedNode::new(value.as_str()).map_err(|_| ())?),
            Some(TermKind::Literal) => {
                if let Some(dt) = value.datatype() {
                    let dt = M::NamedNode::new(dt.as_str()).map_err(|_| ())?;
                    T::Literal(M::Literal::new_typed_literal(value.value.as_ref(), dt))
                } else if let Some(lang) = value.language_tag() {
                    T::Literal(
                        M::Literal::new_language_tagged_literal(
                            value.value.as_ref(),
                            lang.as_str(),
                        )
                        .map_err(|_| ())?,
                    )
                } else {
                    T::Literal(M::Literal::new_simple_literal(value.value.as_ref()))
                }
            }
            Some(TermKind::BlankNode) => {
                T::BlankNode(M::BlankNode::new(value.value.as_ref()).map_err(|_| ())?)
            }
            _ => {
                return Err(());
            }
        };
        return Ok(output);
    }
}

impl<'a> Quad for MyQuad<'a> {
    type Term = MyTerm<'a>;

    fn s(&self) -> sophia_api::quad::QBorrowTerm<'_, Self> {
        self.subject.borrow_term()
    }

    fn p(&self) -> sophia_api::quad::QBorrowTerm<'_, Self> {
        self.predicate.borrow_term()
    }

    fn o(&self) -> sophia_api::quad::QBorrowTerm<'_, Self> {
        self.object.borrow_term()
    }

    fn g(&self) -> GraphName<sophia_api::quad::QBorrowTerm<'_, Self>> {
        None
    }

    fn to_spog(self) -> sophia_api::quad::Spog<Self::Term> {
        ([self.subject, self.predicate, self.object], None)
    }
}
// pub type MyQuad<'a> = ([MyTerm<'a>; 3], GraphName<MyTerm<'a>>);

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum TermContext<'a> {
    None,
    DataType(Cow<'a, str>),
    LangTag(Cow<'a, str>),
}
impl<'a> TermContext<'a> {
    pub fn to_owned(&self) -> TermContext<'static> {
        match self {
            TermContext::None => TermContext::None,
            TermContext::DataType(cow) => TermContext::DataType(Cow::Owned(cow.to_string())),
            TermContext::LangTag(cow) => TermContext::LangTag(Cow::Owned(cow.to_string())),
        }
    }
}

#[derive(Debug, Clone, Eq)]
pub struct MyTerm<'a> {
    pub value: Cow<'a, str>,
    pub ty: Option<TermKind>,
    pub span: std::ops::Range<usize>,
    pub context: TermContext<'a>,
}
impl<'a> PartialOrd for MyTerm<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.value.partial_cmp(&other.value) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        self.ty.partial_cmp(&other.ty)
    }
}
impl<'a> Ord for MyTerm<'a> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.value.cmp(&other.value) {
            core::cmp::Ordering::Equal => {}
            ord => return ord,
        }
        self.ty.cmp(&other.ty)
    }
}

impl Hash for MyTerm<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Ignore span
        self.value.hash(state);
        self.ty.hash(state);
    }
}

impl PartialEq for MyTerm<'_> {
    fn eq(&self, other: &Self) -> bool {
        // Ignore span
        other.value == self.value && other.ty == self.ty
    }
}

impl<'a> std::fmt::Display for MyTerm<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind() {
            TermKind::Iri => write!(f, "<{}>", self.value),
            TermKind::Literal => write!(f, "\"{}\"", self.value),
            TermKind::BlankNode => write!(f, "_:{}", self.value),
            TermKind::Triple => write!(f, "<{}>", self.value),
            TermKind::Variable => write!(f, "?{}", self.value),
        }
    }
}

impl<'a> MyTerm<'a> {
    pub fn to_owned(&self) -> MyTerm<'static> {
        let value = Cow::Owned(self.value.to_string());
        MyTerm {
            value,
            ty: self.ty.clone(),
            span: self.span.clone(),
            context: self.context.to_owned(),
        }
    }
    pub fn variable<T: Into<Cow<'a, str>>>(value: T, span: std::ops::Range<usize>) -> Self {
        Self {
            value: value.into(),
            ty: TermKind::Variable.into(),
            span,
            context: TermContext::None,
        }
    }
    pub fn named_node<T: Into<Cow<'a, str>>>(value: T, span: std::ops::Range<usize>) -> Self {
        Self {
            value: value.into(),
            ty: TermKind::Iri.into(),
            span,
            context: TermContext::None,
        }
    }
    pub fn blank_node<T: Into<Cow<'a, str>>>(value: T, span: std::ops::Range<usize>) -> Self {
        Self {
            value: value.into(),
            ty: TermKind::BlankNode.into(),
            span,
            context: TermContext::None,
        }
    }
    pub fn literal<T: Into<Cow<'a, str>>>(
        value: T,
        span: std::ops::Range<usize>,
        context: TermContext<'a>,
    ) -> Self {
        Self {
            value: value.into(),
            ty: TermKind::Literal.into(),
            span,
            context,
        }
    }

    pub fn invalid(span: std::ops::Range<usize>) -> Self {
        Self {
            value: Cow::default(),
            ty: TermKind::Iri.into(),
            span,

            context: TermContext::None,
        }
    }

    pub fn as_str(&'a self) -> &'a str {
        &self.value
    }
}

impl<'a> Term for MyTerm<'a> {
    type BorrowTerm<'x>
        = &'x Self
    where
        Self: 'x;

    fn kind(&self) -> sophia_api::term::TermKind {
        self.ty.unwrap_or(TermKind::Triple)
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        self
    }

    fn iri(&self) -> Option<sophia_api::term::IriRef<sophia_api::MownStr<'_>>> {
        self.is_iri()
            .then(|| IriRef::new_unchecked(MownStr::from_ref(&self.value)))
    }

    fn bnode_id(&self) -> Option<sophia_api::term::BnodeId<sophia_api::MownStr<'_>>> {
        self.is_blank_node()
            .then(|| BnodeId::new_unchecked(MownStr::from_ref(&self.value)))
    }

    fn lexical_form(&self) -> Option<sophia_api::MownStr<'_>> {
        self.is_literal().then(|| MownStr::from_ref(&self.value))
    }

    fn datatype(&self) -> Option<sophia_api::term::IriRef<sophia_api::MownStr<'_>>> {
        None
    }

    fn language_tag(&self) -> Option<sophia_api::term::LanguageTag<sophia_api::MownStr<'_>>> {
        None
    }

    fn variable(&self) -> Option<sophia_api::term::VarName<sophia_api::MownStr<'_>>> {
        panic!("MyTerm does not supported variables")
    }

    fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
        panic!("MyTerm does not supported triples")
    }

    fn to_triple(self) -> Option<[Self; 3]>
    where
        Self: Sized,
    {
        panic!("MyTerm does not supported triples")
    }
}

impl<'a> Borrow<str> for &MyTerm<'a> {
    fn borrow(&self) -> &str {
        &self.value
    }
}

#[derive(Default, Debug)]
pub struct Triples2<'a> {
    pub base_url: String,
    pub triples: Vec<MyQuad<'a>>,
    pub base: Option<MyTerm<'a>>,
}

impl<'a> Triples2<'a> {
    pub fn to_owned(&self) -> Triples2<'static> {
        let triples = self.triples.iter().map(|q| q.to_owned()).collect();
        let base: Option<MyTerm<'static>> = self.base.as_ref().map(|x| x.to_owned());

        Triples2 {
            base,
            triples,
            base_url: self.base_url.clone(),
        }
    }

    pub fn imports(&self, cb: impl FnMut(IriRef<MownStr<'_>>) -> ()) {
        if let Some(ref base) = self.base {
            self.triples
                .quads_matching([base], [owl::imports], Any, Any)
                .flatten()
                .flat_map(|s| s.o().iri())
                .for_each(cb);
        }
    }

    pub fn sub_class_of(&self, mut cb: impl FnMut(IriRef<MownStr<'_>>, IriRef<MownStr<'_>>) -> ()) {
        self.triples
            .quads_matching(Any, [rdfs::subClassOf], Any, Any)
            .flatten()
            .flat_map(|s| match (s.s().iri(), s.o().iri()) {
                (Some(s), Some(o)) => Some((s, o)),
                _ => None,
            })
            .for_each(|(x, y)| cb(x, y));
    }
}

impl<'a> std::ops::Deref for Triples2<'a> {
    type Target = Vec<MyQuad<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.triples
    }
}
