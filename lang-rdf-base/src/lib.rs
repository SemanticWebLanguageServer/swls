pub mod code_actions;
pub mod parse;
pub mod rename;
pub mod traits;
pub mod triples;

use bevy_ecs::{
    component::Component, event::EntityEvent, observer::On, system::Commands, world::World,
};
use rdf_parsers::model::Turtle;
use swls_core::{
    feature::{
        diagnostics::publish_diagnostics,
        semantic::{basic_semantic_tokens, semantic_tokens_system},
    },
    lang::{Lang, LangHelper},
    prelude::*,
    CreateEvent,
};

/// Register a language with the ECS world, handling all `setup_world()` boilerplate:
/// - Semantic token type registration in `SemanticTokensDict`
/// - `CreateEvent` observer for file detection by `language_id` or extension
/// - Diagnostics system scheduling
/// - Basic semantic highlighting scheduling
///
/// Call language-specific setup (parsing, completion, code actions, etc.) after this function.
///
/// # Type parameters
/// - `L`: The language marker component (e.g. `TurtleLang`). Must implement `Default`.
/// - `H`: The language helper (e.g. `TurtleHelper`). Must implement `Default`.
pub fn register_rdf_lang<L, H>(
    world: &mut World,
    language_id: &'static [&'static str],
    extensions: &'static [&'static str],
) where
    L: Lang<Element = Turtle> + Component + Default + Send + Sync + 'static,
    L::ElementError: 'static + Clone,
    H: LangHelper + Default + Send + Sync + 'static,
{
    let mut semantic_token_dict = world.resource_mut::<SemanticTokensDict>();
    L::LEGEND_TYPES.iter().for_each(|lt| {
        if !semantic_token_dict.contains_key(lt) {
            let l = semantic_token_dict.0.len();
            semantic_token_dict.insert(lt.clone(), l);
        }
    });

    world.add_observer(move |trigger: On<CreateEvent>, mut commands: Commands| {
        let e = trigger.event();
        let matches = trigger
            .language_id
            .as_ref()
            .map(|lang_id| language_id.iter().any(|x| x == lang_id))
            .unwrap_or_default()
            || extensions.iter().any(|ext| e.url.as_str().ends_with(ext));
        if matches {
            commands
                .entity(e.event_target())
                .insert(L::default())
                .insert(DynLang(Box::new(H::default())));
        }
    });

    world.schedule_scope(swls_core::feature::ParseLabel, |_, schedule| {
        use bevy_ecs::schedule::IntoScheduleConfigs;
        schedule.add_systems(publish_diagnostics::<L>.after(swls_core::feature::parse::end));
    });

    world.schedule_scope(swls_core::feature::SemanticLabel, |_, schedule| {
        use bevy_ecs::schedule::IntoScheduleConfigs;
        schedule.add_systems((
            basic_semantic_tokens::<L>.before(semantic_tokens_system),
            semantic_tokens::<L>
                .after(basic_semantic_tokens::<L>)
                .before(semantic_tokens_system),
        ));
    });
}

pub use tokens::semantic_tokens;
mod tokens {
    use bevy_ecs::prelude::*;
    use rdf_parsers::model::{BlankNode, NamedNode, Term, Turtle};
    use swls_core::lsp_types::SemanticTokenType;
    use swls_core::prelude::semantic::*;
    use swls_core::prelude::*;

    /// True when `span` covers a JSON-LD node object (`{ … }`) rather than a
    /// plain term.  Objects with an `@id` become a [`NamedNode`] whose span is
    /// the whole `{ … }`; stamping that would wipe the inner coloring.
    fn span_is_nested_object(span: &std::ops::Range<usize>, source: &str) -> bool {
        source
            .get(span.clone())
            .map(|s| s.trim_start().starts_with('{'))
            .unwrap_or(false)
    }

    fn add_term(
        term: &Spanned<Term>,
        ttc: &mut TokenTypesComponent,
        kind: SemanticTokenType,
        source: &str,
    ) {
        match term.value() {
            Term::NamedNode(NamedNode::Prefixed { prefix, .. }) => {
                let skip = prefix.len();
                let (start, end) = (term.span().start, term.span().end);
                ttc.push(spanned(kind, start + skip + 1..end));
            }
            Term::Variable(_) | Term::NamedNode(_) => {
                // A JSON-LD node object with an @id becomes a NamedNode whose
                // span covers the entire nested { } object.  Stamping it would
                // wipe the inner coloring (same reasoning as anonymous blank
                // nodes below); the @id is already colored as the subject of the
                // inner triples.
                if span_is_nested_object(term.span(), source) {
                    return;
                }
                ttc.push(spanned(kind, term.span().clone()));
            }
            // Named blank nodes (_:label) get their coloring from the CST pass
            // (NAMESPACE+PROPERTY via BlankNodeLabel token kind in Turtle).  For
            // JSON-LD, anonymous blank nodes have a Spanned span that covers the
            // entire nested { } object, so stamping ENUM_MEMBER here would wipe
            // out all inner coloring — inner triples handle their own content.
            Term::BlankNode(BlankNode::Named(_, _)) => return,
            Term::BlankNode(BlankNode::Unnamed(pos, _, _)) => {
                for po in pos {
                    for o in &po.object {
                        add_term(o, ttc, kind.clone(), source);
                    }
                }
            }
            Term::Collection(spanneds) => {
                for e in spanneds {
                    add_term(e, ttc, kind.clone(), source);
                }
            }
            _ => return,
        }
    }

    pub fn semantic_tokens<L: Lang<Element = Turtle> + Component>(
        query: Query<(&Element<L>, &Source, &mut TokenTypesComponent), With<HighlightRequest>>,
    ) {
        for (turtle, source, mut ttc) in query {
            let source = source.0.as_str();
            for t in &turtle.triples {
                add_term(&t.subject, &mut ttc, SemanticTokenType::ENUM_MEMBER, source);
                for po in &t.po {
                    for o in &po.object {
                        add_term(o, &mut ttc, SemanticTokenType::ENUM_MEMBER, source);
                    }
                }
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        fn full(iri: &str, offset: usize, span: std::ops::Range<usize>) -> Spanned<Term> {
            spanned(Term::NamedNode(NamedNode::Full(iri.into(), offset)), span)
        }

        // A JSON-LD node object with @id is a NamedNode whose span covers the
        // whole `{ … }`; it must NOT be stamped (it would wipe inner coloring).
        // A plain IRI reference of the same node must still be stamped.
        #[test]
        fn nested_named_object_is_skipped() {
            let source = r#"{ "@id": "http://ex/x", "name": "n" }"#;
            let mut ttc: TokenTypesComponent = Wrapped(Vec::new());
            // Whole-object span (0..source.len()) — the conformsTo-style object.
            add_term(
                &full("http://ex/x", 9, 0..source.len()),
                &mut ttc,
                SemanticTokenType::ENUM_MEMBER,
                source,
            );
            assert!(ttc.0.is_empty(), "nested {{ }} object should be skipped");

            // The @id reference itself (just the IRI token) must be stamped.
            let id_src = r#""http://ex/x""#;
            add_term(
                &full("http://ex/x", 0, 0..id_src.len()),
                &mut ttc,
                SemanticTokenType::ENUM_MEMBER,
                id_src,
            );
            assert_eq!(ttc.0.len(), 1, "plain IRI reference should be stamped");
        }
    }
}
