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

    world.schedule_scope(swls_core::feature::DiagnosticsLabel, |_, schedule| {
        schedule.add_systems(publish_diagnostics::<L>);
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

    fn add_term(term: &Spanned<Term>, ttc: &mut TokenTypesComponent, kind: SemanticTokenType) {
        match term.value() {
            Term::NamedNode(NamedNode::Prefixed { prefix, .. }) => {
                let skip = prefix.len();
                let (start, end) = (term.span().start, term.span().end);
                ttc.push(spanned(kind, start + skip + 1..end));
            }
            Term::Variable(_) | Term::NamedNode(_) => {
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
                        add_term(o, ttc, kind.clone());
                    }
                }
            }
            Term::Collection(spanneds) => {
                for e in spanneds {
                    add_term(e, ttc, kind.clone());
                }
            }
            _ => return,
        }
    }

    pub fn semantic_tokens<L: Lang<Element = Turtle> + Component>(
        query: Query<(&Element<L>, &mut TokenTypesComponent), With<HighlightRequest>>,
    ) {
        for (turtle, mut ttc) in query {
            for t in &turtle.triples {
                add_term(&t.subject, &mut ttc, SemanticTokenType::ENUM_MEMBER);
                for po in &t.po {
                    for o in &po.object {
                        add_term(o, &mut ttc, SemanticTokenType::ENUM_MEMBER);
                    }
                }
            }
        }
    }
}
