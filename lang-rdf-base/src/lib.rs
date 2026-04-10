pub mod triples;

use bevy_ecs::{component::Component, event::EntityEvent, observer::On, system::Commands, world::World};
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
    language_id: &'static str,
    extensions: &'static [&'static str],
) where
    L: Lang + Component + Default + Send + Sync + 'static,
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
        let matches = e.language_id.as_deref() == Some(language_id)
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
        schedule.add_systems(basic_semantic_tokens::<L>.before(semantic_tokens_system));
    });
}
