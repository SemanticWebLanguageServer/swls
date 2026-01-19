use bevy_ecs::prelude::*;

use crate::prelude::*;

#[cfg(feature = "shapes")]
mod shapes;
use completion::{CompletionRequest, SimpleCompletion};
#[cfg(feature = "shapes")]
pub use shapes::*;
mod typed;
pub use typed::*;
mod links;
pub use links::*;
pub mod prefix;
use crate::lsp_types::CompletionItemKind;
mod properties;
pub use properties::{
    complete_class, complete_properties, derive_ontologies, hover_class, hover_property,
    DefinedClass, DefinedClasses, DefinedProperties, DefinedProperty,
};
mod lov;
pub use lov::{
    check_added_ontology_extract, fetch_lov_properties, init_onology_extractor, open_imports,
    populate_known_ontologies, FromPrefix, OntologyExtractor,
};
use tracing::instrument;

pub fn spawn_or_insert(
    url: crate::lsp_types::Url,
    bundle: impl Bundle,
    language_id: Option<String>,
    extra: impl Bundle,
) -> impl (FnOnce(&mut World) -> Entity) + 'static + Send + Sync {
    move |world: &mut World| {
        let out = if let Some(entity) = world
            .query::<(Entity, &Label)>()
            .iter(&world)
            .find(|x| x.1 .0 == url)
            .map(|x| x.0)
        {
            world.entity_mut(entity).insert(bundle).insert(extra);
            entity
        } else {
            let entity = world.spawn(bundle).insert(extra).id();
            world.trigger(CreateEvent {
                url,
                language_id,
                entity,
            });
            entity
        };

        world.flush();
        world.run_schedule(ParseLabel);
        out
    }
}

pub fn handle_tasks(mut commands: Commands, mut receiver: ResMut<CommandReceiver>) {
    while let Ok(Some(mut com)) = receiver.0.try_next() {
        commands.append(&mut com);
    }
}

#[instrument(skip(query))]
pub fn keyword_complete(
    mut query: Query<(
        Option<&TokenComponent>,
        &PositionComponent,
        &DynLang,
        &mut CompletionRequest,
    )>,
) {
    tracing::debug!("Keyword complete!");
    for (m_token, position, helper, mut req) in &mut query {
        let range = if let Some(ct) = m_token {
            ct.range
        } else {
            crate::lsp_types::Range {
                start: position.0,
                end: position.0,
            }
        };

        for kwd in helper.keyword() {
            let completion = SimpleCompletion::new(
                CompletionItemKind::KEYWORD,
                kwd.to_string(),
                crate::lsp_types::TextEdit {
                    range: range.clone(),
                    new_text: kwd.to_string(),
                },
            );
            req.push(completion);
        }
    }
}
