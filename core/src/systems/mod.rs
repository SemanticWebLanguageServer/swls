use bevy_ecs::prelude::*;

use crate::prelude::*;

mod shapes;
use completion::{CompletionRequest, SimpleCompletion};
pub use shapes::*;
mod typed;
pub use typed::*;
mod links;
pub use links::*;
mod imports;
pub use imports::{import_url, open_imports};
pub mod prefix;
mod prefix_diagnostics;
pub use prefix_diagnostics::{
    add_missing_prefix_code_action, prefix_diagnostic_helper, prefix_diagnostics,
};
mod prefix_hover;
pub use prefix_hover::{get_current_prefix, goto_prefix, hover_prefix, PrefixComponent};
mod namespace_properties;
use crate::lsp_types::CompletionItemKind;
pub use namespace_properties::{
    unknown_property_code_action, validate_namespace_properties, ALLOW_PROPERTY_COMMAND,
};
mod properties;
pub use properties::{
    complete_class, complete_properties, derive_ontologies, hover_class, hover_excluded_property,
    hover_property, DefinedClass, DefinedClasses, DefinedProperties, DefinedProperty,
};
mod lov;
pub use lov::{
    check_added_ontology_extract, configured_url, fetch_lov_properties, init_ontology_extractor,
    namespace_sources, populate_known_ontologies, spawn_document, ConfiguredOntology, FromPrefix,
    NamespaceSource, OntologyExtractor, PreferredPrefixes, PrefixEntry,
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
            .iter(world)
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

        // Several systems add links (prefixes, owl:imports). If the component
        // were missing, each would insert its own through deferred commands in
        // the same pass, and only the last one would survive.
        if !world.entity(out).contains::<DocumentLinks>() {
            world.entity_mut(out).insert(DocumentLinks(Vec::new()));
        }

        world.flush();
        world.run_schedule(ParseLabel);
        out
    }
}

/// Like [`spawn_or_insert`], for documents the server loads in the background
/// (LOV ontologies, `owl:imports`, configured ontologies and shapes).
///
/// If the user has a document with the same URL [`Open`], their buffer is the
/// source of truth: `content` is dropped and only `extra` is added. Without this
/// guard a late fetch, or an `owl:imports` of a file being edited, would replace
/// the editor's text with the copy on disk.
pub fn spawn_background(
    url: crate::lsp_types::Url,
    content: impl Bundle,
    language_id: Option<String>,
    extra: impl Bundle,
) -> impl (FnOnce(&mut World) -> Entity) + 'static + Send + Sync {
    move |world: &mut World| {
        let open = world
            .query_filtered::<(Entity, &Label), With<Open>>()
            .iter(world)
            .find(|x| x.1 .0 == url)
            .map(|x| x.0);
        if let Some(entity) = open {
            tracing::debug!(
                "{} is open in the editor, keeping the editor's content",
                url
            );
            world.entity_mut(entity).insert((extra, Background));
            return entity;
        }
        spawn_or_insert(url, content, language_id, (extra, Background))(world)
    }
}

pub fn handle_tasks(mut commands: Commands, mut receiver: ResMut<CommandReceiver>) {
    while let Ok(mut com) = receiver.0.try_recv() {
        commands.append(&mut com);
    }
}

#[instrument(skip(query, config))]
pub fn keyword_complete(
    mut query: Query<(
        Option<&TokenComponent>,
        &PositionComponent,
        &DynLang,
        &mut CompletionRequest,
    )>,
    config: Res<ServerConfig>,
) {
    if config.config.local.is_disabled(Disabled::CompletionKeyword) {
        return;
    }
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
                    range,
                    new_text: kwd.to_string(),
                },
            );
            req.push(completion);
        }
    }
}
