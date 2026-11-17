use bevy_ecs::prelude::*;
use tracing::instrument;

use crate::{
    lsp_types::Url,
    prelude::*,
    systems::{import_url, namespace_sources, ConfiguredOntology},
    util::ns::owl,
};

/// Link every document to the ontology documents of the prefixes it declares.
///
/// The targets come from [`namespace_sources`], the same function the fetch
/// system uses, so a link always names a document the server actually loads,
/// including one link per configured ontology when several share a namespace.
pub fn derive_prefix_links(
    mut query: Query<(Entity, Ref<Prefixes>, Option<&mut DocumentLinks>)>,
    locals: Query<(&swls_lov::LocalPrefix, Option<&ConfiguredOntology>)>,
    new_configured: Query<(), Added<ConfiguredOntology>>,
    mut commands: Commands,
    fs: Res<Fs>,
) {
    const SOURCE: &'static str = "prefix import";
    let refresh_all = !new_configured.is_empty();
    for (e, prefixes, mut links) in &mut query {
        if !refresh_all && !prefixes.is_changed() {
            continue;
        }
        let mut new_links = Vec::new();
        for u in prefixes.0.iter() {
            let sources = namespace_sources(&fs, u.url.as_str(), &u.prefix, &locals);
            if sources.is_empty() {
                new_links.push((u.url.clone(), SOURCE));
            }
            new_links.extend(sources.into_iter().map(|s| (s.url, SOURCE)));
        }
        if let Some(links) = links.as_mut() {
            links.retain(|e| e.1 != SOURCE);
        }
        match (new_links.is_empty(), links) {
            (false, None) => {
                commands.entity(e).insert(DocumentLinks(new_links));
            }
            (false, Some(mut links)) => {
                links.extend(new_links);
            }
            _ => {}
        }
    }
}

/// Link every document to the documents it `owl:imports`, as loaded by
/// [`open_imports`](crate::systems::open_imports) (see [`import_url`]).
#[instrument(skip(query, commands, fs))]
pub fn derive_owl_imports_links(
    mut query: Query<(Entity, &Label, &Triples, Option<&mut DocumentLinks>), Changed<Triples>>,
    mut commands: Commands,
    fs: Res<Fs>,
) {
    const SOURCE: &'static str = "owl:imports";
    for (e, label, triples, mut links) in &mut query {
        if let Some(links) = links.as_mut() {
            links.retain(|e| e.1 != SOURCE);
        }

        let new_links: Vec<_> = triples
            .0
            .iter()
            .filter(|t| t.predicate.as_str() == owl::imports.iriref().as_str())
            .flat_map(|t| Url::parse(t.object.as_str()))
            .flat_map(|iri| import_url(&fs, &iri))
            .map(|obj| (obj, SOURCE))
            .collect();

        for (u, _) in &new_links {
            tracing::debug!("owl:imports {} to {}", label.as_str(), u);
        }

        if !new_links.is_empty() {
            match links {
                Some(mut links) => {
                    links.extend(new_links);
                }
                None => {
                    commands.entity(e).insert(DocumentLinks(new_links));
                }
            }
        }
    }
}
