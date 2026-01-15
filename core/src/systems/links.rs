use crate::lsp_types::Url;
use bevy_ecs::prelude::*;
use tracing::instrument;

use crate::{prelude::*, util::ns::owl};

pub fn derive_prefix_links(
    mut query: Query<(Entity, &Prefixes, Option<&mut DocumentLinks>), Changed<Prefixes>>,
    mut commands: Commands,
    // helper: Res<LovHelper>,
    fs: Res<Fs>,
) {
    const SOURCE: &'static str = "prefix import";
    for (e, prefixes, mut links) in &mut query {
        let mut new_links = Vec::new();
        for u in prefixes.0.iter() {
            let url: Url =
                fs.0.lov_url(u.url.as_str(), &u.prefix)
                    .unwrap_or(u.url.clone());
            tracing::debug!(
                "Mapping prefix {}: {} to {}",
                u.prefix,
                u.url.as_str(),
                url.as_str()
            );

            new_links.push((url.clone(), SOURCE));
        }
        if let Some(links) = links.as_mut() {
            links.retain(|e| e.1 != SOURCE);
        }
        // let new_links: Vec<_> = turtle.0.iter().map(|u| (u.url.clone(), SOURCE)).collect();
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

#[instrument(skip(query, commands))]
pub fn derive_owl_imports_links(
    mut query: Query<(Entity, &Label, &Triples, Option<&mut DocumentLinks>), Changed<Triples>>,
    mut commands: Commands,
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
