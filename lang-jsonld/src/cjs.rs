use std::collections::HashSet;

use bevy_ecs::{
    entity::Entity,
    system::{Commands, Local, Query, Res},
};
use swls_core::{
    lsp_types::Url,
    prelude::{Label, Wrapped},
};

use crate::{ecs::ContextRequest, Registry};

#[tracing::instrument(skip(requests, json_ld_vals, res, commander, found))]
pub fn cjs_loader(
    requests: Query<&ContextRequest>,
    json_ld_vals: Query<(Entity, &Label)>,
    res: Res<Registry>,
    mut commander: Commands,
    mut found: Local<HashSet<String>>,
) {
    for r in requests {
        tracing::debug!("jsonld loader {}", r.url.as_str());

        if found.contains(r.url.as_str()) {
            continue;
        }

        if let Some(val) = res.1.contexts.get(r.url.as_str()) {
            tracing::debug!("jsonld loader {} found context", r.url.as_str());
            found.insert(r.url.as_str().to_string());

            let mut found = false;

            for (e, l) in json_ld_vals.iter() {
                if l.as_str() == r.url.as_str() {
                    tracing::debug!("Found entity {}", r.url);
                    found = true;
                    commander.entity(e).insert(Wrapped(val.clone()));
                }
            }

            if !found {
                if let Ok(l) = Url::parse(r.url.as_str()) {
                    tracing::debug!("Spawning {}", r.url);
                    commander.spawn((Label(l), Wrapped(val.clone())));
                }
            }
        } else {
            tracing::warn!("no context found for {}", r.url.as_str());
        }
    }
}
