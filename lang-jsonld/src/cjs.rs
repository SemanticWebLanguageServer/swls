use std::collections::HashSet;

use bevy_ecs::{
    entity::Entity,
    system::{Commands, Local, Query, Res},
};
use rdf_parsers::jsonld::convert::JsonLdVal;
use swls_core::{
    lsp_types::Url,
    prelude::{Label, Wrapped},
};

use crate::{ecs::ContextRequest, Registry};

pub fn serde_to_jsonld(value: &serde_json::Value) -> JsonLdVal {
    match value {
        serde_json::Value::Object(map) => {
            let members = map
                .iter()
                .map(|(k, v)| (k.to_string(), 0..0, 0..0, serde_to_jsonld(v)))
                .collect();
            JsonLdVal::Object(members, 0..0)
        }
        serde_json::Value::Array(arr) => {
            JsonLdVal::Array(arr.into_iter().map(serde_to_jsonld).collect())
        }
        serde_json::Value::String(s) => JsonLdVal::Str(s.clone()),
        serde_json::Value::Number(n) => JsonLdVal::Number(n.to_string()),
        serde_json::Value::Bool(b) => JsonLdVal::Bool(*b),
        serde_json::Value::Null => JsonLdVal::Null,
    }
}

#[tracing::instrument(skip(requests, json_ld_vals, res, commander, found))]
pub fn cjs_loader(
    requests: Query<&ContextRequest>,
    json_ld_vals: Query<(Entity, &Label)>,
    res: Res<Registry>,
    mut commander: Commands,
    mut found: Local<HashSet<String>>,
) {
    for r in requests {
        tracing::info!("jsonld loader {}", r.url.as_str());

        if found.contains(r.url.as_str()) {
            continue;
        }

        if let Some(ctx) = res.1.contexts.get(r.url.as_str()) {
            tracing::info!("jsonld loader {} found context", r.url.as_str());
            found.insert(r.url.as_str().to_string());
            let val = serde_to_jsonld(ctx);

            let mut found = false;

            for (e, l) in json_ld_vals.iter() {
                if l.as_str() == r.url.as_str() {
                    tracing::info!("Found entity {}", r.url);
                    found = true;
                    commander.entity(e).insert(Wrapped(val.clone()));
                }
            }

            if !found {
                if let Ok(l) = Url::parse(r.url.as_str()) {
                    tracing::info!("Spawning {}", r.url);
                    commander.spawn((Label(l), Wrapped(val)));
                }
            }
        } else {
            let mut keys: Vec<_> = res.1.contexts.keys().collect();
            keys.sort();
            tracing::info!("no context found in {:?}", keys);
        }
    }
}
