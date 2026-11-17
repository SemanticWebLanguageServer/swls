use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

use bevy_ecs::{prelude::*, world::CommandQueue};
use serde::Deserialize;
use sophia_api::{
    prelude::{Any, Dataset},
    quad::Quad,
    term::Term as _,
};
use tracing::{debug, error, span};

use super::setup::{namespace_sources, ConfiguredOntology, FromPrefix};
use crate::{
    lsp_types::{TextDocumentItem, Url},
    prelude::*,
    util::{fs::Fs, ns::owl},
};

#[derive(Deserialize, Debug)]
struct Version {
    #[serde(rename = "fileURL")]
    file_url: Option<String>,
    issued: chrono::DateTime<chrono::Utc>,
}

#[derive(Deserialize, Debug)]
struct Vocab {
    versions: Vec<Version>,
}

async fn extract_file_url(prefix: &str, client: &impl Client) -> Option<String> {
    let url = format!(
        "https://lov.linkeddata.es/dataset/lov/api/v2/vocabulary/info?vocab={}",
        prefix
    );
    match client.fetch(&url, &std::collections::HashMap::new()).await {
        Ok(resp) if resp.status == 200 => match serde_json::from_str::<Vocab>(&resp.body) {
            Ok(x) => {
                let versions: Vec<_> = x.versions.iter().flat_map(|x| &x.file_url).collect();
                debug!(
                    "Found lov response ({} versions) {:?}",
                    x.versions.len(),
                    versions
                );
                x.versions
                    .into_iter()
                    .flat_map(|x| x.file_url.map(|url| (url, x.issued)))
                    .max_by_key(|x| x.1)
                    .map(|x| x.0)
            }
            Err(e) => {
                error!("Deserialize failed ({}) {:?}", url, e);
                None
            }
        },
        Ok(resp) => {
            error!("Fetch ({}) failed status {}", url, resp.status);
            None
        }
        Err(e) => {
            error!("Fetch ({}) failed {:?}", url, e);
            None
        }
    }
}

pub fn open_imports<C: Client + Resource>(
    query: Query<(&Triples, &RopeC), Changed<Triples>>,
    mut opened: Local<HashSet<String>>,
    sender: Res<CommandSender>,
    fs: Res<Fs>,
    client: Res<C>,
) {
    for (triples, _) in &query {
        for object in triples
            .quads_matching(Any, [owl::imports], Any, Any)
            .flatten()
            .flat_map(|s| s.o().iri())
            .flat_map(|s| Url::parse(s.as_str()))
        {
            if opened.contains(object.as_str()) {
                continue;
            }
            opened.insert(object.as_str().to_string());

            let fs = fs.clone();
            let sender = sender.clone();
            let fut = async move {
                if let Some(content) = fs.0.read_file(&object).await {
                    spawn_document(object, content, &sender.0, |_, _| {});

                    let mut command_queue = CommandQueue::default();
                    command_queue.push(move |world: &mut World| {
                        world.run_schedule(SaveLabel);
                    });
                    let _ = sender.unbounded_send(command_queue);
                } else {
                    tracing::warn!("No content found for {}", object);
                }
            };
            client.spawn(fut);
        }
    }
}

/// Load the ontology documents for every prefix used in an open document.
///
/// Which documents are loaded, and under which label, is decided by
/// [`namespace_sources`]: configured ontologies each get their own document,
/// otherwise the built-in or remote LOV ontology is loaded at `<cache>/<prefix>.ttl`.
///
/// Runs for open documents whose prefixes changed, and for all open documents
/// when configured ontologies arrive (they are loaded asynchronously at startup,
/// possibly after the first document was opened).
pub fn fetch_lov_properties<C: Client + Resource>(
    sender: Res<CommandSender>,
    changed: Query<&Prefixes, (Or<((Changed<Prefixes>, With<Open>), Changed<Open>)>,)>,
    all_open: Query<&Prefixes, With<Open>>,
    new_configured: Query<(), Added<ConfiguredOntology>>,
    locals: Query<(&swls_lov::LocalPrefix, Option<&ConfiguredOntology>)>,
    mut loaded: Local<HashSet<String>>,
    client: Res<C>,
    fs: Res<Fs>,
) {
    let prefixes: Vec<&Prefixes> = if new_configured.is_empty() {
        changed.iter().collect()
    } else {
        all_open.iter().collect()
    };

    for prefs in prefixes {
        for prefix in prefs.0.iter() {
            for source in namespace_sources(&fs, prefix.url.as_str(), &prefix.prefix, &locals) {
                if !loaded.insert(source.url.to_string()) {
                    continue;
                }
                let sender = sender.0.clone();
                let c = client.as_ref().clone();
                match source.local {
                    Some(local) => {
                        debug!(
                            "Local ontology for prefix {} {} at {}",
                            prefix.prefix,
                            prefix.url.as_str(),
                            source.url
                        );
                        client.spawn(local_lov::<C>(
                            local.clone(),
                            source.url,
                            source.is_cache,
                            sender,
                            fs.clone(),
                            c,
                        ));
                    }
                    None => {
                        debug!(
                            "Remote lov for prefix {} {}",
                            prefix.prefix,
                            prefix.url.as_str()
                        );
                        client.spawn(fetch_lov(prefix.clone(), source.url, c, sender, fs.clone()));
                    }
                }
            }
        }
    }
}

type Sender = futures::channel::mpsc::UnboundedSender<CommandQueue>;

pub fn spawn_document(
    url: Url,
    content: String,
    sender: &Sender,
    extra: impl FnOnce(Entity, &mut World) -> () + Send + Sync + 'static,
) {
    let mut command_queue = CommandQueue::default();
    let item = TextDocumentItem {
        version: 1,
        uri: url.clone(),
        language_id: String::from("turtle"),
        text: String::new(),
    };

    let spawn = spawn_background(
        url.clone(),
        (
            RopeC(LineIndex::new(&content)),
            Source(content.clone()),
            Label(url.clone()),
            Wrapped(item),
            Types(HashMap::new()),
        ),
        Some("turtle".into()),
        (),
    );

    command_queue.push(move |world: &mut World| {
        let span = span!(tracing::Level::INFO, "span lov");
        let _enter = span.enter();
        let e = spawn(world);

        extra(e, world);

        world.run_schedule(ParseLabel);
    });

    let _ = sender.unbounded_send(command_queue);
}

/// Tag the spawned document with the prefix it was loaded for and, when `url` is
/// a cache location, write the content there so the editor can open it.
fn extra_from_lov<C: Client + Resource>(
    from: FromPrefix,
    content: String,
    url: Url,
    is_cache: bool,
    fs: Fs,
) -> impl FnOnce(Entity, &mut World) + Send + Sync + 'static {
    move |e, world| {
        world.entity_mut(e).insert(from);

        if is_cache {
            let client = world.resource::<C>();
            client.spawn(async move {
                fs.0.write_file(&url, &content).await;
            });
        }
    }
}

pub(super) async fn fetch_lov_body<C: Client + Resource>(
    prefix: &str,
    namespace: &str,
    c: C,
) -> Option<String> {
    // Try the LOV mirror first: one CDN request, no JSON parsing.
    let mirror_url = format!(
        "https://ajuvercr.github.io/lov-mirror/by-prefix/{}/ontology.ttl",
        prefix
    );
    match c
        .fetch(&mirror_url, &std::collections::HashMap::new())
        .await
    {
        Ok(resp) if resp.status == 200 => {
            tracing::warn!("Mirror hit for prefix {}", prefix);
            return Some(resp.body);
        }
        Ok(resp) => {
            tracing::warn!(
                "Mirror miss for prefix {} (status {}), falling back to LOV API",
                prefix,
                resp.status
            );
        }
        Err(e) => {
            tracing::warn!(
                "Mirror fetch failed for prefix {} ({:?}), falling back to LOV API",
                prefix,
                e
            );
        }
    }

    // Fall back to the LOV API: fetch vocabulary info to find the latest file URL.
    if namespace.ends_with('#') || namespace.ends_with('/') {
        if let Some(url) = extract_file_url(&prefix, &c).await {
            match c.fetch(&url, &std::collections::HashMap::new()).await {
                Ok(resp) if resp.status == 200 => return Some(resp.body),
                Ok(resp) => {
                    error!("Fetch ({}) failed status {}", url, resp.status);
                }
                Err(e) => {
                    error!("Fetch ({}) failed {:?}", url, e);
                }
            }
        }
    }
    None
}

async fn fetch_lov<C: Client + Resource>(prefix: Prefix, label: Url, c: C, sender: Sender, fs: Fs) {
    tracing::debug!("Fetching LOV for prefix {}", prefix.prefix);
    if prefix.prefix.to_ascii_lowercase() == prefix.prefix {
        if let Some(body) = fetch_lov_body(&prefix.prefix, prefix.url.as_str(), c).await {
            let extra =
                extra_from_lov::<C>(FromPrefix(prefix), body.clone(), label.clone(), true, fs);
            spawn_document(label, body, &sender, extra);
        }
    }
}

async fn local_lov<C: Client + Resource>(
    local: swls_lov::LocalPrefix,
    label: Url,
    is_cache: bool,
    sender: Sender,
    fs: Fs,
    c: C,
) {
    tracing::debug!(
        "Using local {} {} Label {}",
        local.name,
        local.namespace,
        label.as_str()
    );
    let content = if local.content.is_empty() {
        tracing::debug!("Fetching from LOV {}", local.name);
        if let Some(body) = fetch_lov_body(&local.name, &local.namespace, c).await {
            Cow::Owned(body)
        } else {
            return;
        }
    } else {
        local.content
    };

    let Ok(namespace) = Url::parse(&local.namespace) else {
        tracing::warn!("Invalid namespace {} for {}", local.namespace, local.name);
        return;
    };
    let from = FromPrefix(Prefix {
        prefix: local.name.to_string(),
        url: namespace,
    });

    let extra = extra_from_lov::<C>(from, content.to_string(), label.clone(), is_cache, fs);
    spawn_document(label, content.to_string(), &sender, extra);
}
