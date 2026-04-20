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
use tracing::{debug, error, info, span};

use super::setup::FromPrefix;
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
                    info!("No content found for {}", object);
                }
            };
            client.spawn(fut);
        }
    }
}

/// First of all, fetch the lov dataset information at url <https://lov.linkeddata.es/dataset/lov/api/v2/vocabulary/info?vocab=${prefix}>
/// Next, extract that json object into an object and find the latest dataset
pub fn fetch_lov_properties<C: Client + Resource>(
    sender: Res<CommandSender>,
    query: Query<&Prefixes, (Or<((Changed<Prefixes>, With<Open>), Changed<Open>)>,)>,
    ontologies: Query<(Entity, &swls_lov::LocalPrefix)>,
    mut prefixes: Local<HashSet<String>>,
    client: Res<C>,
    fs: Res<Fs>,
) {
    for prefs in &query {
        for prefix in prefs.0.iter() {
            if !prefixes.contains(prefix.url.as_str()) {
                prefixes.insert(prefix.url.to_string());
                let mut found = false;
                for (_e, local) in ontologies
                    .iter()
                    .filter(|(_, x)| x.namespace == prefix.url.as_str())
                {
                    debug!(
                        "Local lov for Prefix {} {} is entry {} {} {} {}",
                        prefix.prefix,
                        prefix.url.as_str(),
                        local.name,
                        local.namespace,
                        local.location,
                        local.content.is_empty()
                    );

                    let label = match crate::lsp_types::Url::parse(&local.location) {
                        Ok(label) => label,
                        Err(e) => {
                            tracing::error!(
                                "Failed to parse defined prefix location {} {:?}",
                                local.location,
                                e
                            );
                            continue;
                        }
                    };

                    found = true;
                    let c = client.as_ref().clone();
                    let sender = sender.0.clone();
                    client.spawn(local_lov::<C>(local.clone(), label, sender, fs.clone(), c));
                }

                if !found {
                    if let Some(url) = fs.0.lov_url(prefix.url.as_str(), &prefix.prefix) {
                        debug!(
                            "Remote lov for prefix {} {}",
                            prefix.prefix,
                            prefix.url.as_str()
                        );
                        let sender = sender.0.clone();
                        let c = client.as_ref().clone();
                        client.spawn(fetch_lov(
                            prefix.clone(),
                            url.clone(),
                            c,
                            sender,
                            fs.clone(),
                        ));
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

    let spawn = spawn_or_insert(
        url.clone(),
        (
            RopeC(ropey::Rope::from_str(&content)),
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

fn extra_from_lov<C: Client + Resource>(
    from: FromPrefix,
    content: String,
    url: Url,
    fs: Fs,
) -> impl FnOnce(Entity, &mut World) + Send + Sync + 'static {
    move |e, world| {
        world.entity_mut(e).insert(from);

        let client = world.resource::<C>();
        client.spawn(async move {
            fs.0.write_file(&url, &content).await;
        });
    }
}

pub(super) async fn fetch_lov_body<C: Client + Resource>(
    prefix: &str,
    namespace: &str,
    c: C,
) -> Option<String> {
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
    tracing::info!("A FUTURE IS STARTING");
    if prefix.prefix.to_ascii_lowercase() == prefix.prefix {
        if let Some(body) = fetch_lov_body(&prefix.prefix, prefix.url.as_str(), c).await {
            let extra = extra_from_lov::<C>(FromPrefix(prefix), body.clone(), label.clone(), fs);
            spawn_document(label, body, &sender, extra);
        }
    }
}

async fn local_lov<C: Client + Resource>(
    local: swls_lov::LocalPrefix,
    label: Url,
    sender: Sender,
    fs: Fs,
    c: C,
) {
    info!(
        "Using local {} {} Label {}",
        local.name,
        local.namespace,
        label.as_str()
    );
    let content = if local.content.is_empty() {
        info!("Fetching from LOV {}", local.name);
        if let Some(body) = fetch_lov_body(&local.name, &local.namespace, c).await {
            Cow::Owned(body)
        } else {
            return;
        }
    } else {
        local.content
    };

    let from = FromPrefix(Prefix {
        prefix: local.name.to_string(),
        url: Url::parse(&local.namespace).unwrap(),
    });

    let extra = extra_from_lov::<C>(from, content.to_string(), label.clone(), fs);
    spawn_document(label, content.to_string(), &sender, extra);
}
