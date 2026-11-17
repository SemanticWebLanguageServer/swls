use std::collections::{HashMap, HashSet};

use bevy_ecs::{prelude::*, world::CommandQueue};
use serde::Deserialize;
use tracing::{debug, error, span};

use super::setup::{namespace_sources, ConfiguredOntology, FromPrefix, PreferredPrefixes};
use crate::{
    lsp_types::{TextDocumentItem, Url},
    prelude::*,
    util::{cache, fs::Fs},
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
    preferred: Res<PreferredPrefixes>,
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
            for source in
                namespace_sources(&fs, prefix.url.as_str(), &prefix.prefix, &preferred, locals)
            {
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
                        // Fetch by the vocabulary's preferred name, not the typed one.
                        let canonical = Prefix {
                            prefix: source.prefix.clone(),
                            url: prefix.url.clone(),
                        };
                        client.spawn(fetch_lov(canonical, source.url, c, sender, fs.clone()));
                    }
                }
            }
        }
    }
}

type Sender = futures::channel::mpsc::UnboundedSender<CommandQueue>;

/// Load a background document with `content` at `url` and parse it.
///
/// `language` is the language id. Pass `None` to let the file extension of
/// `url` decide: a document matches every language whose id *or* extension
/// matches, so forcing `turtle` on a `.jsonld` URL would give it both.
pub fn spawn_document(
    url: Url,
    content: String,
    language: Option<String>,
    sender: &Sender,
    extra: impl FnOnce(Entity, &mut World) + Send + Sync + 'static,
) {
    let mut command_queue = CommandQueue::default();
    let item = TextDocumentItem {
        version: 1,
        uri: url.clone(),
        language_id: language.clone().unwrap_or_default(),
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
        language,
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

/// Tag the spawned document with the prefix it was loaded for.
fn extra_from_lov(from: FromPrefix) -> impl FnOnce(Entity, &mut World) + Send + Sync + 'static {
    move |e, world| {
        world.entity_mut(e).insert(from);
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
        if let Some(url) = extract_file_url(prefix, &c).await {
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

/// Load the ontology for a prefix without a built-in or configured entry, from
/// the cache when fresh, otherwise from the LOV mirror / API.
async fn fetch_lov<C: Client + Resource>(prefix: Prefix, label: Url, c: C, sender: Sender, fs: Fs) {
    tracing::debug!("Fetching LOV for prefix {}", prefix.prefix);
    if prefix.prefix.to_ascii_lowercase() != prefix.prefix {
        return;
    }
    let namespace = prefix.url.as_str();
    let body = cache::cached(&fs, &label, || async {
        let body = fetch_lov_body(&prefix.prefix, namespace, c).await?;
        // LOV is queried by name, so make sure the vocabulary we got actually
        // describes this namespace before caching and loading it.
        if !mentions_namespace(&body, namespace) {
            tracing::warn!(
                "LOV vocabulary {} does not describe {}, ignoring it",
                prefix.prefix,
                namespace
            );
            return None;
        }
        Some(body)
    })
    .await;
    if let Some(body) = body {
        spawn_document(
            label,
            body,
            Some("turtle".into()),
            &sender,
            extra_from_lov(FromPrefix(prefix)),
        );
    }
}

/// Load a built-in or configured ontology. Its content is known, it is only
/// written to the cache (when `label` is a cache location) so the editor can
/// open it.
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
        let fetched = cache::cached(&fs, &label, || {
            fetch_lov_body(&local.name, &local.namespace, c)
        })
        .await;
        let Some(body) = fetched else {
            return;
        };
        body
    } else {
        if is_cache {
            cache::store(&fs, &label, &local.content).await;
        }
        local.content.to_string()
    };

    let Ok(namespace) = Url::parse(&local.namespace) else {
        tracing::warn!("Invalid namespace {} for {}", local.namespace, local.name);
        return;
    };
    let from = FromPrefix(Prefix {
        prefix: local.name.to_string(),
        url: namespace,
    });

    spawn_document(
        label,
        content,
        Some("turtle".into()),
        &sender,
        extra_from_lov(from),
    );
}

/// Whether `body` mentions `namespace`, also accepting the other of `http` and
/// `https` (many vocabularies are declared with one and used with the other).
fn mentions_namespace(body: &str, namespace: &str) -> bool {
    if body.contains(namespace) {
        return true;
    }
    let swapped = if let Some(rest) = namespace.strip_prefix("https://") {
        format!("http://{rest}")
    } else if let Some(rest) = namespace.strip_prefix("http://") {
        format!("https://{rest}")
    } else {
        return false;
    };
    body.contains(&swapped)
}

#[cfg(test)]
mod tests {
    use super::mentions_namespace;

    #[test]
    fn namespace_check_accepts_either_scheme() {
        let body = "@prefix schema: <http://schema.org/> .";
        assert!(mentions_namespace(body, "http://schema.org/"));
        assert!(mentions_namespace(body, "https://schema.org/"));
        assert!(!mentions_namespace(body, "http://xmlns.com/foaf/0.1/"));
    }
}
