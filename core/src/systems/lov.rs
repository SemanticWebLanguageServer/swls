use std::collections::{HashMap, HashSet};

use bevy_ecs::{prelude::*, world::CommandQueue};
use lsp_types::{TextDocumentItem, Url};
use serde::Deserialize;
use sophia_api::{
    prelude::{Any, Dataset},
    quad::Quad,
    term::{matcher::TermMatcher, Term as _},
};
use tracing::{debug, error, info, instrument, span};

use crate::{
    prelude::*,
    util::{
        fs::Fs,
        ns::{owl, rdfs},
    },
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

// Do we check whether or not the namespace url and the prefix url are the same?
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

/// First of al, fetch the lov dataset information at url https://lov.linkeddata.es/dataset/lov/api/v2/vocabulary/info?vocab=${prefix}
/// Next, extract that json object into an object and find the latest dataset
pub fn fetch_lov_properties<C: Client + Resource>(
    sender: Res<CommandSender>,
    query: Query<
        &Prefixes,
        (
            Or<((Changed<Prefixes>, With<Open>), Changed<Open>)>,
            // Without<Dirty>,
        ),
    >,
    mut prefixes: Local<HashSet<String>>,
    client: Res<C>,
    fs: Res<Fs>,
) {
    for prefs in &query {
        for prefix in prefs.0.iter() {
            if !prefixes.contains(prefix.url.as_str()) {
                prefixes.insert(prefix.url.to_string());
                if let Some(url) = fs.0.lov_url(prefix.url.as_str(), &prefix.prefix) {
                    info!("Other virtual url {}", url);
                    if let Some(local) = lov::LOCAL_PREFIXES
                        .iter()
                        .find(|x| x.location == prefix.url.as_str())
                    {
                        debug!("Local lov");
                        local_lov::<C>(local, url, &sender, fs.clone());
                    } else {
                        debug!("Remove lov");
                        let sender = sender.0.clone();
                        let c = client.as_ref().clone();
                        client.spawn(fetch_lov(prefix.clone(), url, c, sender, fs.clone()));
                    }
                } else {
                    debug!("Failed to find url");
                }
            } else {
                debug!("Prefixes is already present {}", prefix.url);
            }
        }
    }
}

type Sender = futures::channel::mpsc::UnboundedSender<CommandQueue>;
fn spawn_document(
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
            Label(url.clone()), // this might crash
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
        drop(_enter);
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

async fn fetch_lov<C: Client + Resource>(prefix: Prefix, label: Url, c: C, sender: Sender, fs: Fs) {
    if let Some(url) = extract_file_url(&prefix.prefix, &c).await {
        match c.fetch(&url, &std::collections::HashMap::new()).await {
            Ok(resp) if resp.status == 200 => {
                let extra =
                    extra_from_lov::<C>(FromPrefix(prefix), resp.body.clone(), label.clone(), fs);
                spawn_document(label, resp.body, &sender, extra);
            }
            Ok(resp) => {
                error!("Fetch ({}) failed status {}", url, resp.status);
            }
            Err(e) => {
                error!("Fetch ({}) failed {:?}", url, e);
            }
        }
    }
}

fn local_lov<C: Client + Resource>(
    local: &lov::LocalPrefix,
    label: Url,
    sender: &Res<CommandSender>,
    fs: Fs,
) {
    info!("Using local {}", local.name);

    let from = FromPrefix(Prefix {
        prefix: local.name.to_string(),
        url: Url::parse(&local.location).unwrap(),
    });

    let extra = extra_from_lov::<C>(from, local.content.to_string(), label.clone(), fs);
    spawn_document(label, local.content.to_string(), &sender.0, extra);
}

#[derive(Component)]
pub struct OntologyExtract;

#[instrument(skip(commands))]
pub fn init_onology_extractor(mut commands: Commands, fs: Res<Fs>) {
    for local in lov::LOCAL_PREFIXES
        .iter()
        .filter(|x| ["rdf", "rdfs", "owl"].iter().any(|y| *y == x.name))
    {
        let url = fs.0.lov_url(&local.location, &local.name).unwrap();
        info!("Virtual url {}", url.to_string());

        // let url = lsp_types::Url::from_str(local.location).unwrap();
        let item = TextDocumentItem {
            version: 1,
            uri: url.clone(),
            language_id: String::from("turtle"),
            text: String::new(),
        };

        let spawn = spawn_or_insert(
            url.clone(),
            (
                Source(local.content.to_string()),
                RopeC(ropey::Rope::from_str(local.content)),
                Label(url),
                Wrapped(item),
                Types(HashMap::new()),
            ),
            Some("turtle".into()),
            OntologyExtract,
        );

        info!("Init onology {}", local.name);
        commands.push(move |world: &mut World| {
            info!("Spawned");
            spawn(world);
        });
    }
}

#[instrument(skip(query, extractor))]
pub fn check_added_ontology_extract(
    query: Query<(&Triples, &Label), (Added<Triples>, With<OntologyExtract>)>,
    mut extractor: ResMut<OntologyExtractor>,
) {
    let mut changed = false;
    for (triples, label) in &query {
        info!("Added triples from {}", label.as_str());
        extractor.quads.extend(triples.0.iter().cloned());
        changed = true;
    }
    if changed {
        extractor.extract();
    }
}

#[derive(Debug, Resource)]
pub struct OntologyExtractor {
    quads: Vec<MyQuad<'static>>,
    properties: Vec<MyTerm<'static>>,
    classes: Vec<MyTerm<'static>>,
}

struct LocalMatcher<'a> {
    properties: &'a [MyTerm<'static>],
}

impl TermMatcher for LocalMatcher<'_> {
    type Term = MyTerm<'static>;

    fn matches<T2: sophia_api::prelude::Term + ?Sized>(&self, term: &T2) -> bool {
        for p in self.properties {
            if term.eq(p) {
                return false;
            }
        }

        true
    }
}

impl OntologyExtractor {
    pub fn new() -> Self {
        Self {
            quads: vec![],
            classes: vec![MyTerm::<'static>::named_node(
                "http://www.w3.org/2000/01/rdf-schema#Class",
                0..1,
            )],
            properties: vec![MyTerm::<'static>::named_node(
                "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property",
                0..1,
            )],
        }
    }

    pub fn properties<'a>(&'a self) -> &'a [MyTerm<'static>] {
        &self.properties[..]
    }

    pub fn classes<'a>(&'a self) -> &'a [MyTerm<'static>] {
        &self.classes[..]
    }

    fn extract_step(quads: &Vec<MyQuad<'static>>, items: &mut Vec<MyTerm<'static>>) -> bool {
        let new_items: Vec<_> = quads
            .quads_matching(
                LocalMatcher { properties: &items },
                [rdfs::subClassOf],
                &items[..],
                Any,
            )
            .flatten()
            .map(|x| x.to_s().to_owned())
            .collect();

        let added = !new_items.is_empty();
        items.extend(new_items);
        added
    }

    fn extract(&mut self) {
        loop {
            if !OntologyExtractor::extract_step(&self.quads, &mut self.properties) {
                break;
            }
        }

        loop {
            if !OntologyExtractor::extract_step(&self.quads, &mut self.classes) {
                break;
            }
        }
    }
}

// #[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
// pub enum EntryState {
//     Ready,
//     Transit,
// }
//
// #[derive(Debug, Clone, Serialize, Deserialize)]
// pub struct LovEntry {
//     prefix: String,
//     url: String,
//     num: usize,
//     state: EntryState,
// }
//
// impl LovEntry {
//     fn name(&self) -> String {
//         format!("{}-{}.ttl", self.num, self.prefix)
//     }
//     pub fn url(&self, cache: &Cache) -> Option<Url> {
//         self.file_url(cache).or_else(|| self.remote_url())
//     }
//
//     #[cfg(not(target_arch = "wasm32"))]
//     fn file_url(&self, cache: &Cache) -> Option<Url> {
//         let p = cache.path()?;
//         let url = p.join(self.name());
//         Url::from_file_path(url).ok()
//     }
//
//     #[cfg(target_arch = "wasm32")]
//     fn file_url(&self, cache: &Cache) -> Option<Url> {
//         None
//     }
//
//     fn remote_url(&self) -> Option<Url> {
//         Url::from_str(&self.url).ok()
//     }
//
//     pub fn save(&mut self, cache: &Cache, content: &str) -> Option<()> {
//         self.state = EntryState::Ready;
//         cache.write_file(&self.name(), content)
//     }
// }
//
// #[derive(Debug, Clone, Serialize, Deserialize, Resource)]
// pub struct LovHelper {
//     entries: Vec<LovEntry>,
// }
//
// impl LovHelper {
//     fn try_from_cache(cache: &Cache) -> Option<Self> {
//         let c = cache.get_file("index.json")?;
//         info!("Found index file! {}", c);
//         serde_json::from_str(&c).ok()
//     }
//
//     pub fn from_cache(cache: &Cache) -> Self {
//         Self::try_from_cache(cache).unwrap_or_else(|| Self {
//             entries: Vec::new(),
//         })
//     }
//
//     pub fn save(mut self, cache: &Cache) -> Option<()> {
//         self.entries = self
//             .entries
//             .into_iter()
//             .filter(|x| x.state == EntryState::Ready)
//             .collect();
//         let st = serde_json::to_string(&self).ok()?;
//         info!("Save index file! {}", st);
//         cache.write_file("index.json", &st)
//     }
//
//     pub fn has_entry_mut(&mut self, prefix: &Prefix) -> Option<&mut LovEntry> {
//         self.entries
//             .iter_mut()
//             .find(|e| e.prefix == prefix.prefix && e.url == prefix.url.as_str())
//     }
//
//     pub fn has_entry(&self, prefix: &Prefix) -> Option<&LovEntry> {
//         self.entries
//             .iter()
//             .find(|e| e.prefix == prefix.prefix && e.url == prefix.url.as_str())
//     }
//
//     pub fn create_entry(&mut self, prefix: &Prefix) -> &LovEntry {
//         debug!("Create entry for {:?}", prefix);
//         if let Some(e) = self.entries.iter().enumerate().find_map(|(i, e)| {
//             (e.prefix == prefix.prefix && e.url == prefix.url.as_str()).then_some(i)
//         }) {
//             return &self.entries[e];
//         }
//         let c = self
//             .entries
//             .iter()
//             .filter(|x| x.prefix == prefix.prefix)
//             .count();
//         let entry = LovEntry {
//             prefix: prefix.prefix.to_string(),
//             url: prefix.url.to_string(),
//             num: c,
//             state: EntryState::Transit,
//         };
//         self.entries.push(entry);
//         self.entries.last().unwrap()
//     }
//
//     pub fn save_prefix(&mut self, cache: &Cache, prefix: &Prefix, content: &str) -> Option<()> {
//         let e = self
//             .entries
//             .iter_mut()
//             .find(|e| (e.prefix == prefix.prefix && e.url == prefix.url.as_str()))?;
//         e.save(cache, content)
//     }
// }

#[derive(Debug, Clone, Component)]
pub struct FromPrefix(pub Prefix);
