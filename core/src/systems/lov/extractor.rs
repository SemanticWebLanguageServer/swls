use std::collections::HashMap;

use bevy_ecs::prelude::*;
use sophia_api::{
    prelude::{Any, Dataset},
    quad::Quad as _,
    term::{matcher::TermMatcher, Term as _},
};
use tracing::{debug, instrument};

use crate::{
    lsp_types::TextDocumentItem,
    prelude::*,
    util::{fs::Fs, ns::rdfs},
};


#[derive(Component, Debug)]
pub struct OntologyExtract;

#[instrument(skip(commands))]
pub fn init_ontology_extractor(mut commands: Commands, fs: Res<Fs>) {
    for local in lov::LOCAL_PREFIXES
        .iter()
        .filter(|x| ["rdf", "rdfs", "owl"].iter().any(|y| *y == x.name))
    {
        let url = fs.0.lov_url(&local.location, &local.name).unwrap();
        debug!("Virtual url {}", url.to_string());

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
                RopeC(ropey::Rope::from_str(&local.content)),
                Label(url),
                Wrapped(item),
                Types(HashMap::new()),
            ),
            Some("turtle".into()),
            OntologyExtract,
        );

        debug!("Init ontology {}", local.name);
        commands.queue(move |world: &mut World| {
            debug!("spawned ontology entity");
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
        debug!("Added triples from {}", label.as_str());
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
