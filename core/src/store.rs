use bevy_ecs::prelude::*;
use tracing::debug;

use crate::prelude::*;

/// Shared Oxigraph store holding the triples of every background (not [`Open`])
/// document.
///
/// Each document is loaded into its own named graph, named after its [`Label`],
/// so a document can be replaced or removed without leaving stale triples
/// behind. Queries must therefore treat the default graph as the union of all
/// graphs (see [`union_query`]).
#[derive(Resource)]
pub struct Store(pub oxigraph::store::Store);

impl Store {
    fn graph_name(label: &Label) -> Option<oxigraph::model::GraphName> {
        oxigraph::model::NamedNode::new(label.as_str())
            .ok()
            .map(oxigraph::model::GraphName::NamedNode)
    }

    /// Replace the triples loaded for `label` with `triples`.
    pub fn load_document(&self, label: &Label, triples: &Triples) {
        self.remove_document(label);
        let Some(graph) = Self::graph_name(label) else {
            return;
        };
        let mut loader = self.0.bulk_loader();
        let _ = loader.load_quads(
            triples
                .0
                .iter()
                .flat_map(|x| x.into_oxi(Some(graph.clone()))),
        );
        let _ = loader.commit();
        debug!("Loaded {} triples from {}", triples.len(), label.as_str());
    }

    /// Drop every triple that was loaded for `label`.
    pub fn remove_document(&self, label: &Label) {
        if let Some(graph) = Self::graph_name(label) {
            let _ = self.0.clear_graph(graph.as_ref());
        }
    }
}

/// Parse `query` so that its default graph is the union of all named graphs in
/// the [`Store`], i.e. it sees the triples of every loaded document.
pub fn union_query(query: &str) -> Option<oxigraph::sparql::PreparedSparqlQuery> {
    let mut prepared = oxigraph::sparql::SparqlEvaluator::new()
        .parse_query(query)
        .ok()?;
    prepared.dataset_mut().set_default_graph_as_union();
    Some(prepared)
}

/// (Re)load the triples of every background document whose triples changed.
///
/// The document's previous triples are removed first, so a document that is
/// re-fetched, replaced or edited on disk never accumulates stale data.
pub fn load_store(
    query: Query<(&Triples, &Label), (Changed<Triples>, Without<Open>)>,
    store: Res<Store>,
    mut revisions: ResMut<Revisions>,
) {
    if !query.is_empty() {
        revisions.store += 1;
    }
    for (t, label) in &query {
        store.load_document(label, t);
    }
}
