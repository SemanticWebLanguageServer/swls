use bevy_ecs::prelude::*;

use crate::prelude::*;

#[derive(Resource)]
pub struct Store(pub oxigraph::store::Store);

pub fn load_store(
    query: Query<(&Triples, &Label), (Added<Triples>, Without<Open>)>,
    store: Res<Store>,
) {
    let mut loader = store.0.bulk_loader();
    for (t, label) in query {
        let _ = loader.load_quads(t.0.iter().map(|x| x.into_oxi(None)).flatten());
        tracing::info!("Loading {} triples from {}", t.len(), label.as_str());
    }
    let _ = loader.commit();
}
