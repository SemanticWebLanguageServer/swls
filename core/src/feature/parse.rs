use bevy_ecs::{prelude::*, schedule::ScheduleLabel};

pub use crate::systems::{
    derive_ontologies, derive_prefix_links, extract_type_hierarchy, fetch_lov_properties,
    infer_types,
};
use crate::{
    client::Client,
    store::Store,
    systems::{check_added_ontology_extract, derive_owl_imports_links, open_imports},
};

/// Parse schedule barrier, after this system, triples should be derived
pub fn triples() {}
/// Parse schedule barrier, after this system, prefixes should be derived
pub fn prefixes() {}

/// [`ScheduleLabel`] related to the Parse schedule
#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Label;

pub fn setup_schedule<C: Client + Resource>(world: &mut World) {
    let mut parse_schedule = bevy_ecs::schedule::Schedule::new(Label);
    parse_schedule.add_systems((
        prefixes,
        triples,
        derive_prefix_links.after(prefixes),
        derive_owl_imports_links.after(triples),
        fetch_lov_properties::<C>.after(prefixes),
        extract_type_hierarchy.after(triples),
        infer_types.after(triples),
        check_added_ontology_extract.after(triples),
        open_imports::<C>.after(triples),
        // store things
        crate::store::load_store.after(triples),
        derive_ontologies.after(crate::store::load_store),
    ));

    #[cfg(feature = "shapes")]
    parse_schedule.add_systems((crate::systems::derive_shapes.after(triples),));
    world.add_schedule(parse_schedule);
    world.insert_resource(Store(oxigraph::store::Store::new().unwrap()));
}
