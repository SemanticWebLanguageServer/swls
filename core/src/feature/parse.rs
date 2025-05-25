use bevy_ecs::{prelude::*, schedule::ScheduleLabel};

pub use crate::systems::{
    derive_classes, derive_prefix_links, derive_properties, derive_shapes, extract_type_hierarchy,
    fetch_lov_properties, infer_types,
};
use crate::{
    client::Client,
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
        derive_classes.after(triples),
        derive_properties.after(triples),
        fetch_lov_properties::<C>.after(prefixes),
        extract_type_hierarchy.after(triples),
        infer_types.after(triples),
        derive_shapes.after(triples),
        check_added_ontology_extract.after(triples),
        open_imports.after(triples),
    ));
    world.add_schedule(parse_schedule);
}
