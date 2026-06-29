use bevy_ecs::{prelude::*, schedule::ScheduleLabel};

pub use crate::systems::{
    derive_ontologies, derive_prefix_links, extract_type_hierarchy, fetch_lov_properties,
    infer_types,
};
use crate::{
    client::Client,
    store::Store,
    systems::{
        check_added_ontology_extract, derive_owl_imports_links, open_imports, prefix_diagnostics,
        validate_namespace_properties, validate_shapes,
    },
};

/// Parse schedule barrier, after this system, triples should be derived
pub fn triples() {}
/// Parse schedule barrier, after this system, prefixes should be derived
pub fn prefixes() {}
/// Parse schedule barrier marking the end of all derivation.
///
/// Every system that *produces* data (triples, prefixes, ontologies, store,
/// type hierarchy, …) runs before this barrier; every system that merely
/// *consumes* the fully-derived state to publish diagnostics (syntax errors,
/// prefix diagnostics, SHACL validation) runs `.after(end)`.
pub fn end() {}

/// [`ScheduleLabel`] related to the Parse schedule
#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Label;

pub fn setup_schedule<C: Client + Resource>(world: &mut World) {
    let mut parse_schedule = bevy_ecs::schedule::Schedule::new(Label);
    parse_schedule.add_systems((
        prefixes,
        triples,
        // ── producers: everything that derives data runs before `end` ──────────
        derive_prefix_links.after(prefixes).before(end),
        derive_owl_imports_links.after(triples).before(end),
        fetch_lov_properties::<C>.after(prefixes).before(end),
        extract_type_hierarchy.after(triples).before(end),
        infer_types.after(triples).before(end),
        check_added_ontology_extract.after(triples).before(end),
        open_imports::<C>.after(triples).before(end),
        // store things
        crate::store::load_store.after(triples).before(end),
        derive_ontologies
            .after(crate::store::load_store)
            .before(end),
        // ── end-of-derivation barrier ──────────────────────────────────────────
        end.after(triples).after(prefixes),
        // ── consumers: publish diagnostics from the fully-derived state ─────────
        validate_shapes.after(end),
        prefix_diagnostics.after(end),
        validate_namespace_properties.after(end),
    ));

    parse_schedule.add_systems((crate::systems::derive_shapes.after(triples).before(end),));
    world.add_schedule(parse_schedule);
    world.insert_resource(Store(oxigraph::store::Store::new().unwrap()));
}
