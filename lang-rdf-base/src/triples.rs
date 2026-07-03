use std::fmt::Debug;

use swls_core::prelude::MyQuad;

/// Trait for extracting normalized RDF triples from a parsed language element.
///
/// Implement this on the parsed model (the shared `Turtle` type) to enable a
/// generic derive-triples ECS system.
/// The implementation must be in a crate that owns either this trait or the element type.
pub trait IntoTriples {
    type Error: Debug;
    fn into_triples(&self) -> Result<Vec<MyQuad<'static>>, Self::Error>;
}
