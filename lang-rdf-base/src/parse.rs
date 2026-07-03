//! Shared, language-agnostic parse-phase systems for the text RDF syntaxes whose
//! parsed model is the [`Turtle`](rdf_parsers::model::Turtle) model (Turtle / TriG / …).

use bevy_ecs::prelude::*;
use swls_core::prelude::*;

use crate::traits::NamedNodeExt;

/// Derive the [`Prefixes`] component (declared prefix → namespace map + base URL)
/// from the parsed [`Element`] model.  The language marker `L` is used only to
/// scope the query (`With<L>`), so Turtle, TriG and any other language whose
/// element is the shared `Turtle` model reuse one implementation.
pub fn derive_prefixes_system<L: Component>(
    query: Query<(Entity, &Label, &Element), (Changed<Element>, With<L>)>,
    mut commands: Commands,
) {
    for (entity, url, turtle) in &query {
        let prefixes: Vec<_> = turtle
            .prefixes
            .iter()
            .flat_map(|prefix| {
                let url = prefix.value.value().expand(turtle.value())?;
                let url = swls_core::lsp_types::Url::parse(&url).ok()?;
                Some(Prefix {
                    url,
                    prefix: prefix.prefix.value().clone(),
                })
            })
            .collect();

        let base = turtle
            .base
            .as_ref()
            .and_then(|b| {
                b.0 .1
                    .value()
                    .expand(turtle.value())
                    .and_then(|x| swls_core::lsp_types::Url::parse(&x).ok())
            })
            .unwrap_or(url.0.clone());

        commands.entity(entity).insert(Prefixes(prefixes, base));
    }
}
