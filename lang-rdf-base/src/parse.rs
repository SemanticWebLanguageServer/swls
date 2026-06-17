//! Shared, language-agnostic parse-phase systems for the text RDF syntaxes whose
//! [`Lang::Element`] is the [`Turtle`] model (Turtle / TriG / …).

use bevy_ecs::prelude::*;
use rdf_parsers::model::Turtle;
use swls_core::{lang::Lang, prelude::*};

use crate::traits::NamedNodeExt;

/// Derive the [`Prefixes`] component (declared prefix → namespace map + base URL)
/// from the parsed [`Turtle`] model.  Generic over the language marker `L`, so
/// Turtle, TriG and any other language with `Element = Turtle` share one
/// implementation.
pub fn derive_prefixes_system<L>(
    query: Query<(Entity, &Label, &Element<L>), Changed<Element<L>>>,
    mut commands: Commands,
) where
    L: Lang<Element = Turtle> + Send + Sync + 'static,
{
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
