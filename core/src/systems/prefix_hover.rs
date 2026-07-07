//! Hover and goto-definition for prefix declarations and JSON-LD `@context`
//! terms.
//!
//! A prefix line (`@prefix foaf: <…>`, `PREFIX foaf: <…>`) or a JSON-LD
//! `@context` entry is never part of a triple, so the shared
//! [`get_current_triple`] falls back to the *nearest* unrelated triple and the
//! triple-based hover/goto systems then render information about the wrong term
//! (or nothing). [`get_current_prefix`] detects this case from the parsed model
//! — whose prefix declarations, unlike the derived `Triples`, carry spans —
//! records a [`PrefixComponent`], and removes the stale [`TripleComponent`] so
//! those systems no-op. [`hover_prefix`] then shows the namespace mapping and
//! [`goto_prefix`] jumps to the ontology document.

use bevy_ecs::prelude::*;
use rdf_parsers::model::NamedNode;
use swls_lov::LocalPrefix;
use tracing::instrument;

use crate::{
    feature::goto_definition::GotoDefinitionRequest,
    lsp_types::{Location, Range, Url},
    prelude::*,
    util::offset_to_position,
};

/// Set when the cursor is on a prefix declaration (Turtle/SPARQL/TriG) or a
/// JSON-LD `@context` term. Carries the resolved namespace so hover and
/// goto-definition can describe / navigate to it instead of an unrelated triple.
#[derive(Component, Debug, Clone)]
pub struct PrefixComponent {
    /// Prefix name (`foaf`), or the empty string for the default `:` prefix.
    pub name: String,
    /// Resolved namespace / target IRI, as text.
    pub namespace: String,
    /// Range of the whole declaration — used as the hover range.
    pub range: Range,
}

impl PrefixComponent {
    /// A "namespace" prefix (usable as `name:local`) ends in a gen-delim; a
    /// JSON-LD term alias points at one specific IRI and does not. Goto uses this
    /// to leave alias resolution (e.g. Components.js) to language-specific
    /// systems while it handles the ontology-file jump for real namespaces.
    pub fn is_namespace(&self) -> bool {
        self.namespace.ends_with('/') || self.namespace.ends_with('#')
    }
}

/// Resolve a model [`NamedNode`] to its absolute IRI (a JSON-LD compact IRI
/// carries its context-computed expansion in `computed`).
fn named_node_iri(nn: &NamedNode) -> Option<String> {
    match nn {
        NamedNode::Full(iri, _) => Some(iri.clone()),
        NamedNode::Prefixed {
            computed: Some(c), ..
        } => Some(c.clone()),
        _ => None,
    }
}

/// Detect whether the cursor sits on a prefix declaration / JSON-LD `@context`
/// term and, if so, insert a [`PrefixComponent`] and drop the (necessarily
/// wrong) [`TripleComponent`]. Shared by the Hover and GotoDefinition schedules.
#[instrument(skip(query, commands))]
pub fn get_current_prefix(
    query: Query<
        (Entity, &Element, &Prefixes, &PositionComponent, &RopeC),
        Changed<PositionComponent>,
    >,
    mut commands: Commands,
) {
    for (e, element, prefixes, position, rope) in &query {
        commands.entity(e).remove::<PrefixComponent>();

        let Some(offset) = position_to_offset(position.0, &rope.0) else {
            continue;
        };

        // Prefix declarations carry a span in the parsed model; the derived
        // `Triples` do not, which is why we match against the model here.
        let Some(decl) = element
            .value()
            .prefixes
            .iter()
            .filter(|p| p.span().contains(&offset))
            .min_by_key(|p| p.span().end - p.span().start)
        else {
            continue;
        };

        let name = decl.value().prefix.value().clone();

        // Prefer the derived (expanded, validated) namespace; fall back to the
        // raw model value so a JSON-LD alias whose target does not parse as a URL
        // still hovers.
        let namespace = prefixes
            .iter()
            .find(|p| p.prefix == name)
            .map(|p| p.url.to_string())
            .or_else(|| named_node_iri(decl.value().value.value()))
            .unwrap_or_default();

        let (Some(start), Some(end)) = (
            offset_to_position(decl.span().start, &rope.0),
            offset_to_position(decl.span().end, &rope.0),
        ) else {
            continue;
        };

        commands
            .entity(e)
            .insert(PrefixComponent {
                name,
                namespace,
                range: Range::new(start, end),
            })
            // A prefix line is never part of a triple: drop the nearest-triple
            // fallback so the triple-based hover/goto systems skip this entity.
            .remove::<TripleComponent>();
    }
}

/// Hover system: describe the prefix → namespace mapping, enriched with the LOV
/// ontology title when the namespace is known.
pub fn hover_prefix(
    mut query: Query<(&PrefixComponent, &mut HoverRequest)>,
    lovs: Query<&LocalPrefix>,
) {
    for (prefix, mut req) in &mut query {
        let heading = if prefix.name.is_empty() {
            "Default prefix".to_string()
        } else {
            format!("Prefix `{}:`", prefix.name)
        };

        // LOV knows a human-readable title for many namespaces.
        let title = lovs
            .iter()
            .find(|l| l.namespace.as_ref() == prefix.namespace && !l.title.is_empty())
            .map(|l| l.title.as_ref());

        let mut md = format!("{}\n\n<{}>", heading, prefix.namespace);
        if let Some(title) = title {
            md = format!("{}\n\n{}", md, title);
        }

        req.0.push(md);
        if req.1.is_none() {
            req.1 = Some(prefix.range.clone());
        }
    }
}

/// Goto-definition system: jump a real namespace prefix to its ontology
/// document (the LOV-resolved file, or the namespace IRI itself). Term aliases
/// (`is_namespace() == false`) are left to language-specific resolvers.
#[instrument(skip(query, fs))]
pub fn goto_prefix(
    mut query: Query<(&PrefixComponent, &mut GotoDefinitionRequest)>,
    fs: Res<Fs>,
) {
    for (prefix, mut req) in &mut query {
        if !prefix.is_namespace() {
            continue;
        }
        // Same resolution `derive_prefix_links` uses for the document-link, so
        // goto lands on whatever file the server loads for this prefix.
        let target = fs
            .0
            .lov_url(&prefix.namespace, &prefix.name)
            .or_else(|| Url::parse(&prefix.namespace).ok());

        if let Some(uri) = target {
            req.0.push(Location {
                uri,
                range: Range::default(),
            });
        }
    }
}
