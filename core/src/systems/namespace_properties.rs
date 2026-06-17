//! Validation: warn about IRIs used as properties (predicates) that live in a
//! user-configured "closed" namespace but are not defined in any known ontology.
//!
//! This lets a user assert "every predicate under `http://example.org/ns#` must be
//! a real, declared property" and get a warning when they typo or use an undefined
//! term.  Individual IRIs can be allow-listed via [`ServerConfig`]'s
//! `allowed_properties`, which is what the accompanying quick-fix / `executeCommand`
//! handler mutates.

use bevy_ecs::prelude::*;
use sophia_api::{quad::Quad, term::TermKind};

use crate::{
    feature::{code_action::CodeActionRequest, diagnostics::DiagnosticPublisher},
    lsp_types::{
        CodeAction, CodeActionKind, Command, Diagnostic, DiagnosticSeverity, Range,
        TextDocumentItem,
    },
    prelude::*,
    util::offset_to_position,
};

/// `executeCommand` identifier that adds an IRI to the user's `allowed_properties`.
pub const ALLOW_PROPERTY_COMMAND: &str = "swls.allowProperty";

/// Walk the document's triples and return `(iri, range)` for every predicate IRI
/// that starts with a configured closed namespace, is not a known ontology
/// property, and is not already allow-listed by the user.
fn unknown_namespace_properties(
    triples: &Triples,
    rope: &RopeC,
    ontologies: &Ontologies,
    closed_namespaces: &std::collections::HashSet<String>,
    allowed_properties: &std::collections::HashSet<String>,
) -> Vec<(String, Range)> {
    if closed_namespaces.is_empty() {
        return Vec::new();
    }

    let mut out = Vec::new();
    for quad in triples.0.iter() {
        for term in &[quad.s(), quad.p(), quad.o()] {
            if term.ty != Some(TermKind::Iri) {
                continue;
            }
            let iri = term.as_str();
            if !closed_namespaces.iter().any(|ns| iri.starts_with(ns)) {
                continue;
            }
            if allowed_properties.contains(iri)
                || ontologies.properties.contains_key(iri)
                || ontologies.classes.values().any(|c| c.term.value == iri)
            {
                continue;
            }

            let span = &term.span;
            if span.is_empty() {
                continue;
            }
            if let (Some(start), Some(end)) = (
                offset_to_position(span.start, &rope.0),
                offset_to_position(span.end, &rope.0),
            ) {
                out.push((iri.to_string(), Range::new(start, end)));
            }
        }
    }
    out
}

/// ECS system (ParseLabel): publish warnings for predicates in closed namespaces
/// that are not defined in any known ontology.
pub fn validate_namespace_properties(
    query: Query<(&Triples, &RopeC, &Wrapped<TextDocumentItem>), With<Open>>,
    ontologies: Res<Ontologies>,
    config: Res<ServerConfig>,
    mut client: ResMut<DiagnosticPublisher>,
) {
    let closed = &config.config.local.closed_namespaces;
    let allowed = &config.config.local.allowed_properties;

    for (triples, rope, item) in &query {
        let violations = unknown_namespace_properties(triples, rope, &ontologies, closed, allowed);

        let diagnostics: Vec<Diagnostic> = violations
            .into_iter()
            .map(|(iri, range)| Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::WARNING),
                message: format!(
                    "Property \"{}\" is not defined in the ontology for its namespace",
                    iri
                ),
                ..Default::default()
            })
            .collect();

        let _ = client.publish(&item.0, diagnostics, "namespace_properties");
    }
}

/// ECS system (CodeActionLabel): offer a quick-fix that allow-lists an unknown
/// property IRI.  The action carries a [`Command`] so the editor round-trips back
/// through `workspace/executeCommand`, letting the server persist the choice to
/// the global config and clear the warning at runtime.
///
/// Only the unknown property *under the cursor* is offered, so the quick-fix does
/// not show up when the cursor is elsewhere in the document.
pub fn unknown_property_code_action(
    mut query: Query<(&Triples, &RopeC, &PositionComponent, &mut CodeActionRequest), With<Open>>,
    ontologies: Res<Ontologies>,
    config: Res<ServerConfig>,
) {
    let closed = &config.config.local.closed_namespaces;
    let allowed = &config.config.local.allowed_properties;

    for (triples, rope, position, mut req) in &mut query {
        let cursor = position.0;
        let mut seen = std::collections::HashSet::new();
        for (iri, range) in
            unknown_namespace_properties(triples, rope, &ontologies, closed, allowed)
        {
            if !position_in_range(cursor, range) {
                continue;
            }
            if !seen.insert(iri.clone()) {
                continue;
            }
            req.0.push(CodeAction {
                title: format!("Mark \"{}\" as a known property", iri),
                kind: Some(CodeActionKind::QUICKFIX),
                command: Some(Command {
                    title: "Allow property".to_string(),
                    command: ALLOW_PROPERTY_COMMAND.to_string(),
                    arguments: Some(vec![serde_json::Value::String(iri)]),
                }),
                ..Default::default()
            });
        }
    }
}

/// Whether `pos` falls within `range` (inclusive of both ends).
fn position_in_range(pos: crate::lsp_types::Position, range: Range) -> bool {
    pos >= range.start && pos <= range.end
}
