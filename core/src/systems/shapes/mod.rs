use std::collections::HashMap;

use crate::lsp_types::{DiagnosticSeverity, TextDocumentItem};
use bevy_ecs::prelude::*;
use ropey::Rope;
use rudof_lib::{
    shacl_validation::{
        shacl_engine::native::NativeEngine, shape_validation::Validate as _,
        validation_report::result::ValidationResult,
    },
    ShaclSchemaIR,
};

use shacl_rdf::ShaclParser;
use sophia_api::quad::Quad as _;
use srdf::FocusRDF;
use tracing::{info, info_span, instrument};

use crate::prelude::*;
use shacl_ast::ast::Schema as ShaclSchema;

mod data;
use data::*;

fn shacl_schema_from_data<RDF: FocusRDF + std::fmt::Debug>(
    rdf_data: RDF,
) -> Option<ShaclSchema<RDF>> {
    ShaclParser::new(rdf_data).parse().ok()
}

pub fn derive_shapes(
    query: Query<(Entity, &Label, &Triples), (Changed<Triples>, Without<Dirty>)>,
    mut commands: Commands,
    res: Res<ServerConfig>,
) {
    if res.config.local.disabled.contains(&Disabled::Shapes) {
        return;
    }
    for (e, label, data) in &query {
        let _entered = info_span!("derive_shape", label = label.as_str()).entered();
        commands.entity(e).remove::<ShaclShapes>();

        let rdf = Rdf::new(data);

        let Some(schema) = shacl_schema_from_data(rdf) else {
            continue;
        };

        let Some(shacl_ir) = ShaclSchemaIR::compile(&schema).ok() else {
            continue;
        };
        info!("Found shacl ir with {} shapes", shacl_ir.iter().count());

        commands.entity(e).insert(ShaclShapes::new(shacl_ir));
    }
}

fn group_per_fn_per_path(
    res: &Vec<ValidationResult>,
    triples: &Triples,
    prefixes: &Prefixes,
) -> HashMap<std::ops::Range<usize>, HashMap<String, Vec<String>>> {
    let mut per_fn_per_path = HashMap::new();
    for r in res {
        let foc = r.focus_node().to_string();

        let mut done = std::collections::HashSet::new();
        for t in triples.0.iter() {
            if t.s().as_str() == &foc && !done.contains(t.s()) {
                done.insert(t.s().to_owned());

                let entry: &mut HashMap<String, Vec<String>> =
                    per_fn_per_path.entry(t.s().span.clone()).or_default();

                let path = r.path().map(|x| x.to_string()).unwrap_or(String::new());
                let entry = entry.entry(path).or_default();

                let component = r.component().to_string();
                let component = prefixes.shorten(&component).unwrap_or(component);

                entry.push(component);
            }
        }
    }

    per_fn_per_path
}

fn push_diagnostics(
    rope: &Rope,
    res: &Vec<ValidationResult>,
    triples: &Triples,
    prefixes: &Prefixes,
    diagnostics: &mut Vec<crate::lsp_types::Diagnostic>,
) {
    for (range, per_path) in group_per_fn_per_path(&res, triples, prefixes) {
        if let Some(range) = range_to_range(&range, &rope) {
            for (path, components) in per_path {
                let mut comps = components[0].clone();
                for c in components.into_iter().skip(1) {
                    comps += ", ";
                    comps += &c;
                }

                diagnostics.push(crate::lsp_types::Diagnostic {
                    range: range.clone(),
                    severity: Some(DiagnosticSeverity::ERROR),
                    source: Some(String::from("SWLS")),
                    message: format!("Path {} violates {}", path, comps),
                    related_information: None,
                    ..Default::default()
                });
            }
        }
    }
}

/// System evaluates linked shapes
#[instrument(skip(query, other, client, res))]
pub fn validate_shapes(
    query: Query<
        (
            &RopeC,
            &Label,
            &DocumentLinks,
            &Wrapped<TextDocumentItem>,
            &Triples,
        ),
        (Changed<Triples>, Without<Dirty>, With<Open>),
    >,
    other: Query<(&Label, &ShaclShapes, &Prefixes, Option<&Global>)>,
    mut client: ResMut<DiagnosticPublisher>,
    res: Res<ServerConfig>,
) {
    if res.config.local.disabled.contains(&Disabled::Shapes) {
        return;
    }

    for (rope, label, links, item, triples) in &query {
        info!("Validate shapes {}", label.as_str());
        let other: &Query<(&Label, &ShaclShapes, &Prefixes, Option<&Global>)> = &other;
        let client: &mut DiagnosticPublisher = &mut client;
        let mut diagnostics: Vec<crate::lsp_types::Diagnostic> = Vec::new();

        for (other_label, schema, prefixes, global) in other {
            if global.is_none()
                && links
                    .iter()
                    .find(|link| link.0.as_str().starts_with(other_label.0.as_str()))
                    .is_none()
                && label.0 != other_label.0
            {
                continue;
            }

            let _entered = info_span!(
                "validate_shapes",
                data = label.as_str(),
                shapes = other_label.as_str()
            )
            .entered();

            let mut validation_results = Vec::new();
            let mut runner = NativeEngine::new();

            let rdf = Rdf::new(triples);
            let ir = schema.ir();
            for (_, shape) in ir.iter_with_targets() {
                let results = match shape.validate(&rdf, &mut runner, None, Some(shape), ir) {
                    Ok(r) => r,
                    Err(e) => {
                        tracing::debug!("errors {}", e);
                        continue;
                    }
                };
                validation_results.extend(results);
            }

            push_diagnostics(
                rope,
                &validation_results,
                triples,
                prefixes,
                &mut diagnostics,
            );
        }

        let _ = client.publish(&item.0, diagnostics, "shacl_validation");
    }
}
