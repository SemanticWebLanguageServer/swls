use crate::{lsp_types::TextDocumentItem, util};
use bevy_ecs::prelude::*;
use shacl_validation::{
    shacl_engine::native::NativeEngine, shape_validation::Validate as _,
    validation_report::result::ValidationResult,
};
use std::{collections::HashMap, fmt::Write as _};

use shacl_ir::compiled::schema_ir::SchemaIR as ShaclSchemaIR;
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

/// System evaluates linked shapes
#[instrument(skip(query, other, client, res))]
pub fn validate_shapes(
    query: Query<
        (
            &RopeC,
            &Label,
            &DocumentLinks,
            &Wrapped<TextDocumentItem>,
            &Prefixes,
            &Triples,
        ),
        (Changed<Triples>, With<Open>),
    >,
    other: Query<(&Label, &ShaclShapes, Option<&Global>, &Triples)>,
    mut client: ResMut<DiagnosticPublisher>,
    res: Res<ServerConfig>,
) {
    if res.config.local.disabled.contains(&Disabled::Shapes) {
        return;
    }

    for (rope, label, links, item, prefixes, triples) in &query {
        info!("Validate shapes {}", label.as_str());
        let client: &mut DiagnosticPublisher = &mut client;
        let mut diagnostics: Vec<crate::lsp_types::Diagnostic> = Vec::new();

        let rdf = Rdf::new(triples);
        let mut validation_results = Vec::new();
        let mut runner = NativeEngine::new();

        for (other_label, schema, global, shape_triples) in &other {
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

            let ir = schema.ir();
            for (_, shape) in ir.iter_with_targets() {
                let results = match shape.validate(&rdf, &mut runner, None, Some(shape), ir) {
                    Ok(r) => r,
                    Err(e) => {
                        tracing::debug!("errors {}", e);
                        continue;
                    }
                };

                if !results.is_empty() {
                    tracing::info!("{}: {:?}", shape.id().to_string(), results);
                }

                let mut map = HashMap::new();

                for res in &results {
                    let key = (res.source(), res.path());
                    let entry: &mut Vec<&ValidationResult> = map.entry(key).or_default();
                    entry.push(res);
                }

                for (_, all) in map {
                    let res = all[0];
                    let severity = match res.severity() {
                        shacl_ir::severity::CompiledSeverity::Info => {
                            Some(crate::lsp_types::DiagnosticSeverity::INFORMATION)
                        }
                        shacl_ir::severity::CompiledSeverity::Warning => {
                            Some(crate::lsp_types::DiagnosticSeverity::WARNING)
                        }
                        shacl_ir::severity::CompiledSeverity::Violation => {
                            Some(crate::lsp_types::DiagnosticSeverity::ERROR)
                        }
                        _ => None,
                    };

                    let term: MyTerm = MyTerm::from(res.focus_node().clone());
                    let related_subjects = triples.iter().map(|x| x.s()).filter(|&x| x == &term);
                    let related_objects = triples.iter().map(|x| x.o()).filter(|&x| x == &term);
                    let Some(span) = related_subjects
                        .chain(related_objects)
                        .min_by_key(|x| x.span.start)
                        .map(|x| x.span.clone())
                    else {
                        continue;
                    };
                    let Some(range) = util::range_to_range(&span, rope) else {
                        continue;
                    };

                    let mut writer = String::new();

                    for res in all {
                        if let Some(m) = res.message() {
                            write!(&mut writer, "{}\n", m).unwrap();
                        } else {
                            write!(&mut writer, "Component {} ", res.component().to_string())
                                .unwrap();
                        }
                    }

                    if let Some(path) = res.path() {
                        write!(
                            &mut writer,
                            "on path {} ",
                            PrefixedPath::new(path, prefixes)
                        )
                        .unwrap();
                    }

                    let id = MyTerm::from(shape.id().clone());
                    if id.ty == Some(sophia_api::term::TermKind::Iri) {
                        write!(&mut writer, "({})\n", id.value).unwrap();
                    } else {
                        write!(&mut writer, "\n",).unwrap();
                    }

                    if let Some(source) = res.source().cloned() {
                        let source = MyTerm::from(source);

                        if let Some(message) = shape_triples.object(
                            [&source],
                            [
                                &MyTerm::named_node(
                                    "http://www.w3.org/2000/01/rdf-schema#comment",
                                    0..0,
                                ),
                                &MyTerm::named_node("http://www.w3.org/ns/shacl#message", 0..0),
                            ],
                        ) {
                            write!(
                                &mut writer,
                                "\n{}\n",
                                message
                                    .value
                                    .split_whitespace()
                                    .collect::<Vec<_>>()
                                    .join(" ")
                            )
                            .unwrap();
                        }
                    }

                    diagnostics.push(crate::lsp_types::Diagnostic {
                        range,
                        severity,
                        source: Some(String::from("SWLS")),
                        message: writer,
                        ..Default::default()
                    });
                }

                validation_results.extend(results);
            }
        }

        let _ = client.publish(&item.0, diagnostics, "shacl_validation");
    }
}
