use std::{collections::HashMap, fmt::Write as _};

use bevy_ecs::prelude::*;
use rudof_rdf::rdf_core::FocusRDF;
use shacl_ast::ast::ShaclSchema;
use shacl_ir::compiled::schema_ir::SchemaIR as ShaclSchemaIR;
use shacl_rdf::ShaclParser;
use shacl_validation::{
    shacl_engine::native::NativeEngine, shape_validation::Validate as _,
    validation_report::result::ValidationResult,
};
use sophia_api::quad::Quad as _;
use tracing::{info_span, instrument};

use crate::{
    lsp_types::TextDocumentItem,
    prelude::*,
    util::{
        self,
        lang_text::{normalize_ws, LangTexts},
    },
};

mod data;
use data::*;
mod describe;
pub(crate) mod model;
mod template;
pub mod visible;
pub use describe::{hover_shape_property, shape_property_completions};
pub use template::complete_shape_templates;

fn shacl_schema_from_data<RDF: FocusRDF + std::fmt::Debug>(
    rdf_data: RDF,
) -> Option<ShaclSchema<RDF>> {
    ShaclParser::new(rdf_data).parse().ok()
}

/// Compile the SHACL shapes of every document whose triples changed. Bumps
/// [`Revisions::shapes`] when a document gains, changes or loses shapes.
pub fn derive_shapes(
    query: Query<
        (Entity, &Label, &Triples, Option<&ShaclShapes>),
        (Changed<Triples>, Without<Dirty>),
    >,
    mut commands: Commands,
    res: Res<ServerConfig>,
    mut revisions: ResMut<Revisions>,
) {
    if res.config.local.disabled.contains(&Disabled::Shapes) {
        return;
    }
    for (e, label, data, previous) in &query {
        let _entered = info_span!("derive_shape", label = label.as_str()).entered();
        commands.entity(e).remove::<ShaclShapes>();
        // Every document compiles to a (usually empty) set of shapes; only a
        // document that has or had actual shapes changes what others see.
        let had_shapes = previous.is_some_and(|s| s.ir().iter().next().is_some());
        if had_shapes {
            revisions.shapes += 1;
        }

        let rdf = Rdf::new(data);

        let Some(schema) = shacl_schema_from_data(rdf) else {
            continue;
        };

        let Some(shacl_ir) = ShaclSchemaIR::compile(&schema).ok() else {
            continue;
        };
        tracing::debug!("Found shacl ir with {} shapes", shacl_ir.iter().count());

        if !had_shapes && shacl_ir.iter().next().is_some() {
            revisions.shapes += 1;
        }
        commands.entity(e).insert(ShaclShapes::new(shacl_ir));
    }
}

/// System evaluates linked shapes
///
/// Validates open documents whose triples changed, and every open document when
/// shapes changed ([`Revisions::shapes`]): shapes usually arrive in the
/// background (LOV, `owl:imports`, configured shapes), after the document was
/// opened.
#[instrument(skip(query, other, revisions, seen, client, res))]
pub fn validate_shapes(
    query: Query<
        (
            &RopeC,
            &Label,
            &DocumentLinks,
            &Wrapped<TextDocumentItem>,
            &Prefixes,
            Ref<Triples>,
            &DynLang,
        ),
        With<Open>,
    >,
    other: Query<(&Label, &ShaclShapes, Option<&Global>, &Triples)>,
    revisions: Res<Revisions>,
    mut seen: Local<Seen>,
    mut client: ResMut<DiagnosticPublisher>,
    res: Res<ServerConfig>,
) {
    let shapes_changed = seen.update(revisions.shapes);
    if res.config.local.disabled.contains(&Disabled::Shapes) {
        return;
    }

    for (rope, label, links, item, prefixes, triples, lang) in &query {
        if !shapes_changed && !triples.is_changed() {
            continue;
        }
        let triples: &Triples = &triples;
        if !lang.supports_shape_validation() {
            continue;
        }
        tracing::debug!("Validate shapes {}", label.as_str());
        let client: &mut DiagnosticPublisher = &mut client;
        let mut diagnostics: Vec<crate::lsp_types::Diagnostic> = Vec::new();

        let rdf = Rdf::new(triples);
        let mut validation_results = Vec::new();
        let mut runner = NativeEngine::new();

        for (other_label, schema, global, shape_triples) in &other {
            if !visible::shapes_apply(other_label, global.is_some(), label, links) {
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
                            writeln!(&mut writer, "{}", m).unwrap();
                        } else {
                            write!(&mut writer, "Component {} ", res.component()).unwrap();
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
                        writeln!(&mut writer, "({})", id.value).unwrap();
                    } else {
                        writeln!(&mut writer,).unwrap();
                    }

                    if let Some(source) = res.source().cloned() {
                        let source = MyTerm::from(source);

                        // The shape's own explanation, in the preferred language:
                        // `sh:message`, or else `rdfs:comment`.
                        let texts = |p: &str| -> LangTexts {
                            shape_triples
                                .objects([&source], [&MyTerm::named_node(p, 0..0)])
                                .collect()
                        };
                        let mut message = texts("http://www.w3.org/ns/shacl#message");
                        if message.is_empty() {
                            message = texts("http://www.w3.org/2000/01/rdf-schema#comment");
                        }
                        if let Some(message) = message.first() {
                            write!(&mut writer, "\n{}\n", normalize_ws(message)).unwrap();
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
