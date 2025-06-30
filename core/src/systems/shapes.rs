use std::{cell::OnceCell, collections::HashMap};

use bevy_ecs::prelude::*;
use lsp_types::{DiagnosticSeverity, TextDocumentItem};
use ropey::Rope;
use rudof_lib::{
    shacl_ast::{compiled::shape::CompiledShape, ShaclParser},
    shacl_validation::{
        shacl_processor::{GraphValidation, ShaclProcessor},
        shape::Validate,
        store::graph::Graph,
        validation_report::result::ValidationResult,
    },
    srdf::{Object, SRDFGraph},
    RdfData,
};
use sophia_api::quad::Quad as _;
use tracing::{debug, error, info, instrument};

use crate::prelude::*;

fn get_reader<'a>(rope: &'a Rope) -> impl std::io::Read + 'a {
    use std::io::prelude::*;
    let reader: Box<dyn Read> = rope
        .chunks()
        .map(|x| std::io::Cursor::new(x.as_bytes()))
        .fold(Box::new(std::io::Cursor::new(&[])), |acc, chunk| {
            Box::new(acc.chain(chunk))
        });
    reader
}

type ShaclShape = CompiledShape<RdfData>;
type ShaclShapes = Wrapped<Vec<CompiledShape<RdfData>>>;
pub fn derive_shapes(
    query: Query<(Entity, &RopeC, &Label), (Changed<Triples>, Without<Dirty>)>,
    mut commands: Commands,
    res: Res<ServerConfig>,
) {
    if res.config.local.disabled.contains(&Disabled::Shapes) {
        return;
    }
    for (e, rope, label) in &query {
        commands.entity(e).remove::<ShaclShapes>();

        match SRDFGraph::from_reader(
            get_reader(&rope),
            &rudof_lib::RDFFormat::Turtle,
            Some(label.0.as_str()),
            &rudof_lib::ReaderMode::Lax,
        )
        .map_err(|x| x.to_string())
        .and_then(|data| RdfData::from_graph(data).map_err(|e| e.to_string()))
        .and_then(|data| {
            let mut parser = ShaclParser::new(data.clone());
            let result = parser.parse().map_err(|e| e.to_string());

            if !parser.errors().is_empty() {
                info!("Parsing shapes had some errors");
                for e in parser.errors() {
                    info!("Error: {}", e);
                }
            }

            result
        }) {
            Ok(schema) => {
                let compiled: Vec<_> = schema
                    .iter()
                    .flat_map(
                        |(node, shape)| match ShaclShape::compile(shape.clone(), &schema) {
                            Ok(x) => Some(x),
                            Err(err) => {
                                info!("Failed to parse shacl shape {}: {}", node, err);
                                None
                            }
                        },
                    )
                    .collect();
                let is_some = !compiled.is_empty();

                debug!(
                    "Compiled shapes for {} (is some {})",
                    label.as_str(),
                    is_some
                );
                if !compiled.is_empty() {
                    commands.entity(e).insert(Wrapped(compiled));
                }
            }
            Err(reason) => {
                error!(
                    "Failed to compile shapes for {} ({})",
                    label.as_str(),
                    reason
                );
            }
        }
    }
}

fn get_path(
    source: Option<&Object>,
    s: &CompiledShape<RdfData>,
    prefixes: &Prefixes,
) -> Option<String> {
    let source = source?;
    let property = s
        .property_shapes()
        .iter()
        .find(|x| match (x.id(), source) {
            (rudof_lib::oxrdf::Term::NamedNode(named_node), Object::Iri(iri_s)) => {
                named_node.as_str() == iri_s.as_str()
            }
            (rudof_lib::oxrdf::Term::BlankNode(blank_node), Object::BlankNode(st)) => {
                blank_node.as_str() == st.as_str()
            }
            _ => false,
        })?;
    let path = property.path_str()?;
    Some(prefixes.shorten(&path).unwrap_or(path))
}

fn group_per_fn_per_path(
    res: &Vec<ValidationResult>,
    s: &CompiledShape<RdfData>,
    triples: &Triples,
    prefixes: &Prefixes,
) -> HashMap<std::ops::Range<usize>, HashMap<String, Vec<String>>> {
    let mut per_fn_per_path = HashMap::new();
    for r in res {
        let foc = r.focus_node().to_string();

        let mut done = std::collections::HashSet::new();
        for t in &triples.0 {
            if t.s().as_str() == &foc && !done.contains(t.s()) {
                done.insert(t.s().to_owned());

                let entry: &mut HashMap<String, Vec<String>> =
                    per_fn_per_path.entry(t.s().span.clone()).or_default();

                let path = get_path(r.source(), s, prefixes).unwrap_or(String::new());
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
    s: &CompiledShape<RdfData>,
    triples: &Triples,
    prefixes: &Prefixes,
    diagnostics: &mut Vec<lsp_types::Diagnostic>,
) {
    for (range, per_path) in group_per_fn_per_path(&res, s, triples, prefixes) {
        if let Some(range) = range_to_range(&range, &rope) {
            for (path, components) in per_path {
                let mut comps = components[0].clone();
                for c in components.into_iter().skip(1) {
                    comps += ", ";
                    comps += &c;
                }

                diagnostics.push(lsp_types::Diagnostic {
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

fn derive_shapes_diagnostics_for(
    rope: &RopeC,
    label: &Label,
    links: &DocumentLinks,
    item: &Wrapped<TextDocumentItem>,
    triples: &Triples,
    other: &Query<(&Label, &ShaclShapes, &Prefixes)>,
    client: &mut DiagnosticPublisher,
) {
    let mut diagnostics: Vec<lsp_types::Diagnostic> = Vec::new();

    let build_validator = || {
        SRDFGraph::from_reader(
            get_reader(&rope),
            &rudof_lib::RDFFormat::Turtle,
            Some(label.0.as_str()),
            &rudof_lib::ReaderMode::Lax,
        )
        .ok()
        .and_then(|data| RdfData::from_graph(data).ok())
        .map(|data| {
            debug!("Created graph validator for {}", label.as_str());
            GraphValidation::from_graph(
                Graph::from_data(data),
                rudof_lib::ShaclValidationMode::Native,
            )
        })
    };

    // Delayed building, maybe no shapes are linked to this document, and we don't need to build a
    // validator
    let validator = OnceCell::<Option<GraphValidation>>::new();
    for (other_label, schema, prefixes) in other {
        if links
            .iter()
            .find(|link| link.0.as_str().starts_with(other_label.0.as_str()))
            .is_none()
            && label.0 != other_label.0
        {
            continue;
        }

        if let Some(validator) = validator.get_or_init(build_validator) {
            debug!("Schema {}", other_label.as_str());
            for s in schema.iter() {
                if let Ok(res) = s.validate(validator.store(), validator.runner(), None, None) {
                    if !res.is_empty() {
                        push_diagnostics(rope, &res, s, triples, prefixes, &mut diagnostics);
                    }
                }
            }
        } else {
            break;
        }
    }

    let _ = client.publish(&item.0, diagnostics, "shacl_validation");
}

/// System evaluates linked shapes
// #[instrument(skip(query, other, client))]
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
    other: Query<(&Label, &ShaclShapes, &Prefixes)>,
    mut client: ResMut<DiagnosticPublisher>,
    res: Res<ServerConfig>,
) {
    if res.config.local.disabled.contains(&Disabled::Shapes) {
        return;
    }
    for (rope, label, links, item, triples) in &query {
        info!("Validate shapes {}", label.as_str());
        derive_shapes_diagnostics_for(rope, label, links, item, triples, &other, &mut client);
    }
}

/// System checks what entities should retrigger a shape evaluation when a shape changes
#[instrument(skip(changed_schemas, query, other, client))]
pub fn validate_with_updated_shapes(
    changed_schemas: Query<&Label, (Changed<Wrapped<ShaclShape>>, With<Open>)>,
    query: Query<
        (
            &RopeC,
            &Label,
            &DocumentLinks,
            &Wrapped<TextDocumentItem>,
            &Triples,
        ),
        With<Open>,
    >,
    other: Query<(&Label, &ShaclShapes, &Prefixes)>,
    mut client: ResMut<DiagnosticPublisher>,
    res: Res<ServerConfig>,
) {
    if res.config.local.disabled.contains(&Disabled::Shapes) {
        return;
    }
    for l in &changed_schemas {
        info!("Changed schema {}", l.as_str());
        for (rope, label, links, item, triples) in &query {
            if links
                .iter()
                .find(|(url, _)| url.as_str().starts_with(l.as_str()))
                .is_some()
            {
                debug!("Found reverse linked document! {}", label.as_str());
                derive_shapes_diagnostics_for(
                    rope,
                    label,
                    links,
                    item,
                    triples,
                    &other,
                    &mut client,
                );
            }
        }
    }
}
