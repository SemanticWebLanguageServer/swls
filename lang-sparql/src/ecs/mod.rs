use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use bevy_ecs::{prelude::*, world::World};
use completion::{CompletionRequest, SimpleCompletion};
use rdf_parsers::{IncrementalBias, PrevParseInfo};
use rowan::NodeOrToken;
use sophia_api::{
    quad::Quad as _,
    term::{Term as _, TermKind},
};
use swls_core::{
    components::*,
    lsp_types::CompletionItemKind,
    prelude::*,
};
use swls_lang_rdf_base::traits::{NamedNodeExt, TurtleExt};

use crate::Sparql;

pub fn setup_parse(world: &mut World) {
    use swls_core::feature::parse::*;
    world.schedule_scope(Label, |_, schedule| {
        schedule.add_systems((
            parse_sparql_system,
            derive_triples
                .after(parse_sparql_system)
                .before(prefixes)
                .before(triples),
        ));
    });
}

pub fn setup_format(world: &mut World) {
    world.schedule_scope(FormatLabel, |_, schedule| {
        schedule.add_systems(format_sparql_system);
    });
}

fn format_sparql_system(
    mut query: Query<
        (&RopeC, &Wrapped<rowan::GreenNode>, &mut FormatRequest),
        (With<Sparql>, Without<Dirty>),
    >,
    config: Res<ServerConfig>,
) {
    use swls_core::lsp_types::{Position, Range, TextEdit};
    if !config.config.local.format.sparql() {
        tracing::debug!("SPARQL formatting disabled by config");
        return;
    }
    for (source, node, mut request) in &mut query {
        if request.0.is_some() {
            tracing::debug!("Didn't format with the sparql format system, already formatted");
            continue;
        }
        tracing::debug!("Formatting with sparql format system");

        let root = rowan::SyntaxNode::new_root(node.0.clone());
        let formatted = rdf_parsers::sparql::format::format(&root, 80);

        request.0 = Some(vec![TextEdit::new(
            Range::new(
                Position::new(0, 0),
                Position::new(source.0.len_lines() as u32 + 1, 0),
            ),
            formatted,
        )]);
    }
}

pub fn setup_completion(world: &mut World) {
    use swls_core::feature::completion::*;
    world.schedule_scope(Label, |_, schedule| {
        schedule.add_systems((variable_completion.after(generate_completions),));
    });
}

fn extract_sparql_cst_tokens(
    node: &rowan::SyntaxNode<rdf_parsers::sparql::parser::Lang>,
) -> Vec<swls_core::prelude::Spanned<rowan::SyntaxKind>> {
    use rdf_parsers::sparql::parser::SyntaxKind;
    let mut tokens = Vec::new();
    for node_or_token in node.descendants_with_tokens() {
        if let NodeOrToken::Token(t) = node_or_token {
            let kind = t.kind();
            if kind == SyntaxKind::WhiteSpace {
                continue;
            }
            let range = t.text_range();
            let span = usize::from(range.start())..usize::from(range.end());
            tokens.push(swls_core::prelude::spanned(
                rowan::SyntaxKind(kind as u16),
                span,
            ));
        }
    }
    tokens
}

#[instrument(skip(query, commands, prev_infos, config))]
fn parse_sparql_system(
    query: Query<(Entity, &Source, &Label), (Changed<Source>, With<Sparql>)>,
    mut commands: Commands,
    mut prev_infos: Local<HashMap<String, PrevParseInfo>>,
    config: Res<ServerConfig>,
) {
    if !config.config.sparql.unwrap_or(true) {
        return;
    }
    for (entity, source, label) in &query {
        use rdf_parsers::sparql::{
            convert::convert,
            parser::{Lang, Rule, SyntaxKind},
        };

        // `.ru` files hold SPARQL updates (INSERT DATA, DELETE ... WHERE, ...),
        // which are not queries.
        let unit = if label.0.path().ends_with(".ru") {
            SyntaxKind::UpdateUnit
        } else {
            SyntaxKind::QueryUnit
        };
        let prev = prev_infos.get(label.as_str());
        let (parse, new_prev) = rdf_parsers::parse_incremental(
            Rule::new(unit),
            source.0.as_str(),
            prev,
            IncrementalBias::default(),
        );
        prev_infos.insert(label.to_string(), new_prev);

        let syntax = parse.syntax::<Lang>();
        let errors = collect_errors(&syntax);
        let mut turtle_model = convert(&syntax);
        turtle_model.set_base = Some(label.to_string());

        let cst_tokens = extract_sparql_cst_tokens(&syntax);

        let span = 0..source.0.len();
        let element = Element(swls_core::prelude::spanned(turtle_model, span));

        if errors.is_empty() {
            commands
                .entity(entity)
                .insert((element, Errors(errors), CstTokens(cst_tokens)))
                .insert(Wrapped(syntax.green().into_owned()))
                .remove::<Dirty>();
        } else {
            commands
                .entity(entity)
                .insert((element, Errors(errors), CstTokens(cst_tokens), Dirty));
        }
    }
}

fn collect_errors(
    node: &rowan::SyntaxNode<rdf_parsers::sparql::parser::Lang>,
) -> Vec<swls_lang_turtle::lang::parser::TurtleParseError> {
    use rdf_parsers::sparql::parser::SyntaxKind;
    use swls_lang_turtle::lang::parser::TurtleParseError;

    let mut errors = Vec::new();
    let mut stack = vec![node.clone()];
    while let Some(current) = stack.pop() {
        for child in current.children_with_tokens() {
            match child {
                NodeOrToken::Node(n) => {
                    if n.kind() == SyntaxKind::Error {
                        let range = rdf_parsers::effective_error_span::<
                            rdf_parsers::sparql::parser::Lang,
                        >(&n);
                        let msg = n
                            .parent()
                            .map(|p| format!("Expected: {:?}", p.kind()))
                            .unwrap_or_else(|| format!("Unexpected: {}", n.text()));
                        errors.push(TurtleParseError { range, msg });
                    } else {
                        stack.push(n);
                    }
                }
                NodeOrToken::Token(t) => {
                    if t.kind() == SyntaxKind::Error {
                        let r = t.text_range();
                        errors.push(TurtleParseError {
                            range: r.start().into()..r.end().into(),
                            msg: format!("Unexpected: {}", t.text()),
                        });
                    }
                }
            }
        }
    }
    errors
}

#[instrument(skip(query, commands))]
fn derive_triples(
    query: Query<(Entity, &Label, &Element), (Changed<Element>, With<Sparql>)>,
    mut commands: Commands,
) {
    for (entity, label, el) in &query {
        let turtle = el.0.value();

        let prefixes: Vec<_> = turtle
            .prefixes
            .iter()
            .flat_map(|prefix| {
                let url = prefix.value().value.expand(turtle)?;
                let url = swls_core::lsp_types::Url::parse(&url).ok()?;
                Some(Prefix {
                    url,
                    prefix: prefix.value().prefix.value().clone(),
                })
            })
            .collect();

        commands
            .entity(entity)
            .insert(Prefixes(prefixes, label.0.clone()));

        match el.0.get_simple_triples() {
            Ok(triples) => {
                let triples: Vec<_> = triples.iter().map(|x| x.to_owned()).collect();
                commands.entity(entity).insert(Triples(Arc::new(triples)));
            }
            Err(e) => {
                tracing::warn!("derive_triples error for {}: {:?}", label.as_str(), e);
            }
        }
    }
}

#[instrument(skip(query))]
pub fn variable_completion(
    mut query: Query<(&Triples, &TokenComponent, &mut CompletionRequest), With<Sparql>>,
) {
    for (triples, token, mut req) in &mut query {
        if token.text.starts_with('?') {
            let var_set: HashSet<_> = triples
                .0
                .iter()
                .flat_map(|q| [q.s(), q.p(), q.o()])
                .filter(|t| t.kind() == TermKind::Variable)
                .map(|t| t.value.clone())
                .collect();

            for name in var_set {
                let t = format!("?{}", name);
                let completion = SimpleCompletion::new(
                    CompletionItemKind::VARIABLE,
                    t.clone(),
                    swls_core::lsp_types::TextEdit {
                        range: token.range.clone(),
                        new_text: t,
                    },
                );
                req.push(completion);
            }
        }
    }
}

#[cfg(test)]
mod format_tests {
    use swls_core::prelude::*;
    use swls_test_utils::{create_file, setup_world, TestClient};
    use test_log::test;

    fn format_with(sparql_enabled: Option<bool>) -> Option<String> {
        let (mut world, _rx) = setup_world(TestClient::new(), crate::setup_world);

        let mut cfg = ServerConfig::default();
        cfg.config.local.format.sparql = sparql_enabled;
        world.insert_resource(cfg);

        let entity = create_file(
            &mut world,
            "SELECT ?s WHERE { ?s <a> ?o . FILTER(?o>5) }",
            "http://example.com/query.rq",
            "sparql",
            Open,
        );
        // ParseLabel populates the green node the formatter consumes.
        world.run_schedule(ParseLabel);

        world.entity_mut(entity).insert(FormatRequest(None));
        world.run_schedule(FormatLabel);

        world
            .entity_mut(entity)
            .take::<FormatRequest>()
            .and_then(|r| r.0)
            .map(|edits| edits[0].new_text.clone())
    }

    fn format_update(src: &str) -> (bool, Option<String>) {
        let (mut world, _rx) = setup_world(TestClient::new(), crate::setup_world);
        let mut cfg = ServerConfig::default();
        cfg.config.local.format.sparql = Some(true);
        world.insert_resource(cfg);

        let entity = create_file(&mut world, src, "http://example.com/update.ru", "sparql", Open);
        world.run_schedule(ParseLabel);
        let dirty = world.entity(entity).contains::<Dirty>();

        world.entity_mut(entity).insert(FormatRequest(None));
        world.run_schedule(FormatLabel);
        let formatted = world
            .entity_mut(entity)
            .take::<FormatRequest>()
            .and_then(|r| r.0)
            .map(|edits| edits[0].new_text.clone());
        (dirty, formatted)
    }

    #[test]
    fn update_files_parse_and_format() {
        let (dirty, formatted) = format_update(
            "PREFIX ex: <http://ex/> INSERT DATA { ex:a ex:b ex:c } ; DELETE { ?s ex:p ?o } WHERE { ?s ex:p ?o . FILTER(?o>1) }",
        );
        assert!(!dirty, "a valid update should parse without errors");
        assert_eq!(
            formatted.as_deref(),
            Some(
                "PREFIX ex: <http://ex/>\n\nINSERT DATA {\n  ex:a ex:b ex:c\n};\n\nDELETE {\n  ?s ex:p ?o\n}\nWHERE {\n  ?s ex:p ?o.\n  FILTER (?o > 1)\n}\n"
            )
        );
    }

    #[test]
    fn sparql_formatting_off_by_default() {
        assert!(
            format_with(None).is_none(),
            "SPARQL formatting should be disabled unless explicitly enabled"
        );
    }

    #[test]
    fn sparql_formatting_can_be_enabled() {
        assert_eq!(
            format_with(Some(true)).as_deref(),
            Some("SELECT ?s\nWHERE {\n  ?s <a> ?o.\n  FILTER (?o > 5)\n}\n")
        );
    }
}
