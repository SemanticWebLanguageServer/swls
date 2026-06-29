use std::collections::HashMap;

use bevy_ecs::prelude::*;
use rdf_parsers::{IncrementalBias, PrevParseInfo};
use rowan::{GreenNode, NodeOrToken};
use swls_core::prelude::*;
use swls_lang_turtle::{ecs::parse::derive_triples_system, lang::parser::TurtleParseError};
use tracing::instrument;

use crate::N3Lang;

pub fn setup_parsing(world: &mut World) {
    use swls_core::feature::parse::*;
    world.schedule_scope(ParseLabel, |_, schedule| {
        schedule.add_systems((
            parse_n3_system,
            swls_lang_rdf_base::parse::derive_prefixes_system::<N3Lang>
                .after(parse_n3_system)
                .before(prefixes),
            derive_triples_system::<N3Lang>
                .after(parse_n3_system)
                .before(triples),
        ));
    });
}

fn extract_n3_cst_tokens(
    node: &rowan::SyntaxNode<rdf_parsers::n3::parser::Lang>,
) -> Vec<Spanned<rowan::SyntaxKind>> {
    use rdf_parsers::n3::parser::SyntaxKind;
    let mut tokens = Vec::new();
    for node_or_token in node.descendants_with_tokens() {
        if let NodeOrToken::Token(t) = node_or_token {
            let kind = t.kind();
            if kind == SyntaxKind::WhiteSpace {
                continue;
            }
            let range = t.text_range();
            let span = usize::from(range.start())..usize::from(range.end());
            tokens.push(spanned(rowan::SyntaxKind(kind as u16), span));
        }
    }
    tokens
}

fn collect_errors(
    node: &rowan::SyntaxNode<rdf_parsers::n3::parser::Lang>,
) -> Vec<TurtleParseError> {
    use rdf_parsers::n3::parser::SyntaxKind;
    let mut errors = Vec::new();
    let mut stack = vec![node.clone()];
    while let Some(current) = stack.pop() {
        for child in current.children_with_tokens() {
            match child {
                NodeOrToken::Node(n) => {
                    if n.kind() == SyntaxKind::Error {
                        let range = rdf_parsers::effective_error_span::<
                            rdf_parsers::n3::parser::Lang,
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

#[instrument(skip(query, commands, prev_infos, config))]
fn parse_n3_system(
    query: Query<(Entity, &Source, &Label), (Changed<Source>, With<N3Lang>)>,
    mut commands: Commands,
    mut prev_infos: Local<HashMap<String, PrevParseInfo>>,
    config: Res<ServerConfig>,
) {
    if !config.config.n3.unwrap_or(true) {
        return;
    }
    for (entity, source, label) in &query {
        use rdf_parsers::n3::{
            convert::convert,
            parser::{Lang, Rule, SyntaxKind},
        };

        let prev = prev_infos.get(label.as_str());
        let (parse, new_prev) = rdf_parsers::parse_incremental(
            Rule::new(SyntaxKind::N3Doc),
            source.0.as_str(),
            prev,
            IncrementalBias::default(),
        );

        let gn = parse.green_node.clone();
        prev_infos.insert(label.to_string(), new_prev);

        let syntax = parse.syntax::<Lang>();
        let errors = collect_errors(&syntax);
        let mut n3_model = convert(&syntax);
        n3_model.set_base = Some(label.to_string());

        let cst_tokens = extract_n3_cst_tokens(&syntax);

        tracing::debug!(
            "{} triples ({} parse errors)",
            n3_model.triples.len(),
            errors.len()
        );

        let span = 0..source.0.len();
        let element = Element::<N3Lang>(spanned(n3_model, span));

        if errors.is_empty() {
            commands
                .entity(entity)
                .insert((element, Errors(errors), CstTokens(cst_tokens), Wrapped(gn)))
                .remove::<Dirty>();
        } else {
            commands.entity(entity).insert((
                element,
                Errors(errors),
                CstTokens(cst_tokens),
                Dirty,
                Wrapped(gn),
            ));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use swls_core::feature::ParseLabel;
    use swls_test_utils::{create_file, setup_world, TestClient};
    use test_log::test;

    #[test]
    fn n3_parses_triples_and_prefixes() {
        let (mut world, _rx) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        let src = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n\
                   <http://ex/me> a foaf:Person;\n\
                   \tfoaf:name \"Me\".\n";
        let entity = create_file(&mut world, src, "http://example.com/ns#", "n3", Open);
        world.run_schedule(ParseLabel);

        // The N3 source is detected, parsed without errors, and yields triples.
        let element: &Element<N3Lang> = world
            .entity(entity)
            .get()
            .expect("N3 element should be present");
        assert!(
            !element.0.triples.is_empty(),
            "expected triples from N3 parse"
        );
        assert!(
            world.entity(entity).get::<Dirty>().is_none(),
            "well-formed N3 should not be marked Dirty"
        );

        let prefixes: &Prefixes = world
            .entity(entity)
            .get()
            .expect("prefixes should be derived");
        assert!(
            prefixes
                .0
                .iter()
                .any(|p| p.url.as_str().contains("xmlns.com/foaf")),
            "foaf prefix should be derived from the N3 document"
        );
    }

    #[test]
    fn n3_rule_syntax_parses_without_errors() {
        let (mut world, _rx) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        // `{ ... } => { ... }` (a formula/implication) is N3-only syntax that is
        // *not* valid Turtle/TriG — this is the whole reason for a dedicated lang.
        let src = "@prefix : <http://ex/>.\n{ :a :b :c } => { :a :d :e }.\n";
        let entity = create_file(&mut world, src, "http://example.com/ns#", "n3", Open);
        world.run_schedule(ParseLabel);

        assert!(
            world.entity(entity).get::<Dirty>().is_none(),
            "N3 implication syntax should parse cleanly in the N3 language"
        );
    }

    // Regression: a prefix used only inside an N3 formula (`{ … } => { … }`)
    // must count as used. Previously the empty prefix `:` here was wrongly
    // flagged "declared but never used".
    #[test]
    fn prefix_used_only_in_formula_is_not_reported_unused() {
        let (mut world, mut rx) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        let src = "@prefix : <http://ex/> .\n\n{\n    :a :b :c\n} => {\n    :a :d :e \n} .\n";
        create_file(&mut world, src, "http://example.com/ns#", "n3", Open);
        world.run_schedule(ParseLabel);

        let mut messages: Vec<String> = Vec::new();
        while let Ok(Some(item)) = rx.try_next() {
            for d in item.diagnostics {
                messages.push(d.message);
            }
        }

        assert!(
            !messages.iter().any(|m| m.contains("never used")),
            "no prefix should be reported unused, got: {:?}",
            messages
        );
    }
}

pub(crate) fn format_n3_system(
    mut query: Query<(&RopeC, &Wrapped<GreenNode>, &mut FormatRequest), With<N3Lang>>,
    config: Res<ServerConfig>,
) {
    use swls_core::lsp_types::{Position, Range};
    if !config.config.format.n3() {
        tracing::debug!("N3 formatting disabled by config");
        return;
    }
    for (source, node, mut request) in &mut query {
        if request.0.is_some() {
            tracing::debug!("Didn't format with the n3 format system, already formatted");
            continue;
        }
        tracing::debug!("Formatting with n3 format system");

        let root = rowan::SyntaxNode::new_root(node.0.clone());

        let formatted = rdf_parsers::n3::format::format(&root, 80);

        request.0 = Some(vec![swls_core::lsp_types::TextEdit::new(
            Range::new(
                Position::new(0, 0),
                Position::new(source.0.len_lines() as u32 + 1, 0),
            ),
            formatted,
        )]);
    }
}
