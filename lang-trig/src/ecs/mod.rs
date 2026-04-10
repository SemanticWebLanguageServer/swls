use std::collections::HashMap;

use bevy_ecs::prelude::*;
use rdf_parsers::{IncrementalBias, PrevParseInfo};
use rowan::NodeOrToken;
use swls_core::prelude::*;
use swls_lang_turtle::ecs::parse::derive_triples_system;
use swls_lang_turtle::lang::model::NamedNodeExt;
use swls_lang_turtle::lang::parser::TurtleParseError;
use tracing::{info, instrument};

use crate::TriGLang;

pub mod completion;

pub fn setup_parsing(world: &mut World) {
    use swls_core::feature::parse::*;
    world.schedule_scope(ParseLabel, |_, schedule| {
        schedule.add_systems((
            parse_trig_system,
            derive_prefixes.after(parse_trig_system).before(prefixes),
            derive_triples_system::<TriGLang>
                .after(parse_trig_system)
                .before(triples),
        ));
    });
}

pub fn setup_completion(world: &mut World) {
    use swls_core::feature::completion::*;
    world.schedule_scope(Label, |_, schedule| {
        schedule.add_systems(
            completion::trig_lov_undefined_prefix_completion.after(generate_completions),
        );
    });
}

fn extract_trig_cst_tokens(
    node: &rowan::SyntaxNode<rdf_parsers::trig::parser::Lang>,
) -> Vec<Spanned<rowan::SyntaxKind>> {
    use rdf_parsers::trig::parser::SyntaxKind;
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
    node: &rowan::SyntaxNode<rdf_parsers::trig::parser::Lang>,
) -> Vec<TurtleParseError> {
    use rdf_parsers::trig::parser::SyntaxKind;
    let mut errors = Vec::new();
    let mut stack = vec![node.clone()];
    while let Some(current) = stack.pop() {
        for child in current.children_with_tokens() {
            match child {
                NodeOrToken::Node(n) => {
                    if n.kind() == SyntaxKind::Error {
                        let range = rdf_parsers::effective_error_span::<
                            rdf_parsers::trig::parser::Lang,
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
fn parse_trig_system(
    query: Query<(Entity, &Source, &Label), (Changed<Source>, With<TriGLang>)>,
    mut commands: Commands,
    mut prev_infos: Local<HashMap<String, PrevParseInfo>>,
    config: Res<ServerConfig>,
) {
    if !config.config.trig.unwrap_or(true) {
        return;
    }
    for (entity, source, label) in &query {
        use rdf_parsers::trig::convert::convert;
        use rdf_parsers::trig::parser::{Lang, Rule, SyntaxKind};

        let prev = prev_infos.get(label.as_str());
        let (parse, new_prev) = rdf_parsers::parse_incremental(
            Rule::new(SyntaxKind::TrigDoc),
            source.0.as_str(),
            prev,
            IncrementalBias::default(),
        );
        prev_infos.insert(label.to_string(), new_prev);

        let syntax = parse.syntax::<Lang>();
        let errors = collect_errors(&syntax);
        let mut trig_model = convert(&syntax);
        trig_model.set_base = Some(label.to_string());

        let cst_tokens = extract_trig_cst_tokens(&syntax);

        info!(
            "{} triples ({} parse errors)",
            trig_model.triples.len(),
            errors.len()
        );

        let span = 0..source.0.len();
        let element = Element::<TriGLang>(spanned(trig_model, span));

        if errors.is_empty() {
            commands
                .entity(entity)
                .insert((element, Errors(errors), CstTokens(cst_tokens)))
                .remove::<Dirty>();
        } else {
            commands
                .entity(entity)
                .insert((element, Errors(errors), CstTokens(cst_tokens), Dirty));
        }
    }
}

#[instrument(skip(query, commands))]
fn derive_prefixes(
    query: Query<(Entity, &Label, &Element<TriGLang>), Changed<Element<TriGLang>>>,
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
