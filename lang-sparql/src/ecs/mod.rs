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
    systems::{prefix::prefix_completion_helper, PrefixEntry},
};
use swls_lang_turtle::lang::model::{NamedNodeExt, TurtleExt};
use swls_lov::LocalPrefix;

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

pub fn setup_completion(world: &mut World) {
    use swls_core::feature::completion::*;
    world.schedule_scope(Label, |_, schedule| {
        schedule.add_systems((
            sparql_lov_undefined_prefix_completion.after(generate_completions),
            variable_completion.after(generate_completions),
        ));
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

        let prev = prev_infos.get(label.as_str());
        let (parse, new_prev) = rdf_parsers::parse_incremental(
            Rule::new(SyntaxKind::QueryUnit),
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
        let element = Element::<Sparql>(swls_core::prelude::spanned(turtle_model, span));

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
    query: Query<(Entity, &Label, &Element<Sparql>), Changed<Element<Sparql>>>,
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
                info!("derive_triples error for {}: {:?}", label.as_str(), e);
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

pub fn sparql_lov_undefined_prefix_completion(
    mut query: Query<(
        &TokenComponent,
        &Element<Sparql>,
        &Prefixes,
        &mut CompletionRequest,
        &DynLang,
    )>,
    lovs: Query<&LocalPrefix>,
    prefix_cc: Query<&PrefixEntry>,
    config: Res<ServerConfig>,
) {
    for (word, el, prefixes, mut req, lang) in &mut query {
        let turtle = el.0.value();
        let mut start = swls_core::lsp_types::Position::new(0, 0);
        if turtle.base.is_some() {
            start = swls_core::lsp_types::Position::new(1, 0);
        }

        use swls_core::lsp_types::Range;
        prefix_completion_helper(
            word,
            prefixes,
            &mut req.0,
            |name, location| {
                Some(vec![swls_core::lsp_types::TextEdit {
                    range: Range::new(start.clone(), start),
                    new_text: format!("PREFIX {}: <{}>\n", name, location),
                }])
            },
            lovs.iter(),
            prefix_cc.iter(),
            &config.config.local,
            lang,
        );
    }
}
