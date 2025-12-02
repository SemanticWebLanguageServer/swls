use bevy_ecs::prelude::*;
use lsp_core::{
    components::Element, feature::semantic::TokenTypesComponent, lsp_types::SemanticTokenType,
    prelude::*,
};
use sophia_api::term::{Term, TermKind};

use crate::{lang::parser, JsonLd};

fn walk_json(json: &Spanned<parser::Json>, ttc: &mut Vec<Spanned<SemanticTokenType>>) {
    let check_token =
        |token: &Spanned<Token>, ttc: &mut Vec<Spanned<SemanticTokenType>>| match token.value() {
            Token::Str(x, _) if x.starts_with("@") => {
                ttc.push(Spanned(SemanticTokenType::KEYWORD, token.span().clone()));
            }
            _ => {}
        };

    match json.value() {
        parser::Json::Token(token) => check_token(&Spanned(token.clone(), json.1.clone()), ttc),
        parser::Json::Array(vec) => {
            vec.iter().for_each(|json| walk_json(json, ttc));
        }
        parser::Json::Object(vec) => {
            for o in vec.iter() {
                let v = match o.value() {
                    parser::ObjectMember::Full(k, v) => {
                        check_token(k, ttc);
                        v
                    }
                    parser::ObjectMember::Partial(k, _, mv) => {
                        check_token(k, ttc);
                        if let Some(v) = mv {
                            v
                        } else {
                            continue;
                        }
                    }
                };
                walk_json(&v, ttc);
            }
        }
        _ => {}
    }
}

pub fn highlight_named_nodes(
    mut query: Query<
        (&Triples, &mut TokenTypesComponent),
        (With<HighlightRequest>, With<Element<JsonLd>>),
    >,
) {
    for (triples, mut ttc) in &mut query {
        for MyQuad {
            subject,
            predicate,
            object,
            ..
        } in triples.iter()
        {
            for t in [subject, predicate, object] {
                if t.kind() == TermKind::Iri {
                    let s = &t.span;
                    ttc.push(Spanned(SemanticTokenType::PROPERTY, s.start - 1..s.end + 1));
                }
            }
        }
    }
}

pub fn keyword_highlight(
    mut query: Query<(&Element<JsonLd>, &mut TokenTypesComponent), With<HighlightRequest>>,
) {
    for (json, mut ttc) in &mut query {
        walk_json(&json.0, &mut ttc.0);
    }
}
