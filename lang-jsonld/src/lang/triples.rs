use std::{borrow::Cow, ops::Range};

use lsp_core::prelude::*;

use crate::lang::parser::{Json, ObjectMember};

fn visit_obj(json: &Spanned<Json>, f: &mut dyn FnMut(&[Spanned<ObjectMember>], &Range<usize>)) {
    match json.value() {
        Json::Array(vec) => {
            for v in vec.iter() {
                visit_obj(v, f);
            }
        }
        Json::Object(vec) => f(&vec, json.span()),
        _ => {}
    }
}

fn get_str(tok: &Token) -> Option<&str> {
    match tok {
        Token::Str(x, _) => Some(x),
        _ => None,
    }
}

fn correct_field(obj: &ObjectMember, field: &str) -> bool {
    let f = obj.field();
    match f.value() {
        Token::Str(st, _) => st == field,
        _ => false,
    }
}

fn find_field<'a>(
    mem: &'a [Spanned<ObjectMember>],
    field: &str,
) -> Option<(&'a Spanned<Json>, &'a Range<usize>)> {
    mem.iter()
        .find(|x| correct_field(x, field))
        .and_then(|x| x.json_value().map(|y| (y, x.field().span())))
}

pub fn derive_prefixes(json: &Spanned<Json>, base: &lsp_core::lsp_types::Url) -> Prefixes {
    let mut options: Vec<(Cow<str>, Cow<str>)> = Vec::new();

    // Extract prefixes
    visit_obj(json, &mut |mem, _| {
        let Some((ctx, _)) = find_field(mem, "@context") else {
            return;
        };

        visit_obj(ctx, &mut |mems, _| {
            for mem in mems {
                let Some(key) = get_str(mem.field()) else {
                    continue;
                };

                let Some(value) = mem
                    .json_value()
                    .and_then(|x| x.as_ref().try_map(|x| x.token()))
                    .and_then(|x| x.try_map(|x| get_str(x)))
                else {
                    continue;
                };

                options.push((Cow::Owned(key.to_string()), Cow::Owned(value.to_string())));
            }
        });
    });

    // Expand prefixes
    let mut changed = true;
    let mut steps = 0;
    while changed && steps < 5 {
        changed = false;
        for i in 0..options.len() {
            let new_expaned = if let Some(pref) = options[i].1.find(':') {
                let prefix = &options[i].1[..pref];
                let Some((_, expaned)) = options.iter().find(|x| x.0 == prefix) else {
                    continue;
                };
                format!("{}{}", expaned, &options[i].1[pref + 1..])
            } else {
                continue;
            };
            options[i].1 = Cow::from(new_expaned);
            changed = true;
        }
        steps += 1;
    }
    let mut out = Vec::new();

    for (k, v) in options {
        if let Some(url) = lsp_core::lsp_types::Url::parse(&v).ok() {
            out.push(Prefix {
                prefix: k.into_owned(),
                url,
            });
        }
    }

    Prefixes(out, base.clone())
}

fn shorten_span(span: &Range<usize>) -> Range<usize> {
    span.start + 1..span.end - 1
}

fn derive_triples_sub(
    json: &Spanned<Json>,
    prefixes: &Prefixes,
    out: &mut Vec<MyQuad<'static>>,
    bn_f: &mut dyn FnMut(Range<usize>) -> MyTerm<'static>,
) -> Option<MyTerm<'static>> {
    let mut subj_out = None;
    visit_obj(json, &mut |mems, span| {
        let subj = find_field(mems, "@id")
            .and_then(|x| x.0.as_ref().try_map(|x| x.token()))
            .and_then(|x| x.try_map(|x| prefixes.expand_json(x)))
            .map(|Spanned(v, r)| MyTerm::named_node(v, shorten_span(&r)))
            .unwrap_or_else(|| bn_f(span.clone()));

        if let Some((ctx, _)) = find_field(mems, "@graph") {
            derive_triples_sub(ctx, prefixes, out, bn_f);
        }
        if let Some((mem, span)) = find_field(mems, "@type") {
            let object = match mem {
                Spanned(Json::Token(tok), span) => {
                    if let Some(st) = prefixes.expand_json(&tok) {
                        MyTerm::named_node(st, span.clone())
                    } else {
                        MyTerm::invalid(span.clone())
                    }
                }
                json => derive_triples_sub(json, prefixes, out, bn_f)
                    .unwrap_or_else(|| MyTerm::invalid(json.span().clone())),
            };

            out.push(MyQuad {
                subject: subj.clone(),
                predicate: MyTerm::named_node(
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
                    span.clone(),
                ),
                object,
                span: mem.span().clone(),
            });
        }
        for mem in mems {
            let field = mem.field();
            let st = get_str(&field);
            if st.map(|x| x.starts_with("@")).unwrap_or(false) {
                continue;
            }

            let Some(pred) = prefixes
                .expand_json(&field)
                .or_else(|| st.map(|x| x.to_string()))
            else {
                continue;
            };
            let pred = MyTerm::named_node(pred, shorten_span(field.span()));

            // get value
            let object = match mem.json_value() {
                None => MyTerm::invalid(0..0),
                // TODO: this might not be a literal, should look it up in the context
                Some(Spanned(Json::Token(tok), span)) => match tok {
                    Token::Str(x, _) => MyTerm::literal(x.clone(), span.clone()),
                    _ => MyTerm::invalid(span.clone()),
                },
                Some(json) => derive_triples_sub(json, prefixes, out, bn_f)
                    .unwrap_or_else(|| MyTerm::invalid(json.span().clone())),
            };

            out.push(MyQuad {
                subject: subj.clone(),
                predicate: pred,
                object,
                span: mem.span().clone(),
            });
        }

        subj_out = Some(subj);
    });
    subj_out
}

pub fn derive_triples(json: &Spanned<Json>, prefixes: &Prefixes) -> Vec<MyQuad<'static>> {
    let mut bn_count = 0;
    let mut bn = move |span| {
        let out = MyTerm::blank_node(format!("_:{}", bn_count), span);
        bn_count += 1;
        out
    };
    let mut out = Vec::new();
    derive_triples_sub(json, prefixes, &mut out, &mut bn);
    out
}

#[cfg(test)]
mod tests {

    use lsp_core::prelude::{MyQuad, Spanned};
    use sophia_api::term::{Term, TermKind};

    use super::{derive_prefixes, derive_triples};
    use crate::lang::{
        parser::{parse, Json},
        tokenizer::tokenize,
    };

    fn parse_json(st: &str) -> Option<Spanned<Json>> {
        let (tok, es) = tokenize(st);
        if !es.is_empty() {
            return None;
        }
        let (jsonld, es) = parse(st, tok);
        if !es.is_empty() {
            return None;
        }
        Some(jsonld)
    }

    #[test]
    fn simple_context_foaf_1() {
        let st = r#" { 
            "@context": {"foaf": "http://xmlns.com/foaf/0.1/"},
            "@id": "http://example.com/ns#me",
            "foaf:name": "Arthur"
        } "#;
        let url = lsp_core::lsp_types::Url::parse("memory://test.jsonld").unwrap();

        let json = parse_json(st).expect("valid json");
        let prefixes = derive_prefixes(&json, &url);

        assert_eq!(prefixes.0.len(), 1);
        let foaf_prefix = prefixes
            .iter()
            .find(|x| x.prefix == "foaf")
            .expect("foaf prefix");
        assert_eq!(foaf_prefix.url.as_str(), "http://xmlns.com/foaf/0.1/");
    }

    #[test]
    fn simple_context_foaf_2() {
        let st = r#" { 
            "@context": [ {"foaf": "http://xmlns.com/foaf/0.1/"} ],
            "@id": "http://example.com/ns#me",
            "foaf:name": "Arthur"
        } "#;
        let url = lsp_core::lsp_types::Url::parse("memory://test.jsonld").unwrap();

        let json = parse_json(st).expect("valid json");
        let prefixes = derive_prefixes(&json, &url);

        assert_eq!(prefixes.0.len(), 1);

        let foaf_prefix = prefixes
            .iter()
            .find(|x| x.prefix == "foaf")
            .expect("foaf prefix");
        assert_eq!(foaf_prefix.url.as_str(), "http://xmlns.com/foaf/0.1/");
    }

    #[test]
    fn simple_context_foaf_3() {
        let st = r#" { 
            "@context": {"foaf": "http://xmlns.com/foaf/0.1/", "name": "foaf:name"},
            "@id": "http://example.com/ns#me",
            "name": "Arthur"
        } "#;
        let url = lsp_core::lsp_types::Url::parse("memory://test.jsonld").unwrap();

        let json = parse_json(st).expect("valid json");
        let prefixes = derive_prefixes(&json, &url);

        assert_eq!(prefixes.0.len(), 2);
        let name_prefix = prefixes
            .iter()
            .find(|x| x.prefix == "name")
            .expect("name prefix");
        assert_eq!(name_prefix.url.as_str(), "http://xmlns.com/foaf/0.1/name");

        let foaf_prefix = prefixes
            .iter()
            .find(|x| x.prefix == "foaf")
            .expect("foaf prefix");
        assert_eq!(foaf_prefix.url.as_str(), "http://xmlns.com/foaf/0.1/");
    }

    #[test]
    fn simple_context_foaf_3_ignore_extra_ctx() {
        let st = r#" { 
            "@context": [ {"foaf": "http://xmlns.com/foaf/0.1/"}, "http://xmlns.com/foaf/0.1/context.jsonld" ],
            "@id": "http://example.com/ns#me",
            "foaf:name": "Arthur"
        } "#;
        let url = lsp_core::lsp_types::Url::parse("memory://test.jsonld").unwrap();

        let json = parse_json(st).expect("valid json");
        let prefixes = derive_prefixes(&json, &url);

        assert_eq!(prefixes.0.len(), 1);
        let foaf_prefix = prefixes
            .iter()
            .find(|x| x.prefix == "foaf")
            .expect("foaf prefix");
        assert_eq!(foaf_prefix.url.as_str(), "http://xmlns.com/foaf/0.1/");
    }

    #[test]
    fn derive_simple_triples() {
        let st = r#" { 
            "@context": {"foaf": "http://xmlns.com/foaf/0.1/"} ,
            "@id": "http://example.com/ns#me",
            "foaf:name": "Arthur"
        } "#;
        let url = lsp_core::lsp_types::Url::parse("memory://test.jsonld").unwrap();

        let json = parse_json(st).expect("valid json");
        let prefixes = derive_prefixes(&json, &url);
        let triples = derive_triples(&json, &prefixes);

        assert_eq!(triples.len(), 1);
        let MyQuad {
            subject,
            predicate,
            object,
            ..
        } = &triples[0];

        assert_eq!(subject.as_str(), "http://example.com/ns#me");
        assert_eq!(subject.kind(), TermKind::Iri);
        assert_eq!(predicate.as_str(), "http://xmlns.com/foaf/0.1/name");
        assert_eq!(predicate.kind(), TermKind::Iri);
        assert_eq!(object.as_str(), "Arthur");
        assert_eq!(object.kind(), TermKind::Literal);
    }

    #[test]
    fn derive_simple_triples_type() {
        let st = r#" { 
            "@context": {"foaf": "http://xmlns.com/foaf/0.1/"} ,
            "@id": "http://example.com/ns#me",
            "@type": "http://example.com/ns#my_type"
        } "#;
        let url = lsp_core::lsp_types::Url::parse("memory://test.jsonld").unwrap();

        let json = parse_json(st).expect("valid json");
        let prefixes = derive_prefixes(&json, &url);
        let triples = derive_triples(&json, &prefixes);

        assert_eq!(triples.len(), 1);
        let MyQuad {
            subject,
            predicate,
            object,
            ..
        } = &triples[0];

        assert_eq!(subject.as_str(), "http://example.com/ns#me");
        assert_eq!(subject.kind(), TermKind::Iri);
        assert_eq!(
            predicate.as_str(),
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
        );
        assert_eq!(predicate.kind(), TermKind::Iri);
        assert_eq!(object.as_str(), "http://example.com/ns#my_type");
        assert_eq!(object.kind(), TermKind::Iri);
    }

    #[test]
    fn derive_simple_triples_bn() {
        let st = r#" { 
            "@context": {"foaf": "http://xmlns.com/foaf/0.1/"} ,
            "foaf:name": "Arthur"
        } "#;
        let url = lsp_core::lsp_types::Url::parse("memory://test.jsonld").unwrap();

        let json = parse_json(st).expect("valid json");
        let prefixes = derive_prefixes(&json, &url);
        let triples = derive_triples(&json, &prefixes);

        assert_eq!(triples.len(), 1);
        let MyQuad {
            subject,
            predicate,
            object,
            ..
        } = &triples[0];

        assert_eq!(subject.kind(), TermKind::BlankNode);
        assert_eq!(predicate.as_str(), "http://xmlns.com/foaf/0.1/name");
        assert_eq!(predicate.kind(), TermKind::Iri);
        assert_eq!(object.as_str(), "Arthur");
        assert_eq!(object.kind(), TermKind::Literal);
    }

    #[test]
    fn derive_simple_triples_graph() {
        let st = r#" { 
            "@context": {"foaf": "http://xmlns.com/foaf/0.1/"} ,
            "@graph": [ {
                "@id": "http://example.com/ns#me",
                "foaf:name": "Arthur"
            } ]
        } "#;
        let url = lsp_core::lsp_types::Url::parse("memory://test.jsonld").unwrap();

        let json = parse_json(st).expect("valid json");
        let prefixes = derive_prefixes(&json, &url);
        let triples = derive_triples(&json, &prefixes);

        assert_eq!(triples.len(), 1);
        let MyQuad {
            subject,
            predicate,
            object,
            ..
        } = &triples[0];

        assert_eq!(subject.as_str(), "http://example.com/ns#me");
        assert_eq!(subject.kind(), TermKind::Iri);
        assert_eq!(predicate.as_str(), "http://xmlns.com/foaf/0.1/name");
        assert_eq!(predicate.kind(), TermKind::Iri);
        assert_eq!(object.as_str(), "Arthur");
        assert_eq!(object.kind(), TermKind::Literal);
    }

    #[test]
    fn derive_simple_triples_bn_graph() {
        let st = r#" {
            "@context": {"foaf": "http://xmlns.com/foaf/0.1/"} ,
            "@graph": [ {
                "foaf:name": "Arthur"
            } ]
        } "#;
        let url = lsp_core::lsp_types::Url::parse("memory://test.jsonld").unwrap();

        let json = parse_json(st).expect("valid json");
        let prefixes = derive_prefixes(&json, &url);
        let triples = derive_triples(&json, &prefixes);

        assert_eq!(triples.len(), 1);
        let MyQuad {
            subject,
            predicate,
            object,
            ..
        } = &triples[0];

        assert_eq!(subject.kind(), TermKind::BlankNode);
        assert_eq!(predicate.as_str(), "http://xmlns.com/foaf/0.1/name");
        assert_eq!(predicate.kind(), TermKind::Iri);
        assert_eq!(object.as_str(), "Arthur");
        assert_eq!(object.kind(), TermKind::Literal);
    }

    #[test]
    fn derive_simple_triples_deep() {
        let st = r#" { 
            "@context": {"foaf": "http://xmlns.com/foaf/0.1/"} ,
            "@id": "http://example.com/ns#me",
            "foaf:friend": {
                "foaf:name": "Arthur",
                "foaf:friend": {
                    "foaf:name": "Julian"
                }
            }
        } "#;
        let url = lsp_core::lsp_types::Url::parse("memory://test.jsonld").unwrap();

        let json = parse_json(st).expect("valid json");
        let prefixes = derive_prefixes(&json, &url);
        let triples = derive_triples(&json, &prefixes);

        assert_eq!(triples.len(), 4);
        let MyQuad {
            subject,
            predicate,
            object,
            ..
        } = triples
            .iter()
            .find(|x| x.subject.as_str() == "http://example.com/ns#me")
            .expect("my triple");
        assert_eq!(subject.as_str(), "http://example.com/ns#me");
        assert_eq!(subject.kind(), TermKind::Iri);
        assert_eq!(predicate.as_str(), "http://xmlns.com/foaf/0.1/friend");
        assert_eq!(predicate.kind(), TermKind::Iri);
        assert_eq!(object.kind(), TermKind::BlankNode);

        let friend: Vec<_> = triples.iter().filter(|x| &x.subject == object).collect();
        assert_eq!(friend.len(), 2);
        let friend_name = &friend
            .iter()
            .find(|x| x.predicate.as_str() == "http://xmlns.com/foaf/0.1/name")
            .expect("friend name")
            .object;
        assert_eq!(friend_name.as_str(), "Arthur");
        assert_eq!(friend_name.kind(), TermKind::Literal);

        let friend_friend = &friend
            .iter()
            .find(|x| x.predicate.as_str() == "http://xmlns.com/foaf/0.1/friend")
            .expect("friend name")
            .object;
        assert_eq!(friend_friend.kind(), TermKind::BlankNode);

        let friend_friend: Vec<_> = triples
            .iter()
            .filter(|x| &x.subject == friend_friend)
            .collect();
        assert_eq!(friend_friend.len(), 1);
        assert_eq!(friend_friend[0].object.as_str(), "Julian");
        assert_eq!(friend_friend[0].object.kind(), TermKind::Literal);
    }
}
