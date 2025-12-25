#![allow(non_snake_case)]
use std::{collections::HashSet, str::FromStr as _};

use lang_turtle::lang::{
    model::{Term, Triple, Turtle},
    parse_source,
    // parser2::parse_source as parse_source2,
};

fn check_turtle_defined_prefixes(turtle: Option<&Turtle>) -> bool {
    // check defined prefixes
    let mut prefixes = HashSet::new();
    if let Some(turtle) = turtle {
        for pref in &turtle.prefixes {
            prefixes.insert(pref.value().prefix.value().to_string());
        }

        turtle.triples.iter().all(|t| check_triple(&t, &prefixes))
    } else {
        true
    }
}

pub fn test_syntax(location: &str, is_positive: bool) {
    let url = lsp_core::lsp_types::Url::from_str(location).unwrap();
    let path = url.to_file_path().expect("file path");
    let turtle_source = std::fs::read_to_string(&path).expect("Failed to turtle");
    let (turtle, errors) = parse_source(&url, &turtle_source);
    // let turtle2 = parse_source2(&url, &turtle_source);

    let combinator = errors.is_empty() && turtle.is_some();
    // let new = turtle2.is_some();
    // assert!(turtle.is_some(), "Always return some turtle");
    let defined_prefixes = check_turtle_defined_prefixes(turtle.as_ref());

    // if combinator != new {
    //     eprintln!(
    //         "Expected new parser to be the same as the old one. Old {} New {}",
    //         combinator, new
    //     );
    //
    //     println!("Trutle\n{:?}", turtle2);
    //     for e in &errors {
    //         eprintln!("Error {:?}", e)
    //     }
    //
    //     assert!(false, "Expected new parser to be the same as the old one");
    // }
    if is_positive {
        if !combinator || !defined_prefixes {
            eprintln!(
                "Failed to parse turtle\n{}\n\nErrors {:?}",
                turtle_source, errors
            );
            assert!(false, "Parsing failed, and shouldn't have failed");
        }
    } else {
        if errors.is_empty() && turtle.is_some() && defined_prefixes {
            eprintln!(
                "Failed to succeeded but shouldn't have: turtle\n{}",
                turtle_source
            );

            assert!(false, "Parsing succeeded, and should have failed");
        }
    }
}

fn check_triple(triple: &Triple, defined: &HashSet<String>) -> bool {
    if !check_term(&triple.subject, &defined) {
        return false;
    }

    for po in &triple.po {
        if !check_term(&po.predicate, defined) {
            return false;
        }

        if !po.object.iter().all(|t| check_term(t, defined)) {
            return false;
        }
    }

    true
}

fn check_term(term: &Term, defined: &HashSet<String>) -> bool {
    match term {
        Term::BlankNode(lang_turtle::lang::model::BlankNode::Unnamed(pos, _, _)) => {
            for po in pos {
                if !check_term(&po.predicate, defined) {
                    return false;
                }

                if !po.object.iter().all(|t| check_term(t, defined)) {
                    return false;
                }
            }
            true
        }
        Term::NamedNode(lang_turtle::lang::model::NamedNode::Prefixed { prefix, .. }) => {
            defined.contains(prefix)
        }
        Term::Collection(spanneds) => spanneds.iter().all(|t| check_term(t, defined)),
        _ => true,
    }
}

include!(concat!(env!("OUT_DIR"), "/generated_tests.rs"));
