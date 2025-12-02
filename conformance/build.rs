use std::{
    collections::HashSet,
    env, fs,
    path::{self, PathBuf},
};

use sophia_api::{
    graph::Graph, ns::rdf, parser::TripleParser, prelude::Any, source::TripleSource, term::Term,
    triple::Triple,
};
use sophia_inmem::graph::FastGraph;
use sophia_iri::Iri;

pub mod rdfs {
    use sophia_api::namespace;
    namespace! {
      "http://www.w3.org/2000/01/rdf-schema#",
      comment
    }
}

pub mod mf {
    use sophia_api::namespace;
    namespace! {
      "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#",
      entries, name, action, result
    }
}

pub mod rdft {
    use sophia_api::namespace;
    namespace! {
      "http://www.w3.org/ns/rdftest#",
      approval, Approved, TestTurtleEval, TestTurtlePositiveSyntax, TestTurtleNegativeSyntax
    }
}

fn basic_info<T: Term + Clone>(
    graph: &FastGraph,
    subj: T,
) -> Option<(String, Option<String>, String, String)> {
    let name = graph
        .triples_matching([subj.clone()], [mf::name], Any)
        .next()?
        .ok()?
        .o()
        .lexical_form()?
        .to_string();

    let comment = graph
        .triples_matching([subj.clone()], [rdfs::comment], Any)
        .next()
        .and_then(|x| Some(x.ok()?.o().lexical_form()?.to_string()));

    let approval = graph
        .triples_matching([subj.clone()], [rdft::approval], Any)
        .next()?
        .ok()?
        .o()
        .iri()?
        .to_string();

    let action = graph
        .triples_matching([subj.clone()], [mf::action], Any)
        .next()?
        .ok()?
        .o()
        .iri()?
        .to_string();
    Some((name, comment, approval, action))
}

fn make_unique(sanitized: String, existing: &mut HashSet<String>) -> String {
    let mut name = sanitized.to_string();
    let mut counter = 1;
    while existing.contains(&name) {
        name = format!("{}_{}", sanitized, counter);
        counter += 1;
    }
    existing.insert(name.clone());
    name
}
fn sanitize_test_name(original: &str, existing: &mut HashSet<String>) -> String {
    let mut s = String::new();
    // Prefix if first char is not a letter or underscore
    if !original
        .chars()
        .next()
        .map(|c| c.is_ascii_alphabetic() || c == '_')
        .unwrap_or(false)
    {
        s.push_str("case_");
    }
    for c in original.chars() {
        if c.is_ascii_alphanumeric() || c == '_' {
            s.push(c);
        } else {
            s.push('_');
        }
    }

    make_unique(s, existing)
}

#[derive(Debug)]
struct TurtleEval {
    name: String,
    comment: Option<String>,
    approval: String,
    action: String,
    result: String,
}

impl TurtleEval {
    fn from_graph<S: Term + Clone>(graph: &FastGraph, subj: S) -> Option<Self> {
        let (name, comment, approval, action) = basic_info(graph, subj.clone())?;

        let result = graph
            .triples_matching([subj.clone()], [mf::result], Any)
            .next()?
            .ok()?
            .o()
            .iri()?
            .to_string();

        Some(Self {
            name,
            comment,
            approval,
            action,
            result,
        })
    }

    fn add_to_string(&self, string: &mut String, existing: &mut HashSet<String>) {
        let name = sanitize_test_name(&self.name, existing);

        string.push_str(&format!(
            "
#[test]
pub fn {}() {{
    test_syntax(\"{}\", {});
}}\n",
            name, self.action, true
        ));
    }
}

#[derive(Debug)]
struct TurtleSyntax {
    name: String,
    comment: Option<String>,
    approval: String,
    action: String,
    is_positive: bool,
}

impl TurtleSyntax {
    fn from_graph<S: Term + Clone>(graph: &FastGraph, subj: S, positive: bool) -> Option<Self> {
        let (name, comment, approval, action) = basic_info(graph, subj.clone())?;

        Some(Self {
            name,
            comment,
            approval,
            action,
            is_positive: positive,
        })
    }

    fn add_to_string(&self, string: &mut String, existing: &mut HashSet<String>) {
        let name = sanitize_test_name(&self.name, existing);

        string.push_str(&format!(
            "
#[test]
pub fn {}() {{
    test_syntax(\"{}\", {});
}}",
            name, self.action, self.is_positive
        ));
    }
}

fn main() {
    println!("cargo:rerun-if-changed=conformance_tests.json");

    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let path = path::absolute("./w3c/rdf/rdf11/rdf-turtle/manifest.ttl").unwrap();
    let location = path.to_str().unwrap();

    println!("cargo:warning=Location {}", location);
    let tests_manifest = fs::read_to_string(&path).expect("Failed to read spec");

    let base = Iri::new(format!("file://{}", location)).unwrap();
    let parser = sophia_turtle::parser::turtle::TurtleParser {
        base: Some(base.clone()),
    };

    let quads: FastGraph = parser
        .parse_str(&tests_manifest)
        .collect_triples()
        .expect("valid turtle");

    let mut evals = 0;
    let mut generated = "\n".to_string();

    let mut names = HashSet::new();

    for subject in quads
        .triples_matching(Any, [rdf::type_], [rdft::TestTurtleEval])
        .flatten()
        .map(|t| t.to_s())
    {
        if let Some(turtle) = TurtleEval::from_graph(&quads, subject.to_owned()) {
            turtle.add_to_string(&mut generated, &mut names);
            evals += 1;
        }
    }

    let mut syntax = 0;
    for subject in quads
        .triples_matching(Any, [rdf::type_], [rdft::TestTurtlePositiveSyntax])
        .flatten()
        .map(|t| t.to_s())
    {
        if let Some(turtle) = TurtleSyntax::from_graph(&quads, subject.to_owned(), true) {
            turtle.add_to_string(&mut generated, &mut names);
            syntax += 1;
        }
    }

    for subject in quads
        .triples_matching(Any, [rdf::type_], [rdft::TestTurtleNegativeSyntax])
        .flatten()
        .map(|t| t.to_s())
    {
        if let Some(turtle) = TurtleSyntax::from_graph(&quads, subject.to_owned(), false) {
            turtle.add_to_string(&mut generated, &mut names);
            syntax += 1;
        }
    }

    println!("cargo:warning=Eval {} Syntax {}", evals, syntax);
    fs::write(out_dir.join("generated_tests.rs"), generated)
        .expect("Failed to write generated tests");
}
