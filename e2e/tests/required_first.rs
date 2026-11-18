//! Predicates that SHACL shapes require and the subject does not have yet are suggested
//! before everything else.

use swls_core::feature::completion::{sort_completions, SimpleCompletion};
use swls_e2e_tests::LspHarness;

const SHAPES_URL: &str = "https://example.test/shapes.ttl";
const SHAPES: &str = r#"@prefix ex: <http://ex.test/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

ex:PersonShape a sh:NodeShape ;
  sh:targetClass ex:Person ;
  sh:property [ sh:path ex:nickname ] ;
  sh:property [ sh:path ex:name ; sh:minCount 1 ] ;
  sh:property [ sh:path ex:age ; sh:minCount 1 ] ;
  sh:property [ sh:path [ sh:alternativePath ( ex:email ex:phone ) ] ; sh:minCount 1 ] .
"#;

/// An ontology that also defines `ex:name`, so it is offered by property completion too.
const ONTO: &str = "@prefix ex: <http://ex.test/>.\n\
    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.\n\
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.\n\
    ex:name a rdf:Property ; rdfs:label \"name\" .\n\
    ex:about a rdf:Property .";

fn harness() -> LspHarness {
    let mut h = LspHarness::with_resources([(SHAPES_URL, SHAPES)]);
    h.load_config(|c| {
        c.shapes.insert(SHAPES_URL.into());
    });
    h.open_linked_file("file:///onto.ttl", "turtle", ONTO);
    h.drain_tasks();
    h
}

/// Completions in the order the server returns them (sorted by sort text).
fn sorted(h: &mut LspHarness, src: &str, line: u32, col: u32) -> Vec<SimpleCompletion> {
    let file = h.open_file_and_drain("file:///data.ttl", "turtle", src);
    let mut items = h.completions(&file, line, col);
    sort_completions(&mut items);
    items
}

fn labels(items: &[SimpleCompletion]) -> Vec<&str> {
    items.iter().map(|c| c.label.as_str()).collect()
}

fn description(item: &SimpleCompletion) -> Option<&str> {
    item._label_details.as_ref()?.description.as_deref()
}

#[test_log::test]
fn missing_required_predicates_come_first() {
    let mut h = harness();
    let items = sorted(
        &mut h,
        "@prefix ex: <http://ex.test/>.\n<p> a ex:Person ;\n  ",
        2,
        2,
    );
    let top = labels(&items[..4]);

    // Required: `ex:age` (shapes only) and `ex:name` (also in the ontology); then one of
    // the required alternatives `ex:email` / `ex:phone`.
    assert_eq!(
        top,
        ["ex:age", "ex:name", "ex:email", "ex:phone"],
        "{:?}",
        labels(&items)
    );
    assert_eq!(description(&items[0]), Some("required by shape"));
    // An ontology suggestion keeps its title after the marker.
    assert_eq!(description(&items[1]), Some("required by shape · name"));
    assert_eq!(description(&items[2]), Some("one of required by shape"));

    // Optional predicates are not promoted.
    let nickname = labels(&items)
        .iter()
        .position(|l| *l == "ex:nickname")
        .unwrap();
    assert!(nickname > 3, "{:?}", labels(&items));
}

#[test_log::test]
fn present_predicates_are_no_longer_promoted() {
    let mut h = harness();
    let src = "@prefix ex: <http://ex.test/>.\n<p> a ex:Person ;\n  ex:name \"Bob\" ;\n  ex:phone \"1\" ;\n  ";
    let items = sorted(&mut h, src, 4, 2);
    assert_eq!(labels(&items[..1]), ["ex:age"], "{:?}", labels(&items));
    // The alternatives are satisfied by `ex:phone`, and `ex:name` is present.
    for l in ["ex:name", "ex:email"] {
        if let Some(item) = items.iter().find(|c| c.label == l) {
            assert!(
                !item
                    ._sort_text
                    .as_deref()
                    .unwrap_or_default()
                    .starts_with('!'),
                "{l} should not be promoted"
            );
        }
    }
}

#[test_log::test]
fn required_predicates_come_before_templates_and_keywords() {
    let mut h = harness();
    let items = sorted(
        &mut h,
        "@prefix ex: <http://ex.test/>.\n<p> a ex:Person ;\n  ",
        2,
        2,
    );
    let first_template = labels(&items)
        .iter()
        .position(|l| l.starts_with("shape template"))
        .unwrap();
    let first_keyword = labels(&items).iter().position(|l| *l == "a").unwrap();
    assert!(
        first_template >= 4 && first_keyword >= 4,
        "{:?}",
        labels(&items)
    );
}
