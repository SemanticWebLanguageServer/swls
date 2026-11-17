//! IRIs are only written as prefixed names when the result is valid Turtle/SPARQL
//! (`ex:a/b` is not); otherwise they are written in full. The longest matching prefix wins.

use swls_e2e_tests::LspHarness;

const ONTO: &str = "@prefix ex: <http://ex.test/>.\n\
    @prefix owl: <http://www.w3.org/2002/07/owl#>.\n\
    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.\n\
    <http://ex.test/shapes/Widget> a owl:Class .\n\
    ex:Gadget a owl:Class .\n\
    <http://ex.test/props/size> a rdf:Property .";

fn labels(h: &mut LspHarness, src: &str, line: u32, col: u32) -> Vec<(String, String)> {
    let file = h.open_file_and_drain("file:///data.ttl", "turtle", src);
    h.completions(&file, line, col)
        .into_iter()
        .map(|c| (c.label.clone(), c.edits[0].new_text.clone()))
        .collect()
}

fn harness() -> LspHarness {
    let mut h = LspHarness::new();
    h.open_linked_file("file:///onto.ttl", "turtle", ONTO);
    h.drain_tasks();
    h
}

#[test_log::test]
fn class_with_a_slash_in_its_local_part_is_written_in_full() {
    let mut h = harness();
    let items = labels(&mut h, "@prefix ex: <http://ex.test/>.\n<x> a ", 1, 6);
    let texts: Vec<_> = items.iter().map(|(_, t)| t.as_str()).collect();
    assert!(texts.contains(&"ex:Gadget"), "{texts:?}");
    assert!(
        texts.contains(&"<http://ex.test/shapes/Widget>"),
        "{texts:?}"
    );
    assert!(!texts.iter().any(|t| t.contains("ex:shapes/")), "{texts:?}");
}

#[test_log::test]
fn the_longest_declared_prefix_wins() {
    let mut h = harness();
    let src = "@prefix ex: <http://ex.test/>.\n@prefix exs: <http://ex.test/shapes/>.\n<x> a ";
    let items = labels(&mut h, src, 2, 6);
    let texts: Vec<_> = items.iter().map(|(_, t)| t.as_str()).collect();
    assert!(texts.contains(&"exs:Widget"), "{texts:?}");
}

#[test_log::test]
fn property_with_a_slash_in_its_local_part_is_written_in_full() {
    let mut h = harness();
    let items = labels(
        &mut h,
        "@prefix ex: <http://ex.test/>.\n<x> a ex:Gadget ;\n  ",
        2,
        2,
    );
    let texts: Vec<_> = items.iter().map(|(_, t)| t.as_str()).collect();
    assert!(texts.contains(&"<http://ex.test/props/size>"), "{texts:?}");
    assert!(!texts.iter().any(|t| t.contains("ex:props/")), "{texts:?}");
}

#[test_log::test]
fn shape_docs_use_a_matching_prefix_of_the_shape_document() {
    let shapes = "@prefix ex: <http://ex.test/> .\n\
        @prefix exsh: <http://ex.test/shapes/> .\n\
        @prefix sh: <http://www.w3.org/ns/shacl#> .\n\
        exsh:PersonShape a sh:NodeShape ; sh:targetClass ex:Person ;\n\
          sh:property [ sh:path ex:name ; sh:node exsh:NameShape ] .";
    let mut h = LspHarness::with_resources([("https://example.test/shapes.ttl", shapes)]);
    h.load_config(|c| {
        c.shapes.insert("https://example.test/shapes.ttl".into());
    });
    let file = h.open_file_and_drain(
        "file:///data.ttl",
        "turtle",
        "@prefix ex: <http://ex.test/>.\n<p> a ex:Person ; ex:name \"Bob\" .",
    );
    let hover = h.hover(&file, 1, 19).join("\n");
    // The document's `ex:` would give `ex:shapes/PersonShape`, which is not a valid name.
    assert!(hover.contains("**Shape** `exsh:PersonShape`"), "{hover}");
    assert!(hover.contains("conforms to `exsh:NameShape`"), "{hover}");
    assert!(!hover.contains("ex:shapes/"), "{hover}");
}
