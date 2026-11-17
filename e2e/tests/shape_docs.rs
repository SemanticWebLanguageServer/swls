//! Documentation from SHACL property shapes on predicates (hover and completion), and the
//! preferred language (untagged, then English, then the first) wherever text is shown.

use swls_e2e_tests::LspHarness;

const SHAPES_URL: &str = "https://example.test/shapes.ttl";
const SHAPES: &str = r#"@prefix ex: <http://ex.test/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:PersonShape a sh:NodeShape ;
  sh:targetClass ex:Person ;
  sh:property [
    sh:path ex:name ; sh:minCount 1 ; sh:maxCount 1 ; sh:datatype xsd:string ;
    sh:name "naam"@nl , "name"@en ;
    sh:description "De volledige naam."@nl , "The full name."@en ;
    rdfs:label "Name label" ;
    rdfs:comment """A comment
       over two lines."""
  ] ;
  sh:property ex:StatusProperty ;
  sh:property [
    sh:path ex:age ; sh:minCount 1 ;
    sh:message "Leeftijd ontbreekt."@nl , "Age is missing."@en
  ] .

ex:StatusProperty sh:path ex:status ;
  sh:in ( ex:Active ex:Inactive ) ;
  sh:name "status" ;
  sh:severity sh:Warning .

ex:ThingShape a sh:NodeShape ;
  sh:targetClass ex:Thing ;
  sh:property [ sh:path ex:name ; sh:name "thing name" ] .
"#;

fn harness() -> LspHarness {
    let mut h = LspHarness::with_resources([(SHAPES_URL, SHAPES)]);
    h.load_config(|c| {
        c.shapes.insert(SHAPES_URL.into());
    });
    h
}

const DOC: &str =
    "@prefix ex: <http://ex.test/>.\n<p> a ex:Person ; ex:name \"Bob\" ; ex:status ex:Active .";

fn shape_hover(h: &mut LspHarness, src: &str, line: u32, col: u32) -> String {
    let file = h.open_file_and_drain("file:///data.ttl", "turtle", src);
    h.hover(&file, line, col)
        .into_iter()
        .filter(|s| s.starts_with("**Shape**"))
        .collect::<Vec<_>>()
        .join("\n---\n")
}

#[test_log::test]
fn hover_shows_name_description_label_comment_and_constraints() {
    let mut h = harness();
    // `ex:name` starts at column 18 of line 1.
    let hover = shape_hover(&mut h, DOC, 1, 19);

    assert!(hover.contains("**Shape** `ex:PersonShape`"), "{hover}");
    assert!(hover.contains("**Name:** name"), "{hover}");
    assert!(hover.contains("**Description:** The full name."), "{hover}");
    assert!(hover.contains("**Label:** Name label"), "{hover}");
    assert!(
        hover.contains("**Comment:** A comment over two lines."),
        "{hover}"
    );
    assert!(
        hover.contains("**Constraints:** required · at most 1 · datatype `xsd:string`"),
        "{hover}"
    );
    // Dutch texts are not shown, and shapes for other classes do not apply.
    assert!(
        !hover.contains("naam") && !hover.contains("volledige"),
        "{hover}"
    );
    assert!(!hover.contains("thing name"), "{hover}");
}

#[test_log::test]
fn hover_on_a_named_property_shape() {
    let mut h = harness();
    // `ex:status` starts at column 34 of line 1.
    let hover = shape_hover(&mut h, DOC, 1, 35);
    assert!(
        hover.contains("· property shape `ex:StatusProperty`"),
        "{hover}"
    );
    assert!(
        hover.contains("one of `ex:Active`, `ex:Inactive` · severity warning"),
        "{hover}"
    );
}

#[test_log::test]
fn no_shape_hover_when_no_shape_applies() {
    let mut h = harness();
    let src = "@prefix ex: <http://ex.test/>.\n<x> a ex:Other ; ex:name \"Bob\" .";
    assert_eq!(shape_hover(&mut h, src, 1, 18), "");
}

#[test_log::test]
fn completion_offers_and_documents_shape_predicates() {
    let mut h = harness();
    let src = "@prefix ex: <http://ex.test/>.\n<p> a ex:Person ;\n  ";
    let file = h.open_file_and_drain("file:///data.ttl", "turtle", src);
    let items = h.completions(&file, 2, 2);

    let name = items
        .iter()
        .find(|c| c.label == "ex:name")
        .expect("ex:name is offered from the shape");
    let docs = name._documentation.as_deref().unwrap();
    assert!(docs.contains("**Description:** The full name."), "{docs}");
    assert_eq!(
        name._label_details
            .as_ref()
            .and_then(|d| d.description.as_deref()),
        Some("required by shape")
    );
    assert!(items.iter().any(|c| c.label == "ex:status"));

    // Templates use the preferred-language `sh:name` as placeholder.
    let template = items
        .iter()
        .find(|c| c.label == "shape template: required properties")
        .unwrap();
    assert!(
        template.edits[0].new_text.contains("${1:name}"),
        "{}",
        template.edits[0].new_text
    );
}

#[test_log::test]
fn shape_docs_are_added_to_ontology_property_completions() {
    let mut h = harness();
    h.open_linked_file(
        "file:///onto.ttl",
        "turtle",
        "@prefix ex: <http://ex.test/>.\n\
         @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.\n\
         @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.\n\
         ex:name a rdf:Property ; rdfs:comment \"From the ontology.\" .",
    );
    h.drain_tasks();
    let src = "@prefix ex: <http://ex.test/>.\n<p> a ex:Person ;\n  ex:";
    let file = h.open_file_and_drain("file:///data.ttl", "turtle", src);
    let items = h.completions(&file, 2, 2);

    let names: Vec<_> = items.iter().filter(|c| c.label == "ex:name").collect();
    assert_eq!(names.len(), 1, "documented once, not duplicated");
    let docs = names[0]._documentation.as_deref().unwrap();
    assert!(docs.contains("From the ontology."), "{docs}");
    assert!(docs.contains("**Description:** The full name."), "{docs}");
}

#[test_log::test]
fn ontology_hover_uses_the_preferred_language() {
    let mut h = LspHarness::new();
    h.open_linked_file(
        "file:///onto.ttl",
        "turtle",
        "@prefix ex: <http://ex.test/>.\n\
         @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.\n\
         @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.\n\
         ex:name a rdf:Property ;\n\
           rdfs:label \"naam\"@nl , \"name\"@en ;\n\
           rdfs:comment \"De naam.\"@nl , \"The name.\"@en .",
    );
    h.drain_tasks();
    let file = h.open_file_and_drain(
        "file:///data.ttl",
        "turtle",
        "@prefix ex: <http://ex.test/>.\n<p> ex:name \"Bob\" .",
    );
    let hover = h.hover(&file, 1, 5).join("\n");
    assert!(
        hover.contains("## name") && hover.contains("The name."),
        "{hover}"
    );
    assert!(
        !hover.contains("naam") && !hover.contains("De naam"),
        "{hover}"
    );
}

#[test_log::test]
fn validation_message_uses_the_preferred_language() {
    let mut h = harness();
    h.open_file_and_drain(
        "file:///data.ttl",
        "turtle",
        "@prefix ex: <http://ex.test/>.\n<p> a ex:Person ; ex:name \"Bob\" ; ex:status ex:Active .",
    );
    let diags = h.run_diagnostics();
    let age: Vec<_> = diags
        .iter()
        .filter(|(_, d)| d.message.contains("ex:age"))
        .map(|(_, d)| d.message.clone())
        .collect();
    assert!(age.iter().any(|m| m.contains("Age is missing.")), "{age:?}");
    assert!(age.iter().all(|m| !m.contains("Leeftijd")), "{age:?}");
}
