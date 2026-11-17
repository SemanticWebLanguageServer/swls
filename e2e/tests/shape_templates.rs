//! Completion templates from SHACL shapes: "required properties" and "all properties" for
//! the subject at the cursor.

use swls_core::{feature::completion::SimpleCompletion, prelude::Disabled};
use swls_e2e_tests::LspHarness;

const SHAPES_URL: &str = "https://example.test/shapes.ttl";
const SHAPES: &str = r#"@prefix ex: <http://ex.test/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:PersonShape a sh:NodeShape ;
  sh:targetClass ex:Person ;
  sh:property [ sh:path ex:age ; sh:datatype xsd:integer ] ;
  sh:property [ sh:path ex:status ; sh:minCount 1 ; sh:in ( ex:Active ex:Inactive ) ] ;
  sh:property [ sh:path ex:name ; sh:minCount 1 ; sh:datatype xsd:string ; sh:order 1 ;
                sh:description "Full name." ] ;
  sh:property [ sh:path ex:address ; sh:minCount 1 ; sh:node ex:AddressShape ] ;
  sh:property [ sh:path ex:homepage ; sh:nodeKind sh:IRI ] ;
  sh:property [ sh:path [ sh:inversePath ex:knows ] ; sh:minCount 1 ] ;
  sh:and ( ex:AgentShape ) .

ex:AgentShape a sh:NodeShape ;
  sh:property [ sh:path ex:id ; sh:minCount 1 ] .

ex:AddressShape a sh:NodeShape ;
  sh:property [ sh:path ex:city ; sh:minCount 1 ; sh:datatype xsd:string ] ;
  sh:property [ sh:path ex:zip ] .

ex:MapShape a sh:NodeShape ;
  sh:targetClass ex:Map ;
  sh:property [ sh:path ex:part ; sh:minCount 1 ] ;
  sh:and (
    [ sh:path [ sh:alternativePath ( ex:subject ex:subjectMap ) ] ; sh:minCount 1 ]
    [ sh:path ex:subject ; sh:nodeKind sh:IRI ]
  ) .

ex:PartShape a sh:NodeShape ;
  sh:targetObjectsOf ex:part ;
  sh:property [ sh:path ex:kind ; sh:minCount 1 ] ;
  sh:property [ sh:path ex:size ; sh:minCount 1 ] .

ex:StepShape a sh:NodeShape ;
  sh:targetObjectsOf ex:step ;
  sh:property [ sh:path ex:action ; sh:minCount 1 ] ;
  sh:property [ sh:path ex:note ; sh:nodeKind sh:Literal ] .
"#;

const REQUIRED: &str = "shape template: required properties";
const ALL: &str = "shape template: all properties";

fn harness() -> LspHarness {
    let mut h = LspHarness::with_resources([(SHAPES_URL, SHAPES)]);
    h.load_config(|c| {
        c.shapes.insert(SHAPES_URL.into());
    });
    h
}

fn templates(h: &mut LspHarness, src: &str, line: u32, col: u32) -> Vec<SimpleCompletion> {
    let file = h.open_file_and_drain("file:///data.ttl", "turtle", src);
    h.completions(&file, line, col)
        .into_iter()
        .filter(|c| c.label.starts_with("shape template"))
        .collect()
}

fn find<'a>(items: &'a [SimpleCompletion], label: &str) -> &'a SimpleCompletion {
    items
        .iter()
        .find(|c| c.label == label)
        .unwrap_or_else(|| panic!("no {label:?} in {:?}", labels(items)))
}

fn labels(items: &[SimpleCompletion]) -> Vec<&str> {
    items.iter().map(|c| c.label.as_str()).collect()
}

const PERSON: &str = "@prefix ex: <http://ex.test/>.\n<p> a ex:Person ;\n  ";

#[test_log::test]
fn required_template_lists_required_properties_with_typed_placeholders() {
    let mut h = harness();
    let items = templates(&mut h, PERSON, 2, 2);
    let required = find(&items, REQUIRED);

    assert_eq!(
        required.edits[0].new_text,
        "ex:name \"${1:name}\" ;\n  \
         ex:status ${2|ex:Active,ex:Inactive|} ;\n  \
         ex:address [ ex:city \"${3:city}\" ] ;\n  \
         ex:id ${4:id}$0"
    );
    assert!(required._is_snippet);
    let docs = required._documentation.as_deref().unwrap();
    assert!(docs.contains("`ex:name` (required): Full name."), "{docs}");
}

#[test_log::test]
fn all_template_adds_optional_properties() {
    let mut h = harness();
    let items = templates(&mut h, PERSON, 2, 2);
    let all = &find(&items, ALL).edits[0].new_text;

    assert!(all.starts_with("ex:name "), "{all}");
    assert!(all.contains("ex:age ${5:age} ;"), "{all}");
    assert!(all.contains("ex:homepage <${6:homepage}>$0"), "{all}");
    // Only the required properties of a nested shape are expanded.
    assert!(!all.contains("ex:zip"), "{all}");
    // Inverse paths cannot be written as `subject predicate value`.
    assert!(!all.contains("knows"), "{all}");
}

#[test_log::test]
fn properties_the_subject_already_has_are_left_out() {
    let mut h = harness();
    let src = "@prefix ex: <http://ex.test/>.\n<p> a ex:Person ;\n  ex:name \"Bob\" ;\n  ";
    let items = templates(&mut h, src, 3, 2);
    let text = &find(&items, REQUIRED).edits[0].new_text;
    assert!(text.starts_with("ex:status "), "{text}");
    assert!(!text.contains("ex:name"), "{text}");
}

#[test_log::test]
fn blank_node_targeted_by_targets_objects_of() {
    let mut h = harness();
    let src = "@prefix ex: <http://ex.test/>.\n<p> ex:step [\n    \n] .";
    let items = templates(&mut h, src, 2, 4);
    let required = find(&items, REQUIRED);
    assert_eq!(required.edits[0].new_text, "ex:action ${1:action}$0");
    // Inserted at the cursor: the closing `]` on the next line stays.
    assert_eq!(required.edits[0].range.start, required.edits[0].range.end);
    assert_eq!(required.edits[0].range.start.line, 2);

    let all = &find(&items, ALL).edits[0].new_text;
    assert!(all.ends_with("ex:note \"${2:note}\"$0"), "{all}");
}

#[test_log::test]
fn template_after_a_semicolon_on_the_same_line() {
    let mut h = harness();
    let src = "@prefix ex: <http://ex.test/>.\n<p> a ex:Person ;";
    let items = templates(&mut h, src, 1, 17);
    let edit = &find(&items, REQUIRED).edits[0];
    assert!(
        edit.new_text.starts_with(" ex:name "),
        "{:?}",
        edit.new_text
    );
    assert_eq!(edit.range.start.character, 17);
    // Continuation lines are indented under the subject.
    assert!(
        edit.new_text.contains(" ;\n    ex:status"),
        "{:?}",
        edit.new_text
    );
}

#[test_log::test]
fn template_replaces_a_partially_typed_predicate() {
    let mut h = harness();
    let src = "@prefix ex: <http://ex.test/>.\n<p> a ex:Person ;\n  ex:";
    let items = templates(&mut h, src, 2, 2);
    let required = find(&items, REQUIRED);
    assert_eq!(required.edits[0].range.start.character, 2);
    assert_eq!(required.edits[0].range.end.character, 5);
    // Clients filter on what was typed; keep the template visible.
    assert!(required._filter_text.as_deref().unwrap().starts_with("ex:"));
}

#[test_log::test]
fn shapes_of_superclasses_apply() {
    let mut h = harness();
    h.open_linked_file(
        "file:///onto.ttl",
        "turtle",
        "@prefix ex: <http://ex.test/>.\n\
         @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.\n\
         @prefix owl: <http://www.w3.org/2002/07/owl#>.\n\
         ex:Person a owl:Class .\nex:Manager a owl:Class ; rdfs:subClassOf ex:Person .",
    );
    h.drain_tasks();
    let src = "@prefix ex: <http://ex.test/>.\n<m> a ex:Manager ;\n  ";
    let items = templates(&mut h, src, 2, 2);
    assert!(find(&items, REQUIRED).edits[0]
        .new_text
        .starts_with("ex:name "));
}

#[test_log::test]
fn no_template_without_an_applicable_shape() {
    let mut h = harness();
    let src = "@prefix ex: <http://ex.test/>.\n<x> a ex:Unrelated ;\n  ";
    assert!(templates(&mut h, src, 2, 2).is_empty());
}

#[test_log::test]
fn templates_can_be_disabled() {
    let mut h = harness();
    h.set_config(|c| {
        c.disabled.insert(Disabled::CompletionShapeTemplate);
    });
    assert!(templates(&mut h, PERSON, 2, 2).is_empty());
}

const MAP: &str = "@prefix ex: <http://ex.test/>.\n<m> a ex:Map ;\n  ";

#[test_log::test]
fn alternative_paths_nested_targets_and_property_shapes_in_and() {
    let mut h = harness();
    let items = templates(&mut h, MAP, 2, 2);
    assert_eq!(
        find(&items, REQUIRED).edits[0].new_text,
        // `ex:part` values are described by a shape targeting objects of `ex:part`;
        // `sh:and` lists a property shape whose path is a choice of predicates, and
        // the optional `ex:subject` declaration gives the choice its IRI value.
        "ex:part [\n      ex:kind ${1:kind} ;\n      ex:size ${2:size}\n  ] ;\n  \
         ${3|ex:subject,ex:subjectMap|} <${4:subject}>$0"
    );
    // The optional `ex:subject` is folded into the choice, so "all" adds nothing.
    assert!(!labels(&items).contains(&ALL), "{:?}", labels(&items));
}

#[test_log::test]
fn alternative_is_satisfied_by_any_of_its_paths() {
    let mut h = harness();
    let src = "@prefix ex: <http://ex.test/>.\n<m> a ex:Map ;\n  ex:subjectMap [ ] ;\n  ";
    let items = templates(&mut h, src, 3, 2);
    let text = &find(&items, REQUIRED).edits[0].new_text;
    assert!(!text.contains("subject"), "{text}");
}
