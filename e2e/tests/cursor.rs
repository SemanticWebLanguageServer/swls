//! Requests use plain LSP cursor positions (between characters), the same in the server and
//! in tests.

use swls_e2e_tests::LspHarness;

const ONTO: &str = "@prefix ex: <http://ex.test/>.\n\
    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.\n\
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.\n\
    @prefix owl: <http://www.w3.org/2002/07/owl#>.\n\
    ex:Car a owl:Class .\n\
    ex:size a rdf:Property ; rdfs:comment \"The size.\" .\n\
    ex:wheels a rdf:Property ; rdfs:domain ex:Car .";

fn harness() -> LspHarness {
    let mut h = LspHarness::new();
    h.open_linked_file("file:///onto.ttl", "turtle", ONTO);
    h.drain_tasks();
    h
}

#[test_log::test]
fn completing_a_word_right_before_a_semicolon() {
    let mut h = harness();
    // Cursor right after `ex:si`, directly followed by `;`.
    let src = "@prefix ex: <http://ex.test/>.\n<x> ex:si; ex:size 1 .";
    let file = h.open_file_and_drain("file:///data.ttl", "turtle", src);
    let items = h.completions(&file, 1, 9);
    let size = items
        .iter()
        .find(|c| c.label == "ex:size")
        .expect("ex:size is offered");
    // It replaces the typed word, not the `;`.
    assert_eq!(size.edits[0].range.start.character, 4);
    assert_eq!(size.edits[0].range.end.character, 9);
}

#[test_log::test]
fn hover_on_the_first_character_of_a_term() {
    let mut h = harness();
    let src = "@prefix ex: <http://ex.test/>.\n<x> ex:size 1 .";
    let file = h.open_file_and_drain("file:///data.ttl", "turtle", src);
    // Column 4 is the `e` of `ex:size`; the character before it is a space.
    let hover = h.hover(&file, 1, 4).join("\n");
    assert!(hover.contains("The size."), "{hover}");
}

#[test_log::test]
fn property_completion_inside_a_blank_node() {
    let mut h = harness();
    let src = "@prefix ex: <http://ex.test/>.\n<x> ex:part [ a ex:Car ;\n    \n] .";
    let file = h.open_file_and_drain("file:///data.ttl", "turtle", src);
    let items = h.completions(&file, 2, 4);
    let wheels = items
        .iter()
        .find(|c| c.label == "ex:wheels")
        .expect("ex:wheels is offered for the blank node");
    // Its domain matches the blank node's type.
    assert_eq!(
        wheels.kind,
        swls_core::lsp_types::CompletionItemKind::FIELD,
        "{:?}",
        wheels.kind
    );
    // Inserted at the cursor, the `]` on the next line stays.
    assert_eq!(wheels.edits[0].range.start, wheels.edits[0].range.end);
}
