//! Shared data (ontologies, the class hierarchy, shapes) usually arrives after the documents
//! that use it were opened. Open documents must pick it up without being edited.

use swls_core::lsp_types::DiagnosticSeverity;
use swls_e2e_tests::LspHarness;

const ONTO: &str = "@prefix ex: <http://ex.test/>.\n\
    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.\n\
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.\n\
    @prefix owl: <http://www.w3.org/2002/07/owl#>.\n\
    ex:Car a owl:Class .\n\
    ex:wheels a rdf:Property ; rdfs:domain ex:Car .";

const DOC: &str = "@prefix ex: <http://ex.test/>.\n<car> ex:wheels 4 .";

#[test_log::test]
fn types_are_inferred_once_the_ontology_arrives() {
    let mut h = LspHarness::new();
    let file = h.open_file_and_drain("file:///data.ttl", "turtle", DOC);
    assert!(
        !h.hover(&file, 1, 1).join("\n").contains("ex:Car"),
        "no type is known before the ontology"
    );

    // The ontology loads later; the document is not edited.
    h.open_linked_file("file:///onto.ttl", "turtle", ONTO);
    h.drain_tasks();

    let hover = h.hover(&file, 1, 1).join("\n");
    assert!(hover.contains("ex:Car"), "{hover}");
}

#[test_log::test]
fn closed_namespace_warning_clears_once_the_ontology_arrives() {
    let mut h = LspHarness::new();
    h.set_config(|c| {
        c.closed_namespaces.insert("http://ex.test/".to_string());
    });
    let file = h.open_file_and_drain("file:///data.ttl", "turtle", DOC);
    let warns = |h: &mut LspHarness| {
        h.run_diagnostics()
            .into_iter()
            .filter(|(u, d)| {
                u.as_str() == file.url
                    && d.severity == Some(DiagnosticSeverity::WARNING)
                    && d.message.contains("not defined in the ontology")
            })
            .count()
    };
    assert_eq!(warns(&mut h), 1, "ex:wheels is unknown at first");

    h.open_linked_file("file:///onto.ttl", "turtle", ONTO);
    h.drain_tasks();
    assert_eq!(warns(&mut h), 0, "ex:wheels is defined by the ontology");
}

#[test_log::test]
fn unrelated_edits_do_not_revalidate_other_documents() {
    let mut h = LspHarness::new();
    h.set_config(|c| {
        c.closed_namespaces.insert("http://ex.test/".to_string());
    });
    let a = h.open_file_and_drain("file:///a.ttl", "turtle", DOC);
    let b = h.open_file_and_drain("file:///b.ttl", "turtle", DOC);
    h.run_diagnostics();

    // Editing `b` republishes diagnostics for `b` only.
    h.update_file(&b, &format!("{DOC}\n"));
    let published: Vec<_> = h.run_diagnostics().into_iter().map(|(u, _)| u).collect();
    assert!(published.iter().any(|u| u.as_str() == b.url));
    assert!(
        published.iter().all(|u| u.as_str() != a.url),
        "{published:?}"
    );
}
