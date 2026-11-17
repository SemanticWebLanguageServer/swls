//! Document identity for ontologies and shapes (issue #30).
//!
//! Several configured ontologies / shapes can describe the *same* namespace (e.g. the
//! RML modules all use `http://w3id.org/rml/`). Each must stay a distinct document:
//! all of them contribute to completion, validation and goto-definition.

use swls_e2e_tests::LspHarness;

const CORE_URL: &str = "https://example.test/rml-core/ontology.ttl";
const LV_URL: &str = "https://example.test/rml-lv/ontology.ttl";

const CORE: &str = "@prefix rml: <http://w3id.org/rml/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix vann: <http://purl.org/vocab/vann/> .
<http://w3id.org/rml/core> a owl:Ontology ;
  vann:preferredNamespacePrefix \"rml\" ;
  vann:preferredNamespaceUri \"http://w3id.org/rml/\" .
rml:TriplesMap a owl:Class .
";

const LV: &str = "@prefix rml: <http://w3id.org/rml/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix vann: <http://purl.org/vocab/vann/> .
<http://w3id.org/rml/lv> a owl:Ontology ;
  vann:preferredNamespacePrefix \"rml\" ;
  vann:preferredNamespaceUri \"http://w3id.org/rml/\" .
rml:LogicalView a owl:Class .
";

const MAPPING: &str = "@prefix rml: <http://w3id.org/rml/>.\n<> a rml:";

fn harness() -> LspHarness {
    LspHarness::with_resources([(CORE_URL, CORE), (LV_URL, LV)])
}

fn configure_both(h: &mut LspHarness) {
    h.load_config(|c| {
        c.ontologies.insert(CORE_URL.into());
        c.ontologies.insert(LV_URL.into());
    });
}

#[test_log::test]
fn classes_from_all_ontologies_sharing_a_namespace_are_completed() {
    let mut h = harness();
    configure_both(&mut h);

    let file = h.open_file_and_drain("file:///mapping.ttl", "turtle", MAPPING);

    let completions = h.completions(&file, 1, 5);
    h.assert_completions(&completions)
        .contains_label("rml:TriplesMap")
        .contains_label("rml:LogicalView");
}

#[test_log::test]
fn ontologies_configured_after_the_document_opened_are_still_loaded() {
    let mut h = harness();
    let file = h.open_file_and_drain("file:///mapping.ttl", "turtle", MAPPING);

    configure_both(&mut h);

    let completions = h.completions(&file, 1, 5);
    h.assert_completions(&completions)
        .contains_label("rml:TriplesMap")
        .contains_label("rml:LogicalView");
}

#[test_log::test]
fn goto_prefix_lists_every_configured_ontology() {
    let mut h = harness();
    configure_both(&mut h);
    let file = h.open_file_and_drain("file:///mapping.ttl", "turtle", MAPPING);

    // Cursor on `rml` in the @prefix declaration.
    let targets: Vec<String> = h
        .definition(&file, 0, 9)
        .into_iter()
        .map(|l| l.uri.to_string())
        .collect();

    assert_eq!(targets.len(), 2, "one target per ontology, got {targets:?}");
    assert!(targets
        .iter()
        .any(|t| t.ends_with("example.test/rml-core/ontology.ttl")));
    assert!(targets
        .iter()
        .any(|t| t.ends_with("example.test/rml-lv/ontology.ttl")));
}

const SH_PREFIXES: &str = "@prefix rml: <http://w3id.org/rml/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
";

#[test_log::test]
fn shapes_from_all_configured_shape_files_are_validated() {
    let core_shapes = format!(
        "{SH_PREFIXES}rml:CoreShape a sh:NodeShape ; sh:targetClass rml:TriplesMap ;
  sh:property [ sh:path rml:logicalSource ; sh:minCount 1 ] ."
    );
    let lv_shapes = format!(
        "{SH_PREFIXES}rml:LvShape a sh:NodeShape ; sh:targetClass rml:LogicalView ;
  sh:property [ sh:path rml:viewOn ; sh:minCount 1 ] ."
    );
    let mut h = LspHarness::with_resources([
        (
            "https://example.test/rml-core/shapes.ttl",
            core_shapes.as_str(),
        ),
        ("https://example.test/rml-lv/shapes.ttl", lv_shapes.as_str()),
    ]);
    h.load_config(|c| {
        c.shapes
            .insert("https://example.test/rml-core/shapes.ttl".into());
        c.shapes
            .insert("https://example.test/rml-lv/shapes.ttl".into());
    });

    let src =
        "@prefix rml: <http://w3id.org/rml/>.\n<a> a rml:TriplesMap .\n<b> a rml:LogicalView .";
    h.open_file_and_drain("file:///mapping.ttl", "turtle", src);

    let diags = h.run_diagnostics();
    let shacl: Vec<_> = diags
        .iter()
        .filter(|(_, d)| d.message.contains("minCount") || d.message.contains("MinCount"))
        .map(|(_, d)| d.range.start.line)
        .collect();
    assert!(
        shacl.contains(&1),
        "core shape should flag line 1: {diags:#?}"
    );
    assert!(
        shacl.contains(&2),
        "lv shape should flag line 2: {diags:#?}"
    );
}

const FICT_V1: &str = "@prefix fict: <http://fictional.test/onto#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
fict:Old a owl:Class .";
const FICT_V2: &str = "@prefix fict: <http://fictional.test/onto#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
fict:New a owl:Class .";

#[test_log::test]
fn changed_background_document_replaces_its_triples_in_the_store() {
    let mut h = LspHarness::new();
    let onto = h.open_linked_file("file:///fict-onto.ttl", "turtle", FICT_V1);
    h.drain_tasks();
    h.update_file(&onto, FICT_V2);
    h.drain_tasks();

    let src = "@prefix fict: <http://fictional.test/onto#>.\n<> a fict:";
    let file = h.open_file_and_drain("file:///class_complete.ttl", "turtle", src);

    let completions = h.completions(&file, 1, 5);
    h.assert_completions(&completions)
        .contains_label("fict:New")
        .does_not_contain_label("fict:Old");
}

#[test_log::test]
fn background_load_does_not_overwrite_an_open_buffer() {
    let dir = std::env::temp_dir().join(format!("swls-e2e-open-buffer-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("imported.ttl");
    std::fs::write(&path, FICT_V1).unwrap();
    let url = swls_core::lsp_types::Url::from_file_path(&path).unwrap();

    let mut h = LspHarness::new();
    // The user is editing the file; their unsaved buffer differs from disk.
    let edited = h.open_file_and_drain(url.as_str(), "turtle", FICT_V2);

    // Another document imports it, which makes the server read it from disk.
    let importer = format!(
        "@prefix owl: <http://www.w3.org/2002/07/owl#>.\n<> owl:imports <{}> .",
        url
    );
    h.open_file_and_drain("file:///importer.ttl", "turtle", &importer);

    assert_eq!(
        h.source(&edited),
        FICT_V2,
        "the editor buffer must win over disk"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
