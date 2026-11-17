//! `owl:imports`: imported documents are loaded (local and remote, transitively),
//! cached at readable locations, and reachable with goto-definition.

use swls_e2e_tests::LspHarness;

const PREFIXES: &str = "@prefix fict: <http://fictional.test/onto#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
";

fn onto(classes: &[&str], imports: &[&str]) -> String {
    let mut s = String::from(PREFIXES);
    for c in classes {
        s.push_str(&format!("fict:{c} a owl:Class .\n"));
    }
    for i in imports {
        s.push_str(&format!("<> owl:imports <{i}> .\n"));
    }
    s
}

fn importer(iri: &str) -> String {
    format!("{PREFIXES}<> owl:imports <{iri}> .\n<> a fict:")
}

/// Class completions offered on line 3 (`<> a fict:`) of [`importer`].
fn classes(h: &mut LspHarness, iri: &str) -> Vec<String> {
    let file = h.open_file_and_drain("file:///user.ttl", "turtle", &importer(iri));
    h.drain_tasks();
    h.completions(&file, 3, 5)
        .into_iter()
        .map(|c| c.label)
        .filter(|l| l.starts_with("fict:"))
        .collect()
}

#[test_log::test]
fn remote_import_is_fetched_and_cached() {
    let body = onto(&["Widget"], &[]);
    let mut h = LspHarness::with_resources([("https://example.test/onto", body.as_str())]);

    assert!(classes(&mut h, "https://example.test/onto").contains(&"fict:Widget".into()));

    let cache = h.cache_url("https://example.test/onto");
    assert!(
        cache.as_str().ends_with("/sources/example.test/onto.ttl"),
        "{cache}"
    );
    let on_disk = std::fs::read_to_string(cache.to_file_path().unwrap()).unwrap();
    assert_eq!(on_disk, body);
}

#[test_log::test]
fn imports_are_followed_transitively() {
    let a = onto(&["A"], &["https://example.test/b"]);
    let b = onto(&["B"], &["https://example.test/c"]);
    let c = onto(&["C"], &[]);
    let mut h = LspHarness::with_resources([
        ("https://example.test/a", a.as_str()),
        ("https://example.test/b", b.as_str()),
        ("https://example.test/c", c.as_str()),
    ]);

    let found = classes(&mut h, "https://example.test/a");
    for class in ["fict:A", "fict:B", "fict:C"] {
        assert!(
            found.contains(&class.into()),
            "{class} missing from {found:?}"
        );
    }
}

#[test_log::test]
fn import_cycles_terminate() {
    let a = onto(&["A"], &["https://example.test/b"]);
    let b = onto(&["B"], &["https://example.test/a"]);
    let mut h = LspHarness::with_resources([
        ("https://example.test/a", a.as_str()),
        ("https://example.test/b", b.as_str()),
    ]);

    let found = classes(&mut h, "https://example.test/a");
    assert!(found.contains(&"fict:A".into()) && found.contains(&"fict:B".into()));
}

#[test_log::test]
fn rdf_xml_import_is_skipped_and_not_cached() {
    let rdf_xml = "<?xml version=\"1.0\"?>\n<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"/>";
    let mut h = LspHarness::with_resources([("https://example.test/onto.owl", rdf_xml)]);

    classes(&mut h, "https://example.test/onto.owl");

    let cache = h.cache_url("https://example.test/onto.owl");
    assert!(
        h.handle(cache.as_str()).is_none(),
        "no document for {cache}"
    );
    assert!(!cache.to_file_path().unwrap().exists());
}

#[test_log::test]
fn local_import_keeps_its_own_url() {
    let dir = std::env::temp_dir().join(format!("swls-e2e-import-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("local.ttl");
    std::fs::write(&path, onto(&["Local"], &[])).unwrap();
    let url = swls_core::lsp_types::Url::from_file_path(&path).unwrap();

    let mut h = LspHarness::new();
    assert!(classes(&mut h, url.as_str()).contains(&"fict:Local".into()));
    assert!(h.handle(url.as_str()).is_some());
    let _ = std::fs::remove_dir_all(&dir);
}

#[test_log::test]
fn goto_definition_on_an_import_opens_the_imported_document() {
    let body = onto(&["Widget"], &[]);
    let mut h = LspHarness::with_resources([("https://example.test/onto", body.as_str())]);
    let file = h.open_file_and_drain(
        "file:///user.ttl",
        "turtle",
        &importer("https://example.test/onto"),
    );

    // Line 2 is `<> owl:imports <https://example.test/onto> .`, col 20 is inside the IRI.
    let targets: Vec<_> = h
        .definition(&file, 2, 20)
        .into_iter()
        .map(|l| l.uri)
        .collect();
    assert!(
        targets.contains(&h.cache_url("https://example.test/onto")),
        "{targets:?}"
    );
}

#[test_log::test]
fn shapes_in_an_imported_document_validate_the_importer() {
    let shapes = "@prefix fict: <http://fictional.test/onto#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
fict:WidgetShape a sh:NodeShape ; sh:targetClass fict:Widget ;
  sh:property [ sh:path fict:name ; sh:minCount 1 ] .";
    let mut h = LspHarness::with_resources([("https://example.test/shapes", shapes)]);

    let src =
        format!("{PREFIXES}<> owl:imports <https://example.test/shapes> .\n<w> a fict:Widget .");
    // The shapes arrive after the document was opened and validated; their
    // arrival alone must revalidate it.
    h.open_file_and_drain("file:///user.ttl", "turtle", &src);

    let diags = h.run_diagnostics();
    assert!(
        diags
            .iter()
            .any(|(_, d)| d.range.start.line == 3 && d.message.contains("fict:name")),
        "{diags:#?}"
    );
}
