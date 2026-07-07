//! E2E tests for hover / goto-definition on prefix declarations and JSON-LD
//! `@context` terms.
//!
//! Before this feature the cursor on a `@prefix` line (or inside `@context`) was
//! not recognised as a prefix, so hover either showed nothing or leaked
//! information about an unrelated nearby triple, and goto-definition did nothing.

use swls_e2e_tests::LspHarness;

#[test_log::test]
fn turtle_hover_on_prefix_declaration_shows_mapping() {
    let mut h = LspHarness::new();
    let src = "@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n\
               <http://a.example/s> foaf:name \"Alice\" .";
    let file = h.open_file("file:///hover_prefix.ttl", "turtle", src);

    // Cursor on `foaf` in the `@prefix` line (chars 8..12).
    let hover = h.hover(&file, 0, 10);

    assert!(
        hover.iter().any(|s| s.contains("foaf")
            && s.contains("http://xmlns.com/foaf/0.1/")
            && s.contains("Prefix")),
        "expected a prefix→namespace hover, got: {hover:?}"
    );
}

#[test_log::test]
fn turtle_hover_on_prefix_does_not_leak_triple_hover() {
    let mut h = LspHarness::new();
    // A property whose IRI would produce ontology hover if the cursor were on the
    // triple. Hovering the *prefix* line must not surface that.
    let src = "@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n\
               <http://a.example/s> foaf:name \"Alice\" .";
    let file = h.open_file("file:///hover_prefix2.ttl", "turtle", src);

    let hover = h.hover(&file, 0, 10);

    // Every hover string is the prefix mapping — none is a "## <property>" doc block.
    assert!(
        hover.iter().all(|s| s.contains("Prefix")),
        "prefix hover leaked non-prefix content: {hover:?}"
    );
}

#[test_log::test]
fn turtle_goto_on_prefix_returns_ontology_location() {
    let mut h = LspHarness::new();
    let src = "@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n\
               <http://a.example/s> foaf:name \"Alice\" .";
    let file = h.open_file("file:///goto_prefix.ttl", "turtle", src);

    let locs = h.definition(&file, 0, 10);

    assert!(
        !locs.is_empty(),
        "expected a goto-definition target for the prefix, got none"
    );
}

#[test_log::test]
fn jsonld_hover_in_context_shows_mapping() {
    let mut h = LspHarness::new();
    let src = "{\n  \"@context\": { \"foaf\": \"http://xmlns.com/foaf/0.1/\" },\n  \"foaf:name\": \"Alice\"\n}";
    let file = h.open_file("file:///hover_ctx.jsonld", "json-ld", src);
    h.drain_tasks();

    // Cursor on the `foaf` context key (line 1, inside `"foaf"`).
    let hover = h.hover(&file, 1, 19);

    assert!(
        hover
            .iter()
            .any(|s| s.contains("foaf") && s.contains("http://xmlns.com/foaf/0.1/")),
        "expected a JSON-LD @context prefix hover, got: {hover:?}"
    );
}

#[test_log::test]
fn jsonld_goto_on_context_namespace_returns_location() {
    let mut h = LspHarness::new();
    let src = "{\n  \"@context\": { \"foaf\": \"http://xmlns.com/foaf/0.1/\" },\n  \"foaf:name\": \"Alice\"\n}";
    let file = h.open_file("file:///goto_ctx.jsonld", "json-ld", src);
    h.drain_tasks();

    let locs = h.definition(&file, 1, 19);

    // Exactly the ontology target from `goto_prefix` — `goto_cjs` steps aside for
    // a real namespace prefix, so we don't get a duplicate/unrelated location.
    assert_eq!(
        locs.len(),
        1,
        "expected a single ontology goto target, got: {locs:?}"
    );
}
