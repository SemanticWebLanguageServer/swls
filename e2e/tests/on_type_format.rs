//! E2E tests for on-type formatting: auto-inserting a prefix declaration when
//! the `:` of a known-but-undeclared prefixed name is typed.

use swls_e2e_tests::LspHarness;

#[test_log::test]
fn typing_colon_after_known_undeclared_prefix_inserts_declaration() {
    let mut h = LspHarness::new();
    // `foaf` is a bundled LOV prefix but is not declared in this document.
    // Simulate the user having just typed the `:` of `foaf:`.
    let src = "<> foaf:";
    let file = h.open_file("file:///otf_foaf.ttl", "turtle", src);

    // "<> foaf:" — col 8 is just after the `:`.
    let edits = h
        .on_type_format(&file, 0, 8)
        .expect("typing ':' after a known prefix should insert a declaration");

    assert_eq!(edits.len(), 1, "expected a single declaration edit, got {edits:?}");
    assert_eq!(
        edits[0].new_text, "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n",
        "should insert the foaf prefix declaration"
    );
    // Inserted at the very top of the document.
    assert_eq!(edits[0].range.start.line, 0);
    assert_eq!(edits[0].range.start.character, 0);
}

#[test_log::test]
fn typing_colon_for_already_declared_prefix_does_nothing() {
    let mut h = LspHarness::new();
    let src = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n<> foaf:";
    let file = h.open_file("file:///otf_declared.ttl", "turtle", src);

    // Line 1: "<> foaf:" — col 8 just after the `:`.
    let edits = h.on_type_format(&file, 1, 8);
    assert!(
        edits.is_none(),
        "no declaration should be inserted when the prefix already exists: {edits:?}"
    );
}

#[test_log::test]
fn typing_colon_for_unknown_prefix_does_nothing() {
    let mut h = LspHarness::new();
    // `zzznope` is not a known prefix anywhere.
    let src = "<> zzznope:";
    let file = h.open_file("file:///otf_unknown.ttl", "turtle", src);

    let edits = h.on_type_format(&file, 0, 11);
    assert!(
        edits.is_none(),
        "unknown prefixes should not produce a declaration: {edits:?}"
    );
}

#[test_log::test]
fn typing_colon_inside_prefix_declaration_does_nothing() {
    let mut h = LspHarness::new();
    // The `:` here is part of the declaration being written — must not insert
    // a second `@prefix foaf: …` at the top.
    let src = "@prefix foaf:";
    let file = h.open_file("file:///otf_decl.ttl", "turtle", src);

    // col 13 is just after the `:` in "@prefix foaf:".
    let edits = h.on_type_format(&file, 0, 13);
    assert!(
        edits.is_none(),
        "typing ':' inside a @prefix declaration must not insert another: {edits:?}"
    );
}

#[test_log::test]
fn typing_colon_respects_sparql_prefix_format() {
    let mut h = LspHarness::new();
    // SPARQL uses `PREFIX name: <url>` (no leading `@`, no trailing `.`).
    let src = "SELECT * WHERE { ?s foaf: ?o }";
    let file = h.open_file("file:///otf.sq", "sparql", src);

    // "SELECT * WHERE { ?s foaf: ?o }" — col 25 just after `:`.
    let col = src.find("foaf:").unwrap() as u32 + 5;
    let edits = h
        .on_type_format(&file, 0, col)
        .expect("SPARQL prefix should be auto-declared");

    assert_eq!(edits.len(), 1);
    assert_eq!(
        edits[0].new_text, "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n",
        "SPARQL declaration format expected"
    );
}

// ─── JSON-LD: splice into @context ───────────────────────────────────────────

#[test_log::test]
fn jsonld_typing_colon_in_string_adds_prefix_to_context() {
    let mut h = LspHarness::new();
    // An existing @context without foaf; the user is typing `foaf:` inside a
    // term string in the body.
    let src = r#"{"@context": {"ex": "http://example.org/"}, "foaf:": "x"}"#;
    let file = h.open_file("file:///otf.jsonld", "jsonld", src);
    h.drain_tasks();

    // Position just after the `:` of the `"foaf:"` body key.
    let col = src.rfind("foaf:").unwrap() as u32 + 5;
    let edits = h
        .on_type_format(&file, 0, col)
        .expect("typing ':' inside a JSON string should splice foaf into @context");

    assert_eq!(edits.len(), 1);
    assert!(
        edits[0].new_text.contains("foaf")
            && edits[0].new_text.contains("http://xmlns.com/foaf/0.1/"),
        "edit should add the foaf prefix to @context, got {:?}",
        edits[0].new_text
    );
}

#[test_log::test]
fn jsonld_typing_colon_for_declared_prefix_does_nothing() {
    let mut h = LspHarness::new();
    let src =
        r#"{"@context": {"foaf": "http://xmlns.com/foaf/0.1/"}, "foaf:knows": "x"}"#;
    let file = h.open_file("file:///otf_decl.jsonld", "jsonld", src);
    h.drain_tasks();

    let col = src.rfind("foaf:").unwrap() as u32 + 5;
    let edits = h.on_type_format(&file, 0, col);
    assert!(
        edits.is_none(),
        "foaf already in @context — nothing to add: {edits:?}"
    );
}

#[test_log::test]
fn jsonld_typing_colon_with_no_context_inserts_one() {
    let mut h = LspHarness::new();
    let src = r#"{"foaf:": "x"}"#;
    let file = h.open_file("file:///otf_noctx.jsonld", "jsonld", src);
    h.drain_tasks();

    let col = src.find("foaf:").unwrap() as u32 + 5;
    let edits = h
        .on_type_format(&file, 0, col)
        .expect("a new @context should be inserted");

    assert_eq!(edits.len(), 1);
    assert!(
        edits[0].new_text.contains("@context") && edits[0].new_text.contains("foaf"),
        "edit should insert an @context with foaf, got {:?}",
        edits[0].new_text
    );
}
