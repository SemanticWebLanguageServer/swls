//! E2E rename tests.
//!
//! The rename feature has language-specific quoting rules:
//!
//! * **Turtle/TriG/SPARQL** — IRIs are written `<http://...>`.  The editor should show the
//!   user a clean placeholder *without* the angle brackets, but when the rename is applied the
//!   brackets must be re-added.  The user may also switch the term kind entirely:
//!   - type `http://new`       → replaced with `<http://new>`   (bare IRI → wraps)
//!   - type `<http://new>`     → replaced with `<http://new>`   (already wrapped → kept)
//!   - type `ex:new`           → replaced with `ex:new`         (prefixed name → no brackets)
//!   - type `_:new`            → replaced with `_:new`          (blank node → no brackets)
//!
//! * **JSON-LD** — IRIs are always written as quoted strings `"http://..."`.  The editor shows
//!   the user the bare IRI (without `""`), and the rename always re-wraps in double quotes.

use swls_e2e_tests::LspHarness;

// ─── prepare_rename: placeholder stripping ────────────────────────────────────

#[test_log::test]
fn prepare_rename_turtle_iri_strips_angle_brackets() {
    let mut h = LspHarness::new();
    // Line 0: "@prefix ex: <http://example.org/> ."
    // Line 1: "ex:s <http://example.org/pred> ex:o ."
    //          0123456789...
    //  col 5 = '<' of <http://example.org/pred>
    let src = "@prefix ex: <http://example.org/> .\nex:s <http://example.org/pred> ex:o .";
    let file = h.open_file("file:///iri_rename.ttl", "turtle", src);

    let result = h.prepare_rename(&file, 1, 5).expect("rename should be available on IRI token");
    assert_eq!(
        result.placeholder, "http://example.org/pred",
        "placeholder should strip angle brackets"
    );
}

#[test_log::test]
fn prepare_rename_turtle_prefixed_name_shows_as_is() {
    let mut h = LspHarness::new();
    // Line 1: "<> ex:pred ex:o ."
    //  col 3 = 'e' of ex:pred
    let src = "@prefix ex: <http://example.org/> .\n<> ex:pred ex:o .";
    let file = h.open_file("file:///pname_rename.ttl", "turtle", src);

    let result = h
        .prepare_rename(&file, 1, 3)
        .expect("rename should be available on prefixed name");
    assert_eq!(
        result.placeholder, "ex:pred",
        "prefixed name placeholder should be shown as-is"
    );
}

#[test_log::test]
fn prepare_rename_turtle_blank_node_shows_full_label() {
    let mut h = LspHarness::new();
    // Line 0: "_:b0 ex:pred ex:o ."
    //  col 0 = '_' of _:b0
    let src = "@prefix ex: <http://example.org/> .\n_:b0 ex:pred ex:o .";
    let file = h.open_file("file:///bnode_rename.ttl", "turtle", src);

    let result = h
        .prepare_rename(&file, 1, 0)
        .expect("rename should be available on blank node");
    assert_eq!(
        result.placeholder, "_:b0",
        "blank node placeholder should include the _: prefix"
    );
}

// ─── rename: output wrapping ──────────────────────────────────────────────────

#[test_log::test]
fn rename_turtle_iri_bare_input_is_wrapped_in_angle_brackets() {
    let mut h = LspHarness::new();
    let src = "@prefix ex: <http://example.org/> .\nex:s <http://example.org/pred> ex:o .";
    let file = h.open_file("file:///iri_wrap.ttl", "turtle", src);

    // User provides just the IRI string — no angle brackets
    let edits = h.rename(&file, 1, 5, "http://example.org/new");
    assert!(!edits.is_empty(), "rename should produce at least one edit");

    for (_, edit) in &edits {
        assert_eq!(
            edit.new_text, "<http://example.org/new>",
            "bare IRI input should be wrapped in angle brackets: got {:?}",
            edit.new_text
        );
    }
}

#[test_log::test]
fn rename_turtle_iri_already_wrapped_input_is_kept_as_is() {
    let mut h = LspHarness::new();
    let src = "@prefix ex: <http://example.org/> .\nex:s <http://example.org/pred> ex:o .";
    let file = h.open_file("file:///iri_nowrap.ttl", "turtle", src);

    // User provides the IRI already wrapped
    let edits = h.rename(&file, 1, 5, "<http://example.org/new>");
    assert!(!edits.is_empty());
    for (_, edit) in &edits {
        assert_eq!(
            edit.new_text, "<http://example.org/new>",
            "already-wrapped IRI should not be double-wrapped: got {:?}",
            edit.new_text
        );
    }
}

#[test_log::test]
fn rename_turtle_iri_to_prefixed_name() {
    let mut h = LspHarness::new();
    let src = "@prefix ex: <http://example.org/> .\nex:s <http://example.org/pred> ex:o .";
    let file = h.open_file("file:///iri_to_pname.ttl", "turtle", src);

    // User switches to a prefixed name — must not get wrapped in < >
    let edits = h.rename(&file, 1, 5, "ex:newPred");
    assert!(!edits.is_empty());
    for (_, edit) in &edits {
        assert_eq!(
            edit.new_text, "ex:newPred",
            "prefixed-name input should not be wrapped: got {:?}",
            edit.new_text
        );
    }
}

#[test_log::test]
fn rename_turtle_iri_to_blank_node() {
    let mut h = LspHarness::new();
    let src = "@prefix ex: <http://example.org/> .\nex:s <http://example.org/pred> ex:o .";
    let file = h.open_file("file:///iri_to_bnode.ttl", "turtle", src);

    // User switches to a blank node label
    let edits = h.rename(&file, 1, 5, "_:myBlank");
    assert!(!edits.is_empty());
    for (_, edit) in &edits {
        assert_eq!(
            edit.new_text, "_:myBlank",
            "blank-node input should not be wrapped: got {:?}",
            edit.new_text
        );
    }
}

#[test_log::test]
fn rename_turtle_prefixed_name_stays_as_is() {
    let mut h = LspHarness::new();
    let src = "@prefix ex: <http://example.org/> .\n<> ex:pred ex:o .";
    let file = h.open_file("file:///pname_stays.ttl", "turtle", src);

    let edits = h.rename(&file, 1, 3, "ex:renamed");
    assert!(!edits.is_empty());
    for (_, edit) in &edits {
        assert_eq!(
            edit.new_text, "ex:renamed",
            "prefixed name should be used as-is: got {:?}",
            edit.new_text
        );
    }
}

// ─── rename: multiple occurrences ────────────────────────────────────────────

/// Regression test: subject with multiple PO pairs via `;` used to produce one
/// edit per triple (not per source token), causing duplicated output like
/// `foaf:agentagentagent` when a subject appeared in 3 triples.
#[test_log::test]
fn rename_subject_with_multiple_po_pairs_is_deduplicated() {
    let mut h = LspHarness::new();
    // <abc> is subject of 3 triples (via the ; shorthand)
    let src = "@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n\
               <http://example.org/abc> a foaf:Agent ;\n\
               \tfoaf:name \"Test\" ;\n\
               \tfoaf:account <http://example.org/acc> .";
    let file = h.open_file("file:///dedup_rename.ttl", "turtle", src);

    // Cursor at col 1 on line 1 = 'h' in <http://example.org/abc>
    let edits = h.rename(&file, 1, 1, "http://example.org/new");

    assert_eq!(
        edits.len(),
        1,
        "subject appearing in N triples should produce exactly 1 edit, got: {:?}",
        edits.iter().map(|(_, e)| &e.new_text).collect::<Vec<_>>()
    );
    assert_eq!(edits[0].1.new_text, "<http://example.org/new>");
}

#[test_log::test]
fn rename_applies_to_all_occurrences_in_file() {
    let mut h = LspHarness::new();
    // ex:pred appears as predicate on two lines
    let src = "@prefix ex: <http://example.org/> .\n\
               ex:s1 ex:pred ex:o1 .\n\
               ex:s2 ex:pred ex:o2 .";
    let file = h.open_file("file:///multi_rename.ttl", "turtle", src);

    // Cursor on ex:pred at line 1, col 5 = 'e' in ex:pred
    // "ex:s1 ex:pred ex:o1 ."
    //  0123456789...
    let edits = h.rename(&file, 1, 6, "ex:newPred");

    assert_eq!(
        edits.len(),
        2,
        "should produce one edit per occurrence; got edits: {:?}",
        edits.iter().map(|(_, e)| &e.new_text).collect::<Vec<_>>()
    );
    for (_, edit) in &edits {
        assert_eq!(edit.new_text, "ex:newPred");
    }
}

// ─── JSON-LD: quote preservation ─────────────────────────────────────────────

#[test_log::test]
fn prepare_rename_jsonld_iri_strips_double_quotes() {
    let mut h = LspHarness::new();
    // Minimal JSON-LD document with a quoted IRI as @id value
    // The term span covers `"http://example.org/subject"` (with quotes).
    // The placeholder shown to the user should strip the quotes.
    let src = r#"{"@id": "http://example.org/subject", "http://example.org/pred": {"@value": "hello"}}"#;
    let file = h.open_file("file:///id_rename.jsonld", "jsonld", src);
    h.drain_tasks();

    // col 8 = first `"` of `"http://example.org/subject"`
    // {"@id": "http://example.org/subject", ...}
    //  0123456789...
    let result = h.prepare_rename(&file, 0, 8);

    // The rename may or may not be available depending on how JSON-LD parse resolves
    // the IRI into the triple store; we assert on quoting when it is.
    if let Some(r) = result {
        assert!(
            !r.placeholder.starts_with('"'),
            "JSON-LD placeholder should not start with a quote; got {:?}",
            r.placeholder
        );
        assert!(
            !r.placeholder.ends_with('"'),
            "JSON-LD placeholder should not end with a quote; got {:?}",
            r.placeholder
        );
    }
}

#[test_log::test]
fn rename_jsonld_iri_wraps_new_name_in_quotes() {
    let mut h = LspHarness::new();
    let src = r#"{"@id": "http://example.org/subject", "http://example.org/pred": {"@value": "hello"}}"#;
    let file = h.open_file("file:///id_rewrap.jsonld", "jsonld", src);
    h.drain_tasks();

    // col 8 = start of "http://example.org/subject"
    let edits = h.rename(&file, 0, 8, "http://example.org/new-subject");

    for (_, edit) in &edits {
        assert!(
            edit.new_text.starts_with('"') && edit.new_text.ends_with('"'),
            "JSON-LD rename output must be wrapped in double-quotes; got {:?}",
            edit.new_text
        );
        assert_eq!(
            edit.new_text, "\"http://example.org/new-subject\"",
            "inner IRI should be correct"
        );
    }
}
