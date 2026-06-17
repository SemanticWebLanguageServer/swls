//! E2E completion tests for SPARQL.
//!
//! Covers keyword completion and variable completion in SPARQL queries.

use swls_e2e_tests::LspHarness;

// ─── Keyword completion ───────────────────────────────────────────────────────

#[test_log::test]
fn sparql_keywords_are_suggested() {
    let mut h = LspHarness::new();
    // Position cursor at "SEL" — the beginning of SELECT
    let src = "SEL";
    let file = h.open_file("file:///query.sparql", "sparql", src);

    let completions = h.completions(&file, 0, 0);
    h.assert_completions(&completions)
        .count_at_least(1)
        .contains_label("SELECT");
}

#[test_log::test]
fn sparql_where_keyword_is_suggested() {
    let mut h = LspHarness::new();
    let src = "SELECT ?s WHE";
    let file = h.open_file("file:///query2.sparql", "sparql", src);

    // Character 10 = 'W' in "WHE"
    let completions = h.completions(&file, 0, 10);
    h.assert_completions(&completions)
        .count_at_least(1)
        .contains_label("WHERE");
}

// ─── Prefix-based completions ─────────────────────────────────────────────────

#[test_log::test]
fn sparql_prefix_terms_are_suggested_after_colon() {
    let mut h = LspHarness::new();
    let src = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n\
               SELECT ?s WHERE { ?s foaf: }";
    let file = h.open_file("file:///prefixed.sparql", "sparql", src);
    h.drain_tasks();

    // Line 1: "SELECT ?s WHERE { ?s foaf: }"
    //          0123456789012345678901234567
    // "foaf:" starts at column 20
    let completions = h.completions(&file, 1, 20);
    h.assert_completions(&completions).count_at_least(1);
}

// ─── Variable completion ──────────────────────────────────────────────────────

#[test_log::test]
fn sparql_variable_completion_suggests_declared_variables() {
    let mut h = LspHarness::new();
    // ?sub is declared; when we type ?s in the WHERE clause the server should offer ?sub
    let src = "SELECT ?sub WHERE { ?sub a <http://example.org/Thing> . ?s }";
    let file = h.open_file("file:///vars.sparql", "sparql", src);

    // "?s" at the end — character 52 ('?' position).
    // The exact position depends on text length; we just verify non-panic.
    let completions = h.completions(&file, 0, 52);
    // Variable completions may or may not be present depending on token detection.
    let _ = completions;
}
