//! E2E tests for prefix diagnostics and the "add missing prefix" code action.
//!
//! Features tested:
//! * ERROR diagnostic for a prefix used but not declared
//! * WARNING diagnostic for a prefix declared but not used
//! * No diagnostics when all prefixes are both declared and used
//! * "Add prefix declaration" code action for an undefined prefix

use swls_core::components::Disabled;
use swls_core::lsp_types::DiagnosticSeverity;
use swls_e2e_tests::LspHarness;

// ─── Undefined prefix → ERROR ─────────────────────────────────────────────────

#[test_log::test]
fn undefined_prefix_produces_error_diagnostic() {
    let mut h = LspHarness::new();
    // `ex` is used but never declared
    let src = "<> ex:pred ex:obj .";
    let file = h.open_file("file:///undef_prefix.ttl", "turtle", src);

    let diags = h.run_diagnostics();

    let prefix_errors: Vec<_> = diags
        .iter()
        .filter(|(url, d)| {
            url.as_str() == file.url
                && d.severity == Some(DiagnosticSeverity::ERROR)
                && d.message.contains("ex")
        })
        .collect();

    assert!(
        !prefix_errors.is_empty(),
        "Expected at least one ERROR diagnostic for undefined prefix 'ex', got: {:?}",
        diags.iter().map(|(_, d)| &d.message).collect::<Vec<_>>()
    );
}

#[test_log::test]
fn undefined_prefix_diagnostic_spans_the_token() {
    let mut h = LspHarness::new();
    // Line 0: "<> ex:pred ex:obj ."
    //             ^^ col 3–4 is the `ex` prefix + `:pred`
    let src = "<> ex:pred ex:obj .";
    let file = h.open_file("file:///undef_span.ttl", "turtle", src);

    let diags = h.run_diagnostics();

    let error = diags
        .iter()
        .find(|(url, d)| {
            url.as_str() == file.url
                && d.severity == Some(DiagnosticSeverity::ERROR)
                && d.message.contains("\"ex\"")
        });

    assert!(
        error.is_some(),
        "Expected ERROR with message containing '\"ex\"', got: {:?}",
        diags.iter().map(|(_, d)| &d.message).collect::<Vec<_>>()
    );

    let (_, diag) = error.unwrap();
    // The diagnostic should be on line 0
    assert_eq!(diag.range.start.line, 0, "Diagnostic should be on line 0");
}

// ─── Unused prefix → WARNING ──────────────────────────────────────────────────

#[test_log::test]
fn unused_prefix_produces_warning_diagnostic() {
    let mut h = LspHarness::new();
    // `foaf` declared but never used in any triple
    let src = "@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n<> a <http://example.org/Thing> .";
    let file = h.open_file("file:///unused_prefix.ttl", "turtle", src);

    let diags = h.run_diagnostics();

    let warnings: Vec<_> = diags
        .iter()
        .filter(|(url, d)| {
            url.as_str() == file.url
                && d.severity == Some(DiagnosticSeverity::WARNING)
                && d.message.contains("foaf")
        })
        .collect();

    assert!(
        !warnings.is_empty(),
        "Expected at least one WARNING for unused prefix 'foaf', got: {:?}",
        diags.iter().map(|(_, d)| &d.message).collect::<Vec<_>>()
    );
}

// ─── All prefixes valid → no prefix diagnostics ───────────────────────────────

#[test_log::test]
fn declared_and_used_prefix_produces_no_prefix_diagnostic() {
    let mut h = LspHarness::new();
    let src = "@prefix ex: <http://example.org/> .\n<> ex:pred ex:obj .";
    let file = h.open_file("file:///valid_prefix.ttl", "turtle", src);

    let diags = h.run_diagnostics();

    let prefix_diags: Vec<_> = diags
        .iter()
        .filter(|(url, d)| {
            url.as_str() == file.url
                && (d.message.contains("prefix") || d.message.contains("Prefix"))
        })
        .collect();

    assert!(
        prefix_diags.is_empty(),
        "Expected no prefix diagnostics, but got: {:?}",
        prefix_diags
            .iter()
            .map(|(_, d)| &d.message)
            .collect::<Vec<_>>()
    );
}

// ─── Multiple undefined prefixes ─────────────────────────────────────────────

#[test_log::test]
fn two_undefined_prefixes_produce_two_diagnostics() {
    let mut h = LspHarness::new();
    let src = "<> ex:pred schema:name .";
    let file = h.open_file("file:///two_undef.ttl", "turtle", src);

    let diags = h.run_diagnostics();

    let errors: Vec<_> = diags
        .iter()
        .filter(|(url, d)| {
            url.as_str() == file.url && d.severity == Some(DiagnosticSeverity::ERROR)
        })
        .collect();

    assert_eq!(
        errors.len(),
        2,
        "Expected one ERROR per undefined prefix, got: {:?}",
        errors.iter().map(|(_, d)| &d.message).collect::<Vec<_>>()
    );
}

// ─── Code action: add missing prefix ─────────────────────────────────────────

#[test_log::test]
fn add_missing_prefix_code_action_exists_for_undefined_prefix() {
    let mut h = LspHarness::new();
    // Use `foaf` without a declaration — there is a well-known LOV entry for it
    let src = "<> foaf:name \"Alice\" .";
    let file = h.open_file("file:///add_prefix_action.ttl", "turtle", src);
    // Drain LOV data so bundled prefix entries are registered
    h.drain_tasks();

    let actions = h.code_actions(&file);

    let add_foaf: Vec<_> = actions
        .iter()
        .filter(|a| a.title.contains("foaf"))
        .collect();

    assert!(
        !add_foaf.is_empty(),
        "Expected a code action for adding 'foaf' prefix, got actions: {:?}",
        actions.iter().map(|a| &a.title).collect::<Vec<_>>()
    );
}

#[test_log::test]
fn add_missing_prefix_code_action_inserts_at_file_top() {
    let mut h = LspHarness::new();
    let src = "<> foaf:name \"Alice\" .";
    let file = h.open_file("file:///add_prefix_top.ttl", "turtle", src);
    h.drain_tasks();

    let actions = h.code_actions(&file);

    let add_foaf = actions.iter().find(|a| a.title.contains("foaf"));
    assert!(add_foaf.is_some(), "Expected code action for 'foaf'");

    let action = add_foaf.unwrap();
    let edits = action
        .edit
        .as_ref()
        .and_then(|e| e.changes.as_ref())
        .and_then(|c| c.values().next())
        .expect("code action should have text edits");

    assert!(!edits.is_empty(), "code action should have at least one edit");

    // The insert position should be at the top (line 0, char 0)
    let edit = &edits[0];
    assert_eq!(
        edit.range.start,
        swls_core::lsp_types::Position::new(0, 0),
        "should insert at top of file when no existing prefix declarations"
    );

    // The edit text should look like a valid prefix declaration
    assert!(
        edit.new_text.contains("@prefix foaf:"),
        "edit should insert a @prefix foaf: declaration, got: {:?}",
        edit.new_text
    );
    assert!(
        edit.new_text.contains("http://"),
        "edit should include a URL, got: {:?}",
        edit.new_text
    );
}

// ─── JSON-LD prefix diagnostics ──────────────────────────────────────────────
// Note: JSON-LD silently drops triples with undefined prefix terms (JSON-LD
// semantics), so "undefined prefix" detection is only possible for prefixes
// that DO appear in successfully-produced triples.  What we CAN reliably test:
// * Unused prefixes (declared in @context but never appear in a triple)
// * No false positives when all declared prefixes are used

#[test_log::test]
fn jsonld_unused_prefix_produces_warning_diagnostic() {
    let mut h = LspHarness::new();
    // `rdfs2` declared but never used as a prefix in any triple
    let src = r#"{
  "@context": {
    "foaf": "http://xmlns.com/foaf/0.1/",
    "rdfs2": "http://www.w3.org/2000/01/rdf-schema#"
  },
  "foaf:knows": "foaf:testing"
}"#;
    let file = h.open_file("file:///unused_jsonld.jsonld", "json-ld", src);
    h.drain_tasks();
    let diags = h.run_diagnostics();

    let warnings: Vec<_> = diags
        .iter()
        .filter(|(url, d)| {
            url.as_str() == file.url
                && d.severity == Some(DiagnosticSeverity::WARNING)
                && d.message.contains("rdfs2")
        })
        .collect();

    assert!(
        !warnings.is_empty(),
        "Expected WARNING for unused prefix 'rdfs2' in JSON-LD, got: {:?}",
        diags.iter().map(|(_, d)| &d.message).collect::<Vec<_>>()
    );
}

#[test_log::test]
fn jsonld_unused_prefix_warning_spans_declaration_line() {
    let mut h = LspHarness::new();
    let src = r#"{
  "@context": {
    "foaf": "http://xmlns.com/foaf/0.1/",
    "rdfs2": "http://www.w3.org/2000/01/rdf-schema#"
  },
  "foaf:name": "Alice"
}"#;
    let file = h.open_file("file:///unused_jsonld_span.jsonld", "json-ld", src);
    h.drain_tasks();
    let diags = h.run_diagnostics();

    let warning = diags.iter().find(|(url, d)| {
        url.as_str() == file.url
            && d.severity == Some(DiagnosticSeverity::WARNING)
            && d.message.contains("rdfs2")
    });

    assert!(warning.is_some(), "Expected WARNING for 'rdfs2'");
    let (_, diag) = warning.unwrap();
    // `"rdfs2": ...` is on line 3 (0-indexed)
    assert_eq!(
        diag.range.start.line, 3,
        "Diagnostic should be on the 'rdfs2' declaration line (line 3), got line {}",
        diag.range.start.line
    );
}

#[test_log::test]
fn jsonld_used_term_alias_produces_no_warning() {
    let mut h = LspHarness::new();
    // `name` is a JSON-LD term alias (value is a specific term, not a
    // namespace). It is used as a bare key `"name"`, not as `name:local`, so it
    // must NOT be flagged as "declared but never used".
    let src = r#"{ "@context": { "name": "foaf:name" }, "name": "name" }"#;
    let file = h.open_file("file:///alias_used.jsonld", "json-ld", src);
    h.drain_tasks();
    let diags = h.run_diagnostics();

    let warnings: Vec<_> = diags
        .iter()
        .filter(|(url, d)| {
            url.as_str() == file.url
                && d.severity == Some(DiagnosticSeverity::WARNING)
                && d.message.contains("name")
        })
        .collect();

    assert!(
        warnings.is_empty(),
        "Used term alias 'name' should not warn, got: {:?}",
        diags.iter().map(|(_, d)| &d.message).collect::<Vec<_>>()
    );
}

#[test_log::test]
fn jsonld_unused_term_alias_still_warns() {
    let mut h = LspHarness::new();
    // `name` alias is declared but never referenced anywhere → should warn.
    let src = r#"{ "@context": { "name": "foaf:name" }, "foaf:knows": "x" }"#;
    let file = h.open_file("file:///alias_unused.jsonld", "json-ld", src);
    h.drain_tasks();
    let diags = h.run_diagnostics();

    let warning = diags.iter().find(|(url, d)| {
        url.as_str() == file.url
            && d.severity == Some(DiagnosticSeverity::WARNING)
            && d.message.contains("name")
    });

    assert!(
        warning.is_some(),
        "Unused term alias 'name' should warn, got: {:?}",
        diags.iter().map(|(_, d)| &d.message).collect::<Vec<_>>()
    );
    let (_, diag) = warning.unwrap();
    // The warning must point at the `"name"` context key, not at 0..0.
    assert_eq!(diag.range.start.line, 0);
    assert_eq!(
        &src[diag.range.start.character as usize..diag.range.end.character as usize],
        "\"name\"",
        "Warning range should cover the context key, got {:?}",
        diag.range
    );
}

#[test_log::test]
fn jsonld_no_false_positives_when_all_prefixes_used() {
    let mut h = LspHarness::new();
    let src = r#"{
  "@context": {
    "foaf": "http://xmlns.com/foaf/0.1/"
  },
  "foaf:name": "Alice"
}"#;
    let file = h.open_file("file:///valid_jsonld.jsonld", "json-ld", src);
    h.drain_tasks();
    let diags = h.run_diagnostics();

    let prefix_diags: Vec<_> = diags
        .iter()
        .filter(|(url, d)| {
            url.as_str() == file.url
                && (d.message.contains("prefix") || d.message.contains("foaf"))
        })
        .collect();

    assert!(
        prefix_diags.is_empty(),
        "Expected no prefix diagnostics for valid JSON-LD, got: {:?}",
        prefix_diags
            .iter()
            .map(|(_, d)| &d.message)
            .collect::<Vec<_>>()
    );
}

#[test_log::test]
fn no_code_action_for_already_declared_prefix() {
    let mut h = LspHarness::new();
    let src = "@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n<> foaf:name \"Alice\" .";
    let file = h.open_file("file:///no_add_action.ttl", "turtle", src);
    h.drain_tasks();

    let actions = h.code_actions(&file);

    let add_foaf: Vec<_> = actions
        .iter()
        .filter(|a| a.title.contains("Add prefix") && a.title.contains("foaf"))
        .collect();

    assert!(
        add_foaf.is_empty(),
        "Should NOT offer 'add prefix' action for already declared 'foaf', got: {:?}",
        add_foaf.iter().map(|a| &a.title).collect::<Vec<_>>()
    );
}

// ─── Config toggles: disabled.unused_prefix / disabled.undefined_prefix ──────

#[test_log::test]
fn unused_prefix_warning_disabled_by_config() {
    let mut h = LspHarness::new();
    h.set_config(|c| {
        c.disabled.insert(Disabled::UnusedPrefix);
    });
    let src = "@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n<> a <http://example.org/Thing> .";
    let file = h.open_file("file:///unused_prefix_disabled.ttl", "turtle", src);

    let diags = h.run_diagnostics();

    let warnings: Vec<_> = diags
        .iter()
        .filter(|(url, d)| {
            url.as_str() == file.url
                && d.severity == Some(DiagnosticSeverity::WARNING)
                && d.message.contains("foaf")
        })
        .collect();

    assert!(
        warnings.is_empty(),
        "Expected no unused-prefix warning when disabled, got: {:?}",
        diags.iter().map(|(_, d)| &d.message).collect::<Vec<_>>()
    );
}

#[test_log::test]
fn undefined_prefix_error_disabled_by_config() {
    let mut h = LspHarness::new();
    h.set_config(|c| {
        c.disabled.insert(Disabled::UndefinedPrefix);
    });
    let src = "<> ex:pred ex:obj .";
    let file = h.open_file("file:///undef_prefix_disabled.ttl", "turtle", src);

    let diags = h.run_diagnostics();

    let errors: Vec<_> = diags
        .iter()
        .filter(|(url, d)| {
            url.as_str() == file.url && d.severity == Some(DiagnosticSeverity::ERROR)
        })
        .collect();

    assert!(
        errors.is_empty(),
        "Expected no undefined-prefix error when disabled, got: {:?}",
        diags.iter().map(|(_, d)| &d.message).collect::<Vec<_>>()
    );
}

#[test_log::test]
fn disabling_undefined_prefix_does_not_affect_unused_prefix_warning() {
    let mut h = LspHarness::new();
    h.set_config(|c| {
        c.disabled.insert(Disabled::UndefinedPrefix);
    });
    let src = "@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n<> a <http://example.org/Thing> .";
    let file = h.open_file("file:///undef_disabled_unused_kept.ttl", "turtle", src);

    let diags = h.run_diagnostics();

    let warnings: Vec<_> = diags
        .iter()
        .filter(|(url, d)| {
            url.as_str() == file.url
                && d.severity == Some(DiagnosticSeverity::WARNING)
                && d.message.contains("foaf")
        })
        .collect();

    assert!(
        !warnings.is_empty(),
        "Unused-prefix warning should still fire when only undefined_prefix is disabled, got: {:?}",
        diags.iter().map(|(_, d)| &d.message).collect::<Vec<_>>()
    );
}

#[test_log::test]
fn add_missing_prefix_code_action_disabled_by_config() {
    let mut h = LspHarness::new();
    h.set_config(|c| {
        c.disabled.insert(Disabled::UndefinedPrefix);
    });
    let src = "<> foaf:name \"Alice\" .";
    let file = h.open_file("file:///add_prefix_disabled.ttl", "turtle", src);
    h.drain_tasks();

    let actions = h.code_actions(&file);

    let add_foaf: Vec<_> = actions.iter().filter(|a| a.title.contains("foaf")).collect();

    assert!(
        add_foaf.is_empty(),
        "Should NOT offer 'add prefix' quick-fix when undefined_prefix diagnostic is disabled, got: {:?}",
        add_foaf.iter().map(|a| &a.title).collect::<Vec<_>>()
    );
}
