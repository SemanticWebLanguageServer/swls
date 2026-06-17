//! E2E tests for the two settings-driven features:
//!
//! 1. Preferred Turtle prefix kind (`@prefix` vs `PREFIX`) used when completing prefixes.
//! 2. Namespace property validation: warn about predicate IRIs in a configured
//!    namespace that are not defined in any known ontology, plus the allow-list
//!    quick-fix and the "excluded by user" hover message.

use swls_core::components::PrefixFormat;
use swls_core::lsp_types::DiagnosticSeverity;
use swls_e2e_tests::LspHarness;

// ─── Feature 1: preferred prefix format ───────────────────────────────────────

fn foaf_completion_insert_text(h: &mut LspHarness, file: &swls_e2e_tests::FileHandle) -> String {
    // Completing "foa" (with foaf NOT declared) offers the bundled LOV "foaf" prefix;
    // its edits include the prefix declaration inserted at the top of the document.
    h.drain_tasks();
    let completions = h.completions(file, 0, 0);
    let foaf = completions
        .into_iter()
        .find(|c| c.label == "foaf")
        .expect("expected a 'foaf' prefix completion");
    foaf.edits
        .iter()
        .map(|e| e.new_text.clone())
        .collect::<Vec<_>>()
        .join("")
}

#[test_log::test]
fn prefix_completion_defaults_to_turtle_at_prefix() {
    let mut h = LspHarness::new();
    let file = h.open_file("file:///pf_default.ttl", "turtle", "foa");

    let inserted = foaf_completion_insert_text(&mut h, &file);
    assert!(
        inserted.contains("@prefix foaf:"),
        "default prefix completion should use @prefix form, got: {inserted:?}"
    );
}

#[test_log::test]
fn prefix_completion_honors_sparql_format() {
    let mut h = LspHarness::new();
    h.set_config(|c| c.prefix_format = Some(PrefixFormat::Sparql));

    let file = h.open_file("file:///pf_sparql.ttl", "turtle", "foa");

    let inserted = foaf_completion_insert_text(&mut h, &file);
    assert!(
        inserted.contains("PREFIX foaf:") && !inserted.contains("@prefix foaf:"),
        "with prefix_format=sparql completion should use PREFIX form, got: {inserted:?}"
    );
}

// ─── Feature 2: namespace property validation ─────────────────────────────────

// A namespace that does not exist anywhere on the network, so the only "known"
// property in it is the one we inject below.
const CFG_NS: &str = "http://no-such-ns.invalid/cfg#";

const CFG_ONTO_TTL: &str = r#"
@prefix cfg: <http://no-such-ns.invalid/cfg#> .
@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

cfg:known a rdf:Property ;
    rdfs:label "known" .
"#;

const DOC_TTL: &str = "@prefix cfg: <http://no-such-ns.invalid/cfg#>.\n\
                       <#s> cfg:known \"a\" ;\n\
                       cfg:unknown \"b\" .";

#[test_log::test]
fn unknown_property_in_closed_namespace_warns() {
    let mut h = LspHarness::new();
    h.set_config(|c| {
        c.closed_namespaces.insert(CFG_NS.to_string());
    });

    h.open_linked_file("file:///cfg-onto.ttl", "turtle", CFG_ONTO_TTL);
    h.drain_tasks();
    let file = h.open_file("file:///cfg-doc.ttl", "turtle", DOC_TTL);
    h.drain_tasks();

    let diags = h.run_diagnostics();
    let warnings: Vec<_> = diags
        .iter()
        .filter(|(url, d)| {
            url.as_str() == file.url
                && d.severity == Some(DiagnosticSeverity::WARNING)
                && d.message.contains("not defined in the ontology")
        })
        .collect();

    // cfg:unknown should warn, cfg:known should NOT.
    assert!(
        warnings.iter().any(|(_, d)| d.message.contains("unknown")),
        "expected a warning for cfg:unknown, got: {:?}",
        diags.iter().map(|(_, d)| &d.message).collect::<Vec<_>>()
    );
    assert!(
        !warnings.iter().any(|(_, d)| d.message.contains("#known")),
        "cfg:known is a defined property and must NOT warn, got: {:?}",
        warnings.iter().map(|(_, d)| &d.message).collect::<Vec<_>>()
    );
}

#[test_log::test]
fn allow_listed_property_does_not_warn() {
    let mut h = LspHarness::new();
    h.set_config(|c| {
        c.closed_namespaces.insert(CFG_NS.to_string());
        c.allowed_properties
            .insert(format!("{CFG_NS}unknown"));
    });

    h.open_linked_file("file:///cfg-onto2.ttl", "turtle", CFG_ONTO_TTL);
    h.drain_tasks();
    let file = h.open_file("file:///cfg-doc2.ttl", "turtle", DOC_TTL);
    h.drain_tasks();

    let diags = h.run_diagnostics();
    let warnings: Vec<_> = diags
        .iter()
        .filter(|(url, d)| {
            url.as_str() == file.url
                && d.severity == Some(DiagnosticSeverity::WARNING)
                && d.message.contains("not defined in the ontology")
        })
        .collect();

    assert!(
        warnings.is_empty(),
        "allow-listed cfg:unknown must not warn, got: {:?}",
        warnings.iter().map(|(_, d)| &d.message).collect::<Vec<_>>()
    );
}

#[test_log::test]
fn no_warnings_without_closed_namespace_config() {
    let mut h = LspHarness::new();
    // No closed_namespaces configured at all.
    h.open_linked_file("file:///cfg-onto3.ttl", "turtle", CFG_ONTO_TTL);
    h.drain_tasks();
    let file = h.open_file("file:///cfg-doc3.ttl", "turtle", DOC_TTL);
    h.drain_tasks();

    let diags = h.run_diagnostics();
    let warnings: Vec<_> = diags
        .iter()
        .filter(|(url, d)| {
            url.as_str() == file.url && d.message.contains("not defined in the ontology")
        })
        .collect();

    assert!(
        warnings.is_empty(),
        "no namespace validation should happen without closed_namespaces, got: {:?}",
        warnings.iter().map(|(_, d)| &d.message).collect::<Vec<_>>()
    );
}

#[test_log::test]
fn unknown_property_offers_allow_quick_fix() {
    let mut h = LspHarness::new();
    h.set_config(|c| {
        c.closed_namespaces.insert(CFG_NS.to_string());
    });

    h.open_linked_file("file:///cfg-onto4.ttl", "turtle", CFG_ONTO_TTL);
    h.drain_tasks();
    let file = h.open_file("file:///cfg-doc4.ttl", "turtle", DOC_TTL);
    h.drain_tasks();

    // Cursor on the "cfg:unknown" predicate (line 2).
    let actions = h.code_actions_at(&file, 2, 3);
    let allow = actions
        .iter()
        .find(|a| a.command.as_ref().map(|c| c.command.as_str()) == Some("swls.allowProperty"));

    let allow = allow.expect("expected an allow-property quick-fix");
    let args = allow
        .command
        .as_ref()
        .and_then(|c| c.arguments.as_ref())
        .expect("command should carry arguments");
    assert!(
        args.iter().any(|v| v.as_str() == Some(&format!("{CFG_NS}unknown"))),
        "quick-fix should target cfg:unknown, got: {args:?}"
    );
}

#[test_log::test]
fn allow_quick_fix_not_offered_away_from_unknown_property() {
    let mut h = LspHarness::new();
    h.set_config(|c| {
        c.closed_namespaces.insert(CFG_NS.to_string());
    });

    h.open_linked_file("file:///cfg-onto4b.ttl", "turtle", CFG_ONTO_TTL);
    h.drain_tasks();
    let file = h.open_file("file:///cfg-doc4b.ttl", "turtle", DOC_TTL);
    h.drain_tasks();

    // Cursor on line 1 (the "cfg:known" statement), not on the unknown property.
    let actions = h.code_actions_at(&file, 1, 5);
    assert!(
        !actions
            .iter()
            .any(|a| a.command.as_ref().map(|c| c.command.as_str()) == Some("swls.allowProperty")),
        "allow-property quick-fix should not appear when cursor is not on the unknown property"
    );
}

#[test_log::test]
fn hover_on_allow_listed_property_explains_exclusion() {
    let mut h = LspHarness::new();
    h.set_config(|c| {
        c.closed_namespaces.insert(CFG_NS.to_string());
        c.allowed_properties
            .insert(format!("{CFG_NS}unknown"));
    });

    h.open_linked_file("file:///cfg-onto5.ttl", "turtle", CFG_ONTO_TTL);
    h.drain_tasks();
    let file = h.open_file("file:///cfg-doc5.ttl", "turtle", DOC_TTL);
    h.drain_tasks();

    // Line 2: "cfg:unknown \"b\" ." — hover over the predicate token.
    //          0123456789
    let hover = h.hover(&file, 2, 0);
    let joined = hover.join("\n");
    assert!(
        joined.contains("allowed_properties") && joined.contains("not defined"),
        "hover should explain the user-exclusion, got: {hover:?}"
    );
}
