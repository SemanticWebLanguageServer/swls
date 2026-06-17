//! E2E completion tests for the Turtle language.
//!
//! These tests verify that the LSP returns sensible completions for Turtle documents, covering:
//! - Keyword completion (`@prefix`, `@base`, `a`)
//! - Defined-prefix expansion (completing a prefix name from declarations in the document)
//! - Class completion via a locally injected ontology
//! - Property completion via a locally injected ontology
//! - Cross-file subject completion (subjects defined in a linked open file)
//! - LOV-based prefix completion (suggested prefixes from the bundled vocabulary)

use swls_e2e_tests::LspHarness;

// ─── Fictional ontology shared across tests ───────────────────────────────────
//
// This namespace (`http://fictional.test/onto#`) does not exist anywhere on the
// network, so any completions derived from it can only come from the locally
// injected file — not from LOV, prefix.cc, or any external source.

const FICT_ONTO_TTL: &str = r#"
@prefix fict: <http://fictional.test/onto#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

fict:Widget a rdfs:Class ;
    rdfs:label "Widget" ;
    rdfs:comment "A reusable fictional component." .

fict:Gadget a rdfs:Class ;
    rdfs:subClassOf fict:Widget ;
    rdfs:label "Gadget" .

fict:hasComponent a rdf:Property ;
    rdfs:label "hasComponent" ;
    rdfs:domain fict:Widget ;
    rdfs:range fict:Widget .

fict:weight a rdf:Property ;
    rdfs:label "weight" ;
    rdfs:domain fict:Widget .
"#;

// ─── Keyword completion ───────────────────────────────────────────────────────

#[test_log::test]
fn turtle_keywords_are_suggested_at_start_of_line() {
    let mut h = LspHarness::new();
    let file = h.open_file("file:///keywords.ttl", "turtle", "");

    let completions = h.completions(&file, 0, 0);
    // keyword_complete always adds all Turtle keywords regardless of token text
    h.assert_completions(&completions)
        .contains_label("@prefix")
        .contains_label("@base")
        .contains_label("a");
}

#[test_log::test]
fn turtle_keyword_a_is_suggested_as_predicate() {
    let mut h = LspHarness::new();
    // Line 1: "ex:subject " — cursor at col 11, after the subject and space
    let src = "@prefix ex: <http://example.org/>.\nex:subject ";
    let file = h.open_file("file:///kw_a.ttl", "turtle", src);

    let completions = h.completions(&file, 1, 11);
    h.assert_completions(&completions).contains_label("a");
}

// ─── Prefix name completion (bundled LOV) ─────────────────────────────────────

#[test_log::test]
fn completing_partial_prefix_name_suggests_matching_lov_prefix() {
    let mut h = LspHarness::new();
    // "foa" as a lone subject token — the bundled LOV vocabulary contains "foaf"
    // whose name starts with "foa", so it should be offered as a prefix suggestion.
    // The label produced by prefix_completion_helper is the bare prefix name (e.g. "foaf").
    let src = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\nfoa";
    let file = h.open_file("file:///prefix_expand.ttl", "turtle", src);

    // character 0: start of "foa" token on line 1
    let completions = h.completions(&file, 1, 0);
    h.assert_completions(&completions)
        .contains_label("@prefix") // always present via keyword_complete
        .contains_label("foaf"); // bundled LOV prefix whose name starts with "foa"
}

#[test_log::test]
fn lov_bundled_prefixes_are_suggested_without_any_declaration() {
    let mut h = LspHarness::new();
    // No prefix declaration in the document at all.
    // "foa" still matches "foaf" from the bundled LOV static data (loaded at Startup).
    let src = "foa";
    let file = h.open_file("file:///lov_prefix.ttl", "turtle", src);
    h.drain_tasks();

    let completions = h.completions(&file, 0, 0);
    h.assert_completions(&completions).contains_label("foaf");
}

// ─── Predicate-position keywords ─────────────────────────────────────────────

#[test_log::test]
fn completing_after_colon_includes_keywords() {
    let mut h = LspHarness::new();
    // Cursor on the "foaf:" token at the predicate position.
    // Without the ontology loaded the server still returns all Turtle keywords.
    let src = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n<> foaf:";
    let file = h.open_file("file:///prefix_colon.ttl", "turtle", src);

    // line 1: "<> foaf:" — col 3 is the 'f' in "foaf:"
    let completions = h.completions(&file, 1, 3);
    h.assert_completions(&completions)
        .contains_label("a")
        .contains_label("@prefix");
}

// ─── Class completion from a locally injected fictional ontology ──────────────

#[test_log::test]
fn class_completion_returns_classes_from_injected_ontology() {
    // Open the fictional ontology as a linked background file.
    // Its triples are loaded into the shared Oxigraph store via load_store, then
    // derive_ontologies runs a SPARQL query and populates Ontologies.classes with
    // fict:Widget and fict:Gadget.
    let mut h = LspHarness::new();
    h.open_linked_file("file:///fict-onto.ttl", "turtle", FICT_ONTO_TTL);
    h.drain_tasks();

    // The active document declares the fictional prefix and has a partial rdf:type object.
    // "<> a fict:" — the object token is "fict:" (cursor on col 5, the 'f').
    //  positions:  '<'=0,'>'=1,' '=2,'a'=3,' '=4,'f'=5
    let src = "@prefix fict: <http://fictional.test/onto#>.\n<> a fict:";
    let file = h.open_file("file:///class_complete.ttl", "turtle", src);
    h.drain_tasks();

    let completions = h.completions(&file, 1, 5);
    h.assert_completions(&completions)
        // complete_class shortens the IRI using the document's prefix declarations.
        // "http://fictional.test/onto#Widget" → "fict:Widget"
        .contains_label("fict:Widget")
        .contains_label("fict:Gadget");
}

// ─── Property completion from a locally injected fictional ontology ───────────

#[test_log::test]
fn property_completion_returns_domain_matching_properties() {
    // Same fictional ontology: fict:Widget has properties fict:hasComponent and fict:weight.
    let mut h = LspHarness::new();
    h.open_linked_file("file:///fict-onto.ttl", "turtle", FICT_ONTO_TTL);
    h.drain_tasks();

    // The document declares fict:thing as a fict:Widget, then starts a new predicate.
    // ParseLabel runs infer_types which maps fict:thing → fict:Widget type-id, enabling
    // domain-aware property filtering in complete_properties.
    //
    // Line 2: "fict:thing fict:"
    //          01234567890123456
    //  col 11 = 'f' of the second "fict:"
    let src = "@prefix fict: <http://fictional.test/onto#>.\n\
               fict:thing a fict:Widget .\n\
               fict:thing fict:";
    let file = h.open_file("file:///prop_complete.ttl", "turtle", src);
    h.drain_tasks();

    let completions = h.completions(&file, 2, 11);
    h.assert_completions(&completions)
        .contains_label("fict:hasComponent")
        .contains_label("fict:weight");
}

// ─── Cross-file subject completion ───────────────────────────────────────────

#[test_log::test]
fn subject_completion_pulls_subjects_from_linked_open_file() {
    let mut h = LspHarness::new();

    // Linked file defines "foaf:me" as a subject with IRI http://xmlns.com/foaf/0.1/me.
    let linked_src = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n\
                      foaf:me foaf:name \"Alice\".";
    h.open_file("file:///linked.ttl", "turtle", linked_src);

    // Primary file types "foaf:" — subject_completion iterates all Open triples and
    // finds foaf:me because its expanded IRI starts with the expanded prefix.
    // The completion label is the full subject IRI (not the shortened form).
    let primary_src = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\nfoaf:";
    let file = h.open_file("file:///primary.ttl", "turtle", primary_src);
    h.drain_tasks();

    // col 0 = 'f' in "foaf:"
    let completions = h.completions(&file, 1, 0);
    h.assert_completions(&completions)
        .contains_label("http://xmlns.com/foaf/0.1/me");
}

// ─── Fully fictional ontology (guaranteed not from any external source) ────────

#[test_log::test]
fn completions_from_locally_injected_ontology() {
    // This namespace has never been published anywhere. Any class or property completions
    // using it can only originate from the injected file — not from LOV or prefix.cc.
    const ENG_ONTO_TTL: &str = r#"
@prefix eng: <http://no-such-ns.invalid/engine-ontology#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

eng:Engine a rdfs:Class ;
    rdfs:label "Engine" .

eng:Fuel a rdfs:Class ;
    rdfs:label "Fuel" .

eng:burnsFuel a rdf:Property ;
    rdfs:label "burnsFuel" ;
    rdfs:domain eng:Engine ;
    rdfs:range eng:Fuel .

eng:hasCylinders a rdf:Property ;
    rdfs:label "hasCylinders" ;
    rdfs:domain eng:Engine .
"#;

    let mut h = LspHarness::new();

    // Inject the fictional ontology as a linked background document.
    h.open_linked_file("file:///eng-onto.ttl", "turtle", ENG_ONTO_TTL);
    h.drain_tasks();

    // ── Class completions ──────────────────────────────────────────────────────
    // "<> a eng:" — object of rdf:type predicate, token = "eng:"
    //  positions: '<'=0,'>'=1,' '=2,'a'=3,' '=4,'e'=5
    let src_class = "@prefix eng: <http://no-such-ns.invalid/engine-ontology#>.\n<> a eng:";
    let class_file = h.open_file("file:///eng_class.ttl", "turtle", src_class);
    h.drain_tasks();

    let class_completions = h.completions(&class_file, 1, 5);
    h.assert_completions(&class_completions)
        .contains_label("eng:Engine")
        .contains_label("eng:Fuel")
        // must NOT contain anything from the real foaf or other known ontologies
        .does_not_contain_label("foaf:Person")
        .does_not_contain_label("foaf:Agent");

    // ── Property completions ───────────────────────────────────────────────────
    // Line 2: "eng:thing eng:" — col 10 = 'e' of the second "eng:"
    let src_prop = "@prefix eng: <http://no-such-ns.invalid/engine-ontology#>.\n\
                    eng:thing a eng:Engine .\n\
                    eng:thing eng:";
    let prop_file = h.open_file("file:///eng_prop.ttl", "turtle", src_prop);
    h.drain_tasks();

    let prop_completions = h.completions(&prop_file, 2, 10);
    println!("Completions {:#?} ", prop_completions);
    h.assert_completions(&prop_completions)
        .contains_label("eng:burnsFuel")
        .contains_label("eng:hasCylinders");
}
