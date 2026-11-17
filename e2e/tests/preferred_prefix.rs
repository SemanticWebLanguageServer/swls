//! Ontologies are resolved by a namespace's preferred prefix name, not the one typed in the
//! document: `@prefix f: <http://xmlns.com/foaf/0.1/>` still loads `foaf.ttl`.

use swls_e2e_tests::LspHarness;

const MADS_NS: &str = "http://www.loc.gov/mads/rdf/v1#";
const MIRROR: &str = "https://ajuvercr.github.io/lov-mirror/by-prefix";

fn mads_ontology() -> String {
    format!(
        "@prefix madsrdf: <{MADS_NS}> .\n@prefix owl: <http://www.w3.org/2002/07/owl#> .\n\
         madsrdf:Authority a owl:Class ."
    )
}

/// A vocabulary that happens to be published under the name `m`, for another namespace.
const DECOY: &str = "@prefix m: <http://decoy.test/m#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
m:Decoy a owl:Class .";

fn class_labels(h: &mut LspHarness, prefix: &str, ns: &str) -> Vec<String> {
    let src = format!("@prefix {prefix}: <{ns}>.\n<> a {prefix}:");
    let file = h.open_file_and_drain(&format!("file:///{prefix}.ttl"), "turtle", &src);
    h.drain_tasks();
    // `<> a ` is 5 characters, the cursor is on the prefix.
    h.completions(&file, 1, 5)
        .into_iter()
        .map(|c| c.label)
        .filter(|l| l.starts_with(&format!("{prefix}:")))
        .collect()
}

fn lov_file(h: &LspHarness, name: &str) -> swls_core::lsp_types::Url {
    // `cache_url` of any URL gives `<cache>/sources/...`; derive the cache root from it.
    let probe = h.cache_url("https://probe.test/x.ttl");
    let root = probe
        .as_str()
        .split("/sources/")
        .next()
        .unwrap()
        .to_string();
    swls_core::lsp_types::Url::parse(&format!("{root}/lov/{name}.ttl")).unwrap()
}

#[test_log::test]
fn builtin_vocabulary_is_used_whatever_the_prefix_is_called() {
    let mut h = LspHarness::new();
    let src = "@prefix f: <http://xmlns.com/foaf/0.1/>.\n<> a f:";
    let file = h.open_file_and_drain("file:///user.ttl", "turtle", src);
    h.drain_tasks();

    let completions = h.completions(&file, 1, 5);
    h.assert_completions(&completions)
        .contains_label("f:Person");

    let targets: Vec<_> = h
        .definition(&file, 0, 9)
        .into_iter()
        .map(|l| l.uri)
        .collect();
    assert_eq!(targets, vec![lov_file(&h, "foaf")]);
}

#[test_log::test]
fn remote_vocabulary_is_fetched_by_its_preferred_name() {
    let mads = mads_ontology();
    let mut h = LspHarness::with_resources([
        (
            format!("{MIRROR}/madsrdf/ontology.ttl").as_str(),
            mads.as_str(),
        ),
        (format!("{MIRROR}/m/ontology.ttl").as_str(), DECOY),
    ]);

    let found = class_labels(&mut h, "m", MADS_NS);
    assert!(found.contains(&"m:Authority".into()), "{found:?}");
    assert!(!found.contains(&"m:Decoy".into()), "{found:?}");

    assert!(lov_file(&h, "madsrdf").to_file_path().unwrap().exists());
    assert!(!lov_file(&h, "m").to_file_path().unwrap().exists());
}

#[test_log::test]
fn documents_with_different_prefix_names_share_one_ontology_document() {
    let mads = mads_ontology();
    let mut h = LspHarness::with_resources([(
        format!("{MIRROR}/madsrdf/ontology.ttl").as_str(),
        mads.as_str(),
    )]);

    assert!(class_labels(&mut h, "m", MADS_NS).contains(&"m:Authority".into()));
    assert!(class_labels(&mut h, "mads", MADS_NS).contains(&"mads:Authority".into()));

    assert!(h.handle(lov_file(&h, "madsrdf").as_str()).is_some());
    assert!(h.handle(lov_file(&h, "m").as_str()).is_none());
    assert!(h.handle(lov_file(&h, "mads").as_str()).is_none());
}

#[test_log::test]
fn unknown_namespace_ignores_a_vocabulary_for_another_namespace() {
    // `m` is not a known name for this namespace, so the typed name is tried, but the
    // vocabulary published as `m` describes another namespace.
    let mut h = LspHarness::with_resources([(format!("{MIRROR}/m/ontology.ttl").as_str(), DECOY)]);

    let found = class_labels(&mut h, "m", "http://unknown.test/ns#");
    assert!(found.is_empty(), "{found:?}");
    assert!(!lov_file(&h, "m").to_file_path().unwrap().exists());
}
