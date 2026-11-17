//! The ontology cache and the editor lifecycle of cached documents.
//!
//! Remote ontologies are cached at readable locations (`…/sources/<host>/<path>`), reused
//! instead of refetched, and treated as snapshots: opening one in the editor never changes
//! what the server knows, and closing it restores the server's copy.

use std::time::{Duration, SystemTime};

use swls_core::util::{
    cache::{cached, CACHE_TTL},
    fs::Fs,
};
use swls_e2e_tests::LspHarness;

const ONTO_URL: &str = "https://example.test/fict/ontology.ttl";
const ONTO: &str = "@prefix fict: <http://fictional.test/onto#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix vann: <http://purl.org/vocab/vann/> .
<http://fictional.test/onto> a owl:Ontology ;
  vann:preferredNamespacePrefix \"fict\" ;
  vann:preferredNamespaceUri \"http://fictional.test/onto#\" .
fict:Widget a owl:Class .
";
const EDITED: &str = "@prefix fict: <http://fictional.test/onto#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
fict:Hacked a owl:Class .
";
const USER_DOC: &str = "@prefix fict: <http://fictional.test/onto#>.\n<> a fict:";

fn configured() -> LspHarness {
    let mut h = LspHarness::with_resources([(ONTO_URL, ONTO)]);
    h.load_config(|c| {
        c.ontologies.insert(ONTO_URL.into());
    });
    h
}

#[test_log::test]
fn remote_ontology_is_cached_at_a_readable_location() {
    let mut h = configured();
    h.open_file_and_drain("file:///user.ttl", "turtle", USER_DOC);

    let cache = h.cache_url(ONTO_URL);
    assert!(
        cache
            .as_str()
            .ends_with("/sources/example.test/fict/ontology.ttl"),
        "{cache}"
    );
    let on_disk = std::fs::read_to_string(cache.to_file_path().unwrap()).unwrap();
    assert_eq!(on_disk, ONTO);
}

#[test_log::test]
fn editing_a_cached_copy_does_not_change_what_the_server_knows() {
    let mut h = configured();
    let user = h.open_file_and_drain("file:///user.ttl", "turtle", USER_DOC);
    let cache = h.cache_url(ONTO_URL);

    // Goto-definition took the user to the cached file and they edit it.
    let opened = h.editor_open(cache.as_str(), "turtle", EDITED);
    h.drain_tasks();

    let diags = h.run_diagnostics();
    assert!(
        diags
            .iter()
            .any(|(u, d)| u == &cache && d.message.contains("copy cached by swls")),
        "opening a cached file shows a notice: {diags:#?}"
    );

    let completions = h.completions(&user, 1, 5);
    h.assert_completions(&completions)
        .contains_label("fict:Widget")
        .does_not_contain_label("fict:Hacked");

    // Closing it restores the server's copy and clears its diagnostics.
    h.editor_close(&opened);
    assert_eq!(h.source(&opened), ONTO);
    let diags = h.run_diagnostics();
    assert!(
        diags.iter().all(|(u, _)| u != &cache),
        "closed file keeps no diagnostics: {diags:#?}"
    );
    let completions = h.completions(&user, 1, 5);
    h.assert_completions(&completions)
        .contains_label("fict:Widget");
}

#[test_log::test]
fn closing_a_user_document_clears_its_diagnostics() {
    let mut h = LspHarness::new();
    let broken = h.editor_open("file:///broken.ttl", "turtle", "@prefix x: <a> .\n<> x:");
    h.drain_tasks();
    assert!(h
        .run_diagnostics()
        .iter()
        .any(|(u, _)| u.as_str() == "file:///broken.ttl"));

    h.editor_close(&broken);
    let diags = h.run_diagnostics();
    assert!(
        diags
            .iter()
            .all(|(u, _)| u.as_str() != "file:///broken.ttl"),
        "{diags:#?}"
    );
}

// ─── Freshness policy ─────────────────────────────────────────────────────────

fn test_fs() -> Fs {
    Fs(std::sync::Arc::new(swls_test_utils::TestFs::new()))
}

#[test]
fn fresh_cache_entry_is_used_without_fetching() {
    futures::executor::block_on(async {
        let fs = test_fs();
        let url = fs.0.virtual_url("lov/fict.ttl").unwrap();

        let first = cached(&fs, &url, || async { Some(String::from("v1")) }).await;
        assert_eq!(first.as_deref(), Some("v1"));

        let second = cached(&fs, &url, || async {
            panic!("a fresh entry must not be refetched")
        })
        .await;
        assert_eq!(second.as_deref(), Some("v1"));
    });
}

fn age(url: &swls_core::lsp_types::Url, by: Duration) {
    let path = url.to_file_path().unwrap();
    let file = std::fs::File::options().write(true).open(path).unwrap();
    file.set_modified(SystemTime::now() - by).unwrap();
}

#[test]
fn stale_cache_entry_is_refreshed() {
    futures::executor::block_on(async {
        let fs = test_fs();
        let url = fs.0.virtual_url("lov/fict.ttl").unwrap();
        cached(&fs, &url, || async { Some(String::from("v1")) }).await;
        age(&url, CACHE_TTL + Duration::from_secs(60));

        let refreshed = cached(&fs, &url, || async { Some(String::from("v2")) }).await;
        assert_eq!(refreshed.as_deref(), Some("v2"));
        assert_eq!(fs.0.read_file(&url).await.as_deref(), Some("v2"));
    });
}

#[test]
fn stale_cache_entry_is_used_when_offline() {
    futures::executor::block_on(async {
        let fs = test_fs();
        let url = fs.0.virtual_url("lov/fict.ttl").unwrap();
        cached(&fs, &url, || async { Some(String::from("v1")) }).await;
        age(&url, CACHE_TTL + Duration::from_secs(60));

        let offline = cached(&fs, &url, || async { None }).await;
        assert_eq!(offline.as_deref(), Some("v1"));
    });
}
