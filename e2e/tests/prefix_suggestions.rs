//! Prefix suggestions: declared prefixes always, undeclared ones only for a typed word and
//! capped, so an empty position is not flooded with every known prefix.

use swls_core::{lsp_types::CompletionItemKind, systems::prefix::MAX_UNDECLARED_PREFIXES};
use swls_e2e_tests::LspHarness;

fn prefix_labels(src: &str, line: u32, col: u32) -> Vec<String> {
    let mut h = LspHarness::new();
    let file = h.open_file_and_drain("file:///data.ttl", "turtle", src);
    h.completions(&file, line, col)
        .into_iter()
        .filter(|c| c.kind == CompletionItemKind::MODULE)
        .map(|c| c.label)
        .collect()
}

#[test_log::test]
fn empty_word_offers_only_declared_prefixes() {
    let labels = prefix_labels("@prefix ex: <http://ex.test/>.\n<x> a ex:C ;\n  ", 2, 2);
    assert_eq!(labels, ["ex"], "{labels:?}");
}

#[test_log::test]
fn typed_word_offers_matching_undeclared_prefixes_capped() {
    let labels = prefix_labels("<x> a ", 0, 6);
    assert!(labels.is_empty(), "nothing typed: {labels:?}");

    let labels = prefix_labels("<x> a fo", 0, 8);
    assert!(labels.contains(&"foaf".to_string()), "{labels:?}");
    assert!(labels.iter().all(|l| l.starts_with("fo")), "{labels:?}");

    // `a` matches hundreds of known prefixes.
    let labels = prefix_labels("<x> a a", 0, 7);
    assert_eq!(labels.len(), MAX_UNDECLARED_PREFIXES, "{labels:?}");
}

#[test_log::test]
fn a_declared_prefix_is_suggested_once() {
    let labels = prefix_labels(
        "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n<x> a fo",
        1,
        8,
    );
    assert_eq!(
        labels.iter().filter(|l| l.as_str() == "foaf").count(),
        1,
        "{labels:?}"
    );
}
