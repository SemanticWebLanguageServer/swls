//! E2E tests for the Turtle "extract blank node" code action.
//!
//! When the cursor is inside an anonymous blank node `[ … ]`, the server offers a
//! refactor that replaces it with a labelled blank node `_:bN` and appends a
//! standalone statement defining that node.

use swls_core::lsp_types::{CodeActionKind, Position, TextEdit};
use swls_e2e_tests::LspHarness;

/// Apply a set of single-file `TextEdit`s to `src`, returning the new text.
fn apply_edits(src: &str, edits: &[TextEdit]) -> String {
    fn pos_to_off(s: &str, p: Position) -> usize {
        let mut off = 0usize;
        for (i, line) in s.split_inclusive('\n').enumerate() {
            if i as u32 == p.line {
                return off + p.character as usize;
            }
            off += line.len();
        }
        off + p.character as usize
    }

    // Apply right-to-left so earlier offsets stay valid.
    let mut edits: Vec<&TextEdit> = edits.iter().collect();
    edits.sort_by_key(|e| std::cmp::Reverse(pos_to_off(src, e.range.start)));

    let mut out = src.to_string();
    for e in edits {
        let start = pos_to_off(src, e.range.start);
        let end = pos_to_off(src, e.range.end);
        out.replace_range(start..end, &e.new_text);
    }
    out
}

const PRELUDE: &str = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n";

#[test_log::test]
fn extract_blank_node_offered_and_applies() {
    let mut h = LspHarness::new();
    // Line 1: "<#s> foaf:knows [ foaf:name \"Alice\" ] ."
    let src = format!("{PRELUDE}<#s> foaf:knows [ foaf:name \"Alice\" ] .");
    let file = h.open_file("file:///bnode.ttl", "turtle", &src);
    h.drain_tasks();

    // Cursor inside the blank node (over "foaf:name").
    let actions = h.code_actions_at(&file, 1, 20);
    let extract = actions
        .iter()
        .find(|a| a.kind == Some(CodeActionKind::REFACTOR_EXTRACT))
        .expect("expected an extract-blank-node code action");

    assert!(extract.title.contains("Extract blank node"));

    let edits = extract
        .edit
        .as_ref()
        .and_then(|w| w.changes.as_ref())
        .and_then(|c| c.values().next())
        .expect("expected workspace edits");

    let result = apply_edits(&src, edits);
    assert!(
        result.contains("foaf:knows _:b0 ."),
        "inline blank node should be replaced with _:b0, got:\n{result}"
    );
    assert!(
        result.contains("_:b0 foaf:name \"Alice\" ."),
        "extracted statement should be appended, got:\n{result}"
    );
    assert!(
        !result.contains('['),
        "no inline blank node should remain, got:\n{result}"
    );
}

#[test_log::test]
fn no_extract_when_cursor_outside_blank_node() {
    let mut h = LspHarness::new();
    let src = format!("{PRELUDE}<#s> foaf:knows [ foaf:name \"Alice\" ] .");
    let file = h.open_file("file:///bnode_outside.ttl", "turtle", &src);
    h.drain_tasks();

    // Cursor on the subject "<#s>" (column 1), not inside the blank node.
    let actions = h.code_actions_at(&file, 1, 1);
    assert!(
        !actions
            .iter()
            .any(|a| a.kind == Some(CodeActionKind::REFACTOR_EXTRACT)),
        "extract should not be offered outside a blank node"
    );
}

#[test_log::test]
fn no_extract_for_empty_blank_node() {
    let mut h = LspHarness::new();
    let src = format!("{PRELUDE}<#s> foaf:knows [] .");
    let file = h.open_file("file:///bnode_empty.ttl", "turtle", &src);
    h.drain_tasks();

    // Cursor on the "[]" (column 16/17).
    let actions = h.code_actions_at(&file, 1, 16);
    assert!(
        !actions
            .iter()
            .any(|a| a.kind == Some(CodeActionKind::REFACTOR_EXTRACT)),
        "extract should not be offered for an empty blank node"
    );
}

#[test_log::test]
fn extract_innermost_nested_blank_node() {
    let mut h = LspHarness::new();
    // Nested: [ foaf:knows [ foaf:name "Bob" ] ]
    let src = format!("{PRELUDE}<#s> foaf:knows [ foaf:knows [ foaf:name \"Bob\" ] ] .");
    let file = h.open_file("file:///bnode_nested.ttl", "turtle", &src);
    h.drain_tasks();

    // Position over the inner "foaf:name" (well inside the inner brackets).
    // Line 1: "<#s> foaf:knows [ foaf:knows [ foaf:name \"Bob\" ] ] ."
    //          0         1         2         3
    //          0123456789012345678901234567890123456789
    // inner "foaf:name" starts around column 31.
    let actions = h.code_actions_at(&file, 1, 33);
    let extract = actions
        .iter()
        .find(|a| a.kind == Some(CodeActionKind::REFACTOR_EXTRACT))
        .expect("expected extract action for inner blank node");

    let edits = extract
        .edit
        .as_ref()
        .and_then(|w| w.changes.as_ref())
        .and_then(|c| c.values().next())
        .unwrap();

    let result = apply_edits(&src, edits);
    // The inner node is extracted; the outer blank node remains inline and now
    // references the new label.
    assert!(
        result.contains("foaf:knows _:b0 ]"),
        "inner node should be replaced inside the outer blank node, got:\n{result}"
    );
    assert!(
        result.contains("_:b0 foaf:name \"Bob\" ."),
        "inner content should be extracted, got:\n{result}"
    );
}

// ─── Cross-language reuse (TriG / SPARQL) ─────────────────────────────────────

#[test_log::test]
fn extract_blank_node_works_in_trig() {
    let mut h = LspHarness::new();
    // TriG is a Turtle superset; the same extraction applies.
    let src = format!("{PRELUDE}<#s> foaf:knows [ foaf:name \"Alice\" ] .");
    let file = h.open_file("file:///bnode.trig", "trig", &src);
    h.drain_tasks();

    let actions = h.code_actions_at(&file, 1, 20);
    let extract = actions
        .iter()
        .find(|a| a.kind == Some(CodeActionKind::REFACTOR_EXTRACT))
        .expect("expected extract action in TriG");

    let edits = extract
        .edit
        .as_ref()
        .and_then(|w| w.changes.as_ref())
        .and_then(|c| c.values().next())
        .unwrap();
    let result = apply_edits(&src, edits);
    assert!(result.contains("foaf:knows _:b0 ."), "got:\n{result}");
    assert!(result.contains("_:b0 foaf:name \"Alice\" ."), "got:\n{result}");
}

#[test_log::test]
fn extract_blank_node_works_in_sparql() {
    let mut h = LspHarness::new();
    // SPARQL blank node property list inside a WHERE clause.
    let src = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n\
               SELECT * WHERE { ?s foaf:knows [ foaf:name \"Alice\" ] }";
    let file = h.open_file("file:///bnode.rq", "sparql", src);
    h.drain_tasks();

    // Cursor over the inner "foaf:name" (column ~36 on line 1).
    let actions = h.code_actions_at(&file, 1, 36);
    let extract = actions
        .iter()
        .find(|a| a.kind == Some(CodeActionKind::REFACTOR_EXTRACT))
        .expect("expected extract action in SPARQL");

    let edits = extract
        .edit
        .as_ref()
        .and_then(|w| w.changes.as_ref())
        .and_then(|c| c.values().next())
        .unwrap();
    let result = apply_edits(src, edits);
    assert!(result.contains("foaf:knows _:b0 "), "got:\n{result}");
    assert!(result.contains("_:b0 foaf:name \"Alice\" ."), "got:\n{result}");
}

// ─── Inline named blank node (inverse of extract) ─────────────────────────────

#[test_log::test]
fn inline_blank_node_offered_and_applies() {
    let mut h = LspHarness::new();
    // A labelled blank node defined on its own line and referenced once.
    let src = format!(
        "{PRELUDE}<#s> foaf:knows _:b0 .\n_:b0 foaf:name \"Alice\" .\n"
    );
    let file = h.open_file("file:///inline.ttl", "turtle", &src);
    h.drain_tasks();

    // Cursor on the reference "_:b0" in the first statement (column ~16).
    let actions = h.code_actions_at(&file, 1, 17);
    let inline = actions
        .iter()
        .find(|a| a.kind == Some(CodeActionKind::REFACTOR_INLINE))
        .expect("expected an inline-blank-node code action");
    assert!(inline.title.contains("Inline"));

    let edits = inline
        .edit
        .as_ref()
        .and_then(|w| w.changes.as_ref())
        .and_then(|c| c.values().next())
        .expect("expected workspace edits");

    let result = apply_edits(&src, edits);
    assert!(
        result.contains("foaf:knows [ foaf:name \"Alice\" ] ."),
        "reference should be inlined, got:\n{result}"
    );
    assert!(
        !result.contains("_:b0"),
        "no labelled blank node should remain, got:\n{result}"
    );
}

#[test_log::test]
fn inline_offered_when_cursor_on_definition() {
    let mut h = LspHarness::new();
    let src = format!(
        "{PRELUDE}<#s> foaf:knows _:b0 .\n_:b0 foaf:name \"Alice\" .\n"
    );
    let file = h.open_file("file:///inline_def.ttl", "turtle", &src);
    h.drain_tasks();

    // Cursor on the subject "_:b0" of the definition statement (line 2).
    let actions = h.code_actions_at(&file, 2, 1);
    let inline = actions
        .iter()
        .find(|a| a.kind == Some(CodeActionKind::REFACTOR_INLINE))
        .expect("expected inline action from the definition site");

    let edits = inline
        .edit
        .as_ref()
        .and_then(|w| w.changes.as_ref())
        .and_then(|c| c.values().next())
        .unwrap();
    let result = apply_edits(&src, edits);
    assert!(
        result.contains("foaf:knows [ foaf:name \"Alice\" ] ."),
        "got:\n{result}"
    );
    assert!(!result.contains("_:b0"), "got:\n{result}");
}

#[test_log::test]
fn no_inline_when_referenced_multiple_times() {
    let mut h = LspHarness::new();
    // _:b0 referenced twice → cannot be a single inline blank node.
    let src = format!(
        "{PRELUDE}<#s> foaf:knows _:b0 .\n<#t> foaf:knows _:b0 .\n_:b0 foaf:name \"Alice\" .\n"
    );
    let file = h.open_file("file:///inline_multi.ttl", "turtle", &src);
    h.drain_tasks();

    let actions = h.code_actions_at(&file, 1, 17);
    assert!(
        !actions
            .iter()
            .any(|a| a.kind == Some(CodeActionKind::REFACTOR_INLINE)),
        "inline should not be offered when the node is referenced more than once"
    );
}

#[test_log::test]
fn inline_blank_node_works_in_trig() {
    let mut h = LspHarness::new();
    let src = format!(
        "{PRELUDE}<#s> foaf:knows _:b0 .\n_:b0 foaf:name \"Alice\" .\n"
    );
    let file = h.open_file("file:///inline.trig", "trig", &src);
    h.drain_tasks();

    let actions = h.code_actions_at(&file, 1, 17);
    let inline = actions
        .iter()
        .find(|a| a.kind == Some(CodeActionKind::REFACTOR_INLINE))
        .expect("expected inline action in TriG");

    let edits = inline
        .edit
        .as_ref()
        .and_then(|w| w.changes.as_ref())
        .and_then(|c| c.values().next())
        .unwrap();
    let result = apply_edits(&src, edits);
    assert!(
        result.contains("foaf:knows [ foaf:name \"Alice\" ] ."),
        "got:\n{result}"
    );
    assert!(!result.contains("_:b0"), "got:\n{result}");
}
