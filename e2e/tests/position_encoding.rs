//! E2E tests pinning the LSP **position encoding** to UTF-16.
//!
//! The LSP spec defaults `positionEncoding` to UTF-16: a `Position.character` is
//! a count of UTF-16 code units from the start of the line, *not* a byte offset
//! and *not* a Unicode scalar (char) count.
//!
//! Historically SWLS emitted **byte** columns (`offset_to_position` computed
//! `column = byte_offset - line_start_byte`). That is correct only for ASCII; on
//! any line containing a multi-byte character the reported column is wrong, which
//! manifests as misplaced diagnostics, hovers, rename ranges, etc.
//!
//! These tests observe the column through diagnostic ranges (the easiest public
//! observation point) and assert it equals the UTF-16 code-unit count.

use swls_core::lsp_types::DiagnosticSeverity;
use swls_e2e_tests::LspHarness;

/// Find the undefined-prefix ERROR diagnostic for `prefix` and return its start
/// position `(line, character)`.
fn undefined_prefix_start(
    diags: &[(swls_core::lsp_types::Url, swls_core::lsp_types::Diagnostic)],
    file_url: &str,
    prefix: &str,
) -> (u32, u32) {
    let needle = format!("\"{}\"", prefix);
    let (_, diag) = diags
        .iter()
        .find(|(url, d)| {
            url.as_str() == file_url
                && d.severity == Some(DiagnosticSeverity::ERROR)
                && d.message.contains(&needle)
        })
        .unwrap_or_else(|| {
            panic!(
                "expected an undefined-prefix ERROR for {:?}, got: {:?}",
                prefix,
                diags.iter().map(|(_, d)| &d.message).collect::<Vec<_>>()
            )
        });
    (diag.range.start.line, diag.range.start.character)
}

// ─── BMP multi-byte char: byte column ≠ UTF-16 column ─────────────────────────

/// `é` (U+00E9) is 2 bytes in UTF-8 but a single UTF-16 code unit. A token after
/// it on the same line must be reported at its UTF-16 column, not its byte column.
#[test_log::test]
fn diagnostic_column_is_utf16_after_two_byte_char() {
    let mut h = LspHarness::new();
    // line 0: prefix decl   line 1: `é` literal then an UNDECLARED prefix `bad`
    let line1 = "ex:s ex:p \"é\" , bad:obj .";
    let src = format!("@prefix ex: <http://example.org/> .\n{line1}");
    let file = h.open_file("file:///utf16_bmp.ttl", "turtle", &src);

    let diags = h.run_diagnostics();
    let (line, character) = undefined_prefix_start(&diags, &file.url, "bad");

    let byte_off = line1.find("bad").unwrap();
    let expected_utf16 = line1[..byte_off].encode_utf16().count() as u32;
    let byte_col = byte_off as u32; // what the old byte-based code returned

    assert_eq!(line, 1, "diagnostic should be on line 1");
    assert_ne!(
        expected_utf16, byte_col,
        "test is only meaningful if the byte column differs from the UTF-16 column"
    );
    assert_eq!(
        character, expected_utf16,
        "Position.character must be the UTF-16 column ({expected_utf16}), not the byte column ({byte_col})"
    );
}

// ─── Astral char (surrogate pair): UTF-16 ≠ char count ≠ byte count ───────────

/// `😀` (U+1F600) is 4 bytes in UTF-8, **2** UTF-16 code units, and **1** Unicode
/// scalar. A token after it pins the encoding to UTF-16 specifically: the expected
/// column differs from the byte column (regression we're fixing) *and* from a
/// naive `chars().count()` implementation (regression we must not introduce).
#[test_log::test]
fn diagnostic_column_is_utf16_after_surrogate_pair() {
    let mut h = LspHarness::new();
    let line1 = "ex:s ex:p \"😀\" , bad:obj .";
    let src = format!("@prefix ex: <http://example.org/> .\n{line1}");
    let file = h.open_file("file:///utf16_astral.ttl", "turtle", &src);

    let diags = h.run_diagnostics();
    let (line, character) = undefined_prefix_start(&diags, &file.url, "bad");

    let byte_off = line1.find("bad").unwrap();
    let expected_utf16 = line1[..byte_off].encode_utf16().count() as u32;
    let char_count = line1[..byte_off].chars().count() as u32;
    let byte_col = byte_off as u32;

    // All three must be distinct so the assertion truly pins UTF-16.
    assert_eq!(line, 1, "diagnostic should be on line 1");
    assert!(
        expected_utf16 != byte_col && expected_utf16 != char_count,
        "expected UTF-16 ({expected_utf16}) to differ from byte ({byte_col}) and char ({char_count}) counts"
    );
    assert_eq!(
        character, expected_utf16,
        "Position.character must be the UTF-16 column ({expected_utf16}); \
         got {character} (byte col = {byte_col}, char count = {char_count})"
    );
}
