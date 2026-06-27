//! Minimal text indexing for LSP position math.
//!
//! Replaces our previous use of [`ropey`]. We never actually used ropey for what
//! ropey is *for* — incremental edits and cheap snapshots — because the server
//! uses full-document sync and rebuilds the buffer from a `String` on every
//! change. The only thing we used was index conversion, and ropey hid the one
//! decision that actually matters for correctness: **which encoding a column is
//! measured in**.
//!
//! [`LineIndex`] makes that explicit. It owns the source text plus a table of
//! line-start byte offsets, and every conversion takes a [`PositionEncoding`] so
//! the byte/UTF-16 choice is visible at the call site instead of buried in a
//! dependency.
//!
//! ## Encodings
//!
//! An LSP `Position.character` is a count *from the start of its line*. The unit
//! depends on the negotiated [`PositionEncoding`]:
//!
//! * [`PositionEncoding::Utf16`] — UTF-16 code units. This is the LSP **default**
//!   and what clients assume unless the server negotiates otherwise.
//! * [`PositionEncoding::Utf8`] — bytes. Cheapest, and what this codebase used to
//!   emit unconditionally (correct only for ASCII).
//!
//! Internally everything is a UTF-8 **byte** offset; encodings only ever affect
//! the in-line column number.

use crate::lsp_types::Position;
use std::ops::Range;

/// The unit in which an LSP `Position.character` column is measured.
///
/// Defaults to [`Utf16`](PositionEncoding::Utf16), matching the LSP spec default.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum PositionEncoding {
    /// UTF-16 code units (LSP default).
    #[default]
    Utf16,
    /// Raw UTF-8 bytes.
    Utf8,
}

/// Source text plus a line-start index, supporting byte ↔ [`Position`] conversion.
///
/// Owns the text so it can serve slices and per-line lookups without the caller
/// also threading a `&str` through. This is the same memory profile as the old
/// `RopeC(Rope)` (which already duplicated the `Source` string).
#[derive(Clone, Debug, Default)]
pub struct LineIndex {
    text: String,
    /// Byte offset of the start of each line. Always begins with `0`; its length
    /// is the number of lines (a trailing `\n` yields a final empty line).
    line_starts: Vec<usize>,
}

impl LineIndex {
    /// Build an index over `text`, scanning once for line breaks (`O(n)`).
    pub fn new(text: impl Into<String>) -> Self {
        let text = text.into();
        let mut line_starts = vec![0];
        line_starts.extend(
            text.bytes()
                .enumerate()
                .filter(|&(_, b)| b == b'\n')
                .map(|(i, _)| i + 1),
        );
        Self { text, line_starts }
    }

    /// The full source text.
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.text
    }

    /// Total length in bytes.
    #[inline]
    pub fn len_bytes(&self) -> usize {
        self.text.len()
    }

    /// Number of lines (a trailing newline counts as starting one more line).
    #[inline]
    pub fn len_lines(&self) -> usize {
        self.line_starts.len()
    }

    /// Byte offset at which `line` starts, or `None` if out of range. A
    /// one-past-the-end `line` is *not* accepted (use [`len_bytes`] for that).
    #[inline]
    pub fn line_start(&self, line: usize) -> Option<usize> {
        self.line_starts.get(line).copied()
    }

    /// Byte range `[start, end)` of `line`, including its trailing newline (if any).
    pub fn line_byte_range(&self, line: usize) -> Option<Range<usize>> {
        let start = self.line_start(line)?;
        let end = self.line_starts.get(line + 1).copied().unwrap_or(self.text.len());
        Some(start..end)
    }

    /// Text of `line`, including its trailing newline (if any).
    pub fn line_str(&self, line: usize) -> Option<&str> {
        let r = self.line_byte_range(line)?;
        self.text.get(r)
    }

    /// Sub-slice by **byte** range. Returns `None` for out-of-bounds ranges or
    /// ranges that do not fall on `char` boundaries.
    #[inline]
    pub fn byte_slice(&self, range: Range<usize>) -> Option<&str> {
        self.text.get(range)
    }

    /// The `char` starting at byte offset `byte`, if `byte` is a char boundary
    /// within the text.
    pub fn char_at_byte(&self, byte: usize) -> Option<char> {
        self.text.get(byte..)?.chars().next()
    }

    /// Convert a byte offset to an LSP [`Position`] in the given encoding.
    ///
    /// Accepts `byte == len_bytes()` (end of document). Returns `None` if `byte`
    /// is out of range or not on a char boundary.
    pub fn byte_to_position(&self, byte: usize, encoding: PositionEncoding) -> Option<Position> {
        if byte > self.text.len() {
            return None;
        }
        let line = match self.line_starts.binary_search(&byte) {
            Ok(line) => line,
            Err(next) => next - 1, // next >= 1 because line_starts[0] == 0 <= byte
        };
        let line_start = self.line_starts[line];
        let segment = self.text.get(line_start..byte)?; // None if not a char boundary
        let character = match encoding {
            PositionEncoding::Utf16 => segment.encode_utf16().count(),
            PositionEncoding::Utf8 => segment.len(),
        };
        Some(Position::new(line as u32, character as u32))
    }

    /// Convert an LSP [`Position`] (interpreted in `encoding`) to a byte offset.
    ///
    /// A `character` past the end of its line is clamped to the line's end (which
    /// keeps the lenient behaviour editors rely on when the cursor sits after the
    /// last character). Returns `None` only if the line itself is out of range.
    pub fn position_to_byte(&self, position: Position, encoding: PositionEncoding) -> Option<usize> {
        let line = position.line as usize;
        let line_range = self.line_byte_range(line)?;
        let line_text = self.text.get(line_range.clone())?;
        let target = position.character as usize;

        let col_bytes = match encoding {
            PositionEncoding::Utf8 => target.min(line_text.len()),
            PositionEncoding::Utf16 => {
                let mut utf16 = 0usize;
                let mut found = None;
                for (byte_off, ch) in line_text.char_indices() {
                    if utf16 >= target {
                        found = Some(byte_off);
                        break;
                    }
                    utf16 += ch.len_utf16();
                }
                found.unwrap_or(line_text.len())
            }
        };
        Some(line_range.start + col_bytes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const U16: PositionEncoding = PositionEncoding::Utf16;
    const U8: PositionEncoding = PositionEncoding::Utf8;

    fn pos(l: u32, c: u32) -> Position {
        Position::new(l, c)
    }

    #[test]
    fn line_counting_matches_ropey_conventions() {
        assert_eq!(LineIndex::new("").len_lines(), 1);
        assert_eq!(LineIndex::new("abc").len_lines(), 1);
        assert_eq!(LineIndex::new("abc\n").len_lines(), 2); // trailing newline → empty last line
        assert_eq!(LineIndex::new("a\nb\nc").len_lines(), 3);
    }

    #[test]
    fn ascii_byte_to_position() {
        let idx = LineIndex::new("ab\ncde");
        assert_eq!(idx.byte_to_position(0, U16), Some(pos(0, 0)));
        assert_eq!(idx.byte_to_position(2, U16), Some(pos(0, 2))); // before '\n'
        assert_eq!(idx.byte_to_position(3, U16), Some(pos(1, 0))); // start of line 1
        assert_eq!(idx.byte_to_position(6, U16), Some(pos(1, 3))); // end of document
        assert_eq!(idx.byte_to_position(7, U16), None); // past end
    }

    #[test]
    fn two_byte_char_column_differs_by_encoding() {
        // 'é' is 2 bytes, 1 UTF-16 unit. "xé" then 'y' at byte 3.
        let idx = LineIndex::new("xéy");
        let y_byte = "xé".len(); // 3
        assert_eq!(idx.byte_to_position(y_byte, U16), Some(pos(0, 2))); // x, é
        assert_eq!(idx.byte_to_position(y_byte, U8), Some(pos(0, 3))); // x, é(2 bytes)
    }

    #[test]
    fn astral_char_is_two_utf16_units_one_char() {
        // '😀' is 4 bytes, 2 UTF-16 units, 1 scalar. 'z' follows.
        let idx = LineIndex::new("😀z");
        let z_byte = "😀".len(); // 4
        assert_eq!(idx.byte_to_position(z_byte, U16), Some(pos(0, 2))); // surrogate pair
        assert_eq!(idx.byte_to_position(z_byte, U8), Some(pos(0, 4)));
    }

    #[test]
    fn position_to_byte_roundtrips_utf16() {
        for text in ["", "abc", "ab\ncde", "xéy\nfoo", "😀z\n€uro", "a\r\nb"] {
            let idx = LineIndex::new(text);
            // Every char boundary should round-trip byte → position → byte.
            for (byte, _) in text.char_indices().chain(std::iter::once((text.len(), ' '))) {
                let p = idx.byte_to_position(byte, U16).unwrap();
                assert_eq!(
                    idx.position_to_byte(p, U16),
                    Some(byte),
                    "roundtrip failed for {text:?} at byte {byte} (pos {p:?})"
                );
            }
        }
    }

    #[test]
    fn position_to_byte_clamps_past_end_of_line() {
        let idx = LineIndex::new("ab\ncd");
        // character way past the line length clamps to end of that line (incl. '\n').
        assert_eq!(idx.position_to_byte(pos(0, 99), U16), Some(3)); // "ab\n" -> byte 3
        assert_eq!(idx.position_to_byte(pos(1, 99), U16), Some(5)); // end of doc
        assert_eq!(idx.position_to_byte(pos(9, 0), U16), None); // line out of range
    }

    #[test]
    fn crlf_line_starts() {
        let idx = LineIndex::new("a\r\nb");
        assert_eq!(idx.len_lines(), 2);
        // '\r' is part of line 0; line 1 starts after '\n'.
        assert_eq!(idx.byte_to_position(3, U16), Some(pos(1, 0)));
        assert_eq!(idx.line_str(0), Some("a\r\n"));
        assert_eq!(idx.line_str(1), Some("b"));
    }

    #[test]
    fn byte_slice_and_char_at_byte() {
        let idx = LineIndex::new("xéy");
        assert_eq!(idx.byte_slice(0..1), Some("x"));
        assert_eq!(idx.byte_slice(0..2), None); // splits 'é'
        assert_eq!(idx.char_at_byte(1), Some('é'));
        assert_eq!(idx.char_at_byte(2), None); // mid-'é'
        assert_eq!(idx.char_at_byte(3), Some('y'));
    }
}
