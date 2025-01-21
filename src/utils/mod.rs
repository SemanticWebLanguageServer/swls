mod loader;
pub use loader::*;
pub mod turtle;

mod fetch;
pub use fetch::*;

use crate::lsp_types::{Position, Range};
use ropey::Rope;

pub fn range_to_range(range: &std::ops::Range<usize>, rope: &Rope) -> Option<Range> {
    let start = offset_to_position(range.start, rope)?;
    let end = offset_to_position(range.end, rope)?;
    Range::new(start, end).into()
}

pub fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char;
    Some(Position::new(line as u32, column as u32))
}
pub fn position_to_offset(position: Position, rope: &Rope) -> Option<usize> {
    let line_offset = rope.try_line_to_char(position.line as usize).ok()?;
    let line_length = rope.line(position.line as usize).len_chars();

    if (position.character as usize) < line_length {
        Some(line_offset + position.character as usize)
    } else {
        None
    }
}
pub fn offsets_to_range(start: usize, end: usize, rope: &Rope) -> Option<Range> {
    let start = offset_to_position(start, rope)?;
    let end = offset_to_position(end, rope)?;
    Some(Range { start, end })
}
