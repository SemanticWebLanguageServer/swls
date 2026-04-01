use ropey::Rope;

use crate::{
    lsp_types::{Location, Position, Range},
    Label,
};

pub mod fs;
/// Commonly used RDF prefixes
pub mod ns;
pub mod token;
pub mod triple;

pub use turtle::{spanned, Spanned};

// /// Maps http:// and https:// urls to virtual:// urls
// /// This enables the editor to show them
// pub fn make_virtual_url(url: &str, prefix: &str) -> Option<Url> {
//     if !url.starts_with("http") {
//         return None;
//     }
//
//     let url = format!("virtual://prefix/{}.ttl", prefix);
//
//     crate::lsp_types::Url::parse(&url).ok()
// }

pub fn range_to_range(range: &std::ops::Range<usize>, rope: &Rope) -> Option<Range> {
    let start = offset_to_position(range.start, rope)?;
    let end = offset_to_position(range.end, rope)?;
    Range::new(start, end).into()
}

pub fn lsp_range_to_range(
    range: &crate::lsp_types::Range,
    rope: &Rope,
) -> Option<std::ops::Range<usize>> {
    if range.start.line as usize >= rope.len_lines() || range.end.line as usize >= rope.len_lines()
    {
        return None;
    }

    let start = rope.line_to_byte(range.start.line as usize) + range.start.character as usize;
    let end = rope.line_to_byte(range.end.line as usize) + range.end.character as usize;

    Some(start..end)
}

pub fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char;
    Some(Position::new(line as u32, column as u32))
}
pub fn position_to_offset(position: Position, rope: &Rope) -> Option<usize> {
    let line_offset = rope.try_line_to_char(position.line as usize).ok()?;
    let line_length = rope.get_line(position.line as usize)?.len_chars();

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

pub fn token_to_location(
    token: &std::ops::Range<usize>,
    label: &Label,
    rope: &Rope,
) -> Option<Location> {
    let range = range_to_range(token, rope)?;
    Some(Location {
        range,
        uri: label.0.clone(),
    })
}
