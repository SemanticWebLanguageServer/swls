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

pub use rdf_parsers::{spanned, Spanned};

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
    let line = rope.try_byte_to_line(offset).ok()?;
    let line_start = rope.line_to_byte(line);
    let column = offset - line_start;
    Some(Position::new(line as u32, column as u32))
}
pub fn position_to_offset(position: Position, rope: &Rope) -> Option<usize> {
    let line_start = rope.try_line_to_byte(position.line as usize).ok()?;
    let line_length = rope.get_line(position.line as usize)?.len_bytes();

    // Allow cursor at exactly line_length (end of line / end of file). This is a
    // valid LSP position that editors send when the cursor is after the last character.
    if (position.character as usize) <= line_length {
        Some(line_start + position.character as usize)
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

// ── IRI resolution ────────────────────────────────────────────────────────────

/// Resolve `relative` against `base` using RFC 3986 path resolution, without
/// any IRI validation.  This intentionally accepts non-standard / "invalid"
/// IRIs so that tokens in the source document can be round-tripped back to the
/// triple store without percent-encoding transformations.
pub fn resolve_iri(base: &str, relative: &str) -> String {
    // Already absolute (has a scheme colon before any slash or query char).
    if looks_absolute(relative) {
        return relative.to_string();
    }
    // Protocol-relative  //authority/path
    if relative.starts_with("//") {
        let scheme = base.split("://").next().unwrap_or("http");
        return format!("{}:{}", scheme, relative);
    }
    // Root-relative  /path
    if relative.starts_with('/') {
        let authority_end = base
            .find("://")
            .and_then(|i| base[i + 3..].find('/').map(|j| i + 3 + j))
            .unwrap_or(base.len());
        return format!("{}{}", &base[..authority_end], relative);
    }
    // Fragment-only or empty  #frag  or  ""
    if relative.is_empty() || relative.starts_with('#') {
        let base_no_frag = base.split('#').next().unwrap_or(base);
        return format!("{}{}", base_no_frag, relative);
    }
    // Relative path — merge with the base's "directory"
    let base_dir = match base.rfind('/') {
        Some(i) => &base[..=i],
        None => "",
    };
    let merged = format!("{}{}", base_dir, relative);
    normalize_path_segments(&merged)
}

/// Heuristic: does `s` carry its own scheme?  We look for `<scheme>:` where
/// scheme starts with a letter and contains only [A-Za-z0-9+\-.].
fn looks_absolute(s: &str) -> bool {
    let bytes = s.as_bytes();
    if bytes.is_empty() || !bytes[0].is_ascii_alphabetic() {
        return false;
    }
    for (i, &b) in bytes.iter().enumerate() {
        if b == b':' {
            return i > 0;
        }
        if !matches!(b, b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'+' | b'-' | b'.') {
            return false;
        }
    }
    false
}

/// Remove `.` and `..` segments from a merged path string, preserving any
/// authority prefix (`scheme://host`).
fn normalize_path_segments(path: &str) -> String {
    // Split off a leading scheme://authority so we don't mangle it.
    let (prefix, rest) = if let Some(i) = path.find("://") {
        let after = &path[i + 3..];
        let slash = after.find('/').map(|j| i + 3 + j).unwrap_or(path.len());
        (&path[..slash], &path[slash..])
    } else {
        ("", path)
    };

    let mut stack: Vec<&str> = Vec::new();
    for seg in rest.split('/') {
        match seg {
            "." => {}
            ".." => {
                stack.pop();
            }
            s => stack.push(s),
        }
    }
    // If the original path ended with /. or /.. keep a trailing slash.
    let trailing = if rest.ends_with("/..") || rest.ends_with("/.") {
        "/"
    } else {
        ""
    };
    format!("{}/{}{}", prefix, stack.join("/"), trailing)
}
