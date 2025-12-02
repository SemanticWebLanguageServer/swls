use std::{
    borrow::Borrow,
    ops::{Deref, DerefMut},
};

use ropey::Rope;

use crate::{
    lsp_types::{Location, Position, Range},
    Label,
};

// pub mod cache;
pub mod fs;
/// Commonly used RDF prefixes
pub mod ns;
pub mod token;
pub mod triple;

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

#[derive(Debug, Clone)]
pub struct Spanned<T>(pub T, pub std::ops::Range<usize>);
impl<T> Default for Spanned<T>
where
    T: Default,
{
    fn default() -> Self {
        Self(T::default(), 0..1)
    }
}

impl<T> Spanned<T> {
    pub fn map<O>(self, f: impl Fn(T) -> O) -> Spanned<O> {
        let v = f(self.0);
        Spanned(v, self.1)
    }
    pub fn map_ref<'a, O: 'a>(&'a self, f: impl Fn(&'a T) -> O) -> Spanned<O> {
        let v = f(&self.0);
        Spanned(v, self.1.clone())
    }
    pub fn as_ref<'a>(&'a self) -> Spanned<&'a T> {
        Spanned(&self.0, self.1.clone())
    }

    pub fn try_map_ref<'a, O>(&'a self, f: impl FnOnce(&'a T) -> Option<O>) -> Option<Spanned<O>> {
        if let Some(v) = f(&self.0) {
            Some(Spanned(v, self.1.clone()))
        } else {
            None
        }
    }
    pub fn try_map<O>(self, f: impl FnOnce(T) -> Option<O>) -> Option<Spanned<O>> {
        if let Some(v) = f(self.0) {
            Some(Spanned(v, self.1))
        } else {
            None
        }
    }
}
impl<T> Spanned<Option<T>> {
    pub fn transpose(self) -> Option<Spanned<T>> {
        self.0.map(|inner| Spanned(inner, self.1))
    }
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}
impl<T: std::hash::Hash> std::hash::Hash for Spanned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}
impl<T: PartialEq> Eq for Spanned<T> {}

pub fn spanned<T>(t: T, span: std::ops::Range<usize>) -> Spanned<T> {
    Spanned(t, span)
}

impl Borrow<str> for Spanned<String> {
    #[inline]
    fn borrow(&self) -> &str {
        &self[..]
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> Spanned<T> {
    pub fn into_value(self) -> T {
        self.0
    }
    pub fn into_span(self) -> std::ops::Range<usize> {
        self.1
    }
    pub fn value(&self) -> &T {
        &self.0
    }
    pub fn value_mut(&mut self) -> &mut T {
        &mut self.0
    }
    pub fn span(&self) -> &std::ops::Range<usize> {
        &self.1
    }
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
