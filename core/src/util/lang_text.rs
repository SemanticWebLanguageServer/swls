//! Human-readable literals in several languages.
//!
//! Labels, comments, titles and descriptions often come in several languages
//! (`"Name"@en`, `"Naam"@nl`). The server does not know the editor's language,
//! so everywhere text is shown, one language is chosen: untagged text first,
//! then English (`en`, `en-GB`, …), then the first language found.

use std::borrow::Cow;

use crate::util::triple::{MyTerm, TermContext};

/// Language tag of a literal term, if any.
pub fn term_language<'a>(term: &'a MyTerm<'_>) -> Option<&'a str> {
    match &term.context {
        TermContext::LangTag(tag) => Some(tag.as_ref()),
        _ => None,
    }
}

fn is_english(tag: &str) -> bool {
    let tag = tag.to_ascii_lowercase();
    tag == "en" || tag.starts_with("en-")
}

/// The language to show among `languages` (`None` = untagged).
fn preferred<'a>(
    languages: impl Iterator<Item = Option<&'a str>> + Clone,
) -> Option<Option<&'a str>> {
    if languages.clone().any(|l| l.is_none()) {
        return Some(None);
    }
    if let Some(en) = languages
        .clone()
        .flatten()
        .find(|l| l.eq_ignore_ascii_case("en"))
    {
        return Some(Some(en));
    }
    if let Some(en) = languages.clone().flatten().find(|l| is_english(l)) {
        return Some(Some(en));
    }
    languages.into_iter().next()
}

/// Whether `tag` belongs to the language `chosen` (case-insensitive).
fn same_language(tag: Option<&str>, chosen: Option<&str>) -> bool {
    match (tag, chosen) {
        (None, None) => true,
        (Some(a), Some(b)) => a.eq_ignore_ascii_case(b),
        _ => false,
    }
}

/// Literals with their language tags. Insertion order is kept and duplicates
/// are dropped.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LangTexts(Vec<(String, Option<String>)>);

impl LangTexts {
    pub fn insert(&mut self, value: impl Into<String>, language: Option<&str>) {
        let value = value.into();
        let language = language.map(str::to_string);
        if !self.0.iter().any(|(v, l)| *v == value && *l == language) {
            self.0.push((value, language));
        }
    }

    /// Add a literal term (other terms are ignored).
    pub fn insert_term(&mut self, term: &MyTerm<'_>) {
        if term.ty == Some(sophia_api::term::TermKind::Literal) {
            self.insert(term.value.to_string(), term_language(term));
        }
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Every text in the preferred language, in insertion order.
    pub fn preferred(&self) -> Vec<&str> {
        let Some(chosen) = preferred(self.0.iter().map(|(_, l)| l.as_deref())) else {
            return Vec::new();
        };
        self.0
            .iter()
            .filter(|(_, l)| same_language(l.as_deref(), chosen))
            .map(|(v, _)| v.as_str())
            .collect()
    }

    /// The first text in the preferred language.
    pub fn first(&self) -> Option<&str> {
        self.preferred().into_iter().next()
    }
}

impl<'a> FromIterator<&'a MyTerm<'a>> for LangTexts {
    fn from_iter<T: IntoIterator<Item = &'a MyTerm<'a>>>(iter: T) -> Self {
        let mut out = LangTexts::default();
        for t in iter {
            out.insert_term(t);
        }
        out
    }
}

/// Collapse runs of whitespace (multi-line `"""…"""` literals) to single spaces.
pub fn normalize_ws(s: &str) -> Cow<'_, str> {
    if s.split_whitespace().count() <= 1 && !s.contains(char::is_whitespace) {
        return Cow::Borrowed(s);
    }
    Cow::Owned(s.split_whitespace().collect::<Vec<_>>().join(" "))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn texts(items: &[(&str, Option<&str>)]) -> LangTexts {
        let mut t = LangTexts::default();
        for (v, l) in items {
            t.insert(*v, *l);
        }
        t
    }

    #[test]
    fn untagged_wins() {
        let t = texts(&[("Naam", Some("nl")), ("Name", None), ("Name", Some("en"))]);
        assert_eq!(t.preferred(), vec!["Name"]);
    }

    #[test]
    fn english_then_regional_english() {
        let t = texts(&[
            ("Naam", Some("nl")),
            ("Colour", Some("en-GB")),
            ("Name", Some("EN")),
        ]);
        assert_eq!(t.preferred(), vec!["Name"]);
        let t = texts(&[("Naam", Some("nl")), ("Colour", Some("en-GB"))]);
        assert_eq!(t.preferred(), vec!["Colour"]);
    }

    #[test]
    fn otherwise_the_first_language() {
        let t = texts(&[
            ("Naam", Some("nl")),
            ("Nom", Some("fr")),
            ("Benaming", Some("nl")),
        ]);
        assert_eq!(t.preferred(), vec!["Naam", "Benaming"]);
        assert_eq!(t.first(), Some("Naam"));
        assert!(LangTexts::default().preferred().is_empty());
    }
}
