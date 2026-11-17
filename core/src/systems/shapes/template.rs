//! Completion templates generated from SHACL shapes.
//!
//! At a predicate position, the node shapes that apply to the subject are
//! collected and turned into two snippet completions:
//!
//! - **required properties**: every property with `sh:minCount >= 1`,
//! - **all properties**: every property the shapes declare.
//!
//! Properties the subject already has are left out, so a template can also be
//! used to complete a partially written resource.
//!
//! Which shapes apply follows SHACL targets, evaluated on the document:
//! `sh:targetClass` (including superclasses known from the ontologies, and
//! implicit class targets), `sh:targetSubjectsOf`, `sh:targetObjectsOf` and
//! `sh:targetNode`. The properties of a shape include those of the shapes it
//! refers to with `sh:node` and `sh:and`. Branches of `sh:or` / `sh:xone` are
//! alternatives, so none of them is assumed.
//!
//! Values are placeholders shaped by the constraints of the property:
//! `sh:hasValue` is filled in, `sh:in` becomes a choice, a nested `sh:node`
//! becomes `[ … ]` with that shape's required properties, and `sh:nodeKind` /
//! `sh:datatype` decide between `<…>`, `"…"` and plain placeholders.

use bevy_ecs::prelude::*;

use super::model::*;
use crate::{
    feature::completion::{CompletionRequest, CursorContext, SimpleCompletion},
    lsp_types::{CompletionItemKind, Position, TextEdit},
    prelude::*,
    systems::shapes::visible::{visible_shapes, ShapeDocs},
};

// ─── Rendering ───────────────────────────────────────────────────────────────

/// Escape text for use inside an LSP snippet.
fn escape(text: &str) -> String {
    text.replace('\\', "\\\\")
        .replace('$', "\\$")
        .replace('}', "\\}")
}

/// Escape a choice of an LSP snippet `${1|a,b|}`.
fn escape_choice(text: &str) -> String {
    escape(text).replace(',', "\\,").replace('|', "\\|")
}

struct Renderer<'a> {
    prefixes: &'a Prefixes,
    tabstop: usize,
}

impl Renderer<'_> {
    fn iri(&self, iri: &str) -> String {
        self.prefixes
            .shorten(iri)
            .unwrap_or_else(|| format!("<{}>", iri))
    }

    fn term(&self, term: &Term) -> String {
        match term {
            Term::Iri(iri) => self.iri(iri),
            Term::Literal(l) => format!("\"{}\"", l.replace('"', "\\\"")),
        }
    }

    fn next(&mut self) -> usize {
        self.tabstop += 1;
        self.tabstop
    }

    /// The predicate of `p`: a choice for an alternative path.
    fn predicate(&mut self, p: &Property) -> String {
        if p.is_group() {
            let n = self.next();
            let choices: Vec<_> = p
                .alternatives
                .iter()
                .map(|a| escape_choice(&self.iri(a)))
                .collect();
            format!("${{{}|{}|}}", n, choices.join(","))
        } else {
            escape(&self.iri(&p.path))
        }
    }

    /// `predicate value` for each property, values laid out for `indent`.
    fn lines(&mut self, props: &[Property], indent: &str) -> Vec<String> {
        props
            .iter()
            .map(|p| {
                let predicate = self.predicate(p);
                format!("{} {}", predicate, self.value(p, indent))
            })
            .collect()
    }

    fn placeholder(&mut self, name: &str) -> String {
        let n = self.next();
        format!("${{{}:{}}}", n, escape_choice(name))
    }

    /// The value of `p`, for a property written on a line indented by `indent`.
    fn value(&mut self, p: &Property, indent: &str) -> String {
        match &p.value {
            Value::Fixed(t) => escape(&self.term(t)),
            Value::OneOf(options) if !options.is_empty() => {
                let n = self.next();
                let choices: Vec<_> = options
                    .iter()
                    .map(|t| escape_choice(&self.term(t)))
                    .collect();
                format!("${{{}|{}|}}", n, choices.join(","))
            }
            Value::Nested(props) if props.len() == 1 => {
                format!("[ {} ]", self.lines(props, indent).join(""))
            }
            Value::Nested(props) => {
                // One property per line, indented one level deeper.
                let inner = format!("{}    ", indent);
                let lines = self.lines(props, &inner);
                format!(
                    "[\n{inner}{}\n{indent}]",
                    lines.join(&format!(" ;\n{inner}"))
                )
            }
            Value::Iri => format!("<{}>", self.placeholder(&p.name)),
            Value::Literal(None) => format!("\"{}\"", self.placeholder(&p.name)),
            Value::Literal(Some(dt)) => match dt.strip_prefix(XSD) {
                Some("boolean") => {
                    let n = self.next();
                    format!("${{{}|true,false|}}", n)
                }
                Some("integer" | "int" | "long" | "decimal" | "double" | "float") => {
                    self.placeholder(&p.name)
                }
                _ => format!(
                    "\"{}\"^^{}",
                    self.placeholder(&p.name),
                    escape(&self.iri(dt))
                ),
            },
            _ => self.placeholder(&p.name),
        }
    }
}

/// Snippet text for `props`: one `predicate value` per line, separated by `;`,
/// with continuation lines indented by `indent`.
pub fn render(props: &[Property], prefixes: &Prefixes, indent: &str) -> String {
    let mut r = Renderer {
        prefixes,
        tabstop: 0,
    };
    let lines = r.lines(props, indent);
    let mut out = lines.join(&format!(" ;\n{}", indent));
    out.push_str("$0");
    out
}

fn documentation(props: &[Property], prefixes: &Prefixes, preview: &str) -> String {
    let r = Renderer {
        prefixes,
        tabstop: 0,
    };
    let mut md = String::new();
    for p in props {
        let names: Vec<_> = if p.is_group() {
            p.alternatives
                .iter()
                .map(|a| format!("`{}`", r.iri(a)))
                .collect()
        } else {
            vec![format!("`{}`", r.iri(&p.path))]
        };
        md.push_str(&format!(
            "- {}{}",
            names.join(" or "),
            if p.required { " (required)" } else { "" }
        ));
        if let Some(d) = &p.description {
            md.push_str(&format!(": {}", d));
        }
        md.push('\n');
    }
    md.push_str(&format!("\n```turtle\n{}\n```", preview));
    md
}

/// Strip snippet syntax to show a template as plain text.
fn plain(snippet: &str) -> String {
    let mut out = String::new();
    let mut chars = snippet.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '\\' => out.extend(chars.next()),
            '$' if chars.peek() == Some(&'{') => {
                chars.next();
                let mut body = String::new();
                let mut depth = 1;
                while let Some(c) = chars.next() {
                    match c {
                        '\\' => body.extend(chars.next()),
                        '{' => {
                            depth += 1;
                            body.push(c)
                        }
                        '}' => {
                            depth -= 1;
                            if depth == 0 {
                                break;
                            }
                            body.push(c)
                        }
                        _ => body.push(c),
                    }
                }
                let body = body.trim_start_matches(|c: char| c.is_ascii_digit());
                if let Some(name) = body.strip_prefix(':') {
                    out.push_str(name);
                } else if let Some(choices) = body.strip_prefix('|') {
                    out.push_str(
                        choices
                            .trim_end_matches('|')
                            .split(',')
                            .next()
                            .unwrap_or(""),
                    );
                }
            }
            '$' if chars.peek() == Some(&'0') => {
                chars.next();
            }
            _ => out.push(c),
        }
    }
    out
}

// ─── Completion system ───────────────────────────────────────────────────────

/// Indentation for continuation lines of a template inserted at `at`.
fn indentation(rope: &RopeC, at: Position) -> String {
    let line = rope.0.line_str(at.line as usize).unwrap_or_default();
    let leading: String = line
        .chars()
        .take_while(|c| *c == ' ' || *c == '\t')
        .collect();
    let before: String = line.chars().take(at.character as usize).collect();
    if before.trim().is_empty() {
        leading
    } else {
        format!("{}    ", leading)
    }
}

/// Completion system offering "required" and "all properties" templates from
/// the SHACL shapes that apply to the subject at the cursor.
pub fn complete_shape_templates(
    mut query: Query<(
        &CursorContext,
        &Prefixes,
        &Triples,
        &Label,
        &DocumentLinks,
        &RopeC,
        &DynLang,
        &mut CompletionRequest,
    )>,
    shapes: ShapeDocs,
    hierarchy: Res<TypeHierarchy>,
    config: Res<ServerConfig>,
) {
    let local = &config.config.local;
    if local.is_disabled(Disabled::CompletionShapeTemplate) || local.is_disabled(Disabled::Shapes) {
        return;
    }

    for (cursor, prefixes, triples, label, links, rope, lang, mut request) in &mut query {
        if !lang.shape_templates() || !cursor.at_predicate() {
            continue;
        }
        let Some(subject) = cursor.subject.as_ref() else {
            continue;
        };

        let (docs, _) = visible_shapes(&shapes, label, links);
        if docs.is_empty() {
            continue;
        }
        let graph = ShapeGraph::new(docs);
        let focus = Focus::new(subject, &triples.0, &hierarchy);
        let all = missing_properties(&graph, &focus);
        if all.is_empty() {
            continue;
        }
        let required: Vec<_> = all.iter().filter(|p| p.required).cloned().collect();

        let range = cursor.range;
        let indent = indentation(rope, range.start);

        let mut offer = |props: &[Property], label: &str, sort: &str| {
            let snippet = render(props, prefixes, &indent);
            let preview = plain(&snippet);
            let names: Vec<_> = props.iter().map(|p| p.name.as_str()).collect();
            request.push(
                SimpleCompletion::new(
                    CompletionItemKind::SNIPPET,
                    label.to_string(),
                    TextEdit {
                        range,
                        new_text: cursor.spaced(&snippet),
                    },
                )
                .as_snippet()
                .label_description(names.join(", "))
                .documentation(documentation(props, prefixes, &preview))
                .sort_text(sort)
                // Clients filter on the typed word; keep the template visible.
                .filter_text(format!("{}{}", cursor.typed, label)),
            );
        };

        if !required.is_empty() {
            offer(&required, "shape template: required properties", "00");
        }
        if all.len() > required.len() {
            offer(&all, "shape template: all properties", "01");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn plain_strips_snippet_syntax() {
        assert_eq!(
            plain("ex:a ${1:name} ;\n  ex:b ${2|ex:x,ex:y|} ;\n  ex:c \"\\$5\"$0"),
            "ex:a name ;\n  ex:b ex:x ;\n  ex:c \"$5\""
        );
    }

    #[test]
    fn escapes_snippet_specials() {
        assert_eq!(escape("a$b}c\\"), "a\\$b\\}c\\\\");
        assert_eq!(escape_choice("a,b|c"), "a\\,b\\|c");
    }

    #[test]
    fn local_names() {
        assert_eq!(
            local_name("http://w3id.org/rml/logicalSource"),
            "logicalSource"
        );
        assert_eq!(local_name("http://ex.test/onto#name"), "name");
    }
}
