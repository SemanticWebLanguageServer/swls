//! Turtle formatter.
//!
//! Formatting happens in two passes:
//! 1. The AST is converted into a [`pretty`] document. All style decisions
//!    (what must break, what may break, where comments go) are made here.
//! 2. `pretty` renders the document, choosing per group whether it fits on
//!    the current line. A hard line inside a group always breaks that group,
//!    so "a child is multi-line" propagates upwards without backtracking.

use std::ops::Range;

use pretty::RcDoc;
use swls_core::text::LineIndex;
use swls_core::{lsp_types::FormattingOptions, prelude::Spanned};
use tracing::warn;

use swls_lang_rdf_base::traits::{Base, BlankNode, Term, Triple, Turtle, TurtlePrefix, PO};

type Doc = RcDoc<'static, ()>;

#[derive(Default, Clone, Copy)]
pub enum PrefixStyle {
    #[default]
    OldSchool,
    Sparql,
}

/// Knobs that influence how the document is laid out.
#[derive(Clone, Copy)]
struct Style {
    indent: isize,
    max_width: usize,
    /// Blank nodes with more than one predicate-object pair are always
    /// written over multiple lines, even when they would fit.
    break_multi_po_bnodes: bool,
}

impl Style {
    fn new(options: &FormattingOptions) -> Self {
        Self {
            indent: options.tab_size as isize,
            max_width: 80,
            break_multi_po_bnodes: true,
        }
    }
}

struct FormatError(&'static str);

type Result<T> = std::result::Result<T, FormatError>;

struct DocBuilder<'a> {
    style: Style,
    comments: &'a [Spanned<String>],
    comments_idx: usize,
}

impl<'a> DocBuilder<'a> {
    /// Takes all not yet emitted comments that start before `pos`.
    fn take_comments(&mut self, pos: usize) -> Vec<&'a str> {
        let mut out = Vec::new();
        while let Some(c) = self.comments.get(self.comments_idx) {
            if c.1.start > pos {
                break;
            }
            out.push(c.0.as_str());
            self.comments_idx += 1;
        }
        out
    }

    /// Comments before `pos`, each followed by a hard line.
    fn comments_doc(&mut self, pos: usize) -> Doc {
        Doc::concat(
            self.take_comments(pos)
                .into_iter()
                .map(|c| Doc::text(c.to_string()).append(Doc::hardline())),
        )
    }

    fn has_comment_in(&self, span: &Range<usize>) -> bool {
        self.comments[self.comments_idx..]
            .iter()
            .any(|c| span.contains(&c.1.start))
    }

    /// Whether this term is always written over multiple lines,
    /// independent of where it ends up on the line.
    fn forces_break(&self, term: &Term, span: &Range<usize>) -> bool {
        match term {
            Term::BlankNode(BlankNode::Unnamed(pos, _, _)) => self.forces_break_bnode(pos, span),
            Term::Collection(items) => {
                self.has_comment_in(span) || items.iter().any(|t| self.forces_break(&t.0, &t.1))
            }
            _ => false,
        }
    }

    fn forces_break_bnode(&self, pos: &[Spanned<PO>], span: &Range<usize>) -> bool {
        (self.style.break_multi_po_bnodes && pos.len() > 1)
            || self.has_comment_in(span)
            || pos
                .iter()
                .flat_map(|po| po.object.iter())
                .any(|o| self.forces_break(&o.0, &o.1))
    }

    fn prefix(&self, prefix: &TurtlePrefix, style: PrefixStyle) -> String {
        match style {
            PrefixStyle::OldSchool => format!("@prefix {}: {}.", prefix.prefix.0, prefix.value.0),
            PrefixStyle::Sparql => format!("PREFIX {}: {}", prefix.prefix.0, prefix.value.0),
        }
    }

    fn base(&self, base: &Base, style: PrefixStyle) -> String {
        match style {
            PrefixStyle::OldSchool => format!("@base {}.", base.1 .0),
            PrefixStyle::Sparql => format!("BASE {}", base.1 .0),
        }
    }

    fn term(&mut self, term: &Spanned<Term>) -> Result<Doc> {
        match &term.0 {
            Term::Literal(s) => Ok(Doc::text(s.to_string())),
            Term::NamedNode(n) => Ok(Doc::text(n.to_string())),
            Term::BlankNode(b) => self.bnode(b, &term.1),
            Term::Collection(items) => self.collection(items),
            Term::Invalid => Err(FormatError("cannot format turtle with invalid terms")),
            Term::Variable(_) => Err(FormatError("cannot format turtle with variables")),
        }
    }

    fn bnode(&mut self, bnode: &BlankNode, span: &Range<usize>) -> Result<Doc> {
        let pos = match bnode {
            BlankNode::Named(x, _) => return Ok(Doc::text(format!("_:{}", x))),
            BlankNode::Unnamed(pos, _, _) => pos,
            BlankNode::Invalid => return Err(FormatError("invalid blank node")),
        };
        if pos.is_empty() {
            return Ok(Doc::text("[ ]"));
        }

        let forced = self.forces_break_bnode(pos, span);
        if pos.len() == 1 && !forced {
            // `[ p o ]` when it fits, otherwise break after `[`.
            let body = self.comments_doc(pos[0].1.start).append(self.po(&pos[0])?);
            return Ok(Doc::text("[")
                .append(Doc::line().append(body).nest(self.style.indent))
                .append(Doc::line())
                .append(Doc::text("]"))
                .group());
        }

        if pos.len() == 1 {
            // Multi-line content: break after `[`, no trailing `;`.
            let body = self.comments_doc(pos[0].1.start).append(self.po(&pos[0])?);
            return Ok(Doc::text("[")
                .append(Doc::hardline().append(body).nest(self.style.indent))
                .append(Doc::hardline())
                .append(Doc::text("]")));
        }

        let mut body = Doc::nil();
        for po in pos {
            body = body
                .append(Doc::hardline())
                .append(self.comments_doc(po.1.start))
                .append(self.po(po)?)
                .append(Doc::text(";"));
        }
        Ok(Doc::text("[")
            .append(body.nest(self.style.indent))
            .append(Doc::hardline())
            .append(Doc::text("]")))
    }

    fn collection(&mut self, items: &[Spanned<Term>]) -> Result<Doc> {
        if items.is_empty() {
            return Ok(Doc::text("( )"));
        }
        // `( a b )` when it fits, otherwise one item per line.
        let mut body = Doc::nil();
        for item in items {
            body = body
                .append(Doc::line())
                .append(self.comments_doc(item.1.start))
                .append(self.term(item)?);
        }
        Ok(Doc::text("(")
            .append(body.nest(self.style.indent))
            .append(Doc::line())
            .append(Doc::text(")"))
            .group())
    }

    fn is_atomic(term: &Term) -> bool {
        match term {
            Term::BlankNode(BlankNode::Unnamed(pos, _, _)) => pos.is_empty(),
            Term::Collection(items) => items.is_empty(),
            _ => true,
        }
    }

    fn po(&mut self, po: &PO) -> Result<Doc> {
        let mut doc = Doc::text(format!("{} ", po.predicate.0));
        let Some((first, rest)) = po.object.split_first() else {
            return Err(FormatError("predicate without object"));
        };
        doc = doc.append(self.term(first)?);

        if po.object.iter().all(|o| Self::is_atomic(&o.0)) {
            // All on one line, or every object on its own (indented) line.
            let mut tail = Doc::nil();
            for o in rest {
                tail = tail
                    .append(Doc::text(","))
                    .append(Doc::line())
                    .append(self.comments_doc(o.1.start))
                    .append(self.term(o)?);
            }
            Ok(doc.append(tail.nest(self.style.indent)).group())
        } else {
            // Nested structures lay themselves out; keep `], [` together.
            for o in rest {
                doc = doc
                    .append(Doc::text(", "))
                    .append(self.comments_doc(o.1.start))
                    .append(self.term(o)?);
            }
            Ok(doc)
        }
    }

    fn triple(&mut self, triple: &Triple) -> Result<Doc> {
        let mut doc = match &triple.subject.0 {
            Term::BlankNode(bn) => self.bnode(bn, &triple.subject.1)?,
            Term::NamedNode(n) => Doc::text(n.to_string()),
            _ => Doc::text("invalid"),
        };
        let Some((first, rest)) = triple.po.split_first() else {
            return Ok(doc.append(Doc::text(".")));
        };
        doc = doc.append(Doc::text(" ")).append(self.po(first)?);

        let mut tail = Doc::nil();
        for po in rest {
            tail = tail
                .append(Doc::text(";"))
                .append(Doc::hardline())
                .append(self.comments_doc(po.1.start))
                .append(self.po(po)?);
        }
        Ok(doc
            .append(tail.nest(self.style.indent))
            .append(Doc::text(".")))
    }

    fn render(&self, doc: Doc) -> String {
        let mut out = String::new();
        // Writing to a String cannot fail.
        let _ = doc.render_fmt(self.style.max_width, &mut out);
        out
    }

    fn turtle(&mut self, turtle: &Turtle, prefix_style: PrefixStyle) -> Result<String> {
        let mut out = String::new();
        let mut line_count = 0;
        let push_line = |out: &mut String, line_count: &mut usize, s: &str| {
            out.push_str(s);
            out.push('\n');
            *line_count += s.matches('\n').count() + 1;
        };

        if let Some(ref b) = turtle.base {
            for c in self.take_comments(b.1.start) {
                push_line(&mut out, &mut line_count, c);
            }
            push_line(&mut out, &mut line_count, &self.base(b, prefix_style));
        }
        for p in &turtle.prefixes {
            for c in self.take_comments(p.1.start) {
                push_line(&mut out, &mut line_count, c);
            }
            push_line(&mut out, &mut line_count, &self.prefix(p, prefix_style));
        }

        // Separate a statement from the previous one with a blank line
        // when the previous chunk spanned multiple lines.
        let mut prev_line = 0;
        for t in &turtle.triples {
            if prev_line + 1 < line_count {
                push_line(&mut out, &mut line_count, "");
            }
            prev_line = line_count;
            for c in self.take_comments(t.1.start) {
                push_line(&mut out, &mut line_count, c);
            }
            let doc = self.triple(t)?;
            let rendered = self.render(doc);
            push_line(&mut out, &mut line_count, &rendered);
        }
        push_line(&mut out, &mut line_count, "");

        for c in self.take_comments(usize::MAX) {
            push_line(&mut out, &mut line_count, c);
        }

        Ok(out)
    }
}

pub fn format_turtle(
    turtle: &Turtle,
    config: FormattingOptions,
    comments: &[Spanned<String>],
    _source: &LineIndex,
    prefix_style: impl Into<PrefixStyle>,
) -> Option<String> {
    let mut builder = DocBuilder {
        style: Style::new(&config),
        comments,
        comments_idx: 0,
    };
    match builder.turtle(turtle, prefix_style.into()) {
        Ok(out) => {
            tracing::debug!("Format successful");
            Some(out)
        }
        Err(e) => {
            warn!("Format unsuccessful: {}", e.0);
            None
        }
    }
}

#[cfg(test)]
mod tests {

    use std::str::FromStr;

    use rdf_parsers::turtle::SyntaxKind;
    use rowan::NodeOrToken;
    use swls_core::prelude::Spanned;
    use swls_core::text::LineIndex;

    use crate::lang::{
        formatter::{format_turtle, PrefixStyle},
        parser::parse_new,
        Turtle,
    };

    fn parse_turtle(inp: &str, url: &swls_core::lsp_types::Url) -> (Turtle, Vec<Spanned<String>>) {
        let (turtle, errs, _, syntax, _) = parse_new(inp, url.as_str(), None);
        for e in &errs {
            println!("Parse error: {:?}", e);
        }

        // Extract comments from the CST
        let mut comments: Vec<Spanned<String>> = Vec::new();
        for node_or_token in syntax.descendants_with_tokens() {
            if let NodeOrToken::Token(t) = node_or_token {
                if t.kind() == SyntaxKind::Comment {
                    let range = t.text_range();
                    let span = usize::from(range.start())..usize::from(range.end());
                    let text = t.text().to_string();
                    comments.push(swls_core::prelude::spanned(text, span));
                }
            }
        }
        comments.sort_by_key(|x| x.1.start);

        (turtle, comments)
    }

    #[test]
    fn easy_format() {
        let txt = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@base <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

[] a foaf:Name;
   foaf:knows <abc>;.
"#;

        let expected = r#"@base <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.

[ ] a foaf:Name;
  foaf:knows <abc>.

"#;

        let url = swls_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let (output, comments) = parse_turtle(txt, &url);
        let formatted = format_turtle(
            &output,
            swls_core::lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments,
            &LineIndex::new(txt),
            PrefixStyle::OldSchool,
        )
        .expect("formatting");
        assert_eq!(formatted, expected);
    }

    #[test]
    fn harder_format_pos() {
        let txt = r#"
[] a foaf:Name;
   foaf:knows <abc>; foaf:knows2 <abc>.

"#;

        let expected = r#"[ ] a foaf:Name;
  foaf:knows <abc>;
  foaf:knows2 <abc>.

"#;

        let url = swls_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let (output, comments) = parse_turtle(txt, &url);
        let formatted = format_turtle(
            &output,
            swls_core::lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments,
            &LineIndex::new(txt),
            PrefixStyle::OldSchool,
        )
        .expect("formatting");
        assert_eq!(formatted, expected);
    }

    #[test]
    fn format_blanknodes() {
        let txt = r#"
        [ <a> foaf:Person; foaf:knows <abc>; foaf:knows <def> ] foaf:knows [
        a foaf:Person;
        foaf:knows <abc>;
        foaf:knows <def>;
        ] .

"#;

        let expected = r#"[
  <a> foaf:Person;
  foaf:knows <abc>;
  foaf:knows <def>;
] foaf:knows [
  a foaf:Person;
  foaf:knows <abc>;
  foaf:knows <def>;
].

"#;

        let url = swls_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let (output, comments) = parse_turtle(txt, &url);
        let formatted = format_turtle(
            &output,
            swls_core::lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments,
            &LineIndex::new(txt),
            PrefixStyle::OldSchool,
        )
        .expect("formatting");
        assert_eq!(formatted, expected);
    }

    #[test]
    fn long_objectlist() {
        let txt = r#"
        <abc> a <something-long>, <something-longer-still>, <something-longer>, <something-tes>, <soemthing-eeeellssee>.
"#;

        let expected = r#"<abc> a <something-long>,
  <something-longer-still>,
  <something-longer>,
  <something-tes>,
  <soemthing-eeeellssee>.

"#;

        let url = swls_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let (output, comments) = parse_turtle(txt, &url);
        let formatted = format_turtle(
            &output,
            swls_core::lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments,
            &LineIndex::new(txt),
            PrefixStyle::OldSchool,
        )
        .expect("formatting");
        assert_eq!(formatted, expected);
    }

    #[test]
    fn short_collection() {
        let txt = r#"
        <abc> a (), (<abc> <def>).
"#;

        let expected = r#"<abc> a ( ), ( <abc> <def> ).

"#;

        let url = swls_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let (output, comments) = parse_turtle(txt, &url);
        let formatted = format_turtle(
            &output,
            swls_core::lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments,
            &LineIndex::new(txt),
            PrefixStyle::OldSchool,
        )
        .expect("formatting");
        assert_eq!(formatted, expected);
    }

    #[test]
    fn long_collection() {
        let txt = r#"
        <abc> a (), (<somevery-very-very-long-item> <and-othersss> <and-ottteeehs> <wheeeeeeeeeeeee>).
"#;

        let expected = r#"<abc> a ( ), (
  <somevery-very-very-long-item>
  <and-othersss>
  <and-ottteeehs>
  <wheeeeeeeeeeeee>
).

"#;

        let url = swls_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let (output, comments) = parse_turtle(txt, &url);
        let formatted = format_turtle(
            &output,
            swls_core::lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments,
            &LineIndex::new(txt),
            PrefixStyle::OldSchool,
        )
        .expect("formatting");
        assert_eq!(formatted, expected);
    }

    #[test]
    fn easy_comments() {
        let txt = r#"
# Test this is a cool test or something!
            # Another comment!

[] a foaf:Name;
   foaf:knows <abc>; foaf:knows2 <abc>.

"#;

        let expected = r#"# Test this is a cool test or something!
# Another comment!
[ ] a foaf:Name;
  foaf:knows <abc>;
  foaf:knows2 <abc>.

"#;

        let url = swls_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let (output, comments) = parse_turtle(txt, &url);
        println!("OUtput {:?}", output);
        let formatted = format_turtle(
            &output,
            swls_core::lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments,
            &LineIndex::new(txt),
            PrefixStyle::OldSchool,
        )
        .expect("formatting");
        assert_eq!(formatted, expected);
    }

    #[test]
    fn hard_comments() {
        let txt = r#"

[] a foaf:Name; # Nested comment
   foaf:knows <abc>;     # Another comment!
   foaf:knows2 <abc>.

   #trailing comments
"#;

        let expected = r#"[ ] a foaf:Name;
  # Nested comment
  foaf:knows <abc>;
  # Another comment!
  foaf:knows2 <abc>.

#trailing comments
"#;
        let url = swls_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let (output, comments) = parse_turtle(txt, &url);
        let formatted = format_turtle(
            &output,
            swls_core::lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments,
            &LineIndex::new(txt),
            PrefixStyle::OldSchool,
        )
        .expect("formatting");
        assert_eq!(formatted, expected);
    }

    #[test]
    fn bug_1() {
        let txt = r#"
[] a sh:NodeShape;
  sh:targetClass js:Echo;
  sh:property [
    sh:class :ReaderChannel;
    sh:path js:input;
    sh:name "Input Channel"
  ], [
    sh:class :WriterChannel;
    sh:path js:output;
    sh:name "Output Channel"
  ].

"#;

        let expected = r#"[ ] a sh:NodeShape;
  sh:targetClass js:Echo;
  sh:property [
    sh:class :ReaderChannel;
    sh:path js:input;
    sh:name "Input Channel";
  ], [
    sh:class :WriterChannel;
    sh:path js:output;
    sh:name "Output Channel";
  ].

"#;

        let url = swls_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let (output, comments) = parse_turtle(txt, &url);
        let formatted = format_turtle(
            &output,
            swls_core::lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments,
            &LineIndex::new(txt),
            PrefixStyle::OldSchool,
        )
        .expect("formatting");
        assert_eq!(formatted, expected);
    }

    #[test]
    fn format_nested_bnodes() {
        let txt = r#"[] sh:or ( [ sh:property [
    sh:path crm:P4_has_time-span;
    sh:minCount 1;
  ] ] [ sh:property [
    sh:path crm:P7_took_place_at;
    sh:minCount 1;
  ] ] ).
        "#;

        let expected = r#"[ ] sh:or (
  [
    sh:property [
      sh:path crm:P4_has_time-span;
      sh:minCount 1;
    ]
  ]
  [
    sh:property [
      sh:path crm:P7_took_place_at;
      sh:minCount 1;
    ]
  ]
).

"#;

        let url = swls_core::lsp_types::Url::from_str("http://example.com/ns#").unwrap();
        let (output, comments) = parse_turtle(txt, &url);
        let formatted = format_turtle(
            &output,
            swls_core::lsp_types::FormattingOptions {
                tab_size: 2,
                ..Default::default()
            },
            &comments,
            &LineIndex::new(txt),
            PrefixStyle::OldSchool,
        )
        .expect("formatting");

        assert_eq!(formatted, expected);
    }
}
