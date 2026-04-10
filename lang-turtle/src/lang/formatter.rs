use std::{
    io::{self, Cursor, Write},
    ops::Range,
};

use ropey::Rope;
use swls_core::prelude::Spanned;
use swls_core::{lsp_types::FormattingOptions, prelude::spanned};
use tracing::{info, trace, warn};

use crate::lang::model::{Base, BlankNode, Term, Triple, Turtle, TurtlePrefix, PO};

type Buf = Cursor<Vec<u8>>;
struct FormatState<'a> {
    indent_level: usize,
    indent: String,
    buf: Buf,
    line_start: u64,
    comments: &'a [Spanned<String>],
    comments_idx: usize,
    tail: Spanned<String>,
    line_count: usize,
}

impl<'a> FormatState<'a> {
    fn new(
        options: FormattingOptions,
        buf: Buf,
        comments: &'a [Spanned<String>],
        source: &'a Rope,
    ) -> Self {
        let mut indent = String::new();
        for _ in 0..options.tab_size {
            indent.push(' ');
        }

        let tail = spanned(
            String::new(),
            source.len_chars() + 1..source.len_chars() + 1,
        );
        Self {
            tail,
            line_start: 0,
            indent_level: 0,
            indent,
            buf,
            comments,
            comments_idx: 0,
            line_count: 0,
        }
    }

    fn check_comments(&mut self, span: &Range<usize>) -> io::Result<bool> {
        trace!("checking comments with span {:?}", span);
        let mut first = true;
        loop {
            let current = self.comments.get(self.comments_idx).unwrap_or(&self.tail);

            if current.1.start > span.start {
                break;
            }

            first = false;
            write!(self.buf, "{}", current.0)?;
            self.new_line()?;
            self.comments_idx += 1;
        }
        Ok(!first)
    }
    fn current_line_length(&self) -> u64 {
        self.buf.position() - self.line_start
    }
    fn new_line(&mut self) -> io::Result<()> {
        self.line_count += 1;
        write!(self.buf, "\n")?;
        self.line_start = self.buf.position();
        for _ in 0..self.indent_level {
            write!(self.buf, "{}", &self.indent)?;
        }
        Ok(())
    }
    fn inc(&mut self) {
        self.indent_level += 1;
    }
    fn decr(&mut self) {
        self.indent_level -= 1;
    }
}

impl FormatState<'_> {
    fn write_turtle(&mut self, turtle: &Turtle) -> io::Result<()> {
        if let Some(ref b) = turtle.base {
            self.check_comments(&b.1)?;
            self.write_base(b)?;
            self.new_line()?;
        }
        for p in &turtle.prefixes {
            self.check_comments(&p.1)?;
            self.write_prefix(p)?;
            self.new_line()?;
        }

        let mut prev_line = 0;

        for t in &turtle.triples {
            if prev_line + 1 < self.line_count {
                self.new_line()?;
            }
            prev_line = self.line_count;
            self.check_comments(&t.1)?;
            self.write_triple(&t)?;
            self.new_line()?;
            // request_newline = t.0.po.len() > 1 || t.0.po[0].0.object.len() > 1;
        }
        self.new_line()?;

        for i in self.comments_idx..self.comments.len() {
            write!(self.buf, "{}", self.comments[i].0)?;
            self.new_line()?;
        }

        Ok(())
    }

    fn write_prefix(&mut self, prefix: &TurtlePrefix) -> io::Result<()> {
        write!(self.buf, "@prefix {}: {}.", prefix.prefix.0, prefix.value.0)
    }

    fn write_base(&mut self, base: &Base) -> io::Result<()> {
        write!(self.buf, "@base {}.", base.1 .0)
    }

    fn write_bnode(&mut self, bnode: &BlankNode) -> io::Result<()> {
        match bnode {
            BlankNode::Named(x, _) => write!(self.buf, "_:{}", x)?,
            BlankNode::Unnamed(pos, _, _) => {
                if pos.len() == 0 {
                    return write!(self.buf, "[ ]");
                }
                if pos.len() == 1 {
                    write!(self.buf, "[ ")?;
                    self.write_po(&pos[0])?;
                    return write!(self.buf, " ]");
                }
                let is_first_of_line = self.current_line_length() == 0;
                self.inc();
                write!(self.buf, "[")?;
                let should_skip = if is_first_of_line {
                    write!(self.buf, " ")?;
                    self.write_po(&pos[0])?;
                    write!(self.buf, ";")?;
                    1
                } else {
                    0
                };
                for po in pos.iter().skip(should_skip) {
                    self.new_line()?;
                    self.check_comments(&po.1)?;
                    self.write_po(&po)?;
                    write!(self.buf, ";")?;
                }
                self.decr();
                self.new_line()?;
                write!(self.buf, "]")?;
            }
            BlankNode::Invalid => return Err(io::Error::new(io::ErrorKind::Other, "")),
        }
        Ok(())
    }

    fn write_collection(&mut self, coll: &Vec<Spanned<Term>>) -> io::Result<()> {
        if coll.is_empty() {
            return write!(self.buf, "( )");
        }

        let mut should_indent = false;
        let start = self.buf.position();
        let current_line = self.line_count;

        write!(self.buf, "( ")?;

        self.check_comments(&coll[0].1)?;
        self.write_term(&coll[0])?;

        for po in coll.iter().skip(1) {
            self.check_comments(&po.1)?;
            write!(self.buf, " ")?;
            self.write_term(&po)?;
            if self.current_line_length() > 80 {
                should_indent = true;
                break;
            }
        }
        write!(self.buf, " )")?;

        if should_indent {
            self.buf.set_position(start);
            self.line_count = current_line;
            write!(self.buf, "(")?;
            self.inc();
            for po in coll.iter() {
                self.new_line()?;
                self.check_comments(&po.1)?;
                self.write_term(&po)?;
            }
            self.decr();
            self.new_line()?;
            write!(self.buf, ")")?;
        }

        Ok(())
    }

    fn write_term(&mut self, term: &Term) -> io::Result<()> {
        match term {
            Term::Literal(s) => write!(self.buf, "{}", s)?,
            Term::BlankNode(b) => self.write_bnode(b)?,
            Term::NamedNode(n) => write!(self.buf, "{}", n)?,
            Term::Collection(ts) => self.write_collection(ts)?,
            Term::Invalid => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "cannot format turtle with invalid terms",
                ))
            }
            Term::Variable(_) => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "cannot format turtle with variables",
                ))
            }
        }
        Ok(())
    }

    fn write_po(&mut self, po: &PO) -> io::Result<()> {
        write!(self.buf, "{} ", po.predicate.0)?;
        self.write_term(&po.object[0])?;
        let mut should_indent = false;

        let start = self.buf.position();
        let current_line = self.line_count;
        for i in 1..po.object.len() {
            write!(self.buf, ", ")?;
            self.write_term(&po.object[i])?;

            if self.current_line_length() > 80 {
                should_indent = true;
                break;
            }
        }

        if should_indent {
            self.buf.set_position(start);
            self.line_count = current_line;
            self.inc();
            for i in 1..po.object.len() {
                write!(self.buf, ",")?;
                self.new_line()?;
                self.check_comments(&po.object[i].1)?;
                self.write_term(&po.object[i])?;
            }
            self.decr();
        }

        Ok(())
    }

    fn write_triple(&mut self, triple: &Triple) -> io::Result<()> {
        match &triple.subject.0 {
            Term::BlankNode(bn) => self.write_bnode(bn)?,
            Term::NamedNode(n) => write!(self.buf, "{}", n)?,
            _ => write!(self.buf, "invalid")?,
        }
        if triple.po.is_empty() {
            write!(self.buf, ".")?;
            return Ok(());
        }
        write!(self.buf, " ")?;
        self.write_po(&triple.po[0])?;
        if triple.po.len() == 1 {
            write!(self.buf, ".")?;
            return Ok(());
        }
        write!(self.buf, ";")?;
        self.inc();

        self.new_line()?;
        self.check_comments(&triple.po[1].1)?;
        self.write_po(&triple.po[1])?;

        if triple.po.len() == 2 {
            self.decr();
            write!(self.buf, ".")?;
            return Ok(());
        }

        for i in 2..triple.po.len() {
            write!(self.buf, ";")?;
            self.new_line()?;
            self.check_comments(&triple.po[i].1)?;
            self.write_po(&triple.po[i])?;
        }

        write!(self.buf, ".")?;
        self.decr();
        Ok(())
    }
}

pub fn format_turtle(
    turtle: &Turtle,
    config: FormattingOptions,
    comments: &[Spanned<String>],
    source: &Rope,
) -> Option<String> {
    let buf: Buf = Cursor::new(Vec::new());
    let mut state = FormatState::new(config, buf, comments, source);
    match state.write_turtle(turtle) {
        Ok(_) => info!("Format successful"),
        Err(e) => {
            warn!("Format unsuccessful {:?}", e);
            return None;
        }
    }
    String::from_utf8(state.buf.into_inner()).ok()
}

#[cfg(test)]
mod tests {

    use std::str::FromStr;

    use ropey::Rope;
    use rowan::NodeOrToken;
    use swls_core::prelude::Spanned;

    use crate::lang::{formatter::format_turtle, model::Turtle, parser::parse_new};
    use rdf_parsers::turtle::SyntaxKind;

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
            &Rope::from_str(txt),
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
            &Rope::from_str(txt),
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

        let expected = r#"[ <a> foaf:Person;
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
            &Rope::from_str(txt),
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
            &Rope::from_str(txt),
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
            &Rope::from_str(txt),
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
            &Rope::from_str(txt),
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
            &Rope::from_str(txt),
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
            &Rope::from_str(txt),
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
            &Rope::from_str(txt),
        )
        .expect("formatting");
        assert_eq!(formatted, expected);
    }
}
