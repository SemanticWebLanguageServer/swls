use swls_core::prelude::SimpleDiagnostic;
use turtle::{
    model::Turtle,
    turtle::{
        convert::convert,
        Lang,
        SyntaxKind,
        Rule,
    },
    IncrementalBias, PrevParseInfo,
};

#[derive(Debug, Clone)]
pub struct TurtleParseError {
    pub range: std::ops::Range<usize>,
    pub msg: String,
}

impl From<TurtleParseError> for SimpleDiagnostic {
    fn from(e: TurtleParseError) -> Self {
        SimpleDiagnostic::new(e.range, e.msg)
    }
}

impl From<&TurtleParseError> for SimpleDiagnostic {
    fn from(e: &TurtleParseError) -> Self {
        SimpleDiagnostic::new(e.range.clone(), e.msg.clone())
    }
}

pub type TurtleNode = rowan::SyntaxNode<Lang>;

/// Parse a Turtle document using the A* error-recovering parser.
///
/// Returns the model, parse errors, incremental parse state, and the syntax node.
pub fn parse_new(
    source: &str,
    base_url: &str,
    prev: Option<&PrevParseInfo>,
) -> (
    Turtle,
    Vec<TurtleParseError>,
    PrevParseInfo,
    TurtleNode,
    rowan::GreenNode,
) {
    let (parse, prev_info) = turtle::parse_incremental(
        Rule::new(SyntaxKind::TurtleDoc),
        source,
        prev,
        IncrementalBias::default(),
    );

    let node: TurtleNode = parse.syntax::<Lang>();
    let mut model = convert(&node);
    model.set_base = Some(base_url.to_string());

    let errors = collect_errors(&node);
    (model, errors, prev_info, node, parse.green_node)
}

pub fn collect_errors(node: &TurtleNode) -> Vec<TurtleParseError> {
    use rowan::NodeOrToken;
    let mut errors = Vec::new();
    let mut stack = vec![node.clone()];
    while let Some(current) = stack.pop() {
        for child in current.children_with_tokens() {
            match child {
                NodeOrToken::Node(n) => {
                    if n.kind() == SyntaxKind::Error {
                        let range = turtle::effective_error_span::<Lang>(&n);
                        let msg = n
                            .parent()
                            .map(|p| format!("Expected: {:?}", p.kind()))
                            .unwrap_or_else(|| format!("Unexpected: {}", n.text()));
                        errors.push(TurtleParseError { range, msg });
                    } else {
                        stack.push(n);
                    }
                }
                NodeOrToken::Token(t) => {
                    if t.kind() == SyntaxKind::Error {
                        let range = t.text_range();
                        errors.push(TurtleParseError {
                            range: range.start().into()..range.end().into(),
                            msg: format!("Unexpected: {}", t.text()),
                        });
                    }
                }
            }
        }
    }
    errors
}
