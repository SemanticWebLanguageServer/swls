use std::{collections::HashMap, fmt::Display, hash::Hash, ops::Range};

use bevy_ecs::{prelude::*, schedule::ScheduleLabel};
use chumsky::prelude::Simple;
use futures::channel::mpsc;
/// [`ScheduleLabel`] related to the PrepareRename schedule
pub use systems::prefix::undefined_prefix;

use crate::{
    lsp_types::{Diagnostic, DiagnosticSeverity, TextDocumentItem, Url},
    prelude::*,
};
#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Label;

pub fn setup_schedule(world: &mut World) {
    let mut diagnostics = Schedule::new(Label);
    diagnostics.add_systems((undefined_prefix,));
    world.add_schedule(diagnostics);
}

#[derive(Resource)]
pub struct DiagnosticPublisher {
    tx: mpsc::UnboundedSender<DiagnosticItem>,
    diagnostics: HashMap<crate::lsp_types::Url, Vec<(Diagnostic, &'static str)>>,
}

impl DiagnosticPublisher {
    pub fn new() -> (Self, mpsc::UnboundedReceiver<DiagnosticItem>) {
        let (tx, rx) = mpsc::unbounded();
        (
            Self {
                tx,
                diagnostics: HashMap::new(),
            },
            rx,
        )
    }

    pub fn publish(
        &mut self,
        params: &TextDocumentItem,
        diagnostics: Vec<Diagnostic>,
        reason: &'static str,
    ) -> Option<()> {
        let items = self.diagnostics.entry(params.uri.clone()).or_default();
        items.retain(|(_, r)| *r != reason);
        items.extend(diagnostics.into_iter().map(|x| (x, reason)));
        let diagnostics: Vec<_> = items.iter().map(|(x, _)| x).cloned().collect();
        let uri = params.uri.clone();
        let version = Some(params.version);
        let item = DiagnosticItem {
            diagnostics,
            uri,
            version,
        };
        self.tx.unbounded_send(item).ok()
    }
}

#[derive(Debug)]
pub struct SimpleDiagnostic {
    pub range: Range<usize>,
    pub msg: String,
    pub severity: Option<DiagnosticSeverity>,
}

impl SimpleDiagnostic {
    pub fn new(range: Range<usize>, msg: String) -> Self {
        Self {
            range,
            msg,
            severity: None,
        }
    }

    pub fn new_severity(range: Range<usize>, msg: String, severity: DiagnosticSeverity) -> Self {
        Self {
            range,
            msg,
            severity: Some(severity),
        }
    }
}

impl<T: Display + Eq + Hash> From<Simple<T>> for SimpleDiagnostic {
    fn from(e: Simple<T>) -> Self {
        let msg = if let chumsky::error::SimpleReason::Custom(msg) = e.reason() {
            msg.clone()
        } else {
            format!(
                "{}{}, expected {}",
                if e.found().is_some() {
                    "Unexpected token"
                } else {
                    "Unexpected end of input"
                },
                if let Some(label) = e.label() {
                    format!(" while parsing {}", label)
                } else {
                    String::new()
                },
                if e.expected().len() == 0 {
                    "something else".to_string()
                } else {
                    e.expected()
                        .map(|expected| match expected {
                            Some(expected) => format!("'{}'", expected),
                            None => "end of input".to_string(),
                        })
                        .collect::<Vec<_>>()
                        .join(" or ")
                },
            )
        };

        SimpleDiagnostic::new(e.span(), msg)
    }
}

impl<T: Display + Eq + Hash> From<(usize, Simple<T>)> for SimpleDiagnostic {
    fn from(this: (usize, Simple<T>)) -> Self {
        let (len, e) = this;
        let msg = if let chumsky::error::SimpleReason::Custom(msg) = e.reason() {
            msg.clone()
        } else {
            format!(
                "{}{}, expected {}",
                if e.found().is_some() {
                    "Unexpected token"
                } else {
                    "Unexpected end of input"
                },
                if let Some(label) = e.label() {
                    format!(" while parsing {}", label)
                } else {
                    String::new()
                },
                if e.expected().len() == 0 {
                    "something else".to_string()
                } else {
                    e.expected()
                        .map(|expected| match expected {
                            Some(expected) => format!("'{}'", expected),
                            None => "end of input".to_string(),
                        })
                        .collect::<Vec<_>>()
                        .join(" or ")
                },
            )
        };

        let range = (len - e.span().end)..(len - e.span().start);
        SimpleDiagnostic::new(range, msg)
    }
}

#[derive(Clone)]
pub struct DiagnosticSender {
    tx: mpsc::UnboundedSender<Vec<SimpleDiagnostic>>,
}

#[derive(Debug)]
pub struct DiagnosticItem {
    pub diagnostics: Vec<Diagnostic>,
    pub uri: Url,
    pub version: Option<i32>,
}
impl DiagnosticSender {
    pub fn push(&self, diagnostic: SimpleDiagnostic) -> Option<()> {
        let out = self.tx.unbounded_send(vec![diagnostic]).ok();
        out
    }

    pub fn push_all(&self, diagnostics: Vec<SimpleDiagnostic>) -> Option<()> {
        self.tx.unbounded_send(diagnostics).ok()
    }
}

pub fn publish_diagnostics<L: Lang>(
    query: Query<
        (
            &Errors<L::TokenError>,
            &Errors<L::ElementError>,
            &Wrapped<TextDocumentItem>,
            &RopeC,
            &crate::components::Label,
        ),
        (
            Or<(
                Changed<Errors<L::TokenError>>,
                Changed<Errors<L::ElementError>>,
            )>,
            With<Open>,
        ),
    >,
    mut client: ResMut<DiagnosticPublisher>,
) where
    L::TokenError: 'static + Clone,
    L::ElementError: 'static + Clone,
{
    for (token_errors, element_errors, params, rope, label) in &query {
        tracing::info!("Publish diagnostics for {}", label.0);
        use std::iter::Iterator as _;
        let token_iter = token_errors
            .0
            .iter()
            .cloned()
            .map(|x| Into::<SimpleDiagnostic>::into(x));
        let turtle_iter = element_errors
            .0
            .iter()
            .cloned()
            .map(|x| Into::<SimpleDiagnostic>::into(x));

        let diagnostics: Vec<_> = Iterator::chain(token_iter, turtle_iter)
            .flat_map(|item| {
                let (span, message) = (item.range, item.msg);
                let start_position = offset_to_position(span.start, &rope.0)?;
                let end_position = offset_to_position(span.end, &rope.0)?;
                Some(Diagnostic {
                    range: crate::lsp_types::Range::new(start_position, end_position),
                    message,
                    severity: item.severity,
                    ..Default::default()
                })
            })
            .collect();

        let _ = client.publish(&params.0, diagnostics, "syntax");
    }
}
