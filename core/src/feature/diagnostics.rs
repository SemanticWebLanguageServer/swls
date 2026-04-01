use std::{collections::HashMap, hash::Hash, ops::Range};

use bevy_ecs::{prelude::*, schedule::ScheduleLabel};
use futures::channel::mpsc;

use crate::{
    lsp_types::{Diagnostic, DiagnosticSeverity, TextDocumentItem, Url},
    prelude::*,
};

#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Label;

pub fn setup_schedule(world: &mut World) {
    let mut diagnostics = Schedule::new(Label);
    // Prefix diagnostics are disabled pending CST-based reimplementation.
    // Parse-error diagnostics are still published via publish_diagnostics::<L> added
    // by each language's setup_world.
    diagnostics.add_systems(|| {});
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
        self.tx.unbounded_send(vec![diagnostic]).ok()
    }

    pub fn push_all(&self, diagnostics: Vec<SimpleDiagnostic>) -> Option<()> {
        self.tx.unbounded_send(diagnostics).ok()
    }
}

pub fn publish_diagnostics<L: Lang>(
    query: Query<
        (
            &Errors<L::ElementError>,
            &Wrapped<TextDocumentItem>,
            &RopeC,
            &crate::components::Label,
        ),
        (Changed<Errors<L::ElementError>>, With<Open>),
    >,
    mut client: ResMut<DiagnosticPublisher>,
) where
    L::ElementError: 'static + Clone,
{
    for (element_errors, params, rope, label) in &query {
        tracing::debug!("Publish diagnostics for {}", label.0);
        let diagnostics: Vec<_> = element_errors
            .0
            .iter()
            .cloned()
            .map(|x| Into::<SimpleDiagnostic>::into(x))
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
