use crate::lsp_types::{MarkupContent, MarkupKind};
use crate::prelude::{
    MyTerm, PositionComponent, RopeC, TokenComponent, TripleComponent, TripleTarget,
};
use bevy_ecs::{prelude::*, schedule::ScheduleLabel};
use derive_more::{AsMut, AsRef, Deref, DerefMut};

use crate::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, CompletionTextEdit,
    Documentation, InsertTextFormat, TextEdit,
};
pub use crate::{
    systems::{
        complete_class, complete_properties, keyword_complete,
        prefix::rdf_lov_undefined_prefix_completion,
    },
    util::{token::get_current_cst_token, triple::get_current_triple},
};

/// [`ScheduleLabel`] related to the Completion schedule
#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Label;

/// [`Component`] indicating that the current document is handling a Completion request.
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct CompletionRequest(pub Vec<SimpleCompletion>);

pub fn setup_schedule(world: &mut World) {
    let mut completion = Schedule::new(Label);
    completion.add_systems((
        get_current_cst_token,
        get_current_triple
            .after(get_current_cst_token)
            .before(generate_completions),
        get_cursor_context
            .after(get_current_triple)
            .before(generate_completions),
        keyword_complete.after(generate_completions),
        complete_class.after(generate_completions),
        complete_properties.after(generate_completions),
        crate::systems::complete_shape_templates.after(generate_completions),
        crate::systems::shape_property_completions.after(complete_properties),
        rdf_lov_undefined_prefix_completion.after(generate_completions),
    ));
    world.add_schedule(completion);
}

pub fn generate_completions() {}

/// What a completion request is about, resolved once from the cursor so every
/// completion source agrees. Inserted by [`get_cursor_context`].
#[derive(Component, Debug, Clone)]
pub struct CursorContext {
    /// The role of the position at the cursor.
    pub target: TripleTarget,
    /// The resource being described: the subject of the triple at the cursor,
    /// or the blank node whose `[ … ]` the cursor is in.
    pub subject: Option<MyTerm<'static>>,
    /// The word being typed at the cursor (empty in whitespace or after
    /// punctuation).
    pub typed: String,
    /// Where completion text goes: the typed word, or an empty range at the
    /// cursor.
    pub range: crate::lsp_types::Range,
    /// Whether inserted text needs a leading space (the cursor is right after
    /// `;`, `,` or `[`).
    pub needs_space: bool,
}

impl CursorContext {
    /// Whether the cursor is where a predicate of [`Self::subject`] goes.
    pub fn at_predicate(&self) -> bool {
        self.target == TripleTarget::Predicate && self.subject.is_some()
    }

    /// `text` prefixed with a space when [`Self::needs_space`].
    pub fn spaced(&self, text: &str) -> String {
        if self.needs_space {
            format!(" {}", text)
        } else {
            text.to_string()
        }
    }
}

/// Resolve the [`CursorContext`] of a completion request from the token and
/// triple at the cursor.
pub fn get_cursor_context(
    query: Query<(
        Entity,
        &PositionComponent,
        Option<&TokenComponent>,
        Option<&TripleComponent>,
        &RopeC,
    )>,
    mut commands: Commands,
) {
    for (entity, pos, token, triple, rope) in &query {
        let offset = crate::util::position_to_offset(pos.0, &rope.0).unwrap_or_default();
        let at_cursor = crate::lsp_types::Range::new(pos.0, pos.0);

        let token_text = token.map(|t| t.text.as_str()).unwrap_or_default();
        let punctuation =
            !token_text.is_empty() && token_text.chars().all(|c| ";.,[](){}".contains(c));
        let (typed, range, needs_space) = match token {
            Some(t) if !t.text.is_empty() && !punctuation => (t.text.clone(), t.range, false),
            // Right after `;` / `,` / `[`: insert behind it, separated by a space.
            Some(t) if punctuation && t.source_span.end <= offset => {
                let end = crate::lsp_types::Range::new(t.range.end, t.range.end);
                (String::new(), end, matches!(token_text, ";" | "," | "["))
            }
            _ => (String::new(), at_cursor, false),
        };

        let (mut target, mut subject) = match triple {
            Some(t) => (t.target, Some(t.triple.subject.to_owned())),
            None => (TripleTarget::Subject, None),
        };
        // Inside `[ … ]` but not on a term: a predicate of that blank node.
        if let Some(t) = triple {
            let inside = |term: &MyTerm<'_>| {
                term.ty == Some(sophia_api::term::TermKind::BlankNode)
                    && term.span.start < offset
                    && offset < term.span.end
            };
            let on_term = !typed.is_empty();
            if !on_term && target == TripleTarget::Subject && inside(&t.triple.subject) {
                target = TripleTarget::Predicate;
            } else if !on_term && target == TripleTarget::Object && inside(&t.triple.object) {
                target = TripleTarget::Predicate;
                subject = Some(t.triple.object.to_owned());
            }
        }

        commands.entity(entity).insert(CursorContext {
            target,
            subject,
            typed,
            range,
            needs_space,
        });
    }
}

#[derive(Debug)]
pub struct SimpleCompletion {
    pub kind: CompletionItemKind,
    pub label: String,
    pub _is_snippet: bool,
    pub _label_details: Option<CompletionItemLabelDetails>,
    pub _documentation: Option<String>,
    pub _sort_text: Option<String>,
    pub _filter_text: Option<String>,
    pub edits: Vec<TextEdit>,
    pub _commit_char: Option<String>,
}

impl SimpleCompletion {
    pub fn new(kind: CompletionItemKind, label: String, edit: TextEdit) -> Self {
        Self {
            kind,
            label,
            edits: vec![edit],
            _is_snippet: false,
            _label_details: None,
            _documentation: None,
            _sort_text: None,
            _filter_text: None,
            _commit_char: None,
        }
    }
    pub fn as_snippet(mut self) -> Self {
        self._is_snippet = true;
        self
    }

    pub fn label_detail(mut self, detail: impl Into<String>) -> Self {
        if let Some(ref mut t) = self._label_details {
            t.detail = Some(detail.into());
        } else {
            self._label_details = Some(CompletionItemLabelDetails {
                detail: Some(detail.into()),
                description: None,
            });
        }
        self
    }

    pub fn label_description(mut self, description: impl Into<String>) -> Self {
        self.set_label_description(description);
        self
    }

    /// Set the description shown next to the label of an existing item.
    pub fn set_label_description(&mut self, description: impl Into<String>) {
        if let Some(ref mut t) = self._label_details {
            t.description = Some(description.into());
        } else {
            self._label_details = Some(CompletionItemLabelDetails {
                description: Some(description.into()),
                detail: None,
            });
        }
    }

    pub fn text_edit(mut self, edit: TextEdit) -> Self {
        self.edits.push(edit);
        self
    }

    pub fn documentation(mut self, documentation: impl Into<String>) -> Self {
        self._documentation = Some(documentation.into());
        self
    }

    pub fn m_documentation<S: Into<String>>(mut self, documentation: Option<S>) -> Self {
        self._documentation = documentation.map(|x| x.into());
        self
    }

    pub fn sort_text(mut self, sort_text: impl Into<String>) -> Self {
        self._sort_text = Some(sort_text.into());
        self
    }

    pub fn filter_text(mut self, filter_text: impl Into<String>) -> Self {
        self._filter_text = Some(filter_text.into());
        self
    }

    pub fn commit_char(mut self, commit_char: impl Into<String>) -> Self {
        self._commit_char = Some(commit_char.into());
        self
    }
}

/// Order completions the way LSP clients do: by sort text, or by label when an
/// item has none. (Comparing `Option`s would put every item without a sort
/// text first.)
pub fn sort_completions(items: &mut [SimpleCompletion]) {
    items.sort_by(|a, b| {
        let key = |c: &SimpleCompletion| c._sort_text.clone().unwrap_or_else(|| c.label.clone());
        key(a).cmp(&key(b))
    });
}

impl From<SimpleCompletion> for CompletionItem {
    fn from(val: SimpleCompletion) -> Self {
        let SimpleCompletion {
            _is_snippet: is_snippet,
            _filter_text: filter_text,
            _sort_text: sort_text,
            label,
            _label_details,
            _documentation: documentation,
            kind,
            edits,
            _commit_char: commit_char,
        } = val;

        let text_edit = edits.first().map(|x| CompletionTextEdit::Edit(x.clone()));

        let additional_text_edits = edits.into_iter().skip(1).collect();

        CompletionItem {
            label,
            kind: Some(kind),
            sort_text,
            insert_text_format: (kind == CompletionItemKind::SNIPPET || is_snippet)
                .then_some(InsertTextFormat::SNIPPET),
            filter_text,
            label_details: _label_details,
            documentation: documentation.map(|st| {
                Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: st,
                })
            }),
            text_edit,
            additional_text_edits: Some(additional_text_edits),
            commit_characters: commit_char.map(|x| vec![x]),
            ..Default::default()
        }
    }
}
