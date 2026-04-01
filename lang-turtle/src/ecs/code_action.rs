use std::collections::HashMap;

use bevy_ecs::prelude::*;
use swls_core::{
    feature::code_action::CodeActionRequest,
    prelude::*,
};
use swls_core::lsp_types::{CodeAction, CodeActionKind, TextEdit, WorkspaceEdit};
use crate::lang::model::NamedNodeExt;

use crate::TurtleLang;

pub fn organize_imports(
    mut query: Query<(
        &Element<TurtleLang>,
        &RopeC,
        &Label,
        &mut CodeActionRequest,
    )>,
) {
    for (turtle, rope, label, mut req) in &mut query {
        let prefixes = &turtle.prefixes;
        if prefixes.len() < 2 {
            continue;
        }

        // Collect (prefix_name, url_str) for each declared prefix
        let mut items: Vec<(String, String)> = prefixes
            .iter()
            .filter_map(|sp| {
                let p = sp.value();
                let url = p.value.value().expand(turtle.value())?;
                Some((p.prefix.value().clone(), url))
            })
            .collect();

        // Skip if we couldn't expand all prefixes
        if items.len() != prefixes.len() {
            continue;
        }

        // Check if already sorted — skip if so
        let sorted = items.windows(2).all(|w| w[0].0 <= w[1].0);
        if sorted {
            continue;
        }

        // Sort by prefix name
        items.sort_by(|a, b| a.0.cmp(&b.0));

        // Compute the total span from start of first to end of last prefix declaration
        let start = prefixes.first().unwrap().span().start;
        let end = prefixes.last().unwrap().span().end;

        // Convert byte offsets to LSP positions via rope
        let start_pos = match offset_to_position(start, &rope.0) {
            Some(p) => p,
            None => continue,
        };
        let end_pos = match offset_to_position(end, &rope.0) {
            Some(p) => p,
            None => continue,
        };

        // Build sorted prefix text
        let new_text = items
            .iter()
            .map(|(name, url)| format!("@prefix {}: <{}>.\n", name, url))
            .collect::<String>();
        let new_text = new_text.trim_end_matches('\n').to_string();

        let edit = TextEdit {
            range: swls_core::lsp_types::Range::new(start_pos, end_pos),
            new_text,
        };

        let mut changes = HashMap::new();
        changes.insert(label.0.clone(), vec![edit]);

        req.0.push(CodeAction {
            title: String::from("Organize Imports"),
            kind: Some(CodeActionKind::SOURCE_ORGANIZE_IMPORTS),
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                ..Default::default()
            }),
            ..Default::default()
        });
    }
}

#[cfg(test)]
mod tests {
    use swls_core::{feature::code_action::Label as CodeActionLabel, prelude::*};
    use swls_test_utils::{create_file, setup_world, TestClient};

    use super::*;

    #[test]
    fn organize_imports_produces_action_when_unsorted() {
        let (mut world, _) =
            setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        let source = "@prefix z: <http://z.org/>.\n@prefix a: <http://a.org/>.\n<> a z:Foo.\n";
        let entity = create_file(
            &mut world,
            source,
            "http://example.com/ns#",
            "turtle",
            Open,
        );

        world.run_schedule(ParseLabel);
        world.entity_mut(entity).insert(CodeActionRequest::default());
        world.run_schedule(CodeActionLabel);

        let req: Option<CodeActionRequest> = world.entity_mut(entity).take();
        let actions = req.map(|r| r.0).unwrap_or_default();

        assert_eq!(actions.len(), 1, "expected 1 code action");
        assert_eq!(actions[0].title, "Organize Imports");
    }

    #[test]
    fn organize_imports_no_action_when_sorted() {
        let (mut world, _) =
            setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        let source = "@prefix a: <http://a.org/>.\n@prefix z: <http://z.org/>.\n<> a z:Foo.\n";
        let entity = create_file(
            &mut world,
            source,
            "http://example.com/ns#",
            "turtle",
            Open,
        );

        world.run_schedule(ParseLabel);
        world.entity_mut(entity).insert(CodeActionRequest::default());
        world.run_schedule(CodeActionLabel);

        let req: Option<CodeActionRequest> = world.entity_mut(entity).take();
        let actions = req.map(|r| r.0).unwrap_or_default();

        assert_eq!(actions.len(), 0, "expected no code actions when already sorted");
    }
}
