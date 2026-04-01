use bevy_ecs::{prelude::*, system::Query, world::World};
use completion::{subject_completion, turtle_lov_undefined_prefix_completion};
use format::format_turtle_system;
use swls_core::prelude::*;
use parse::{derive_triples, parse_source, parse_turtle_system};

use crate::{lang::model::NamedNodeExt, TurtleLang};

mod code_action;
mod completion;
mod format;
mod parse;

pub fn setup_parsing(world: &mut World) {
    use swls_core::feature::parse::*;
    world.schedule_scope(ParseLabel, |_, schedule| {
        schedule.add_systems((
            parse_source,
            parse_turtle_system.after(parse_source),
            derive_prefixes.after(parse_turtle_system).before(prefixes),
            derive_triples.after(parse_turtle_system).before(triples),
        ));
    });
}

pub fn setup_formatting(world: &mut World) {
    world.schedule_scope(FormatLabel, |_, schedule| {
        schedule.add_systems(format_turtle_system);
    });
}

pub fn setup_code_action(world: &mut World) {
    use swls_core::feature::code_action::Label as CodeActionLabel;
    world.schedule_scope(CodeActionLabel, |_, schedule| {
        schedule.add_systems(code_action::organize_imports);
    });
}

pub fn setup_completion(world: &mut World) {
    use swls_core::feature::completion::*;
    world.schedule_scope(CompletionLabel, |_, schedule| {
        schedule.add_systems((
            turtle_lov_undefined_prefix_completion.after(get_current_token),
            subject_completion.after(get_current_token),
        ));
    });
}

fn derive_prefixes(
    query: Query<(Entity, &Label, &Element<TurtleLang>), Changed<Element<TurtleLang>>>,
    mut commands: Commands,
) {
    for (entity, url, turtle) in &query {
        let prefixes: Vec<_> = turtle
            .prefixes
            .iter()
            .flat_map(|prefix| {
                let url = prefix.value.value().expand(turtle.value())?;
                let url = swls_core::lsp_types::Url::parse(&url).ok()?;
                Some(Prefix {
                    url,
                    prefix: prefix.prefix.value().clone(),
                })
            })
            .collect();

        let base = turtle
            .base
            .as_ref()
            .and_then(|b| {
                b.0 .1
                    .value()
                    .expand(turtle.value())
                    .and_then(|x| swls_core::lsp_types::Url::parse(&x).ok())
            })
            .unwrap_or(url.0.clone());

        commands.entity(entity).insert(Prefixes(prefixes, base));
    }
}

#[cfg(test)]
mod tests {
    use futures::executor::block_on;
    use swls_core::{
        components::*,
        prelude::{diagnostics::DiagnosticItem, *},
    };
    use ropey::Rope;
    use test_log::test;
    use swls_test_utils::{create_file, setup_world, TestClient};

    #[test]
    fn diagnostics_work() {
        let (mut world, mut rx) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        // t1: prefix declared but never used
        let t1 = "\n@prefix foaf: <>.\n            ";
        // t2: prefix declared AND used (foaf:foaf) — but foaf:foaf misses predicate/object → syntax error
        let t2 = "\n@prefix foaf: <>.\nfoaf:foaf\n            ";
        // t3: prefix declared but not used (foa is an invalid token, not a prefixed name)
        let t3 = "\n@prefix foaf: <>.\nfoa\n            ";

        // Drain all published diagnostic items and return the last (most complete) merged set.
        let mut last_diags = move || -> Vec<swls_core::lsp_types::Diagnostic> {
            let mut items: Vec<DiagnosticItem> = Vec::new();
            while let Ok(Some(x)) = rx.try_next() {
                items.push(x);
            }
            items.into_iter().last().map(|i| i.diagnostics).unwrap_or_default()
        };

        let entity = create_file(&mut world, t1, "http://example.com/ns#", "turtle", Open);
        world.run_schedule(ParseLabel);
        world.run_schedule(DiagnosticsLabel);

        // t1: the declared prefix 'foaf' is never used → exactly one unused-prefix warning
        let diags = last_diags();
        let unused: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("declared but never used"))
            .collect();
        assert_eq!(unused.len(), 1, "t1: expected 1 unused prefix warning");
        let other: Vec<_> = diags
            .iter()
            .filter(|d| !d.message.contains("declared but never used"))
            .collect();
        assert!(other.is_empty(), "t1: expected no other diagnostics");

        // t2: foaf IS used, but foaf:foaf is missing predicate and object → syntax errors, no warning
        world
            .entity_mut(entity)
            .insert((Source(t2.to_string()), RopeC(Rope::from_str(t2))));
        world.run_schedule(ParseLabel);
        world.run_schedule(DiagnosticsLabel);

        let diags = last_diags();
        let unused: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("declared but never used"))
            .collect();
        assert_eq!(unused.len(), 0, "t2: prefix is used, expected no unused warning");
        assert!(!diags.is_empty(), "t2: expected syntax errors");

        // t3: 'foa' is an invalid token (not a prefixed name), so foaf is again unused → warning + token error
        world
            .entity_mut(entity)
            .insert((Source(t3.to_string()), RopeC(Rope::from_str(t3))));
        world.run_schedule(ParseLabel);
        world.run_schedule(DiagnosticsLabel);

        let diags = last_diags();
        let unused: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("declared but never used"))
            .collect();
        assert_eq!(unused.len(), 1, "t3: foaf unused again, expected 1 warning");
    }

    #[test_log::test]
    fn fetch_lov_properties_test() {
        let mut client = TestClient::new();
        client.add_res("http://xmlns.com/foaf/0.1/", " @prefix foaf: <>. ");
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        let t1 = " @prefix foaf: <http://xmlns.com/foaf/0.1/>.";
        create_file(&mut world, t1, "http://example.com/ns#", "turtle", Open);

        // assert_eq!(world.entities().len(), 1);
        let c = world.resource::<TestClient>().clone();
        block_on(c.await_futures(|| world.run_schedule(swls_core::feature::ParseLabel)));

        // We added 3 ontologies that are always present
        assert!(world.entities().len() >= 2 + 3);
    }

    #[test]
    fn turtle_does_prefix_links() {
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        let t1 = " @prefix foaf: <http://xmlns.com/foaf/0.1/>.";
        let entity = create_file(&mut world, t1, "http://example.com/ns#", "turtle", Open);

        let links: &DocumentLinks = world.entity(entity).get().expect("document links exists");
        assert_eq!(links.len(), 1);
        assert_eq!(links[0].0.as_str(), "file:///tmp/swls/test/foaf.ttl");
        assert_eq!(links[0].1, "prefix import");
    }
}
