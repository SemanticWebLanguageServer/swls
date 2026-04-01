use bevy_ecs::prelude::*;
use completion::{CompletionRequest, SimpleCompletion};
use swls_lov::LocalPrefix;
use swls_core::lsp_types::CompletionItemKind;
use swls_core::systems::PrefixEntry;
use swls_core::{components::*, prelude::*, systems::prefix::prefix_completion_helper};
use tracing::debug;

use crate::{lang::model::TurtleExt, TurtleLang};

pub fn turtle_lov_undefined_prefix_completion(
    mut query: Query<(
        &TokenComponent,
        &Element<TurtleLang>,
        &Prefixes,
        &mut CompletionRequest,
    )>,
    lovs: Query<&LocalPrefix>,
    prefix_cc: Query<&PrefixEntry>,
    config: Res<ServerConfig>,
) {
    for (word, turtle, prefixes, mut req) in &mut query {
        let mut start = swls_core::lsp_types::Position::new(0, 0);

        if turtle.base.is_some() {
            start = swls_core::lsp_types::Position::new(1, 0);
        }

        use swls_core::lsp_types::Range;
        prefix_completion_helper(
            word,
            prefixes,
            &mut req.0,
            |name, location| {
                Some(vec![swls_core::lsp_types::TextEdit {
                    range: Range::new(start.clone(), start),
                    new_text: format!("@prefix {}: <{}>.\n", name, location),
                }])
            },
            lovs.iter(),
            prefix_cc.iter(),
            &config.config.local,
        );
    }
}

pub fn subject_completion(
    mut query: Query<(
        &TokenComponent,
        &Element<TurtleLang>,
        &Prefixes,
        &mut CompletionRequest,
    )>,
    triples: Query<(&Triples, &Label), With<Open>>,
) {
    for (word, turtle, prefixes, mut req) in &mut query {
        // Only attempt subject completion when the text looks like a prefixed name.
        let text = &word.text;
        let Some(colon_pos) = text.find(':') else {
            continue;
        };
        let pref_name = &text[..colon_pos];
        let local = &text[colon_pos + 1..];

        // Find the declared prefix URL
        let Some(prefix) = prefixes.0.iter().find(|p| p.prefix == pref_name) else {
            continue;
        };
        let expanded = format!("{}{}", prefix.url, local);

        for (triples, label) in &triples {
            for triple in triples.0.iter() {
                debug!("Triple {} start with {}", triple.subject.as_str(), expanded);
                let subj = triple.subject.as_str();
                if subj.starts_with(&expanded) {
                    let new_text = turtle.0.shorten(subj).unwrap_or_else(|| String::from(subj));

                    if new_text != *text {
                        req.push(
                            SimpleCompletion::new(
                                CompletionItemKind::MODULE,
                                subj.to_string(),
                                swls_core::lsp_types::TextEdit {
                                    new_text,
                                    range: word.range.clone(),
                                },
                            )
                            .documentation(format!("Subject from {}", label.0)),
                        );
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use completion::CompletionRequest;
    use futures::executor::block_on;
    use swls_core::{components::*, lang::LangHelper, prelude::*, Tasks};
    use ropey::Rope;
    use test_log::test;
    use swls_test_utils::{create_file, setup_world, TestClient};
    use tracing::info;

    use crate::TurtleHelper;

    #[test]
    fn completion_event_works() {
        println!("completion_event_works");
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        let t1 = "
@prefix foaf: <http://xmlns.com/foaf/0.1/>.
            ";

        let t2 = "
@prefix foaf: <http://xmlns.com/foaf/0.1/>.
foa
            ";

        let entity = create_file(&mut world, t1, "http://example.com/ns#", "turtle", Open);

        world
            .entity_mut(entity)
            .insert((Source(t2.to_string()), RopeC(Rope::from_str(t2))));
        world.run_schedule(ParseLabel);

        // start call completion
        world.entity_mut(entity).insert((
            CompletionRequest(vec![]),
            PositionComponent(swls_core::lsp_types::Position {
                line: 2,
                character: 0,
            }),
        ));

        world.run_schedule(CompletionLabel);
        let m_completions = world.entity_mut(entity).take::<CompletionRequest>();

        assert!(m_completions.is_some());
        let completions = m_completions.unwrap().0;
        // keywords + prefix completions (foaf from prefix_cc that starts with "foa")
        assert!(completions.len() >= TurtleHelper.keyword().len());
    }

    #[test_log::test]
    fn completion_event_works_multiple_files() {
        info!("Testing multiple files");
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);
        let t1_1 = "
@prefix foaf: <http://xmlns.com/foaf/0.1/>.
            ";

        let t1_2 = "
@prefix foaf: <http://xmlns.com/foaf/0.1/>.
foaf:
            ";

        let t2 = "
@prefix foaf: <http://xmlns.com/foaf/0.1/>.

foaf:me foaf:friend <#me>.
            ";

        let entity = create_file(
            &mut world,
            t1_1,
            "http://example.com/first_file#",
            "turtle",
            Open,
        );

        create_file(
            &mut world,
            t2,
            "http://example.com/second_file#",
            "turtle",
            Open,
        );

        world.entity_mut(entity).insert((
            Source(t1_2.to_string()),
            RopeC(Rope::from_str(t1_2)),
            Open,
        ));
        world.run_schedule(ParseLabel);

        // start call completion
        world.entity_mut(entity).insert((
            CompletionRequest(vec![]),
            PositionComponent(swls_core::lsp_types::Position {
                line: 2,
                character: 0,
            }),
        ));
        world.run_schedule(CompletionLabel);

        let completions = world
            .entity_mut(entity)
            .take::<CompletionRequest>()
            .expect("Completions exists")
            .0;

        // keywords + defined prefix completion for "foaf:" + subject completion for "foaf:me"
        assert!(completions.len() >= TurtleHelper.keyword().len());
    }

    #[test_log::test]
    fn test_autocomplete_classes() {
        println!("completion_event_works");
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        let t1 = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.";

        let t2 = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.
<> a foa";

        let entity = create_file(&mut world, t1, "http://example.com/ns#", "turtle", Open);

        let c = world.resource::<TestClient>().clone();
        block_on(c.await_futures(|| world.run_schedule(Tasks)));

        world
            .entity_mut(entity)
            .insert((Source(t2.to_string()), RopeC(Rope::from_str(t2)), Open));
        world.run_schedule(ParseLabel);

        block_on(c.await_futures(|| world.run_schedule(Tasks)));

        // start call completion
        world.entity_mut(entity).insert((
            CompletionRequest(vec![]),
            PositionComponent(swls_core::lsp_types::Position {
                line: 1,
                character: 6,
            }),
        ));
        world.run_schedule(CompletionLabel);
        let completions = world
            .entity_mut(entity)
            .take::<CompletionRequest>()
            .expect("completion request")
            .0;

        for c in &completions {
            println!("c {:?} {:?}", c.label, c._documentation);
        }
        // keywords + prefix.cc completions starting with "foa"
        assert!(completions.len() >= TurtleHelper.keyword().len());
    }

    #[test_log::test]
    fn test_autocomplete_properties_3() {
        println!("completion_event_works");
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        let t1 = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.";

        let t2 = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.
<> foaf:";

        let entity = create_file(&mut world, t1, "http://example.com/ns#", "turtle", Open);

        let c = world.resource::<TestClient>().clone();
        block_on(c.await_futures(|| world.run_schedule(Tasks)));

        world
            .entity_mut(entity)
            .insert((Source(t2.to_string()), RopeC(Rope::from_str(t2)), Open));
        world.run_schedule(ParseLabel);

        block_on(c.await_futures(|| world.run_schedule(Tasks)));

        // start call completion
        world.entity_mut(entity).insert((
            CompletionRequest(vec![]),
            PositionComponent(swls_core::lsp_types::Position {
                line: 1,
                character: 4,
            }),
        ));
        world.run_schedule(CompletionLabel);
        let completions = world
            .entity_mut(entity)
            .take::<CompletionRequest>()
            .expect("completion request")
            .0;

        assert!(completions.len() >= TurtleHelper.keyword().len());
    }

    #[test_log::test]
    fn test_autocomplete_properties_2() {
        println!("completion_event_works");
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        let t1 = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.
<> a foaf:Person;
    foaf:name \"Arthur\".";

        let t2 = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.
<> a foaf:Person;
    foaf:
    foaf:name \"Arthur\".";

        let entity = create_file(&mut world, t1, "http://example.com/ns#", "turtle", Open);

        let c = world.resource::<TestClient>().clone();
        block_on(c.await_futures(|| world.run_schedule(Tasks)));

        world
            .entity_mut(entity)
            .insert((Source(t2.to_string()), RopeC(Rope::from_str(t2)), Open));
        world.run_schedule(ParseLabel);

        block_on(c.await_futures(|| world.run_schedule(Tasks)));

        // start call completion
        world.entity_mut(entity).insert((
            CompletionRequest(vec![]),
            PositionComponent(swls_core::lsp_types::Position {
                line: 2,
                character: 5,
            }),
        ));
        world.run_schedule(CompletionLabel);
        let completions = world
            .entity_mut(entity)
            .take::<CompletionRequest>()
            .expect("completion request")
            .0;

        assert!(completions.len() >= TurtleHelper.keyword().len());
    }
}
