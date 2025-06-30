use bevy_ecs::prelude::*;
use completion::{CompletionRequest, SimpleCompletion};
use lov::LocalPrefix;
use lsp_core::{components::*, prelude::*, systems::prefix::prefix_completion_helper};
use lsp_types::CompletionItemKind;
use tracing::debug;

use crate::{lang::model::NamedNode, TurtleLang};

pub fn turtle_lov_undefined_prefix_completion(
    mut query: Query<(
        &TokenComponent,
        &Element<TurtleLang>,
        &Prefixes,
        &mut CompletionRequest,
    )>,
    lovs: Query<&LocalPrefix>,
) {
    for (word, turtle, prefixes, mut req) in &mut query {
        let mut start = Position::new(0, 0);

        if turtle.base.is_some() {
            start = Position::new(1, 0);
        }

        use lsp_types::{Position, Range};
        prefix_completion_helper(
            word,
            prefixes,
            &mut req.0,
            |name, location| {
                Some(vec![lsp_types::TextEdit {
                    range: Range::new(start.clone(), start),
                    new_text: format!("@prefix {}: <{}>.\n", name, location),
                }])
            },
            lovs.iter(),
        );
    }
}

pub fn subject_completion(
    mut query: Query<(
        &TokenComponent,
        &Element<TurtleLang>,
        &mut CompletionRequest,
    )>,
    triples: Query<(&Triples, &Label), With<Open>>,
) {
    for (word, turtle, mut req) in &mut query {
        let m_expaned = match word.token.value() {
            Token::PNameLN(pref, value) => NamedNode::Prefixed {
                prefix: pref.clone().unwrap_or_default(),
                value: value.clone(),
                idx: 0,
            }
            .expand(turtle.0.value()),
            _ => continue,
        };
        let Some(expanded) = m_expaned else { continue };

        for (triples, label) in &triples {
            for triple in &triples.0 {
                debug!("Triple {} start with {}", triple.subject.as_str(), expanded);
                let subj = triple.subject.as_str();
                if subj.starts_with(&expanded) {
                    let new_text = turtle.0.shorten(subj).unwrap_or_else(|| String::from(subj));

                    if new_text != word.text {
                        req.push(
                            SimpleCompletion::new(
                                CompletionItemKind::MODULE,
                                subj.to_string(),
                                lsp_types::TextEdit {
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
    use lsp_core::{components::*, lang::LangHelper, prelude::*, Tasks};
    use ropey::Rope;
    use test_log::test;
    use test_utils::{create_file, setup_world, TestClient};
    use tracing::info;

    use crate::TurtleHelper;

    #[test]
    fn completion_event_works() {
        println!("completion_event_works");
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world);

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
            PositionComponent(lsp_types::Position {
                line: 2,
                character: 0,
            }),
        ));

        world.run_schedule(CompletionLabel);
        let m_completions = world.entity_mut(entity).take::<CompletionRequest>();

        assert!(m_completions.is_some());
        let completions = m_completions.unwrap().0;
        assert_eq!(completions.len(), 4 + TurtleHelper.keyword().len());
    }

    #[test_log::test]
    fn completion_event_works_multiple_files() {
        info!("Testing multiple files");
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world);
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
            PositionComponent(lsp_types::Position {
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

        assert_eq!(completions.len(), 1 + TurtleHelper.keyword().len());
    }

    #[test_log::test]
    fn test_autocomplete_classes() {
        println!("completion_event_works");
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world);

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
            PositionComponent(lsp_types::Position {
                line: 1,
                character: 6,
            }),
        ));
        world.run_schedule(CompletionLabel);
        let completions = world
            .entity_mut(entity)
            .take::<CompletionRequest>()
            .expect("competion request")
            .0;

        for c in &completions {
            println!("c {:?} {:?}", c.label, c._documentation);
        }
        assert_eq!(
            completions.len(),
            4 /* prefix.cc */ + 14 /*completions */ + TurtleHelper.keyword().len()
        );
    }

    #[test_log::test]
    fn test_autocomplete_properties_3() {
        println!("completion_event_works");
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world);

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
            PositionComponent(lsp_types::Position {
                line: 1,
                character: 4,
            }),
        ));
        world.run_schedule(CompletionLabel);
        let completions = world
            .entity_mut(entity)
            .take::<CompletionRequest>()
            .expect("competion request")
            .0;

        assert_eq!(completions.len(), 62 + TurtleHelper.keyword().len());
    }

    #[test_log::test]
    fn test_autocomplete_properties_2() {
        println!("completion_event_works");
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world);

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
            PositionComponent(lsp_types::Position {
                line: 2,
                character: 5,
            }),
        ));
        world.run_schedule(CompletionLabel);
        let completions = world
            .entity_mut(entity)
            .take::<CompletionRequest>()
            .expect("competion request")
            .0;

        assert_eq!(completions.len(), 62 + TurtleHelper.keyword().len());
    }
}
