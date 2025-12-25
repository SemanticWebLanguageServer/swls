use bevy_ecs::{schedule::IntoScheduleConfigs as _, world::World};
use lsp_core::prelude::*;
mod highlight;
pub use highlight::*;

mod parse;
use parse::derive_triples;
pub use parse::{parse_jsonld_system, parse_source};

pub fn setup_parse(world: &mut World) {
    use lsp_core::prelude::parse::*;
    world.schedule_scope(ParseLabel, |_, schedule| {
        schedule.add_systems((
            parse_source,
            parse_jsonld_system.after(parse_source),
            derive_triples
                .after(parse_jsonld_system)
                .before(triples)
                .before(prefixes),
        ));
    });
}

#[cfg(test)]
mod tests {
    use completion::CompletionRequest;
    use futures::executor::block_on;
    use lsp_core::{components::*, prelude::*, util::lsp_range_to_range, Tasks};
    use ropey::Rope;
    use test_utils::{create_file, setup_world, TestClient};
    use tracing::info;

    use crate::JsonLd;

    #[test]
    fn parse_works() {
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world);

        let t1 = r#"
{
    "@context" : { "foaf": "http://xmlns.com/foaf/0.1/" },
    "@id": "http://example.com/ns#me",
    "foaf:friend": "http://example.com/ns#you"
}"#;
        let entity = create_file(&mut world, t1, "http://example.com/ns#", "jsonld", Open);

        let tokens = world.entity(entity).get::<Tokens>().expect("tokens exists");
        let jsonld = world
            .entity(entity)
            .get::<Element<JsonLd>>()
            .expect("jsonld exists");

        assert_eq!(tokens.0.len(), 17);

        let triples = world
            .entity(entity)
            .get::<Triples>()
            .expect("triples exists");

        assert_eq!(triples.0.len(), 1);
    }

    #[test_log::test]
    fn current_triple_works() {
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world);

        let t1 = r#"{
    "@context" : { "foaf": "http://xmlns.com/foaf/0.1/" },
    "@id": "http://example.com/ns#me",
    "foaf:friend": "http://example.com/ns#you"
}"#;
        let entity = create_file(&mut world, t1, "http://example.com/ns#", "jsonld", Open);

        // start call completion
        world.entity_mut(entity).insert((
            CompletionRequest(vec![]),
            PositionComponent(lsp_core::lsp_types::Position {
                line: 3,
                character: 6,
            }),
        ));
        world.run_schedule(CompletionLabel);

        let _ = world
            .entity_mut(entity)
            .take::<TokenComponent>()
            .expect("token component");
        let triple = world
            .entity_mut(entity)
            .take::<TripleComponent>()
            .expect("triple component");

        assert_eq!(triple.target, TripleTarget::Predicate);

        world.entity_mut(entity).insert((
            CompletionRequest(vec![]),
            PositionComponent(lsp_core::lsp_types::Position {
                line: 3,
                character: 22,
            }),
        ));
        world.run_schedule(CompletionLabel);

        let _ = world
            .entity_mut(entity)
            .take::<TokenComponent>()
            .expect("token component");
        let triple = world
            .entity_mut(entity)
            .take::<TripleComponent>()
            .expect("triple component");

        assert_eq!(triple.target, TripleTarget::Object);
    }

    #[test_log::test]
    fn current_triple_works_2() {
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world);

        let t1 = r#"{
  "@context": {
    "foaf": "http://xmlns.com/foaf/0.1/"
  },
  "@id": "meee",
  "@type": "foaf:Document",
  "foa:": "foaf:Document"
}
"#;
        let entity = create_file(&mut world, t1, "http://example.com/ns#", "jsonld", Open);

        // start call completion
        world.entity_mut(entity).insert((
            CompletionRequest(vec![]),
            PositionComponent(lsp_core::lsp_types::Position {
                line: 6,
                character: 6,
            }),
        ));
        world.run_schedule(CompletionLabel);

        let _ = world
            .entity_mut(entity)
            .take::<TokenComponent>()
            .expect("token component");
        let triple = world
            .entity_mut(entity)
            .take::<TripleComponent>()
            .expect("triple component");

        assert_eq!(triple.target, TripleTarget::Predicate);
    }

    #[test_log::test]
    fn current_triple_works_corrupt() {
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world);
        lang_turtle::setup_world::<TestClient>(&mut world);

        let t1 = r#"{
    "@context" : { "foaf": "http://xmlns.com/foaf/0.1/" },
    "@id": "http://example.com/ns#me"
}"#;

        let t2 = r#"{
    "@context" : { "foaf": "http://xmlns.com/foaf/0.1/" },
    "@id": "http://example.com/ns#me",
    "foa"
}"#;
        let entity = create_file(&mut world, t1, "http://example.com/ns#", "jsonld", Open);

        let c = world.resource::<TestClient>().clone();
        block_on(c.await_futures(|| world.run_schedule(Tasks)));

        world
            .entity_mut(entity)
            .insert((Source(t2.to_string()), RopeC(Rope::from_str(t2)), Open));
        world.run_schedule(ParseLabel);

        // start call completion
        world.entity_mut(entity).insert((
            CompletionRequest(vec![]),
            PositionComponent(lsp_core::lsp_types::Position {
                line: 3,
                character: 6,
            }),
        ));
        world.run_schedule(Tasks);
        world.run_schedule(Tasks);
        world.run_schedule(CompletionLabel);

        let _ = world
            .entity_mut(entity)
            .take::<TokenComponent>()
            .expect("token component");
        let triple = world
            .entity_mut(entity)
            .take::<TripleComponent>()
            .expect("triple component");

        assert_eq!(triple.target, TripleTarget::Predicate);

        let comppletions = world
            .entity_mut(entity)
            .take::<CompletionRequest>()
            .expect("completion request")
            .0;

        let rope_c = world.entity(entity).get::<RopeC>().expect("rope component");

        for comp in &comppletions {
            let range = lsp_range_to_range(&comp.edits[0].range, &rope_c).expect("valid range");
            let txt = rope_c.slice(range).to_string();
            info!("comp {} {} -> {}", comp.label, txt, comp.edits[0].new_text);
        }

        assert_eq!(comppletions.len(), 63);
    }

    #[test_log::test]
    fn current_triple_works_corrupt_bn() {
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world);
        lang_turtle::setup_world::<TestClient>(&mut world);

        let t1 = r#"{
    "@context" : { "foaf": "http://xmlns.com/foaf/0.1/" }
}"#;

        let t2 = r#"{
    "@context" : { "foaf": "http://xmlns.com/foaf/0.1/" },
    "foa"
}"#;
        let entity = create_file(&mut world, t1, "http://example.com/ns#", "jsonld", Open);

        let c = world.resource::<TestClient>().clone();
        block_on(c.await_futures(|| world.run_schedule(Tasks)));

        world
            .entity_mut(entity)
            .insert((Source(t2.to_string()), RopeC(Rope::from_str(t2)), Open));
        world.run_schedule(ParseLabel);

        // start call completion
        world.entity_mut(entity).insert((
            CompletionRequest(vec![]),
            PositionComponent(lsp_core::lsp_types::Position {
                line: 2,
                character: 6,
            }),
        ));
        world.run_schedule(Tasks);
        world.run_schedule(Tasks);
        world.run_schedule(CompletionLabel);

        let _ = world
            .entity_mut(entity)
            .take::<TokenComponent>()
            .expect("token component");

        let triple = world
            .entity_mut(entity)
            .take::<TripleComponent>()
            .expect("triple component");

        assert_eq!(triple.target, TripleTarget::Predicate);

        let comppletions = world
            .entity_mut(entity)
            .take::<CompletionRequest>()
            .expect("completion request")
            .0;

        let rope_c = world.entity(entity).get::<RopeC>().expect("rope component");

        for comp in &comppletions {
            let range = lsp_range_to_range(&comp.edits[0].range, &rope_c).expect("valid range");
            let txt = rope_c.slice(range).to_string();
            info!("comp {} {} -> {}", comp.label, txt, comp.edits[0].new_text);
        }

        assert_eq!(comppletions.len(), 63);
    }
}
