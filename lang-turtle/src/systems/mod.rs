use bevy_ecs::{schedule::IntoSystemConfigs as _, system::Query, world::World};
use completion::{subject_completion, turtle_lov_undefined_prefix_completion};
use formatting::format_turtle_system;
use lsp_core::feature::*;
use parsing::{derive_triples, parse_source, parse_turtle_system};

mod completion;
mod formatting;
mod parsing;

pub fn setup_parsing(world: &mut World) {
    use lsp_core::feature::parse::*;
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

pub fn setup_completion(world: &mut World) {
    use lsp_core::feature::completion::*;
    world.schedule_scope(CompletionLabel, |_, schedule| {
        schedule.add_systems((
            turtle_lov_undefined_prefix_completion.after(get_current_token),
            subject_completion.after(get_current_token),
        ));
    });
}

use bevy_ecs::prelude::*;
use lsp_core::components::*;

use crate::TurtleLang;

fn derive_prefixes(
    query: Query<(Entity, &Label, &Element<TurtleLang>), Changed<Element<TurtleLang>>>,
    mut commands: Commands,
) {
    for (entity, url, turtle) in &query {
        let prefixes: Vec<_> = turtle
            .prefixes
            .iter()
            .flat_map(|prefix| {
                let url = prefix.value.expand(turtle.value())?;
                let url = lsp_types::Url::parse(&url).ok()?;
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
                    .expand(turtle.value())
                    .and_then(|x| lsp_types::Url::parse(&x).ok())
            })
            .unwrap_or(url.0.clone());

        commands.entity(entity).insert(Prefixes(prefixes, base));
    }
}

#[cfg(test)]
mod tests {
    use chumsky::chain::Chain;
    use futures::executor::block_on;
    use lsp_core::{components::*, prelude::DiagnosticItem, Diagnostics, Parse};
    use ropey::Rope;
    use test_utils::{create_file, setup_world, TestClient};

    #[test]
    fn diagnostics_work() {
        let (mut world, mut rx) = setup_world(TestClient::new(), crate::setup_world);

        let t1 = "
@prefix foaf: <>.
            ";

        let t2 = "
@prefix foaf: <>.
foaf:foaf
            ";

        let t3 = "
@prefix foaf: <>.
foa
            ";

        let entity = create_file(&mut world, t1, "http://example.com/ns#", "turtle", Open);
        world.run_schedule(Parse);
        world.run_schedule(Diagnostics);

        let mut get_diagnostics = move || {
            let mut out: Vec<DiagnosticItem> = Vec::new();
            while let Ok(Some(x)) = rx.try_next() {
                out.push(x);
            }
            out
        };
        let items = get_diagnostics();
        assert!(items[0].diagnostics.is_empty());

        world
            .entity_mut(entity)
            .insert((Source(t2.to_string()), RopeC(Rope::from_str(t2))));
        world.run_schedule(Parse);
        world.run_schedule(Diagnostics);

        let items = get_diagnostics();
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].diagnostics.len(), 2);
        let msgs: Vec<_> = items[0].diagnostics.iter().map(|x| &x.message).collect();
        world
            .entity_mut(entity)
            .insert((Source(t3.to_string()), RopeC(Rope::from_str(t2))));
        world.run_schedule(Parse);
        world.run_schedule(Diagnostics);

        let items = get_diagnostics();
        let msgs: Vec<_> = items[0].diagnostics.iter().map(|x| &x.message).collect();
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].diagnostics.len(), 5);
    }

    #[test_log::test]
    fn fetch_lov_properties_test() {
        let mut client = TestClient::new();
        client.add_res("http://xmlns.com/foaf/0.1/", " @prefix foaf: <>. ");
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world);

        let t1 = " @prefix foaf: <http://xmlns.com/foaf/0.1/>.";
        create_file(&mut world, t1, "http://example.com/ns#", "turtle", Open);

        // assert_eq!(world.entities().len(), 1);
        let c = world.resource::<TestClient>().clone();
        block_on(c.await_futures(|| world.run_schedule(lsp_core::Parse)));

        assert_eq!(world.entities().len(), 2);
    }

    #[test]
    fn turtle_does_prefix_links() {
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world);

        let t1 = " @prefix foaf: <http://xmlns.com/foaf/0.1/>.";
        let entity = create_file(&mut world, t1, "http://example.com/ns#", "turtle", Open);

        let links: &DocumentLinks = world.entity(entity).get().expect("document links exists");
        assert_eq!(links.len(), 1);
        assert_eq!(links[0].0.as_str(), "http://xmlns.com/foaf/0.1/");
        assert_eq!(links[0].1, "prefix import");
    }
}
