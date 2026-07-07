use bevy_ecs::{prelude::*, world::World};
use completion::{infer_predicate_position_from_cst, subject_completion};
use format::format_turtle_system;
use swls_core::prelude::*;

use crate::TurtleLang;

mod code_action;
mod completion;
mod format;
pub mod parse;

pub fn setup_parsing(world: &mut World) {
    use swls_core::feature::parse::*;
    world.schedule_scope(ParseLabel, |_, schedule| {
        schedule.add_systems((
            parse::parse_turtle_system,
            swls_lang_rdf_base::parse::derive_prefixes_system::<TurtleLang>
                .after(parse::parse_turtle_system)
                .before(prefixes),
            parse::derive_triples_system::<TurtleLang>
                .after(parse::parse_turtle_system)
                .before(triples),
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
            subject_completion.after(generate_completions),
            infer_predicate_position_from_cst.after(generate_completions),
        ));
    });
}

#[cfg(test)]
mod tests {
    use futures::executor::block_on;
    use swls_core::text::LineIndex;
    use swls_core::{
        components::*,
        prelude::{diagnostics::DiagnosticItem, *},
    };
    use swls_test_utils::{create_file, setup_world, TestClient};
    use test_log::test;

    #[test]
    fn diagnostics_work() {
        let (mut world, mut rx) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        // t2: prefix declared AND used (foaf:foaf) — but foaf:foaf misses predicate/object → syntax error
        let t2 = "\n@prefix foaf: <>.\nfoaf:foaf\n            ";
        // t3: prefix declared but not used (foa is an invalid token, not a prefixed name)
        let t3 = "\n@prefix foaf: <>.\nfoa\n            ";

        let mut last_diags = move || -> Vec<swls_core::lsp_types::Diagnostic> {
            let mut items: Vec<DiagnosticItem> = Vec::new();
            while let Ok(Some(x)) = rx.try_next() {
                items.push(x);
            }
            items
                .into_iter()
                .last()
                .map(|i| i.diagnostics)
                .unwrap_or_default()
        };

        let entity = create_file(&mut world, t2, "http://example.com/ns#", "turtle", Open);
        world.run_schedule(ParseLabel);

        // t2: foaf IS used (foaf:foaf is a subject), but it's missing predicate+object → syntax errors
        let diags = last_diags();
        assert!(
            !diags.is_empty(),
            "t2: expected syntax errors for incomplete triple"
        );

        // t3: 'foa' is an invalid token → syntax errors
        world
            .entity_mut(entity)
            .insert((Source(t3.to_string()), RopeC(LineIndex::new(t3))));
        world.run_schedule(ParseLabel);

        let diags = last_diags();
        assert!(
            !diags.is_empty(),
            "t3: expected syntax errors for invalid token"
        );
    }

    #[test_log::test]
    fn fetch_lov_properties_test() {
        let mut client = TestClient::new();
        client.add_res("http://xmlns.com/foaf/0.1/", " @prefix foaf: <>. ");
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        let t1 = " @prefix foaf: <http://xmlns.com/foaf/0.1/>.";
        create_file(&mut world, t1, "http://example.com/ns#", "turtle", Open);

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

/// Prefix-usage diagnostics must see prefixes that appear only in a literal's
/// datatype (`"5"^^xsd:integer`).  These exercise the model-based
/// `prefix_diagnostic_helper` directly, since the derived triples store datatypes
/// pre-expanded and cannot express the prefix.
#[cfg(test)]
mod prefix_datatype_tests {
    use swls_core::lsp_types::Url;
    use swls_core::prelude::*;
    use swls_core::systems::prefix_diagnostic_helper;
    use swls_core::text::LineIndex;

    use crate::lang::parser::parse_new;

    const BASE: &str = "http://example.com/";

    fn run(src: &str, declared: &[&str], report_unused: bool) -> Vec<String> {
        let (turtle, ..) = parse_new(src, BASE, None);
        let prefixes = Prefixes(
            declared
                .iter()
                .map(|p| Prefix {
                    prefix: p.to_string(),
                    url: Url::parse("http://www.w3.org/2001/XMLSchema#").unwrap(),
                })
                .collect(),
            Url::parse(BASE).unwrap(),
        );
        let rope = RopeC(LineIndex::new(src));
        let label = Label(Url::parse(BASE).unwrap());

        let (diags, _) = prefix_diagnostic_helper(
            &turtle,
            &prefixes,
            &rope,
            &label,
            std::iter::empty(),
            std::iter::empty(),
            |_, _| Vec::new(),
            true,
            report_unused,
        );
        diags.into_iter().map(|d| d.message).collect()
    }

    #[test]
    fn datatype_prefix_counts_as_used() {
        let src = "@prefix xsd: <http://www.w3.org/2001/XMLSchema#>.\n<s> <p> \"5\"^^xsd:integer .\n";
        let msgs = run(src, &["xsd"], true);
        assert!(
            !msgs.iter().any(|m| m.contains("never used")),
            "xsd used only in a datatype must not be reported unused: {msgs:?}",
        );
    }

    #[test]
    fn undefined_datatype_prefix_is_flagged() {
        let src = "<s> <p> \"5\"^^xsd:integer .\n";
        let msgs = run(src, &[], false);
        assert!(
            msgs.iter().any(|m| m.contains("Undefined prefix \"xsd\"")),
            "xsd in a datatype but undeclared must be flagged: {msgs:?}",
        );
    }
}
