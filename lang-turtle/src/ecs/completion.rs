use bevy_ecs::prelude::*;
use completion::{CompletionRequest, SimpleCompletion};
use swls_lov::LocalPrefix;
use swls_core::lsp_types::CompletionItemKind;
use swls_core::systems::PrefixEntry;
use swls_core::util::triple::{MyQuad, MyTerm, TripleComponent, TripleTarget};
use swls_core::{components::*, prelude::*, systems::prefix::prefix_completion_helper};
use tracing::debug;

use crate::{lang::model::{NamedNodeExt, TurtleExt}, TurtleLang};

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

/// Fallback system that detects predicate position from CST tokens when the parser did not
/// produce a complete enough triple for `get_current_triple` to identify the context.
///
/// This covers the case where only a subject has been written (`<> ` with the cursor after it)
/// and nothing of the predicate has been typed yet. The system looks at the last non-whitespace
/// CST token before the cursor: if it is a subject-like token (IRI, prefixed name, blank node)
/// and the cursor is strictly past its end (i.e. there is whitespace between them), it inserts
/// a synthetic [`TripleComponent`] with [`TripleTarget::Predicate`] so that the downstream
/// property/class completion systems can offer suggestions.
pub fn infer_predicate_position_from_cst(
    query: Query<
        (
            Entity,
            &CstTokens,
            &PositionComponent,
            &RopeC,
            &Source,
            &Element<TurtleLang>,
        ),
        (With<TurtleLang>, Without<TripleComponent>),
    >,
    mut commands: Commands,
) {
    use rdf_parsers::turtle::SyntaxKind as TSK;

    // Raw u16 values for token kinds that can appear as the subject of a triple.
    const SUBJECT_KINDS: &[u16] = &[
        TSK::Iriref as u16,
        TSK::PnameLn as u16,
        TSK::PnameNs as u16,
        TSK::BlankNodeLabel as u16,
    ];

    for (entity, cst_tokens, position, rope, source, element) in &query {
        let Some(offset) = position_to_offset(position.0, &rope.0) else {
            continue;
        };

        // Find the last non-whitespace CST token whose span ends at or before the cursor.
        let Some(preceding) = cst_tokens
            .0
            .iter()
            .filter(|t| t.span().end <= offset)
            .max_by_key(|t| t.span().end)
        else {
            continue;
        };

        // If the cursor is exactly at the token's end, the user might still be editing it.
        if preceding.span().end == offset {
            continue;
        }

        // Only proceed when the preceding token looks like a subject.
        if !SUBJECT_KINDS.contains(&preceding.value().0) {
            continue;
        }

        let subj_span = preceding.span().clone();
        if subj_span.end > source.0.len() {
            continue;
        }

        let turtle = element.0.value();
        let subj_text = &source.0[subj_span.clone()];

        // Attempt to expand the subject to a full IRI for type-aware completions.
        let subj_value = expand_subject_iri(subj_text, turtle)
            .unwrap_or_else(|| subj_text.to_string());

        debug!(
            "Inferred Predicate position from CST: subject='{}' offset={}",
            subj_value, offset
        );

        commands.entity(entity).insert(TripleComponent {
            triple: MyQuad {
                subject: MyTerm::named_node(subj_value, subj_span),
                predicate: MyTerm::invalid(offset..offset),
                object: MyTerm::invalid(offset..offset),
                span: preceding.span().start..offset,
            },
            target: TripleTarget::Predicate,
        });
    }
}

fn expand_subject_iri(
    text: &str,
    turtle: &rdf_parsers::model::Turtle,
) -> Option<String> {
    use rdf_parsers::model::NamedNode;

    if text.starts_with('<') && text.ends_with('>') {
        let inner = text[1..text.len() - 1].to_string();
        let nn = NamedNode::Full(inner, 0);
        return NamedNodeExt::expand(&nn, turtle);
    }

    if let Some(colon_pos) = text.find(':') {
        let prefix = text[..colon_pos].to_string();
        let value = text[colon_pos + 1..].to_string();
        let nn = NamedNode::Prefixed { prefix, value, idx: 0 };
        return NamedNodeExt::expand(&nn, turtle);
    }

    None
}

#[cfg(test)]
mod tests {

    use completion::CompletionRequest;
    use futures::executor::block_on;
    use swls_core::{components::*, lang::LangHelper, prelude::*, util::triple::{TripleComponent, TripleTarget}, Tasks};
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
    fn triple_target_predicate_position() {
        // Verifies that get_current_triple correctly identifies Predicate when
        // the cursor is on the predicate token (e.g. `foaf:name`).
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        // line 0: "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n"  (45 bytes)
        // line 1: "<> foaf:name \"Arthur\"."
        //          01234567890123456789012
        //  cursor at char 3 → 'f' in foaf:name → predicate
        let t1 = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n<> foaf:name \"Arthur\".";

        let entity = create_file(&mut world, t1, "http://example.com/ns#", "turtle", Open);
        world.run_schedule(ParseLabel);

        world.entity_mut(entity).insert((
            CompletionRequest(vec![]),
            PositionComponent(swls_core::lsp_types::Position {
                line: 1,
                character: 3, // 'f' in foaf:name
            }),
        ));
        world.run_schedule(CompletionLabel);

        let triple_comp = world.entity(entity).get::<TripleComponent>();
        assert!(triple_comp.is_some(), "TripleComponent must be set when cursor is on a predicate");
        let triple_comp = triple_comp.unwrap();
        println!("target: {:?}  predicate: {}", triple_comp.target, triple_comp.triple.predicate.value);
        assert_eq!(
            triple_comp.target,
            TripleTarget::Predicate,
            "Expected Predicate target at cursor on foaf:name, got {:?}",
            triple_comp.target
        );
    }

    #[test_log::test]
    fn triple_target_object_position() {
        // Verifies that get_current_triple correctly identifies Object when
        // the cursor is on the object token (e.g. `\"Arthur\"`).
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        // line 1: "<> foaf:name \"Arthur\"."
        //  cursor at char 13 → '"' in "Arthur" → object
        let t1 = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n<> foaf:name \"Arthur\".";

        let entity = create_file(&mut world, t1, "http://example.com/ns#", "turtle", Open);
        world.run_schedule(ParseLabel);

        world.entity_mut(entity).insert((
            CompletionRequest(vec![]),
            PositionComponent(swls_core::lsp_types::Position {
                line: 1,
                character: 13, // '"' at start of "Arthur"
            }),
        ));
        world.run_schedule(CompletionLabel);

        let triple_comp = world.entity(entity).get::<TripleComponent>();
        assert!(triple_comp.is_some(), "TripleComponent must be set when cursor is on an object");
        let triple_comp = triple_comp.unwrap();
        println!("target: {:?}  object: {}", triple_comp.target, triple_comp.triple.object.value);
        assert_eq!(
            triple_comp.target,
            TripleTarget::Object,
            "Expected Object target at cursor on \"Arthur\", got {:?}",
            triple_comp.target
        );
    }

    #[test_log::test]
    fn triple_target_subject_position() {
        // Verifies that get_current_triple correctly identifies Subject when
        // the cursor is on the subject token (e.g. `<>`).
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        // line 1: "<> foaf:name \"Arthur\"."
        //  cursor at char 0 → '<' in <> → subject
        let t1 = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n<> foaf:name \"Arthur\".";

        let entity = create_file(&mut world, t1, "http://example.com/ns#", "turtle", Open);
        world.run_schedule(ParseLabel);

        world.entity_mut(entity).insert((
            CompletionRequest(vec![]),
            PositionComponent(swls_core::lsp_types::Position {
                line: 1,
                character: 0, // '<' in <>
            }),
        ));
        world.run_schedule(CompletionLabel);

        let triple_comp = world.entity(entity).get::<TripleComponent>();
        assert!(triple_comp.is_some(), "TripleComponent must be set when cursor is on a subject");
        let triple_comp = triple_comp.unwrap();
        println!("target: {:?}  subject: {}", triple_comp.target, triple_comp.triple.subject.value);
        assert_eq!(
            triple_comp.target,
            TripleTarget::Subject,
            "Expected Subject target at cursor on <>, got {:?}",
            triple_comp.target
        );
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

    // ── New tests for "nothing typed yet" completion trigger ──────────────────

    /// Verifies that property completions are offered when the cursor is immediately after
    /// a subject IRI with no predicate written yet (`<> |`).
    #[test_log::test]
    fn triple_target_predicate_position_nothing_written() {
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        // line 0: "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n"
        // line 1: "<> "   cursor at char 3 → past the subject, predicate not written
        let t1 = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n<> ";

        let entity = create_file(&mut world, t1, "http://example.com/ns#", "turtle", Open);
        world.run_schedule(ParseLabel);

        world.entity_mut(entity).insert((
            CompletionRequest(vec![]),
            PositionComponent(swls_core::lsp_types::Position {
                line: 1,
                character: 3,
            }),
        ));
        world.run_schedule(CompletionLabel);

        let triple_comp = world.entity(entity).get::<TripleComponent>();
        assert!(
            triple_comp.is_some(),
            "TripleComponent must be set when cursor is in predicate position (nothing typed)"
        );
        assert_eq!(
            triple_comp.unwrap().target,
            TripleTarget::Predicate,
            "Expected Predicate target when cursor is after subject with nothing typed"
        );
    }

    /// Verifies that object completions are offered when cursor is after `foaf:name ` with
    /// no object written yet (predicate written, object position, nothing typed).
    #[test_log::test]
    fn triple_target_object_position_nothing_written() {
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        // line 1: "<> foaf:name "  cursor at char 13 → past the predicate, object not written
        let t1 = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n<> foaf:name ";

        let entity = create_file(&mut world, t1, "http://example.com/ns#", "turtle", Open);
        world.run_schedule(ParseLabel);

        world.entity_mut(entity).insert((
            CompletionRequest(vec![]),
            PositionComponent(swls_core::lsp_types::Position {
                line: 1,
                character: 13,
            }),
        ));
        world.run_schedule(CompletionLabel);

        let triple_comp = world.entity(entity).get::<TripleComponent>();
        assert!(
            triple_comp.is_some(),
            "TripleComponent must be set when cursor is in object position (nothing typed)"
        );
        assert_eq!(
            triple_comp.unwrap().target,
            TripleTarget::Object,
            "Expected Object target when cursor is after predicate with nothing typed"
        );
    }

    /// Verifies that after a semicolon (`;`) the cursor is correctly identified as being
    /// in predicate position for the same subject.
    #[test_log::test]
    fn triple_target_predicate_position_after_semicolon() {
        let (mut world, _) = setup_world(TestClient::new(), crate::setup_world::<TestClient>);

        // line 1: "<> foaf:name \"Arthur\"; "  cursor at char 23 → after `;`, new predicate
        let t1 = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n<> foaf:name \"Arthur\"; ";

        let entity = create_file(&mut world, t1, "http://example.com/ns#", "turtle", Open);
        world.run_schedule(ParseLabel);

        world.entity_mut(entity).insert((
            CompletionRequest(vec![]),
            PositionComponent(swls_core::lsp_types::Position {
                line: 1,
                character: 23,
            }),
        ));
        world.run_schedule(CompletionLabel);

        let triple_comp = world.entity(entity).get::<TripleComponent>();
        assert!(
            triple_comp.is_some(),
            "TripleComponent must be set after a semicolon"
        );
        assert_eq!(
            triple_comp.unwrap().target,
            TripleTarget::Predicate,
            "Expected Predicate target after semicolon"
        );
    }
}
