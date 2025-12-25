#![doc(
    html_logo_url = "https://ajuvercr.github.io/semantic-web-lsp/assets/icons/favicon.png",
    html_favicon_url = "https://ajuvercr.github.io/semantic-web-lsp/assets/icons/favicon.ico"
)]
//! Core and common implementation for the semantic web language server.
//!
//! Proivdes the backbone for the [semantic web lsp binary](../lsp_bin/index.html) and [semantic web
//! lsp wasm](../lsp_web/index.html).
//!
//! With the language server protocol, each different request is handled by an ECS schedule,
//! combining different systems together.
//! A system can generate new data and attach it to an entity, a document, or use this data to
//! respond to requests.
//!
//! Language specific implementations that handle things like tokenizing and parsing are
//! implemented in separate crates. The binary currently supports [Turtle](../lang_turtle/index.html), [JSON-LD](../lang_jsonld/index.html) and [SPARQL](../lang_sparql/index.html).
//! The goal is that each language at least generates [`Tokens`], [`Triples`] and
//! [`Prefixes`].
//! These components are then used to derive properties for autcompletion but also derive
//! [`TokenComponent`] and [`TripleComponent`] enabling completion.
//!
//! The different schedules can be found at [`prelude::feature`].
//!
//! ## Example add completion for all subjects that start with `a`
//! ```
//! use bevy_ecs::prelude::*;
//! use lsp_core::prelude::*;
//! # use sophia_api::dataset::Dataset;
//! # use sophia_api::prelude::Quad;
//!
//! // Define the extra data struct
//! #[derive(Component)]
//! struct MyCompletions {
//!     subjects: Vec<String>,
//! }
//!
//! // Define the generating system
//! // Don't forget to add it to the ecs later
//! fn generate_my_completion(
//!   // Only derive the completions when the document is parsed fully, aka is not Dirty
//!   query: Query<(Entity, &Triples), (Changed<Triples>, Without<Dirty>)>,
//!   mut commands: Commands,
//! ) {
//!   for (e, triples) in &query {
//!     let mut subjects = Vec::new();
//!     for q in triples.quads().flatten() {
//!       if q.s().as_str().starts_with('a') {
//!         subjects.push(q.s().as_str().to_string());
//!       }
//!     }
//!     commands.entity(e).insert(MyCompletions { subjects });
//!   }
//! }
//!
//! // Define a system that adds these completions to the completion request
//! fn complete_my_completion(
//!   mut query: Query<(
//!     &TokenComponent, &TripleComponent, &MyCompletions, &mut CompletionRequest
//!   )>,
//! ) {
//!   for (token, this_triple, completions, mut request) in &mut query {
//!     if this_triple.target == TripleTarget::Subject {
//!       for my_completion in &completions.subjects {
//!         request.push(
//!           SimpleCompletion::new(
//!             lsp_core::lsp_types::CompletionItemKind::FIELD,
//!             my_completion.clone(),
//!             lsp_core::lsp_types::TextEdit {
//!               range: token.range.clone(),
//!               new_text: my_completion.clone(),
//!             }
//!           )
//!         )
//!       }
//!     }
//!   }
//! }
//! ```
//! Note that [`Prefixes`] can help expand and shorten iri's in a document.
//!
//!

use bevy_ecs::{prelude::*, schedule::ScheduleLabel};
use prelude::SemanticTokensDict;
use systems::{init_onology_extractor, populate_known_ontologies, OntologyExtractor};
pub use tower_lsp::lsp_types;

use crate::prelude::*;

/// Main language tower_lsp server implementation.
///
/// [`Backend`](struct@backend::Backend) implements [`LanguageServer`](tower_lsp::LanguageServer).
/// Each incoming request a schedule is ran on the main [`World`].
pub mod backend;

/// Handle platform specific implementations for fetching and spawning tasks.
pub mod client;
/// Common utils
///
/// Includes range transformations between [`std::ops::Range`] and [`lsp_core::lsp_types::Range`].
/// And commonly used [`Spanned`].
pub mod util;

/// Defines all common [`Component`]s and [`Resource`]s
///
/// In this [`World`], [Entity]s are documents and [`Components`](`Component`) are derived from these documents.
/// Different [`System`]s derive new [`Components`](`Component`) from existing [`Components`](`Component`), that are added to
/// the [`Entity`].
/// For example, if [`Triples`] are defined, [systems::derive_classes] will
/// derive [`DefinedClass`](struct@systems::DefinedClass) from them and add them to the [`Entity`].
pub mod components;
/// Hosts all common features of the semantic language server.
pub mod feature;
/// Defines common language traits
pub mod lang;
pub mod prelude;
pub mod systems;

/// Initializes a [`World`], including [`Resources`](`Resource`) and [`Schedules`].
/// All systems defined in [`crate`] are added to the [`World`].
pub fn setup_schedule_labels<C: Client + Resource>(world: &mut World) {
    world.init_resource::<SemanticTokensDict>();
    world.init_resource::<TypeHierarchy<'static>>();
    world.insert_resource(OntologyExtractor::new());

    parse::setup_schedule::<C>(world);
    hover::setup_schedule(world);
    completion::setup_schedule(world);
    rename::setup_schedules(world);
    diagnostics::setup_schedule(world);
    save::setup_schedule(world);
    format::setup_schedule(world);
    references::setup_schedule(world);
    inlay::setup_schedule(world);
    goto_definition::setup_schedule(world);
    goto_type::setup_schedule(world);

    semantic::setup_world(world);

    world.add_schedule(Schedule::new(Tasks));

    let mut schedule = Schedule::new(Startup);
    schedule.add_systems((
        init_onology_extractor,
        populate_known_ontologies,
        // extract_known_prefixes_from_config::<C>,
    ));
    world.add_schedule(schedule);
}

/// Event triggers when a document is opened
///
/// Example
/// ```rust
/// # use lsp_core::components::DynLang;
/// # use lsp_core::CreateEvent;
/// # use lsp_core::lang::LangHelper;
/// # use bevy_ecs::event::EntityEvent;
/// # use bevy_ecs::prelude::{Commands, On, World, Component};
///
/// #[derive(Component)]
/// pub struct TurtleLang;
///
/// #[derive(Debug)]
/// pub struct TurtleHelper;
/// impl LangHelper for TurtleHelper {
///     fn keyword(&self) -> &[&'static str] {
///         &[
///             "@prefix",
///             "@base",
///             "a",
///         ]
///     }
/// }
///
/// let mut world = World::new();
/// // This example tells the ECS system that the document is Turtle,
/// // adding Turtle specific components
/// world.add_observer(|trigger: On<CreateEvent>, mut commands: Commands| {
///     match &trigger.event().language_id {
///         Some(x) if x == "turtle" => {
///             commands
///                 .entity(trigger.event_target())
///                 .insert((TurtleLang, DynLang(Box::new(TurtleHelper))));
///             return;
///         }
///         _ => {}
///     }
///     if trigger.event().url.as_str().ends_with(".ttl") {
///         commands
///             .entity(trigger.event_target())
///             .insert((TurtleLang, DynLang(Box::new(TurtleHelper))));
///         return;
///     }
/// });
/// ```
///
#[derive(EntityEvent)]
pub struct CreateEvent {
    pub url: crate::lsp_types::Url,
    pub language_id: Option<String>,
    #[event_target]
    entity: Entity,
}

/// [`ScheduleLabel`] related to the Tasks schedule
/// This schedule is used for async tasks, things that should be done at some point.
///
/// For example [`systems::handle_tasks`] spawns command queues sent with
/// [`CommandSender`]
#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Tasks;

#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Startup;
