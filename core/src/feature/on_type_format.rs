//! On-type formatting: react to a typed character and return text edits.
//!
//! Currently this powers a single behaviour: when the user types the `:` of a
//! prefixed name (e.g. `foaf:`) whose prefix is **known** (from the bundled LOV
//! data or prefix.cc) but **not yet declared** in the document, the missing
//! `@prefix foaf: <…> .` declaration is inserted automatically — no completion
//! popup or extra keystroke required.
//!
//! The declaration text and insertion point are produced by
//! [`LangHelper::prefix_edits`](crate::lang::LangHelper::prefix_edits), the same
//! single source of truth used by prefix completion and the "add missing prefix"
//! quick-fix, so all three stay consistent (including the user's
//! `PrefixFormat` preference).  Languages whose prefix model is incompatible
//! (JSON-LD `@context`, where `:` is also the JSON key/value separator) opt out
//! via [`handles_prefix_completion`](crate::lang::LangHelper::handles_prefix_completion).

use bevy_ecs::{prelude::*, schedule::ScheduleLabel};
use swls_lov::LocalPrefix;
use tracing::instrument;

use crate::{
    lsp_types::TextEdit,
    prelude::*,
    systems::PrefixEntry,
    util::position_to_offset,
};

/// [`Component`] collecting the on-type-formatting [`TextEdit`]s for a request.
#[derive(Component, Debug)]
pub struct OnTypeFormatRequest(pub Option<Vec<TextEdit>>);

/// [`ScheduleLabel`] for the on-type-formatting schedule.
#[derive(ScheduleLabel, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Label;

pub fn setup_schedule(world: &mut World) {
    let mut schedule = bevy_ecs::schedule::Schedule::new(Label);
    schedule.add_systems(prefix_on_type_format);
    world.add_schedule(schedule);
}

/// Resolve a prefix `name` to a namespace from the bundled LOV data or prefix.cc,
/// keeping only hash/slash namespaces (the ones that form valid prefixes).
fn lookup_namespace<'a>(
    name: &str,
    lovs: impl Iterator<Item = &'a LocalPrefix>,
    prefix_cc: impl Iterator<Item = &'a PrefixEntry>,
) -> Option<String> {
    let ns = lovs
        .filter(|l| l.name.as_ref() == name)
        .map(|l| l.namespace.to_string())
        .next()
        .or_else(|| {
            prefix_cc
                .filter(|p| p.name.as_ref() == name)
                .map(|p| p.namespace.to_string())
                .next()
        })?;
    (ns.ends_with('#') || ns.ends_with('/')).then_some(ns)
}

/// ECS system: when `:` is typed after a known-but-undeclared prefix, fill the
/// [`OnTypeFormatRequest`] with the edit that declares it.
#[instrument(skip(query, lovs, prefix_cc, config))]
pub fn prefix_on_type_format(
    mut query: Query<(
        &Source,
        &RopeC,
        Option<&Prefixes>,
        &PositionComponent,
        &DynLang,
        &mut OnTypeFormatRequest,
    )>,
    lovs: Query<&LocalPrefix>,
    prefix_cc: Query<&PrefixEntry>,
    config: Res<ServerConfig>,
) {
    let fmt = config.config.local.prefix_format.unwrap_or_default();
    for (source, rope, prefixes, position, lang, mut req) in &mut query {
        let Some(offset) = position_to_offset(position.0, &rope.0) else {
            continue;
        };
        let Some(name) = lang.prefix_name_at(&source.0, offset) else {
            continue;
        };
        // Already declared → nothing to insert.  (Languages whose `prefix_edits`
        // splice into an existing structure, like JSON-LD's `@context`, also
        // re-check for duplicates themselves, so a missing `Prefixes` is safe.)
        if prefixes
            .map(|p| p.iter().any(|p| p.prefix.as_str() == name))
            .unwrap_or(false)
        {
            continue;
        }
        let Some(namespace) = lookup_namespace(name, lovs.iter(), prefix_cc.iter()) else {
            continue;
        };
        if let Some(edits) = lang.prefix_edits(&source.0, &rope.0, name, &namespace, fmt) {
            tracing::debug!("on-type: declaring prefix {name} → {namespace}");
            req.0 = Some(edits);
        }
    }
}
