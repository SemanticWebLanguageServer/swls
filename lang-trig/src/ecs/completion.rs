use bevy_ecs::prelude::*;
use completion::CompletionRequest;
use swls_core::{
    components::*,
    prelude::*,
    systems::{prefix::prefix_completion_helper, PrefixEntry},
};
use swls_lov::LocalPrefix;

pub fn trig_lov_undefined_prefix_completion(
    mut query: Query<(
        &TokenComponent,
        &Source,
        &RopeC,
        &Prefixes,
        &mut CompletionRequest,
        &DynLang,
    )>,
    lovs: Query<&LocalPrefix>,
    prefix_cc: Query<&PrefixEntry>,
) {
    for (word, source, rope, prefixes, mut req, lang) in &mut query {
        prefix_completion_helper(
            word,
            prefixes,
            &mut req.0,
            |name, location| {
                if prefixes.iter().any(|p| p.prefix == name) {
                    None
                } else {
                    lang.prefix_edits(&source.0, &rope.0, name, location)
                }
            },
            lovs.iter(),
            prefix_cc.iter(),
            lang,
        );
    }
}
