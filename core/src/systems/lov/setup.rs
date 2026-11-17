use std::{borrow::Cow, collections::HashSet};

use bevy_ecs::prelude::*;

use swls_lov::LocalPrefix;

use crate::{lsp_types::Url, prelude::*, systems::prefix::PREFIX_CC};

#[derive(Component, Debug)]
pub struct PrefixEntry {
    pub namespace: Cow<'static, str>,
    pub name: Cow<'static, str>,
    pub rank: usize,
}

pub fn populate_known_ontologies(mut commands: Commands) {
    let mut actual_local = HashSet::new();
    for lov in swls_lov::LOCAL_PREFIXES.iter() {
        actual_local.insert(lov.name.clone());
        // TODO:  I actually want to spawn the ontology fully
        commands.spawn(lov.clone());
    }

    for (i, (name, url)) in PREFIX_CC
        .split('\n')
        .flat_map(|x| {
            let mut s = x.split(' ');
            let first = s.next()?;
            let second = s.next()?;
            Some((first.to_string(), second.to_string()))
        })
        .enumerate()
    {
        let name: Cow<'static, str> = name.into();
        if actual_local.contains(&name) {
            continue;
        }
        let namespace: Cow<'static, str> = url.into();
        let lov = PrefixEntry {
            namespace,
            name: name.clone(),
            rank: i,
        };

        commands.spawn(lov);
    }
}

#[derive(Debug, Clone, Component)]
pub struct FromPrefix(pub Prefix);

/// Marks a [`LocalPrefix`] that was loaded from the user's `ontologies` config.
///
/// Configured ontologies are identified by their own location rather than by
/// prefix name, so several of them can describe the same namespace (issue #30),
/// and they take precedence over the built-in / LOV ontology for that namespace.
#[derive(Debug, Clone, Copy, Component)]
pub struct ConfiguredOntology;

/// One document the server loads to describe a namespace.
#[derive(Debug, Clone)]
pub struct NamespaceSource<'a> {
    /// Label (and editor-visible location) of the document.
    pub url: Url,
    /// The configured or built-in entry it comes from; `None` for a remote LOV lookup.
    pub local: Option<&'a LocalPrefix>,
    /// Whether `url` is a cache location the server owns and may (over)write.
    /// `false` when `url` is the user's own file.
    pub is_cache: bool,
}

/// Label of the document loaded for a configured ontology at `location`.
///
/// Local files keep their own URL, so goto-definition opens the real file and
/// nothing is copied. Remote files are cached at a location derived from their
/// URL (see [`FsTrait::cache_url`]).
pub fn configured_url(fs: &Fs, location: &str) -> Option<(Url, bool)> {
    let url = Url::parse(location).ok()?;
    match url.scheme() {
        "file" => Some((url, false)),
        _ => fs.0.cache_url(&url).map(|u| (u, true)),
    }
}

/// Every document the server loads for `namespace`, written as `prefix:` in the
/// user's document.
///
/// This is the single source of truth for document identity: the fetch system
/// spawns exactly these documents, and links / goto-definition point at them.
///
/// - Configured ontologies for the namespace, one document per location.
/// - Otherwise built-in LOV entries for the namespace, at `<cache>/<prefix>.ttl`.
/// - Otherwise a remote LOV lookup, also at `<cache>/<prefix>.ttl`.
pub fn namespace_sources<'a>(
    fs: &Fs,
    namespace: &str,
    prefix: &str,
    locals: impl IntoIterator<Item = (&'a LocalPrefix, Option<&'a ConfiguredOntology>)>,
) -> Vec<NamespaceSource<'a>> {
    let matching: Vec<_> = locals
        .into_iter()
        .filter(|(l, _)| l.namespace == namespace)
        .collect();

    let configured: Vec<_> = matching
        .iter()
        .filter(|(_, c)| c.is_some())
        .flat_map(|(local, _)| {
            let (url, is_cache) = configured_url(fs, &local.location)?;
            Some(NamespaceSource {
                url,
                local: Some(*local),
                is_cache,
            })
        })
        .collect();
    if !configured.is_empty() {
        return configured;
    }

    let Some(url) = fs.0.lov_url(namespace, prefix) else {
        return Vec::new();
    };
    // Several built-in entries for one namespace all resolve to the same
    // prefix-named file, keep only one of them.
    let local = matching.first().map(|(l, _)| *l);
    vec![NamespaceSource {
        url,
        local,
        is_cache: true,
    }]
}
