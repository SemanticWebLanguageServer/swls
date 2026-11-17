use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

use bevy_ecs::prelude::*;

use swls_lov::LocalPrefix;

use crate::{lsp_types::Url, prelude::*, systems::prefix::PREFIX_CC};

#[derive(Component, Debug)]
pub struct PrefixEntry {
    pub namespace: Cow<'static, str>,
    pub name: Cow<'static, str>,
    pub rank: usize,
}

/// The preferred prefix name of every known namespace.
///
/// Ontologies are fetched and cached by prefix name (`lov/<prefix>.ttl`, the
/// LOV mirror is keyed by it). That must be the vocabulary's own name, not the
/// one a user happened to type: `@prefix f: <http://xmlns.com/foaf/0.1/>` should
/// still load (and share) `foaf.ttl`, never whatever vocabulary is called `f`.
///
/// Built from the built-in LOV vocabularies first, then prefix.cc, which lists
/// the most popular name of a namespace first.
#[derive(Resource, Debug, Default)]
pub struct PreferredPrefixes(HashMap<String, String>);

impl PreferredPrefixes {
    /// Index the built-in LOV vocabularies and the prefix.cc list.
    pub fn known() -> Self {
        let mut map = HashMap::new();
        for lov in swls_lov::LOCAL_PREFIXES.iter() {
            map.entry(lov.namespace.to_string())
                .or_insert_with(|| lov.name.to_string());
        }
        for (name, namespace) in prefix_cc_entries() {
            map.entry(namespace).or_insert(name);
        }
        Self(map)
    }

    /// Preferred prefix name for `namespace`, if it is known.
    pub fn get(&self, namespace: &str) -> Option<&str> {
        self.0.get(namespace).map(String::as_str)
    }

    /// Preferred prefix name for `namespace`, falling back to `typed`, the name
    /// used in the document.
    pub fn resolve<'a>(&'a self, namespace: &str, typed: &'a str) -> &'a str {
        self.get(namespace).unwrap_or(typed)
    }
}

/// `(name, namespace)` pairs of prefix.cc, most popular name first.
fn prefix_cc_entries() -> impl Iterator<Item = (String, String)> {
    PREFIX_CC.split('\n').flat_map(|x| {
        let mut s = x.split(' ');
        let first = s.next()?;
        let second = s.next()?;
        Some((first.to_string(), second.to_string()))
    })
}

pub fn populate_known_ontologies(mut commands: Commands) {
    commands.insert_resource(PreferredPrefixes::known());

    let mut actual_local = HashSet::new();
    for lov in swls_lov::LOCAL_PREFIXES.iter() {
        actual_local.insert(lov.name.clone());
        // TODO:  I actually want to spawn the ontology fully
        commands.spawn(lov.clone());
    }

    for (i, (name, url)) in prefix_cc_entries().enumerate() {
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
    /// Prefix name the document is loaded under: the vocabulary's preferred
    /// name (see [`PreferredPrefixes`]), not necessarily the one in the document.
    pub prefix: String,
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
/// - Otherwise built-in LOV entries for the namespace, at `<cache>/lov/<name>.ttl`.
/// - Otherwise a remote LOV lookup, also at `<cache>/lov/<name>.ttl`.
///
/// `<name>` is the vocabulary's preferred prefix name, so every document using
/// the namespace shares one file whatever prefix it declares. Only for an unknown
/// namespace does the name `typed` in the document get used.
pub fn namespace_sources<'a>(
    fs: &Fs,
    namespace: &str,
    typed: &str,
    preferred: &PreferredPrefixes,
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
                prefix: local.name.to_string(),
                local: Some(*local),
                is_cache,
            })
        })
        .collect();
    if !configured.is_empty() {
        return configured;
    }

    // Several built-in entries for one namespace all resolve to the same
    // prefix-named file, keep only one of them.
    let local = matching.first().map(|(l, _)| *l);
    let prefix = match local {
        Some(local) => local.name.to_string(),
        None => preferred.resolve(namespace, typed).to_string(),
    };
    let Some(url) = fs.0.lov_url(namespace, &prefix) else {
        return Vec::new();
    };
    vec![NamespaceSource {
        url,
        prefix,
        local,
        is_cache: true,
    }]
}
