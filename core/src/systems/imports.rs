//! `owl:imports`: load every imported document in the background.
//!
//! - `file:` imports are read from disk and keep their own URL, so
//!   goto-definition opens the real file.
//! - `http(s):` imports are fetched with content negotiation and cached like
//!   other remote ontologies, at `<cache>/sources/<host>/<path>`
//!   (see [`FsTrait::cache_url`](crate::util::fs::FsTrait::cache_url)).
//!
//! [`import_url`] is the single mapping from an imported IRI to the document
//! that holds it. Loading, document links and goto-definition all use it.
//!
//! Imports are followed transitively: an imported document's own imports are
//! loaded too, each IRI at most once.

use std::collections::{HashMap, HashSet};

use bevy_ecs::prelude::*;
use sophia_api::{
    prelude::{Any, Dataset},
    quad::Quad as _,
    term::Term as _,
};

use crate::{
    lsp_types::Url,
    prelude::*,
    systems::spawn_document,
    util::{cache, ns::owl},
};

/// Label of the document loaded for `owl:imports <iri>`.
pub fn import_url(fs: &Fs, iri: &Url) -> Option<Url> {
    match iri.scheme() {
        "http" | "https" => fs.0.cache_url(iri),
        _ => Some(iri.clone()),
    }
}

/// File extensions a language registers itself for. A document with one of
/// these gets its language from the extension.
const KNOWN_EXTENSIONS: &[&str] = &[".ttl", ".nt", ".owl", ".trig", ".n3", ".jsonld"];

/// Language id for an imported document at `url`: from its extension when
/// known, Turtle otherwise.
fn import_language(url: &Url) -> Option<String> {
    let path = url.path();
    if KNOWN_EXTENSIONS.iter().any(|ext| path.ends_with(ext)) {
        None
    } else {
        Some(String::from("turtle"))
    }
}

/// Content types accepted for remote imports, best first. These are the
/// syntaxes the Turtle parser reads.
const ACCEPT: &str = "text/turtle, application/n-triples;q=0.9, text/plain;q=0.1";

/// Whether `content` looks like a syntax swls cannot parse: RDF/XML (common
/// for `.owl` files) or an HTML page served instead of RDF.
fn unsupported_syntax(content: &str) -> Option<&'static str> {
    let start = content.trim_start();
    if start.starts_with("<?xml") || start.starts_with("<rdf:RDF") {
        Some("RDF/XML")
    } else if start.len() >= 5 && start[..5].eq_ignore_ascii_case("<html")
        || start.len() >= 9 && start[..9].eq_ignore_ascii_case("<!doctype")
    {
        Some("HTML")
    } else {
        None
    }
}

/// Fetch the remote import `iri`; `None` when it fails or is not RDF swls reads.
async fn fetch_remote(iri: &Url, client: &impl Client) -> Option<String> {
    let headers = HashMap::from([(String::from("Accept"), String::from(ACCEPT))]);
    let resp = match client.fetch(iri.as_str(), &headers).await {
        Ok(resp) => resp,
        Err(e) => {
            tracing::warn!("Fetching import {} failed: {}", iri, e);
            return None;
        }
    };
    if resp.status != 200 {
        tracing::warn!("Fetching import {} failed with status {}", iri, resp.status);
        return None;
    }
    let content_type = resp
        .headers
        .iter()
        .find(|(k, _)| k.eq_ignore_ascii_case("content-type"))
        .map(|(_, v)| v.to_ascii_lowercase());
    if let Some(ct) = content_type.as_deref() {
        if ct.contains("rdf+xml") || ct.contains("html") || ct.contains("json") {
            tracing::warn!("Import {} is served as {}, which swls cannot read", iri, ct);
            return None;
        }
    }
    if let Some(syntax) = unsupported_syntax(&resp.body) {
        tracing::warn!("Import {} is {}, which swls cannot read", iri, syntax);
        return None;
    }
    Some(resp.body)
}

/// Content of the import `iri` stored at `label`.
async fn load_import(iri: Url, label: &Url, fs: &Fs, client: &impl Client) -> Option<String> {
    match iri.scheme() {
        "http" | "https" => cache::cached(fs, label, || fetch_remote(&iri, client)).await,
        _ => {
            let content = fs.0.read_file(&iri).await?;
            if let Some(syntax) = unsupported_syntax(&content) {
                tracing::warn!("Import {} is {}, which swls cannot read", iri, syntax);
                return None;
            }
            Some(content)
        }
    }
}

/// Load the targets of `owl:imports` of every document whose triples changed.
pub fn open_imports<C: Client + Resource>(
    query: Query<&Triples, Changed<Triples>>,
    mut opened: Local<HashSet<String>>,
    sender: Res<CommandSender>,
    fs: Res<Fs>,
    client: Res<C>,
) {
    for triples in &query {
        for iri in triples
            .quads_matching(Any, [owl::imports], Any, Any)
            .flatten()
            .flat_map(|s| s.o().iri())
            .flat_map(|s| Url::parse(s.as_str()))
        {
            if !opened.insert(iri.as_str().to_string()) {
                continue;
            }
            let Some(label) = import_url(&fs, &iri) else {
                continue;
            };

            let fs = fs.clone();
            let sender = sender.clone();
            let c = client.as_ref().clone();
            client.spawn(async move {
                let Some(content) = load_import(iri.clone(), &label, &fs, &c).await else {
                    tracing::warn!("No content found for import {}", iri);
                    return;
                };
                tracing::debug!("Loaded import {} as {}", iri, label);
                let language = import_language(&label);
                // Spawning parses the document; shapes and ontologies it
                // brings in reach open documents through `Revisions`.
                spawn_document(label, content, language, &sender.0, |_, _| {});
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detects_unreadable_syntaxes() {
        assert_eq!(
            unsupported_syntax("<?xml version=\"1.0\"?>"),
            Some("RDF/XML")
        );
        assert_eq!(unsupported_syntax("  <rdf:RDF>"), Some("RDF/XML"));
        assert_eq!(unsupported_syntax("<!DOCTYPE html><html>"), Some("HTML"));
        assert_eq!(unsupported_syntax("<HTML>"), Some("HTML"));
        assert_eq!(unsupported_syntax("@prefix ex: <http://e/> ."), None);
        assert_eq!(
            unsupported_syntax("<http://a> <http://b> <http://c> ."),
            None
        );
    }

    #[test]
    fn language_follows_known_extensions() {
        let lang = |u: &str| import_language(&Url::parse(u).unwrap());
        assert_eq!(lang("file:///a/onto.jsonld"), None);
        assert_eq!(lang("file:///a/onto.ttl"), None);
        assert_eq!(lang("file:///a/onto"), Some(String::from("turtle")));
    }
}
