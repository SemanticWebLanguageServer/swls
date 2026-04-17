//! JSON-LD term expansion, context resolution, and IRI compaction.
//!
//! # [`ContextResolver`]
//!
//! A per-file resolver built from a single document's `@context` value (string URL, inline
//! object, or array of both).  Call [`ContextResolver::from_context_value`] with the parsed
//! `@context` and the project's preloaded context map, then use [`ContextResolver::expand_term`]
//! to turn short terms into full IRIs during component or config extraction.
//!
//! # [`IriCompactor`]
//!
//! A project-wide compactor built by merging *all* context documents from [`ModuleState`].
//! Used by the LSP at display time — e.g., to show `oo:Class` in a hover card instead of the
//! full `https://linkedsoftwaredependencies.org/…#Class` IRI.
//!
//! # [`extract_graph_nodes`]
//!
//! Extracts the `@graph` entries (or the document root if there is no `@graph`) from a parsed
//! `JsonLdVal` document, expanding all term keys to full IRIs.  The returned [`ExpandedNode`]
//! values form the input to the two-phase component collection in
//! [`crate::components::registry`].
//!
//! [`ModuleState`]: crate::module_state::ModuleState

use std::collections::HashMap;

use rdf_parsers::jsonld::convert::JsonLdVal;

use crate::error::{ComponentsJsError, Result};

/// A resolved JSON-LD context that maps short terms to full IRIs.
#[derive(Debug, Clone, Default)]
pub struct ContextResolver {
    /// @vocab — default IRI prefix for unmapped terms
    pub vocab: Option<String>,
    /// prefix:suffix mappings (e.g., "oo" -> "https://...#")
    pub prefixes: HashMap<String, String>,
    /// Direct term mappings (e.g., "Class" -> TermDef { iri, type_coercion })
    pub terms: HashMap<String, TermDef>,
}

#[derive(Debug, Clone)]
pub struct TermDef {
    pub iri: String,
    pub type_coercion: Option<String>,
    pub container: Option<String>,
}

impl ContextResolver {
    pub fn new() -> Self {
        Self::default()
    }

    /// Parse a JSON-LD `@context` value and build the resolver.
    /// `known_contexts` maps context IRIs to their parsed `JsonLdVal` content.
    pub fn from_context_value(
        context_value: &JsonLdVal,
        known_contexts: &HashMap<String, JsonLdVal>,
    ) -> Result<Self> {
        let mut resolver = Self::new();
        resolver.load_context_value(context_value, known_contexts)?;
        Ok(resolver)
    }

    fn load_context_value(
        &mut self,
        value: &JsonLdVal,
        known_contexts: &HashMap<String, JsonLdVal>,
    ) -> Result<()> {
        match value {
            JsonLdVal::Array(arr) => {
                for (item, _) in arr {
                    self.load_context_value(item, known_contexts)?;
                }
            }
            JsonLdVal::Str(url) => {
                if let Some(ctx_doc) = known_contexts.get(url.as_str()) {
                    if let Some(inner) = ctx_doc.get("@context") {
                        self.load_context_value(inner, known_contexts)?;
                    } else {
                        self.load_context_object(ctx_doc)?;
                    }
                } else {
                    tracing::warn!("Unknown context URL: {url} — skipping");
                }
            }
            JsonLdVal::Object(_, _) => {
                self.load_context_object(value)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn load_context_object(&mut self, obj: &JsonLdVal) -> Result<()> {
        let members = obj
            .as_object()
            .ok_or_else(|| ComponentsJsError::ContextResolution("Expected object".into()))?;

        for (key, _, _, val) in members {
            match key.as_str() {
                "@vocab" => {
                    if let Some(s) = val.as_str() {
                        self.vocab = Some(s.to_string());
                    }
                }
                k if k.starts_with('@') => {}
                _ => match val {
                    JsonLdVal::Str(iri) => {
                        if iri.ends_with('/') || iri.ends_with('#') || iri.ends_with(':') {
                            self.prefixes.insert(key.clone(), iri.clone());
                        } else {
                            self.terms.insert(
                                key.clone(),
                                TermDef {
                                    iri: iri.clone(),
                                    type_coercion: None,
                                    container: None,
                                },
                            );
                        }
                    }
                    JsonLdVal::Object(_, _) => {
                        if let Some(id) = val.get("@id").and_then(|v| v.as_str()) {
                            let type_coercion =
                                val.get("@type").and_then(|v| v.as_str()).map(String::from);
                            let container =
                                val.get("@container").and_then(|v| v.as_str()).map(String::from);
                            self.terms.insert(
                                key.clone(),
                                TermDef {
                                    iri: id.to_string(),
                                    type_coercion,
                                    container,
                                },
                            );
                        }
                    }
                    _ => {}
                },
            }
        }
        Ok(())
    }

    /// Expand a compacted term to a full IRI.
    pub fn expand_term(&self, term: &str) -> String {
        self.expand_term_depth(term, 0)
    }

    fn expand_term_depth(&self, term: &str, depth: usize) -> String {
        if depth > 10 {
            return term.to_string();
        }

        if let Some(def) = self.terms.get(term) {
            return self.expand_term_depth(&def.iri, depth + 1);
        }

        if let Some((prefix, suffix)) = term.split_once(':') {
            if !suffix.starts_with("//") {
                if let Some(base) = self.prefixes.get(prefix) {
                    let expanded_base = self.expand_term_depth(base, depth + 1);
                    return format!("{expanded_base}{suffix}");
                }
            }
        }

        if term.contains("://") {
            return term.to_string();
        }

        if let Some(vocab) = &self.vocab {
            return format!("{vocab}{term}");
        }

        term.to_string()
    }

    /// Compact a full IRI back to a prefixed form.
    pub fn compact_iri(&self, iri: &str) -> String {
        for (term, def) in &self.terms {
            let expanded = self.expand_term(&def.iri);
            if expanded == iri {
                return term.clone();
            }
        }

        let mut best: Option<(String, usize)> = None;
        for (prefix, base_iri) in &self.prefixes {
            let expanded_base = self.expand_term(base_iri);
            if let Some(suffix) = iri.strip_prefix(expanded_base.as_str()) {
                let base_len = expanded_base.len();
                if best.as_ref().is_none_or(|(_, bl)| base_len > *bl) {
                    best = Some((format!("{prefix}:{suffix}"), base_len));
                }
            }
        }
        if let Some((compact, _)) = best {
            return compact;
        }

        if let Some(vocab) = &self.vocab {
            if let Some(suffix) = iri.strip_prefix(vocab.as_str()) {
                if !suffix.contains('/') && !suffix.contains('#') {
                    return suffix.to_string();
                }
            }
        }

        iri.to_string()
    }
}

/// Project-wide bidirectional IRI translator.
///
/// Built by merging all known contexts across the project. Used by the LSP to
/// convert full IRIs shown in hover cards to compact forms for display, and to
/// expand compact IRIs typed by the user during completion.
#[derive(Debug, Clone, Default)]
pub struct IriCompactor {
    prefixes: Vec<(String, String)>,
    terms: Vec<(String, String)>,
    vocab: Option<String>,
}

impl IriCompactor {
    /// Build a project-wide compactor from all known context documents.
    pub fn from_contexts(known_contexts: &HashMap<String, JsonLdVal>) -> Result<Self> {
        let mut resolver = ContextResolver::new();
        for ctx_doc in known_contexts.values() {
            if let Some(inner) = ctx_doc.get("@context") {
                resolver.load_context_value(inner, known_contexts)?;
            } else {
                resolver.load_context_object(ctx_doc)?;
            }
        }

        let mut prefixes: Vec<(String, String)> = resolver
            .prefixes
            .iter()
            .map(|(name, base)| {
                let expanded = resolver.expand_term(base);
                (name.clone(), expanded)
            })
            .collect();
        prefixes.sort_by(|a, b| b.1.len().cmp(&a.1.len()));

        let terms: Vec<(String, String)> = resolver
            .terms
            .iter()
            .map(|(name, def)| {
                let expanded = resolver.expand_term(&def.iri);
                (name.clone(), expanded)
            })
            .collect();

        Ok(Self {
            prefixes,
            terms,
            vocab: resolver.vocab,
        })
    }

    /// Compact a full IRI to its shortest prefixed form.
    pub fn compact(&self, iri: &str) -> String {
        for (term, expanded) in &self.terms {
            if expanded == iri {
                return term.clone();
            }
        }
        for (prefix, base) in &self.prefixes {
            if let Some(suffix) = iri.strip_prefix(base.as_str()) {
                return format!("{prefix}:{suffix}");
            }
        }
        if let Some(vocab) = &self.vocab {
            if let Some(suffix) = iri.strip_prefix(vocab.as_str()) {
                if !suffix.contains('/') && !suffix.contains('#') {
                    return suffix.to_string();
                }
            }
        }
        iri.to_string()
    }

    /// Expand a compact term to a full IRI.
    pub fn expand(&self, term: &str) -> String {
        for (name, expanded) in &self.terms {
            if name == term {
                return expanded.clone();
            }
        }
        if let Some((prefix, suffix)) = term.split_once(':') {
            if !suffix.starts_with("//") {
                for (name, base) in &self.prefixes {
                    if name == prefix {
                        return format!("{base}{suffix}");
                    }
                }
            }
        }
        if term.contains("://") {
            return term.to_string();
        }
        if let Some(vocab) = &self.vocab {
            return format!("{vocab}{term}");
        }
        term.to_string()
    }
}

/// An expanded JSON-LD node with full IRIs as keys.
///
/// Produced by [`extract_graph_nodes`] during the collection phase. Both `id`
/// and all keys in `properties` are fully expanded IRIs; values in `properties`
/// are the raw `JsonLdVal`s as they appear in the source (terms within values
/// are NOT expanded, because components may define their own inline nodes).
#[derive(Debug, Clone)]
pub struct ExpandedNode {
    /// Fully expanded `@id` of this node, or `None` if absent.
    pub id: Option<String>,
    /// Fully expanded `@type` IRIs.
    pub types: Vec<String>,
    /// Property values keyed by fully expanded predicate IRI.
    pub properties: HashMap<String, Vec<JsonLdVal>>,
}

/// Extract the `@graph` entries from a JSON-LD document, expanding all terms.
pub fn extract_graph_nodes(
    doc: &JsonLdVal,
    known_contexts: &HashMap<String, JsonLdVal>,
) -> Result<Vec<ExpandedNode>> {
    let resolver = if let Some(ctx) = doc.get("@context") {
        ContextResolver::from_context_value(ctx, known_contexts)?
    } else {
        ContextResolver::new()
    };

    let entries: Vec<&JsonLdVal> = if let Some(graph) = doc.get("@graph") {
        match graph.as_array() {
            Some(arr) => arr.iter().map(|(v, _)| v).collect(),
            None => vec![graph],
        }
    } else if doc.get("@id").is_some() || doc.get("@type").is_some() {
        vec![doc]
    } else {
        vec![]
    };

    let mut nodes = Vec::new();
    for entry in entries {
        if let Some(node) = expand_node(entry, &resolver) {
            nodes.push(node);
        }
    }
    Ok(nodes)
}

fn expand_node(value: &JsonLdVal, resolver: &ContextResolver) -> Option<ExpandedNode> {
    let members = value.as_object()?;

    let id = members
        .iter()
        .find(|(k, _, _, _)| k == "@id")
        .and_then(|(_, _, _, v)| v.as_str())
        .map(|s| resolver.expand_term(s));

    let types: Vec<String> = match value.get("@type") {
        Some(JsonLdVal::Str(t)) => vec![resolver.expand_term(t)],
        Some(v) => v
            .as_array()
            .map(|arr| {
                arr.iter()
                    .filter_map(|(item, _)| item.as_str())
                    .map(|s| resolver.expand_term(s))
                    .collect()
            })
            .unwrap_or_default(),
        None => vec![],
    };

    let mut properties = HashMap::new();
    for (key, _, _, val) in members {
        if key.starts_with('@') {
            continue;
        }
        let expanded_key = resolver.expand_term(key);
        let values = match val {
            JsonLdVal::Array(arr) => arr.iter().map(|(v, _)| v.clone()).collect(),
            other => vec![other.clone()],
        };
        properties.insert(expanded_key, values);
    }

    Some(ExpandedNode {
        id,
        types,
        properties,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rdf_parsers::jsonld::convert::parse_json;

    fn make_cjs_context() -> HashMap<String, JsonLdVal> {
        let ctx_json = parse_json(r#"{
            "@context": {
                "oo": "https://linkedsoftwaredependencies.org/vocabularies/object-oriented#",
                "Module": { "@id": "oo:Module" },
                "Class": { "@id": "oo:Class" },
                "AbstractClass": { "@id": "oo:AbstractClass" },
                "components": { "@id": "oo:component" },
                "parameters": { "@id": "oo:parameter" },
                "extends": { "@id": "rdfs:subClassOf", "@type": "@id" },
                "rdfs": "http://www.w3.org/2000/01/rdf-schema#",
                "doap": "http://usefulinc.com/ns/doap#",
                "requireName": { "@id": "doap:name" },
                "requireElement": { "@id": "oo:componentPath" },
                "import": { "@id": "rdfs:seeAlso", "@type": "@id" }
            }
        }"#)
        .unwrap();
        let mut known = HashMap::new();
        known.insert(
            "https://linkedsoftwaredependencies.org/bundles/npm/componentsjs/^4.0.0/components/context.jsonld".to_string(),
            ctx_json,
        );
        known
    }

    #[test]
    fn test_expand_term_direct_mapping() {
        let known = make_cjs_context();
        let ctx_ref = parse_json(r#"["https://linkedsoftwaredependencies.org/bundles/npm/componentsjs/^4.0.0/components/context.jsonld"]"#).unwrap();
        let resolver = ContextResolver::from_context_value(&ctx_ref, &known).unwrap();

        assert_eq!(
            resolver.expand_term("Class"),
            "https://linkedsoftwaredependencies.org/vocabularies/object-oriented#Class"
        );
        assert_eq!(
            resolver.expand_term("Module"),
            "https://linkedsoftwaredependencies.org/vocabularies/object-oriented#Module"
        );
    }

    #[test]
    fn test_expand_term_prefix() {
        let known = make_cjs_context();
        let ctx_ref = parse_json(r#"["https://linkedsoftwaredependencies.org/bundles/npm/componentsjs/^4.0.0/components/context.jsonld"]"#).unwrap();
        let resolver = ContextResolver::from_context_value(&ctx_ref, &known).unwrap();

        assert_eq!(
            resolver.expand_term("oo:Class"),
            "https://linkedsoftwaredependencies.org/vocabularies/object-oriented#Class"
        );
    }

    #[test]
    fn test_expand_term_with_local_context() {
        let known = make_cjs_context();
        let ctx_ref = parse_json(r#"[
            "https://linkedsoftwaredependencies.org/bundles/npm/componentsjs/^4.0.0/components/context.jsonld",
            { "ex": "http://example.org/", "hello": "http://example.org/hello/" }
        ]"#).unwrap();
        let resolver = ContextResolver::from_context_value(&ctx_ref, &known).unwrap();

        assert_eq!(resolver.expand_term("ex:MyModule"), "http://example.org/MyModule");
        assert_eq!(resolver.expand_term("hello:say"), "http://example.org/hello/say");
    }

    #[test]
    fn test_extract_graph_nodes() {
        let known = make_cjs_context();
        let doc = parse_json(r#"{
            "@context": [
                "https://linkedsoftwaredependencies.org/bundles/npm/componentsjs/^4.0.0/components/context.jsonld",
                { "ex": "http://example.org/", "hello": "http://example.org/hello/" }
            ],
            "@graph": [
                {
                    "@id": "ex:HelloWorldModule",
                    "@type": "Module",
                    "requireName": "helloworld",
                    "components": [
                        {
                            "@id": "ex:HelloWorldModule#SayHelloComponent",
                            "@type": "Class",
                            "requireElement": "Hello",
                            "parameters": [
                                { "@id": "hello:say" },
                                { "@id": "hello:hello" }
                            ]
                        }
                    ]
                }
            ]
        }"#).unwrap();

        let nodes = extract_graph_nodes(&doc, &known).unwrap();
        assert_eq!(nodes.len(), 1);
        let module = &nodes[0];
        assert_eq!(module.id.as_deref(), Some("http://example.org/HelloWorldModule"));
        assert_eq!(
            module.types,
            vec!["https://linkedsoftwaredependencies.org/vocabularies/object-oriented#Module"]
        );
    }

    #[test]
    fn test_vocab_expansion() {
        let known = HashMap::new();
        let ctx = parse_json(r#"{
            "@vocab": "https://linkedsoftwaredependencies.org/vocabularies/object-oriented#",
            "ex": "http://example.org/"
        }"#).unwrap();
        let resolver = ContextResolver::from_context_value(&ctx, &known).unwrap();

        assert_eq!(
            resolver.expand_term("SomeUnknownTerm"),
            "https://linkedsoftwaredependencies.org/vocabularies/object-oriented#SomeUnknownTerm"
        );
    }

    #[test]
    fn test_compact_iri_term() {
        let known = make_cjs_context();
        let ctx_ref = parse_json(r#"["https://linkedsoftwaredependencies.org/bundles/npm/componentsjs/^4.0.0/components/context.jsonld"]"#).unwrap();
        let resolver = ContextResolver::from_context_value(&ctx_ref, &known).unwrap();

        assert_eq!(
            resolver.compact_iri(
                "https://linkedsoftwaredependencies.org/vocabularies/object-oriented#Class"
            ),
            "Class"
        );
        assert_eq!(
            resolver.compact_iri("http://www.w3.org/2000/01/rdf-schema#label"),
            "rdfs:label"
        );
    }

    #[test]
    fn test_compact_iri_prefix() {
        let known = make_cjs_context();
        let ctx_ref = parse_json(r#"[
            "https://linkedsoftwaredependencies.org/bundles/npm/componentsjs/^4.0.0/components/context.jsonld",
            { "ex": "http://example.org/" }
        ]"#).unwrap();
        let resolver = ContextResolver::from_context_value(&ctx_ref, &known).unwrap();

        assert_eq!(resolver.compact_iri("http://example.org/Foo"), "ex:Foo");
    }

    #[test]
    fn test_compact_iri_unknown() {
        let known = make_cjs_context();
        let ctx_ref = parse_json(r#"["https://linkedsoftwaredependencies.org/bundles/npm/componentsjs/^4.0.0/components/context.jsonld"]"#).unwrap();
        let resolver = ContextResolver::from_context_value(&ctx_ref, &known).unwrap();

        assert_eq!(
            resolver.compact_iri("https://unknown.example.org/Something"),
            "https://unknown.example.org/Something"
        );
    }

    #[test]
    fn test_iri_compactor_roundtrip() {
        let known = make_cjs_context();
        let compactor = IriCompactor::from_contexts(&known).unwrap();

        let full = "https://linkedsoftwaredependencies.org/vocabularies/object-oriented#Class";
        let compact = compactor.compact(full);
        assert_eq!(compact, "Class");
        assert_eq!(compactor.expand(&compact), full);

        let full2 = "http://www.w3.org/2000/01/rdf-schema#subClassOf";
        let compact2 = compactor.compact(full2);
        assert_eq!(compact2, "extends");
        assert_eq!(compactor.expand(&compact2), full2);
    }

    #[test]
    fn test_iri_compactor_expand() {
        let known = make_cjs_context();
        let compactor = IriCompactor::from_contexts(&known).unwrap();

        assert_eq!(
            compactor.expand("oo:Module"),
            "https://linkedsoftwaredependencies.org/vocabularies/object-oriented#Module"
        );
    }
}
