use std::{collections::HashSet, path::PathBuf};

use bevy_ecs::prelude::*;
use serde::{Deserialize, Serialize};

use crate::{
    lsp_types::{Url, WorkspaceFolder},
    util::fs::Fs,
};

#[derive(Debug, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Disabled {
    #[serde(alias = "SHAPES", alias = "shapes")]
    Shapes,
}

/// How Turtle/TriG prefix declarations should be written when the editor inserts
/// them (e.g. during prefix completion or the "add missing prefix" quick-fix).
///
/// Turtle 1.1 allows both the classic `@prefix ex: <...> .` form and the
/// SPARQL-style `PREFIX ex: <...>` form (no trailing dot).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum PrefixFormat {
    /// `@prefix ex: <...> .`
    #[default]
    Turtle,
    /// `PREFIX ex: <...>`
    Sparql,
}

#[derive(Resource, Debug, Default)]
pub struct ServerConfig {
    pub workspaces: Vec<WorkspaceFolder>,
    pub config: Config,
}

#[derive(Debug, Deserialize)]
pub struct Config {
    /// Log level
    #[serde(default = "debug")]
    pub log: String,
    /// Enable turtle
    pub turtle: Option<bool>,
    /// Enable trig
    pub trig: Option<bool>,
    /// Enable jsonld
    pub jsonld: Option<bool>,
    /// Enable sparql
    pub sparql: Option<bool>,
    /// Extra local configuration
    #[serde(flatten)]
    pub local: LocalConfig,
}

#[derive(Debug, Deserialize, Serialize, Default)]
#[serde(default)]
pub struct LocalConfig {
    /// Extra ontologies to import
    pub ontologies: HashSet<String>,
    /// Extra shapes to import
    pub shapes: HashSet<String>,
    /// Features to disable
    pub disabled: HashSet<Disabled>,
    /// disable which prefices from prefix.cc to show
    pub prefix_disabled: HashSet<String>,
    /// confiure completion behavior
    pub completion: CompletionConfig,
    /// Preferred way to write Turtle/TriG prefix declarations when inserting them.
    pub prefix_format: Option<PrefixFormat>,
    /// Namespaces for which IRIs used as properties (predicates) must be defined in a
    /// known ontology. Predicate IRIs that start with one of these namespaces but are
    /// not a known property (and not in [`allowed_properties`]) are flagged with a warning.
    pub closed_namespaces: HashSet<String>,
    /// User-approved property IRIs that should not be flagged by the
    /// [`closed_namespaces`] validation, even though they are absent from the ontology.
    pub allowed_properties: HashSet<String>,
}

/// Lets the user configure how the property completion should happen.
/// There are two main modes: strict and loose (default)
/// On loose, the editor will suggest anything, not caring about the domain.
/// On strict, the editor will only suggest property that have a matching domain or anything if the
/// type could not be determined.
///
/// Both options can be specialized, ie only strict on these properties or only loose on these
/// properties.
///
/// For example { loose: ["http://www.w3.org/2000/01/rdf-schema#"] }, here the editor will be strict, and show properties
/// from rdfs
/// On the other hand { strict: ["http://www.w3.org/ns/shacl#"] }, here the editor will be loose,
/// and only show shacl properties if the objects is the correct type
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
#[serde(untagged)]
pub enum CompletionConfig {
    // "strict" | "loose" | "none"
    Mode(CompletionMode),

    // { "except": [...] }
    Except(ExceptRules),

    // { "strict": [...] }
    Strict(StrictRules),
}

impl Default for CompletionConfig {
    fn default() -> Self {
        Self::Mode(CompletionMode::None)
    }
}

impl CompletionConfig {
    fn combine(&mut self, other: CompletionConfig) {
        use CompletionConfig::*;

        if matches!(other, Mode(CompletionMode::None)) {
            return;
        }

        if matches!(self, Mode(CompletionMode::None)) {
            *self = other;
            return;
        }

        if let Strict(r) = self {
            if let Strict(r2) = other {
                r.strict.extend(r2.strict);
                return;
            }
        }

        if let Except(r) = self {
            if let Except(r2) = other {
                r.loose.extend(r2.loose);
                return;
            }
        }

        *self = other;
    }
    pub fn correct_domain_required(&self, property: &str) -> bool {
        match self {
            CompletionConfig::Mode(CompletionMode::Loose)
            | CompletionConfig::Mode(CompletionMode::None) => false,
            CompletionConfig::Mode(CompletionMode::Strict) => true,
            CompletionConfig::Except(completion_rules) => !completion_rules
                .loose
                .iter()
                .any(|x| property.starts_with(x)),
            CompletionConfig::Strict(completion_rules) => completion_rules
                .strict
                .iter()
                .any(|x| property.starts_with(x)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum CompletionMode {
    #[default]
    None,
    Loose,
    Strict,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize, Default)]
#[serde(deny_unknown_fields)]
pub struct ExceptRules {
    #[serde(default)]
    pub loose: Vec<String>,
}
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize, Default)]
#[serde(deny_unknown_fields)]
pub struct StrictRules {
    #[serde(default)]
    pub strict: Vec<String>,
}

impl LocalConfig {
    /// Combines this config with another config, giving precedence to the other config
    pub fn combine(&mut self, other: LocalConfig) {
        self.ontologies.extend(other.ontologies);
        self.shapes.extend(other.shapes);
        self.disabled.extend(other.disabled);
        self.prefix_disabled.extend(other.prefix_disabled);
        self.completion.combine(other.completion);
        if other.prefix_format.is_some() {
            self.prefix_format = other.prefix_format;
        }
        self.closed_namespaces.extend(other.closed_namespaces);
        self.allowed_properties.extend(other.allowed_properties);
    }
    #[cfg(target_arch = "wasm32")]
    pub async fn global(_: &Fs) -> Option<Self> {
        None
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn global_url() -> Option<Url> {
        let global_path = dirs::config_dir()
            .unwrap_or_else(|| PathBuf::from("."))
            .join("swls/config.json");
        crate::lsp_types::Url::from_file_path(global_path).ok()
    }

    #[cfg(target_arch = "wasm32")]
    pub fn global_url() -> Option<Url> {
        None
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub async fn global(fs: &Fs) -> Option<Self> {
        let url = Self::global_url()?;

        tracing::debug!("Found global config url {}", url.as_str());
        let content = fs.0.read_file(&url).await?;
        tracing::debug!("Read global config content");

        match serde_json::from_str(&content) {
            Ok(x) => Some(x),
            Err(e) => {
                tracing::error!("Deserialize failed\n{:?}", e);
                None
            }
        }
    }

    /// Add `iri` to the global config's `allowed_properties` array on disk,
    /// preserving the rest of the existing global configuration.  Used by the
    /// `swls.allowProperty` command so the user's choice survives restarts.
    #[cfg(not(target_arch = "wasm32"))]
    pub async fn persist_allowed_property(fs: &Fs, iri: &str) -> Option<()> {
        let url = Self::global_url()?;
        let mut existing = Self::global(fs).await.unwrap_or_default();
        existing.allowed_properties.insert(iri.to_string());
        let content = serde_json::to_string_pretty(&existing).ok()?;
        fs.0.write_file(&url, &content).await
    }

    #[cfg(target_arch = "wasm32")]
    pub async fn persist_allowed_property(_: &Fs, _: &str) -> Option<()> {
        None
    }

    pub async fn local(fs: &Fs, url: &Url) -> Option<Self> {
        let url = Url::parse(&format!("{}/.swls/config.json", url.as_str())).ok()?;
        tracing::debug!("Found local config url {}", url.as_str());
        let content = fs.0.read_file(&url).await?;
        tracing::debug!("Read local config content");
        match serde_json::from_str(&content) {
            Ok(x) => Some(x),
            Err(e) => {
                tracing::error!("Deserialize failed\n{:?}", e);
                None
            }
        }
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            log: "debug".to_string(),
            turtle: None,
            trig: None,
            jsonld: None,
            sparql: None,
            local: LocalConfig::default(),
        }
    }
}

fn debug() -> String {
    String::from("debug")
}
