use std::{collections::HashSet, path::PathBuf};

use bevy_ecs::prelude::*;
use serde::Deserialize;

use crate::{
    lsp_types::{Url, WorkspaceFolder},
    util::fs::Fs,
};

#[derive(Debug, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Disabled {
    #[serde(alias = "SHAPES", alias = "shapes")]
    Shapes,
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
    /// Enable jsonld
    pub jsonld: Option<bool>,
    /// Enable sparql
    pub sparql: Option<bool>,
    /// Extra local configuration
    #[serde(flatten)]
    pub local: LocalConfig,
}

#[derive(Debug, Deserialize, Default)]
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
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
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

#[derive(Debug, Clone, PartialEq, Eq, Default, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum CompletionMode {
    #[default]
    None,
    Loose,
    Strict,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Default)]
#[serde(deny_unknown_fields)]
pub struct ExceptRules {
    #[serde(default)]
    pub loose: Vec<String>,
}
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Default)]
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
    }
    #[cfg(target_arch = "wasm32")]
    pub async fn global(_: &Fs) -> Option<Self> {
        None
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub async fn global(fs: &Fs) -> Option<Self> {
        let global_path = dirs::config_dir()
            .unwrap_or_else(|| PathBuf::from("."))
            .join("swls/config.json");
        let url = crate::lsp_types::Url::from_file_path(global_path).ok()?;

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
            jsonld: None,
            sparql: None,
            local: LocalConfig::default(),
        }
    }
}

fn debug() -> String {
    String::from("debug")
}
