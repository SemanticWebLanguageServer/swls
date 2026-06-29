use std::{collections::HashSet, path::PathBuf};

use bevy_ecs::prelude::*;
use serde::{Deserialize, Serialize};

use crate::{
    lsp_types::{Url, WorkspaceFolder},
    util::fs::Fs,
};

#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Disabled {
    #[serde(alias = "SHAPES", alias = "shapes")]
    Shapes,

    // --- diagnostics ---
    /// Diagnostic for predicate/object IRIs that use an undeclared prefix.
    #[serde(alias = "UNDEFINED_PREFIX", alias = "undefined_prefix")]
    UndefinedPrefix,
    /// Diagnostic for prefixes that are declared but never used.
    #[serde(alias = "UNUSED_PREFIX", alias = "unused_prefix")]
    UnusedPrefix,
    /// Diagnostic for properties under a `closed_namespaces` namespace that are
    /// not known to the ontology and not in `allowed_properties`.
    #[serde(alias = "NAMESPACE_PROPERTIES", alias = "namespace_properties")]
    NamespaceProperties,
    /// Diagnostic for syntax/parse errors.
    #[serde(alias = "SYNTAX_DIAGNOSTICS", alias = "syntax_diagnostics")]
    SyntaxDiagnostics,

    // --- LSP features ---
    /// Master switch: disables `textDocument/completion` entirely (and stops
    /// advertising the capability). See also the `completion_*` variants below
    /// to disable individual completion sources while keeping completion on.
    #[serde(alias = "COMPLETION", alias = "completion")]
    Completion,
    /// Keyword completion (e.g. `@prefix`, `@context`).
    #[serde(alias = "COMPLETION_KEYWORD", alias = "completion_keyword")]
    CompletionKeyword,
    /// RDF class-name completion (e.g. after `a `/`rdf:type`).
    #[serde(alias = "COMPLETION_CLASS", alias = "completion_class")]
    CompletionClass,
    /// RDF property/predicate-name completion.
    #[serde(alias = "COMPLETION_PROPERTY", alias = "completion_property")]
    CompletionProperty,
    /// Prefix-name completion sourced from bundled LOV / prefix.cc data (also
    /// inserts the matching declaration).
    #[serde(alias = "COMPLETION_PREFIX", alias = "completion_prefix")]
    CompletionPrefix,
    /// Subject-IRI completion that reuses subjects already used in the document
    /// (Turtle only).
    #[serde(alias = "COMPLETION_SUBJECT", alias = "completion_subject")]
    CompletionSubject,

    /// Master switch: disables `textDocument/hover` entirely (and stops
    /// advertising the capability). See also the `hover_*` variants below to
    /// disable individual hover sources while keeping hover on.
    #[serde(alias = "HOVER", alias = "hover")]
    Hover,
    /// Hover showing the inferred RDF type(s) of the term under the cursor.
    #[serde(alias = "HOVER_TYPE", alias = "hover_type")]
    HoverType,
    /// Hover showing ontology documentation for an RDF class IRI.
    #[serde(alias = "HOVER_CLASS", alias = "hover_class")]
    HoverClass,
    /// Hover showing ontology documentation for a property/predicate IRI.
    #[serde(alias = "HOVER_PROPERTY", alias = "hover_property")]
    HoverProperty,
    /// Hover explanation shown for a property that is only accepted because it
    /// is in the user's `allowed_properties` allow-list.
    #[serde(
        alias = "HOVER_EXCLUDED_PROPERTY",
        alias = "hover_excluded_property"
    )]
    HoverExcludedProperty,

    /// Master switch: disables `textDocument/definition` entirely (and stops
    /// advertising the capability). Covers generic RDF term goto-definition.
    #[serde(alias = "GOTO_DEFINITION", alias = "goto_definition")]
    GotoDefinition,
    /// Components.js-specific goto-definition: resolves component/module/
    /// parameter IRIs and import/context URLs to their source file.
    #[serde(
        alias = "GOTO_DEFINITION_COMPONENTS_JS",
        alias = "goto_definition_components_js"
    )]
    GotoDefinitionComponentsJs,
    #[serde(alias = "GOTO_TYPE_DEFINITION", alias = "goto_type_definition")]
    GotoTypeDefinition,
    #[serde(alias = "REFERENCES", alias = "references")]
    References,
    #[serde(alias = "RENAME", alias = "rename")]
    Rename,
    #[serde(alias = "SEMANTIC_TOKENS", alias = "semantic_tokens")]
    SemanticTokens,
    #[serde(alias = "FORMAT", alias = "format")]
    Format,
    /// Auto-inserts the missing prefix/context declaration while typing
    /// `prefix:` (formerly named `on_type_format`).
    #[serde(
        alias = "PREFIX_AUTO_INSERT",
        alias = "prefix_auto_insert",
        alias = "ON_TYPE_FORMAT",
        alias = "on_type_format"
    )]
    PrefixAutoInsert,
    /// Master switch: disables `textDocument/codeAction` entirely (and stops
    /// advertising the capability). The "add missing prefix" and "allow
    /// property" quick-fixes are controlled by their respective diagnostic
    /// toggles ([`Disabled::UndefinedPrefix`], [`Disabled::NamespaceProperties`])
    /// instead, since they only make sense alongside their diagnostic.
    #[serde(alias = "CODE_ACTION", alias = "code_action")]
    CodeAction,
    /// "Organize Imports" quick-fix that sorts `@prefix` declarations (Turtle).
    #[serde(
        alias = "CODE_ACTION_ORGANIZE_IMPORTS",
        alias = "code_action_organize_imports"
    )]
    CodeActionOrganizeImports,
    /// "Extract blank node" / "Inline named blank node" quick-fixes.
    #[serde(
        alias = "CODE_ACTION_BLANK_NODE_REFACTOR",
        alias = "code_action_blank_node_refactor"
    )]
    CodeActionBlankNodeRefactor,
    #[serde(alias = "INLAY_HINT", alias = "inlay_hint")]
    InlayHint,
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
    /// Enable n3
    pub n3: Option<bool>,
    /// Enable jsonld
    pub jsonld: Option<bool>,
    /// Enable sparql
    pub sparql: Option<bool>,
    /// Per-language `textDocument/formatting` toggles. Formatting is disabled by
    /// default for every language except Turtle.
    #[serde(default)]
    pub format: FormatConfig,
    /// Extra local configuration
    #[serde(flatten)]
    pub local: LocalConfig,
}

/// Per-language toggles for `textDocument/formatting`.
///
/// Each field is an `Option<bool>`, but the *effective* default differs per
/// language: Turtle formatting is on unless explicitly disabled, while every
/// other language is off unless explicitly enabled. Use the accessor methods
/// ([`FormatConfig::turtle`] etc.) to resolve a field to its effective value
/// rather than reading the `Option` directly.
#[derive(Debug, Deserialize, Serialize, Default)]
#[serde(default)]
pub struct FormatConfig {
    /// Enable Turtle formatting (default: `true`).
    pub turtle: Option<bool>,
    /// Enable TriG formatting (default: `false`).
    pub trig: Option<bool>,
    /// Enable N3 formatting (default: `false`).
    pub n3: Option<bool>,
    /// Enable JSON-LD formatting (default: `false`).
    pub jsonld: Option<bool>,
}

impl FormatConfig {
    /// Whether Turtle formatting is enabled (defaults to `true`).
    pub fn turtle(&self) -> bool {
        self.turtle.unwrap_or(true)
    }
    /// Whether TriG formatting is enabled (defaults to `false`).
    pub fn trig(&self) -> bool {
        self.trig.unwrap_or(false)
    }
    /// Whether N3 formatting is enabled (defaults to `false`).
    pub fn n3(&self) -> bool {
        self.n3.unwrap_or(false)
    }
    /// Whether JSON-LD formatting is enabled (defaults to `false`).
    pub fn jsonld(&self) -> bool {
        self.jsonld.unwrap_or(false)
    }
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
    /// Whether the given feature/diagnostic has been disabled by the user.
    pub fn is_disabled(&self, d: Disabled) -> bool {
        self.disabled.contains(&d)
    }

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
            n3: None,
            jsonld: None,
            sparql: None,
            format: FormatConfig::default(),
            local: LocalConfig::default(),
        }
    }
}

fn debug() -> String {
    String::from("debug")
}
