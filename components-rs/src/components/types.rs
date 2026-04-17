//! Core data types produced by the component extractor.
//!
//! All four main types carry an `iri_span: Range<usize>` — the byte range of the `@id` value
//! in the source file.  Combine it with the `source_file` field (on [`CjsModule`] and
//! [`ConfigInstance`]) or with `module_iri → CjsModule.source_file` (for [`CjsComponent`] and
//! [`CjsParameter`]) to build an LSP `Location` for goto-definition responses.
//!
//! Spans use `0..0` as a sentinel when the source byte range could not be determined (e.g.,
//! because the file failed to parse or because the node was synthesised during merging).

use std::ops::Range;

use rdf_parsers::jsonld::convert::JsonLdVal;

pub const PREFIX_OO: &str =
    "https://linkedsoftwaredependencies.org/vocabularies/object-oriented#";
pub const PREFIX_OM: &str =
    "https://linkedsoftwaredependencies.org/vocabularies/object-mapping#";
pub const PREFIX_RDF: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
pub const PREFIX_RDFS: &str = "http://www.w3.org/2000/01/rdf-schema#";
pub const PREFIX_XSD: &str = "http://www.w3.org/2001/XMLSchema#";
pub const PREFIX_DOAP: &str = "http://usefulinc.com/ns/doap#";
pub const PREFIX_OWL: &str = "http://www.w3.org/2002/07/owl#";

// Well-known OO IRIs
pub const IRI_MODULE: &str =
    "https://linkedsoftwaredependencies.org/vocabularies/object-oriented#Module";
pub const IRI_CLASS: &str =
    "https://linkedsoftwaredependencies.org/vocabularies/object-oriented#Class";
pub const IRI_ABSTRACT_CLASS: &str =
    "https://linkedsoftwaredependencies.org/vocabularies/object-oriented#AbstractClass";
pub const IRI_COMPONENT_INSTANCE: &str =
    "https://linkedsoftwaredependencies.org/vocabularies/object-oriented#ComponentInstance";
pub const IRI_COMPONENT: &str =
    "https://linkedsoftwaredependencies.org/vocabularies/object-oriented#component";
pub const IRI_COMPONENT_PATH: &str =
    "https://linkedsoftwaredependencies.org/vocabularies/object-oriented#componentPath";
pub const IRI_PARAMETER: &str =
    "https://linkedsoftwaredependencies.org/vocabularies/object-oriented#parameter";
pub const IRI_CONSTRUCTOR_ARGUMENTS: &str =
    "https://linkedsoftwaredependencies.org/vocabularies/object-oriented#constructorArguments";

pub const IRI_DOAP_NAME: &str = "http://usefulinc.com/ns/doap#name";
pub const IRI_RDFS_SUBCLASS_OF: &str = "http://www.w3.org/2000/01/rdf-schema#subClassOf";
pub const IRI_RDFS_SEE_ALSO: &str = "http://www.w3.org/2000/01/rdf-schema#seeAlso";
pub const IRI_RDFS_RANGE: &str = "http://www.w3.org/2000/01/rdf-schema#range";
pub const IRI_RDFS_COMMENT: &str = "http://www.w3.org/2000/01/rdf-schema#comment";

/// Whether a component is a concrete class, an abstract class, or a singleton instance.
///
/// Used by LSP completion to decide what hover/detail info to show:
/// - `Class` → instantiable, show constructor parameters
/// - `AbstractClass` → not directly instantiable, show as base type in completions
/// - `Instance` → pre-built singleton, no constructor parameters needed
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComponentType {
    Class,
    AbstractClass,
    Instance,
}

impl ComponentType {
    pub fn from_type_iris(types: &[String]) -> Option<Self> {
        for t in types {
            match t.as_str() {
                IRI_CLASS => return Some(ComponentType::Class),
                IRI_ABSTRACT_CLASS => return Some(ComponentType::AbstractClass),
                IRI_COMPONENT_INSTANCE => return Some(ComponentType::Instance),
                _ => {}
            }
        }
        None
    }
}

impl std::fmt::Display for ComponentType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ComponentType::Class => write!(f, "Class"),
            ComponentType::AbstractClass => write!(f, "AbstractClass"),
            ComponentType::Instance => write!(f, "Instance"),
        }
    }
}

/// A CJS module — the top-level container declared in a `components.jsonld` file.
///
/// Each npm package with `lsd:module` exposes exactly one module. The module
/// groups all its component classes and is the entry point for import-path
/// resolution. Use `source_file` + `iri_span` for goto-definition on module IRIs
/// that appear in config `@context` entries.
#[derive(Debug, Clone)]
pub struct CjsModule {
    /// Full expanded IRI of this module (e.g. `https://…/MyPackage`).
    pub iri: String,
    /// npm package name used in `require()` calls (from `doap:name`).
    pub require_name: Option<String>,
    /// All component classes declared in this module.
    pub components: Vec<CjsComponent>,
    /// Absolute path to the `components.jsonld` file that defines this module.
    pub source_file: String,
    /// Byte range of the `@id` value for this module in `source_file`.
    /// Use this for LSP goto-definition when the cursor is on a module IRI.
    pub iri_span: Range<usize>,
}

/// A CJS component — a class, abstract class, or singleton instance declared
/// inside a module.
///
/// Returned by [`ComponentRegistry`] and used for:
/// - **Autocompletion** of `@type` values in config files (all variants)
/// - **Hover documentation** — `comment` provides the description
/// - **Goto-definition** — `source_file` on the parent module + `iri_span`
///   points to the `@id` in the components file
/// - **Parameter completion** — `parameters` drives key completion inside
///   config instance objects
#[derive(Debug, Clone)]
pub struct CjsComponent {
    /// Fully expanded IRI identifying this component.
    pub iri: String,
    pub component_type: ComponentType,
    /// The JavaScript export name to require (from `oo:componentPath`).
    pub require_element: Option<String>,
    /// Human-readable description from `rdfs:comment` — shown in hover cards.
    pub comment: Option<String>,
    /// Constructor parameters for this component (own + inherited via `finalize`).
    pub parameters: Vec<CjsParameter>,
    /// IRIs of parent classes (from `rdfs:subClassOf`). Populated so the LSP
    /// can show the full inheritance chain and resolve inherited parameters.
    pub extends: Vec<String>,
    /// Raw constructor arguments descriptor (kept for completeness).
    pub constructor_arguments: Option<JsonLdVal>,
    /// IRI of the module that declares this component.
    pub module_iri: Option<String>,
    /// Byte range of the `@id` value for this component in the components file.
    /// Combined with `module_iri → CjsModule.source_file` for goto-definition.
    pub iri_span: Range<usize>,
}

/// A single named parameter on a CJS component.
///
/// Used for:
/// - **Key completion** inside config instance objects (the parameter IRI,
///   compacted via the active context, is the JSON key)
/// - **Type-aware value completion** — `range` restricts accepted value types
/// - **Goto-definition** on a config parameter key — `iri_span` points to the
///   `@id` in the component definition file
#[derive(Debug, Clone)]
pub struct CjsParameter {
    /// Fully expanded parameter IRI (the JSON key in a config file after expansion).
    pub iri: String,
    /// Accepted value type IRI (from `rdfs:range`), e.g. `xsd:string`.
    pub range: Option<String>,
    /// Human-readable description from `rdfs:comment`.
    pub comment: Option<String>,
    /// Whether this parameter must be provided in every config instance.
    pub required: bool,
    /// Whether the value is lazily resolved at runtime.
    pub lazy: bool,
    /// Whether only a single value is allowed (no array).
    pub unique: bool,
    /// Default value used when the parameter is omitted.
    pub default_value: Option<JsonLdVal>,
    /// Byte range of the `@id` value for this parameter in the components file.
    pub iri_span: Range<usize>,
}

/// A concrete configuration instance — a pre-wired instantiation of a component
/// with specific parameter values, declared in a `config/*.jsonld` file.
///
/// Config instances are the primary subject of config files. The LSP uses them
/// to validate that all required parameters are present and that value types
/// match the `CjsParameter.range` constraints. `source_file` + `iri_span`
/// support goto-definition when the instance IRI is referenced elsewhere.
#[derive(Debug, Clone)]
pub struct ConfigInstance {
    /// IRI of this config instance.
    pub iri: String,
    /// IRI of the component class this instance instantiates (`@type` value).
    pub component_type_iri: String,
    /// Parameter values keyed by their fully expanded parameter IRI.
    pub parameters: std::collections::HashMap<String, JsonLdVal>,
    /// Absolute path to the config file containing this instance.
    pub source_file: String,
    /// Byte range of the `@id` value for this instance in `source_file`.
    pub iri_span: Range<usize>,
}
