//! `package.json` parsing and pre-processing for Components.js metadata.
//!
//! Reads the `lsd:*` fields that Components.js adds to `package.json`:
//!
//! | Field | Meaning |
//! |-------|---------|
//! | `lsd:module` | `true` (auto-expand) or an explicit module IRI string |
//! | `lsd:components` | relative path to `components.jsonld` |
//! | `lsd:contexts` | map of context IRI → relative path to the context file |
//! | `lsd:importPaths` | map of IRI prefix → relative directory for import resolution |
//! | `lsd:basePath` | optional path prefix applied to all relative paths above |
//!
//! [`preprocess_package_json`] handles the `lsd:module: true` shorthand: it expands the
//! module IRI to `https://linkedsoftwaredependencies.org/bundles/npm/<name>` and
//! auto-detects the standard `components/` and `config/` directories.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::error::{ComponentsJsError, Result};
use crate::fs::Fs;

const LSD_BUNDLES_PREFIX: &str = "https://linkedsoftwaredependencies.org/bundles/npm/";

/// Parsed package.json with CJS-specific fields.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageJson {
    #[serde(default)]
    pub name: String,
    #[serde(default)]
    pub version: String,

    /// `lsd:module` — can be `true` (auto-expand) or a string IRI.
    #[serde(rename = "lsd:module", default)]
    pub lsd_module: Option<LsdModule>,

    /// `lsd:components` — relative path to components.jsonld
    #[serde(rename = "lsd:components", default)]
    pub lsd_components: Option<String>,

    /// `lsd:contexts` — map of context IRI → relative path
    #[serde(rename = "lsd:contexts", default)]
    pub lsd_contexts: Option<HashMap<String, String>>,

    /// `lsd:importPaths` — map of IRI prefix → relative path
    #[serde(rename = "lsd:importPaths", default)]
    pub lsd_import_paths: Option<HashMap<String, String>>,

    /// `lsd:basePath` — optional base path prefix
    #[serde(rename = "lsd:basePath", default)]
    pub lsd_base_path: Option<String>,
}

/// `lsd:module` can be either `true` or a string IRI.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum LsdModule {
    Bool(bool),
    Iri(String),
}

impl LsdModule {
    pub fn as_iri(&self) -> Option<&str> {
        match self {
            LsdModule::Iri(s) => Some(s.as_str()),
            LsdModule::Bool(_) => None,
        }
    }

    pub fn is_enabled(&self) -> bool {
        match self {
            LsdModule::Bool(b) => *b,
            LsdModule::Iri(_) => true,
        }
    }
}

/// Read package.json files from all given module paths.
pub async fn read_package_jsons(
    fs: &dyn Fs,
    module_paths: &[PathBuf],
) -> Result<HashMap<PathBuf, PackageJson>> {
    let mut result = HashMap::new();
    for module_path in module_paths {
        let pkg_path = module_path.join("package.json");
        if fs.is_file(&pkg_path).await {
            let contents = fs.read_to_string(&pkg_path).await?;
            let pkg: PackageJson =
                serde_json::from_str(&contents).map_err(|e| ComponentsJsError::JsonParse {
                    path: pkg_path.display().to_string(),
                    source: e,
                })?;
            result.insert(module_path.clone(), pkg);
        }
    }
    Ok(result)
}

/// Preprocess a package.json: expand `lsd:module: true` into full IRI,
/// auto-detect components and config directories.
/// Mirrors `ModuleStateBuilder.preprocessPackageJson`.
pub async fn preprocess_package_json(fs: &dyn Fs, package_path: &Path, pkg: &mut PackageJson) {
    let needs_expansion = matches!(pkg.lsd_module, Some(LsdModule::Bool(true)));
    if !needs_expansion {
        return;
    }

    // Expand lsd:module to full IRI
    let module_iri = format!("{LSD_BUNDLES_PREFIX}{}", pkg.name);
    pkg.lsd_module = Some(LsdModule::Iri(module_iri.clone()));

    let base_path = pkg.lsd_base_path.as_deref().unwrap_or("");

    // Auto-detect components/components.jsonld
    let components_file = package_path
        .join(base_path)
        .join("components/components.jsonld");
    if fs.is_file(&components_file).await {
        pkg.lsd_components = Some(format!("{base_path}components/components.jsonld"));
    }

    // Compute major version for IRI construction
    let major = parse_major_version(&pkg.version).unwrap_or(0);
    let base_iri = format!("{module_iri}/^{major}.0.0/");

    // Auto-detect context
    let context_file = package_path
        .join(base_path)
        .join("components/context.jsonld");
    if fs.is_file(&context_file).await {
        let mut contexts = HashMap::new();
        contexts.insert(
            format!("{base_iri}components/context.jsonld"),
            format!("{base_path}components/context.jsonld"),
        );
        pkg.lsd_contexts = Some(contexts);
    }

    // Auto-detect import paths
    let mut import_paths = HashMap::new();
    let components_dir = package_path.join(base_path).join("components");
    if fs.is_dir(&components_dir).await {
        import_paths.insert(
            format!("{base_iri}components/"),
            format!("{base_path}components/"),
        );
    }
    let config_dir = package_path.join(base_path).join("config");
    if fs.is_dir(&config_dir).await {
        import_paths.insert(format!("{base_iri}config/"), format!("{base_path}config/"));
    }
    pkg.lsd_import_paths = Some(import_paths);
}

/// Preprocess all package.json files.
pub async fn preprocess_all(fs: &dyn Fs, package_jsons: &mut HashMap<PathBuf, PackageJson>) {
    let keys: Vec<PathBuf> = package_jsons.keys().cloned().collect();
    for path in keys {
        if let Some(pkg) = package_jsons.get_mut(&path) {
            preprocess_package_json(fs, &path, pkg).await;
        }
    }
}

fn parse_major_version(version: &str) -> Option<u64> {
    semver::Version::parse(version).ok().map(|v| v.major)
}

/// Get the resolved module IRI from a PackageJson (after preprocessing).
pub fn get_module_iri(pkg: &PackageJson) -> Option<String> {
    match &pkg.lsd_module {
        Some(LsdModule::Iri(iri)) => Some(iri.clone()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lsd_module_deserialization_bool() {
        let json = r#"{"name":"test","version":"1.0.0","lsd:module":true}"#;
        let pkg: PackageJson = serde_json::from_str(json).unwrap();
        assert!(matches!(pkg.lsd_module, Some(LsdModule::Bool(true))));
    }

    #[test]
    fn test_lsd_module_deserialization_string() {
        let json = r#"{"name":"test","version":"1.0.0","lsd:module":"https://example.org/test"}"#;
        let pkg: PackageJson = serde_json::from_str(json).unwrap();
        assert!(matches!(pkg.lsd_module, Some(LsdModule::Iri(_))));
        assert_eq!(
            pkg.lsd_module.unwrap().as_iri(),
            Some("https://example.org/test")
        );
    }

    #[test]
    fn test_no_lsd_module() {
        let json = r#"{"name":"test","version":"1.0.0"}"#;
        let pkg: PackageJson = serde_json::from_str(json).unwrap();
        assert!(pkg.lsd_module.is_none());
    }
}
