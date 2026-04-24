//! Project-wide state: the shared lookup tables built from all discovered packages.
//!
//! [`ModuleState`] is the first thing an LSP session should create.  Call
//! [`ModuleState::build`] with the workspace root and it will:
//!
//! 1. Walk all ancestor `node_modules/` directories (via [`crate::discovery`]).
//! 2. Parse every `package.json` that has `lsd:` fields.
//! 3. Build three lookup tables consumed by the registries:
//!    - `component_modules` — maps module IRI → URL to `components.jsonld`
//!    - `contexts` — maps context IRI → parsed [`rdf_parsers::jsonld::convert::JsonLdVal`]
//!    - `import_paths` — maps IRI prefix → local directory URL (for resolving `rdfs:seeAlso` imports)
//!
//! The state is read-only after construction; pass `&state` into
//! [`crate::components::registry::ComponentRegistry`] and
//! [`crate::config::registry::ConfigRegistry`].

use std::collections::HashMap;

use rdf_parsers::jsonld::convert::{parse_json, JsonLdVal};
use url::Url;

use crate::discovery::node_modules;
use crate::discovery::package_json::{self, get_module_iri, PackageJson};
use crate::error::Result;
use crate::fs::Fs;

/// Represents the fully-resolved state of all discoverable CJS modules.
/// Mirrors the TypeScript `IModuleState`.
#[derive(Debug, Clone)]
pub struct ModuleState {
    /// URL of the root project directory.
    pub main_module_path: Url,
    /// All ancestor URLs used for node_modules searching.
    pub node_module_import_paths: Vec<Url>,
    /// All discovered node module directory URLs.
    pub node_module_paths: Vec<Url>,
    /// Parsed package.json by module directory URL.
    pub package_jsons: HashMap<Url, PackageJson>,
    /// Component modules: module IRI → (major version → absolute components.jsonld URL).
    pub component_modules: HashMap<String, HashMap<u64, Url>>,
    /// Contexts: context IRI → parsed content of context file.
    pub contexts: HashMap<String, JsonLdVal>,
    /// Import paths: IRI prefix → absolute local directory URL.
    pub import_paths: HashMap<String, Url>,
}

impl ModuleState {
    pub fn empty() -> Self {
        Self {
            main_module_path: Url::parse("file:///tmp/").unwrap(),
            node_module_import_paths: Vec::new(),
            node_module_paths: Vec::new(),
            package_jsons: HashMap::new(),
            component_modules: HashMap::new(),
            contexts: HashMap::new(),
            import_paths: HashMap::new(),
        }
    }
    /// Build the full module state from a project root URL.
    pub async fn build(fs: &dyn Fs, main_module_path: &Url) -> Result<Self> {
        let main_module_path = fs.canonicalize(main_module_path).await?;

        tracing::info!("Building module state from: {}", main_module_path.as_str());

        let node_module_import_paths =
            node_modules::build_node_module_import_paths(&main_module_path);
        let node_module_paths =
            node_modules::build_node_module_paths(fs, &node_module_import_paths).await?;

        tracing::info!("Discovered {} node module paths", node_module_paths.len());

        let mut package_jsons = package_json::read_package_jsons(fs, &node_module_paths).await?;
        package_json::preprocess_all(fs, &mut package_jsons).await;

        let component_modules = build_component_modules(&package_jsons)?;
        let contexts = build_component_contexts(fs, &package_jsons).await?;
        let import_paths = build_component_import_paths(&package_jsons)?;

        tracing::info!(
            "Found {} component modules, {} contexts, {} import paths",
            component_modules.len(),
            contexts.len(),
            import_paths.len()
        );

        Ok(ModuleState {
            main_module_path,
            node_module_import_paths,
            node_module_paths,
            package_jsons,
            component_modules,
            contexts,
            import_paths,
        })
    }
}

/// Build the component modules map: module IRI → (major version → absolute URL to components.jsonld).
fn build_component_modules(
    package_jsons: &HashMap<Url, PackageJson>,
) -> Result<HashMap<String, HashMap<u64, Url>>> {
    let mut modules: HashMap<String, HashMap<u64, Url>> = HashMap::new();
    let mut versions: HashMap<String, HashMap<u64, semver::Version>> = HashMap::new();

    for (module_path, pkg) in package_jsons {
        let Some(module_iri) = get_module_iri(pkg) else {
            continue;
        };
        let Some(components_rel) = &pkg.lsd_components else {
            continue;
        };
        let Ok(version) = semver::Version::parse(&pkg.version) else {
            continue;
        };

        let absolute_path = match module_path.join(components_rel) {
            Ok(u) => u,
            Err(_) => continue,
        };

        let major = version.major;
        let entry = modules.entry(module_iri.clone()).or_default();
        let ver_entry = versions.entry(module_iri.clone()).or_default();

        if let Some(existing_ver) = ver_entry.get(&major) {
            if &version > existing_ver {
                entry.insert(major, absolute_path);
                ver_entry.insert(major, version);
            }
        } else {
            entry.insert(major, absolute_path);
            ver_entry.insert(major, version);
        }
    }

    Ok(modules)
}

/// Build the contexts map: context IRI → parsed content.
async fn build_component_contexts(
    fs: &dyn Fs,
    package_jsons: &HashMap<Url, PackageJson>,
) -> Result<HashMap<String, JsonLdVal>> {
    let mut contexts: HashMap<String, JsonLdVal> = HashMap::new();
    let mut ctx_versions: HashMap<String, semver::Version> = HashMap::new();

    for (module_path, pkg) in package_jsons {
        let Some(ctx_map) = &pkg.lsd_contexts else {
            continue;
        };
        let Ok(version) = semver::Version::parse(&pkg.version) else {
            continue;
        };

        for (ctx_iri, rel_path) in ctx_map {
            let file_url = match module_path.join(rel_path) {
                Ok(u) => u,
                Err(_) => continue,
            };

            if let Some(existing_ver) = ctx_versions.get(ctx_iri) {
                if &version <= existing_ver {
                    continue;
                }
            }

            match fs.read_to_string(&file_url).await {
                Ok(contents) => match parse_json(&contents) {
                    Some(parsed) => {
                        contexts.insert(ctx_iri.clone(), parsed);
                        ctx_versions.insert(ctx_iri.clone(), version.clone());
                    }
                    None => {
                        tracing::warn!("Failed to parse context file {}", file_url.as_str());
                    }
                },
                Err(e) => {
                    tracing::warn!("Failed to read context file {}: {}", file_url.as_str(), e);
                }
            }
        }
    }

    Ok(contexts)
}

/// Build the import paths map: IRI prefix → absolute directory URL.
fn build_component_import_paths(
    package_jsons: &HashMap<Url, PackageJson>,
) -> Result<HashMap<String, Url>> {
    let mut import_paths: HashMap<String, Url> = HashMap::new();
    let mut path_versions: HashMap<String, semver::Version> = HashMap::new();

    for (module_path, pkg) in package_jsons {
        let Some(ip_map) = &pkg.lsd_import_paths else {
            continue;
        };
        let Ok(version) = semver::Version::parse(&pkg.version) else {
            continue;
        };

        for (iri_prefix, rel_path) in ip_map {
            // Ensure the relative path is treated as a directory by adding trailing slash.
            let rel_dir = if rel_path.ends_with('/') {
                rel_path.clone()
            } else {
                format!("{}/", rel_path)
            };
            let abs_url = match module_path.join(&rel_dir) {
                Ok(u) => u,
                Err(e) => {
                    tracing::warn!("Failed to join import path: {}", e);
                    continue;
                }
            };

            if let Some(existing_ver) = path_versions.get(iri_prefix) {
                if &version <= existing_ver {
                    continue;
                }
            }

            import_paths.insert(iri_prefix.clone(), abs_url);
            path_versions.insert(iri_prefix.clone(), version.clone());
        }
    }

    Ok(import_paths)
}
