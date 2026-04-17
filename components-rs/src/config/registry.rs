//! Config registry: loads `config/*.jsonld` files and extracts concrete instances.
//!
//! [`ConfigRegistry::discover_configs`] scans every directory whose IRI prefix contains
//! `/config/` in [`ModuleState::import_paths`](crate::module_state::ModuleState) and calls
//! [`ConfigRegistry::load_config_file`] on each `.json`/`.jsonld` file found.
//!
//! Each file is parsed with [`rdf_parsers::jsonld::convert::parse_json`] so that `@id` spans
//! are available.  `import` directives within a config file are followed recursively, mirroring
//! what the Components.js runtime does at startup.

use std::collections::HashMap;
use std::ops::Range;
use std::path::Path;

use rdf_parsers::jsonld::convert::{parse_json, JsonLdVal};

use crate::components::registry::{collect_id_spans, resolve_iri_to_path};
use crate::components::types::*;
use crate::context::expand::ContextResolver;
use crate::error::Result;
use crate::fs::{self as cfs, Fs};
use crate::module_state::ModuleState;

/// Registry of discovered configuration instances.
///
/// Config instances are the primary subjects of `config/*.jsonld` files — each
/// one wires a concrete set of parameter values to a component class. The LSP
/// uses this registry to:
/// - **Validate** that all required parameters are present in an open config file
/// - **Provide diagnostics** when a `@type` IRI does not match any known component
/// - **Enable goto-definition** on instance IRIs via `source_file` + `iri_span`
#[derive(Debug, Clone)]
pub struct ConfigRegistry {
    pub configs: Vec<ConfigInstance>,
}

impl ConfigRegistry {
    pub fn new() -> Self {
        Self {
            configs: Vec::new(),
        }
    }

    /// Discover and load all config files reachable from the import paths.
    pub async fn discover_configs(&mut self, fs: &dyn Fs, state: &ModuleState) -> Result<()> {
        for (iri_prefix, local_dir) in &state.import_paths {
            if iri_prefix.contains("/config/") && fs.is_dir(local_dir).await {
                self.load_config_directory(fs, local_dir, state).await?;
            }
        }
        Ok(())
    }

    /// Load a single config file, following any `import` directives it contains.
    pub fn load_config_file<'a>(
        &'a mut self,
        fs: &'a dyn Fs,
        path: &'a Path,
        state: &'a ModuleState,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<()>> + Send + 'a>> {
        Box::pin(async move {
            tracing::debug!("Loading config file: {}", path.display());

            let contents = fs.read_to_string(path).await?;
            let Some(doc) = parse_json(&contents) else {
                tracing::warn!("Failed to parse config file: {}", path.display());
                return Ok(());
            };

            let resolver = if let Some(ctx) = doc.get("@context") {
                ContextResolver::from_context_value(ctx, &state.contexts)?
            } else {
                ContextResolver::new()
            };

            let mut id_spans: HashMap<String, Range<usize>> = HashMap::new();
            collect_id_spans(&doc, &resolver, &mut id_spans);

            self.process_imports(fs, &doc, &resolver, state, path).await?;

            let entries: Vec<&JsonLdVal> = if let Some(graph) = doc.get("@graph") {
                match graph.as_array() {
                    Some(arr) => arr.iter().map(|(v, _)| v).collect(),
                    None => vec![graph],
                }
            } else if doc.get("@id").is_some() {
                vec![&doc]
            } else {
                vec![]
            };

            for entry in entries {
                if let Some(config) = self.parse_config_entry(entry, &resolver, path, &id_spans) {
                    self.configs.push(config);
                }
            }

            Ok(())
        })
    }

    async fn process_imports(
        &mut self,
        fs: &dyn Fs,
        doc: &JsonLdVal,
        resolver: &ContextResolver,
        state: &ModuleState,
        source_path: &Path,
    ) -> Result<()> {
        if let Some(import_val) = doc.get("import") {
            let iris: Vec<String> = match import_val {
                JsonLdVal::Str(s) => vec![resolver.expand_term(s)],
                _ => import_val
                    .as_array()
                    .map(|arr| {
                        arr.iter()
                            .filter_map(|(v, _)| v.as_str())
                            .map(|s| resolver.expand_term(s))
                            .collect()
                    })
                    .unwrap_or_default(),
            };
            for iri in iris {
                if let Some(local_path) = resolve_iri_to_path(&iri, &state.import_paths) {
                    if cfs::exists(fs, &local_path).await && local_path != source_path {
                        self.load_config_file(fs, &local_path, state).await?;
                    }
                }
            }
        }
        Ok(())
    }

    fn parse_config_entry(
        &self,
        value: &JsonLdVal,
        resolver: &ContextResolver,
        source_path: &Path,
        id_spans: &HashMap<String, Range<usize>>,
    ) -> Option<ConfigInstance> {
        let id_str = value.get("@id")?.as_str()?;
        let iri = resolver.expand_term(id_str);
        let iri_span = id_spans.get(&iri).cloned().unwrap_or(0..0);

        let type_iri = match value.get("@type") {
            Some(JsonLdVal::Str(t)) => resolver.expand_term(t),
            Some(v) => v
                .as_array()?
                .iter()
                .filter_map(|(item, _)| item.as_str())
                .map(|s| resolver.expand_term(s))
                .next()?,
            None => return None,
        };

        if type_iri.contains("Override") {
            return None;
        }

        let mut parameters = HashMap::new();
        if let Some(members) = value.as_object() {
            for (key, _, _, val) in members {
                if key.starts_with('@') {
                    continue;
                }
                let expanded_key = resolver.expand_term(key);
                parameters.insert(expanded_key, val.clone());
            }
        }

        Some(ConfigInstance {
            iri,
            component_type_iri: type_iri,
            parameters,
            source_file: source_path.display().to_string(),
            iri_span,
        })
    }

    async fn load_config_directory(
        &mut self,
        fs: &dyn Fs,
        dir: &Path,
        state: &ModuleState,
    ) -> Result<()> {
        if !fs.is_dir(dir).await {
            return Ok(());
        }

        let files = cfs::walk_dir(fs, dir).await?;
        for path in files {
            let is_config = path
                .extension()
                .is_some_and(|ext| ext == "jsonld" || ext == "json");
            if is_config {
                self.load_config_file(fs, &path, state).await?;
            }
        }

        Ok(())
    }
}
