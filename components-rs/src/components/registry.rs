//! Component registry: loads and merges all `components.jsonld` files.
//!
//! See the [`components`](crate::components) module doc for the two-phase loading overview.
//!
//! ## Span extraction
//!
//! Every file is parsed with [`rdf_parsers::jsonld::convert::parse_json`] before any processing,
//! which yields a [`JsonLdVal`] tree carrying byte-range spans for every token.
//! [`collect_id_spans`] walks the tree and records `expanded_iri → byte_range` in a flat map.
//! These spans are threaded through both collection phases so that every
//! [`CjsModule`](crate::components::types::CjsModule),
//! [`CjsComponent`](crate::components::types::CjsComponent), and
//! [`CjsParameter`](crate::components::types::CjsParameter) ends up with an `iri_span` that
//! the LSP can use for goto-definition without re-reading the source file.

use std::collections::HashMap;
use std::ops::Range;
use std::path::{Path, PathBuf};

use rdf_parsers::jsonld::convert::{parse_json, JsonLdVal};

use crate::components::types::*;
use crate::context::expand::{self, ContextResolver, ExpandedNode};
use crate::error::Result;
use crate::fs::{self as cfs, Fs};
use crate::module_state::ModuleState;

// ── Span helpers ─────────────────────────────────────────────────────────────

/// Walk a `JsonLdVal` tree and collect the byte spans of every `@id` value,
/// keyed by the **expanded** IRI (resolved via `resolver`).
///
/// A span entry maps `expanded_iri → byte_range_of_@id_value_in_source`.
pub fn collect_id_spans(
    val: &JsonLdVal,
    resolver: &ContextResolver,
    out: &mut HashMap<String, Range<usize>>,
) {
    match val {
        JsonLdVal::Object(members, _) => {
            for (key, _key_span, val_span, value) in members {
                if key == "@id" {
                    if let Some(s) = value.as_str() {
                        let expanded = resolver.expand_term(s);
                        out.entry(expanded).or_insert_with(|| val_span.clone());
                    }
                }
                collect_id_spans(value, resolver, out);
            }
        }
        JsonLdVal::Array(items) => {
            for (item, _) in items {
                collect_id_spans(item, resolver, out);
            }
        }
        _ => {}
    }
}

/// Walk a `JsonLdVal` tree and record the source file for every `@id` value
/// (first-seen wins, consistent with [`collect_id_spans`]).
///
/// The resulting map is used to determine which file a component's `@id` was
/// originally defined in, even when the component is later merged into a
/// parent node (e.g., the module node in `components.jsonld`).
pub fn collect_id_sources(
    val: &JsonLdVal,
    resolver: &ContextResolver,
    source_file: &str,
    out: &mut HashMap<String, String>,
) {
    match val {
        JsonLdVal::Object(members, _) => {
            for (key, _, _, value) in members {
                if key == "@id" {
                    if let Some(s) = value.as_str() {
                        let expanded = resolver.expand_term(s);
                        out.entry(expanded)
                            .or_insert_with(|| source_file.to_string());
                    }
                }
                collect_id_sources(value, resolver, source_file, out);
            }
        }
        JsonLdVal::Array(items) => {
            for (item, _) in items {
                collect_id_sources(item, resolver, source_file, out);
            }
        }
        _ => {}
    }
}

// ── Registry ─────────────────────────────────────────────────────────────────

/// Registry of all discovered CJS components and modules.
///
/// Populated by [`register_available_modules`](Self::register_available_modules)
/// (two-phase: collect → merge → process) and then finalised with
/// [`finalize`](Self::finalize) which resolves inherited parameters.
///
/// Primary LSP uses:
/// - **Completion** — iterate `components` to offer `@type` values
/// - **Hover** — look up a component by IRI to get `comment` and parameter list
/// - **Goto-definition** — `CjsComponent::iri_span` + `CjsModule::source_file`
///   give the exact location in the components file
#[derive(Debug, Clone)]
pub struct ComponentRegistry {
    /// All components indexed by their fully expanded IRI.
    pub components: HashMap<String, CjsComponent>,
    /// All modules indexed by their fully expanded IRI.
    pub modules: HashMap<String, CjsModule>,
    /// All parameters indexed by their fully expanded IRI, pointing to
    /// `(source_file, iri_span)` for goto-definition without searching every file.
    pub parameters: HashMap<String, (String, Range<usize>)>,
    /// Raw source text of every component file that was loaded, keyed by the
    /// absolute file path (same strings used in `CjsComponent::source_file` and
    /// `CjsModule::source_file`).  Used by the LSP to convert `iri_span` byte
    /// offsets to LSP line/column positions without re-reading files from disk.
    pub file_sources: HashMap<String, String>,
}

/// Intermediate node collected during phase 1, before merging by `@id`.
///
/// Stores the context resolver from the file where this node was first seen so
/// that inline component `@id` strings can be expanded later during phase 2
/// without re-reading the source file.
#[derive(Debug, Clone)]
struct CollectedNode {
    id: String,
    types: Vec<String>,
    properties: HashMap<String, Vec<JsonLdVal>>,
    source_file: String,
    /// Byte span of the `@id` value in `source_file`.
    id_span: Range<usize>,
    /// Context resolver active in `source_file`; kept so that compact IRIs
    /// inside this node's property values can be expanded during phase 2.
    resolver: ContextResolver,
}

impl ComponentRegistry {
    pub fn new() -> Self {
        Self {
            components: HashMap::new(),
            modules: HashMap::new(),
            parameters: HashMap::new(),
            file_sources: HashMap::new(),
        }
    }

    /// Discover and register all modules reachable from the module state.
    ///
    /// **Phase 1** — recursively loads every `components.jsonld` file (following
    /// `rdfs:seeAlso` imports), parses each with `JsonLdVal` to harvest `@id`
    /// byte spans, then merges all nodes by IRI into `all_nodes`.
    ///
    /// **Phase 2** — walks `all_nodes` to find `oo:Module` nodes and extracts
    /// their inline component definitions into `CjsModule`/`CjsComponent`.
    pub async fn register_available_modules(
        &mut self,
        fs: &dyn Fs,
        state: &ModuleState,
    ) -> Result<()> {
        let mut all_nodes: HashMap<String, CollectedNode> = HashMap::new();
        let mut visited_files: std::collections::HashSet<PathBuf> =
            std::collections::HashSet::new();
        let mut id_spans: HashMap<String, Range<usize>> = HashMap::new();
        let mut id_source_files: HashMap<String, String> = HashMap::new();
        let mut file_sources: HashMap<String, String> = HashMap::new();

        for version_map in state.component_modules.values() {
            for component_path in version_map.values() {
                if cfs::exists(fs, component_path).await {
                    self.collect_nodes_from_file(
                        fs,
                        component_path,
                        state,
                        &mut all_nodes,
                        &mut visited_files,
                        &mut id_spans,
                        &mut id_source_files,
                        &mut file_sources,
                    )
                    .await?;
                } else {
                    tracing::warn!(
                        "Component file does not exist: {}",
                        component_path.display()
                    );
                }
            }
        }

        tracing::info!(
            "Collected {} unique nodes from component files",
            all_nodes.len()
        );

        self.process_merged_nodes(&all_nodes, &id_spans, &id_source_files, state)?;
        self.file_sources = file_sources;

        Ok(())
    }

    /// Recursively load a component file and its `rdfs:seeAlso` imports.
    fn collect_nodes_from_file<'a>(
        &'a self,
        fs: &'a dyn Fs,
        path: &'a Path,
        state: &'a ModuleState,
        all_nodes: &'a mut HashMap<String, CollectedNode>,
        visited: &'a mut std::collections::HashSet<PathBuf>,
        id_spans: &'a mut HashMap<String, Range<usize>>,
        id_source_files: &'a mut HashMap<String, String>,
        file_sources: &'a mut HashMap<String, String>,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<()>> + 'a + Send>> {
        Box::pin(async move {
            let canonical = fs
                .canonicalize(path)
                .await
                .unwrap_or_else(|_| path.to_path_buf());
            if visited.contains(&canonical) {
                return Ok(());
            }
            visited.insert(canonical.clone());

            tracing::debug!("Loading component file: {}", path.display());

            let contents = fs.read_to_string(path).await?;
            let Some(doc) = parse_json(&contents) else {
                tracing::warn!("Failed to parse component file: {}", path.display());
                return Ok(());
            };

            let resolver = if let Some(ctx) = doc.get("@context") {
                ContextResolver::from_context_value(ctx, &state.contexts)?
            } else {
                ContextResolver::new()
            };

            collect_id_spans(&doc, &resolver, id_spans);

            let nodes = expand::extract_graph_nodes(&doc, &state.contexts)?;
            let source = path.display().to_string();

            collect_id_sources(&doc, &resolver, &source, id_source_files);
            file_sources.insert(source.clone(), contents);

            for node in &nodes {
                if let Some(id) = &node.id {
                    let span = id_spans.get(id).cloned().unwrap_or(0..0);
                    let entry = all_nodes
                        .entry(id.clone())
                        .or_insert_with(|| CollectedNode {
                            id: id.clone(),
                            types: Vec::new(),
                            properties: HashMap::new(),
                            source_file: source.clone(),
                            id_span: span,
                            resolver: resolver.clone(),
                        });
                    for t in &node.types {
                        if !entry.types.contains(t) {
                            entry.types.push(t.clone());
                        }
                    }
                    for (key, vals) in &node.properties {
                        entry
                            .properties
                            .entry(key.clone())
                            .or_default()
                            .extend(vals.clone());
                    }
                }
            }

            self.process_imports_collect(
                fs,
                &doc,
                &nodes,
                &resolver,
                state,
                all_nodes,
                visited,
                id_spans,
                id_source_files,
                file_sources,
            )
            .await?;

            Ok(())
        })
    }

    /// Follow `import` / `rdfs:seeAlso` IRIs and recursively collect nodes.
    fn process_imports_collect<'a>(
        &'a self,
        fs: &'a dyn Fs,
        doc: &'a JsonLdVal,
        nodes: &'a [ExpandedNode],
        resolver: &'a ContextResolver,
        state: &'a ModuleState,
        all_nodes: &'a mut HashMap<String, CollectedNode>,
        visited: &'a mut std::collections::HashSet<PathBuf>,
        id_spans: &'a mut HashMap<String, Range<usize>>,
        id_source_files: &'a mut HashMap<String, String>,
        file_sources: &'a mut HashMap<String, String>,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<()>> + 'a + Send>> {
        Box::pin(async move {
            let mut import_iris = Vec::new();

            if let Some(import_val) = doc.get("import") {
                collect_import_iris(import_val, resolver, &mut import_iris);
            }

            for node in nodes {
                if let Some(imports) = node.properties.get(IRI_RDFS_SEE_ALSO) {
                    for import_val in imports {
                        collect_import_iris(import_val, resolver, &mut import_iris);
                    }
                }
            }

            for iri in import_iris {
                if let Some(local_path) = resolve_iri_to_path(&iri, &state.import_paths) {
                    if cfs::exists(fs, &local_path).await {
                        self.collect_nodes_from_file(
                            fs,
                            &local_path,
                            state,
                            all_nodes,
                            visited,
                            id_spans,
                            id_source_files,
                            file_sources,
                        )
                        .await?;
                    }
                }
            }

            Ok(())
        })
    }

    fn process_merged_nodes(
        &mut self,
        all_nodes: &HashMap<String, CollectedNode>,
        id_spans: &HashMap<String, Range<usize>>,
        id_source_files: &HashMap<String, String>,
        _state: &ModuleState,
    ) -> Result<()> {
        for node in all_nodes.values() {
            if node.types.contains(&IRI_MODULE.to_string()) {
                self.register_module_from_merged(node, all_nodes, id_spans, id_source_files)?;
            }
        }
        Ok(())
    }

    fn register_module_from_merged(
        &mut self,
        node: &CollectedNode,
        _all_nodes: &HashMap<String, CollectedNode>,
        id_spans: &HashMap<String, Range<usize>>,
        id_source_files: &HashMap<String, String>,
    ) -> Result<()> {
        let require_name = node
            .properties
            .get(IRI_DOAP_NAME)
            .and_then(|v| v.first())
            .and_then(|v| v.as_str())
            .map(String::from);

        let mut components = Vec::new();

        if let Some(component_vals) = node.properties.get(IRI_COMPONENT) {
            for comp_val in component_vals {
                if let Some(comp) = self.parse_component(
                    comp_val,
                    &node.id,
                    &node.resolver,
                    id_spans,
                    id_source_files,
                    &node.source_file,
                ) {
                    self.components.insert(comp.iri.clone(), comp.clone());
                    components.push(comp);
                }
            }
        }

        let module = CjsModule {
            iri: node.id.clone(),
            require_name,
            components,
            source_file: node.source_file.clone(),
            iri_span: node.id_span.clone(),
        };

        self.modules.insert(node.id.clone(), module);
        Ok(())
    }

    /// Extract a `CjsComponent` from a raw property-value object.
    ///
    /// The `resolver` comes from the file that originally defined this component
    /// so that compact IRIs inside the object can be properly expanded. The
    /// `id_spans` map provides the source location of the `@id` value.
    fn parse_component(
        &self,
        value: &JsonLdVal,
        module_iri: &str,
        resolver: &ContextResolver,
        id_spans: &HashMap<String, Range<usize>>,
        id_source_files: &HashMap<String, String>,
        fallback_source_file: &str,
    ) -> Option<CjsComponent> {
        let id_str = value.get("@id")?.as_str()?;
        let iri = resolver.expand_term(id_str);
        let iri_span = id_spans.get(&iri).cloned().unwrap_or(0..0);
        // Use the file where this @id was first seen; fall back to the module file.
        let source_file = id_source_files
            .get(&iri)
            .cloned()
            .unwrap_or_else(|| fallback_source_file.to_string());

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

        let component_type = ComponentType::from_type_iris(&types).or_else(|| {
            for t in &types {
                match t.as_str() {
                    "Class" => return Some(ComponentType::Class),
                    "AbstractClass" => return Some(ComponentType::AbstractClass),
                    "Instance" => return Some(ComponentType::Instance),
                    _ => {}
                }
            }
            None
        })?;

        let require_element = value
            .get("requireElement")
            .or_else(|| value.get(IRI_COMPONENT_PATH))
            .and_then(|v| v.as_str())
            .map(String::from);

        let comment = value
            .get("comment")
            .or_else(|| value.get(IRI_RDFS_COMMENT))
            .and_then(|v| v.as_str())
            .map(String::from);

        let parameters =
            self.parse_parameters(value, resolver, id_spans, id_source_files, &source_file);

        let extends: Vec<String> = match value
            .get("extends")
            .or_else(|| value.get(IRI_RDFS_SUBCLASS_OF))
        {
            Some(JsonLdVal::Str(s)) => vec![resolver.expand_term(s)],
            Some(v) if v.as_array().is_some() => v
                .as_array()
                .unwrap()
                .iter()
                .filter_map(|(item, _)| match item {
                    JsonLdVal::Str(s) => Some(resolver.expand_term(s)),
                    _ => item.get("@id")?.as_str().map(|s| resolver.expand_term(s)),
                })
                .collect(),
            Some(v) => v
                .get("@id")
                .and_then(|v| v.as_str())
                .map(|s| resolver.expand_term(s))
                .into_iter()
                .collect(),
            None => vec![],
        };

        let constructor_arguments = value
            .get("constructorArguments")
            .or_else(|| value.get(IRI_CONSTRUCTOR_ARGUMENTS))
            .cloned();

        Some(CjsComponent {
            iri,
            component_type,
            require_element,
            comment,
            parameters,
            extends,
            constructor_arguments,
            module_iri: Some(module_iri.to_string()),
            source_file,
            iri_span,
        })
    }

    /// Extract `CjsParameter`s from a component value.
    fn parse_parameters(
        &self,
        value: &JsonLdVal,
        resolver: &ContextResolver,
        id_spans: &HashMap<String, Range<usize>>,
        id_source_files: &HashMap<String, String>,
        fallback_source_file: &str,
    ) -> Vec<CjsParameter> {
        let params = match value.get("parameters").or_else(|| value.get(IRI_PARAMETER)) {
            Some(v) => v,
            None => return vec![],
        };

        let arr = match params.as_array() {
            Some(a) => a,
            None => return vec![],
        };

        arr.iter()
            .filter_map(|(p, _)| {
                let id_str = p.get("@id")?.as_str()?;
                let iri = resolver.expand_term(id_str);
                let iri_span = id_spans.get(&iri).cloned().unwrap_or(0..0);

                let range =
                    p.get("range")
                        .or_else(|| p.get(IRI_RDFS_RANGE))
                        .and_then(|v| match v {
                            JsonLdVal::Str(s) => Some(resolver.expand_term(s)),
                            _ => v.get("@id")?.as_str().map(|s| resolver.expand_term(s)),
                        });
                let comment = p
                    .get("comment")
                    .or_else(|| p.get(IRI_RDFS_COMMENT))
                    .and_then(|v| v.as_str())
                    .map(String::from);
                let required = p.get("required").and_then(|v| v.as_bool()).unwrap_or(false);
                let lazy = p.get("lazy").and_then(|v| v.as_bool()).unwrap_or(false);
                let unique = p.get("unique").and_then(|v| v.as_bool()).unwrap_or(false);
                let default_value = p.get("default").cloned();
                let source_file = id_source_files
                    .get(&iri)
                    .cloned()
                    .unwrap_or_else(|| fallback_source_file.to_string());

                Some(CjsParameter {
                    iri,
                    range,
                    comment,
                    required,
                    lazy,
                    unique,
                    default_value,
                    source_file,
                    iri_span,
                })
            })
            .collect()
    }

    /// Resolve inheritance: walk each component's `extends` chain and merge in
    /// any parameters not already declared on the component itself.
    ///
    /// Must be called after all files have been loaded. Without this step,
    /// completion only shows parameters declared directly on a component and
    /// misses those inherited from abstract base classes.
    pub fn finalize(&mut self) {
        let component_iris: Vec<String> = self.components.keys().cloned().collect();
        for iri in component_iris {
            let inherited_params = self.collect_inherited_params(&iri, &mut Vec::new());
            if let Some(comp) = self.components.get_mut(&iri) {
                for param in inherited_params {
                    if !comp.parameters.iter().any(|p| p.iri == param.iri) {
                        comp.parameters.push(param);
                    }
                }
            }
        }

        // Build flat parameter index for O(1) goto-definition lookups.
        // Use first-seen (the defining component wins over inheritors).
        for comp in self.components.values() {
            for param in &comp.parameters {
                self.parameters
                    .entry(param.iri.clone())
                    .or_insert_with(|| (param.source_file.clone(), param.iri_span.clone()));
            }
        }
    }

    fn collect_inherited_params(&self, iri: &str, visited: &mut Vec<String>) -> Vec<CjsParameter> {
        if visited.contains(&iri.to_string()) {
            return vec![];
        }
        visited.push(iri.to_string());

        let Some(comp) = self.components.get(iri) else {
            return vec![];
        };

        let mut params = Vec::new();
        for parent_iri in &comp.extends.clone() {
            if let Some(parent) = self.components.get(parent_iri) {
                params.extend(parent.parameters.clone());
            }
            params.extend(self.collect_inherited_params(parent_iri, visited));
        }
        params
    }
}

fn collect_import_iris(value: &JsonLdVal, resolver: &ContextResolver, out: &mut Vec<String>) {
    match value {
        JsonLdVal::Str(s) => out.push(resolver.expand_term(s)),
        _ => {
            if let Some(arr) = value.as_array() {
                for (item, _) in arr {
                    if let Some(s) = item.as_str() {
                        out.push(resolver.expand_term(s));
                    }
                }
            }
        }
    }
}

/// Resolve an IRI to a local file path using the import_paths mapping.
pub fn resolve_iri_to_path(
    iri: &str,
    import_paths: &std::collections::HashMap<String, PathBuf>,
) -> Option<PathBuf> {
    for (prefix_iri, local_dir) in import_paths {
        if iri.starts_with(prefix_iri.as_str()) {
            let suffix = &iri[prefix_iri.len()..];
            return Some(local_dir.join(suffix));
        }
    }
    // Handle file:// URIs: strip scheme + optional host component so that
    // both "file:///home/..." and "file:////home/..." (4 slashes) produce a
    // valid absolute path "/home/...".

    if let Some(rest) = iri.strip_prefix("file://") {
        let clean = rest.trim_start_matches('/');
        return Some(PathBuf::from(format!("/{clean}")));
    }
    None
}
