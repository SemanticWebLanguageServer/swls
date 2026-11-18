//! The workspace's RDF files, loaded in the background.
//!
//! At startup ([`index_workspace`]) every RDF file in the workspace folders is
//! loaded as a [`Background`] document, so its classes, properties and shapes
//! are known without opening it. Files git ignores (`.gitignore`, excludes,
//! `.ignore`), hidden folders, `node_modules` and `target` are skipped, also
//! for files created later. File changes keep them current (see
//! [`crate::document`]). Disable with `"disabled": ["workspace_index"]`.

use bevy_ecs::prelude::*;

use crate::{
    lsp_types::Url,
    prelude::*,
    systems::{imports::unsupported_syntax, spawn_document},
};

/// Extensions of the files the workspace index loads. RDF/XML (`.rdf`) and
/// JSON-LD are left out; `.owl` files that turn out to be RDF/XML are skipped.
pub const WORKSPACE_EXTENSIONS: &[&str] = &[".ttl", ".nt", ".owl", ".trig", ".n3"];

/// At most this many files are loaded per workspace folder.
pub const MAX_WORKSPACE_FILES: usize = 500;

/// Larger files are skipped (generated data dumps, not vocabularies).
pub const MAX_WORKSPACE_FILE_BYTES: usize = 5 * 1024 * 1024;

/// Whether `url` is a file the workspace index loads.
pub fn is_workspace_file(url: &Url) -> bool {
    WORKSPACE_EXTENSIONS
        .iter()
        .any(|ext| url.path().ends_with(ext))
}

/// The workspace folder `url` is inside, if any.
pub fn workspace_root(config: &ServerConfig, url: &Url) -> Option<Url> {
    config
        .workspaces
        .iter()
        .find(|w| {
            let root = w.uri.as_str().trim_end_matches('/');
            url.as_str()
                .strip_prefix(root)
                .is_some_and(|rest| rest.starts_with('/'))
        })
        .map(|w| w.uri.clone())
}

/// Load a workspace file from disk as a background document (unless it is
/// too large or not in a syntax swls reads).
pub async fn load_workspace_file(fs: &Fs, url: Url, sender: &CommandSender) {
    let Some(content) = fs.0.read_file(&url).await else {
        return;
    };
    if content.len() > MAX_WORKSPACE_FILE_BYTES {
        tracing::info!("Not indexing {} ({} bytes)", url, content.len());
        return;
    }
    if let Some(syntax) = unsupported_syntax(&content) {
        tracing::info!("Not indexing {}: {}", url, syntax);
        return;
    }
    // The language follows the extension.
    spawn_document(url, content, None, &sender.0, |_, _| {});
}

/// Load the RDF files of every workspace folder in the background.
pub fn index_workspace<C: Client + Resource>(
    config: Res<ServerConfig>,
    fs: Res<Fs>,
    sender: Res<CommandSender>,
    client: Res<C>,
) {
    if config.config.local.is_disabled(Disabled::WorkspaceIndex) {
        return;
    }
    for folder in &config.workspaces {
        let root = folder.uri.clone();
        let fs = fs.clone();
        let sender = sender.clone();
        client.spawn(async move {
            let files =
                fs.0.walk(&root, WORKSPACE_EXTENSIONS, MAX_WORKSPACE_FILES)
                    .await;
            tracing::info!("Indexing {} files in {}", files.len(), root);
            for url in files {
                load_workspace_file(&fs, url, &sender).await;
            }
        });
    }
}
