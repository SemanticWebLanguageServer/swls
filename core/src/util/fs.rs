use bevy_ecs::prelude::Resource;
use derive_more::derive::AsRef;
use std::sync::Arc;
use crate::lsp_types::Url;

#[derive(Resource, Clone, AsRef, Debug)]
pub struct Fs(pub Arc<dyn FsTrait + Sync + Send>);

#[derive(Debug)]
pub struct File {
    pub name: String,
    pub content: String,
}

#[derive(Debug, Clone)]
pub struct FsDirEntry {
    pub name: String,
    pub path: Url,
    pub is_dir: bool,
}

/// Platform file system used by the server.
///
/// Besides plain file access it owns the **ontology cache**: copies of remote
/// ontologies and shapes that the server loads in the background. Cached files
/// live under [`cache_root`](Self::cache_root), at human-readable locations so
/// goto-definition shows `…/lov/foaf.ttl` instead of an opaque URL:
///
/// - `lov/<prefix>.ttl` for an ontology resolved by prefix ([`lov_url`](Self::lov_url)),
/// - `sources/<host>/<path>` for an ontology or shape at a known URL
///   ([`cache_url`](Self::cache_url)).
///
/// The cache is shared by all server processes of a user and reused across
/// restarts (see [`crate::util::cache`]). Files in it are snapshots managed by
/// the server; editing them has no effect on the server.
#[async_trait::async_trait]
pub trait FsTrait: Send + Sync + 'static + std::fmt::Debug {
    /// Location of `path`, relative to the cache root.
    fn virtual_url(&self, path: &str) -> Option<crate::lsp_types::Url>;

    /// Root of the ontology cache, `None` when there is no on-disk cache.
    fn cache_root(&self) -> Option<Url> {
        None
    }

    /// Whether `url` is a file inside the ontology cache.
    fn is_cache_url(&self, url: &Url) -> bool {
        self.cache_root()
            .is_some_and(|root| url.as_str().starts_with(root.as_str()))
    }

    /// Cache location of the ontology for namespace `url`, written as `prefix:`.
    fn lov_url(&self, url: &str, prefix: &str) -> Option<crate::lsp_types::Url> {
        if !url.starts_with("http") {
            return None;
        }
        self.virtual_url(&format!("lov/{}.ttl", prefix))
    }

    /// Cache location of the remote document at `source`:
    /// `<cache>/sources/<host>/<path>`.
    ///
    /// Unlike [`lov_url`](Self::lov_url), which is keyed by prefix name, this is
    /// keyed by the source itself, so distinct documents that describe the same
    /// namespace never share a location (and therefore never share an entity).
    fn cache_url(&self, source: &Url) -> Option<Url> {
        let host = source.host_str()?;
        let path = source.path().trim_start_matches('/');
        let mut relative = format!("sources/{}/{}", host, path);
        let last = relative.rsplit('/').next().unwrap_or_default();
        if last.is_empty() {
            relative.push_str("index.ttl");
        } else if !last.contains('.') {
            relative.push_str(".ttl");
        }
        self.virtual_url(&relative)
    }

    /// Write a file into the cache.
    ///
    /// Implementations should write atomically (concurrent servers share the
    /// cache) and mark the file read-only, so editors warn before it is edited.
    async fn write_cache(&self, url: &Url, content: &str) -> Option<()> {
        self.write_file(url, content).await
    }

    /// Time since the cached file at `url` was written, `None` if it is absent.
    async fn cache_age(&self, _url: &Url) -> Option<std::time::Duration> {
        None
    }

    async fn read_file(&self, url: &crate::lsp_types::Url) -> Option<String>;
    async fn glob_read(&self, url: &str) -> Option<Vec<File>>;
    async fn write_file(&self, url: &crate::lsp_types::Url, content: &str) -> Option<()>;

    async fn read_dir(&self, path: &crate::lsp_types::Url) -> Option<Vec<FsDirEntry>>;
    async fn is_file(&self, path: &crate::lsp_types::Url) -> bool;
    async fn is_dir(&self, path: &crate::lsp_types::Url) -> bool;

    async fn glob(&self, base: &Url, pattern: &str) -> Option<Vec<FsDirEntry>>;
}
