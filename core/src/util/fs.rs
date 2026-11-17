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

#[async_trait::async_trait]
pub trait FsTrait: Send + Sync + 'static + std::fmt::Debug {
    fn virtual_url(&self, url: &str) -> Option<crate::lsp_types::Url>;
    fn lov_url(&self, url: &str, prefix: &str) -> Option<crate::lsp_types::Url> {
        if !url.starts_with("http") {
            return None;
        }

        // let prefix_url = crate::lsp_types::Url::parse(url).ok();
        // let prefix_origin = prefix_url.as_ref().map(|x| x.path()).unwrap_or("none");
        let url = self.virtual_url(&format!("{}.ttl", prefix))?;
        tracing::debug!("lov url {} {} -> {}", url, prefix, url);
        Some(url)
    }
    /// Local, human-readable location for a cached copy of the remote document at
    /// `source`: `<cache>/<host>/<path>`.
    ///
    /// Unlike [`lov_url`](Self::lov_url), which is keyed by prefix name, this is
    /// keyed by the source itself, so distinct documents that describe the same
    /// namespace never share a location (and therefore never share an entity).
    fn cache_url(&self, source: &Url) -> Option<Url> {
        let host = source.host_str()?;
        let path = source.path().trim_start_matches('/');
        let mut relative = format!("{}/{}", host, path);
        let last = relative.rsplit('/').next().unwrap_or_default();
        if last.is_empty() {
            relative.push_str("index.ttl");
        } else if !last.contains('.') {
            relative.push_str(".ttl");
        }
        self.virtual_url(&relative)
    }
    async fn read_file(&self, url: &crate::lsp_types::Url) -> Option<String>;
    async fn glob_read(&self, url: &str) -> Option<Vec<File>>;
    async fn write_file(&self, url: &crate::lsp_types::Url, content: &str) -> Option<()>;

    async fn read_dir(&self, path: &crate::lsp_types::Url) -> Option<Vec<FsDirEntry>>;
    async fn is_file(&self, path: &crate::lsp_types::Url) -> bool;
    async fn is_dir(&self, path: &crate::lsp_types::Url) -> bool;

    async fn glob(&self, base: &Url, pattern: &str) -> Option<Vec<FsDirEntry>>;
}
