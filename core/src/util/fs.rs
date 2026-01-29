use std::sync::Arc;

use bevy_ecs::prelude::Resource;
use derive_more::derive::AsRef;

#[derive(Resource, Clone, AsRef, Debug)]
pub struct Fs(pub Arc<dyn FsTrait>);

pub struct File {
    pub name: String,
    pub content: String,
}

#[tower_lsp::async_trait]
pub trait FsTrait: Send + Sync + 'static + std::fmt::Debug {
    fn virtual_url(&self, url: &str) -> Option<crate::lsp_types::Url>;
    fn lov_url(&self, url: &str, prefix: &str) -> Option<crate::lsp_types::Url> {
        if !url.starts_with("http") {
            return None;
        }

        // let prefix_url = crate::lsp_types::Url::parse(url).ok();
        // let prefix_origin = prefix_url.as_ref().map(|x| x.path()).unwrap_or("none");
        let url = self.virtual_url(&format!("{}.ttl", prefix))?;
        tracing::info!("lov url {} {} -> {}", url, prefix, url);
        Some(url)
    }
    async fn read_file(&self, url: &crate::lsp_types::Url) -> Option<String>;
    async fn glob_read(&self, url: &str) -> Option<Vec<File>>;
    async fn write_file(&self, url: &crate::lsp_types::Url, content: &str) -> Option<()>;
}
