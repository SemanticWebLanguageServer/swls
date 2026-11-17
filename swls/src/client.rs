use std::{collections::HashMap, fmt::Display, path::PathBuf, pin::Pin};

use bevy_ecs::resource::Resource;
use futures::channel::mpsc::UnboundedSender;
use futures::FutureExt;
use swls_core::prelude::FsDirEntry;
use swls_core::{
    client::{Client, ClientSync, Resp},
    lsp_types::{request::Request, Diagnostic, MessageType, Url},
    prelude::{File, FsTrait},
};
use tokio::fs::{self, read_to_string, write};
use tracing::debug;

pub mod reqwest {
    pub use reqwest::{
        header::{HeaderMap, HeaderName, HeaderValue},
        Error, StatusCode, Url,
    };
}
use std::future::Future;

pub(crate) type LocalFuture = Pin<Box<dyn Future<Output = ()> + 'static>>;
pub type Job = Box<dyn FnOnce() -> LocalFuture + Send>;

/// Native file system, with the ontology cache in the user's cache directory
/// (`~/.cache/swls` on Linux, `~/Library/Caches/swls` on macOS,
/// `%LOCALAPPDATA%\\swls` on Windows), falling back to `<tmp>/swls`.
///
/// The cache is shared by every server process of the user: files are written
/// atomically and marked read-only, so concurrent servers never see a partial
/// file and editors warn before a cached copy is edited.
#[derive(Debug)]
pub struct BinFs(PathBuf);
impl Default for BinFs {
    fn default() -> Self {
        Self::new()
    }
}

impl BinFs {
    pub fn new() -> Self {
        let root = dirs::cache_dir()
            .unwrap_or_else(std::env::temp_dir)
            .join("swls");
        Self(root)
    }
}

/// Write `content` to `path` atomically and leave it read-only.
///
/// The content goes to a uniquely named sibling first, which is then renamed
/// over `path`, so readers (and other servers) only ever see a complete file.
async fn write_read_only(path: &std::path::Path, content: &str) -> std::io::Result<()> {
    let parent = path.parent().unwrap_or(std::path::Path::new("."));
    fs::create_dir_all(parent).await?;

    let file_name = path.file_name().unwrap_or_default().to_string_lossy();
    let tmp = parent.join(format!(".{}.{}.tmp", file_name, std::process::id()));
    write(&tmp, content.as_bytes()).await?;

    let mut perms = fs::metadata(&tmp).await?.permissions();
    perms.set_readonly(true);
    fs::set_permissions(&tmp, perms).await?;

    // Windows refuses to replace a read-only file.
    #[cfg(windows)]
    if let Ok(meta) = fs::metadata(path).await {
        let mut perms = meta.permissions();
        perms.set_readonly(false);
        let _ = fs::set_permissions(path, perms).await;
    }

    if let Err(e) = fs::rename(&tmp, path).await {
        let _ = fs::remove_file(&tmp).await;
        return Err(e);
    }
    Ok(())
}

#[tower_lsp::async_trait]
impl FsTrait for BinFs {
    fn virtual_url(&self, path: &str) -> Option<swls_core::lsp_types::Url> {
        swls_core::lsp_types::Url::from_file_path(self.0.join(path)).ok()
    }

    fn cache_root(&self) -> Option<Url> {
        Url::from_directory_path(&self.0).ok()
    }

    async fn write_cache(&self, url: &Url, content: &str) -> Option<()> {
        let fp = url.to_file_path().ok()?;
        match write_read_only(&fp, content).await {
            Ok(()) => Some(()),
            Err(e) => {
                tracing::warn!("Failed to write cache file {}: {}", fp.display(), e);
                None
            }
        }
    }

    async fn cache_age(&self, url: &Url) -> Option<std::time::Duration> {
        let fp = url.to_file_path().ok()?;
        let modified = fs::metadata(fp).await.ok()?.modified().ok()?;
        Some(modified.elapsed().unwrap_or_default())
    }

    async fn read_file(&self, url: &swls_core::lsp_types::Url) -> Option<String> {
        let fp = url.to_file_path().ok()?;
        let content = read_to_string(fp).await.ok()?;
        Some(content)
    }

    async fn glob_read(&self, url: &str) -> Option<Vec<File>> {
        let mut files = Vec::new();
        for entry in glob::glob(url).ok()? {
            match entry {
                Ok(path) => {
                    let name = path.display().to_string();
                    let content = read_to_string(path).await.ok()?;
                    files.push(File { name, content })
                }
                Err(e) => tracing::error!("{:?}", e),
            }
        }
        Some(files)
    }

    async fn write_file(&self, url: &swls_core::lsp_types::Url, content: &str) -> Option<()> {
        let fp = url.to_file_path().ok()?;
        if let Some(parent) = fp.parent() {
            fs::create_dir_all(parent).await.ok()?;
        }
        write(fp, content.as_bytes()).await.ok()
    }

    async fn read_dir(&self, path: &Url) -> Option<Vec<FsDirEntry>> {
        let mut entries = Vec::new();
        let path = path.to_file_path().ok()?;
        let mut rd = tokio::fs::read_dir(path).await.ok()?;
        while let Some(entry) = rd.next_entry().await.ok()? {
            let metadata = entry.metadata().await.ok()?;

            let path = if metadata.is_file() {
                Url::from_file_path(entry.path())
            } else {
                Url::from_directory_path(entry.path())
            }
            .ok()?;
            entries.push(FsDirEntry {
                name: entry.file_name().to_string_lossy().into_owned(),
                path,
                is_dir: metadata.is_dir(),
            });
        }
        Some(entries)
    }
    async fn is_file(&self, path: &Url) -> bool {
        if let Ok(p) = path.to_file_path() {
            p.is_file()
        } else {
            !path.as_str().ends_with('/')
        }
    }
    async fn is_dir(&self, path: &Url) -> bool {
        if let Ok(p) = path.to_file_path() {
            p.is_dir()
        } else {
            !path.as_str().ends_with('/')
        }
    }

    async fn glob(&self, base: &Url, pattern: &str) -> Option<Vec<FsDirEntry>> {
        let base_path = base.to_file_path().ok()?;
        let full_pattern = base_path.join(pattern).to_string_lossy().into_owned();

        let mut entries = Vec::new();
        let paths = glob::glob(&full_pattern).ok()?;
        for path in paths {
            let path = path.ok()?;
            let is_dir = path.is_dir();
            let name = path
                .file_name()
                .map(|n| n.to_string_lossy().into_owned())
                .unwrap_or_default();

            let entry_url = if is_dir {
                Url::from_directory_path(&path).ok()?
            } else {
                Url::from_file_path(&path).ok()?
            };
            entries.push(FsDirEntry {
                name,
                path: entry_url,
                is_dir,
            });
        }

        Some(entries)
    }
}

#[derive(Resource, Clone)]
pub struct TowerClient {
    client: tower_lsp::Client,
    sender: UnboundedSender<Job>,
    handle: tokio::runtime::Handle,
}
impl TowerClient {
    pub fn new(client: tower_lsp::Client, sender: UnboundedSender<Job>) -> Self {
        Self {
            client,
            sender,
            handle: tokio::runtime::Handle::current(),
        }
    }
}

impl ClientSync for TowerClient {
    fn spawn<F: std::future::Future<Output = ()> + Send + 'static>(&self, fut: F) {
        self.handle.spawn(fut);
    }

    fn fetch(
        &self,
        url: &str,
        headers: &HashMap<String, String>,
    ) -> Pin<Box<dyn Send + std::future::Future<Output = Result<Resp, String>>>> {
        use tokio::{fs::File, io::AsyncReadExt};
        use tracing::error;
        debug!("fetching resource {}", url);

        let m_url = reqwest::Url::parse(url);

        let client = ::reqwest::Client::new();
        let builder = client.get(url);
        let builder = headers
            .iter()
            .fold(builder, |builder, (k, v)| builder.header(k, v));

        async {
            let url = m_url.map_err(|_| String::from("invalid url!"))?;
            if url.scheme() == "file" {
                let mut file = File::open(url.path())
                    .await
                    .map_err(|_| format!("File not found {}", url.path()))?;
                let mut body = String::new();
                file.read_to_string(&mut body)
                    .await
                    .map_err(|_| "Failed to read file".to_string())?;
                let status = 200;
                let headers = Vec::new();
                return Ok(Resp {
                    headers,
                    body,
                    status,
                });
            }

            let resp = match builder.send().await {
                Ok(x) => x,
                Err(e) => {
                    error!(error = ?e);
                    return Err(e.to_string());
                }
            };

            let status = resp.status().as_u16();
            let headers: Vec<_> = resp
                .headers()
                .iter()
                .flat_map(|(h, v)| {
                    if let Ok(value) = v.to_str() {
                        Some((h.to_string(), value.to_string()))
                    } else {
                        None
                    }
                })
                .collect();
            let body = resp.text().await.unwrap();

            Ok(Resp {
                headers,
                body,
                status,
            })
        }
        .boxed()
    }

    fn spawn_local<F, Fut>(&self, f: F)
    where
        F: FnOnce() -> Fut + Send + 'static,
        Fut: std::future::Future<Output = ()> + 'static,
    {
        let _ = self.sender.unbounded_send(Box::new(move || {
            Box::pin(async move {
                f().await;
            })
        }));
    }
}

#[tower_lsp::async_trait]
impl Client for TowerClient {
    async fn log_message<M: Display + Sync + Send + 'static>(&self, ty: MessageType, msg: M) -> () {
        self.client.log_message(ty, msg).await;
    }

    async fn send_request<R: Request>(&self, params: R::Params) -> Option<R::Result>
    where
        R::Params: Sync + Send + 'static,
        R::Result: Sync + Send + 'static,
    {
        self.client.send_request::<R>(params).await.ok()
    }

    async fn publish_diagnostics(
        &self,
        uri: Url,
        diags: Vec<Diagnostic>,
        version: Option<i32>,
    ) -> () {
        self.client.publish_diagnostics(uri, diags, version).await;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fs_in(dir: &std::path::Path) -> BinFs {
        BinFs(dir.to_path_buf())
    }

    #[tokio::test]
    async fn cache_files_are_written_read_only_and_can_be_replaced() {
        let dir = std::env::temp_dir().join(format!("swls-binfs-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let fs = fs_in(&dir);
        let url = fs.virtual_url("lov/foaf.ttl").unwrap();
        let path = url.to_file_path().unwrap();

        fs.write_cache(&url, "v1").await.unwrap();
        assert!(std::fs::metadata(&path).unwrap().permissions().readonly());
        assert!(fs.cache_age(&url).await.unwrap() < std::time::Duration::from_secs(60));

        // The server can still refresh its own read-only file.
        fs.write_cache(&url, "v2").await.unwrap();
        assert_eq!(std::fs::read_to_string(&path).unwrap(), "v2");
        assert!(std::fs::metadata(&path).unwrap().permissions().readonly());

        // No temporary files are left behind.
        let names: Vec<_> = std::fs::read_dir(path.parent().unwrap())
            .unwrap()
            .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
            .collect();
        assert_eq!(names, vec!["foaf.ttl"]);

        assert!(fs.is_cache_url(&url));
        assert!(!fs.is_cache_url(&Url::parse("file:///elsewhere/foaf.ttl").unwrap()));

        let mut perms = std::fs::metadata(&path).unwrap().permissions();
        #[allow(clippy::permissions_set_readonly_false)]
        perms.set_readonly(false);
        let _ = std::fs::set_permissions(&path, perms);
        let _ = std::fs::remove_dir_all(&dir);
    }
}
