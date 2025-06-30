use std::{collections::HashMap, fmt::Display, path::PathBuf, pin::Pin};

use bevy_ecs::system::Resource;
use futures::FutureExt;
use lsp_core::{
    client::{Client, ClientSync, Resp},
    prelude::{File, FsTrait},
};
use lsp_types::{Diagnostic, MessageType, Url};
use tokio::fs::{self, read_to_string, write};
use tracing::info;

pub mod reqwest {
    pub use reqwest::{
        header::{HeaderMap, HeaderName, HeaderValue},
        Error, StatusCode, Url,
    };
}

// TODO: use other tmp file location
#[derive(Debug)]
pub struct BinFs(PathBuf);
impl BinFs {
    pub fn new() -> Self {
        let mut tmp_dir = std::env::temp_dir();
        tmp_dir.push("swls");
        let mut c = 0;
        tmp_dir.push(c.to_string());
        while std::fs::exists(&tmp_dir).unwrap_or(false) {
            tmp_dir.pop();
            c += 1;
            tmp_dir.push(c.to_string());
        }
        Self(tmp_dir)
    }
}

#[tower_lsp::async_trait]
impl FsTrait for BinFs {
    fn virtual_url(&self, url: &str) -> Option<lsp_types::Url> {
        let mut pb = self.0.clone();
        if let Ok(url) = lsp_types::Url::parse(url) {
            pb.push(url.path());
        } else {
            pb.push(url);
        }
        lsp_types::Url::from_file_path(pb).ok()
    }

    async fn read_file(&self, url: &lsp_types::Url) -> Option<String> {
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

    async fn write_file(&self, url: &lsp_types::Url, content: &str) -> Option<()> {
        let fp = url.to_file_path().ok()?;
        if let Some(parent) = fp.parent() {
            fs::create_dir_all(parent).await.ok()?;
        }
        write(fp, content.as_bytes()).await.ok()
    }
}

#[derive(Resource, Clone)]
pub struct TowerClient {
    client: tower_lsp::Client,
    handle: tokio::runtime::Handle,
}
impl TowerClient {
    pub fn new(client: tower_lsp::Client) -> Self {
        Self {
            client,
            handle: tokio::runtime::Handle::current(),
        }
    }
}

impl ClientSync for TowerClient {
    fn spawn<F: std::future::Future<Output = ()> + Send + 'static>(&self, fut: F) {
        let handle = std::thread::current();
        info!("Spawn threaad name {:?}", handle.id());
        self.handle.spawn(fut);
        // info!("Should spawn but won't!");
    }

    fn fetch(
        &self,
        url: &str,
        headers: &HashMap<String, String>,
    ) -> Pin<Box<dyn Send + std::future::Future<Output = Result<Resp, String>>>> {
        use tokio::{fs::File, io::AsyncReadExt};
        use tracing::{debug, error, info};
        info!("Should fetch, fetching!");

        let m_url = reqwest::Url::parse(url);

        let client = ::reqwest::Client::new();
        let builder = client.get(url);
        let builder = headers
            .into_iter()
            .fold(builder, |builder, (k, v)| builder.header(k, v));

        return async {
            let url = m_url.map_err(|_| String::from("invalid url!"))?;
            info!("Found url {} {}", url.scheme(), url);
            if url.scheme() == "file" {
                let mut file = File::open(url.path())
                    .await
                    .map_err(|_| format!("File not found {}", url.path()))?;
                let mut body = String::new();
                file.read_to_string(&mut body)
                    .await
                    .map_err(|_| format!("Failed to read file"))?;
                let status = 200;
                let headers = Vec::new();
                return Ok(Resp {
                    headers,
                    body,
                    status,
                });
            }

            debug!("sending blocking");
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
            debug!("got resp");
            let body = resp.text().await.unwrap();

            Ok(Resp {
                headers,
                body,
                status,
            })
        }
        .boxed();
    }
}

#[tower_lsp::async_trait]
impl Client for TowerClient {
    async fn log_message<M: Display + Sync + Send + 'static>(&self, ty: MessageType, msg: M) -> () {
        self.client.log_message(ty, msg).await;
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
