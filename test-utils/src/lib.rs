#![doc(
    html_logo_url = "https://ajuvercr.github.io/semantic-web-lsp/assets/icons/favicon.png",
    html_favicon_url = "https://ajuvercr.github.io/semantic-web-lsp/assets/icons/favicon.ico"
)]
use std::{
    collections::HashMap,
    fmt::Display,
    fs::{exists, remove_dir_all},
    future::Future,
    path::PathBuf,
    pin::Pin,
    str::FromStr as _,
    sync::{
        atomic::{AtomicU32, Ordering},
        Arc,
    },
    task::{Context, Poll},
    time::Duration,
};

use async_std::fs::{self, read_to_string};
use bevy_ecs::prelude::*;
use futures::{
    channel::mpsc::{unbounded, UnboundedReceiver},
    lock::Mutex,
    FutureExt as _,
};
use swls_core::{
    client::{Client, ClientSync, Resp},
    components::*,
    lsp_types::{Diagnostic, MessageType, TextDocumentItem, Url},
    prelude::{
        diagnostics::{DiagnosticItem, DiagnosticPublisher},
        File, Fs, FsDirEntry, FsTrait,
    },
    setup_schedule_labels,
    systems::{handle_tasks, spawn_or_insert},
    text::LineIndex,
    Startup,
};

#[derive(Resource, Debug, Clone)]
pub struct TestClient {
    logs: Arc<Mutex<Vec<(MessageType, String)>>>,
    diagnostics: Arc<Mutex<Vec<(Url, Vec<swls_core::lsp_types::Diagnostic>)>>>,
    locations: HashMap<String, String>,
    tasks_running: Arc<std::sync::atomic::AtomicU32>,
    executor: Arc<async_executor::Executor<'static>>,
}

impl TestClient {
    pub fn new() -> Self {
        // let pool = LocalPool::new();
        Self {
            logs: Default::default(),
            diagnostics: Default::default(),
            locations: Default::default(),
            tasks_running: Arc::new(AtomicU32::new(0)),
            executor: Arc::new(async_executor::Executor::new()),
        }
    }
}
impl Default for TestClient {
    fn default() -> Self {
        Self::new()
    }
}

impl TestClient {
    pub fn add_res(&mut self, loc: &str, cont: &str) {
        self.locations.insert(loc.to_string(), cont.to_string());
    }
}

impl TestClient {
    pub async fn await_futures<F: FnMut()>(&self, mut tick: F) {
        tick();
        while self.tasks_running.load(Ordering::Relaxed) != 0 {
            self.executor.tick().await;
            tick();
        }
    }
}

use swls_core::lsp_types::request::Request;
#[async_trait::async_trait]
impl Client for TestClient {
    async fn log_message<M: Display + Sync + Send + 'static>(&self, ty: MessageType, msg: M) -> () {
        let mut lock = self.logs.lock().await;
        lock.push((ty, msg.to_string()));
    }

    async fn publish_diagnostics(
        &self,
        uri: Url,
        diags: Vec<Diagnostic>,
        _version: Option<i32>,
    ) -> () {
        let mut lock = self.diagnostics.lock().await;
        lock.push((uri, diags));
    }

    async fn send_request<R: Request + Sync + Send + 'static>(
        &self,
        _params: R::Params,
    ) -> Option<R::Result>
    where
        R::Params: Sync + Send + 'static,
        R::Result: Sync + Send + 'static,
    {
        None
    }
}

struct Sendable<T>(pub T);

// Safety: WebAssembly will only ever run in a single-threaded context.
unsafe impl<T> Send for Sendable<T> {}
impl<O, T> Future for Sendable<T>
where
    T: Future<Output = O>,
{
    type Output = O;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // Safely access the inner future
        let inner = unsafe { self.map_unchecked_mut(|s| &mut s.0) };
        inner.poll(cx)
    }
}

impl ClientSync for TestClient {
    fn spawn<F: std::future::Future<Output = ()> + 'static>(&self, fut: F) {
        self.tasks_running.fetch_add(1, Ordering::AcqRel);
        let tr = self.tasks_running.clone();
        let fut = async move {
            fut.await;
            tr.fetch_sub(1, Ordering::AcqRel);
        };
        self.executor.spawn(Sendable(fut)).detach();
    }

    fn spawn_local<F, Fut>(&self, f: F)
    where
        F: FnOnce() -> Fut + Send + 'static,
        Fut: std::future::Future<Output = ()> + 'static,
    {
        self.tasks_running.fetch_add(1, Ordering::AcqRel);
        let tr = self.tasks_running.clone();
        let fut = async move {
            f().await;
            tr.fetch_sub(1, Ordering::AcqRel);
        };
        self.executor.spawn(Sendable(fut)).detach();
    }

    fn fetch(
        &self,
        url: &str,
        _headers: &std::collections::HashMap<String, String>,
    ) -> std::pin::Pin<
        Box<dyn Send + std::future::Future<Output = Result<swls_core::client::Resp, String>>>,
    > {
        let body = self.locations.get(url).cloned();
        Sendable(async move {
            let mut headers = Vec::new();
            async_std::task::sleep(Duration::from_millis(200)).await;
            headers.push(("Content-Type".to_string(), "text/turtle".to_string()));
            let status = if body.is_some() { 200 } else { 404 };
            Ok(Resp {
                headers,
                body: body.unwrap_or_default(),
                status,
            })
        })
        .boxed()
    }
}

#[derive(Debug)]
pub struct TestFs(PathBuf);
impl TestFs {
    /// A fresh, empty cache directory for this instance.
    ///
    /// Every test gets its own directory so tests running in parallel (threads
    /// or processes) never see or delete each other's cached files.
    pub fn new() -> Self {
        static NEXT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let mut tmp_dir = std::env::temp_dir();
        tmp_dir.push("swls-test");
        tmp_dir.push(format!("{}-{}", std::process::id(), n));

        if exists(&tmp_dir).unwrap_or(false) {
            let _ = remove_dir_all(&tmp_dir);
        }

        Self(tmp_dir)
    }
    pub async fn empty(&self) {}
}

impl Default for TestFs {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for TestFs {
    fn drop(&mut self) {
        let _ = remove_dir_all(&self.0);
    }
}

#[async_trait::async_trait]
impl FsTrait for TestFs {
    fn virtual_url(&self, path: &str) -> Option<swls_core::lsp_types::Url> {
        swls_core::lsp_types::Url::from_file_path(self.0.join(path)).ok()
    }

    fn cache_root(&self) -> Option<Url> {
        Url::from_directory_path(&self.0).ok()
    }

    async fn walk(&self, root: &Url, extensions: &[&str], max: usize) -> Vec<Url> {
        let Ok(root) = root.to_file_path() else {
            return Vec::new();
        };
        swls_core::util::fs::walk_dir(&root, extensions, max)
            .into_iter()
            .flat_map(Url::from_file_path)
            .collect()
    }

    async fn cache_age(&self, url: &Url) -> Option<std::time::Duration> {
        let modified = std::fs::metadata(url.to_file_path().ok()?)
            .ok()?
            .modified()
            .ok()?;
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
        fs::write(fp, content.as_bytes()).await.ok()
    }

    async fn read_dir(&self, _path: &swls_core::lsp_types::Url) -> Option<Vec<FsDirEntry>> {
        None
    }
    async fn is_file(&self, _path: &swls_core::lsp_types::Url) -> bool {
        true
    }
    async fn is_dir(&self, _path: &swls_core::lsp_types::Url) -> bool {
        false
    }

    async fn glob(&self, _base: &Url, _pattern: &str) -> Option<Vec<FsDirEntry>> {
        None
    }
}

pub fn setup_world(
    client: TestClient,
    f: impl FnOnce(&mut World),
) -> (World, UnboundedReceiver<DiagnosticItem>) {
    let mut world = World::new();
    setup_schedule_labels::<TestClient>(&mut world);

    let (tx, rx) = unbounded();
    world.insert_resource(CommandSender(tx));
    world.insert_resource(CommandReceiver(rx));
    world.insert_resource(client);
    world.insert_resource(Fs(Arc::new(TestFs::new())));

    world.schedule_scope(swls_core::Tasks, |_, schedule| {
        schedule.add_systems(handle_tasks);
    });

    f(&mut world);

    let (publisher, rx) = DiagnosticPublisher::new();
    world.insert_resource(publisher);
    world.insert_resource(ServerConfig::default());

    world.run_schedule(Startup);

    (world, rx)
}

pub fn create_file(
    world: &mut World,
    content: &str,
    url: &str,
    lang: &str,
    bundle: impl Bundle,
) -> Entity {
    let url = Url::from_str(url).unwrap();
    let item = TextDocumentItem {
        version: 1,
        uri: url.clone(),
        language_id: String::from(lang),
        text: String::new(),
    };

    let entity = spawn_or_insert(
        url.clone(),
        (
            Source(content.to_string()),
            RopeC(LineIndex::new(content)),
            Label(url),
            Wrapped(item),
            Types(HashMap::new()),
        ),
        Some(lang.into()),
        bundle,
    )(world);
    world.run_schedule(swls_core::feature::parse::Label);
    entity
}

pub fn debug_world(world: &mut World) {
    for e in world.query::<Entity>().iter(world) {
        let e = world.entity(e);
        if let Some(l) = e.get::<Label>() {
            println!("-- Entity {} -- ", l.as_str());
        } else {
            println!("-- Nameless entity --");
        }
        for c in world.components().iter_registered() {
            if e.contains_id(c.id()) {
                println!("c {}", c.name(),);
            }
        }
    }
}
