#![doc(
    html_logo_url = "https://ajuvercr.github.io/semantic-web-lsp/assets/icons/favicon.png",
    html_favicon_url = "https://ajuvercr.github.io/semantic-web-lsp/assets/icons/favicon.ico"
)]
use std::{
    collections::HashMap,
    fmt::Display,
    future::Future,
    pin::Pin,
    str::FromStr as _,
    sync::{
        atomic::{AtomicU32, Ordering},
        Arc,
    },
    task::{Context, Poll},
    time::Duration,
};

use bevy_ecs::prelude::*;
use futures::{
    channel::mpsc::{unbounded, UnboundedReceiver},
    lock::Mutex,
    FutureExt as _,
};
use lsp_core::{
    client::{Client, ClientSync, Resp},
    components::*,
    prelude::diagnostics::{DiagnosticItem, DiagnosticPublisher},
    setup_schedule_labels,
    systems::{handle_tasks, spawn_or_insert},
};
use lsp_types::{Diagnostic, MessageType, TextDocumentItem, Url};

#[derive(Resource, Debug, Clone)]
pub struct TestClient {
    logs: Arc<Mutex<Vec<(MessageType, String)>>>,
    diagnostics: Arc<Mutex<Vec<(Url, Vec<lsp_types::Diagnostic>)>>>,
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

#[tower_lsp::async_trait]
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

    fn fetch(
        &self,
        url: &str,
        _headers: &std::collections::HashMap<String, String>,
    ) -> std::pin::Pin<
        Box<dyn Send + std::future::Future<Output = Result<lsp_core::client::Resp, String>>>,
    > {
        let body = self.locations.get(url).cloned();
        Sendable(async move {
            let mut headers = Vec::new();
            async_std::task::sleep(Duration::from_millis(200)).await;
            headers.push(("Content-Type".to_string(), "text/turtle".to_string()));
            let status = body.is_some().then_some(200).unwrap_or(404);
            Ok(Resp {
                headers,
                body: body.unwrap_or_default(),
                status,
            })
        })
        .boxed()
    }
}

pub fn setup_world(
    client: TestClient,
    f: impl FnOnce(&mut World) -> (),
) -> (World, UnboundedReceiver<DiagnosticItem>) {
    let mut world = World::new();
    setup_schedule_labels::<TestClient>(&mut world);

    let (tx, rx) = unbounded();
    world.insert_resource(CommandSender(tx));
    world.insert_resource(CommandReceiver(rx));
    world.insert_resource(client);

    world.schedule_scope(lsp_core::Tasks, |_, schedule| {
        schedule.add_systems(handle_tasks);
    });

    f(&mut world);

    let (publisher, rx) = DiagnosticPublisher::new();
    world.insert_resource(publisher);

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

    spawn_or_insert(
        url.clone(),
        (
            Source(content.to_string()),
            RopeC(ropey::Rope::from_str(content)),
            Label(url), // this might crash
            Wrapped(item),
            Types(HashMap::new()),
        ),
        Some(lang.into()),
        bundle,
    )(world)
}

pub fn debug_world(world: &mut World) {
    for e in world.query::<Entity>().iter(&world) {
        let e = world.entity(e);
        if let Some(l) = e.get::<Label>() {
            println!("-- Entity {} -- ", l.as_str());
        } else {
            println!("-- Nameless entity --");
        }
        for c in world.components().iter() {
            if e.contains_id(c.id()) {
                println!("c {}", c.name(),);
            }
        }
    }
}
