use std::sync::Arc;

use bevy_ecs::{resource::Resource, world::World};
use futures::{channel::mpsc::unbounded, StreamExt as _};
use swls::{
    client::{BinFs, Job},
    timings, TowerClient,
};
use swls_core::{
    lsp_types::{MessageType, SemanticTokenType},
    prelude::*,
};
use tokio::{runtime::Builder, task::LocalSet};
use tower_lsp::{LspService, Server};
use tracing::{debug, level_filters::LevelFilter};
use tracing_subscriber::{layer::Context, registry::LookupSpan, Layer};

fn version() -> String {
    let base = option_env!("APP_VERSION").unwrap_or(env!("CARGO_PKG_VERSION"));
    let git = option_env!("GIT_HASH").unwrap_or("unknown");
    let branch = option_env!("GIT_BRANCH").unwrap_or("unknown");
    let date = option_env!("VERGEN_BUILD_DATE").unwrap_or("unknown");

    format!("Semantic Web Language Server version={base} (git={git} branch={branch} date={date})")
}

struct MsgVisitor(String);

impl tracing::field::Visit for MsgVisitor {
    fn record_debug(&mut self, field: &tracing::field::Field, value: &dyn std::fmt::Debug) {
        if field.name() == "message" {
            use std::fmt::Write;
            let _ = write!(self.0, "{value:?}");
        }
    }

    fn record_str(&mut self, field: &tracing::field::Field, value: &str) {
        if field.name() == "message" {
            self.0 = value.to_string();
        }
    }
}

struct LspLogLayer {
    sender: tokio::sync::mpsc::UnboundedSender<(MessageType, String)>,
}

impl<S: tracing::Subscriber + for<'a> LookupSpan<'a>> Layer<S> for LspLogLayer {
    fn on_event(&self, event: &tracing::Event<'_>, _ctx: Context<'_, S>) {
        let msg_type = match *event.metadata().level() {
            tracing::Level::ERROR => MessageType::ERROR,
            tracing::Level::WARN => MessageType::WARNING,
            tracing::Level::INFO => MessageType::INFO,
            _ => MessageType::LOG,
        };
        let mut visitor = MsgVisitor(String::new());
        event.record(&mut visitor);
        let target = event.metadata().target();
        let _ = self
            .sender
            .send((msg_type, format!("[{target}] {}", visitor.0)));
    }
}

fn setup_world<C: Client + ClientSync + Resource + Clone>(
    client: C,
) -> (CommandSender, Vec<SemanticTokenType>) {
    let mut world = World::new();

    setup_schedule_labels::<C>(&mut world);

    let (publisher, mut rx) = DiagnosticPublisher::new();
    world.insert_resource(publisher);

    let c = client.clone();
    tokio::spawn(async move {
        while let Some(x) = rx.next().await {
            c.publish_diagnostics(x.uri, x.diagnostics, x.version).await;
        }
    });

    swls_lang_turtle::setup_world::<C>(&mut world);
    swls_lang_sparql::setup_world(&mut world);
    swls_lang_trig::setup_world::<C>(&mut world);
    swls_lang_jsonld::setup_world::<C>(&mut world);

    let (tx, mut rx) = unbounded();
    let sender = CommandSender(tx);
    world.insert_resource(sender.clone());
    world.insert_resource(client.clone());
    world.insert_resource(Fs(Arc::new(BinFs::new())));

    let r = world.resource::<SemanticTokensDict>();
    let mut semantic_tokens: Vec<_> = (0..r.0.len()).map(|_| SemanticTokenType::KEYWORD).collect();
    r.0.iter()
        .for_each(|(k, v)| semantic_tokens[*v] = k.clone());

    tokio::task::spawn_local(async move {
        while let Some(mut x) = rx.next().await {
            world.commands().append(&mut x);
            world.flush();
        }
    });

    (sender, semantic_tokens)
}

fn setup_global_subscriber() -> tokio::sync::mpsc::UnboundedReceiver<(MessageType, String)> {
    use tracing_subscriber::{prelude::*, registry::Registry};

    let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
    let lsp_layer = LspLogLayer { sender: tx }.with_filter(LevelFilter::DEBUG);

    let subscriber = Registry::default()
        .with(timings::TracingLayer::new())
        .with(lsp_layer);

    tracing::subscriber::set_global_default(subscriber).expect("Could not set global default");
    rx
}

#[tokio::main]
async fn main() {
    if std::env::args().any(|arg| arg == "--version" || arg == "-V") {
        println!("{}", version());
        return;
    }
    std::panic::set_hook(Box::new(|panic_info| {
        let backtrace = std::backtrace::Backtrace::capture();
        tracing::error!("panic: {}\n{:#?}", panic_info, backtrace);
    }));

    let mut rx = setup_global_subscriber();

    debug!("Hello world!");
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (job_tx, mut job_rx) = unbounded::<Job>();

    std::thread::spawn(move || {
        let rt = Builder::new_current_thread().enable_all().build().unwrap();

        let local = LocalSet::new();

        rt.block_on(local.run_until(async move {
            while let Ok(job) = job_rx.recv().await {
                tokio::task::spawn_local(async move {
                    job().await;
                });
            }
        }));
    });

    let local = tokio::task::LocalSet::new();
    local
        .run_until(async {
            let (service, socket) = LspService::build(|client| {
                let log_client = client.clone();
                tokio::spawn(async move {
                    while let Some((ty, msg)) = rx.recv().await {
                        log_client.log_message(ty, msg).await;
                    }
                });

                let (sender, rt) = setup_world(TowerClient::new(client.clone(), job_tx));
                Backend::new(sender, client, rt)
            })
            .finish();

            Server::new(stdin, stdout, socket).serve(service).await;
        })
        .await;
    // let (service, socket) = LspService::build(|client| {
    //     let log_client = client.clone();
    //     tokio::spawn(async move {
    //         while let Some((ty, msg)) = rx.recv().await {
    //             log_client.log_message(ty, msg).await;
    //         }
    //     });
    //
    //     let (sender, rt) = setup_world(TowerClient::new(client.clone(), job_tx));
    //     Backend::new(sender, client, rt)
    // })
    // .finish();
    //
    // Server::new(stdin, stdout, socket).serve(service).await;
}
