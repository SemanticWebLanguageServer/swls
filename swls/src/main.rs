use std::{
    fs::File,
    io,
    sync::{Arc, Mutex},
};

use bevy_ecs::{system::Resource, world::World};
use futures::{channel::mpsc::unbounded, StreamExt as _};
use lsp_core::prelude::*;
use lsp_types::SemanticTokenType;
use swls::{client::BinFs, timings, TowerClient};
use tower_lsp::{LspService, Server};
use tracing::{info, level_filters::LevelFilter};

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

    lang_turtle::setup_world::<C>(&mut world);
    lang_jsonld::setup_world(&mut world);
    lang_sparql::setup_world(&mut world);

    let (tx, mut rx) = unbounded();
    let sender = CommandSender(tx);
    world.insert_resource(sender.clone());
    world.insert_resource(client.clone());
    world.insert_resource(Fs(Arc::new(BinFs::new())));

    let r = world.resource::<SemanticTokensDict>();
    let mut semantic_tokens: Vec<_> = (0..r.0.len()).map(|_| SemanticTokenType::KEYWORD).collect();
    r.0.iter()
        .for_each(|(k, v)| semantic_tokens[*v] = k.clone());

    tokio::spawn(async move {
        while let Some(mut x) = rx.next().await {
            world.commands().append(&mut x);
            world.flush_commands();
        }
    });

    (sender, semantic_tokens)
}

fn setup_global_subscriber() {
    use tracing_subscriber::{fmt, prelude::*, registry::Registry};

    let mut tmp = std::env::temp_dir();
    tmp.push("turtle-lsp.txt");
    let target: Box<dyn io::Write + Send + Sync + 'static> = match File::create(&tmp) {
        Ok(x) => Box::new(x),
        Err(_) => Box::new(std::io::stderr()),
    };

    let fmt_layer = fmt::Layer::default()
        .with_line_number(true)
        .with_writer(Mutex::new(target))
        .with_filter(LevelFilter::DEBUG);

    let subscriber = Registry::default()
        .with(timings::TracingLayer::new())
        .with(fmt_layer);

    tracing::subscriber::set_global_default(subscriber).expect("Could not set global default");
}

#[tokio::main]
async fn main() {
    std::panic::set_hook(Box::new(|_panic_info| {
        let backtrace = std::backtrace::Backtrace::capture();
        info!("My backtrace: {:#?}", backtrace);
    }));

    let _ = setup_global_subscriber();

    info!("Hello world!");
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| {
        let (sender, rt) = setup_world(TowerClient::new(client.clone()));

        Backend::new(sender, client, rt)
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
