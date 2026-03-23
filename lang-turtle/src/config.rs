use std::{borrow::Cow, collections::HashMap};

use bevy_ecs::{resource::Resource, system::Res, world::{CommandQueue, World}};
use swls_core::{
    lsp_types::TextDocumentItem,
    prelude::*,
};

use crate::prefix::{find, prefix_from_source, prefix_from_url};

pub fn extract_known_shapes_from_config<C: Client + ClientSync + Resource + Clone>(
    config: Res<ServerConfig>,
    client: Res<C>,
    fs: Res<Fs>,
    sender: Res<CommandSender>,
) {
    for on in config.config.local.shapes.iter().cloned() {
        let c = client.clone();
        let fs = fs.clone();
        let sender = sender.0.clone();

        let fut = async move {
            let Some(files) = find(&on, &fs, &c).await else {
                return;
            };

            for (content, url) in files {
                let mut command_queue = CommandQueue::default();
                let item = TextDocumentItem {
                    version: 1,
                    uri: url.clone(),
                    language_id: String::from("turtle"),
                    text: String::new(),
                };

                let spawn = spawn_or_insert(
                    url.clone(),
                    (
                        RopeC(ropey::Rope::from_str(&content)),
                        Source(content.clone()),
                        Label(url.clone()),
                        Wrapped(item),
                        Types(HashMap::new()),
                    ),
                    Some("turtle".into()),
                    (Global,),
                );

                command_queue.push(move |world: &mut World| {
                    let span = tracing::span!(tracing::Level::INFO, "span shapes");
                    let _enter = span.enter();
                    spawn(world);
                    world.run_schedule(ParseLabel);
                });

                let _ = sender.unbounded_send(command_queue);
            }
        };

        client.spawn(fut);
    }
}

pub fn extract_known_prefixes_from_config<C: Client + ClientSync + Resource + Clone>(
    config: Res<ServerConfig>,
    client: Res<C>,
    fs: Res<Fs>,
    sender: Res<CommandSender>,
) {
    for on in config.config.local.ontologies.iter().cloned() {
        let c = client.clone();
        let fs = fs.clone();
        let sender = sender.0.clone();

        let fut = async move {
            let Some(files) = find(&on, &fs, &c).await else {
                return;
            };

            let mut queue = CommandQueue::default();
            for (content, location) in files {
                let Some((prefix, url)) =
                    prefix_from_source(&location, &content).or_else(|| prefix_from_url(&location))
                else {
                    continue;
                };

                tracing::info!(
                    "Adding local prefix from ontologies config location {} prefix {}",
                    url,
                    prefix
                );

                let lov = swls_lov::LocalPrefix {
                    location: Cow::Owned(location.to_string()),
                    namespace: Cow::Owned(url.to_string()),
                    content: Cow::Owned(content),
                    name: prefix.clone(),
                    title: prefix,
                    rank: 0,
                };

                queue.push(move |world: &mut World| {
                    world.spawn(lov);
                });
            }

            let _ = sender.unbounded_send(queue);
        };

        client.spawn(fut);
    }
}
