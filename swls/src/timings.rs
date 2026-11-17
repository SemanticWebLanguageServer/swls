use std::{
    collections::HashMap,
    sync::Arc,
    time::{Duration, Instant},
};

use tokio::{
    sync::{mpsc, Mutex},
    task,
};
use tracing::{span, Subscriber};
use tracing_subscriber::{layer::Context, registry::LookupSpan, Layer};

#[derive(Debug)]
enum SpanEvent {
    Enter { name: String, metadata: String },
    Exit { name: String },
}

#[derive(Debug)]
#[allow(unused)]
struct SpanTiming {
    name: String,
    metadata: String,
    start: Instant,
    elapsed: Duration,
    count: u128,
}

#[derive(Debug)]
pub struct TracingLayer {
    sender: mpsc::Sender<SpanEvent>,
}

impl<S> Layer<S> for TracingLayer
where
    S: Subscriber + for<'a> LookupSpan<'a>,
{
    fn on_enter(&self, id: &span::Id, ctx: Context<S>) {
        if let Some(span) = ctx.span(id) {
            let name = span.name().to_string();
            let metadata = span
                .extensions()
                .get::<tracing::field::FieldSet>()
                .map(|fields| format!("{:?}", fields))
                .unwrap_or_else(|| "".to_string());

            let _ = self.sender.try_send(SpanEvent::Enter { name, metadata });
        }
    }

    fn on_exit(&self, id: &span::Id, ctx: Context<S>) {
        if let Some(span) = ctx.span(id) {
            let name = span.name().to_string();
            let _ = self.sender.try_send(SpanEvent::Exit { name });
        }
    }
}

impl Default for TracingLayer {
    fn default() -> Self {
        Self::new()
    }
}

impl TracingLayer {
    pub fn new() -> Self {
        let (sender, mut receiver) = mpsc::channel(100);
        let spans = Arc::new(Mutex::new(HashMap::new()));

        // Spawn a single background task
        let spans_clone = spans.clone();
        task::spawn(async move {
            loop {
                tokio::select! {
                    Some(event) = receiver.recv() => {
                        let mut spans = spans_clone.lock().await;
                        match event {
                            SpanEvent::Enter { name, metadata, .. } => {
                                let e = spans.entry(name).or_insert_with_key(|name| SpanTiming {
                                    name: name.clone(), metadata, start: Instant::now(), elapsed: Duration::ZERO, count: 0
                                });
                                e.start = Instant::now();
                                e.count += 1;
                            }
                            SpanEvent::Exit {  name, .. } => {
                                if let Some(span) = spans.get_mut(&name) {
                                    span.elapsed = span.start.elapsed();
                                    span.count += 1;
                                }
                            }
                        }
                    }
                    _ = tokio::time::sleep(Duration::from_secs(5)) => {
                        let spans = spans_clone.lock().await;

                        let mut longest_spans: Vec<_> = spans.iter().collect();
                        longest_spans.sort_by_key(|(_, timing)| timing.elapsed);
                        longest_spans.reverse();

                        // tracing::info!("🔍 Top 10 Longest Spans:");
                        // for (_, timing) in longest_spans.iter().take(10) {
                        //     tracing::info!(
                        //         "⏳ Span: {} | Count: {} | Mean: {:.3}ms | Time: {:.3}ms",
                        //         timing.name,
                        //         timing.count,
                        //         timing.elapsed.as_millis() / timing.count,
                        //         timing.elapsed.as_millis()
                        //     );
                        // }
                        // let names = longest_spans.iter().map(|(x, _)| x.as_str()).fold(String::new(), |mut acc, item| {
                        //     acc += ", ";
                        //     acc += item;
                        //     acc
                        // });
                        //
                        //     tracing::info!(
                        //         "span names {}",
                        //         names
                        //     );
                    }
                }
            }
        });

        Self { sender }
    }
}
