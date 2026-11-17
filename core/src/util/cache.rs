//! Freshness policy for the ontology cache (see [`FsTrait`](super::fs::FsTrait)).
//!
//! A cached copy younger than [`CACHE_TTL`] is used as is, without touching the
//! network. An older one is refreshed, and kept when refreshing fails, so the
//! server keeps working offline with whatever it fetched before.

use std::{future::Future, time::Duration};

use crate::{lsp_types::Url, util::fs::Fs};

/// How long a cached ontology is used before it is fetched again.
pub const CACHE_TTL: Duration = Duration::from_secs(7 * 24 * 60 * 60);

/// Content for the cache entry at `url`, fetching it with `fetch` when the
/// entry is missing or older than [`CACHE_TTL`].
///
/// A successful fetch is written back to the cache. A failed fetch falls back
/// to the stale entry, if any.
pub async fn cached<F, Fut>(fs: &Fs, url: &Url, fetch: F) -> Option<String>
where
    F: FnOnce() -> Fut,
    Fut: Future<Output = Option<String>>,
{
    let age = fs.0.cache_age(url).await;
    if age.is_some_and(|age| age < CACHE_TTL) {
        if let Some(content) = fs.0.read_file(url).await {
            tracing::debug!("Cache hit {}", url);
            return Some(content);
        }
    }

    match fetch().await {
        Some(content) => {
            store(fs, url, &content).await;
            Some(content)
        }
        None if age.is_some() => {
            tracing::info!("Fetch failed, using stale cache {}", url);
            fs.0.read_file(url).await
        }
        None => None,
    }
}

/// Write `content` to the cache entry at `url`, unless it already holds it.
pub async fn store(fs: &Fs, url: &Url, content: &str) {
    if fs.0.read_file(url).await.as_deref() == Some(content) {
        return;
    }
    if fs.0.write_cache(url, content).await.is_none() {
        tracing::warn!("Failed to write cache {}", url);
    }
}
