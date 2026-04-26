//! Abstract filesystem trait and the default OS-backed implementation.
//!
//! All I/O in this crate goes through [`Fs`] rather than `std::fs` / `tokio::fs` directly.
//! This makes it possible to swap in an in-memory implementation for:
//!
//! - **Unit tests** — avoid touching the real disk.
//! - **WASM / LSP clients** — provide file content from the editor's in-memory document model
//!   instead of reading from disk, so the server sees unsaved changes immediately.
//!
//! The default [`OsFs`] implementation (enabled by the `tokio` feature) delegates to
//! `tokio::fs` and is suitable for a native language server process.
//!
//! All paths are represented as [`url::Url`] with the `file://` scheme.  Directory URLs
//! **always** end with a trailing `/` so that [`Url::join`] appends correctly.

use async_trait::async_trait;
use url::Url;

use crate::error::Result;

/// Entry returned by [`Fs::read_dir`].
///
/// `path` is a `file://` URL. For directory entries it ends with `/`.
#[derive(Debug, Clone)]
pub struct FsDirEntry {
    pub name: String,
    pub path: Url,
    pub is_dir: bool,
}

/// Abstract filesystem trait. Implement this to provide a custom backend
/// (e.g., in-memory for WASM, or virtual for testing).
#[async_trait]
pub trait Fs: Send + Sync {
    /// Read the entire contents of a file as a UTF-8 string.
    async fn read_to_string(&self, url: &Url) -> Result<String>;

    /// List the immediate children of a directory.
    async fn read_dir(&self, url: &Url) -> Result<Vec<FsDirEntry>>;

    /// Check whether `url` is a file.
    async fn is_file(&self, url: &Url) -> bool;

    /// Check whether `url` is a directory.
    async fn is_dir(&self, url: &Url) -> bool;

    /// Return all entries whose paths match `pattern` relative to `base`.
    ///
    /// `pattern` uses the same glob syntax as the platform-native glob (e.g.
    /// `node_modules/*/package.json`, `node_modules/@*/*/package.json`).
    /// Implementations should delegate to a native glob facility rather than
    /// hand-rolling their own matching.
    async fn glob(&self, base: &Url, pattern: &str) -> Result<Vec<FsDirEntry>>;
}

// ── Convenience helpers built on top of the trait ────────────────────────

/// Check whether a URL exists (file or directory).
pub async fn exists(fs: &dyn Fs, url: &Url) -> bool {
    fs.is_file(url).await || fs.is_dir(url).await
}

/// Recursively walk a directory, returning all file URLs.
pub async fn walk_dir(fs: &dyn Fs, root: &Url) -> Result<Vec<Url>> {
    let mut result = Vec::new();
    let mut stack = vec![root.clone()];

    while let Some(dir) = stack.pop() {
        if !fs.is_dir(&dir).await {
            continue;
        }
        let entries = fs.read_dir(&dir).await?;
        for entry in entries {
            if entry.is_dir {
                stack.push(entry.path.clone());
            } else {
                result.push(entry.path);
            }
        }
    }

    Ok(result)
}

// ── Default implementation backed by the real OS filesystem ─────────────

/// Standard filesystem implementation using `tokio::fs`.
///
/// Available only when the `tokio` feature is enabled.
#[cfg(feature = "tokio")]
#[derive(Debug, Clone, Copy, Default)]
pub struct OsFs;

#[cfg(feature = "tokio")]
fn url_to_path(url: &Url) -> crate::error::Result<std::path::PathBuf> {
    url.to_file_path()
        .map_err(|_| crate::error::ComponentsJsError::InvalidUrl(url.to_string()))
}

#[cfg(feature = "tokio")]
#[async_trait]
impl Fs for OsFs {
    async fn glob(&self, base: &Url, pattern: &str) -> Result<Vec<FsDirEntry>> {
        let base_path = url_to_path(base)?;
        let full_pattern = base_path.join(pattern).to_string_lossy().into_owned();

        let mut entries = Vec::new();
        let paths = glob::glob(&full_pattern)
            .map_err(|e| crate::error::ComponentsJsError::General(e.to_string()))?;
        for path in paths {
            let path = path.map_err(|e| e.into_error())?;
            let is_dir = path.is_dir();
            let name = path
                .file_name()
                .map(|n| n.to_string_lossy().into_owned())
                .unwrap_or_default();
            let entry_url = if is_dir {
                Url::from_directory_path(&path).map_err(|_| {
                    crate::error::ComponentsJsError::InvalidUrl(path.display().to_string())
                })?
            } else {
                Url::from_file_path(&path).map_err(|_| {
                    crate::error::ComponentsJsError::InvalidUrl(path.display().to_string())
                })?
            };
            entries.push(FsDirEntry {
                name,
                path: entry_url,
                is_dir,
            });
        }

        Ok(entries)
    }

    async fn read_to_string(&self, url: &Url) -> Result<String> {
        let path = url_to_path(url)?;
        Ok(tokio::fs::read_to_string(path).await?)
    }

    async fn read_dir(&self, url: &Url) -> Result<Vec<FsDirEntry>> {
        let path = url_to_path(url)?;
        let mut entries = Vec::new();
        let mut rd = tokio::fs::read_dir(&path).await?;
        while let Some(entry) = rd.next_entry().await? {
            let metadata = entry.metadata().await?;
            let is_dir = metadata.is_dir();
            let entry_path = entry.path();
            let entry_url = if is_dir {
                Url::from_directory_path(&entry_path).map_err(|_| {
                    crate::error::ComponentsJsError::InvalidUrl(entry_path.display().to_string())
                })?
            } else {
                Url::from_file_path(&entry_path).map_err(|_| {
                    crate::error::ComponentsJsError::InvalidUrl(entry_path.display().to_string())
                })?
            };
            entries.push(FsDirEntry {
                name: entry.file_name().to_string_lossy().into_owned(),
                path: entry_url,
                is_dir,
            });
        }
        Ok(entries)
    }

    async fn is_file(&self, url: &Url) -> bool {
        match url_to_path(url) {
            Ok(path) => tokio::fs::metadata(path)
                .await
                .map(|m| m.is_file())
                .unwrap_or(false),
            Err(_) => false,
        }
    }

    async fn is_dir(&self, url: &Url) -> bool {
        match url_to_path(url) {
            Ok(path) => tokio::fs::metadata(path)
                .await
                .map(|m| m.is_dir())
                .unwrap_or(false),
            Err(_) => false,
        }
    }
}
