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

use std::path::{Path, PathBuf};

use async_trait::async_trait;

use crate::error::Result;

/// Entry returned by [`Fs::read_dir`].
#[derive(Debug, Clone)]
pub struct FsDirEntry {
    pub name: String,
    pub path: PathBuf,
    pub is_dir: bool,
}

/// Abstract filesystem trait. Implement this to provide a custom backend
/// (e.g., in-memory for WASM, or virtual for testing).
#[async_trait]
pub trait Fs: Send + Sync {
    /// Read the entire contents of a file as a UTF-8 string.
    async fn read_to_string(&self, path: &Path) -> Result<String>;

    /// List the immediate children of a directory.
    async fn read_dir(&self, path: &Path) -> Result<Vec<FsDirEntry>>;

    /// Check whether `path` is a file.
    async fn is_file(&self, path: &Path) -> bool;

    /// Check whether `path` is a directory.
    async fn is_dir(&self, path: &Path) -> bool;

    /// Resolve symlinks and produce the canonical, absolute path.
    /// In environments without symlinks (e.g., WASM), returning the
    /// input path unchanged is acceptable.
    async fn canonicalize(&self, path: &Path) -> Result<PathBuf>;
}

// ── Convenience helpers built on top of the trait ────────────────────────

/// Check whether a path exists (file or directory).
pub async fn exists(fs: &dyn Fs, path: &Path) -> bool {
    fs.is_file(path).await || fs.is_dir(path).await
}

/// Recursively walk a directory, returning all file paths.
/// Follows directories (like `walkdir` with follow_links).
pub async fn walk_dir(fs: &dyn Fs, root: &Path) -> Result<Vec<PathBuf>> {
    let mut result = Vec::new();
    let mut stack = vec![root.to_path_buf()];

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
#[async_trait]
impl Fs for OsFs {
    async fn read_to_string(&self, path: &Path) -> Result<String> {
        Ok(tokio::fs::read_to_string(path).await?)
    }

    async fn read_dir(&self, path: &Path) -> Result<Vec<FsDirEntry>> {
        let mut entries = Vec::new();
        let mut rd = tokio::fs::read_dir(path).await?;
        while let Some(entry) = rd.next_entry().await? {
            let metadata = entry.metadata().await?;
            entries.push(FsDirEntry {
                name: entry.file_name().to_string_lossy().into_owned(),
                path: entry.path(),
                is_dir: metadata.is_dir(),
            });
        }
        Ok(entries)
    }

    async fn is_file(&self, path: &Path) -> bool {
        tokio::fs::metadata(path)
            .await
            .map(|m| m.is_file())
            .unwrap_or(false)
    }

    async fn is_dir(&self, path: &Path) -> bool {
        tokio::fs::metadata(path)
            .await
            .map(|m| m.is_dir())
            .unwrap_or(false)
    }

    async fn canonicalize(&self, path: &Path) -> Result<PathBuf> {
        Ok(tokio::fs::canonicalize(path).await?)
    }
}
