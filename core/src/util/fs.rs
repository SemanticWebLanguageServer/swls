use crate::lsp_types::Url;
use bevy_ecs::prelude::Resource;
use derive_more::derive::AsRef;
use std::sync::Arc;

#[derive(Resource, Clone, AsRef, Debug)]
pub struct Fs(pub Arc<dyn FsTrait + Sync + Send>);

#[derive(Debug)]
pub struct File {
    pub name: String,
    pub content: String,
}

#[derive(Debug, Clone)]
pub struct FsDirEntry {
    pub name: String,
    pub path: Url,
    pub is_dir: bool,
}

/// Platform file system used by the server.
///
/// Besides plain file access it owns the **ontology cache**: copies of remote
/// ontologies and shapes that the server loads in the background. Cached files
/// live under [`cache_root`](Self::cache_root), at human-readable locations so
/// goto-definition shows `…/lov/foaf.ttl` instead of an opaque URL:
///
/// - `lov/<prefix>.ttl` for an ontology resolved by prefix ([`lov_url`](Self::lov_url)),
/// - `sources/<host>/<path>` for an ontology or shape at a known URL
///   ([`cache_url`](Self::cache_url)).
///
/// The cache is shared by all server processes of a user and reused across
/// restarts (see [`crate::util::cache`]). Files in it are snapshots managed by
/// the server; editing them has no effect on the server.
#[async_trait::async_trait]
pub trait FsTrait: Send + Sync + 'static + std::fmt::Debug {
    /// Location of `path`, relative to the cache root.
    fn virtual_url(&self, path: &str) -> Option<crate::lsp_types::Url>;

    /// Root of the ontology cache, `None` when there is no on-disk cache.
    fn cache_root(&self) -> Option<Url> {
        None
    }

    /// Whether `url` is a file inside the ontology cache.
    fn is_cache_url(&self, url: &Url) -> bool {
        self.cache_root()
            .is_some_and(|root| url.as_str().starts_with(root.as_str()))
    }

    /// Cache location of the ontology for namespace `url`, written as `prefix:`.
    fn lov_url(&self, url: &str, prefix: &str) -> Option<crate::lsp_types::Url> {
        if !url.starts_with("http") {
            return None;
        }
        self.virtual_url(&format!("lov/{}.ttl", prefix))
    }

    /// Cache location of the remote document at `source`:
    /// `<cache>/sources/<host>/<path>`.
    ///
    /// Unlike [`lov_url`](Self::lov_url), which is keyed by prefix name, this is
    /// keyed by the source itself, so distinct documents that describe the same
    /// namespace never share a location (and therefore never share an entity).
    fn cache_url(&self, source: &Url) -> Option<Url> {
        let host = source.host_str()?;
        let path = source.path().trim_start_matches('/');
        let mut relative = format!("sources/{}/{}", host, path);
        let last = relative.rsplit('/').next().unwrap_or_default();
        if last.is_empty() {
            relative.push_str("index.ttl");
        } else if !last.contains('.') {
            relative.push_str(".ttl");
        }
        self.virtual_url(&relative)
    }

    /// Write a file into the cache.
    ///
    /// Implementations should write atomically (concurrent servers share the
    /// cache) and mark the file read-only, so editors warn before it is edited.
    async fn write_cache(&self, url: &Url, content: &str) -> Option<()> {
        self.write_file(url, content).await
    }

    /// Time since the cached file at `url` was written, `None` if it is absent.
    async fn cache_age(&self, _url: &Url) -> Option<std::time::Duration> {
        None
    }

    async fn read_file(&self, url: &crate::lsp_types::Url) -> Option<String>;
    async fn glob_read(&self, url: &str) -> Option<Vec<File>>;
    async fn write_file(&self, url: &crate::lsp_types::Url, content: &str) -> Option<()>;

    async fn read_dir(&self, path: &crate::lsp_types::Url) -> Option<Vec<FsDirEntry>>;
    async fn is_file(&self, path: &crate::lsp_types::Url) -> bool;
    async fn is_dir(&self, path: &crate::lsp_types::Url) -> bool;

    async fn glob(&self, base: &Url, pattern: &str) -> Option<Vec<FsDirEntry>>;

    /// Files under `root` whose name ends with one of `extensions`, at most
    /// `max`. Skips hidden folders, `node_modules` and `target`. The default
    /// finds nothing (no file system to walk).
    async fn walk(&self, _root: &Url, _extensions: &[&str], _max: usize) -> Vec<Url> {
        Vec::new()
    }

    /// Whether [`walk`](Self::walk) would skip `file` inside `root` (ignored
    /// by git, hidden, …). The default ignores nothing.
    async fn is_ignored(&self, _root: &Url, _file: &Url) -> bool {
        false
    }
}

/// Folders a workspace walk does not descend into, even outside a git
/// repository or when nothing ignores them.
pub fn skip_folder(name: &str) -> bool {
    name.starts_with('.') || name == "node_modules" || name == "target"
}

/// Walk `root` on the local file system (see [`FsTrait::walk`]).
///
/// Skips what git would ignore: `.gitignore` files at every level,
/// `.git/info/exclude`, the global git excludes, and `.ignore` files, also
/// outside a git repository. Hidden entries and [`skip_folder`]s are always
/// skipped. Entries are visited in name order.
#[cfg(not(target_arch = "wasm32"))]
pub fn walk_dir(
    root: &std::path::Path,
    extensions: &[&str],
    max: usize,
) -> Vec<std::path::PathBuf> {
    let walker = ignore::WalkBuilder::new(root)
        .hidden(true)
        .git_ignore(true)
        .git_exclude(true)
        .git_global(true)
        .ignore(true)
        .parents(true)
        .require_git(false)
        .sort_by_file_name(|a, b| a.cmp(b))
        .filter_entry(|e| {
            let is_dir = e.file_type().is_some_and(|t| t.is_dir());
            e.depth() == 0 || !is_dir || !skip_folder(&e.file_name().to_string_lossy())
        })
        .build();
    let mut out = Vec::new();
    for entry in walker.flatten() {
        let is_file = entry.file_type().is_some_and(|t| t.is_file());
        let name = entry.file_name().to_string_lossy();
        if is_file && extensions.iter().any(|ext| name.ends_with(ext)) {
            out.push(entry.into_path());
            if out.len() >= max {
                break;
            }
        }
    }
    out
}

/// Whether `file`, inside workspace folder `root`, is one that
/// [`walk_dir`] would skip: in a hidden or [`skip_folder`], or ignored by a
/// `.gitignore` / `.ignore` file between `root` and the file, or by
/// `.git/info/exclude`. Used for files created after the workspace was
/// indexed. (The global git excludes are only applied by [`walk_dir`].)
#[cfg(not(target_arch = "wasm32"))]
pub fn is_ignored(root: &std::path::Path, file: &std::path::Path) -> bool {
    use ignore::{gitignore::GitignoreBuilder, Match};

    let Ok(relative) = file.strip_prefix(root) else {
        return true;
    };
    let components: Vec<_> = relative.components().collect();
    let folders = components.len().saturating_sub(1);
    let hidden_or_skipped = components.iter().enumerate().any(|(i, c)| {
        let name = c.as_os_str().to_string_lossy();
        name.starts_with('.') || (i < folders && skip_folder(&name))
    });
    if hidden_or_skipped {
        return true;
    }

    // Ignore files of `root` and every folder on the way to `file`; deeper
    // files take precedence, as in git.
    let mut dirs = vec![root.to_path_buf()];
    for c in &components[..folders] {
        let next = dirs.last().unwrap().join(c);
        dirs.push(next);
    }
    let is_dir = file.is_dir();
    for (i, dir) in dirs.iter().enumerate().rev() {
        let mut builder = GitignoreBuilder::new(dir);
        let mut any = false;
        for name in [".gitignore", ".ignore"] {
            let path = dir.join(name);
            if path.is_file() {
                any |= builder.add(path).is_none();
            }
        }
        if i == 0 {
            let exclude = dir.join(".git").join("info").join("exclude");
            if exclude.is_file() {
                any |= builder.add(exclude).is_none();
            }
        }
        if !any {
            continue;
        }
        let Ok(matcher) = builder.build() else {
            continue;
        };
        match matcher.matched_path_or_any_parents(file, is_dir) {
            Match::Ignore(_) => return true,
            Match::Whitelist(_) => return false,
            Match::None => {}
        }
    }
    false
}

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;

    fn tree(files: &[(&str, &str)]) -> std::path::PathBuf {
        static N: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        let n = N.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let root = std::env::temp_dir().join(format!("swls-walk-{}-{n}", std::process::id()));
        let _ = std::fs::remove_dir_all(&root);
        for (path, content) in files {
            let path = root.join(path);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, content).unwrap();
        }
        root
    }

    fn walked(root: &std::path::Path) -> Vec<String> {
        let mut out: Vec<String> = walk_dir(root, &[".ttl"], 100)
            .into_iter()
            .map(|p| {
                p.strip_prefix(root)
                    .unwrap()
                    .to_string_lossy()
                    .replace('\\', "/")
            })
            .collect();
        out.sort();
        out
    }

    const FILES: &[(&str, &str)] = &[
        (
            ".gitignore",
            "build/\n*.generated.ttl\n!keep.generated.ttl\n",
        ),
        ("vocab.ttl", ""),
        ("a.generated.ttl", ""),
        ("keep.generated.ttl", ""),
        ("build/out.ttl", ""),
        ("node_modules/pkg/x.ttl", ""),
        (".hidden/x.ttl", ""),
        ("sub/.gitignore", "local.ttl\n"),
        ("sub/local.ttl", ""),
        ("sub/shared.ttl", ""),
        ("notes.md", ""),
    ];

    #[test]
    fn walk_skips_what_git_ignores_even_outside_a_repository() {
        let root = tree(FILES);
        assert_eq!(
            walked(&root),
            ["keep.generated.ttl", "sub/shared.ttl", "vocab.ttl"]
        );
        let _ = std::fs::remove_dir_all(&root);
    }

    #[test]
    fn is_ignored_matches_the_walk() {
        let root = tree(FILES);
        let ignored = |p: &str| is_ignored(&root, &root.join(p));
        for p in [
            "build/out.ttl",
            "a.generated.ttl",
            "sub/local.ttl",
            "node_modules/pkg/x.ttl",
            ".hidden/x.ttl",
        ] {
            assert!(ignored(p), "{p} should be ignored");
        }
        for p in [
            "vocab.ttl",
            "keep.generated.ttl",
            "sub/shared.ttl",
            "sub/new.ttl",
        ] {
            assert!(!ignored(p), "{p} should not be ignored");
        }
        assert!(is_ignored(&root, std::path::Path::new("/elsewhere/x.ttl")));
        let _ = std::fs::remove_dir_all(&root);
    }

    #[test]
    fn git_info_exclude_is_respected() {
        let root = tree(&[
            (".git/info/exclude", "private.ttl\n"),
            ("private.ttl", ""),
            ("public.ttl", ""),
        ]);
        assert_eq!(walked(&root), ["public.ttl"]);
        assert!(is_ignored(&root, &root.join("private.ttl")));
        let _ = std::fs::remove_dir_all(&root);
    }
}
