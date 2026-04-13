use std::{
    fs,
    path::{self, Path, PathBuf},
    time::{Duration, Instant},
};

use swls_core::lsp_types::Url;
use swls_lang_turtle::lang::parse_source;

fn sorted_file_paths_by_size_desc(dir: impl AsRef<Path>) -> std::io::Result<Vec<PathBuf>> {
    let mut files = fs::read_dir(dir)?
        .filter_map(Result::ok)
        .filter_map(|entry| {
            let path = entry.path();
            if path.is_file() {
                match fs::metadata(&path) {
                    Ok(meta) => Some((path, meta.len())),
                    Err(_) => None,
                }
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    files.sort_by(|a, b| b.1.cmp(&a.1));
    Ok(files.into_iter().map(|(path, _)| path).collect())
}

fn main() {
    let paths = sorted_file_paths_by_size_desc("./lov/prefixes/").unwrap();
    let mut total_parse = Duration::default();

    for path in paths {
        let name = path;
        println!("\nName: {}", name.display());
        let absolute = path::absolute(&name).expect("valid url");
        let url = Url::from_file_path(absolute).expect("valid url");
        let content = fs::read_to_string(&name).unwrap();
        if content.is_empty() {
            continue;
        }

        let now = Instant::now();
        let (turtle, errors) = parse_source(&url, &content);
        let elapsed = now.elapsed();
        total_parse += elapsed;

        println!(
            "  Parse: {:.2}ms ({:.2} MB/s) — {} triples, {} errors",
            elapsed.as_millis(),
            content.len() as f64 / 1024.0 / 1024.0 / elapsed.as_secs_f64(),
            turtle.map(|t| t.triples.len()).unwrap_or(0),
            errors.len(),
        );
    }

    println!("\nTotal parse time: {:.2}ms", total_parse.as_millis());
}
