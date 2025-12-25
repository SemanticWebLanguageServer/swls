use std::{
    fs,
    path::{self, Path, PathBuf},
    time::{Duration, Instant},
};

use chumsky::prelude::*;
use lang_turtle::lang::{
    parse_source,
    // parser2::parse_source as parse_source2,
    tokenizer::{parse_tokens, parse_tokens_str_safe},
};
use lsp_core::lsp_types::Url;

fn tokenize_combinator(inp: &str) -> bool {
    parse_tokens().parse(inp).is_ok()
}

fn tokenize_combinator_recovery(inp: &str) -> bool {
    parse_tokens().parse_recovery(inp).0.is_some()
}

fn tokenize_logos(inp: &str) -> bool {
    parse_tokens_str_safe(inp).is_ok()
}

type N = Duration;
#[derive(Debug, Default)]
struct Timer {
    combinator: N,
    combinator_recovery: N,
    logos: N,
}

impl Timer {
    fn new() -> Self {
        Self::default()
    }

    fn add(&mut self, inp: &str) {
        let now = Instant::now();
        let comb = tokenize_combinator(inp);
        self.combinator += now.elapsed();

        let now = Instant::now();
        let comb_rec = tokenize_combinator_recovery(inp);
        self.combinator_recovery += now.elapsed();

        let now = Instant::now();
        let logos = tokenize_logos(inp);
        self.logos += now.elapsed();

        if !(logos) {
            panic!("comb {}, comb_rec {} logos {}", comb, comb_rec, logos);
        }
    }
}

#[derive(Debug, Default)]
struct Timer2 {
    combinator: N,
    parser: N,
}

impl Timer2 {
    fn new() -> Self {
        Self::default()
    }

    fn add(&mut self, inp: &str, url: &Url) {
        let now = Instant::now();
        let _ = parse_source(&url, inp);
        // let comb_good = comb.0.is_some() && comb.1.is_empty();
        self.combinator += now.elapsed();

        // let now = Instant::now();
        // let logos = parse_source2(&url, inp);
        // let par_good = logos.is_some();
        // self.parser += now.elapsed();

        // if !comb_good || !par_good {
        //     panic!("comb {} logos {}", comb_good, par_good);
        // }
    }
}

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

    files.sort_by(|a, b| b.1.cmp(&a.1)); // sort by size descending

    Ok(files.into_iter().map(|(path, _)| path).collect())
}

fn main() {
    let paths = sorted_file_paths_by_size_desc("./lov/prefixes/").unwrap();

    for path in paths {
        let mut timer = Timer::new();
        let mut timer2 = Timer2::new();
        let name = path;
        println!("\nName: {}", name.display());
        let absolute = path::absolute(&name).expect("valid url");
        let url = lsp_core::lsp_types::Url::from_file_path(absolute).expect("valid url");
        let content = fs::read_to_string(name).unwrap();
        if content.len() == 0 {
            continue;
        }

        timer2.add(&content, &url);
        timer.add(&content);
        println!("=== Tokenizing ===");
        println!(
            "combinator is {} times slower",
            (timer.combinator.as_nanos() / timer.logos.as_nanos())
        );
        println!(
            "Combinator speed: {:.2} MB/s\tLogos speed: {:.2} MB/s",
            content.len() as f64 / 1024.0 / 1024.0 / timer.combinator.as_secs_f64(),
            content.len() as f64 / 1024.0 / 1024.0 / timer.logos.as_secs_f64()
        );

        println!("=== Parsing ===");
        println!(
            "combinator is {} times slower",
            (timer2.combinator.as_nanos() / timer2.parser.as_nanos())
        );
        println!(
            "Combinator speed: {:.2} MB/s\tParser speed: {:.2} MB/s",
            content.len() as f64 / 1024.0 / 1024.0 / timer2.combinator.as_secs_f64(),
            content.len() as f64 / 1024.0 / 1024.0 / timer2.parser.as_secs_f64()
        );
        println!(
            "Combinator speed: {:.2}ms\tParser speed: {:.2}ms ({:.2} MB)",
            timer2.combinator.as_millis(),
            timer2.parser.as_millis(),
            content.len() as f64 / 1024.0 / 1024.0
        );
    }
}
