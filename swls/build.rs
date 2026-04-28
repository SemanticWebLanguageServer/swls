use std::process::Command;

fn main() {
    let output = Command::new("git")
        .args(["rev-parse", "HEAD"])
        .output()
        .ok();

    if let Some(output) = output {
        if let Ok(hash) = String::from_utf8(output.stdout) {
            println!("cargo:rustc-env=GIT_HASH={}", hash.trim());
        }
    }
    let branch = Command::new("git")
        .args(["branch", "--show-current"])
        .output()
        .ok()
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| {
            // fallback for detached HEAD
            std::env::var("GITHUB_REF_NAME").unwrap_or_else(|_| "unknown".into())
        });

    println!("cargo:rustc-env=GIT_BRANCH={}", branch);

    vergen::EmitBuilder::builder().build_date().emit().unwrap();
}
