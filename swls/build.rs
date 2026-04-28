fn main() {
    vergen::EmitBuilder::builder()
        .git_sha(true)
        .git_describe(true, true, None) // includes tag if present
        .git_branch()
        .build_date()
        .emit()
        .unwrap();
}
