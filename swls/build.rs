fn main() {
    #[cfg(target_os = "windows")]
    {
        println!("cargo:rustc-link-lib=advapi32");
        println!("cargo:rustc-link-lib=userenv");
        println!("cargo:rustc-link-lib=crypt32");
    }

    vergen::EmitBuilder::builder()
        .git_sha(true)
        .git_describe(true, true, None) // includes tag if present
        .git_branch()
        .build_date()
        .emit()
        .unwrap();
}
