# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## v0.1.4 (2026-11-18)

### Chore

 - <csr-id-b566e5f3228a4578291186a1eaa985ade4c736ca/> fix clippy warnings and configure workspace lints
   Makes `cargo clippy --workspace --all-targets -- -D warnings` pass.
   
   - Workspace lints (`[workspace.lints.clippy]`, inherited by every crate):
     `type_complexity` and `too_many_arguments` are allowed, because Bevy
     systems take their data as parameters and nested `Query` types.
   - `cargo clippy --fix` for the mechanical lints (needless borrows, redundant
     closures and returns, `'static` on constants, …).
   - By hand:
     - `Option::is_none_or` (Rust 1.82) replaced in components-rs, whose MSRV
       is 1.75;
     - `try_next` (deprecated) replaced by `try_recv` on the command and
       diagnostic channels;
     - `MyQuad::into_oxi(_graph)` return `Option` instead of `Result<_, ()>`;
     - `PartialOrd` for `MyTerm` delegates to `Ord`;
     - `assert!(false, …)` → `panic!` in the conformance tests;
     - a few `sort_by_key`, `while let`, `is_err()` and merged `if` branches;
     - `TestFs: Default`;
     - kept the needed clone in the config loaders (clippy's suggestion does
       not compile, the value moves into a spawned future);
     - pinned a lifetime in `prefix_completion_helper` that the elided form
       could not express.
   - `wrong_self_convention` is allowed on the two public `into_triples` trait
     methods rather than renaming them across crates.

### Style

 - <csr-id-8160444de14ceb8c86d3b22412e95a6b62974d46/> cargo fmt --all
   Formatting only, no code changes. Listed in .git-blame-ignore-revs.

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 2 commits contributed to the release.
 - 2 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Fix clippy warnings and configure workspace lints ([`b566e5f`](https://github.com/SemanticWebLanguageServer/swls/commit/b566e5f3228a4578291186a1eaa985ade4c736ca))
    - Cargo fmt --all ([`8160444`](https://github.com/SemanticWebLanguageServer/swls/commit/8160444de14ceb8c86d3b22412e95a6b62974d46))
</details>

## v0.1.3 (2026-10-14)

<csr-id-4debbca4c3e1839781b7728951f91f2ba729165b/>

### New Features

 - <csr-id-37ca76298f9b43001bdcc6f9096a8611b8559128/> bump bevy_ecs to 0.18!
 - <csr-id-35ff9925b3957c491e3798a62ae4e4e32f88d40e/> make cjs work for vscode
 - <csr-id-44cb452ffa0417562a5d26c863981419b56df701/> move components-rs to Url
 - <csr-id-663af2958384584b8111e41a3131fb55984b71bf/> fix many minor mistakes
 - <csr-id-987907ef012c3d41a094a3b7bf853ed0a55d23a8/> bump
 - <csr-id-97441d4d9dc7778d3fcb6b0862ad0d61fa38b01f/> check if cjs to properties and classes is a good idea
 - <csr-id-5ec44b5d4c325bb6687bf70292c29b74d4770ded/> goto @context string
 - <csr-id-1dff5c4281c42f1ad59b955a860b0ffc1ee486e2/> move components-rs into swls

### Other

 - <csr-id-4debbca4c3e1839781b7728951f91f2ba729165b/> fix jsonld goto definition for the last time!

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 12 commits contributed to the release over the course of 180 calendar days.
 - 9 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Release swls-lov v0.1.3, swls-core v0.2.2, components-rs v0.1.3, swls-lang-rdf-base v0.2.2, swls-lang-turtle v0.2.2, swls-lang-jsonld v0.2.2, swls-lang-n3 v0.1.2, swls-lang-sparql v0.2.2, swls-lang-trig v0.2.2, swls v0.5.0 ([`8e33bcd`](https://github.com/SemanticWebLanguageServer/swls/commit/8e33bcd501862e31fb5edefbce63909abf26feff))
    - Adjusting changelogs prior to release of swls-lov v0.1.3, swls-core v0.2.2, components-rs v0.1.3, swls-lang-rdf-base v0.2.2, swls-lang-turtle v0.2.2, swls-lang-jsonld v0.2.2, swls-lang-n3 v0.1.2, swls-lang-sparql v0.2.2, swls-lang-trig v0.2.2, swls v0.5.0 ([`3e999f2`](https://github.com/SemanticWebLanguageServer/swls/commit/3e999f28fe47eadd3d7eaa7067d2d68025365485))
    - Release swls-lov v0.1.1, swls-core v0.1.1, components-rs v0.1.2, swls-lang-rdf-base v0.1.0, swls-lang-turtle v0.1.1, swls-lang-jsonld v0.1.1, swls-lang-sparql v0.1.1, swls-lang-trig v0.1.0, swls v0.1.1 ([`612f47d`](https://github.com/SemanticWebLanguageServer/swls/commit/612f47d34fbde7ad5025165735b0c19eb6d6b203))
    - Bump bevy_ecs to 0.18! ([`37ca762`](https://github.com/SemanticWebLanguageServer/swls/commit/37ca76298f9b43001bdcc6f9096a8611b8559128))
    - Make cjs work for vscode ([`35ff992`](https://github.com/SemanticWebLanguageServer/swls/commit/35ff9925b3957c491e3798a62ae4e4e32f88d40e))
    - Fix jsonld goto definition for the last time! ([`4debbca`](https://github.com/SemanticWebLanguageServer/swls/commit/4debbca4c3e1839781b7728951f91f2ba729165b))
    - Move components-rs to Url ([`44cb452`](https://github.com/SemanticWebLanguageServer/swls/commit/44cb452ffa0417562a5d26c863981419b56df701))
    - Fix many minor mistakes ([`663af29`](https://github.com/SemanticWebLanguageServer/swls/commit/663af2958384584b8111e41a3131fb55984b71bf))
    - Bump ([`987907e`](https://github.com/SemanticWebLanguageServer/swls/commit/987907ef012c3d41a094a3b7bf853ed0a55d23a8))
    - Check if cjs to properties and classes is a good idea ([`97441d4`](https://github.com/SemanticWebLanguageServer/swls/commit/97441d4d9dc7778d3fcb6b0862ad0d61fa38b01f))
    - Goto @context string ([`5ec44b5`](https://github.com/SemanticWebLanguageServer/swls/commit/5ec44b5d4c325bb6687bf70292c29b74d4770ded))
    - Move components-rs into swls ([`1dff5c4`](https://github.com/SemanticWebLanguageServer/swls/commit/1dff5c4281c42f1ad59b955a860b0ffc1ee486e2))
</details>

## v0.1.2 (2026-04-28)

<csr-id-4debbca4c3e1839781b7728951f91f2ba729165b/>

### New Features

 - <csr-id-37ca76298f9b43001bdcc6f9096a8611b8559128/> bump bevy_ecs to 0.18!
 - <csr-id-35ff9925b3957c491e3798a62ae4e4e32f88d40e/> make cjs work for vscode
 - <csr-id-44cb452ffa0417562a5d26c863981419b56df701/> move components-rs to Url
 - <csr-id-663af2958384584b8111e41a3131fb55984b71bf/> fix many minor mistakes
 - <csr-id-987907ef012c3d41a094a3b7bf853ed0a55d23a8/> bump
 - <csr-id-97441d4d9dc7778d3fcb6b0862ad0d61fa38b01f/> check if cjs to properties and classes is a good idea
 - <csr-id-5ec44b5d4c325bb6687bf70292c29b74d4770ded/> goto @context string
 - <csr-id-1dff5c4281c42f1ad59b955a860b0ffc1ee486e2/> move components-rs into swls

### Other

 - <csr-id-4debbca4c3e1839781b7728951f91f2ba729165b/> fix jsonld goto definition for the last time!

