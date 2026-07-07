# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 0.1.1 (2026-07-07)

### New Features

 - <csr-id-817cc6bfcc92911b6c6be7b491ab1b00b85e13b9/> better turtle formatting + consolidate language specific implementation (removing generic marker)

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 2 commits contributed to the release over the course of 3 calendar days.
 - 7 days passed between releases.
 - 1 commit was understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Adjusting changelogs prior to release of swls-core v0.2.1, swls-lang-rdf-base v0.2.1, swls-lang-turtle v0.2.1, swls-lang-jsonld v0.2.1, swls-lang-n3 v0.1.1, swls-lang-sparql v0.2.1, swls-lang-trig v0.2.1, swls v0.4.1 ([`2f1e550`](https://github.com/SemanticWebLanguageServer/swls/commit/2f1e5503bae9428b76613ab8b110700234569e1e))
    - Better turtle formatting + consolidate language specific implementation (removing generic marker) ([`817cc6b`](https://github.com/SemanticWebLanguageServer/swls/commit/817cc6bfcc92911b6c6be7b491ab1b00b85e13b9))
</details>

## v0.1.0 (2026-06-29)

<csr-id-53cc4155da52b7054793c81009832fba55c3e2fb/>

### New Features

 - <csr-id-832b6ae7122bc4b071a1a68ffba7c9bc2956d7e3/> make N3 support opt-in (disabled by default)
   N3 stays compiled in but is now gated off at runtime unless the user sets
   `"n3": true` in config. Flips the two existing per-language gates from
   `unwrap_or(true)` to `unwrap_or(false)`:
   
   - parse gate (`parse_n3_system`): `.n3` documents aren't parsed by default.
   - document selector (`backend.rs`): the server no longer advertises handling
   the `n3` language, so clients don't route `.n3` files to it.
   
   The n3 unit tests set `config.n3 = Some(true)` so they still exercise the
   parser. Other languages remain enabled by default.

### Chore

 - <csr-id-53cc4155da52b7054793c81009832fba55c3e2fb/> prune dead features, dependencies, and the swls-token-helpers crate
   - Remove the always-on `shapes` gate (SHACL validation was never actually
     toggleable since parse/save referenced it unconditionally) and the dead
     `tokio`/`agnostic` features. The `sparql_service`/`mie` deps the `tokio`
     feature gated are kept as version pins with `default-features = false`:
     shacl_validation pulls them in transitively, newer releases break against
     the =0.2.9 rudof/shacl stack, and their default `sparql` feature would chain
     into `rudof_rdf/sparql` and drag in tokio/reqwest/mio — which breaks the
     wasm build (swls-web). SHACL only uses NativeEngine, so `sparql` is never
     wanted.
   - Make `tower-lsp` a direct dependency of the `swls` binary instead of a
     shared workspace dependency (core only references it in docs).
   - Remove dead deps `similar`, `lazy_static`, `logos`, `chumsky`, plus a
     full `cargo machete` sweep across every crate.
   - Delete the orphaned `swls-token-helpers` crate (not a workspace member,
     no dependents).
   - Sync `[workspace.dependencies]` version constraints with the bumped crate
     versions so the workspace resolves again.

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 5 commits contributed to the release.
 - 2 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 1 unique issue was worked on: [#32](https://github.com/SemanticWebLanguageServer/swls/issues/32)

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **[#32](https://github.com/SemanticWebLanguageServer/swls/issues/32)**
    - Refactor/replace ropey lineindex ([`81148b6`](https://github.com/SemanticWebLanguageServer/swls/commit/81148b64d51d9399a6d8d76b8f6c5114b90450af))
 * **Uncategorized**
    - Release swls-core v0.2.0, swls-lang-rdf-base v0.2.0, swls-lang-turtle v0.2.0, swls-lang-jsonld v0.2.0, swls-lang-n3 v0.1.0, swls-lang-sparql v0.2.0, swls-lang-trig v0.2.0, swls v0.4.0 ([`9de2e61`](https://github.com/SemanticWebLanguageServer/swls/commit/9de2e6140c68f374c9dbc0c981647f71dd26d26d))
    - Make N3 support opt-in (disabled by default) ([`832b6ae`](https://github.com/SemanticWebLanguageServer/swls/commit/832b6ae7122bc4b071a1a68ffba7c9bc2956d7e3))
    - Prune dead features, dependencies, and the swls-token-helpers crate ([`53cc415`](https://github.com/SemanticWebLanguageServer/swls/commit/53cc4155da52b7054793c81009832fba55c3e2fb))
    - Feat: add lang n3 feat: update prefix diagnostics feat: add format settings ([`c0e3888`](https://github.com/SemanticWebLanguageServer/swls/commit/c0e3888d726463bffd360a3758a33e4df7aa9b02))
</details>

