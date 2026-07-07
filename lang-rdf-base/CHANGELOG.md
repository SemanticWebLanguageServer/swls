# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### New Features

 - <csr-id-43a8054c274bde6e63475dcdccd65af9d24b7613/> prefix hover/goto-definition + JSON-LD prefix-diagnostics opt-out
   Prefix handling across languages, plus model-authoritative JSON-LD
   semantic-token colouring and the datatype-span work this built on.
   
   Hover / goto-definition on prefixes (new core/src/systems/prefix_hover.rs):
   - get_current_prefix detects a prefix declaration / JSON-LD @context term at the
     cursor from the parsed model (which, unlike derived triples, carries spans) and
     drops the wrong nearest-triple fallback so the triple-based hovers no-op.
   - hover_prefix shows the prefix -> namespace mapping (+ LOV title when known).
   - goto_prefix jumps a real namespace to its ontology file; goto_cjs now steps
     aside for namespaces but still resolves term aliases to their CJS definition.
   - Wired into the Hover and GotoDefinition schedules; new `definition()` e2e
     harness helper; e2e/tests/prefix_hover.rs.
   
   JSON-LD prefix diagnostics:
   - supports_prefix_diagnostics() = false for JSON-LD; @context has no prefix/alias
     distinction and pulls in shared/remote terms, so the span-based detector emitted
     false positives. e2e/tests/prefix_diagnostics.rs updated to assert the opt-out.
   
   Prefix diagnostics core:
   - Walk the parsed Turtle model instead of the derived triples so prefixes that
     only appear in a datatype ("5"^^xsd:integer) are detected.
   
   JSON-LD semantic tokens (model-authoritative):
   - Colour every term from the parsed model: a compact IRI `"ex:obs1"` is the term
     colour (quotes + local = enumMember) with `prefix:` NAMESPACE on top, applied
     consistently to @id subjects and nested object references; @context prefix keys
     are NAMESPACE.
   - The lexer supplies only what the model can't see: keywords (`@id`, `@type`, …)
     and the STRING/NUMBER/BOOLEAN base for real literals. Its previous
     string-followed-by-`:` NAMESPACE guess is removed.
   - Keep KEYWORD colouring for `@type` even though it is the rdf:type predicate in
     the model. Adapt to the rdf-parsers Spanned lang/ty API and carry
     context-computed IRIs. Tests in lang-jsonld/src/ecs/mod.rs pin the byte types.
   
   Depends on rdf-parsers 0.1.16.
 - <csr-id-817cc6bfcc92911b6c6be7b491ab1b00b85e13b9/> better turtle formatting + consolidate language specific implementation (removing generic marker)

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 2 commits contributed to the release over the course of 3 calendar days.
 - 7 days passed between releases.
 - 2 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Prefix hover/goto-definition + JSON-LD prefix-diagnostics opt-out ([`43a8054`](https://github.com/SemanticWebLanguageServer/swls/commit/43a8054c274bde6e63475dcdccd65af9d24b7613))
    - Better turtle formatting + consolidate language specific implementation (removing generic marker) ([`817cc6b`](https://github.com/SemanticWebLanguageServer/swls/commit/817cc6bfcc92911b6c6be7b491ab1b00b85e13b9))
</details>

## 0.2.0 (2026-06-29)

<csr-id-53cc4155da52b7054793c81009832fba55c3e2fb/>

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

### New Features

 - <csr-id-faad6d5d3e8023269a265c58352644e590bc47f2/> gate features with configuration
 - <csr-id-71381b48710c992a6233aa080af53730d2657040/> inline and extract blank nodes

### Bug Fixes

 - <csr-id-5085b23f3eb4309daf5d6b13853cfb89facae3d6/> improve rename robustness
 - <csr-id-3bbf1cd80df9ff26b109c7e30da844cbc262d66e/> remove unused systems
 - <csr-id-1292fb8ccbdb3b10f01a7eb9e6bc64ccfcbda9dc/> better timing on the validation to reduce race conditions
 - <csr-id-dc8482d1c433868c18ec54017d4bc2b12776592c/> diagnostics after on change things works way better
 - <csr-id-00bfb2dc81f0db86b331124e3c0a7856cb5d65da/> nested json-ld objects and arrays

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 10 commits contributed to the release over the course of 7 calendar days.
 - 40 days passed between releases.
 - 8 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Release swls-core v0.2.0, swls-lang-rdf-base v0.2.0, swls-lang-turtle v0.2.0, swls-lang-jsonld v0.2.0, swls-lang-n3 v0.1.0, swls-lang-sparql v0.2.0, swls-lang-trig v0.2.0, swls v0.4.0 ([`9de2e61`](https://github.com/SemanticWebLanguageServer/swls/commit/9de2e6140c68f374c9dbc0c981647f71dd26d26d))
    - Prune dead features, dependencies, and the swls-token-helpers crate ([`53cc415`](https://github.com/SemanticWebLanguageServer/swls/commit/53cc4155da52b7054793c81009832fba55c3e2fb))
    - Release swls-core v0.1.4, swls-lang-rdf-base v0.1.4, swls-lang-turtle v0.1.4, swls-lang-jsonld v0.1.6, swls-lang-sparql v0.1.5, swls-lang-trig v0.1.4, swls v0.3.0 ([`df8a0b7`](https://github.com/SemanticWebLanguageServer/swls/commit/df8a0b7b8223d33cc7547692f7cb9040636584ab))
    - Gate features with configuration ([`faad6d5`](https://github.com/SemanticWebLanguageServer/swls/commit/faad6d5d3e8023269a265c58352644e590bc47f2))
    - Improve rename robustness ([`5085b23`](https://github.com/SemanticWebLanguageServer/swls/commit/5085b23f3eb4309daf5d6b13853cfb89facae3d6))
    - Remove unused systems ([`3bbf1cd`](https://github.com/SemanticWebLanguageServer/swls/commit/3bbf1cd80df9ff26b109c7e30da844cbc262d66e))
    - Inline and extract blank nodes ([`71381b4`](https://github.com/SemanticWebLanguageServer/swls/commit/71381b48710c992a6233aa080af53730d2657040))
    - Better timing on the validation to reduce race conditions ([`1292fb8`](https://github.com/SemanticWebLanguageServer/swls/commit/1292fb8ccbdb3b10f01a7eb9e6bc64ccfcbda9dc))
    - Diagnostics after on change things works way better ([`dc8482d`](https://github.com/SemanticWebLanguageServer/swls/commit/dc8482d1c433868c18ec54017d4bc2b12776592c))
    - Nested json-ld objects and arrays ([`00bfb2d`](https://github.com/SemanticWebLanguageServer/swls/commit/00bfb2dc81f0db86b331124e3c0a7856cb5d65da))
</details>

## 0.1.4 (2026-06-22)

### New Features

 - <csr-id-bb4f08bed28af56f776d32f787a459c7325ec47e/> gate features with configuration
 - <csr-id-ece087810771449d6e3e9badcc21b123af613879/> inline and extract blank nodes

### Bug Fixes

 - <csr-id-3604321f63b609c0095e507c094925d0d49894e5/> improve rename robustness
 - <csr-id-91a3739936dfb66c688d161c5264c997020abc86/> remove unused systems
 - <csr-id-aeb99acaba4869abdf8c7b8608b48c6ff91e0149/> better timing on the validation to reduce race conditions
 - <csr-id-e52d0f65cef812ffeab54b7d110045ce4c74f741/> diagnostics after on change things works way better
 - <csr-id-1b470e19be4b2d8639c2212eaa995dd5face19b8/> nested json-ld objects and arrays

## 0.1.3 (2026-05-20)

### New Features

 - <csr-id-9b17ed366f37598da0a9747dc51d552bee891ded/> better highlighting

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 3 commits contributed to the release.
 - 19 days passed between releases.
 - 1 commit was understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Release swls-core v0.1.3, swls-lang-rdf-base v0.1.3, swls-lang-jsonld v0.1.5, swls-lang-sparql v0.1.4, swls-lang-trig v0.1.3, swls v0.2.2 ([`0d1c8c5`](https://github.com/SemanticWebLanguageServer/swls/commit/0d1c8c52d0b7741321109ad22f1f16d53e4f8dc6))
    - Adjusting changelogs prior to release of swls-core v0.1.3, swls-lang-rdf-base v0.1.3, swls-lang-jsonld v0.1.5, swls-lang-sparql v0.1.4, swls-lang-trig v0.1.3, swls v0.2.2 ([`4f3e731`](https://github.com/SemanticWebLanguageServer/swls/commit/4f3e731b0301e0b689bfe15e790ad4706a3c84e1))
    - Better highlighting ([`9b17ed3`](https://github.com/SemanticWebLanguageServer/swls/commit/9b17ed366f37598da0a9747dc51d552bee891ded))
</details>

## 0.1.2 (2026-04-30)

### Documentation

 - <csr-id-682af7ba71d4e99d0f9516494fcd7ef552232f4d/> point crates to main readme + update readme

### New Features

 - <csr-id-9e7d856a0991850c9fd51981a2555b4bdde9cb57/> move logging to lsp logging + actually spawn local

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 4 commits contributed to the release.
 - 2 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Release swls-lov v0.1.2, swls-core v0.1.2, swls-lang-rdf-base v0.1.2, swls-lang-turtle v0.1.3, swls-lang-jsonld v0.1.3, swls-lang-sparql v0.1.3, swls-lang-trig v0.1.2, swls v0.2.0 ([`bfde48f`](https://github.com/SemanticWebLanguageServer/swls/commit/bfde48f836e70a9b3f08230e2d84c957eb5a72b0))
    - Release swls-lov v0.1.2, swls-core v0.1.2, swls-lang-rdf-base v0.1.2, swls-lang-turtle v0.1.3, swls-lang-jsonld v0.1.3, swls-lang-sparql v0.1.3, swls-lang-trig v0.1.2, swls v0.2.0 ([`eb52296`](https://github.com/SemanticWebLanguageServer/swls/commit/eb52296d24ca7c04061acc584a3423b4213cb2ee))
    - Point crates to main readme + update readme ([`682af7b`](https://github.com/SemanticWebLanguageServer/swls/commit/682af7ba71d4e99d0f9516494fcd7ef552232f4d))
    - Move logging to lsp logging + actually spawn local ([`9e7d856`](https://github.com/SemanticWebLanguageServer/swls/commit/9e7d856a0991850c9fd51981a2555b4bdde9cb57))
</details>

## 0.1.1 (2026-04-30)

<csr-id-e25ab809dad46b143491636b715326c355b06d10/>
<csr-id-ce696b31e10c73e7a42c4427bc984876241a7a1b/>

### Other

 - <csr-id-e25ab809dad46b143491636b715326c355b06d10/> fix workspace all target build + remove linker in .cargo/config.toml

### Chore

 - <csr-id-ce696b31e10c73e7a42c4427bc984876241a7a1b/> update changelogs

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 4 commits contributed to the release over the course of 1 calendar day.
 - 1 day passed between releases.
 - 2 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Release swls-lang-rdf-base v0.1.1, swls-lang-turtle v0.1.2, swls-lang-jsonld v0.1.2, swls-lang-sparql v0.1.2, swls-lang-trig v0.1.1, swls v0.1.2 ([`0c5da41`](https://github.com/SemanticWebLanguageServer/swls/commit/0c5da4135508ba9e6ccece8a14655c2c0a3e3682))
    - Update changelogs ([`ce696b3`](https://github.com/SemanticWebLanguageServer/swls/commit/ce696b31e10c73e7a42c4427bc984876241a7a1b))
    - Fix workspace all target build + remove linker in .cargo/config.toml ([`e25ab80`](https://github.com/SemanticWebLanguageServer/swls/commit/e25ab809dad46b143491636b715326c355b06d10))
    - Release swls-lang-turtle v0.1.1, swls-lang-jsonld v0.1.1, swls-lang-sparql v0.1.1, swls-lang-trig v0.1.0, swls v0.1.1 ([`6647bba`](https://github.com/SemanticWebLanguageServer/swls/commit/6647bba0c2e67c5978cd09f59ecf48ed2ec3847a))
</details>

## v0.1.0 (2026-04-28)

### New Features

 - <csr-id-ade12adaca2ba8bf197b3aaf59091ee9ec266687/> add better jsonld support
 - <csr-id-aa9acc8a6c0565ef86b54545222717e00760ac00/> fix many bugs like highlighting, json-ld, autocompletion when not in a token etc

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 4 commits contributed to the release over the course of 18 calendar days.
 - 2 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Release swls-lang-rdf-base v0.1.0, swls-lang-turtle v0.1.1, swls-lang-jsonld v0.1.1, swls-lang-sparql v0.1.1, swls-lang-trig v0.1.0, swls v0.1.1 ([`3faf76b`](https://github.com/SemanticWebLanguageServer/swls/commit/3faf76b8fe7d6ebc11193368cc65ae1ae4b4b61f))
    - Release swls-lov v0.1.1, swls-core v0.1.1, components-rs v0.1.2, swls-lang-rdf-base v0.1.0, swls-lang-turtle v0.1.1, swls-lang-jsonld v0.1.1, swls-lang-sparql v0.1.1, swls-lang-trig v0.1.0, swls v0.1.1 ([`612f47d`](https://github.com/SemanticWebLanguageServer/swls/commit/612f47d34fbde7ad5025165735b0c19eb6d6b203))
    - Add better jsonld support ([`ade12ad`](https://github.com/SemanticWebLanguageServer/swls/commit/ade12adaca2ba8bf197b3aaf59091ee9ec266687))
    - Fix many bugs like highlighting, json-ld, autocompletion when not in a token etc ([`aa9acc8`](https://github.com/SemanticWebLanguageServer/swls/commit/aa9acc8a6c0565ef86b54545222717e00760ac00))
</details>

