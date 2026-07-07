# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 0.4.1 (2026-07-07)

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

 - 1 commit contributed to the release.
 - 7 days passed between releases.
 - 0 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Adjusting changelogs prior to release of swls-core v0.2.1, swls-lang-rdf-base v0.2.1, swls-lang-turtle v0.2.1, swls-lang-jsonld v0.2.1, swls-lang-n3 v0.1.1, swls-lang-sparql v0.2.1, swls-lang-trig v0.2.1, swls v0.4.1 ([`2f1e550`](https://github.com/SemanticWebLanguageServer/swls/commit/2f1e5503bae9428b76613ab8b110700234569e1e))
</details>

## 0.4.0 (2026-06-29)

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

 - <csr-id-12b14f9f8b92f0163b3e0e25842df707bcf1e785/> add automatic insert of prefix statements when writing colon
 - <csr-id-e76bb7f72a54acfdd3b6ed56687a51930f0c12e6/> add undefined iri's warnings
 - <csr-id-529e4707a83e95733a0b3a470e945122aee8c4b3/> actually add swls/src/server.rs
 - <csr-id-91525d7d08b5f1d52f0ce4096e39bd844d082ded/> move tower_lsp to swls binary

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 9 commits contributed to the release over the course of 7 calendar days.
 - 40 days passed between releases.
 - 5 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 1 unique issue was worked on: [#32](https://github.com/SemanticWebLanguageServer/swls/issues/32)

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **[#32](https://github.com/SemanticWebLanguageServer/swls/issues/32)**
    - Refactor/replace ropey lineindex ([`81148b6`](https://github.com/SemanticWebLanguageServer/swls/commit/81148b64d51d9399a6d8d76b8f6c5114b90450af))
 * **Uncategorized**
    - Release swls-core v0.2.0, swls-lang-rdf-base v0.2.0, swls-lang-turtle v0.2.0, swls-lang-jsonld v0.2.0, swls-lang-n3 v0.1.0, swls-lang-sparql v0.2.0, swls-lang-trig v0.2.0, swls v0.4.0 ([`9de2e61`](https://github.com/SemanticWebLanguageServer/swls/commit/9de2e6140c68f374c9dbc0c981647f71dd26d26d))
    - Prune dead features, dependencies, and the swls-token-helpers crate ([`53cc415`](https://github.com/SemanticWebLanguageServer/swls/commit/53cc4155da52b7054793c81009832fba55c3e2fb))
    - Feat: add lang n3 feat: update prefix diagnostics feat: add format settings ([`c0e3888`](https://github.com/SemanticWebLanguageServer/swls/commit/c0e3888d726463bffd360a3758a33e4df7aa9b02))
    - Release swls-core v0.1.4, swls-lang-rdf-base v0.1.4, swls-lang-turtle v0.1.4, swls-lang-jsonld v0.1.6, swls-lang-sparql v0.1.5, swls-lang-trig v0.1.4, swls v0.3.0 ([`df8a0b7`](https://github.com/SemanticWebLanguageServer/swls/commit/df8a0b7b8223d33cc7547692f7cb9040636584ab))
    - Add automatic insert of prefix statements when writing colon ([`12b14f9`](https://github.com/SemanticWebLanguageServer/swls/commit/12b14f9f8b92f0163b3e0e25842df707bcf1e785))
    - Add undefined iri's warnings ([`e76bb7f`](https://github.com/SemanticWebLanguageServer/swls/commit/e76bb7f72a54acfdd3b6ed56687a51930f0c12e6))
    - Actually add swls/src/server.rs ([`529e470`](https://github.com/SemanticWebLanguageServer/swls/commit/529e4707a83e95733a0b3a470e945122aee8c4b3))
    - Move tower_lsp to swls binary ([`91525d7`](https://github.com/SemanticWebLanguageServer/swls/commit/91525d7d08b5f1d52f0ce4096e39bd844d082ded))
</details>

## 0.3.0 (2026-06-22)

### New Features

 - <csr-id-4bad0c7033f9029315e831d5801922f959a64165/> add automatic insert of prefix statements when writing colon
 - <csr-id-5daeb7fab3c033983ddb34cb6c0518eafcd0cbc1/> add undefined iri's warnings
 - <csr-id-a066345ac46f16e951eea8ff3439761d1d261459/> actually add swls/src/server.rs
 - <csr-id-91c1ad0150e44713d3589bb264d704a4656e4a8a/> move tower_lsp to swls binary

## 0.2.2 (2026-05-20)

### New Features

* use lov-mirror on github for better response times
* better highlighting for JSON-LD and Trig

### Chore

* bump rdf-parsers version (better SPARQL parsing)


### Commit Statistics

<csr-read-only-do-not-edit/>

 - 2 commits contributed to the release.
 - 9 days passed between releases.
 - 0 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Release swls-core v0.1.3, swls-lang-rdf-base v0.1.3, swls-lang-jsonld v0.1.5, swls-lang-sparql v0.1.4, swls-lang-trig v0.1.3, swls v0.2.2 ([`0d1c8c5`](https://github.com/SemanticWebLanguageServer/swls/commit/0d1c8c52d0b7741321109ad22f1f16d53e4f8dc6))
    - Adjusting changelogs prior to release of swls-core v0.1.3, swls-lang-rdf-base v0.1.3, swls-lang-jsonld v0.1.5, swls-lang-sparql v0.1.4, swls-lang-trig v0.1.3, swls v0.2.2 ([`4f3e731`](https://github.com/SemanticWebLanguageServer/swls/commit/4f3e731b0301e0b689bfe15e790ad4706a3c84e1))
</details>

## 0.2.1 (2026-05-11)

### Chore

- Update rdf-parsers version as it introduced an error

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 2 commits contributed to the release.
 - 10 days passed between releases.
 - 0 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Release swls-lang-jsonld v0.1.4, swls v0.2.1 ([`46a0398`](https://github.com/SemanticWebLanguageServer/swls/commit/46a0398769db74c9b1a51f27120d5daf21fbff15))
    - Adjusting changelogs prior to release of swls-lang-jsonld v0.1.4, swls v0.2.1 ([`e288690`](https://github.com/SemanticWebLanguageServer/swls/commit/e288690473bd8b1a81f67279ffa9772000d0b10c))
</details>

## 0.2.0 (2026-04-30)

<csr-id-4010646878a90b97a51b54bb0fb7b0d07aed3269/>

### Chore

 - <csr-id-4010646878a90b97a51b54bb0fb7b0d07aed3269/> bump rdf-parsers version

### Documentation

 - <csr-id-682af7ba71d4e99d0f9516494fcd7ef552232f4d/> point crates to main readme + update readme

### New Features

 - <csr-id-9e7d856a0991850c9fd51981a2555b4bdde9cb57/> move logging to lsp logging + actually spawn local

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 5 commits contributed to the release.
 - 3 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Bump rdf-parsers version ([`4010646`](https://github.com/SemanticWebLanguageServer/swls/commit/4010646878a90b97a51b54bb0fb7b0d07aed3269))
    - Release swls-lov v0.1.2, swls-core v0.1.2, swls-lang-rdf-base v0.1.2, swls-lang-turtle v0.1.3, swls-lang-jsonld v0.1.3, swls-lang-sparql v0.1.3, swls-lang-trig v0.1.2, swls v0.2.0 ([`bfde48f`](https://github.com/SemanticWebLanguageServer/swls/commit/bfde48f836e70a9b3f08230e2d84c957eb5a72b0))
    - Release swls-lov v0.1.2, swls-core v0.1.2, swls-lang-rdf-base v0.1.2, swls-lang-turtle v0.1.3, swls-lang-jsonld v0.1.3, swls-lang-sparql v0.1.3, swls-lang-trig v0.1.2, swls v0.2.0 ([`eb52296`](https://github.com/SemanticWebLanguageServer/swls/commit/eb52296d24ca7c04061acc584a3423b4213cb2ee))
    - Point crates to main readme + update readme ([`682af7b`](https://github.com/SemanticWebLanguageServer/swls/commit/682af7ba71d4e99d0f9516494fcd7ef552232f4d))
    - Move logging to lsp logging + actually spawn local ([`9e7d856`](https://github.com/SemanticWebLanguageServer/swls/commit/9e7d856a0991850c9fd51981a2555b4bdde9cb57))
</details>

## 0.1.2 (2026-04-30)

<csr-id-ce696b31e10c73e7a42c4427bc984876241a7a1b/>

### Chore

 - <csr-id-ce696b31e10c73e7a42c4427bc984876241a7a1b/> update changelogs

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 2 commits contributed to the release.
 - 1 day passed between releases.
 - 1 commit was understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Release swls-lang-rdf-base v0.1.1, swls-lang-turtle v0.1.2, swls-lang-jsonld v0.1.2, swls-lang-sparql v0.1.2, swls-lang-trig v0.1.1, swls v0.1.2 ([`0c5da41`](https://github.com/SemanticWebLanguageServer/swls/commit/0c5da4135508ba9e6ccece8a14655c2c0a3e3682))
    - Update changelogs ([`ce696b3`](https://github.com/SemanticWebLanguageServer/swls/commit/ce696b31e10c73e7a42c4427bc984876241a7a1b))
</details>

## v0.1.1 (2026-04-29)

<csr-id-4debbca4c3e1839781b7728951f91f2ba729165b/>
<csr-id-759734f81769cf8dd9af543dcda417684b696958/>
<csr-id-ff3b60d1bc6e6575309dfeb55f22ea94ff1f187a/>

### New Features

 - <csr-id-37ca76298f9b43001bdcc6f9096a8611b8559128/> bump bevy_ecs to 0.18!
 - <csr-id-35ff9925b3957c491e3798a62ae4e4e32f88d40e/> make cjs work for vscode
 - <csr-id-478139e705d9f72c1ad9e8df5228379afd2b5b7f/> remove PathBuf from read dir
 - <csr-id-44cb452ffa0417562a5d26c863981419b56df701/> move components-rs to Url
 - <csr-id-663af2958384584b8111e41a3131fb55984b71bf/> fix many minor mistakes
 - <csr-id-9a887c510c8740c9d9f01a27f88aeca533a69cf3/> incorporate cjs, but we cannot yet parse the IRIs
 - <csr-id-ade12adaca2ba8bf197b3aaf59091ee9ec266687/> add better jsonld support
 - <csr-id-aa9acc8a6c0565ef86b54545222717e00760ac00/> fix many bugs like highlighting, json-ld, autocompletion when not in a token etc
 - <csr-id-5157d404a94386a17c74ea125a6d8809412d10ac/> improve error spans
 - <csr-id-dc45e8187ffa2cd0a6d6aec35e7477802b8bcd88/> use new parser for turtle and sparql

### Other

 - <csr-id-4debbca4c3e1839781b7728951f91f2ba729165b/> fix jsonld goto definition for the last time!

### Other

 - <csr-id-759734f81769cf8dd9af543dcda417684b696958/> try to fix windows linking again
 - <csr-id-ff3b60d1bc6e6575309dfeb55f22ea94ff1f187a/> fix linking issue on windows ci + add release metadata to build

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 17 commits contributed to the release over the course of 27 calendar days.
 - 13 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Try to fix windows linking again ([`759734f`](https://github.com/SemanticWebLanguageServer/swls/commit/759734f81769cf8dd9af543dcda417684b696958))
    - Fix linking issue on windows ci + add release metadata to build ([`ff3b60d`](https://github.com/SemanticWebLanguageServer/swls/commit/ff3b60d1bc6e6575309dfeb55f22ea94ff1f187a))
    - Release swls-lang-turtle v0.1.1, swls-lang-jsonld v0.1.1, swls-lang-sparql v0.1.1, swls-lang-trig v0.1.0, swls v0.1.1 ([`6647bba`](https://github.com/SemanticWebLanguageServer/swls/commit/6647bba0c2e67c5978cd09f59ecf48ed2ec3847a))
    - Release swls-lang-rdf-base v0.1.0, swls-lang-turtle v0.1.1, swls-lang-jsonld v0.1.1, swls-lang-sparql v0.1.1, swls-lang-trig v0.1.0, swls v0.1.1 ([`3faf76b`](https://github.com/SemanticWebLanguageServer/swls/commit/3faf76b8fe7d6ebc11193368cc65ae1ae4b4b61f))
    - Release swls-lov v0.1.1, swls-core v0.1.1, components-rs v0.1.2, swls-lang-rdf-base v0.1.0, swls-lang-turtle v0.1.1, swls-lang-jsonld v0.1.1, swls-lang-sparql v0.1.1, swls-lang-trig v0.1.0, swls v0.1.1 ([`612f47d`](https://github.com/SemanticWebLanguageServer/swls/commit/612f47d34fbde7ad5025165735b0c19eb6d6b203))
    - Bump bevy_ecs to 0.18! ([`37ca762`](https://github.com/SemanticWebLanguageServer/swls/commit/37ca76298f9b43001bdcc6f9096a8611b8559128))
    - Make cjs work for vscode ([`35ff992`](https://github.com/SemanticWebLanguageServer/swls/commit/35ff9925b3957c491e3798a62ae4e4e32f88d40e))
    - Remove PathBuf from read dir ([`478139e`](https://github.com/SemanticWebLanguageServer/swls/commit/478139e705d9f72c1ad9e8df5228379afd2b5b7f))
    - Fix jsonld goto definition for the last time! ([`4debbca`](https://github.com/SemanticWebLanguageServer/swls/commit/4debbca4c3e1839781b7728951f91f2ba729165b))
    - Move components-rs to Url ([`44cb452`](https://github.com/SemanticWebLanguageServer/swls/commit/44cb452ffa0417562a5d26c863981419b56df701))
    - Fix many minor mistakes ([`663af29`](https://github.com/SemanticWebLanguageServer/swls/commit/663af2958384584b8111e41a3131fb55984b71bf))
    - Incorporate cjs, but we cannot yet parse the IRIs ([`9a887c5`](https://github.com/SemanticWebLanguageServer/swls/commit/9a887c510c8740c9d9f01a27f88aeca533a69cf3))
    - Add better jsonld support ([`ade12ad`](https://github.com/SemanticWebLanguageServer/swls/commit/ade12adaca2ba8bf197b3aaf59091ee9ec266687))
    - Fix many bugs like highlighting, json-ld, autocompletion when not in a token etc ([`aa9acc8`](https://github.com/SemanticWebLanguageServer/swls/commit/aa9acc8a6c0565ef86b54545222717e00760ac00))
    - Improve error spans ([`5157d40`](https://github.com/SemanticWebLanguageServer/swls/commit/5157d404a94386a17c74ea125a6d8809412d10ac))
    - Remove logos/chumsky tokenizers; replace with CST-based token production ([`da53ead`](https://github.com/SemanticWebLanguageServer/swls/commit/da53eadb2b042a3d9c8b4d1d28d190e2ef1462f2))
    - Use new parser for turtle and sparql ([`dc45e81`](https://github.com/SemanticWebLanguageServer/swls/commit/dc45e8187ffa2cd0a6d6aec35e7477802b8bcd88))
</details>

## v0.1.0 (2026-03-23)

### New Features

 - <csr-id-721d151c1dbcaccab3a81f81f7d59f2517fe323c/> rename before publish
 - <csr-id-cf54f0bfd210e1b01311683399a70f8d7ae157e7/> optimize logging

### Bug Fixes

 - <csr-id-29ef7e453047b405fe76ff846b8a0bf230489bef/> make cargo release happy with version numbers

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 9 commits contributed to the release.
 - 3 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Make cargo release happy with version numbers ([`29ef7e4`](https://github.com/SemanticWebLanguageServer/swls/commit/29ef7e453047b405fe76ff846b8a0bf230489bef))
    - Rename before publish ([`721d151`](https://github.com/SemanticWebLanguageServer/swls/commit/721d151c1dbcaccab3a81f81f7d59f2517fe323c))
    - Optimize logging ([`cf54f0b`](https://github.com/SemanticWebLanguageServer/swls/commit/cf54f0bfd210e1b01311683399a70f8d7ae157e7))
    - Derive properties and classes with sparql queries ([`482f1d8`](https://github.com/SemanticWebLanguageServer/swls/commit/482f1d8a9ebb5bbdf0ef628edf7cfa5b8f160971))
    - Update dependencies ([`cec99b9`](https://github.com/SemanticWebLanguageServer/swls/commit/cec99b9c7028b3556e68017c8f67c2cb2af75057))
    - Allow glob ontologies ([`8933499`](https://github.com/SemanticWebLanguageServer/swls/commit/8933499ce8cb504a29e5fd8864998c9292655a8f))
    - Read extra ontology files as ontologies ([`675a559`](https://github.com/SemanticWebLanguageServer/swls/commit/675a5591d85cbac7a84bdb172b79e62fb362bc0c))
    - Remove unused dependencies ([`033dd71`](https://github.com/SemanticWebLanguageServer/swls/commit/033dd718488003e1149f6af6182799d99e8bf5b9))
    - Move lsp-bin to swls ([`2c3ccc0`](https://github.com/SemanticWebLanguageServer/swls/commit/2c3ccc07ea7e71459e960c65c809c99b538d9569))
</details>

