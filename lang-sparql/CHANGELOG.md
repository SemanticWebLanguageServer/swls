# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### Other

 - <csr-id-e25ab809dad46b143491636b715326c355b06d10/> fix workspace all target build + remove linker in .cargo/config.toml

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 1 commit contributed to the release over the course of 1 calendar day.
 - 1 day passed between releases.
 - 1 commit was understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Fix workspace all target build + remove linker in .cargo/config.toml ([`e25ab80`](https://github.com/SemanticWebLanguageServer/swls/commit/e25ab809dad46b143491636b715326c355b06d10))
</details>

## v0.1.1 (2026-04-28)

### New Features

 - <csr-id-37ca76298f9b43001bdcc6f9096a8611b8559128/> bump bevy_ecs to 0.18!
 - <csr-id-44cb452ffa0417562a5d26c863981419b56df701/> move components-rs to Url
 - <csr-id-ade12adaca2ba8bf197b3aaf59091ee9ec266687/> add better jsonld support
 - <csr-id-a47bca07a0b3d01fbd411868bb7308cf9802bcf5/> things
 - <csr-id-7c78bd43551a5afcdaaa8bef83fe9fd9ecd4d443/> make jsonld context resolve with local contexts
 - <csr-id-aa9acc8a6c0565ef86b54545222717e00760ac00/> fix many bugs like highlighting, json-ld, autocompletion when not in a token etc
 - <csr-id-5157d404a94386a17c74ea125a6d8809412d10ac/> improve error spans
 - <csr-id-dc45e8187ffa2cd0a6d6aec35e7477802b8bcd88/> use new parser for turtle and sparql

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 12 commits contributed to the release over the course of 26 calendar days.
 - 8 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Release swls-lang-turtle v0.1.1, swls-lang-jsonld v0.1.1, swls-lang-sparql v0.1.1, swls-lang-trig v0.1.0, swls v0.1.1 ([`6647bba`](https://github.com/SemanticWebLanguageServer/swls/commit/6647bba0c2e67c5978cd09f59ecf48ed2ec3847a))
    - Release swls-lang-rdf-base v0.1.0, swls-lang-turtle v0.1.1, swls-lang-jsonld v0.1.1, swls-lang-sparql v0.1.1, swls-lang-trig v0.1.0, swls v0.1.1 ([`3faf76b`](https://github.com/SemanticWebLanguageServer/swls/commit/3faf76b8fe7d6ebc11193368cc65ae1ae4b4b61f))
    - Release swls-lov v0.1.1, swls-core v0.1.1, components-rs v0.1.2, swls-lang-rdf-base v0.1.0, swls-lang-turtle v0.1.1, swls-lang-jsonld v0.1.1, swls-lang-sparql v0.1.1, swls-lang-trig v0.1.0, swls v0.1.1 ([`612f47d`](https://github.com/SemanticWebLanguageServer/swls/commit/612f47d34fbde7ad5025165735b0c19eb6d6b203))
    - Bump bevy_ecs to 0.18! ([`37ca762`](https://github.com/SemanticWebLanguageServer/swls/commit/37ca76298f9b43001bdcc6f9096a8611b8559128))
    - Move components-rs to Url ([`44cb452`](https://github.com/SemanticWebLanguageServer/swls/commit/44cb452ffa0417562a5d26c863981419b56df701))
    - Add better jsonld support ([`ade12ad`](https://github.com/SemanticWebLanguageServer/swls/commit/ade12adaca2ba8bf197b3aaf59091ee9ec266687))
    - Things ([`a47bca0`](https://github.com/SemanticWebLanguageServer/swls/commit/a47bca07a0b3d01fbd411868bb7308cf9802bcf5))
    - Make jsonld context resolve with local contexts ([`7c78bd4`](https://github.com/SemanticWebLanguageServer/swls/commit/7c78bd43551a5afcdaaa8bef83fe9fd9ecd4d443))
    - Fix many bugs like highlighting, json-ld, autocompletion when not in a token etc ([`aa9acc8`](https://github.com/SemanticWebLanguageServer/swls/commit/aa9acc8a6c0565ef86b54545222717e00760ac00))
    - Improve error spans ([`5157d40`](https://github.com/SemanticWebLanguageServer/swls/commit/5157d404a94386a17c74ea125a6d8809412d10ac))
    - Remove logos/chumsky tokenizers; replace with CST-based token production ([`da53ead`](https://github.com/SemanticWebLanguageServer/swls/commit/da53eadb2b042a3d9c8b4d1d28d190e2ef1462f2))
    - Use new parser for turtle and sparql ([`dc45e81`](https://github.com/SemanticWebLanguageServer/swls/commit/dc45e8187ffa2cd0a6d6aec35e7477802b8bcd88))
</details>

## v0.1.0 (2026-03-23)

### New Features

 - <csr-id-721d151c1dbcaccab3a81f81f7d59f2517fe323c/> rename before publish
 - <csr-id-cf54f0bfd210e1b01311683399a70f8d7ae157e7/> optimize logging
 - <csr-id-799356a263277b24507115aa1574b5d0c1f7303a/> claude refactor
 - <csr-id-27b8c86e7d11ebf2faee236c03b360e37c024e28/> add file based configs + order prefix completions based on popularity

### Bug Fixes

 - <csr-id-29ef7e453047b405fe76ff846b8a0bf230489bef/> make cargo release happy with version numbers

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 40 commits contributed to the release over the course of 425 calendar days.
 - 5 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Make cargo release happy with version numbers ([`29ef7e4`](https://github.com/SemanticWebLanguageServer/swls/commit/29ef7e453047b405fe76ff846b8a0bf230489bef))
    - Rename before publish ([`721d151`](https://github.com/SemanticWebLanguageServer/swls/commit/721d151c1dbcaccab3a81f81f7d59f2517fe323c))
    - Optimize logging ([`cf54f0b`](https://github.com/SemanticWebLanguageServer/swls/commit/cf54f0bfd210e1b01311683399a70f8d7ae157e7))
    - Claude refactor ([`799356a`](https://github.com/SemanticWebLanguageServer/swls/commit/799356a263277b24507115aa1574b5d0c1f7303a))
    - Try to find and fix broken blank node parsing ([`0cc6538`](https://github.com/SemanticWebLanguageServer/swls/commit/0cc65382a738f79d3c673aa451feea229836bfdc))
    - Readd shapes ([`c5dd419`](https://github.com/SemanticWebLanguageServer/swls/commit/c5dd41900a6acac67e5be88966291f24781a44cf))
    - Improve prefix.cc completion ([`20151a1`](https://github.com/SemanticWebLanguageServer/swls/commit/20151a12f96cc9432ee567ce4c9c01ef195bea33))
    - Derive properties and classes with sparql queries ([`482f1d8`](https://github.com/SemanticWebLanguageServer/swls/commit/482f1d8a9ebb5bbdf0ef628edf7cfa5b8f160971))
    - Fix tests ([`27e906f`](https://github.com/SemanticWebLanguageServer/swls/commit/27e906fb1bd395ebb3a66bd5d9067f187208cda0))
    - Update dependencies ([`cec99b9`](https://github.com/SemanticWebLanguageServer/swls/commit/cec99b9c7028b3556e68017c8f67c2cb2af75057))
    - Read extra ontology files as ontologies ([`675a559`](https://github.com/SemanticWebLanguageServer/swls/commit/675a5591d85cbac7a84bdb172b79e62fb362bc0c))
    - Add file based configs + order prefix completions based on popularity ([`27b8c86`](https://github.com/SemanticWebLanguageServer/swls/commit/27b8c86e7d11ebf2faee236c03b360e37c024e28))
    - Remove unused dependencies ([`033dd71`](https://github.com/SemanticWebLanguageServer/swls/commit/033dd718488003e1149f6af6182799d99e8bf5b9))
    - Configure which languages to activate ([`5fe3e6f`](https://github.com/SemanticWebLanguageServer/swls/commit/5fe3e6fe4dd91948bfc2bc6584eb6716e41946a4))
    - Remove logs + add context to sparql + fix race condition in app ([`6770fbf`](https://github.com/SemanticWebLanguageServer/swls/commit/6770fbf9b1ddcc044072c176e763a8f671d71e09))
    - Cleanup ([`8a47a76`](https://github.com/SemanticWebLanguageServer/swls/commit/8a47a7687000fda05d3ea9f11b99283e9d03e727))
    - Move to logos lexer for sparql ([`d99e598`](https://github.com/SemanticWebLanguageServer/swls/commit/d99e598a7fc1344282b5c6a8a7dceec5213ada50))
    - Parse turtle with previous token information (subject or predicate or object) ([`02a28f0`](https://github.com/SemanticWebLanguageServer/swls/commit/02a28f0215c81cdef7bb1798c34398fbdf56b74b))
    - Remember turtle diffs ([`0087222`](https://github.com/SemanticWebLanguageServer/swls/commit/00872223ae341724739c12853a452719bb6f284c))
    - Fix tests ([`87af94f`](https://github.com/SemanticWebLanguageServer/swls/commit/87af94fc0a10aa789a75cf7b44673951c4bb0a57))
    - Refactor + add documentation ([`f45a56d`](https://github.com/SemanticWebLanguageServer/swls/commit/f45a56d88c5dabca1889b1609ea5d49bb8675869))
    - Cargo +nightly fmt ([`d4b3573`](https://github.com/SemanticWebLanguageServer/swls/commit/d4b357369b476ba6c55ce4e934900f5cac43f4dd))
    - Refactor sparql ([`57a5dd1`](https://github.com/SemanticWebLanguageServer/swls/commit/57a5dd1890000eeac42cfd2bb675e895f0437fde))
    - Refactor lang-turtle ([`50cc908`](https://github.com/SemanticWebLanguageServer/swls/commit/50cc9083f1a76b6b2ed9181705d9dd76d1a4de9d))
    - Use refactor sparql ([`678b05f`](https://github.com/SemanticWebLanguageServer/swls/commit/678b05f24e192954ed81db47ab5d90f6cc8d9820))
    - Cargo fmt ([`92ff670`](https://github.com/SemanticWebLanguageServer/swls/commit/92ff67014c3461231042156fff1fe2c8617ecf07))
    - Cargo fix --lib ([`ebcef05`](https://github.com/SemanticWebLanguageServer/swls/commit/ebcef058efc5674a904d4edd5b2699e5e45232c0))
    - Refactor Spanned ([`d35e695`](https://github.com/SemanticWebLanguageServer/swls/commit/d35e695b22a2e71e4219600d0739ae51b5cdd70b))
    - Refactor completion ([`ff89762`](https://github.com/SemanticWebLanguageServer/swls/commit/ff89762c2fe89eb3ed947cd20d59c0e246eef7de))
    - Start add docs and refactor ([`2798feb`](https://github.com/SemanticWebLanguageServer/swls/commit/2798feb2d43106eebb00e1054f6931cda06c22a7))
    - Add rename ([`18937ec`](https://github.com/SemanticWebLanguageServer/swls/commit/18937ec14cbd1186ab4e585b9f398c548371aef9))
    - Add keyboard shortcuts to demo ([`3da8dcc`](https://github.com/SemanticWebLanguageServer/swls/commit/3da8dcc5c28de14aa1818bb464b223c8dca96563))
    - Add prefix.cc completion ([`d5f46bc`](https://github.com/SemanticWebLanguageServer/swls/commit/d5f46bc03e7f8c0c05239eb39151e71991a2634b))
    - Rework frontend ([`87d405c`](https://github.com/SemanticWebLanguageServer/swls/commit/87d405c6079dedf3f8921fe1fa37777825d25dd4))
    - Move to cargo make, move lsp-bin/src/backend.rs to core/src/backend.rs, add wasm compile ([`5ef8c77`](https://github.com/SemanticWebLanguageServer/swls/commit/5ef8c777c59bf4eb78f809ae8dcbe818942f9882))
    - Add types on hover ([`112ea76`](https://github.com/SemanticWebLanguageServer/swls/commit/112ea76a807d22c0dfc77ca75d6c7926238e33cc))
    - Add things ([`e54f7bc`](https://github.com/SemanticWebLanguageServer/swls/commit/e54f7bc5e7470a9c506dd8b263d12d63329a8a1b))
    - Fix some bugs and better derive triples ([`e04ab58`](https://github.com/SemanticWebLanguageServer/swls/commit/e04ab588d0f2cc90b8b2bd003b050fd52b7cb08b))
    - Add sparql-lang triples ([`1718322`](https://github.com/SemanticWebLanguageServer/swls/commit/171832265be634eb7b239b8a8ba6786b9bca57d8))
    - Start sparql ([`865fcdc`](https://github.com/SemanticWebLanguageServer/swls/commit/865fcdcb3c6247613585316e0ad849b8a08eb229))
</details>

