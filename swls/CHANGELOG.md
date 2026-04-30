# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 0.1.2 (2026-04-30)

### Chore

 - <csr-id-ce696b31e10c73e7a42c4427bc984876241a7a1b/> update changelogs

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 1 commit contributed to the release.
 - 1 day passed between releases.
 - 1 commit was understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
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

