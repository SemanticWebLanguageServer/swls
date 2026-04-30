# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 0.1.2 (2026-04-30)

<csr-id-e25ab809dad46b143491636b715326c355b06d10/>

### Other

 - <csr-id-e25ab809dad46b143491636b715326c355b06d10/> fix workspace all target build + remove linker in .cargo/config.toml

### Chore

 - <csr-id-ce696b31e10c73e7a42c4427bc984876241a7a1b/> update changelogs

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 2 commits contributed to the release over the course of 1 calendar day.
 - 1 day passed between releases.
 - 2 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Update changelogs ([`ce696b3`](https://github.com/SemanticWebLanguageServer/swls/commit/ce696b31e10c73e7a42c4427bc984876241a7a1b))
    - Fix workspace all target build + remove linker in .cargo/config.toml ([`e25ab80`](https://github.com/SemanticWebLanguageServer/swls/commit/e25ab809dad46b143491636b715326c355b06d10))
</details>

## v0.1.1 (2026-04-28)

<csr-id-4debbca4c3e1839781b7728951f91f2ba729165b/>

### New Features

 - <csr-id-37ca76298f9b43001bdcc6f9096a8611b8559128/> bump bevy_ecs to 0.18!
 - <csr-id-35ff9925b3957c491e3798a62ae4e4e32f88d40e/> make cjs work for vscode
 - <csr-id-478139e705d9f72c1ad9e8df5228379afd2b5b7f/> remove PathBuf from read dir
 - <csr-id-44cb452ffa0417562a5d26c863981419b56df701/> move components-rs to Url
 - <csr-id-40ea012256eb5e3b768903ae5c92f7882133aa6e/> fix many minor mistakes
 - <csr-id-663af2958384584b8111e41a3131fb55984b71bf/> fix many minor mistakes
 - <csr-id-987907ef012c3d41a094a3b7bf853ed0a55d23a8/> bump
 - <csr-id-97441d4d9dc7778d3fcb6b0862ad0d61fa38b01f/> check if cjs to properties and classes is a good idea
 - <csr-id-5ec44b5d4c325bb6687bf70292c29b74d4770ded/> goto @context string
 - <csr-id-eb8a99d9b4049e72c60ebd47eded01e6720c5632/> reparse when cjs is loaded
 - <csr-id-f2a1679be0c5261e2705335eb2c2c1a14015906d/> improve cjs goto definition
 - <csr-id-1dff5c4281c42f1ad59b955a860b0ffc1ee486e2/> move components-rs into swls
 - <csr-id-9a887c510c8740c9d9f01a27f88aeca533a69cf3/> incorporate cjs, but we cannot yet parse the IRIs
 - <csr-id-ade12adaca2ba8bf197b3aaf59091ee9ec266687/> add better jsonld support
 - <csr-id-a47bca07a0b3d01fbd411868bb7308cf9802bcf5/> things
 - <csr-id-7c78bd43551a5afcdaaa8bef83fe9fd9ecd4d443/> make jsonld context resolve with local contexts
 - <csr-id-aa9acc8a6c0565ef86b54545222717e00760ac00/> fix many bugs like highlighting, json-ld, autocompletion when not in a token etc
 - <csr-id-dc45e8187ffa2cd0a6d6aec35e7477802b8bcd88/> use new parser for turtle and sparql
 - <csr-id-721d151c1dbcaccab3a81f81f7d59f2517fe323c/> rename before publish
 - <csr-id-cf54f0bfd210e1b01311683399a70f8d7ae157e7/> optimize logging

### Bug Fixes

 - <csr-id-29ef7e453047b405fe76ff846b8a0bf230489bef/> make cargo release happy with version numbers

### Other

 - <csr-id-4debbca4c3e1839781b7728951f91f2ba729165b/> fix jsonld goto definition for the last time!

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 62 commits contributed to the release.
 - 22 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Release swls-lang-turtle v0.1.1, swls-lang-jsonld v0.1.1, swls-lang-sparql v0.1.1, swls-lang-trig v0.1.0, swls v0.1.1 ([`6647bba`](https://github.com/SemanticWebLanguageServer/swls/commit/6647bba0c2e67c5978cd09f59ecf48ed2ec3847a))
    - Release swls-lang-rdf-base v0.1.0, swls-lang-turtle v0.1.1, swls-lang-jsonld v0.1.1, swls-lang-sparql v0.1.1, swls-lang-trig v0.1.0, swls v0.1.1 ([`3faf76b`](https://github.com/SemanticWebLanguageServer/swls/commit/3faf76b8fe7d6ebc11193368cc65ae1ae4b4b61f))
    - Release swls-lov v0.1.1, swls-core v0.1.1, components-rs v0.1.2, swls-lang-rdf-base v0.1.0, swls-lang-turtle v0.1.1, swls-lang-jsonld v0.1.1, swls-lang-sparql v0.1.1, swls-lang-trig v0.1.0, swls v0.1.1 ([`612f47d`](https://github.com/SemanticWebLanguageServer/swls/commit/612f47d34fbde7ad5025165735b0c19eb6d6b203))
    - Bump bevy_ecs to 0.18! ([`37ca762`](https://github.com/SemanticWebLanguageServer/swls/commit/37ca76298f9b43001bdcc6f9096a8611b8559128))
    - Make cjs work for vscode ([`35ff992`](https://github.com/SemanticWebLanguageServer/swls/commit/35ff9925b3957c491e3798a62ae4e4e32f88d40e))
    - Remove PathBuf from read dir ([`478139e`](https://github.com/SemanticWebLanguageServer/swls/commit/478139e705d9f72c1ad9e8df5228379afd2b5b7f))
    - Fix jsonld goto definition for the last time! ([`4debbca`](https://github.com/SemanticWebLanguageServer/swls/commit/4debbca4c3e1839781b7728951f91f2ba729165b))
    - Move components-rs to Url ([`44cb452`](https://github.com/SemanticWebLanguageServer/swls/commit/44cb452ffa0417562a5d26c863981419b56df701))
    - Fix many minor mistakes ([`40ea012`](https://github.com/SemanticWebLanguageServer/swls/commit/40ea012256eb5e3b768903ae5c92f7882133aa6e))
    - Fix many minor mistakes ([`663af29`](https://github.com/SemanticWebLanguageServer/swls/commit/663af2958384584b8111e41a3131fb55984b71bf))
    - Bump ([`987907e`](https://github.com/SemanticWebLanguageServer/swls/commit/987907ef012c3d41a094a3b7bf853ed0a55d23a8))
    - Check if cjs to properties and classes is a good idea ([`97441d4`](https://github.com/SemanticWebLanguageServer/swls/commit/97441d4d9dc7778d3fcb6b0862ad0d61fa38b01f))
    - Goto @context string ([`5ec44b5`](https://github.com/SemanticWebLanguageServer/swls/commit/5ec44b5d4c325bb6687bf70292c29b74d4770ded))
    - Reparse when cjs is loaded ([`eb8a99d`](https://github.com/SemanticWebLanguageServer/swls/commit/eb8a99d9b4049e72c60ebd47eded01e6720c5632))
    - Improve cjs goto definition ([`f2a1679`](https://github.com/SemanticWebLanguageServer/swls/commit/f2a1679be0c5261e2705335eb2c2c1a14015906d))
    - Move components-rs into swls ([`1dff5c4`](https://github.com/SemanticWebLanguageServer/swls/commit/1dff5c4281c42f1ad59b955a860b0ffc1ee486e2))
    - Incorporate cjs, but we cannot yet parse the IRIs ([`9a887c5`](https://github.com/SemanticWebLanguageServer/swls/commit/9a887c510c8740c9d9f01a27f88aeca533a69cf3))
    - Add better jsonld support ([`ade12ad`](https://github.com/SemanticWebLanguageServer/swls/commit/ade12adaca2ba8bf197b3aaf59091ee9ec266687))
    - Things ([`a47bca0`](https://github.com/SemanticWebLanguageServer/swls/commit/a47bca07a0b3d01fbd411868bb7308cf9802bcf5))
    - Make jsonld context resolve with local contexts ([`7c78bd4`](https://github.com/SemanticWebLanguageServer/swls/commit/7c78bd43551a5afcdaaa8bef83fe9fd9ecd4d443))
    - Fix many bugs like highlighting, json-ld, autocompletion when not in a token etc ([`aa9acc8`](https://github.com/SemanticWebLanguageServer/swls/commit/aa9acc8a6c0565ef86b54545222717e00760ac00))
    - Use new parser for turtle and sparql ([`dc45e81`](https://github.com/SemanticWebLanguageServer/swls/commit/dc45e8187ffa2cd0a6d6aec35e7477802b8bcd88))
    - Make cargo release happy with version numbers ([`29ef7e4`](https://github.com/SemanticWebLanguageServer/swls/commit/29ef7e453047b405fe76ff846b8a0bf230489bef))
    - Rename before publish ([`721d151`](https://github.com/SemanticWebLanguageServer/swls/commit/721d151c1dbcaccab3a81f81f7d59f2517fe323c))
    - Optimize logging ([`cf54f0b`](https://github.com/SemanticWebLanguageServer/swls/commit/cf54f0bfd210e1b01311683399a70f8d7ae157e7))
    - Readd shapes ([`c5dd419`](https://github.com/SemanticWebLanguageServer/swls/commit/c5dd41900a6acac67e5be88966291f24781a44cf))
    - Fix tests ([`27e906f`](https://github.com/SemanticWebLanguageServer/swls/commit/27e906fb1bd395ebb3a66bd5d9067f187208cda0))
    - Update dependencies ([`cec99b9`](https://github.com/SemanticWebLanguageServer/swls/commit/cec99b9c7028b3556e68017c8f67c2cb2af75057))
    - Remove unused dependencies ([`033dd71`](https://github.com/SemanticWebLanguageServer/swls/commit/033dd718488003e1149f6af6182799d99e8bf5b9))
    - Configure which languages to activate ([`5fe3e6f`](https://github.com/SemanticWebLanguageServer/swls/commit/5fe3e6fe4dd91948bfc2bc6584eb6716e41946a4))
    - Remove logs + add context to sparql + fix race condition in app ([`6770fbf`](https://github.com/SemanticWebLanguageServer/swls/commit/6770fbf9b1ddcc044072c176e763a8f671d71e09))
    - Remember turtle diffs ([`0087222`](https://github.com/SemanticWebLanguageServer/swls/commit/00872223ae341724739c12853a452719bb6f284c))
    - Remove compiler warnings ([`973a7af`](https://github.com/SemanticWebLanguageServer/swls/commit/973a7afa89501d737db1a96bfeefadd9dc2b5070))
    - Fix tests ([`35e616d`](https://github.com/SemanticWebLanguageServer/swls/commit/35e616d027cf5ead6ca4466dcf7402da8fe1a77e))
    - Cargo fmt ([`464a42a`](https://github.com/SemanticWebLanguageServer/swls/commit/464a42a93db9044bd7e661721875e33c2c57f525))
    - Add documentation icons ([`b34ad57`](https://github.com/SemanticWebLanguageServer/swls/commit/b34ad5790074e5d61d909b086f2157809f1d7d36))
    - Fix tests ([`87af94f`](https://github.com/SemanticWebLanguageServer/swls/commit/87af94fc0a10aa789a75cf7b44673951c4bb0a57))
    - Refactor + add documentation ([`f45a56d`](https://github.com/SemanticWebLanguageServer/swls/commit/f45a56d88c5dabca1889b1609ea5d49bb8675869))
    - Cargo +nightly fmt ([`d4b3573`](https://github.com/SemanticWebLanguageServer/swls/commit/d4b357369b476ba6c55ce4e934900f5cac43f4dd))
    - Refactor jsonld ([`f81bb4c`](https://github.com/SemanticWebLanguageServer/swls/commit/f81bb4ceea38754206b4d5e8fb4845e33e938137))
    - Fix tests ([`a21efe7`](https://github.com/SemanticWebLanguageServer/swls/commit/a21efe70613a9ebd4fb1068dd94b57b18e642ec7))
    - Use refactor jsonld ([`390ef93`](https://github.com/SemanticWebLanguageServer/swls/commit/390ef93264f320276393d8bc708cb341706b23c9))
    - Fix tests ([`56ca3e6`](https://github.com/SemanticWebLanguageServer/swls/commit/56ca3e689db15f86f58a541425adf3c7be9492fe))
    - Cargo fix --lib ([`ebcef05`](https://github.com/SemanticWebLanguageServer/swls/commit/ebcef058efc5674a904d4edd5b2699e5e45232c0))
    - Refactor Spanned ([`d35e695`](https://github.com/SemanticWebLanguageServer/swls/commit/d35e695b22a2e71e4219600d0739ae51b5cdd70b))
    - Start add docs and refactor ([`2798feb`](https://github.com/SemanticWebLanguageServer/swls/commit/2798feb2d43106eebb00e1054f6931cda06c22a7))
    - Add keyboard shortcuts to demo ([`3da8dcc`](https://github.com/SemanticWebLanguageServer/swls/commit/3da8dcc5c28de14aa1818bb464b223c8dca96563))
    - Move to cargo make, move lsp-bin/src/backend.rs to core/src/backend.rs, add wasm compile ([`5ef8c77`](https://github.com/SemanticWebLanguageServer/swls/commit/5ef8c777c59bf4eb78f809ae8dcbe818942f9882))
    - Add types on hover ([`112ea76`](https://github.com/SemanticWebLanguageServer/swls/commit/112ea76a807d22c0dfc77ca75d6c7926238e33cc))
    - Fix some bugs and better derive triples ([`e04ab58`](https://github.com/SemanticWebLanguageServer/swls/commit/e04ab588d0f2cc90b8b2bd003b050fd52b7cb08b))
    - Start sparql ([`865fcdc`](https://github.com/SemanticWebLanguageServer/swls/commit/865fcdcb3c6247613585316e0ad849b8a08eb229))
    - Better parse json ([`da2f271`](https://github.com/SemanticWebLanguageServer/swls/commit/da2f271cdc3c010894679b323eb0c1328a52aa7b))
    - Add jsonld completion ([`34037c0`](https://github.com/SemanticWebLanguageServer/swls/commit/34037c066029c5d72b9cb2ebccd5db0e96937777))
    - Parse files with CreateEvent ([`6cdff13`](https://github.com/SemanticWebLanguageServer/swls/commit/6cdff13a4e383ca3f1fb816ced4bbe044584d539))
    - Parse jsonld triples ([`dd4d115`](https://github.com/SemanticWebLanguageServer/swls/commit/dd4d115b746bdd302f3a80805529b1d5683524f6))
    - Cleanup unused implementations ([`a14bcba`](https://github.com/SemanticWebLanguageServer/swls/commit/a14bcbafda4f4a3beff84561b28c21eb2ca41368))
    - Better simple completions ([`9ac70f8`](https://github.com/SemanticWebLanguageServer/swls/commit/9ac70f81993e4e96b7deb48dca56bbaf3636ae89))
    - Cleanup ([`f0208b9`](https://github.com/SemanticWebLanguageServer/swls/commit/f0208b9a0119fcb6b69d26801b7c9e6cc51975b2))
    - Start migrate to bevy ecs ([`32a931e`](https://github.com/SemanticWebLanguageServer/swls/commit/32a931eb641c715871b06307a94457ed0e2d22e7))
    - Format Cargo tomls ([`8aa4380`](https://github.com/SemanticWebLanguageServer/swls/commit/8aa4380fe90b36e63b174032bcc0e4fc9df9adab))
    - Move fetch to client ([`48ce642`](https://github.com/SemanticWebLanguageServer/swls/commit/48ce64280a4e407c637df0ca223b87c717259e61))
    - Split jsonld ([`9eee2f2`](https://github.com/SemanticWebLanguageServer/swls/commit/9eee2f23e6b0ff8b32f31ec407fbff5bf1caad11))
</details>

