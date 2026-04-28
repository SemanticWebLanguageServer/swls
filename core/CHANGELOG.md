# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## v0.1.1 (2026-04-28)

### New Features

 - <csr-id-37ca76298f9b43001bdcc6f9096a8611b8559128/> bump bevy_ecs to 0.18!
 - <csr-id-35ff9925b3957c491e3798a62ae4e4e32f88d40e/> make cjs work for vscode
 - <csr-id-478139e705d9f72c1ad9e8df5228379afd2b5b7f/> remove PathBuf from read dir
 - <csr-id-44cb452ffa0417562a5d26c863981419b56df701/> move components-rs to Url
 - <csr-id-663af2958384584b8111e41a3131fb55984b71bf/> fix many minor mistakes
 - <csr-id-987907ef012c3d41a094a3b7bf853ed0a55d23a8/> bump
 - <csr-id-97441d4d9dc7778d3fcb6b0862ad0d61fa38b01f/> check if cjs to properties and classes is a good idea
 - <csr-id-5ec44b5d4c325bb6687bf70292c29b74d4770ded/> goto @context string
 - <csr-id-f2a1679be0c5261e2705335eb2c2c1a14015906d/> improve cjs goto definition
 - <csr-id-9a887c510c8740c9d9f01a27f88aeca533a69cf3/> incorporate cjs, but we cannot yet parse the IRIs
 - <csr-id-85ebb4941c245ecf65c0146b644eee371cb7b939/> try to render hover as markdown
 - <csr-id-ade12adaca2ba8bf197b3aaf59091ee9ec266687/> add better jsonld support
 - <csr-id-a47bca07a0b3d01fbd411868bb7308cf9802bcf5/> things
 - <csr-id-7c78bd43551a5afcdaaa8bef83fe9fd9ecd4d443/> make jsonld context resolve with local contexts
 - <csr-id-aa9acc8a6c0565ef86b54545222717e00760ac00/> fix many bugs like highlighting, json-ld, autocompletion when not in a token etc
 - <csr-id-5157d404a94386a17c74ea125a6d8809412d10ac/> improve error spans
 - <csr-id-dc45e8187ffa2cd0a6d6aec35e7477802b8bcd88/> use new parser for turtle and sparql
 - <csr-id-721d151c1dbcaccab3a81f81f7d59f2517fe323c/> rename before publish
 - <csr-id-c6d99d1cfab4ab20fec9a1b376f3fa7c443d8a52/> move to upstream rudof
 - <csr-id-cf54f0bfd210e1b01311683399a70f8d7ae157e7/> optimize logging
 - <csr-id-799356a263277b24507115aa1574b5d0c1f7303a/> claude refactor
 - <csr-id-16579c4da87b74784982930c3af0dea8e938b0d5/> add unused prefix diagnostic and organize imports code action
 - <csr-id-27b8c86e7d11ebf2faee236c03b360e37c024e28/> add file based configs + order prefix completions based on popularity

### Bug Fixes

 - <csr-id-29ef7e453047b405fe76ff846b8a0bf230489bef/> make cargo release happy with version numbers
 - <csr-id-acd94a957d7ad001014eadef773bf59ee0fa6391/> remove unused imports warnings

### Other

 - <csr-id-4debbca4c3e1839781b7728951f91f2ba729165b/> fix jsonld goto definition for the last time!

### Commit Statistics

<csr-read-only-do-not-edit/>

 - 129 commits contributed to the release.
 - 26 commits were understood as [conventional](https://www.conventionalcommits.org).
 - 0 issues like '(#ID)' were seen in commit messages

### Commit Details

<csr-read-only-do-not-edit/>

<details><summary>view details</summary>

 * **Uncategorized**
    - Bump bevy_ecs to 0.18! ([`37ca762`](https://github.com/SemanticWebLanguageServer/swls/commit/37ca76298f9b43001bdcc6f9096a8611b8559128))
    - Make cjs work for vscode ([`35ff992`](https://github.com/SemanticWebLanguageServer/swls/commit/35ff9925b3957c491e3798a62ae4e4e32f88d40e))
    - Remove PathBuf from read dir ([`478139e`](https://github.com/SemanticWebLanguageServer/swls/commit/478139e705d9f72c1ad9e8df5228379afd2b5b7f))
    - Fix jsonld goto definition for the last time! ([`4debbca`](https://github.com/SemanticWebLanguageServer/swls/commit/4debbca4c3e1839781b7728951f91f2ba729165b))
    - Move components-rs to Url ([`44cb452`](https://github.com/SemanticWebLanguageServer/swls/commit/44cb452ffa0417562a5d26c863981419b56df701))
    - Fix many minor mistakes ([`663af29`](https://github.com/SemanticWebLanguageServer/swls/commit/663af2958384584b8111e41a3131fb55984b71bf))
    - Bump ([`987907e`](https://github.com/SemanticWebLanguageServer/swls/commit/987907ef012c3d41a094a3b7bf853ed0a55d23a8))
    - Check if cjs to properties and classes is a good idea ([`97441d4`](https://github.com/SemanticWebLanguageServer/swls/commit/97441d4d9dc7778d3fcb6b0862ad0d61fa38b01f))
    - Goto @context string ([`5ec44b5`](https://github.com/SemanticWebLanguageServer/swls/commit/5ec44b5d4c325bb6687bf70292c29b74d4770ded))
    - Improve cjs goto definition ([`f2a1679`](https://github.com/SemanticWebLanguageServer/swls/commit/f2a1679be0c5261e2705335eb2c2c1a14015906d))
    - Incorporate cjs, but we cannot yet parse the IRIs ([`9a887c5`](https://github.com/SemanticWebLanguageServer/swls/commit/9a887c510c8740c9d9f01a27f88aeca533a69cf3))
    - Try to render hover as markdown ([`85ebb49`](https://github.com/SemanticWebLanguageServer/swls/commit/85ebb4941c245ecf65c0146b644eee371cb7b939))
    - Add better jsonld support ([`ade12ad`](https://github.com/SemanticWebLanguageServer/swls/commit/ade12adaca2ba8bf197b3aaf59091ee9ec266687))
    - Things ([`a47bca0`](https://github.com/SemanticWebLanguageServer/swls/commit/a47bca07a0b3d01fbd411868bb7308cf9802bcf5))
    - Make jsonld context resolve with local contexts ([`7c78bd4`](https://github.com/SemanticWebLanguageServer/swls/commit/7c78bd43551a5afcdaaa8bef83fe9fd9ecd4d443))
    - Fix many bugs like highlighting, json-ld, autocompletion when not in a token etc ([`aa9acc8`](https://github.com/SemanticWebLanguageServer/swls/commit/aa9acc8a6c0565ef86b54545222717e00760ac00))
    - Improve error spans ([`5157d40`](https://github.com/SemanticWebLanguageServer/swls/commit/5157d404a94386a17c74ea125a6d8809412d10ac))
    - Remove logos/chumsky tokenizers; replace with CST-based token production ([`da53ead`](https://github.com/SemanticWebLanguageServer/swls/commit/da53eadb2b042a3d9c8b4d1d28d190e2ef1462f2))
    - Use new parser for turtle and sparql ([`dc45e81`](https://github.com/SemanticWebLanguageServer/swls/commit/dc45e8187ffa2cd0a6d6aec35e7477802b8bcd88))
    - Make cargo release happy with version numbers ([`29ef7e4`](https://github.com/SemanticWebLanguageServer/swls/commit/29ef7e453047b405fe76ff846b8a0bf230489bef))
    - Remove unused imports warnings ([`acd94a9`](https://github.com/SemanticWebLanguageServer/swls/commit/acd94a957d7ad001014eadef773bf59ee0fa6391))
    - Rename before publish ([`721d151`](https://github.com/SemanticWebLanguageServer/swls/commit/721d151c1dbcaccab3a81f81f7d59f2517fe323c))
    - Move to upstream rudof ([`c6d99d1`](https://github.com/SemanticWebLanguageServer/swls/commit/c6d99d1cfab4ab20fec9a1b376f3fa7c443d8a52))
    - Optimize logging ([`cf54f0b`](https://github.com/SemanticWebLanguageServer/swls/commit/cf54f0bfd210e1b01311683399a70f8d7ae157e7))
    - Claude refactor ([`799356a`](https://github.com/SemanticWebLanguageServer/swls/commit/799356a263277b24507115aa1574b5d0c1f7303a))
    - Add unused prefix diagnostic and organize imports code action ([`16579c4`](https://github.com/SemanticWebLanguageServer/swls/commit/16579c4da87b74784982930c3af0dea8e938b0d5))
    - Try to find and fix broken blank node parsing ([`0cc6538`](https://github.com/SemanticWebLanguageServer/swls/commit/0cc65382a738f79d3c673aa451feea229836bfdc))
    - Yolo move rudof to github repository ([`cc71e61`](https://github.com/SemanticWebLanguageServer/swls/commit/cc71e61e97eafcc9a57f5f92c6166a09c5118b64))
    - Bring shapes back :party: ([`75763fc`](https://github.com/SemanticWebLanguageServer/swls/commit/75763fc1d8c23ca2d5d54f01730a27c2a910f440))
    - Add global shapes ([`3ea0c89`](https://github.com/SemanticWebLanguageServer/swls/commit/3ea0c89dc67e3cbad528be66bd3da74ba123bd51))
    - Readd shapes ([`c5dd419`](https://github.com/SemanticWebLanguageServer/swls/commit/c5dd41900a6acac67e5be88966291f24781a44cf))
    - Add a bit of logging ([`f0c3911`](https://github.com/SemanticWebLanguageServer/swls/commit/f0c3911a94c31ab90d679c038542d65b1694b516))
    - Improve property extraction + show domain and range in suggestions ([`777e1ab`](https://github.com/SemanticWebLanguageServer/swls/commit/777e1ab90df9a4a86408c5db41f63b5032c004cd))
    - Update prefix.cc prefixes ([`d21bbcc`](https://github.com/SemanticWebLanguageServer/swls/commit/d21bbcc79e0b8fd8924aae4672aa96d93da313aa))
    - Improve prefix.cc completion ([`20151a1`](https://github.com/SemanticWebLanguageServer/swls/commit/20151a12f96cc9432ee567ce4c9c01ef195bea33))
    - Show sub and superclasses on type hover ([`418ae2f`](https://github.com/SemanticWebLanguageServer/swls/commit/418ae2fa9a7d66201f27da70b13e13483e604419))
    - Only inlay super type, not subtypes ([`f20efd9`](https://github.com/SemanticWebLanguageServer/swls/commit/f20efd9224a916a19f1d2b84b317fd2783fc4f9d))
    - Complete properties based on types ([`14bfbf3`](https://github.com/SemanticWebLanguageServer/swls/commit/14bfbf300af28e7cdb392e911f47fcdf2bb4097e))
    - Clearify subclassof and superclassof + fix domain and range hover issues ([`5a04274`](https://github.com/SemanticWebLanguageServer/swls/commit/5a042740f8f5016a4a5c4ee9010a6a14e01591af))
    - Prefer configured ontologies before lov ontologies ([`6706681`](https://github.com/SemanticWebLanguageServer/swls/commit/670668102fb80b4ca571e4cd728707ed6169fc0c))
    - Derive properties and classes with sparql queries ([`482f1d8`](https://github.com/SemanticWebLanguageServer/swls/commit/482f1d8a9ebb5bbdf0ef628edf7cfa5b8f160971))
    - Fix tests ([`27e906f`](https://github.com/SemanticWebLanguageServer/swls/commit/27e906fb1bd395ebb3a66bd5d9067f187208cda0))
    - Fix update dependencies ([`d1ad1bc`](https://github.com/SemanticWebLanguageServer/swls/commit/d1ad1bc26e3ae25dcfaff4913c521835b91d0068))
    - Update dependencies ([`cec99b9`](https://github.com/SemanticWebLanguageServer/swls/commit/cec99b9c7028b3556e68017c8f67c2cb2af75057))
    - Let's just save this for now ([`6a7b5bb`](https://github.com/SemanticWebLanguageServer/swls/commit/6a7b5bb4a88e9268e67bac53da58258911a5ebe4))
    - Allow glob ontologies ([`8933499`](https://github.com/SemanticWebLanguageServer/swls/commit/8933499ce8cb504a29e5fd8864998c9292655a8f))
    - Read extra ontology files as ontologies ([`675a559`](https://github.com/SemanticWebLanguageServer/swls/commit/675a5591d85cbac7a84bdb172b79e62fb362bc0c))
    - Add file based configs + order prefix completions based on popularity ([`27b8c86`](https://github.com/SemanticWebLanguageServer/swls/commit/27b8c86e7d11ebf2faee236c03b360e37c024e28))
    - Remove unused dependencies ([`033dd71`](https://github.com/SemanticWebLanguageServer/swls/commit/033dd718488003e1149f6af6182799d99e8bf5b9))
    - Fix off by a few error for cogs ontology ([`1253c95`](https://github.com/SemanticWebLanguageServer/swls/commit/1253c951106a68b5c7d563dc51449f0e58b42387))
    - Configure which languages to activate ([`5fe3e6f`](https://github.com/SemanticWebLanguageServer/swls/commit/5fe3e6fe4dd91948bfc2bc6584eb6716e41946a4))
    - Move lsp-bin to swls ([`2c3ccc0`](https://github.com/SemanticWebLanguageServer/swls/commit/2c3ccc07ea7e71459e960c65c809c99b538d9569))
    - Remove logs + add context to sparql + fix race condition in app ([`6770fbf`](https://github.com/SemanticWebLanguageServer/swls/commit/6770fbf9b1ddcc044072c176e763a8f671d71e09))
    - Parse turtle with previous token information (subject or predicate or object) ([`02a28f0`](https://github.com/SemanticWebLanguageServer/swls/commit/02a28f0215c81cdef7bb1798c34398fbdf56b74b))
    - Remember turtle diffs ([`0087222`](https://github.com/SemanticWebLanguageServer/swls/commit/00872223ae341724739c12853a452719bb6f284c))
    - Move readFile to fs Resource + implement custom/readFile in vscode extension ([`2fdcd85`](https://github.com/SemanticWebLanguageServer/swls/commit/2fdcd85e82a36afef4110e6c353e84e55040f8fd))
    - Remove compiler warnings ([`973a7af`](https://github.com/SemanticWebLanguageServer/swls/commit/973a7afa89501d737db1a96bfeefadd9dc2b5070))
    - Goto definition resolves to local files in binary and does applyEdits for web lsps ([`36dbe24`](https://github.com/SemanticWebLanguageServer/swls/commit/36dbe2430f7f07c16237cf3b61400ec181ac10a6))
    - Fix semantic tokens issue ([`d418613`](https://github.com/SemanticWebLanguageServer/swls/commit/d4186135bd6bd8a986736b1246cff61b4d30f431))
    - Figure out how to apply edits for goto definitions ([`9281436`](https://github.com/SemanticWebLanguageServer/swls/commit/928143664e71f5f7654e96407d75568a84748caf))
    - Add conformance tests and move turtle tokenization to logos ([`0656e22`](https://github.com/SemanticWebLanguageServer/swls/commit/0656e2291020f2480173388e8b934c935826c1f2))
    - Add wasm guard statements ([`27a1f4c`](https://github.com/SemanticWebLanguageServer/swls/commit/27a1f4c2b68009d7efc1081c063c9a73bd4f7436))
    - Fix tests ([`35e616d`](https://github.com/SemanticWebLanguageServer/swls/commit/35e616d027cf5ead6ca4466dcf7402da8fe1a77e))
    - Add timing information in lgos ([`10307c5`](https://github.com/SemanticWebLanguageServer/swls/commit/10307c5994b75fc75344585ce09048b69bddbbcd))
    - Check expanded term equality for references ([`20ff0c0`](https://github.com/SemanticWebLanguageServer/swls/commit/20ff0c0e70ea58ef21b267293eb26f499f826870))
    - Goto implementation is actually goto definition ([`89a70d2`](https://github.com/SemanticWebLanguageServer/swls/commit/89a70d2b7fe55b80782db17938451148173be323))
    - Add cached lov fetching ([`aeea720`](https://github.com/SemanticWebLanguageServer/swls/commit/aeea7201a0f24373b52138df9e4d315fac0f65e7))
    - Goto implementation ([`ee00eba`](https://github.com/SemanticWebLanguageServer/swls/commit/ee00eba3671d7f6847eb367f58e2f6a0467ebc2b))
    - Add goto references ([`735508d`](https://github.com/SemanticWebLanguageServer/swls/commit/735508d850c83f82647063f86974229e82f92077))
    - Cargo fmt ([`464a42a`](https://github.com/SemanticWebLanguageServer/swls/commit/464a42a93db9044bd7e661721875e33c2c57f525))
    - Derive all property and class types ([`4607c51`](https://github.com/SemanticWebLanguageServer/swls/commit/4607c5154dcb1b42d2e052fe5277f247794e1d44))
    - Add documentation icons ([`b34ad57`](https://github.com/SemanticWebLanguageServer/swls/commit/b34ad5790074e5d61d909b086f2157809f1d7d36))
    - Add favicon ([`6c8ba15`](https://github.com/SemanticWebLanguageServer/swls/commit/6c8ba154c9a7000435931b8e1c3d77fadf243714))
    - Make app work again + refactor links ([`03e187e`](https://github.com/SemanticWebLanguageServer/swls/commit/03e187e04492b42b18d85558d33018438087e9b3))
    - Fix tests ([`87af94f`](https://github.com/SemanticWebLanguageServer/swls/commit/87af94fc0a10aa789a75cf7b44673951c4bb0a57))
    - Refactor + add documentation ([`f45a56d`](https://github.com/SemanticWebLanguageServer/swls/commit/f45a56d88c5dabca1889b1609ea5d49bb8675869))
    - Cargo +nightly fmt ([`d4b3573`](https://github.com/SemanticWebLanguageServer/swls/commit/d4b357369b476ba6c55ce4e934900f5cac43f4dd))
    - Enable semantic highlighting in vscode and publish ([`fbe1ecf`](https://github.com/SemanticWebLanguageServer/swls/commit/fbe1ecfa1bb567e012421f25407b5deb240241d4))
    - Cargo fix ([`b1249be`](https://github.com/SemanticWebLanguageServer/swls/commit/b1249be37853eea36ae35b76a0b79ed0b4d34136))
    - Use refactor in lang-turtle ([`634f221`](https://github.com/SemanticWebLanguageServer/swls/commit/634f221ce1353f5579ac49f3490bd6bd86e58068))
    - Cargo fmt ([`92ff670`](https://github.com/SemanticWebLanguageServer/swls/commit/92ff67014c3461231042156fff1fe2c8617ecf07))
    - Refactor continues ([`ab76f4b`](https://github.com/SemanticWebLanguageServer/swls/commit/ab76f4b7c100289e9f0adbce21336e36702637b1))
    - Document components ([`147b22a`](https://github.com/SemanticWebLanguageServer/swls/commit/147b22a8bae8bdcebde20f2da762c8c86331756f))
    - Fix tests ([`56ca3e6`](https://github.com/SemanticWebLanguageServer/swls/commit/56ca3e689db15f86f58a541425adf3c7be9492fe))
    - Remove warnings ([`f5045f5`](https://github.com/SemanticWebLanguageServer/swls/commit/f5045f5dad25335c1f4bc96aea189a8a2332c15d))
    - Cargo fix --lib ([`ebcef05`](https://github.com/SemanticWebLanguageServer/swls/commit/ebcef058efc5674a904d4edd5b2699e5e45232c0))
    - Refactor Spanned ([`d35e695`](https://github.com/SemanticWebLanguageServer/swls/commit/d35e695b22a2e71e4219600d0739ae51b5cdd70b))
    - Refactor completion ([`ff89762`](https://github.com/SemanticWebLanguageServer/swls/commit/ff89762c2fe89eb3ed947cd20d59c0e246eef7de))
    - Start add docs and refactor ([`2798feb`](https://github.com/SemanticWebLanguageServer/swls/commit/2798feb2d43106eebb00e1054f6931cda06c22a7))
    - Rewrite vscode extension ([`084ee88`](https://github.com/SemanticWebLanguageServer/swls/commit/084ee888f9a923666eed8c6c620ca30638491d21))
    - Fix rename ([`f0b2087`](https://github.com/SemanticWebLanguageServer/swls/commit/f0b2087cfd50d5b834c9829d60373734bfcb974b))
    - Add rename ([`18937ec`](https://github.com/SemanticWebLanguageServer/swls/commit/18937ec14cbd1186ab4e585b9f398c548371aef9))
    - Add keyboard shortcuts to demo ([`3da8dcc`](https://github.com/SemanticWebLanguageServer/swls/commit/3da8dcc5c28de14aa1818bb464b223c8dca96563))
    - Shape validation on save ([`814309a`](https://github.com/SemanticWebLanguageServer/swls/commit/814309aeab9bff1b8998249681cc8247278f4387))
    - Add shacl validations ([`c22f73e`](https://github.com/SemanticWebLanguageServer/swls/commit/c22f73ec5023485bf467e968d25609a0b95301a7))
    - Add prefix.cc completion ([`d5f46bc`](https://github.com/SemanticWebLanguageServer/swls/commit/d5f46bc03e7f8c0c05239eb39151e71991a2634b))
    - Enable lsp on monaco ([`f0a8e89`](https://github.com/SemanticWebLanguageServer/swls/commit/f0a8e89611937a7f374871f1eb3ad59292c66472))
    - Move to cargo make, move lsp-bin/src/backend.rs to core/src/backend.rs, add wasm compile ([`5ef8c77`](https://github.com/SemanticWebLanguageServer/swls/commit/5ef8c777c59bf4eb78f809ae8dcbe818942f9882))
    - Add undefined prefixes diagnostic ([`98585a2`](https://github.com/SemanticWebLanguageServer/swls/commit/98585a2a2b668f547018b0bdcd1ac9a2f20bfb85))
    - Add types on hover ([`d3cf042`](https://github.com/SemanticWebLanguageServer/swls/commit/d3cf04201447f2202cc4401b333de62eb0f29f35))
    - Add types on hover ([`112ea76`](https://github.com/SemanticWebLanguageServer/swls/commit/112ea76a807d22c0dfc77ca75d6c7926238e33cc))
    - Add things ([`e54f7bc`](https://github.com/SemanticWebLanguageServer/swls/commit/e54f7bc5e7470a9c506dd8b263d12d63329a8a1b))
    - Fix some bugs and better derive triples ([`e04ab58`](https://github.com/SemanticWebLanguageServer/swls/commit/e04ab588d0f2cc90b8b2bd003b050fd52b7cb08b))
    - Add sparql-lang triples ([`1718322`](https://github.com/SemanticWebLanguageServer/swls/commit/171832265be634eb7b239b8a8ba6786b9bca57d8))
    - Start sparql ([`865fcdc`](https://github.com/SemanticWebLanguageServer/swls/commit/865fcdcb3c6247613585316e0ad849b8a08eb229))
    - Better parse json ([`da2f271`](https://github.com/SemanticWebLanguageServer/swls/commit/da2f271cdc3c010894679b323eb0c1328a52aa7b))
    - Add jsonld completion ([`34037c0`](https://github.com/SemanticWebLanguageServer/swls/commit/34037c066029c5d72b9cb2ebccd5db0e96937777))
    - Parse files with CreateEvent ([`6cdff13`](https://github.com/SemanticWebLanguageServer/swls/commit/6cdff13a4e383ca3f1fb816ced4bbe044584d539))
    - Parse jsonld triples ([`dd4d115`](https://github.com/SemanticWebLanguageServer/swls/commit/dd4d115b746bdd302f3a80805529b1d5683524f6))
    - Add property completion ([`8aa55a9`](https://github.com/SemanticWebLanguageServer/swls/commit/8aa55a93ca2cb995c60ab4adcd5bbcc894c73f9a))
    - Cleanup unused implementations ([`a14bcba`](https://github.com/SemanticWebLanguageServer/swls/commit/a14bcbafda4f4a3beff84561b28c21eb2ca41368))
    - Better simple completions ([`9ac70f8`](https://github.com/SemanticWebLanguageServer/swls/commit/9ac70f81993e4e96b7deb48dca56bbaf3636ae89))
    - Add turtle prefix document links ([`441b52d`](https://github.com/SemanticWebLanguageServer/swls/commit/441b52d5d0c67f2ec3d31e5c877041a73fb464c0))
    - Add lov prefix completion ([`40c922f`](https://github.com/SemanticWebLanguageServer/swls/commit/40c922f1ab74f606ae024909220cb986e996c6e2))
    - Derive more asref, asmut ([`6e73321`](https://github.com/SemanticWebLanguageServer/swls/commit/6e7332179cd074815b16e188238b9d35038a5bf3))
    - Work on completions ([`7a9d351`](https://github.com/SemanticWebLanguageServer/swls/commit/7a9d35178d954503131136f4c139a79b641669df))
    - Cargo fmt ([`60d4190`](https://github.com/SemanticWebLanguageServer/swls/commit/60d4190d37e1045a613264962f57061369b9bfcf))
    - Cleanup ([`f0208b9`](https://github.com/SemanticWebLanguageServer/swls/commit/f0208b9a0119fcb6b69d26801b7c9e6cc51975b2))
    - Remove mutexed world, use channels ([`a2c1ac9`](https://github.com/SemanticWebLanguageServer/swls/commit/a2c1ac93f6056a33677e6da6f85d5772d7e9b594))
    - Setup formatting ([`3c3054d`](https://github.com/SemanticWebLanguageServer/swls/commit/3c3054dcace0fdd4457978427a1098f6be2cc3c7))
    - Start using the ecs in the backend ([`ef97317`](https://github.com/SemanticWebLanguageServer/swls/commit/ef973170bb66b2cf4854826d6c1ddca39a0a75c9))
    - Start migrate to bevy ecs ([`32a931e`](https://github.com/SemanticWebLanguageServer/swls/commit/32a931eb641c715871b06307a94457ed0e2d22e7))
    - Format Cargo tomls ([`8aa4380`](https://github.com/SemanticWebLanguageServer/swls/commit/8aa4380fe90b36e63b174032bcc0e4fc9df9adab))
    - Move fetch to client ([`48ce642`](https://github.com/SemanticWebLanguageServer/swls/commit/48ce64280a4e407c637df0ca223b87c717259e61))
    - Split jsonld ([`9eee2f2`](https://github.com/SemanticWebLanguageServer/swls/commit/9eee2f23e6b0ff8b32f31ec407fbff5bf1caad11))
    - Split lang-turtle ([`50df638`](https://github.com/SemanticWebLanguageServer/swls/commit/50df638d5687a9a165745066c9fe937ee57cd447))
    - Nexter steps ([`8f5a01d`](https://github.com/SemanticWebLanguageServer/swls/commit/8f5a01de5b88d0ce108a1d6ca8487060c395a982))
    - Next steps ([`cf866ec`](https://github.com/SemanticWebLanguageServer/swls/commit/cf866ec0b80f6da699c0007fd4e51c526d30c3cf))
    - Start adding web and core workspace packages ([`fd37f28`](https://github.com/SemanticWebLanguageServer/swls/commit/fd37f2876fc6b911f0fe467eccb0838c8fcc9aa0))
</details>

