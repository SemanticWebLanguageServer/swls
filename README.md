# Semantic Web Language Server

[![CI](https://github.com/semanticweblanguageserver/swls/actions/workflows/ci.yml/badge.svg)](https://github.com/semanticweblanguageserver/swls/actions/workflows/ci.yml)
[![Docs](https://img.shields.io/badge/docs-latest-blue)](https://semanticweblanguageserver.github.io/swls/docs/lsp_core/index.html)
![LICENSE](https://img.shields.io/badge/License-MIT-8A2BE2)
[![Visual Studio Marketplace Last Updated](https://img.shields.io/visual-studio-marketplace/last-updated/ajuvercr.semantic-web-lsp?label=VSCode%20Extension)](https://marketplace.visualstudio.com/items?itemName=ajuvercr.semantic-web-lsp)

This repo includes the source code for the semantic web language server.
The language server provides IDE like functionality for semantic web languages, including Turtle, JSON-LD and SPARQL.

A live demo can be found [online](https://semanticweblanguageserver.github.io/swls/), built with monaco editors.

## Documentation

- [lsp-core](https://semanticweblanguageserver.github.io/swls/docs/lsp_core/index.html)
- [lang-turtle](https://semanticweblanguageserver.github.io/swls/docs/lang_turtle/index.html)
- [lang-jsonld](https://semanticweblanguageserver.github.io/swls/docs/lang_jsonld/index.html)
- [lang-sparql](https://semanticweblanguageserver.github.io/swls/docs/lang_sparql/index.html)
- [lsp-bin](https://semanticweblanguageserver.github.io/swls/docs/swls/index.html)
- [lsp-web](https://semanticweblanguageserver.github.io/swls/docs/lsp_web/index.html)


## Features

### Diagnostics

- Syntax diagnostics
- Undefined prefix diagnostics
- SHACL shape diagnostics

### Completion

- Prefix completion (just start writing the prefix, `foa` completes to `foaf:` and adding the prefix statement)
- Property completion (ordered according to domain)
- Class completion (when writing the object where the prediate is `a`)

### Hover

- Shows additional information about the entities like class

### Rename

- Rename terms local to the current file 

### Formatting

- Format Turtle

### Highlighting

- Enables semantic highlighting


## Use the LSP

Currently a fluwent install is possible for NeoVim and VSCode.
However the language server protocol enables swift integration into other editors.

### VS Code

Install the semantic web lsp extension ([vscode](https://marketplace.visualstudio.com/items?itemName=ajuvercr.semantic-web-lsp) or [open-vscode](https://open-vsx.org/extension/ajuvercr/semantic-web-lsp)).
The extension starts the lsp from WASM and starts the vscode LSP client.

You can configure the LSP to disable certain languages, this is useful as SPARQL is not fully supported yet, but comes bundled in the LSP.

### Jetbrains

A zip of the Jetbrains plugin is available with the latest releases.
To install the plugin you should download the zip (swls-1.1-SNAPSHOT.zip) and go to Settings (ctrl + alt + s) > Plugins > Gear > Install Plugin from Disk and select the file.
Currently the plugin checks the Github releases on each startup to check if the latest binary is installed, and installs the latest binary.
This is not very user friendly, certainly on low quality internet connections.

PRs are much appreciated on the Jetbrains plugin.

### NeoVim

To use the LSP you will always have to install the binary.
So do that first:

```
cargo install --git https://github.com/SemanticWebLanguageServer/swls.git swls
```
Or locally
```
git clone https://github.com/SemanticWebLanguageServer/swls.git
cargo install --path swls
```

Or download the latest binary from the Github releases.

Configure the LSP in NeoVim.

```lua
vim.api.nvim_create_autocmd("FileType", {
    pattern = { "turtle", "sparql", "jsonld" },
    callback = function()
        vim.lsp.start({
            name = "swls",
            cmd = { "swls" },
            root_dir = vim.fn.getcwd(),
            init_options = {
                sparql = false, -- disable sparql support
                -- turtle = false,
                -- jsonld = false,
            },
        })
    end,
})
```

You can configure the LSP to disable certain languages, this is useful as SPARQL is not fully supported yet, but comes bundled in the LSP.

<details>
<summary>Instructions for configuring an autocmd to detect and assign filetypes automatically.</summary>

```lua
vim.api.nvim_create_autocmd({ "BufNewFile", "BufReadPost" }, {
    pattern = "*.ttl",
    callback = function(args)
        vim.bo[args.buf].filetype = "turtle"
        vim.bo.commentstring = "# %s"
    end,
})

vim.api.nvim_create_autocmd({ "BufNewFile", "BufReadPost" }, {
    pattern = { "*.sq", "*.rq", "*.sparql" },
    callback = function(args)
        vim.bo[args.buf].filetype = "sparql"
        vim.bo.commentstring = "# %s"
    end,
})

vim.api.nvim_create_autocmd({ "BufNewFile", "BufReadPost" }, {
    pattern = { "*.jsonld" },
    callback = function(args)
        vim.bo[args.buf].filetype = "jsonld"
    end,
})
```
</details>


## Screenshots

|Undefined prefix|Shape violation|
|---|---|
| ![Undefined Prefixes](./screenshots/undefined_prefix.png) | ![Shape violations](./screenshots/shape.png) |

|Complete Class|Complete Property|
|---|---|
| ![Complete Class](./screenshots/complete_class.png) | ![Complete Property](./screenshots/complete_property.png) |

## Citation

When using the Semantic Web Language Server, please use the following citation:

> A. Vercruysse, J. A. Rojas Melendez, and P. Colpaert, “The semantic web language server : enhancing the developer experience for semantic web practitioners,” in The Semantic Web : 22nd European Semantic Web Conference, ESWC 2025, Proceedings, Part II, Portoroz, Slovenia, 2025, vol. 15719, pp. 210–225.

Bibtex:
```bibtex
@inproceedings{SWLS,
  author       = {{Vercruysse, Arthur and Rojas Melendez, Julian Andres and Colpaert, Pieter}},
  booktitle    = {{The Semantic Web : 22nd European Semantic Web Conference, ESWC 2025, Proceedings, Part II}},
  editor       = {{Curry, Edward and Acosta, Maribel and Poveda-Villalón, Maria and van Erp, Marieke and Ojo, Adegboyega and Hose, Katja and Shimizu, Cogan and Lisena, Pasquale}},
  isbn         = {{9783031945779}},
  issn         = {{0302-9743}},
  language     = {{eng}},
  location     = {{Portoroz, Slovenia}},
  pages        = {{210--225}},
  publisher    = {{Springer}},
  title        = {{The semantic web language server : enhancing the developer experience for semantic web practitioners}},
  url          = {{http://doi.org/10.1007/978-3-031-94578-6_12}},
  volume       = {{15719}},
  year         = {{2025}},
}
```

## License

Copyright &copy; 2025, IMEC - IDLab - UGent.
Released under the [MIT License](LICENSE).
