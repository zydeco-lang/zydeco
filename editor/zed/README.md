# Zydeco for Zed

This extension recognizes `.zy` and `.zydeco` files
and starts the [Cajun language server](https://github.com/zydeco-lang/zydeco/tree/main/editor/cajun).
It provides live syntax and name-resolution diagnostics, document symbols, definition and reference lookup
across imported files, inferred kind and type information on hover with links to type definitions,
and compiler-aware semantic highlighting.

The extension does not activate its Tree-sitter grammar until a committed grammar revision is available to Zed.
Enable Cajun's full-document semantic tokens for Zydeco in Zed settings:

```json
{
  "languages": {
    "Zydeco": {
      "semantic_tokens": "full"
    }
  }
}
```

Cajun keeps lexical highlighting available while a file is incomplete,
then refines resolved binders and references with kind, value-type, computation-type,
value, and computation information when the corresponding compiler phases succeed.

## Hover width

Cajun returns hover signatures in `zydeco` Markdown code fences and pretty-prints them
to the width supplied by the editor integration.
Zed computes each popover's width locally from the current editor pane,
but the LSP protocol does not report that live width to the server.
The extension therefore uses a conservative 72-column budget by default.
For consistently narrower panes, set a matching column budget in Zed settings:

```json
{
  "lsp": {
    "cajun": {
      "initialization_options": {
        "hover": {
          "lineWidth": 56
        }
      }
    }
  }
}
```

The repository contains the Tree-sitter grammar under `../tree-sitter-zydeco`
and Zed query files under `languages/zydeco`.
Activating it in the extension is a two-commit release operation: first commit the generated parser,
then add its commit SHA to `extension.toml` and set `grammar = "zydeco"` in `config.toml`:

```toml
[grammars.zydeco]
repository = "https://github.com/zydeco-lang/zydeco"
rev = "<commit-containing-the-generated-parser>"
path = "editor/tree-sitter-zydeco"
```

Once the grammar is active, use `"semantic_tokens": "combined"` to retain Tree-sitter highlighting
while applying Cajun's compiler-aware refinements.

## Installing Cajun

Install the server with Cargo:

```sh
cargo install --git https://github.com/zydeco-lang/zydeco.git cajun --bin cajun --locked
```

Zed finds `cajun` on the worktree `PATH`.
You can instead configure an explicit executable in Zed settings:

```json
{
  "lsp": {
    "cajun": {
      "binary": {
        "path": "/absolute/path/to/cajun"
      }
    }
  }
}
```

## Development

Build Cajun from the repository root:

```sh
cargo build -p cajun
```

Then open Zed's extensions view, choose **Install Dev Extension**, and select this directory.
Configure `lsp.cajun.binary.path` to point to `target/debug/cajun` if that binary is not already on `PATH`.
