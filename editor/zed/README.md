# Zydeco for Zed

This extension recognizes `.zy` and `.zydeco` files and starts the
[Cajun language server](https://github.com/zydeco-lang/zydeco/tree/main/editor/cajun).
It provides live syntax and name-resolution diagnostics, document symbols,
definition and reference lookup across imported files, inferred kind and type
information on hover with inline links to type definitions, and compiler-aware semantic
highlighting.

The extension does not bundle a Tree-sitter grammar. Enable Cajun's full-document
semantic tokens for Zydeco in Zed settings:

```json
{
  "languages": {
    "Zydeco": {
      "semantic_tokens": "full"
    }
  }
}
```

Cajun keeps lexical highlighting available while a file is incomplete, then
refines resolved binders and references with kind, value-type, computation-type,
value, and computation information when the corresponding compiler phases succeed.

## Installing Cajun

Install the server with Cargo:

```sh
cargo install --git https://github.com/zydeco-lang/zydeco.git cajun --bin cajun --locked
```

Zed finds `cajun` on the worktree `PATH`. You can instead configure an
explicit executable in Zed settings:

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

Then open Zed's extensions view, choose **Install Dev Extension**, and select
this directory. Configure `lsp.cajun.binary.path` to point to
`target/debug/cajun` if that binary is not already on `PATH`.
