# Zydeco for Zed

This extension recognizes `.zy` and `.zydeco` files and starts the
[Cajun language server](https://github.com/zydeco-lang/zydeco/tree/main/editor/cajun).
It provides live syntax and name-resolution diagnostics, document symbols,
and go to definition across imported files.

The extension currently uses Cajun for structural language features and does
not bundle a Tree-sitter grammar.

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
