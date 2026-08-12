# Zydeco for VS Code

This extension provides Zydeco syntax highlighting
and starts the [Cajun language server](https://github.com/zydeco-lang/zydeco/tree/main/editor/cajun)
for `.zy` and `.zydeco` files.

## Features

- live syntax and name-resolution diagnostics;
- document symbols;
- definition and reference lookup across imported source files;
- inferred kind and type information on hover, with links to type definitions;
- compiler-aware semantic highlighting;
- full synchronization of unsaved editor contents.

## Installing Cajun

Install the server with Cargo:

```sh
cargo install --git https://github.com/zydeco-lang/zydeco.git cajun --bin cajun --locked
```

The extension finds `cajun` on `PATH`.
You can instead set `cajun.server.path` to an explicit executable path.
If neither is available, the extension offers to run the Cargo command above.

## Development

From this directory:

```sh
pnpm install
pnpm compile
```

Build Cajun from the repository root with `cargo build -p cajun`, then press <kbd>F5</kbd> in VS Code.
The development launch configuration supplies the workspace binary through `CAJUN_SERVER_PATH`.
