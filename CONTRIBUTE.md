# Contributing

This repository includes the Zydeco language implementation, its standard
library, and a collection of examples. The notes below collect the most common
workflows described in the project documentation.

## Build the CLI

Build the `zydeco` command-line interface in release mode:

```sh
cargo build --bin=zydeco --release
```

The resulting binary is located at `target/release/zydeco`.

## Use the Interactive REPL

Launch the Ratatui frontend through the main CLI:

```sh
zydeco repl
```

Use `cargo run -p zydeco-tui` to launch the same application directly while working on the frontend crate.

The REPL accepts one complete source term per numbered input. Import an earlier input with `@[import(1)] _`; numeric
targets are session inputs, while quoted targets such as `@[import("library.zy")] _` remain filesystem sources.
Commands are ordinary root annotations: `@[type] expression`, `@[run] expression`, `@[help] _`, and `@[quit] _`.

## Format Source Files

Format one or more Zydeco source files in place:

```sh
zydeco fmt path/to/main.zy path/to/library.zy
```

## Run Programs and Examples

Run a Zydeco source file directly:

```sh
zydeco run path/to/main.zy
```

For example, the declaration-free OOPSLA polynomial root runs with:

```sh
zydeco run lib/tests/oopsla/polynomial.zydeco
```

## Run Tests

The repository provides an aggregate test command:

```sh
cargo test-workspace --release
```

## Check the Tree-sitter Grammar

The editor-oriented Tree-sitter grammar lives under `editor/tree-sitter-zydeco`.
Install its pinned development dependency and run the grammar corpus, Zed query,
and current-source conformance checks with:

```sh
cd editor/tree-sitter-zydeco
pnpm install
pnpm check
```

Commit the generated files under `editor/tree-sitter-zydeco/src` whenever the
grammar changes. The compiler's Logos lexer and LALRPOP grammar remain the
authority for accepted Zydeco programs.

## Create a Zydeco Program

A Zydeco program is one complete term in one source file.
Source dependencies are ordinary term imports written at their use sites:

```zydeco
param (
  (/process) :
  @[import("../std/builtin.zy")] _
) in
  ! (process/exit) 42
```

The launcher supplies the declared Builtin package when this term is run.
No authored project configuration or distinguished `main` declaration is needed.

## Work on the Language Implementation

The language pipeline is structured into phases (parsing, desugaring,
name-resolution, type-checking, linking, evaluation) under `lang/`. Each phase
typically includes `syntax`, `arena`, `err`, `fmt`, and `span` modules to keep
the structure consistent across crates.

## Generate Rust API Docs

To generate documentation for a package (for example, `zydeco-statics`):

```sh
cargo doc --package zydeco-statics --no-deps
```

The HTML output is written to `target/doc/`.

## Pre-commit hooks

Optional hooks run `cargo fmt -- --check` and `cargo check-workspace` before each
commit. Install [pre-commit](https://pre-commit.com), then from the repo root:

```sh
pre-commit install
```

To format and fix before committing, run `cargo fmt` and `cargo check-workspace`
yourself when the hook fails.
