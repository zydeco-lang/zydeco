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

## Run Programs and Examples

Run a Zydeco source file directly:

```sh
zydeco run path/to/main.zy
```

For example, the declaration-free OOPSLA polynomial root runs with:

```sh
zydeco run lib/tests/source/oopsla-polynomial.zy
```

## Run Tests

The repository provides an aggregate test command:

```sh
cargo test-all --release
```

## Create a Zydeco Program

A Zydeco program is one complete term in one source file.
Source dependencies are ordinary term imports written at their use sites:

```zydeco
param (
  (Int, Char, String, OS, api) :
  @[import("../std/builtin.zy")] _
) in
  ! (api/exit) 42
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

Optional hooks run `cargo fmt -- --check` and `cargo check-all` before each
commit. Install [pre-commit](https://pre-commit.com), then from the repo root:

```sh
pre-commit install
```

To format and fix before committing, run `cargo fmt` and `cargo check-all`
yourself when the hook fails.
