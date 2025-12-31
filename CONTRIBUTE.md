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

Run a Zydeco project with a named binary:

```sh
zydeco run path/to/proj.toml --bin=foo
```

To run the OOPSLA examples in batch, use the provided script from the repo
root:

```sh
./run.sh
```

You can also run a specific OOPSLA example directly:

```sh
zydeco run lib/oopsla/proj.toml --bin=polynomial
```

## Run Tests

The repository provides an aggregate test command:

```sh
cargo test-all --release
```

## Create a New Zydeco Project

Zydeco projects are described by a `proj.toml` file. A minimal example is
shown in `lib/playground`:

```toml
name = "playground"
srcs = []
deps = [
  { local = "../std/proj.toml" },
]
bins = [
  "play.zydeco",
]
std = "nostd"
```

Binary files listed in `bins` must contain exactly one `main` declaration.

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
