# Contributing

This repository includes the Zydeco language implementation, its standard library, and a collection of examples.
The notes below collect the most common workflows described in the project documentation.

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

The REPL accepts one complete source term per numbered input.
Import an earlier input with `@[import(1)] _`; numeric targets are session inputs,
while quoted targets such as `@[import("library.zy")] _` remain filesystem sources.
Commands are ordinary root annotations: `@[type] expression`, `@[run] expression`, `@[help] _`, and `@[quit] _`.
Type checking errors remain on the current input number so the source can be edited and submitted again.

## Format Source Files

Format one or more Zydeco source files in place:

```sh
zydeco fmt path/to/main.zy path/to/library.zy
```

Add `--check` to report files that would change instead of writing them; the command then exits unsuccessfully
when at least one file would change.

Formatting policy comes from `@[format(...)]` annotations in the source, scoped to the annotated expression:

```zydeco
@[format(width(100), indent(4), layout(blank_lines))] begin
  ...
end
```

- `width(columns)` and `indent(columns)` set the line width and indentation width;
- `layout(preserve)` keeps observed line breaks and blank lines (the default);
- `layout(blank_lines)` keeps only blank lines while the width decides every single break;
- `layout(ignore)` lets the width decide every optional break;
- `parentheses(minimal)` or `parentheses(preserve)` selects singleton-group treatment;
- `verbatim` copies the annotated expression's original source text unchanged.

Nested annotations override enclosing options for their payload subtree.

## Reflow Markdown Documentation

Reflow explicitly named Markdown files after editing their prose:

```sh
python3 docs/scripts/reflow-markdown.py path/to/document.md
```

Use `--changed` to select only tracked Markdown files changed from `HEAD`.
Combine it with `--check` to report files that need reflowing without writing them:

```sh
python3 docs/scripts/reflow-markdown.py --check --changed
```

The formatter preserves structurally significant Markdown, including code fences, tables,
headings, block quotes, link definitions, hard breaks, and indented code.
Reflow Typst and other specialized document formats manually.

## Run Programs and Examples

Run a Zydeco source file directly:

```sh
zydeco run path/to/main.zy
```

For example, the declaration-free OOPSLA polynomial root runs with:

```sh
zydeco run lib/tests/oopsla/polynomial.zydeco
```

Compile the same source pipeline with either WebAssembly lowering strategy:

```sh
zydeco build path/to/main.zy --target wasm-am --build-dir build
zydeco build path/to/main.zy --target wasm-sps --build-dir build
```

The commands write distinct `.am.wasm` and `.sps.wasm` artifacts. Both modules require the `zydeco` host imports
documented in `DESIGN.md`.

## Run Tests

The source harness exercises the interpreter, AMD64, and both WebAssembly backends.
Cases declared with `e2e_sources!` run on all four; `runtime_source!` cases run on the interpreter
and both WebAssembly implementations. WebAssembly execution uses the Node.js 24 test host
in `lang/tests/wasm-host.mjs`; set `NODE` to select a different compatible Node.js executable.

Run only the WebAssembly corpus while iterating on either backend:

```sh
cargo test -p zydeco-tests wasm_ -- --test-threads=1
```

The repository provides an aggregate test command:

```sh
cargo test-all --release
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

Commit the generated files under `editor/tree-sitter-zydeco/src` whenever the grammar changes.
The compiler's Logos lexer and LALRPOP grammar remain the authority for accepted Zydeco programs.

## Create a Zydeco Program

A Zydeco program is one complete term in one source file.
Source dependencies are ordinary term imports written at their use sites:

```zydeco
param (/system) : @[import("../std/builtin.zy")] _ in
  let (/process) = system in
  ! (process/exit) 42
```

The launcher supplies the declared Builtin package when this term is run.
No authored project configuration or distinguished `main` declaration is needed.

## Work on the Language Implementation

The language pipeline is structured into phases (parsing, desugaring, name-resolution,
type-checking, linking, evaluation) under `lang/`.
Each phase typically includes `syntax`, `arena`, `err`, `fmt`, and `span` modules
to keep the structure consistent across crates.

## Generate Rust API Docs

To generate documentation for a package (for example, `zydeco-statics`):

```sh
cargo doc --package zydeco-statics --no-deps
```

The HTML output is written to `target/doc/`.

## Pre-commit hooks

Optional hooks run `cargo fmt -- --check` and `cargo check-all` before each commit.
Install [pre-commit](https://pre-commit.com), then from the repo root:

```sh
pre-commit install
```

To format and fix before committing, run `cargo fmt` and `cargo check-all` yourself when the hook fails.
