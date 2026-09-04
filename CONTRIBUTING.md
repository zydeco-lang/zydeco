# Contributing

This guide covers working on the current checkout.
Start with [README.md](README.md) to run a first program and [DESIGN.md](DESIGN.md)
for language semantics and implementation boundaries.
Commands below run from the repository root unless a step explicitly changes directories.
Repository conventions, including dependency placement and commit prefixes, are in [AGENTS.md](AGENTS.md).

## Tooling

The workspace declares its Rust version and edition in [Cargo.toml](Cargo.toml): currently Rust 1.97 and edition 2024.
Install `rustfmt` and `clippy` for Rust formatting and linting.
Additional tools depend on the work:

| Work | Additional requirements |
| --- | --- |
| Build the CLI or interpreter on Unix | A C toolchain and `make`; Cargo builds libffi from bundled sources. |
| Build AMD64 executables | `nasm`, `ar`, a linker, and the Rust target for the selected OS. |
| Execute WebAssembly tests | Node.js 24, as configured in CI; `NODE` can select the test executable. |
| Work on the Tree-sitter grammar | Node.js and the pnpm version declared in its `package.json`. |
| Generate the literate chapters | `make`, `zsh`, and `jq`, in addition to the Rust build tools. |
| Reflow Markdown | Python 3. |

The interpreter's returning C imports and their platform requirements are described
in the [C FFI design](docs/proposals/c-ffi.md).
Installing an external library is necessary only for programs that import it.
The old `web/` frontend is outside the active workspace; its README is not a supported build workflow for this checkout.

## Build the CLI

```sh
cargo build --bin zydeco --release
```

The executable is `target/release/zydeco`.
Use it directly or install it with `cargo install --path cli`.
While iterating, use `cargo run --bin zydeco -- <command>` to build and invoke the development binary.
Examples below use `zydeco` to mean the installed CLI.

## Check and Run Source Terms

Check a reusable source independently, or run an executable source with optional program arguments:

```sh
zydeco check lib/std/prelude.zy
zydeco run lib/tests/oopsla/polynomial.zydeco
zydeco run path/to/main.zy -- first-argument second-argument
```

`check` accepts any complete term that synthesizes its classifier, including types and library values.
`run` additionally requires the executable Builtin package boundary.
`run --dry` performs analysis and selects an executable root without evaluating it;
it does not exercise runtime Builtin linking or load foreign libraries.
Use `--lint-types` with a checking command to run the compiler's additional typed-arena invariant checks:

```sh
zydeco --lint-types check path/to/library.zy
```

A program is one complete source term.
Imports appear at their use sites and resolve relative to the importing file.
No authored project configuration or `main` declaration is needed.
A companion `foo.zyi` is an optional, independently checked type contract for `foo.zy`.
Use it when maintaining that contract separately is useful; otherwise, annotate introductions
in place or let `pack` synthesize an existential package type.
`pack` currently accepts type witnesses but not kind witnesses such as `VType` and `CType`.
The [package design](docs/proposals/package-modularization.md#package-annotations-and-companion-files)
explains the prelude's annotated kind prefix and its use of `@[typeof]` to reuse the remaining package type.

## Compile Programs

`zydeco build` first checks the same executable source boundary as `run`, then selects a lowering target:

| Target | Result |
| --- | --- |
| `zir` | Print first-order SPSLow to stdout. |
| `zasm` | Print ZASM to stdout; `--execute` instead runs the ZASM interpreter. |
| `asm` | Print AMD64 assembly to stdout. |
| `llvm` | Print experimental LLVM IR to stdout. |
| `exe` (default) | Assemble and link an AMD64 executable. |
| `llvm-exe` | Attempt an experimental LLVM executable build with `clang`. |
| `wasm-am`, `wasm-sps` | Write a WebAssembly module using the selected strategy. |

For AMD64 on Linux or macOS, with the corresponding toolchain installed:

```sh
zydeco build path/to/main.zy --target exe --target-arch x86-64 --build-dir build --runtime-dir runtime
```

The result is `build/main.exe`, including on Unix.
Add `--execute` to launch it after linking.
The architecture and OS default to the host; an ARM host must explicitly select `x86-64`
for AMD64 output and needs the corresponding target tools and execution support.
Selecting `--target-os` or `--target-arch` does not install a cross-compilation toolchain.
The default build and runtime directories are `build/` and `runtime/`, relative to the command's working directory.
AMD64 linking copies runtime sources into the build directory and invokes Cargo there,
which may fetch runtime dependencies.
LLVM emission and linking remain experimental and are outside the standard source-test backend matrix.

Build both WebAssembly variants with:

```sh
zydeco build path/to/main.zy --target wasm-am --build-dir build
zydeco build path/to/main.zy --target wasm-sps --build-dir build
```

These targets write `build/main.am.wasm` and `build/main.sps.wasm`;
`--execute` is rejected because the CLI has no host embedding.
For local experiments, the repository's Node.js test host can run either module:

```sh
node lang/tests/wasm-host.mjs build/main.am.wasm
node lang/tests/wasm-host.mjs build/main.sps.wasm
```

This is a test host: randomness is deterministic, and its argument fold rejects two or more program arguments.
See the [WebAssembly ABI and limitations](DESIGN.md#webassembly-backend) before writing another embedding.

## Use the Interactive REPL

```sh
zydeco repl
```

Use `cargo run -p zydeco-tui` to work directly on the Ratatui frontend.
The REPL accepts one complete term per numbered input.
`@[import(1)] _` refers to an earlier input; `@[import("1")] _` refers to a file named `1`.

| Root annotation | Behavior |
| --- | --- |
| `@[type] expression` | Inspect the term and its classifier without evaluating it. |
| `@[run] expression` | Explicitly request evaluation, including supplying a supported Builtin host contract. |
| `@[help] _` | Show commands and editing keys. |
| `@[quit] _` | Exit the REPL. |

Without a command, the REPL inspects kinds and types and evaluates values or directly returning computations.
Use `@[run]` for computations requiring the host package; arbitrary unapplied functions still need arguments.
REPL evaluation captures output and supplies empty stdin and an empty argument list.

Enter submits complete syntax and adds a line for incomplete syntax.
Alt+Enter always inserts a newline; Ctrl+Enter submits even incomplete text for diagnostics.
A type checking rejection keeps the editor and input number unchanged for correction and retry.

## Format and Lint

```sh
cargo fmt --all
cargo clippy-all -- -D warnings
```

The Clippy command matches CI. Cargo aliases are defined in [.cargo/config.toml](.cargo/config.toml).
Format explicitly named Zydeco files with:

```sh
zydeco fmt path/to/main.zy path/to/library.zy
```

`--check` lists files that would change, leaves them untouched, and exits unsuccessfully if any need formatting.
Formatting policy comes from source annotations scoped to their payload:

```zydeco
@[format(width(100), indent(4), layout(blank_lines))] begin
  ret 1
end
```

`width` and `indent` set columns. `layout(preserve)` retains observed line breaks and blank lines by default;
`layout(blank_lines)` preserves only blank lines, and `layout(ignore)` lets width determine optional breaks.
`parentheses(minimal)` and `parentheses(preserve)` control singleton groups;
`verbatim` preserves the payload's source text.
Nested annotations override enclosing options.
The [formatting design](docs/proposals/formatting.md) describes the complete policy.

`cargo fmt` does not format Zydeco embedded in Rust strings.
After a repository-wide Zydeco formatting pass, run affected Rust tests and update embedded fixtures or snapshots.
The command for all tracked Zydeco files is in [AGENTS.md](AGENTS.md#validation-and-formatting).

## Run Tests

Start with the affected crate, integration target, or test-name filter:

```sh
cargo test -p zydeco-surface --lib
cargo test -p zydeco-statics --test foreign
cargo test -p zydeco-tests --test value_pi
```

The source harness in [lang/tests/src/lib.rs](lang/tests/src/lib.rs) provides three common registrations:

| Macro | Coverage |
| --- | --- |
| `check_source!` | Check the source without executing it. |
| `runtime_source!` | Interpreter and both WebAssembly backends. |
| `e2e_sources!` | Interpreter, AMD64, and both WebAssembly backends. |

Run only the WebAssembly cases when working on those backends:

```sh
cargo test -p zydeco-tests wasm_ -- --test-threads=1
```

`NODE` selects the Node.js executable used by the harness.
Native end-to-end tests invoke the assembler, linker, and a separate Cargo runtime build;
they require the target toolchain even when the compiler itself runs on another architecture.
Tests for installed foreign libraries or optional native FFI execution are marked ignored;
see the [C FFI checks](docs/proposals/c-ffi.md#examples-and-checks) before enabling them.

The full CI commands are:

```sh
cargo test-all --release
cargo test-doc-all --release
```

Use the full suite for an intentional broad verification pass; agents run it only on explicit user request.
`test-all` expands to all workspace targets but excludes doctests, which the second command runs separately.
See [.github/workflows/rust.yml](.github/workflows/rust.yml) for CI's tool installation and additional checks.

## Work on Editor Support

Install Cajun with `cargo install --path editor/cajun`.
The [editor guide](editor/README.md) links client setup and documents live configuration.
For the Tree-sitter grammar:

```sh
cd editor/tree-sitter-zydeco
pnpm install --frozen-lockfile
pnpm check
```

`pnpm check` regenerates the parser and checks the grammar corpus, Zed queries, and repository source corpus.
Commit generated changes under `editor/tree-sitter-zydeco/src` together with the grammar.
The compiler's Logos lexer and LALRPOP grammar remain the authority for accepted programs.

## Generate Documentation

Generate Rust API documentation for an affected package:

```sh
cargo doc --package zydeco-statics --no-deps
```

The output is written to `target/doc/`.
Generate Markdown from the executable literate chapters with:

```sh
make -C docs/spell build
```

The script builds the release CLI and writes chapter Markdown beside the sources under `docs/spell/`.
After editing Markdown prose, reflow the named files:

```sh
python3 docs/scripts/reflow-markdown.py path/to/document.md
python3 docs/scripts/reflow-markdown.py --check --changed
```

`--changed` selects tracked Markdown changes from `HEAD`.
The formatter preserves structural Markdown, including fences, tables, headings, block quotes, and link definitions.
Reflow Typst and other specialized formats manually.

## Pre-commit Hooks and Releases

Optional hooks run `cargo fmt -- --check` and `cargo check-all` before each commit.
With `pre-commit` installed, enable them using `pre-commit install` from the repository root.

Releases are triggered by pushed `v*` tags.
Choose the next version, update the workspace version in `Cargo.toml` and the lockfile,
and commit before tagging that same version.
The [release workflow](.github/workflows/release.yml) uses [cliff.toml](cliff.toml) to generate notes
and creates a GitHub release; it does not build or upload release binaries.
Follow the [commit convention](AGENTS.md#commit-messages) so changes appear under the intended release-note category.
