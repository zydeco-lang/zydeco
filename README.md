# Zydeco 🪗

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14948044.svg)](https://doi.org/10.5281/zenodo.14948044)

Zydeco is a proof-of-concept programming language based on call-by-push-value (CBPV).
It explores explicit control flow, stack-manipulating computation, and relative monads.
The repository includes an interpreter, a terminal REPL, experimental native and WebAssembly backends,
and editor support through the Cajun language server.

## Quick Start

Use the Rust toolchain required by [Cargo.toml](Cargo.toml), currently Rust 1.97 or newer.
On Unix, building the interpreter also requires a C toolchain and `make` for its bundled libffi dependency.
The commands and import paths below assume a checkout of this repository.

From the repository root, install the CLI:

```sh
cargo install --path cli
```

Create `hello-world.zy` in the repository root:

```zydeco
param (/system) : @[import("lib/std/builtin.zy")] _ in
  let (/stdio; /process) = system in
  ! (stdio/write_line) "hello, world!" { ! (process/exit) 0 }
```

Check and run it:

```sh
zydeco check hello-world.zy
zydeco run hello-world.zy
```

The program prints:

```text
hello, world!
```

During development, `cargo run --bin zydeco -- run hello-world.zy` runs the CLI directly from the checkout.
See [CONTRIBUTING.md](CONTRIBUTING.md#build-the-cli) for release builds and additional tooling requirements.

## Source Files and Libraries

A source file contains one complete term. Imports are relative to the importing file;
there is no implicit prelude, distinguished `main` declaration, or authored project manifest.
The example above explicitly accepts the host's Builtin package and selects the operations it needs.
The launcher supplies that package when running the program.

Pure library terms can import just the foundational kinds and types:

```zydeco
let (/Ret; /Int64) = @(import("lib/std/prelude.zy")) in
fn (value : Int64) => (ret value : Ret Int64)
```

`@(import("path"))` abbreviates `@[import("path")] _`.
`zydeco check` accepts library terms as well as executable programs.
`zydeco run` requires a computation accepting the Builtin package and ending in its `OS` protocol;
a pure function or `ret 1` can pass checking without being a runnable file root.
The [standard library guide](lib/std/README.md) describes the prelude, package assembly, and available operations.

## Interactive REPL

Start the terminal REPL with:

```sh
zydeco repl
```

Enter `ret 1` to evaluate a returning computation, or `@[type] ret 1` to inspect its type.
Each submitted term has a number such as `[1]`; `@[import(1)] _` refers to that source in a later input.
`@[help] _` lists commands, and `@[quit] _` exits.
A type checking rejection keeps the current text and number available for correction.
See the [REPL workflow](CONTRIBUTING.md#use-the-interactive-repl) for submission keys and explicit execution.

## Compiling Programs

The two experimental WebAssembly targets can be built side by side:

```sh
zydeco build hello-world.zy --target wasm-am --build-dir build
zydeco build hello-world.zy --target wasm-sps --build-dir build
```

These commands produce `build/hello-world.am.wasm` and `build/hello-world.sps.wasm`.
The modules require imports from the `zydeco` host namespace; the CLI does not execute them itself.
The [compilation workflow](CONTRIBUTING.md#compile-programs) explains native targets
and execution with the Node.js test host.
[DESIGN.md](DESIGN.md#webassembly-backend) describes the two lowering strategies, shared ABI, and limitations.

## Editor Support

Install the Cajun language server:

```sh
cargo install --path editor/cajun
```

Cajun provides diagnostics, hover types, name and import completion, formatting,
semantic tokens, and definition, reference, and rename support across imported files.
Client setup is documented for [Visual Studio Code](editor/vscode/README.md) and [Zed](editor/zed/README.md).
The [editor guide](editor/README.md) describes shared behavior and runtime configuration.

## Documentation and Examples

- [Language guide](docs/tutorial/zydeco-guide.md): a source-level walkthrough.
- [DESIGN.md](DESIGN.md): semantics, implementation boundaries, and current limitations.
- [CONTRIBUTING.md](CONTRIBUTING.md): build, test, formatting, and contribution workflows.
- [Standard library](lib/std/README.md): types, capabilities, and library composition.
- [OOPSLA artifact instructions](lib/tests/oopsla/README.md): artifact-specific examples and evaluation instructions.
- [Literate chapters](docs/spell): executable Zydeco chapters;
  see the [generation workflow](CONTRIBUTING.md#generate-documentation).
- [Earlier tutorial](docs/tutorial/intro_to_zydeco.md): historical material using an older language design.

Reusable programs live under `lib/`, and regression projects live under `lib/tests/`.
The [repository map](DESIGN.md#repository-layout) locates the compiler, runtime, frontends, and editor integrations.

## Related Work

Zydeco builds on Paul Blain Levy's [call-by-push-value calculus](https://dl.acm.org/doi/10.1145/3537668.3537670).
Related language implementations include [Fiddle](https://github.com/maxsnew/modal-scheme)
and [Riddle](https://github.com/UMjoeypeng/riddle_compiler).
