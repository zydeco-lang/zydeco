# Zydeco 🪗

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14948044.svg)](https://doi.org/10.5281/zenodo.14948044)

Zydeco is a proof-of-concept programming language based on call-by-push-value (CBPV).
It explores explicit control flow, stack-manipulating computation, and relative monads.
Its [term-oriented design](docs/references/language.md#term-oriented-composition) uses expressions
for type construction, local programming, and library composition.

The repository includes an interpreter, a terminal REPL, a compiler to native AMD64 executables,
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
param (/stdio; /process) : @(import("lib/std/builtin.zy")) in
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

A source file contains one complete term.
Quoted imports are relative to the importing file; there is no ambient provider or distinguished `main` declaration.
The [core prelude](docs/references/language.md#core-prelude) supplies common kind and type names by default;
provider choice remains explicit, and `OS` refers to the selected opening's witness.
The example above explicitly accepts the host's Builtin value and selects the operations it needs.
The launcher supplies it when running the program.

Host-independent library terms need no prelude setup:

```zydeco
fn (value : Int) => (ret value : Ret Int)
```

`@(import("path"))` abbreviates `@[import("path")] _`.
`zydeco check` accepts library terms as well as executable programs.
`zydeco run` requires a computation accepting the Builtin value and ending in its `OS` protocol;
a pure function or `ret 1` can pass checking without being a runnable file root.
The [standard library guide](lib/std/README.md) describes the Builtin interface, library assembly,
and available operations.

Prefer complete files as package entry points.
Name one with `@[package(library, name(example/math))]` and reuse it with `@(import(/example/math))`.
Binary and test roles use the same annotation; `@[package(test(of(/example/math)))]` associates a test
from the test side alone.
The CLI detects `package.zy` and `workspace.zy` in the working directory;
their include/exclude globs collect additional package declarations.
Use `-p NAME` (`--pkg` or `--package`) after the command to select a declared package.
Repeat it to check, build, or test multiple packages; it does not load additional declaration files.
From this repository's root, `zydeco show` lists declarations and `zydeco check -p /std` checks std.
Select a test explicitly, for example `zydeco test lib/tests/std/bool.zy`.
[Suite selection from relationships](docs/ideas/README.md#project-context-inspection) remains planned.
Both `run` and `test` default to the interpreter; `-t` (`--target`) selects a backend.
Repeat it for a test matrix: `zydeco test lib/tests/std/bool.zy -t interpreter -t exe`.
Quoted file imports and direct CLI paths also work without named package declarations.
See the [CLI guide](cli/README.md#run-and-test-programs) for command examples
and the [source package reference](docs/references/language.md#source-packages) for declarations and discovery.

## Interactive REPL

Start the terminal REPL with:

```sh
zydeco repl
```

Enter `ret 1` to evaluate a returning computation, or `@[type] ret 1` to inspect its type.
Each submitted term has a number such as `[1]`; `@(import(1))` refers to that source in a later input.
`@(help)` lists commands, and `@(quit)` exits.
A type checking rejection keeps the current text and number available for correction.
See the [REPL workflow](cli/README.md#interactive-repl) for submission keys and explicit execution.

## Compiling Programs

The compiler lowers a checked program through stack-passing style to a native AMD64 executable.
Compile and run `hello-world.zy` in one step, or build the executable:

```sh
zydeco run hello-world.zy -t exe
zydeco build hello-world.zy --target exe --target-arch x86-64
```

`build` defaults to the host architecture, so an ARM host passes `--target-arch x86-64`.
The [native executable guide](cli/README.md#native-executables) lists the required tools,
and the [compiler reference](docs/references/compiler.md#selecting-and-inspecting-passes) explains how
to inspect the stack-passing-style program with `--target zir`.

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
- [References](docs/references/README.md): language and compiler implementation references.
- [Outstanding discrepancies](docs/todos/README.md): documentation and implementation drift requiring repair.
- [Source documentation](docs/references/language.md#source-documentation): `@[doc]` annotations and attached prose.
- [DESIGN.md](DESIGN.md): semantics, implementation boundaries, and current limitations.
- [CONTRIBUTING.md](CONTRIBUTING.md): build, test, formatting, and contribution workflows.
- [Standard library](lib/std/README.md): types, capabilities, and library composition.
- [OOPSLA artifact instructions](lib/tests/oopsla/README.md): artifact-specific examples and evaluation instructions.
- [Literate chapters](docs/spell): executable Zydeco chapters;
  see the [generation workflow](CONTRIBUTING.md#generate-documentation).

Reusable programs live under `lib/`, and regression projects live under `lib/tests/`.
The [repository map](DESIGN.md#repository-layout) locates the compiler, runtime, frontends, and editor integrations.

## Related Work

Zydeco builds on Paul Blain Levy's [call-by-push-value calculus](https://dl.acm.org/doi/10.1145/3537668.3537670).
Related language implementations include [Fiddle](https://github.com/maxsnew/modal-scheme)
and [Riddle](https://github.com/UMjoeypeng/riddle_compiler).
