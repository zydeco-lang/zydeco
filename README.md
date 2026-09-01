# Zydeco 🪗

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14948044.svg)](https://doi.org/10.5281/zenodo.14948044)

Zydeco is a proof-of-concept programming language based on Call-by-push-value.

## OOPSLA Artifact Evaluation

The instructions are located in [lib/tests/oopsla/README.md](lib/tests/oopsla/README.md).

## Documentation

- [DESIGN.md](DESIGN.md): language model and implementation architecture
- [CONTRIBUTE.md](CONTRIBUTE.md): build, test, and development workflows

## Running Zydeco

Create a file `hello-world.zy` in the repository root:

```zydeco
param (
  (/system) :
  @[import("lib/std/builtin.zy")] _
) in
  let (/stdio; /process) = system in
  ! (stdio/write_line) "hello, world!" { ! (process/exit) 0 }
```

Then run

```bash
cargo run -- run hello-world.zy
hello, world!
```

Alternatively, run
```bash
cargo build --release
```
to build the executable which will be stored at `target/release/zydeco`.

Then run
```bash
./target/release/zydeco run hello-world.zydeco
```
and see
```console
hello, world!
```

Run `zydeco --help` for further usage information.

## Building WebAssembly

Zydeco has two experimental WebAssembly lowering strategies. They can be built side by side:

```sh
zydeco build hello-world.zy --target wasm-am --build-dir build
zydeco build hello-world.zy --target wasm-sps --build-dir build
```

The first command lowers ZASM as an abstract machine and writes `build/hello-world.am.wasm`.
The second starts from first-order SPSLow and writes `build/hello-world.sps.wasm`, compiling each SPS block
as structured WebAssembly rather than materializing every ZASM instruction.
Both generated modules export `entry`, `_start`, and `memory`.
Builtin operations are imports from the `zydeco` namespace,
so an application must provide the [host ABI described in `DESIGN.md`](DESIGN.md#webassembly-backend)
before calling `entry`.
The CLI does not execute WebAssembly modules because it is not itself a host embedding.

## Interactive REPL

Start the full-screen terminal REPL with:

```sh
zydeco repl
```

Each submitted term receives an input number such as `[1]`.
A later term can hygienically splice that source with `@[import(1)] _`; the integer is deliberately unquoted,
because `@[import("1")] _` denotes a file named `1`.
The REPL uses root metadata annotations for its commands:

```zydeco
@[type] ret 1
@[run] ret 1
@[help] _
@[quit] _
```

Press Enter to evaluate complete syntax or continue an incomplete term on a new line.
Alt+Enter always inserts a newline, and Ctrl+Enter submits the current text for diagnostics.
A type checking error leaves the editor and input number unchanged so the source can be corrected and retried.

## Intro to Zydeco

For a complete source-level guide, see [docs/tutorial/zydeco-guide.md](docs/tutorial/zydeco-guide.md).

We now have a toy "literate zydeco" written in zydeco!
Try it out by running
```bash
cd docs/spell && make build
```
and the product will show up right in the folder - which is also a series of guide to programming in zydeco.
Maybe we should call it "co-literate zydeco" because it turns commented zydeco into markdown.

A legacy version of the tutorial lies [here](docs/tutorial/intro_to_zydeco.md).
You might find the short tutorial easier to follow if the previous `spell` guide goes too fast.

We will develop more introductory material on zydeco when we have implemented more features.
For now, you can also choose to browse `lib/` for some example programs.

To run all tests
```bash
cargo test-all
```

## Editor Support

The `cajun` binary is Zydeco's Language Server Protocol implementation.
It provides live syntax and name-resolution diagnostics, document symbols, clickable filesystem imports,
and go to definition across imported files:

```bash
cargo install --path editor/cajun
```

Client integrations live under [`editor/vscode`](editor/vscode) and [`editor/zed`](editor/zed).

## Rust API Docs

To generate documentation for a package:
```bash
cargo doc --package zydeco-statics --no-deps
```
The output is written to `target/doc/`.

## Repository Structure

```plain
.
├── Cargo.toml
├── lang
│  ├── derive
│  ├── syntax
│  ├── surface
│  ├── statics
│  ├── session
│  ├── dynamics
│  ├── stackir
│  ├── assembly
│  ├── amd64
│  ├── llvm
│  ├── wasm-am
│  ├── wasm-sps
│  ├── tests
│  └── utils
├── cli
├── tui
├── web
└── ...
```

- `lang/`: the library implementing the parser, type checker and interpreter for the Zydeco language.
- `lib/`: [standard library](lib/std/README.md), reusable examples, and `lib/tests/` regression projects
- `cli/` Command-line interface
- `tui/` Ratatui frontend for the interactive REPL
- `web/` Web interface

## Related Literature

Zydeco is based on the Call-by-push-value calculus introduced by Paul Blain Levy:
https://dl.acm.org/doi/10.1145/3537668.3537670

## Related Language Implementations

- Fiddle : <https://github.com/maxsnew/modal-scheme>
- Riddle: <https://github.com/UMjoeypeng/riddle_compiler>
