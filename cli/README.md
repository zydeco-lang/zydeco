# Zydeco CLI

Use `zydeco` to check, run, test, and compile source programs.
The examples below run from the repository root with an installed CLI;
[CONTRIBUTING](../CONTRIBUTING.md#build-the-cli) covers building and installation.
During development, replace `zydeco` with `cargo run --bin zydeco --`.
Use `zydeco <command> --help` for the complete option list.

## Run and Test Programs

```sh
zydeco check lib/std/builtin.zy
zydeco run path/to/main.zy -- first-argument second-argument
zydeco run path/to/main.zy -t wasm-sps
```

`check` accepts library terms as well as executable programs.
The [quick start](../README.md#quick-start) supplies a complete runnable example;
the [execution profiles reference](../docs/references/language/src/execution-profiles.md) defines each
backend's runtime model, memory limits, and host boundary.

From the repository root, select declared packages by name or tests by file:

```sh
zydeco show
zydeco check -p /std -p /std/memory
zydeco test lib/tests/std/bool.zy -t all
```

See [source packages](../docs/references/language/src/sources/packages.md#source-packages) for declarations
and discovery, and the [standard library guide](../lib/std/README.md#source-packages-and-tests)
for fixture-dependent tests.
Native execution uses `runtime/` by default; `--runtime-dir PATH` selects another source directory.
WebAssembly execution produces standalone WASI (`wasm32-wasip1`) executables and runs them with `wasmtime` (`WASMTIME` selects the executable, falling back to `node` `node:wasi`).

### Selecting an Execution Backend

Backend selection is command-line execution policy, independent of package meta annotations.
`run SOURCE -t TARGET` selects one of `interpreter`, `exe` (AMD64), or `wasm-sps`.
`--target` is the long spelling; omitting it selects `interpreter`.
`run` inherits terminal streams, forwards arguments after `--`, and returns the program's exit status.
`--dry` checks and selects the executable root without linking, emitting code, or requiring backend tools.

`test SOURCE` accepts the same targets and allows repeated `-t` options.
The test-only selection `-t all` expands in place to `interpreter`, `exe`, and `wasm-sps`, in that order.
Explicit choices replace the interpreter default; duplicates are removed, keeping first-occurrence order.
Discovery and source checking are shared across backends, with one lowering per test for compiled targets.
All selected tests and requested backend artifacts are prepared before any test executes.
Source errors, unsupported lowering, and missing toolchains fail preparation rather than skipping a backend.

Execution is sequential: each test in package-plan order runs on its backends in requested order.
Every test/backend pair reports a labeled result.
Zero exits pass; nonzero exits display captured stdout and stderr.
Runtime errors also fail their pair, and remaining pairs still run.
The summary counts pairs, and the command fails if any pair fails.
Wasm execution requires `wasmtime` (or Node.js `node:wasi` fallback); native execution requires the AMD64 tools and runtime sources.

## Interactive REPL

Start with `zydeco repl`; use `cargo run -p zydeco-tui` when developing the frontend.
Each input contains one complete term.
`@(import(1))` reuses source from input `[1]`.

| Input | Action |
| --- | --- |
| `ret 1` | Evaluate a returning computation. |
| `@[type] ret 1` | Inspect its type. |
| `@[run] expression` | Request execution, supplying a supported Builtin host contract when needed. |
| `@(help)` | Show commands and editing keys. |
| `@(quit)` | Exit. |

Enter submits complete syntax or adds a line to an incomplete term.
Alt+Enter inserts a newline; Ctrl+Enter submits the current text for diagnostics.
After a type checking rejection, edit and retry with the same input number.
The [interactive engine](../docs/references/compiler/src/tooling/interactive-and-docs.md#interactive-engine)
explains source history, repeated effects, and captured execution.

## Build Artifacts

`zydeco build SOURCE --target TARGET` emits the selected artifact.

| Target | Output |
| --- | --- |
| `zir` | First-order SPSLow on stdout. |
| `zasm` | ZASM on stdout. |
| `asm` | AMD64 assembly on stdout. |
| `exe` (default) | AMD64 executable. |
| `wasm-sps` | WebAssembly module. |
| `object` | Native Zydeco unit or C library with interfaces and manifest. |
| `staticlib`, `sharedlib` | C library with interfaces and manifest. |

`--execute` runs the linked `exe` artifact.
`zasm` and `wasm-sps` reject it; use `run -t TARGET` to execute a program on the interpreter or WebAssembly.

### Native Executables

Install `nasm`, `ar`, a linker, and the Rust target for the selected OS.
Running the output requires AMD64 execution support.
On Linux or macOS:

```sh
zydeco build path/to/main.zy --target exe --target-arch x86-64 --build-dir build --runtime-dir runtime
```

The result is `build/main.exe`. Add `--execute` to run it after linking.
Architecture and OS default to the host; `--target-arch` and `--target-os` select them explicitly.
Native targets reject every architecture other than AMD64 before compiling,
so an ARM host must pass `--target-arch x86-64`.
Build and runtime directories default to `build/` and `runtime/` relative to the working directory.

Native linking copies runtime sources into the build directory, writes the compiler's embedded model
to `build/machine/`, caches optimized `rustc` builds of `zydeco-machine` and `zydeco-runtime`
under `ZYDECO_RUNTIME_CACHE_DIR` (or the system temporary cache directory) with zero external crate dependencies,
and links the generated archive against them.
Keep runtime edits in the source directory and rebuild the compiler after model changes.
Custom manifests inherit `zydeco-machine` through workspace dependencies,
following [runtime/Cargo.toml](../runtime/Cargo.toml).
See the [shared model contract](../DESIGN.md#shared-rust-runtime-model) for compatibility checks.

Cached runtime compilation uses optimization level 2, single codegen-unit emission,
PIC relocation, aborting panics, and `lto = "off"` to preserve exports referenced
by generated assembly while linking warm executables directly through `rustc`.
The `native_model` module of `suite` checks cached and release runtime linking and execution.

### WebAssembly

Build a module from first-order SPSLow:

```sh
zydeco build path/to/main.zy --target wasm-sps --build-dir build
```

The command links `build/main.sps.wasm`, a standalone WASI (`wasm32-wasip1`) module importing only `wasi_snapshot_preview1`.
Use `run -t wasm-sps` for compilation and execution, or run an existing module directly with `wasmtime`:

```sh
wasmtime run -W tail-call=y build/main.sps.wasm
```

The [WebAssembly backend](../DESIGN.md#webassembly-backend) describes the compiled object and WASI runtime linking.

### C Libraries

Build the [arithmetic example](../lib/ffi/arithmetic.zy):

```sh
zydeco check lib/ffi/arithmetic.zy
zydeco build lib/ffi/arithmetic.zy --target sharedlib --target-arch x86-64 --build-dir build --runtime-dir runtime
```

The command publishes `build/example.arithmetic.sharedlib.library.json`, a C header,
a Zydeco import interface, and the platform library as links into an immutable bundle.
Distribute the bundle with its dependencies.
The [C export rules](../docs/references/language/src/foreign-interfaces.md#compiled-libraries-and-c-exports)
define the supported source interface and call behavior.

Save a C consumer as `consumer.c`:

```c
#include "example.arithmetic.h"
#include <assert.h>
int main(void) {
    assert(example_add(19, 23) == 42);
    return 0;
}
```

On AMD64 Linux, compile and run it with:

```sh
cc consumer.c -Ibuild -Lbuild -lexample.arithmetic -Wl,-rpath,"$PWD/build" -o consumer
./consumer
```

On macOS, add `-arch x86_64` to the compiler command.
For a Zydeco consumer, save this as `consumer.zy` in the repository root:

```zydeco
param (/Ret; /Int; /OS; /numeric; /process) : @(import("lib/std/builtin.zy")) in
let api = @(import("build/example.arithmetic.imports.zy")) in
do sum <- ! api/add 19 23;
! numeric/int/eq OS sum 42 { ! process/exit 0 } { ! process/exit 1 }
```

Pass the manifest when building or running:

```sh
zydeco build consumer.zy --target exe --target-arch x86-64 --build-dir client-build \
  --link-library build/example.arithmetic.sharedlib.library.json
zydeco run consumer.zy -t exe --link-library build/example.arithmetic.sharedlib.library.json
```

Repeat `--link-library` for dependencies; `test` accepts it too.
For object and static-library output, select `--target object` or `--target staticlib`.
The [artifact contract](../docs/references/compiler/src/native-emission.md#compilation-unit-preparation-and-artifacts)
explains runtime support, link order, and compatibility.
Run the focused regressions with `cargo test -p zydeco-cli --test library -- --test-threads=1`.

### Native Zydeco Units

Save this producer as `math.zy` in the repository root:

```zydeco
@[package(library(zydeco), name(example/math))]
param val (/Thk; /Ret; /Int; /numeric) : @(import("lib/std/builtin.zy")) in
(#make_adder = ({ fn (delta : Int) =>
  ret { fn (value : Int) => ! numeric/int/add delta value }
} : Thk (Int -> Ret (Thk (Int -> Ret Int)))))
```

Build its initializer object and interface:

```sh
zydeco check math.zy
zydeco build math.zy --target object --target-arch x86-64 --build-dir unit-build --runtime-dir runtime
```

This publishes `unit-build/example.math.unit.json`, `example.math.unit.o`,
and `example.math.imports.zy` as links into an immutable bundle.
Distribute the bundle with its dependencies.
Save this consumer as `client.zy` in the repository root:

```zydeco
param (/Ret; /Int; /OS; /numeric; /process) : @(import("lib/std/builtin.zy")) in
let initialize = @(import("unit-build/example.math.imports.zy")) in
do math <- ! initialize;
do add_seven <- ! math/make_adder 7;
do result <- ! add_seven 35;
! numeric/int/eq OS result 42 { ! process/exit 0 } { ! process/exit 1 }
```

Build with the unit manifest:

```sh
zydeco build client.zy --target exe --target-arch x86-64 --build-dir client-build --runtime-dir runtime \
  --link-library unit-build/example.math.unit.json
```

Repeat `--link-library` for multiple providers.
The [native unit contract](../docs/references/language/src/foreign-interfaces.md#native-zydeco-units)
defines supported types, initialization, and compatibility.
Run focused regressions with `cargo test -p zydeco-cli --test unit --test unit_manifest`.

## Representation Experiments

Select a representation strategy per build:

```sh
zydeco build lib/tests/core/representation-policies.zy --target asm --target-arch x86-64 --representation shared
zydeco build lib/tests/core/representation-policies.zy --target wasm-sps --representation boxed
```

[Policy selection](../docs/references/compiler/src/zasm.md#policy-selection) defines the strategies and target scope.
Compare static allocation sites and field words with:

```sh
cargo run -p zydeco-cli --example representations -- \
  lib/tests/core/representation-policies.zy lib/tests/core/gc-stress.zy
```

Collect runtime timings separately with `scripts/bench-native.sh --representation shared 5 path/to/main.zy`,
recording the [runtime profile](#native-executables).
Validate selection and execution with:

```sh
cargo test -p zydeco-assembly --lib
cargo test -p zydeco-cli --test representation
cargo test -p zydeco-tests --test suite -- representation_policies::
```
