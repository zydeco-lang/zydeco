# 02 — Vertical Slice

> Scratch study note for re-onboarding. This chapter traces one tiny program through every
> compiler product; the [compiler reference](../../references/compiler.md) owns the contracts.

## Purpose and Setup

Reading the phase chapters in isolation makes every representation look reasonable. Running one
program through all of them shows what each boundary actually buys and what it erases. The route
below uses only commands that were executed for this manual on the snapshot revision
(`e2dd8979`, arm64 macOS; see [01 — Project Map](01-map.md#snapshot)).

```sh
cd /path/to/zydeco                 # repository root
cargo build --bin zydeco           # once; afterwards use target/debug/zydeco
mkdir -p target/study
```

The compiled executable lands at `target/debug/zydeco`. Use it directly below; keep
`cargo run --quiet --bin zydeco --` as the fallback when the debug binary is stale.
The `zydeco` installed on `PATH` is not used in this chapter because it lagged the checkout
and rejected current `lib/std/builtin.zy`.

## The Program

`target/study/hello.zy` (the README example, moved so its relative import still resolves):

```zydeco
param (/stdio; /process) : @(import("../../lib/std/builtin.zy")) in
! (stdio/write_line) "hello, study!" { ! (process/exit) 0 }
```

Read it as three pieces: accept the host `Builtin` package, force `stdio/write_line` with a string
and the suspended next action, and let `process/exit 0` terminate. The suspended action is a thunk,
so the value passed to `write_line` is a *suspended* computation, not the result of running it.

## The Ladder

| # | Command (from repo root) | Product observed | Why this step |
| --- | --- | --- | --- |
| 1 | `target/debug/zydeco check target/study/hello.zy` | `ScopedProgram` → typed acceptance, no output | Confirms parsing, desugaring, resolution, checking, finalization |
| 2 | `target/debug/zydeco run target/study/hello.zy` | `hello, study!` | Reference interpreter over the checked residual |
| 3 | `target/debug/zydeco build target/study/hello.zy -t zir` | 5 lines of SPSLow | First-order stack IR after SPS, normalization, closure conversion |
| 4 | `target/debug/zydeco build target/study/hello.zy -t zasm` | 12 lines of ZASM | Control-flow graph with stack instructions |
| 5 | `target/debug/zydeco build target/study/hello.zy -t asm --target-arch x86-64` | 123 lines of AMD64 | Register allocation, frame/root metadata, runtime ABI |
| 6 | `target/debug/zydeco build target/study/hello.zy -t exe --target-arch x86-64 --build-dir target/study/build` | `target/study/build/hello.exe` | Linked Mach-O x86-64 executable |
| 7 | `target/debug/zydeco run target/study/hello.zy -t wasm-sps` | `hello, study!` | Structured Wasm backend plus the bundled Node host |

Targets 5–6 need `--target-arch x86-64` on this arm64 host. Without it the compiler says:

```text
the AMD64 backend cannot target aarch64; the architecture defaults to the host, so pass `--target-arch x86-64`
```

## Step 1: Check Is Silent

```console
$ target/debug/zydeco check target/study/hello.zy
$ echo $?
0
```

Silence here means the whole front end accepted the file. Rejections are the interesting output;
a complete rejection example appears at the end of this chapter.

## Step 2: Interpret the Checked Residual

```console
$ target/debug/zydeco run target/study/hello.zy
hello, study!
```

The interpreter does not re-check: `lang/dynamics/src/link.rs` erases the checked computation into
the runtime AST and materializes the Builtin package, then `lang/dynamics/src/eval.rs` steps it.
The reference interpreter is the oracle every backend is compared against; keep it working when
changing lowering. See the [execution card](cards/05-execution.md) and
[C7](../../references/compiler.md#c7-linking-and-the-reference-interpreter).

## Step 3: ZIR — High SPS, Normalized, Then SPSLow

```console
$ target/debug/zydeco build target/study/hello.zy -t zir
[parameter:a0] CType
[root]
  <extern:write_line/2> arg("hello, study!") :: arg(pack-closure((), closure[a0] block [__closure_code__[8730#225]](environment: ()) • ->
    <extern:exit/1> arg(0) :: •)) :: •
```

This five-line output is already the **first-order SPSLow**, not high SPS: `-t zir` renders the
product after closure conversion ([C9](../../references/compiler.md#c9-closure-conversion-and-first-order-spslow)).
Read it inside out:

- The root block declares one type parameter `a0 : CType`; the linked Builtin package introduces it.
- `::` is the explicit stack: `<extern:write_line/2> arg("hello, study!") :: arg(...) :: •` says
  "call `write_line` with the string, then the packed closure, then the empty stack `•`".
- The exit continuation was not inlined; it became `pack-closure((), closure[a0] block [...])`,
  a value-existential closure package with an empty environment.
- `[__closure_code__[8730#225]]` is a compilation-scoped synthetic identifier. Do not memorize
  numbers; memorize the shape.

Inspect intermediate stages with the same command plus `--sps-passes none` (skip optional SPS passes)
or `--trace-passes` (timing per pass). The [stack IR card](cards/03-stackir.md) maps the options to
[C8](../../references/compiler.md#c8-sps-lowering-normalization-and-demand).

## Step 4: Why Normalization Matters

`lib/tests/core/fact.zy` is a 23-line program. Its residual shows the cost of the explicit Builtin
interface and what normalization removes:

```console
$ target/debug/zydeco build lib/tests/core/fact.zy -t zir | wc -l
231
$ target/debug/zydeco build lib/tests/core/fact.zy -t zir | grep -c '^\[parameter:'
161
$ target/debug/zydeco build lib/tests/core/fact.zy -t zir --sps-passes none | wc -l
6372
```

161 of the 231 lines declare the Builtin package's type parameters, and without SPS passes the
program expands to 6,372 lines of residual. Normalization's demand analysis removes most Builtin
projections that the program never observes. This is also why the workspace raises
`RUST_MIN_STACK` to 16 MiB and why the explicit reconstruction frames exist; both are cleanup
targets in the [candidate inventory](appendix-cleanup-candidates.md).

## Step 5: ZASM Makes Control Flow Explicit

```console
$ target/debug/zydeco build target/study/hello.zy -t zasm
[sym:__closure_code__[8737#0]] <label:__closure_code___8737_0>
  pop _[8737#1];
  push 0;
  <extern:exit/1:Control>
[sym:[8737#6]] "hello, study!"
[root]
  push __closure_code___8737_0;
  push ();
  pack <product:2/2>;
  push _8737_6;
  <extern:write_line/2:Control>
```

The block and its entry label became explicit, the closure package became `pack <product:2/2>`,
and the call became a control transfer that consumes the constructed stack. ZASM is where the
"stack" stops being lexical and becomes a machine resource; stack analysis and local
representation choices live here ([C10](../../references/compiler.md#c10-zasm-stack-analysis-and-local-representation-choices)).

## Step 6: AMD64 Assembly

```console
$ target/debug/zydeco build target/study/hello.zy -t asm --target-arch x86-64 | head -30
default rel
section .text
        extern zydeco_frame_step
        extern zydeco_abort
        extern zydeco_alloc_scanned
        extern zydeco_alloc_opaque
        ...
        extern zydeco_exit
        extern zydeco_write_line
        global zydeco_entry_frames_664a0a894a00301025b9bd6c710ed2b2
zydeco_entry_frames_664a0a894a00301025b9bd6c710ed2b2:
        lea rdi, [rel frame_entry_8737_3]
;;; pad the stack for the SysV call
        sub rsp, 8
        call zydeco_frame_step
        add rsp, 8
        mov rbp, rax
```

The emitted file is 123 lines for this program. Note the shape, not the names:

- `extern` declarations are the runtime ABI: allocation, abort/backtrace, frame stepping, and the
  builtin operations (`zydeco_write_line`, `zydeco_exit`). The runtime side lives in `runtime/`
  and `lang/machine/`, not in the emitter.
- The entry symbol encodes frame-root metadata; `.text` is followed by data for frames, roots, and
  string literals (the file ends with the UTF-8 bytes of `"hello, study!"`).
- SysV register conventions appear at the boundary; inside, the Zydeco stack discipline is kept.

C11 owns frame preparation and emission; the [native backend card](cards/04-native-backend.md)
lists the code entry points.

## Step 7: Build and Run the Native Executable

```console
$ target/debug/zydeco build target/study/hello.zy -t exe \
    --target-arch x86-64 --build-dir target/study/build
$ file target/study/build/hello.exe
Mach-O 64-bit executable x86_64
$ target/study/build/hello.exe
hello, study!
```

The build directory now contains the executable plus a freshly compiled copy of the native runtime
(`build/machine/`, `libzy<name>.a`, nested `target/x86_64-apple-darwin/...`). The nested runtime
build is a Cargo invocation that can fetch dependencies; expect the first one to take a minute.
On this arm64 host the x86-64 binary runs under Rosetta. Native execution is AMD64/Linux or
macOS only; an ARM host must select the target explicitly ([cli guide](../../../cli/README.md#native-executables)).

## Step 8: The WebAssembly Backend

```console
$ target/debug/zydeco run target/study/hello.zy -t wasm-sps
hello, study!
```

SPSLow reaches `lang/wasm-sps` without passing through ZASM. `wasm-sps` preserves the lexical
structure: each block becomes a Wasm function and jumps cross blocks through a private function
table, with host imports under one namespace. The runtime contract (tagged words, bump allocator,
host-call ABI) is in `lang/wasm-common`; the Node host is `cli/wasm/wasm-host.mjs`.
See [C13](../../references/compiler.md#c13-webassembly-backend-and-embedding). The abstract-machine
Wasm backend and the ZASM interpreter were removed on 2026-10-25; do not look for them.

## Step 9: Watch the Pass Pipeline

```console
$ target/debug/zydeco build lib/tests/core/fact.zy -t zir --trace-passes > /dev/null
before SPS normalize[1] (invocation 1)
after SPS normalize[1] (invocation 1): 28.468 ms
before SPS localize[2] (invocation 1)
after SPS localize[2] (invocation 1): 0.512 ms
```

`-t zir` uses the default plan. Ask the CLI to lay the plan out:

```console
$ target/debug/zydeco passes --sps-passes normalize
SPS scheme: br (name the consumer stack once at each branch)
SPS selection: normalize
  check closed root (required)
  normalize[1]
  check closed root (required)
  closure conversion (required)
```

The required checks bracket the optional passes; the optional stage is a value composed by the
[pass framework](../../references/compiler.md#compiler-pass-composition). This is the concrete
place to experiment before touching `lang/stackir`: `--sps-passes none`, individual names,
duplicates, and `--sps-scheme dup|br|full` are all exposed.

## What Each Stage Adds, Loses, and Hands On

| Stage | Adds | Loses or erases | Consumer |
| --- | --- | --- | --- |
| Textual → bitter → scoped | Administrative forms, resolved references, scopes | Some surface sugar at each step | Checker |
| Checked statics | Types, witnesses, static evidence, typed arenas | Surface syntax | Interpreter linker, static elimination |
| Static elimination → residual | A computation root the backends can consume | Static-only forms, most proofs of typing beyond descriptors | SPS lowering |
| SPS (high) | Explicit stacks, explicit continuations, closure forms | Source control structure | Normalization, localization, closure conversion |
| SPSLow | First-order blocks, jumps, existential closure/continuation packages | Lexical structure and SPS stack lets | ZASM lowering, Wasm |
| ZASM | Basic blocks, stack instructions, explicit control edges | First-order block nesting | Frame analysis, AMD64 emission |
| AMD64 + runtime | Registers, frames, SysV ABI, GC roots | IR-level portability | OS process |

The recurring lesson: each stage pays for what it makes explicit. A cleanup proposal that removes a
representation also removes one of these "adds" columns; the
[cleanup lens](04-cleanup-lens.md) makes that trade-off explicit.

## Failure Contrast: A Rejected Program

```console
$ printf 'fn x => ret x\n' > target/study/reject.zy
$ target/debug/zydeco check target/study/reject.zy
[tyck.unconstrained-inference] Error: Cannot infer a complete type for 1 pattern(s)
   ╭─[ .../target/study/reject.zy:1:4 ]
   │
 1 │ fn x => ret x
   │    ┬
   │    ╰── this pattern type remains unconstrained
   │
   │ Help: add a type annotation to the unconstrained pattern
───╯
```

The failure happens before executable selection: the inference region cannot close, so no backend
input exists. Diagnostics carry a stable code (`tyck.unconstrained-inference`) so tests and docs can
refer to them; the [diagnostic index](../../references/language.md#diagnostic-index) is the catalog.

## Practical Notes

- Build the CLI once with `cargo build --bin zydeco`; `target/debug/zydeco` avoids Cargo overhead per command.
- `-t zir`, `-t zasm`, `-t asm` print to stdout. `-t asm`, `-t exe`, and the library targets are
  native preparation and therefore require `--target-arch x86-64` on this host.
- The REPL and TUI need a terminal; piping stdin into `zydeco repl` panics in ratatui initialization.
  Use `check`, `run`, and `build` for scripted experiments.
- `--target asm` is a good way to verify frame/emission changes without paying for the nested runtime build.
- Do not run `cargo test-all` casually: it is the final validation, not the inner loop. Use focused
  targets and `cargo test-resume` while iterating ([CONTRIBUTING](../../../CONTRIBUTING.md#run-tests)).

## Where to Go Next

- The six [module cards](cards/) explain the code behind each arrow above.
- [03 — Glossary](03-glossary.md) resolves the vocabulary in these outputs (`CType`, `Thk`, `SPSLow`, ...).
- [04 — Cleanup Lens](04-cleanup-lens.md) turns the stage table into removal candidates.
