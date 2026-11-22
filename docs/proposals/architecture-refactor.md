# Master Architecture Refactor

This proposal records the owner-approved six-phase architecture refactor across Zydeco's front end, type checker,
residual IR, middle end, runtime, native code generation, WebAssembly target, and test infrastructure.
It replaces the earlier [artifact boundary](../../docs/proposals/README.md) and checker-query proposals
with a modular full-language workspace (`Option A`) backed by unified intermediate representations,
shared host abstractions, and multi-architecture native code generation.

## Motivation and Baseline

Zydeco separates language concepts cleanly in its specification,
yet the current Rust workspace couples downstream crates tightly to front-end and type-checker representations
and duplicates host semantics across multiple execution backends.
Live profiling on the current debug compiler highlights where compilation time, memory, and runtime overhead accumulate:

| Workload | Wall Time | Instructions Retired | Peak Footprint / Max RSS | Dominant Cause |
| --- | ---: | ---: | ---: | --- |
| `zydeco check lib/std/std.zy` | 1.79 s | 29.94 B | 134.8 MB / 172.3 MB | Whole-graph AST copy, 12-byte sparse IDs, hash/HAMT churn, and eager `std` residualization |
| `zydeco check .../represented-call/main.zy` | 3.14 s | 59.16 B | 271.2 MB / 318.5 MB | 98.3% of compile instructions and 99.8% of peak memory spent in `session`, `surface`, and `statics` |
| `zydeco build -t zir .../represented-call/main.zy` | 3.03 s | 60.16 B | 271.6 MB / 322.4 MB | `SPS normalize` takes 12.8 ms and `localize` takes 0.49 ms; the rest is frontend, statics, and IR checks |
| `zydeco build -t exe lib/tests/core/fact.zy` | 4.99 s | 35.83 B | 152.9 MB / 193.6 MB | Unpacks embedded runtime sources into a temporary directory and runs a cold debug `cargo build` |

Five structural mismatches drive this cost:

1. Multi-keyspace 12-byte IDs, `ArenaSparse` hash maps, and fragmented 4-slot paged vectors are used
   where sequential 4-byte (`u32`) phase-local arenas apply.
2. Whole-program AST cloning
   and `no_eq` Salsa queries bypass Zydeco's closed `@(import(...))` separate-compilation boundary.
3. `StaticsArena` mixes raw source-checking state, environments, and residual runtime CBPV, forcing `zydeco-dynamics`,
   `zydeco-stackir`, and `zydeco-assembly` to depend on `zydeco-surface` and `zydeco-statics`.
4. Builtin signatures and semantics are duplicated manually across `.zy` packages, `zydeco-syntax`,
   `zydeco-statics`, `zydeco-dynamics`, `zydeco-stackir`, `zydeco-assembly`, `zydeco-amd64`, `runtime/stub.rs`,
   `zydeco-wasm-common`, and `cli/wasm/wasm-host.mjs`.
5. Unoptimized `opt-level = 0` native runtimes, registerless stack-machine assembly,
   per-iteration `SFix` closure packing, and global Wasm local tables degrade execution speed and memory safety.

## Locked Architectural Decisions

| # | Area | Decision | Superseded Mechanism Retired in Same Change |
| --- | --- | --- | --- |
| 1 | Feature and workspace scope | Modular full language and tooling (`Option A`): retain fixed-width numerics, manual memory, C FFI/units, `@[cbv]`, `@[monadic]`, `.zyi`, LSP, TUI, and formatter in decoupled crates | Monolithic cross-crate coupling, `docs/legacy/`, and unused `define`/`comatch p => M end` aliases |
| 2 | Core IR boundary | Introduce standalone typed `zydeco-ir` (`CoreProgram`) emitted by lazy demand-driven `StaticElaborator` | Raw/residual co-residency in `StaticsArena` and `surface`/`statics` imports in runtime and backend crates |
| 3 | StackIR typing and verification | Store `RepType` on dense 4-byte `Idx<T>` vectors in `zydeco-stackir`; gate IR verification on `debug_assertions` or `--verify-passes` | `ArenaSparse` IR maps and unconditional `ProtocolGraph` / `Agreement` validation in release compiles |
| 4 | Native codegen backend | Introduce `zydeco-cranelift` (`cranelift-codegen` + `cranelift-object`) for in-process `aarch64` and `x86_64` emission using `CallConv::Tail` and inline bump allocation | `zydeco-assembly` (ZASM), `zydeco-amd64`, external `nasm` processes, and registerless stack-machine emission |
| 5 | Host and WebAssembly runtime | Declare builtins once in `zydeco-abi` and share `zydeco-host` across interpreter, cached `--release` native `libzydeco_runtime.a`, and `wasm32-wasip1` | 11-layer manual builtin tables, 58 host arithmetic stubs, and `cli/wasm/wasm-host.mjs` |
| 6 | Pass stack-safety model | Guard recursive traversals uniformly with `stacker::maybe_grow` alongside typed single-layer folders | 123-variant untyped `Explicit` `Work` state machines (`Output = ()`) and `RUST_MIN_STACK = 16 MiB` |

## Target Crate Architecture

Separating representations along phase boundaries keeps compilation times low
and makes each subsystem reusable independently:

| Layer | Crates | Responsibility |
| --- | --- | --- |
| Foundation | `zydeco-foundation` | 4-byte `Idx<T>` dense arenas, `SymbolInterner` (`Sym(u32)`), `FileId(u32)` spans, `ScopedMap` undo trails, deterministic Tarjan SCC, sorted `CoContext`, and `stacker::maybe_grow` |
| Contracts and IR | `zydeco-abi`, `zydeco-ir` | Declarative `BuiltinSpec`, `RuntimeWord`, FFI layouts, and self-contained typed residual `CoreProgram` (`CtorIdx`, `DtorIdx`, `RepType`, `CheckedImport`) |
| Front end and tooling | `zydeco-surface`, `zydeco-fmt`, `zydeco-statics`, `zydeco-session`, `cajun`, `zydeco-tui` | Two-stage per-file `SyntaxAst -> ResolvedAst` with single-pass typed `ResolvedMeta`; lazy formatting; dense bidirectional checking; per-file Salsa cutoff; `FileId` interval index in LSP; direct REPL snippet checking |
| Middle end and backends | `zydeco-dynamics`, `zydeco-stackir`, `zydeco-cranelift`, `zydeco-wasm-sps` | Zero-clone `CoreProgram` stepper; dense `RepType` SPS/SPSLow with `SFix` loop contification, immediate nullary constructors, and shared unboxing; Cranelift `aarch64`/`x86_64` object emission; Wasm `return_call_indirect` with per-`Case` locals and WASI linking |
| Host and driver | `zydeco-host`, `runtime`, `zydeco-driver`, `cli` | Growable `CheneyHeap`, GC-managed strings/`Bytes`, `.rodata` string literals, reusable FFI instances, cached release runtime archives, headless `zydeco-driver`, and `zydeco` CLI |

Under this layout, `zydeco-dynamics`, `zydeco-stackir`, `zydeco-cranelift`,
and `zydeco-wasm-sps` have zero dependency on `zydeco-surface` or `zydeco-statics`.
`zydeco-fmt` owns trivia and `SurfaceIntentions` capture on demand so ordinary compilation never re-lexes files
or computes unused line/column metadata.

## Staged Execution Plan

Each phase lands cleanly, removes superseded code and callers in the same change,
updates owning chapters in [the references](../references/README.md), and preserves interpreter, native,
and WebAssembly agreement.

| Phase | Target Crates | Deliverables | Owning References Updated |
| --- | --- | --- | --- |
| Phase 1 | `statics`, `stackir`, `wasm-sps`, `runtime`, `cli`, `lang/tests` | Fix `TyEnv::hash`, `check_package_body` flat product arity, `Normalization::shared` single-use closure aliasing, `CompilerSession` clone/LRU eviction, and per-`Case` Wasm `LocalPlan`; lower all 58 `PrimitiveOp`s in `ExternalFunction::make_function`; drop `rand` and cache `--release` `libzydeco_runtime.a`; remove `define` and `comatch p => M end` aliases and `docs/legacy/`; warm `thread_local!` test session, single-compile 3-backend e2e runner, and consolidated `lang/tests` `suite` binary. | [L2](../references/language/src/lexical-and-syntax.md), [C11–C14](../references/compiler/src/native-emission.md), [C16](../references/compiler/src/validation-and-extending.md) |
| Phase 2 | `utils` -> `foundation`, `syntax`, `statics` | Migrate to `SymbolInterner`, `FileId`, 4-byte `Idx<T>` dense arenas, `ScopedMap`, Tarjan SCC, and `stacker::maybe_grow`; remove the 41 Salsa leaf producer queries and 6 `env_*` tables; linearize `FillId::fill`, `InferenceRegion`, `Lub` DAG memoization, and unified `TelescopeCursor`/`FieldIndex`; modularize `@[cbv]` and memoize `@[monadic]`. | [C2](../references/compiler/src/compiler-data.md), [C3](../references/compiler/src/sessions-and-queries.md), [C5–C6](../references/compiler/src/checking-and-inference.md) |
| Phase 3 | `ir`, `statics`, `dynamics`, `stackir` | Introduce `zydeco-ir` (`CoreProgram`); enable lazy `Thunk` residualization and memoized `(UnitId, CompuId, EnvHash)` procedure sharing in `StaticElaborator`; rewrite `zydeco-dynamics` as a borrowed `CoreProgram` stepper; sever `surface`/`statics` dependencies from `dynamics` and `stackir`; retire `RUST_MIN_STACK = 16 MiB`. | [C1](../references/compiler/src/architecture.md), [C6](../references/compiler/src/elaboration-and-validation.md), [C7](../references/compiler/src/reference-interpreter.md) |
| Phase 4 | `surface`, `fmt`, `session`, `editor/cajun`, `tui` | Extract `zydeco-fmt`; fuse `bitter` and `scoped` into `SyntaxAst -> ResolvedAst` with single-pass typed `ResolvedMeta`; compile per-`FileId` units in Salsa with interface-hash cutoff; index per-file spans in `cajun` without `realpath` syscalls or pinned `StaticsArena`; add direct `check_repl_entry` in `tui`. | [C3](../references/compiler/src/sessions-and-queries.md), [C4](../references/compiler/src/parsing-and-resolution.md), [C15](../references/compiler/src/tooling.md) |
| Phase 5 | `abi`, `host`, `machine`, `runtime`, `dynamics`, `wasm-sps` | Unify builtin/FFI specifications in `zydeco-abi`; build shared `zydeco-host` with growable `CheneyHeap`, GC-allocated strings/`Bytes`, static `.rodata` string literals, and reusable FFI `RuntimeInstance`; compile `zydeco-host` to `wasm32-wasip1`, move `wasm-sps` to native linear memory + `return_call_indirect`, and retire `wasm-host.mjs`. | [C12](../references/compiler/src/native-runtime.md), [C13](../references/compiler/src/webassembly.md), [C14](../references/compiler/src/builtins-and-foreign-calls.md) |
| Phase 6 | `stackir`, `cranelift`, `driver`, `cli` | Move `stackir` to dense `Idx<T>` arenas with `RepType`, `O(N)` capture analysis, gated linear IR typechecker, immediate nullary constructors, shared unboxing, scalar SSA, join-point commuting, and `SFix` loop contification; implement `zydeco-cranelift` (`aarch64` + `x86_64`) and retire `zydeco-assembly`, `zydeco-amd64`, and `nasm`. | [C8–C9](../references/compiler/src/sps.md), C10–C11, [`DESIGN.md`](../../DESIGN.md) |

## Validation Criteria

Every phase requires:

1. Passing `cargo fmt --all -- --check` and `cargo clippy-all -- -D warnings`.
2. Passing unit suites across affected crates plus full fixture/oracle parity across `interpreter`,
   native executables, and WebAssembly in `zydeco-tests`.
3. Paired accepted and rejected tests for every changed syntactic or static validation boundary.
4. Passing reference-book validation (`python3 docs/scripts/build-books.py check`) whenever documentation
   or canonical reference scope changes.
