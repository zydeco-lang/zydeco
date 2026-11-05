# 05 — Execution Backends (reference interpreter, WebAssembly backend and host)

> Scratch study note for re-onboarding. Not canonical: contracts below point to their authoritative references.

## 1. Responsibility

`lang/dynamics` is the reference execution path.
It links the checked program's stored residual into its own runtime tree (`DynamicsProgram`),
then evaluates that tree small-step with explicit environments and a persistent continuation stack.
The crate also owns the interpreter-side builtin bodies, host resources and capabilities,
the Unix foreign-call adapter, and a debug printer.
C7 owns this account ([compiler.md](../../../../docs/references/compiler.md#c7-linking-and-the-reference-interpreter)).

`lang/wasm-common`, `lang/wasm-sps`, and `cli/wasm` are the WebAssembly path.
`wasm-common` fixes the runtime contract shared by generated modules and their embedding:
tagged `i64` words, a bump allocator, the host-call ABI, and the core module sections.
`wasm-sps` emits structured `wasm32` modules directly from `SpsLowProgram`.
`cli/wasm/wasm-host.mjs` is the Node host implementing the `zydeco` import namespace:
process I/O, host resources, random integers, and the unmanaged-memory emulation.
The CLI bundles those three `.mjs` files for `run -t wasm-sps`. C13 owns the backend
and embedding account ([compiler.md](../../../../docs/references/compiler.md#c13-webassembly-backend-and-embedding)).

Three execution targets exist now (`cli/src/cli.rs`:30): the reference interpreter,
native AMD64 `exe` (the native card's), and `wasm-sps`.
The abstract-machine Wasm backend and the ZASM interpreter were removed; §9 row 6 records the Wasm one.
This card covers the interpreter side of C14 and the C13 backend; the statics checker,
SPS/SPSLow, the native pipeline, and the general CLI driver belong to other cards.
The in-scope CLI boundary is [`ExecutionRunner`](../../../../cli/src/execution.rs):112,
which turns one checked `ExecutableProgram` into per-target processes while sharing a single lowering.

## 2. Position in the Pipeline

Both execution paths fork after C6 static elimination, which stores the residual root, and both are leaves:
nothing downstream consumes their result except the process boundary.

| Path | Upstream product | Entry | Downstream |
| --- | --- | --- | --- |
| Interpreter | `ExecutableProgram` (residual root + Builtin signature), [C6](../../../../docs/references/compiler.md#static-elimination) | `BuiltinRootLinker::run` → `DynamicsProgram` | `Runtime::run` → `ProgKont` → CLI exit status |
| WebAssembly | `SpsLowProgram` after C9 closure conversion | `Emitter::run` → `WasmModule` | temp `program.wasm` + Node host process → exit status |

The two seams are **checked residual → linker → interpreter** and **SPSLow → Wasm emitter → host**.
The interpreter deliberately does not pass through SPS: C7 keeps a reference evaluation path
that no demand or frame optimization changes.
The Wasm path starts at SPSLow because starting at SPS would duplicate closure conversion,
and recovering lexical structure from ZASM would require reconstructing lexical regions and stack effects
from machine program points ([C13](../../../../docs/references/compiler.md#c13-webassembly-backend-and-embedding)).

C1's product table names the two consumed products and their establishing entry points
([C1](../../../../docs/references/compiler.md#c1-architecture-and-a-programs-path-through-the-compiler)):
`DynamicsProgram` from `BuiltinRootLinker` feeds the interpreter, and `SpsLowProgram`
from `SpsLowPipeline::run` feeds the emitter.
The CLI side is `CommandCompiler::interpret_with_libraries` for the interpreter (`cli/src/compile.rs`:171)
and `CommandCompiler::emit_wasm_sps` for WebAssembly (`cli/src/compile.rs`:514);
`run_source` selects one target and hands an `ExecutableProgram` to the runner (`cli/src/main.rs`:342).
Execution profiles and target spelling are canonical
in [L15](../../../../docs/references/language.md#15-execution-profiles)
and [L15 backend selection](../../../../docs/references/language.md#selecting-an-execution-backend).

## 3. Entry Points and Key Types

| What | Where | Why it matters |
| --- | --- | --- |
| CLI interpreter entry | [compile.rs](../../../../cli/src/compile.rs):171 `interpret_with_libraries` | Links a `BuiltinRootLinker`, runs `Runtime`, maps `ProgKont` to a CLI result |
| Root linkers | [link.rs](../../../../lang/dynamics/src/link.rs):19 `RootLinker`, :26 `ValueRootLinker`, :33 `BuiltinRootLinker`, :41 `BuiltinComputationRootLinker` | Executable, REPL value, and builtin-package roots; the last applies nested package layers |
| Erasure | [link.rs](../../../../lang/dynamics/src/link.rs):160 `Link for ss::VPatId`, :196 `for ss::ValueId`, :250 `for ss::CompuId` | Converts statics-arena identifiers into reference-counted runtime syntax |
| Linked product | [syntax.rs](../../../../lang/dynamics/src/syntax.rs):100 `DynamicsProgram` (scope :93) | Frozen definition-name arena plus one `RcCompu` root; the evaluator's input |
| Evaluation state | [syntax.rs](../../../../lang/dynamics/src/syntax.rs):148 `Runtime`, :162 `ProgKont`, :170 `RuntimeError` | Borrowed streams and args, host/foreign runtimes, `Env<SemValue>`, `rpds::VectorSync` stack |
| Stepper | [eval.rs](../../../../lang/dynamics/src/eval.rs):9 `Eval`, :206 `Computation::step`, :58 `Runtime::run` | Small-step loop; step returns `Done` or the next term |
| Builtin dispatch | [builtin.rs](../../../../lang/dynamics/src/builtin.rs):25 `invoke`, :20 `package_value`, :14 `BuiltinFailure` | Maps a `BuiltinValueRole` to scalar ops, host resources, or memory role impls |
| Interpreter op bodies | [impls.rs](../../../../lang/dynamics/src/impls.rs):51 `Branch`, :60 `Branch::scalar`, :77 `integer_to_string`, :89 `float_to_string` | The interpreter's side of C14 arithmetic, comparisons, text, and I/O adapters |
| Host resources | [host.rs](../../../../lang/dynamics/src/host.rs):10 `HostValue`, :35 `HostRuntime`, :135 `HostIoErrorKind` | Reader/writer handles, retained memory, Unix file resources, I/O error categories |
| Foreign adapter | [foreign.rs](../../../../lang/dynamics/src/foreign.rs):20 `ForeignRuntimeError`, :36 `ForeignRuntime`, :63 `invoke` | Lazy libffi loading/caching for checked C imports on Unix; native units rejected |
| Debug printer | [fmt.rs](../../../../lang/dynamics/src/fmt.rs):5 `Formatter` | Only client is the TUI REPL ([engine.rs](../../../../tui/src/engine.rs):6, :218) |
| Target and runner | [cli.rs](../../../../cli/src/cli.rs):30 `ExecutionTarget`; [execution.rs](../../../../cli/src/execution.rs):112 `prepare`, :156 wasm branch | Interpreter runs in-process; `wasm-sps` writes a temp module plus the host bundle under `node` |
| Wasm emitter | [emit.rs](../../../../lang/wasm-sps/src/emit.rs):38 `Emitter`, :53 `run`, :59 `EmitError` | Public entry from `SpsLowProgram` to `WasmModule` |
| Module planning/encoding | [emit.rs](../../../../lang/wasm-sps/src/emit.rs):244 `ModulePlan::new`, :420 `ModuleEncoder::encode`, :507 `entry_function` | Sorted cases/locals/imports/static strings; section order and trampoline |
| Wasm runtime contract | [module.rs](../../../../lang/wasm-common/src/module.rs):38 `WasmSections`, :111 `AllocFunction`; [host.rs](../../../../lang/wasm-common/src/host.rs):126 `HostSections`, :39 `HostCallKind`, :17 `RuntimeFailure`; [word.rs](../../../../lang/wasm-common/src/word.rs):45 `WordEmitter`; [lib.rs](../../../../lang/wasm-common/src/lib.rs):26 `HOST_MODULE` | Shared sections, allocator, ABI, failure codes, and word instruction sequences |
| Node host | [wasm-host.mjs](../../../../cli/wasm/wasm-host.mjs):324 `ZydecoHost`, :346 `imports`, :393 `installNumeric`, :791 `WasmProgram.run` | Implements imports, supplies streams/args, turns process exit into a Node exit code |
| Module writer | [native.rs](../../../../cli/src/native.rs):169 `write_wasm`; [execution.rs](../../../../cli/src/execution.rs):156–170 | `build -t wasm-sps` artifact naming and the bundled-host `run` path |

## 4. Contracts and Invariants

- **Execution starts from the stored residual, never from source or checked syntax.** `RootLinker::residual`
  matches the requested root against `StaticElaboration.source` and takes its `residual`,
  failing `MissingResidual` otherwise; eliminated forms are `unreachable!` in the linker.
  Evidence: [link.rs](../../../../lang/dynamics/src/link.rs):87, :190, :210.
  Canonical: [C6 static elimination](../../../../docs/references/compiler.md#static-elimination) and C7.
- **Builtin roots are materialized only from the validated typed signature.** `BuiltinRootLinker` builds
  a `BuiltinPackagePlan::for_executable` and turns each `BuiltinPackageValue::Operation` into a thunk;
  `BuiltinComputationRootLinker` peels nested computation contracts with a seen-set that reports a recursive contract.
  Evidence: [link.rs](../../../../lang/dynamics/src/link.rs):124–158,
  [statics/builtin.rs](../../../../lang/statics/src/builtin.rs):733.
  Canonical: [C7](../../../../docs/references/compiler.md#c7-linking-and-the-reference-interpreter)
  and [C14](../../../../docs/references/compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls).
- **Reference values are Rust-owned and host interfaces are borrowed.** `Runtime` holds
  borrowed `BufRead`/`Write` interfaces, an `Env<SemValue>`, and an `rpds::VectorSync` frame stack;
  thunks capture lexical environments; allocations are `Rc` trees, not the tagged heap.
  Evidence: [syntax.rs](../../../../lang/dynamics/src/syntax.rs):121–158,
  [eval.rs](../../../../lang/dynamics/src/eval.rs):41–56.
  Canonical: C7 and [L15](../../../../docs/references/language.md#15-execution-profiles) (interpreter row).
- **Program outcome is one of three cases and failure surfaces at the host boundary.** `ProgKont` distinguishes a
  returned value, an exit code, and a runtime error; a failing run is reported, not silently retried on another target.
  Evidence: [syntax.rs](../../../../lang/dynamics/src/syntax.rs):162–181,
  [eval.rs](../../../../lang/dynamics/src/eval.rs):285–326.
  Canonical: C7 and [L15 backend selection](../../../../docs/references/language.md#selecting-an-execution-backend).
- **The Wasm backend consumes SPSLow directly and keeps source recursion off the host call stack.** The root
  and each SPSLow block or closed label become a table-indexed case function; `entry` initializes the heap,
  program counter, and ambient-stack globals and loops on `call_indirect`.
  Evidence: [wasm-sps/src/lib.rs](../../../../lang/wasm-sps/src/lib.rs):1–20,
  [emit.rs](../../../../lang/wasm-sps/src/emit.rs):390–392, :507–525.
  Canonical: C13 (trampoline paragraph)
  and [L15](../../../../docs/references/language.md#15-execution-profiles) (trampoline sentence).
- **The module/host boundary is a fixed ABI: one import namespace, three exports, no WASI.**
  Generated modules export `memory`, `entry`, and `_start`; imports live under `zydeco`
  with a mandatory first `runtime_error` and an optional `string_literal`;
  returning calls take `i64` arguments and one `i64`, control calls return four `i64` words,
  a potential full-width scalar result carries a trailing `i32` spare-box address, and native C imports are rejected.
  Evidence: [module.rs](../../../../lang/wasm-common/src/module.rs):77–83,
  [host.rs](../../../../lang/wasm-common/src/host.rs):132–161, [emit.rs](../../../../lang/wasm-sps/src/emit.rs):797.
  Canonical: [C13 Module and Host ABI](../../../../docs/references/compiler.md#module-and-host-abi).
- **The scalar boundary is pinned across languages, and language failures share one carrier.** The Rust word encoding
  and the JS host constants are checked against each other, float rendering is checked against Rust `Display`,
  and emitted failures call `zydeco.runtime_error(i32)` followed by `unreachable`.
  Evidence: [syntax/word.rs](../../../../lang/syntax/src/word.rs):87,
  [wasm-host.mjs](../../../../cli/wasm/wasm-host.mjs):6–9, :349–358;
  [wasm-common/host.rs](../../../../lang/wasm-common/src/host.rs):13–35.
  Canonical: C13 tagged-word contract
  and [C16 runtime oracles](../../../../docs/references/compiler.md#source-fixtures-and-runtime-oracles).

## 5. Data Flow

The interpreter keeps the CLI's own path short.
An `ExecutableProgram` carries the checked residual root and the Builtin signature;
`BuiltinRootLinker::run` selects the residual, lowers the validated signature into a package
of operation thunks, recursively erases the statics arena into `Rc` syntax,
and rebinds the definition-name arena (`dynamic_definitions` clones scoped plus generated names,
[arena.rs](../../../../lang/statics/src/arena.rs):854).
`Runtime::new` attaches the caller's streams, argument slice, host resources,
and foreign runtime; `run` evaluates the root.
`Computation::step` pushes `Kont` frames on `do`, `App` frames on application,
and `Dtor` marks on comatch consumers; `Ret` resumes the top frame or returns `ProgKont::Ret`.
A `Prim` pops `role.arity()` argument frames and calls `BuiltinRuntime::invoke`;
`exit` reports the exit code through `BuiltinFailure::Exit`, while language failures become `ProgKont::Error`.
The CLI prints the runtime error to stderr and returns the exit status; the printer is only for REPL values.

The WebAssembly path starts one phase later.
`ExecutionRunner::prepare` shares a single lowering across targets; for `wasm-sps` it calls `emit_wasm_sps`,
writes `program.wasm` and the three helper modules into a temp directory, and spawns `node wasm-host.mjs program.wasm`.
`ModulePlan` sorts arena identifiers into cases, locals, host imports, and static data;
`ModuleEncoder` emits allocator, pair helper, one function per case, the entry trampoline, and the name section.
The Node host instantiates the module with a `zydeco` proxy, runs the `entry` export,
and each case's imports execute against JS implementations.
A returning operation returns one word; a control operation returns count/closure/two argument slots;
`process/exit` and `runtime_error` throw an `ExitSignal` that becomes the process exit code.
`run` inherits terminal streams; `test` captures output with an explicit stdin file.

```text
ExecutableProgram ──BuiltinRootLinker::run──▶ DynamicsProgram ──Runtime::run──▶ ProgKont ──▶ exit status
  residual root, Builtin signature   link.rs:124     defs + Rc<Compu>        eval.rs:58

SpsLowProgram ──Emitter::run──▶ WasmModule bytes ──write──▶ node wasm-host.mjs ──▶ exit status
  blocks, labels         emit.rs:53     sections, cases     execution.rs:156    imports/transfer
```

## 6. Verification Commands

Run from the repository root. The first `cargo run` builds the CLI (minutes); later invocations are quick.
Every row below was actually run at `e2dd8979`; only the documented outcome is quoted.

| Purpose | Command | Expected observable outcome |
| --- | --- | --- |
| Interpreter smoke | `cargo run --quiet --bin zydeco -- run lib/tests/core/fact.zy` | exit 0, empty stdout |
| wasm-sps smoke | `cargo run --quiet --bin zydeco -- run lib/tests/core/fact.zy -t wasm-sps` | exit 0, empty stdout |
| Interpreter I/O | `printf '1\n2\nx\n' \| cargo run --quiet --bin zydeco -- run lib/tests/demos/echo_sum.zydeco` | prints `1 = sum`, `3 = sum`; exit 0 |
| wasm-sps I/O parity | same pipeline with `-t wasm-sps` appended | same two lines; exit 0 |
| Build a module | `cargo run --quiet --bin zydeco -- build lib/tests/demos/echo_sum.zydeco -t wasm-sps --build-dir target/study-execution` | prints `target/study-execution/echo_sum.sps.wasm` |
| Node host outside the CLI | `printf '1\n2\nx\n' \| node cli/wasm/wasm-host.mjs target/study-execution/echo_sum.sps.wasm` | same two lines; exit 0 |
| Exit-status parity | run a scratch `process/exit 7` source three ways: interpreter, `-t wasm-sps`, and `build` + `node cli/wasm/wasm-host.mjs` | all three return status 7 |
| Interpreter unit tests | `cargo test -p zydeco-dynamics --lib` | `16 passed; 0 failed` |
| Emitter unit tests | `cargo test -p zydeco-wasm-sps` | `5 passed` unit tests, 0 doc-tests |
| JS word-boundary pin | `cargo test -p zydeco-syntax --lib javascript_boundary_matches` | `1 passed`; the host constants still match `RuntimeWord` |
| Float text parity | `cargo test -p zydeco-tests --test wasm_float_text` | `1 passed`; 20k sampled bit patterns rendered through the Node host match Rust `Display` |

## 7. Reading Order

1. [`lang/dynamics/src/README.md`](../../../../lang/dynamics/src/README.md) —
   26-line orientation to the crate's modules.
2. [C7](../../../../docs/references/compiler.md#c7-linking-and-the-reference-interpreter) —
   the canonical linking and evaluation contract; read it before the code.
3. [`cli/src/compile.rs`](../../../../cli/src/compile.rs):171 — how a CLI `run` reaches the linker and `Runtime`.
4. [`lang/dynamics/src/link.rs`](../../../../lang/dynamics/src/link.rs) — residual selection
   and the three erasure passes.
5. [`lang/dynamics/src/syntax.rs`](../../../../lang/dynamics/src/syntax.rs)
   with [`eval.rs`](../../../../lang/dynamics/src/eval.rs) — runtime state and the small-step rules side by side.
6. [`builtin.rs`](../../../../lang/dynamics/src/builtin.rs) and [`impls.rs`](../../../../lang/dynamics/src/impls.rs) —
   the role dispatch and the per-role bodies; [`host.rs`](../../../../lang/dynamics/src/host.rs) owns resources.
7. [C13](../../../../docs/references/compiler.md#c13-webassembly-backend-and-embedding)
   with [`lang/wasm-common/src/lib.rs`](../../../../lang/wasm-common/src/lib.rs):1–15 —
   the runtime contract and its owner.
8. [`lang/wasm-sps/src/lib.rs`](../../../../lang/wasm-sps/src/lib.rs):1–20,
   then [`emit.rs`](../../../../lang/wasm-sps/src/emit.rs):228–380 (planning) and :410–525 (encoding, trampoline).
9. [`cli/wasm/wasm-host.mjs`](../../../../cli/wasm/wasm-host.mjs):324–391 (imports)
   and [`execution.rs`](../../../../cli/src/execution.rs):156–170 — the embedding boundary.
10. [L15](../../../../docs/references/language.md#15-execution-profiles)
    with [`cli/README.md`](../../../../cli/README.md#run-and-test-programs) — profiles and exact CLI spellings.

## 8. Checkpoint Questions

1. Which residual does the interpreter run, and what happens when a root was never residualized?
   → `link.rs`:87–95; [C6](../../../../docs/references/compiler.md#static-elimination).
2. Why does the interpreter link a Builtin package as thunks instead of dispatching on the source record?
   → `link.rs`:124–158;
   [C14](../../../../docs/references/compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls).
3. Where do the evaluator's stack, environment, and suspended computations live,
   and why does that make its costs unrepresentative?
   → `syntax.rs`:141–158; C7 paragraph starting "The interpreter uses Rust representations".
4. What are the three program outcomes, and where does a failing run surface to the user? → `syntax.rs`:162–166;
   [L15 backend selection](../../../../docs/references/language.md#selecting-an-execution-backend).
5. Why does Wasm emission start at SPSLow rather than at SPS or ZASM?
   → [C13](../../../../docs/references/compiler.md#c13-webassembly-backend-and-embedding); `wasm-sps/src/lib.rs`:1–20.
6. How does the generated module avoid growing the engine stack on unbounded source recursion?
   → `emit.rs`:507–525; C13 trampoline paragraph; [L15](../../../../docs/references/language.md#15-execution-profiles).
7. What is the exact host ABI for returning, control, and spare-box operations, and which imports are mandatory?
   → [C13 Module and Host ABI](../../../../docs/references/compiler.md#module-and-host-abi);
   `wasm-common/src/host.rs`:132–161.
8. How does the CLI find and run the Node host, and which three JS files travel with the module?
   → `execution.rs`:156–170; [C13](../../../../docs/references/compiler.md#c13-webassembly-backend-and-embedding).
9. Which language failures cross the Wasm boundary, and how are they reported?
   → `wasm-common/src/host.rs`:13–35; `wasm-host.mjs`:349–358; `eval.rs` `RuntimeError` at `syntax.rs`:170.
10. Which execution targets exist now, and what happened to the other Wasm backend?
    → `cli/src/cli.rs`:30; [cleanup-audit update paragraph](../../../proposals/artifact-boundary.md) lines 16–17;
    commit `5e4f4e46`.

## 9. Redundancy and Cleanup Signals

Statuses: `open` = removable per the retired studies and current code, subject to caller rework;
`decision needed` = removal or merge changes approved reference text or waits on a design decision;
`resolved` = already removed or settled.
Estimates are the retired studies' `≈` figures and are not additive.

| # | Signal | Evidence | Status | Assessment |
| --- | --- | --- | --- | --- |
| 1 | Dynamics syntax, linker, and printer mirror the statics residual; the evaluator could run over statics computation IDs directly | [size analysis §Structural simplifications](../../../proposals/artifact-boundary.md#structural-simplifications) (≈0.85k); measured `wc -l`: `syntax.rs` 181 + `link.rs` 385 + `fmt.rs` 254 = 820; link.rs erases node-for-node (:160–333); `dynamic_definitions` clones names (`arena.rs`:854); printer's only client is `tui/src/engine.rs`:6, :218 | decision needed | Gated on the IR-boundary decision (append-only residual IR versus interpreting the statics arena). Deleting changes C7's account of `DynamicsProgram`; the TUI REPL must be reworked in the same change; the defs arena and `fmt.rs` would go with it |
| 2 | Builtin operations are implemented once per consumer, beside one role catalog | [size analysis §Builtins written once per consumer](../../../proposals/artifact-boundary.md#builtins-written-once-per-consumer) (4,503 lines at `6aae43d0`); my `wc -l` now 4,509 across `statics/builtin.rs` 818, `dynamics/builtin.rs` 214, `dynamics/impls.rs` 774, `dynamics/host.rs` 175, `stackir/builtin.rs` 259, `runtime/stub.rs` 1,264, `wasm-common/host.rs` 176, `cli/wasm/wasm-host.mjs` 829; role catalog `lang/syntax/src/lib.rs`:551 with shared `host_name`/`arity` (:671) | decision needed | The naming/arity table is already shared by the role catalog; the duplicated part is behavior bodies for interpreter, native stub, and host. The one-host-ABI-table direction (≈2k) changes C14's account, and giving arithmetic an intrinsic body first is the stated prerequisite |
| 3 | Four language-failure codes are restated in the Rust reference runtime, the native runtime, and the JS host | `wasm-common/src/host.rs`:13–35; `dynamics/src/syntax.rs`:169–181; `runtime/stub.rs`:483–498; `wasm-host.mjs`:348–358, :388–391 | open | Small and located at the embedding boundary; a generated host ABI table (row 2) would collapse it. No canonical rule enumerates the codes, so no reference gate |
| 4 | The eight I/O error categories are defined three times | `dynamics/src/host.rs`:132–159 (`HostIoErrorKind`); `runtime/stub.rs`:320–345 (same enum, native ABI); `wasm-host.mjs`:749–771 (`ioErrorKind` mapping) | open | The retired studies do not list it separately; it is a duplicate of an ABI-level mapping that belongs wherever a merged host table lands. Behaviour is already sourced from one library contract (C14) |
| 5 | Systems-programming thread reaches both execution boundaries: interpreter C imports via libffi, Node host unmanaged-memory emulation | [cleanup audit §Systems-programming thread](../../../proposals/artifact-boundary.md#systems-programming-thread) ("C imports could sit behind a cargo feature, which would also remove libffi and the C toolchain from the default interpreter build"); `lang/dynamics/Cargo.toml` target-unix `libffi`; `foreign.rs` 356 lines; [C14 Node host note](../../../../docs/references/compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls) for `wasm-memory.mjs` 64 lines | decision needed | Removing the thread is an owner decision that moves L14/C14 reference text and reworks std memory clients. The interpreter adapter and the Node memory emulation would go with it |
| 6 | Abstract-machine Wasm backend | Removed in `5e4f4e46` (2026-10-25, "incr: remove the abstract-machine Wasm backend"); `lang/wasm-am` is absent from the workspace; `ExecutionTarget` now has only interpreter/exe/wasm-sps (`cli/src/cli.rs`:30); the audits' update paragraphs retired their rows ([cleanup audit](../../../proposals/artifact-boundary.md) lines 16–17; [size analysis](../../../proposals/artifact-boundary.md) lines 23–26) | resolved | One Wasm code path remains: structured SPSLow → `wasm-sps`. The build target union was updated in the same commit, so no removal work is left; only the stale "four backends" prose in §10 remains |

## 10. Uncertainties and Drift

- **Confirmed stale reference text.** Two reference sentences still say "all four backends"
  for tests that commit `5e4f4e46` reduced to three:
  [C14](../../../../docs/references/compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls)
  line 3076 (argument regressions)
  and [L14 foreign decoding](../../../../docs/references/language.md#foreign-decoding) line 2407 (FFI specimen decoder).
  Current registrations are interpreter, AMD64, and `wasm-sps` (`lang/tests/tests/builtin.rs`:59–102;
  `lang/tests/tests/ffi_examples.rs`:165), so "three backends" is accurate now.
  This belongs in `docs/todos/` per the repository rules; it was not editable from this card.
- **Comment drift.** `cli/wasm/wasm-numeric.mjs`:1 says the decimal spelling is
  "shared by the interpreter and native numeric builtins".
  The module is imported only by the Node host (`wasm-host.mjs`:3) and
  by the parity test (`lang/tests/tests/wasm_float_text.rs`:62); interpreter
  and native backends use Rust's own `Display` ([`impls.rs`](../../../../lang/dynamics/src/impls.rs):89,
  `runtime/stub.rs`:742).
  It is a cross-language copy, not shared code.
- **The dynamics crate guide is incomplete, not wrong.**
  [`lang/dynamics/src/README.md`](../../../../lang/dynamics/src/README.md) lists syntax,
  link, eval, builtin/impls, and fmt, but `lang/dynamics/src/lib.rs`:8–28 also exposes `host`, `foreign`,
  `memory`, and `representation`; README is the intended map per C7, so this drifts as the crate grew.
- **Open performance and termination questions are tracked, not resolved.** Interpreter value sharing
  and deep teardown is an explicit new-baseline item (`docs/todos/README.md`:23–31;
  a 5,000-cell list took 2.4 s at `e00b91f0`, thresholds not rerun).
  Do not treat the interpreter's speed as a reference for the compiled backends: C7 says so,
  and the no-collection Wasm bump heap makes the comparison doubly unsafe.
- **Product-suffix layout coverage is missing at the backend boundary.** `docs/todos/README.md`:47–56 asks
  for explicit layouts covering interpreter/native agreement and the corresponding WebAssembly lowering;
  that coverage does not exist yet, so this card cannot cite it as evidence.
- **`run -t wasm` does not exist.** The target is spelled `wasm-sps`,
  and the unqualified `wasm` name is an open design question,
  not an alias ([wasm-backends proposal](../../../../docs/proposals/wasm-backends.md):3–7).
- **Not verified here.** I did not run the full conformance suite
  or the end-to-end macro harness (`runtime_source!`/`e2e_*`), memory-exhaustion tests,
  or any FFI example; those remain C16 evidence and are not quoted as outcomes.
  The two cross-language rows did rerun the Node host through `cargo test` from current sources.
- **Historical snapshots.** The size analysis and cleanup audit measured older revisions;
  the 4,509-line builtin figure and the 820-line dynamics figure above are my `wc -l` totals at `e2dd8979`,
  and the `≈` estimates are the retired studies' projections, not measured deletions.
