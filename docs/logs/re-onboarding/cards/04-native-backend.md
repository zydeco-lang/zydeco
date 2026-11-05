# 04 — Native Backend (ZASM, stack/frame analysis, AMD64 emission, native runtime)

> Scratch study note for re-onboarding. Not canonical: contracts below point to their authoritative references.

## 1. Responsibility

This module turns first-order stack IR into a native AMD64 process. It owns four adjacent boundaries:
ZASM (a control-flow graph with explicit operand and control stacks, variables, symbols, and instructions);
stack analysis and local representation selection over that graph; checked native preparation, which maps
ZASM variables to activation-frame slots and binder ownership; and AMD64 emission together with the
native runtime and packaging path (shared machine model, SysV conventions, collector roots, host ABI). It does not own SPS or closure conversion (the stack
IR module), the Wasm backend (the execution module), or general CLI/editor tooling (the tooling module).
The canonical phase accounts are [C10](../../../references/compiler.md#c10-zasm-stack-analysis-and-local-representation-choices),
[C11](../../../references/compiler.md#c11-native-preparation-activation-frames-and-amd64-emission), and
[C12](../../../references/compiler.md#c12-shared-native-model-allocation-and-collection); the operations the
generated code calls belong to [C14](../../../references/compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls)
and [L13](../../../references/language.md#13-primitive-values-and-capabilities), and the execution profile to
[L15](../../../references/language.md#15-execution-profiles).

Two products, not one, leave assembly lowering: the portable `AssemblyProgram` for ZASM rendering and
representation analysis, and the frame-aware `NativeProgram` selected by `LoweringPipeline::with_native_frames`
([pipeline.rs:36](../../../../lang/assembly/src/pipeline.rs#L36), [lower.rs:65](../../../../lang/assembly/src/lower.rs#L65),
[C10](../../../references/compiler.md#c10-zasm-stack-analysis-and-local-representation-choices)). Native preparation is
a validator, not a convenience: emission consumes a checked `FramePlan`, and invalid input is a `FramePlanError`
([frames.rs:18](../../../../lang/assembly/src/frames.rs#L18), [frames.rs:37](../../../../lang/assembly/src/frames.rs#L37),
[C11](../../../references/compiler.md#c11-native-preparation-activation-frames-and-amd64-emission)).

## 2. Position in the Pipeline (upstream/downstream, doc anchors; SPSLow → ZASM → AMD64, plus runtime packaging/linking)

Upstream, `SpsLowProgram` is fixed by `SpsLowPipeline`; this module starts when
`LoweringPipeline` reads it ([C1 products](../../../references/compiler.md#c1-architecture-and-a-programs-path-through-the-compiler),
[C9](../../../references/compiler.md#c9-closure-conversion-and-first-order-spslow)). The `Lowerer` is the only lowering
producer: it collects local unboxing opportunities under the selected policy, forms scalar plans, and runs the CPS
assembly folder ([lower.rs:25](../../../../lang/assembly/src/lower.rs#L25), [unbox.rs:32](../../../../lang/assembly/src/unbox.rs#L32)).
The portable path then runs `StackAnalyzer`, renders through `assembly::fmt::Formatter`, and stops at ZASM or at an
external ZASM consumer ([pipeline.rs:58](../../../../lang/assembly/src/pipeline.rs#L58), [fmt.rs:7](../../../../lang/assembly/src/fmt.rs#L7)).
The native path runs the same lowering with `native_frames` enabled, checks a `FramePlan`, and hands
`NativeProgram { assembly, frames }` to the emitter ([pipeline.rs:36](../../../../lang/assembly/src/pipeline.rs#L36)).
Downstream consumers are `build --target asm|exe`, `run --target exe`, the C-export and native-unit adapters, and the
integration tests; the shared CLI entry point is `BackendProgram` ([compile.rs:279](../../../../cli/src/compile.rs#L279)).

`Amd64Artifact` couples emission output to what linking needs: assembly text plus the foreign imports and
libraries collected from that same native program, so the CLI and tests link after one lowering pass
([compile.rs:317](../../../../cli/src/compile.rs#L317), [C11 artifacts](../../../references/compiler.md#compilation-unit-preparation-and-artifacts)).
Packaging then copies the runtime sources, writes the compiler's embedded model to `build/machine/`, rewrites the
runtime manifest's model path, and invokes NASM, `ar`, and Cargo ([native.rs:34](../../../../cli/src/native.rs#L34),
[native.rs:98](../../../../cli/src/native.rs#L98), [cli guide](../../../../cli/README.md#native-executables)).
The Cargo build links the generated archive through `ZYDECO_STATIC_LIB` ([runtime/build.rs:8](../../../../runtime/build.rs#L8));
its `main` is renamed to the published `build/<name>.exe` in one filesystem step
([native.rs:161](../../../../cli/src/native.rs#L161)). The runtime sources and the shared machine model enter here:
`zydeco-machine`'s `bundle` feature embeds its sources in the compiler, and both sides derive the entry symbol from
the same source fingerprint, so a mismatched pair fails at link time ([lib.rs:21](../../../../lang/machine/src/lib.rs#L21),
[build.rs:32](../../../../lang/machine/build.rs#L32), [DESIGN](../../../../DESIGN.md#shared-rust-runtime-model)).

## 3. Entry Points and Key Types

| What | Where | Why it matters |
| --- | --- | --- |
| `LoweringPipeline` / `Lowerer` | [pipeline.rs:16](../../../../lang/assembly/src/pipeline.rs#L16), [lower.rs:25](../../../../lang/assembly/src/lower.rs#L25) | Selects portable ZASM or the native frame-aware product; owns the representation-policy type parameter. |
| `AssemblyArena` / `AssemblyProgram` | [arena.rs:24](../../../../lang/assembly/src/arena.rs#L24), [arena.rs:47](../../../../lang/assembly/src/arena.rs#L47) | Frozen CFG: programs, variables, named symbols, labels, contexts, frame entries, externs. |
| `Instruction` / `Terminator` / `Extern` | [syntax.rs:53](../../../../lang/assembly/src/syntax.rs#L53), [syntax.rs:37](../../../../lang/assembly/src/syntax.rs#L37), [syntax.rs:187](../../../../lang/assembly/src/syntax.rs#L187) | The ZASM surface that stack analysis and emission pattern-match on. |
| `ProductLayout`, `RetainFrame` | [syntax.rs:104](../../../../lang/assembly/src/syntax.rs#L104), [syntax.rs:83](../../../../lang/assembly/src/syntax.rs#L83) | Logical-versus-physical product arity, and the native suspension marker that replaces capture products. |
| `StackAnalyzer` | [analyze.rs:51](../../../../lang/assembly/src/analyze.rs#L51) | Computes per-program operand-stack layouts and inlines statically known `popjmp` targets. |
| `RepresentationPolicy` / `LocalUnboxing` | [representation.rs:26](../../../../lang/assembly/src/representation.rs#L26), [unbox.rs:21](../../../../lang/assembly/src/unbox.rs#L21) | Analysis justifies an opportunity; the policy only accepts or rejects it. |
| `NativeProgram` / `FramePlan` / `FramePlanError` | [frames.rs:31](../../../../lang/assembly/src/frames.rs#L31), [frames.rs:49](../../../../lang/assembly/src/frames.rs#L49), [frames.rs:18](../../../../lang/assembly/src/frames.rs#L18) | Checked activation ownership, entry contracts, slot map, liveness, root maps, scalar homes. |
| `zydeco_machine::frames::{Action, Environment, STEP_SYMBOL}` | [frames.rs:247](../../../../lang/machine/src/frames.rs#L247), [frames.rs:30](../../../../lang/machine/src/frames.rs#L30), [frames.rs:330](../../../../lang/machine/src/frames.rs#L330) | The descriptor wire format and transition capability shared by emitter and stub. |
| `zydeco_amd64::Emitter` / `NativeEntry` / `TargetFormat` | [emit.rs:76](../../../../lang/amd64/src/emit.rs#L76), [emit.rs:35](../../../../lang/amd64/src/emit.rs#L35), [emit.rs:21](../../../../lang/amd64/src/emit.rs#L21) | Emits text and read-only data for a process, C export, or native unit entry. |
| `BackendProgram::emit_amd64` / `Amd64Artifact` | [compile.rs:440](../../../../cli/src/compile.rs#L440), [compile.rs:317](../../../../cli/src/compile.rs#L317) | The single-lowering boundary the CLI and test runners consume. |
| `BuildOptions` / `link_amd64_resolved` | [native.rs:11](../../../../cli/src/native.rs#L11), [native.rs:98](../../../../cli/src/native.rs#L98) | Build-directory contract, toolchain invocation, runtime link, executable publication. |
| `RuntimeInstance` / `run_process` | [stub.rs:1064](../../../../runtime/stub.rs#L1064), [stub.rs:1164](../../../../runtime/stub.rs#L1164) | Per-entry heap, frames, transfer slot, retained memory, I/O, arguments; process launcher. |
| `CheneyHeap` / `BlockStartIndex` | [gc.rs:233](../../../../runtime/gc.rs#L233), [gc.rs:105](../../../../runtime/gc.rs#L105) | Two 1 MiB semispaces and the index that locates an allocation from an interior pointer. |
| `RuntimeWord` / `Closure` / `HostTransfer` | [word.rs:43](../../../../lang/machine/src/word.rs#L43), [closure.rs:9](../../../../lang/machine/src/closure.rs#L9), [native.rs:36](../../../../lang/machine/src/native.rs#L36) | Tagged-word encoding, closure field order, and the host-control return record. |

## 4. Contracts and Invariants

- **Portable versus checked-native split.** Portable rendering may assume ordinary lowering; only
  `NativeProgram::prepare` asserts activation ownership, initialization, alias, and resumption-context
  validity, and emission cannot fall back to an unchecked layout. Evidence:
  [pipeline.rs:36](../../../../lang/assembly/src/pipeline.rs#L36), [frames.rs:118](../../../../lang/assembly/src/frames.rs#L118); canonical:
  [C11 opening](../../../references/compiler.md#c11-native-preparation-activation-frames-and-amd64-emission).
- **Slot assignment preserves pending continuations.** A later result may reuse a dead slot only when no pending
  continuation still needs its binding; a write interferes with everything preserved across it; deterministic
  greedy coloring assigns canonical definitions to slots and aliases inherit their source slot; the same offsets
  feed accesses, capture descriptors, and root maps. Evidence:
  [frames.rs:255](../../../../lang/assembly/src/frames.rs#L255), [frames.rs:285](../../../../lang/assembly/src/frames.rs#L285); canonical:
  [C11 activation lifetime](../../../references/compiler.md#activation-lifetime).
- **Frame actions are static descriptors consumed by the model.** Enter, Suspend, Resume, and Roots serialize as
  header words plus trailing slot indices in read-only data; generated code calls `zydeco_frame_step`; transitions
  validate layout, slots, and suspension nesting and never collect the managed heap. Evidence:
  [emit.rs:345](../../../../lang/amd64/src/emit.rs#L345), [frames.rs:296](../../../../lang/machine/src/frames.rs#L296); canonical:
  [C12 environment actions and roots](../../../references/compiler.md#environment-actions-and-roots).
- **Tagged words and scalar homes stay distinct.** Odd words are immediate and aligned even words pointer-shaped;
  `Int64`/`UInt64`/`Float64` cross general value boundaries as opaque boxes, while verified scalar regions get
  checked value homes and untraced raw homes below the collector's root cursor. Evidence:
  [word.rs:43](../../../../lang/machine/src/word.rs#L43), [primitive.rs:85](../../../../lang/amd64/src/emit/primitive.rs#L85); canonical:
  [C12 scalar value boundaries](../../../references/compiler.md#scalar-value-boundaries), [L13](../../../references/language.md#13-primitive-values-and-capabilities).
- **Model identity is a source fingerprint, not an ABI promise.** `lang/machine/build.rs` hashes the model's
  `Cargo.toml`, `build.rs`, and `src/` with FNV-1a; the compiler embeds the same sources and copies them into the
  build directory, so process and unit entry symbols include the fingerprint and stale pairs fail to link. Evidence:
  [build.rs:32](../../../../lang/machine/build.rs#L32), [native.rs:76](../../../../cli/src/native.rs#L76),
  [native_model.rs:9](../../../../lang/tests/tests/native_model.rs#L9); canonical:
  [C11 artifacts](../../../references/compiler.md#compilation-unit-preparation-and-artifacts), [DESIGN](../../../../DESIGN.md#shared-rust-runtime-model).
- **Host calls cross a SysV boundary with a declared result protocol.** Host operations are exported as
  `zydeco_*` `extern "sysv64"` functions; the emitter places arguments in SysV registers or reuses the incoming
  stack, passes a spare box for full-width scalar results, and returns a `HostTransfer` whose bridge resumes Zydeco code
  (`\x01`-prefixed exports suppress the Mach-O underscore; `nm` shows bare `zydeco_frame_step`). Evidence:
  [emit.rs:910](../../../../lang/amd64/src/emit.rs#L910), [native.rs:63](../../../../lang/machine/src/native.rs#L63),
  [C14 foreign calls](../../../references/compiler.md#foreign-calls).
- **Generated stack alignment is a static-dataflow fact.** `rsp` parity propagates from the root through static
  edges; frame entries start with both parities, and calls at either-parity points align dynamically and restore
  the saved `rsp`. Evidence: [emit.rs:140](../../../../lang/amd64/src/emit.rs#L140),
  [emit.rs:291](../../../../lang/amd64/src/emit.rs#L291); canonical:
  [C11 emission paragraph](../../../references/compiler.md#c11-native-preparation-activation-frames-and-amd64-emission).

## 5. Data Flow

Lowering is a single pass producing two views of one graph. `Lowerer` consumes SPSLow values, patterns, and
computations through the CPS folder, while `LocalUnboxing` and `ScalarPlans` decide which constructions can be
expanded and which scalar operations become raw regions ([unbox.rs:32](../../../../lang/assembly/src/unbox.rs#L32),
[lower.rs:25](../../../../lang/assembly/src/lower.rs#L25)). The portable view finishes after `StackAnalyzer`
records stack layouts and rewrites inlined jumps ([analyze.rs:100](../../../../lang/assembly/src/analyze.rs#L100)).
The native view runs the same lowering with native frames, then `FramePlan::analyze` assigns owners by walking
frame-entry roots and resumption edges, performs must-initialization and alias checks, computes local liveness and
pending-capture may-sets, colors the interference graph, and records scalar storage
([frames.rs:118](../../../../lang/assembly/src/frames.rs#L118)). The emitter then walks the frozen arena once,
emitting the root and named blocks, frame actions, root descriptors, jump tables, and string literals
([emit.rs:646](../../../../lang/amd64/src/emit.rs#L646)).

At packaging time the runtime sources and the machine-model bundle enter the build directory; NASM turns the
assembly into one object, `ar` archives it, and the runtime's Cargo build links it into the `main` binary that
becomes the executable ([native.rs:34](../../../../cli/src/native.rs#L34),
[native.rs:98](../../../../cli/src/native.rs#L98)). At run time `run_process` installs a `RuntimeInstance` and jumps to the model's fingerprinted entry; the
generated root then enters its activation through `zydeco_frame_step`, and the stub owns the heap, frames,
transfer slot, and host resources for that entry ([stub.rs:1064](../../../../runtime/stub.rs#L1064)). Collection
happens only inside allocation, whose deferred root source enumerates the control stack and live frame slots
([gc.rs:264](../../../../runtime/gc.rs#L264)).

```text
SpsLowProgram
  └─ Lowerer(+policy) ────────────────> AssemblyBuild
       └─ StackAnalyzer ─────────────> AssemblyProgram ──> -t zasm (Formatter)
       └─ with_native_frames ────────> FramePlan ──> NativeProgram
             └─ amd64::Emitter(TargetFormat, NativeEntry) ──> Amd64Artifact (.s)
                   └─ BuildOptions::prepare: runtime/*.rs + machine/ bundle + Cargo.toml
                         └─ nasm → ar → cargo build runtime → build/<name>.exe
                               runtime: entry_begin → Frames<Growable> + CheneyHeap
                                        zydeco_frame_step / zydeco_alloc_* / host symbols
```

## 6. Verification Commands

Run from the repository root. The fixture is `target/study-native/hello.zy`, the vertical-slice program:
`param (/stdio; /process) : @(import("../../lib/std/builtin.zy")) in ! (stdio/write_line) "hello, study!" { ! (process/exit) 0 }`.
On this arm64 macOS host the native targets need an explicit x86-64 architecture; without it every native target
fails before compiling ([main.rs:370](../../../../cli/src/main.rs#L370)).

| Purpose | Command | Expected observable outcome |
| --- | --- | --- |
| Render ZASM | `cargo run --quiet --bin zydeco -- build target/study-native/hello.zy --target zasm` | 12 lines from `[sym:...]`/`[root]`, ending in `<extern:write_line/2:Control>`; exit 0. |
| Emit assembly | `cargo run --quiet --bin zydeco -- build target/study-native/hello.zy --target asm --target-arch x86-64` | 123 lines of NASM; `extern zydeco_frame_step`, `global zydeco_entry_frames_<32 hex>`, frame actions and root data; exit 0. |
| Reject a non-AMD64 target | `cargo run --quiet --bin zydeco -- build target/study-native/hello.zy --target asm --target-arch aarch64` | Exit 1 with `the AMD64 backend cannot target aarch64; ... pass --target-arch x86-64`; nothing emitted. |
| Build the native executable | `cargo run --quiet --bin zydeco -- build target/study-native/hello.zy --target exe --target-arch x86-64 --build-dir target/study-native/build` | Exit 0; `target/study-native/build/hello.exe` appears; first run takes about a minute for the nested runtime build. |
| Run the native executable | `target/study-native/build/hello.exe` | Prints `hello, study!` and exits 0; `file` reports `Mach-O 64-bit executable x86_64` (Rosetta on this host). |
| See a representation policy change code | `cargo run --quiet --bin zydeco -- build lib/tests/core/representation-policies.zy --target asm --target-arch x86-64 --representation boxed` then the same with `--representation shared` | My runs: 802 assembly lines with 10 `call zydeco_alloc_scanned` sites for `boxed`, 753 lines with 8 for `shared`; both exit 0. Static code counts, not executed allocations. |
| Machine-model unit tests | `cargo test -q -p zydeco-machine --lib` | 13 passed. |
| Frame-plan unit tests | `cargo test -q -p zydeco-assembly --lib frames::tests` | 5 passed, 24 filtered out. |
| GC and suspended-root tests | `cargo test -q -p zydeco-tests --test native_gc` | 14 passed. |
| Native packaging and model-mismatch test | `cargo test -q -p zydeco-tests --test native_model` | 3 passed, including the release-runtime link and the mismatched-model rejection. |
| Representation CLI test | `cargo test -q -p zydeco-cli --test representation` | 4 passed. |

I did not run `cargo test-all`, `scripts/bench-native.sh`, the C-library/native-unit link paths, or timings; see
§10 for the unverified parts.

## 7. Reading Order

1. [C10](../../../references/compiler.md#c10-zasm-stack-analysis-and-local-representation-choices) and
   [C11](../../../references/compiler.md#c11-native-preparation-activation-frames-and-amd64-emission): phase
   boundaries and the native contract before any code.
2. [02 — Vertical Slice](../02-vertical-slice.md), steps 4–7: the same products this card describes, on one program.
3. [lib.rs](../../../../lang/assembly/src/lib.rs) and [syntax.rs](../../../../lang/assembly/src/syntax.rs): ZASM's
   three sorts, instructions, layouts, and extern forms.
4. [pipeline.rs](../../../../lang/assembly/src/pipeline.rs) then [lower.rs](../../../../lang/assembly/src/lower.rs):
   which two products the one lowering pass can publish.
5. [analyze.rs](../../../../lang/assembly/src/analyze.rs): how operand-stack layouts are computed and why jumps inline.
6. [frames.rs](../../../../lang/assembly/src/frames.rs): ownership, initialization, liveness, pending captures, and
   coloring; this is the densest file in the module.
7. [amd64/src/emit.rs](../../../../lang/amd64/src/emit.rs): parity, frame descriptors, allocation, extern calls,
   then [emit/primitive.rs](../../../../lang/amd64/src/emit/primitive.rs) and
   [emit/memory.rs](../../../../lang/amd64/src/emit/memory.rs) for scalar and memory instructions.
8. [machine/src/native.rs](../../../../lang/machine/src/native.rs), [word.rs](../../../../lang/machine/src/word.rs),
   [closure.rs](../../../../lang/machine/src/closure.rs), then [frames.rs](../../../../lang/machine/src/frames.rs):
   the model both compiler and stub instantiate.
9. [runtime/stub.rs](../../../../runtime/stub.rs), [gc.rs](../../../../runtime/gc.rs), [memory.rs](../../../../runtime/memory.rs),
   [main.rs](../../../../runtime/main.rs): the target side, including the 59 `export_name` sites that publish its host ABI.
10. [cli/src/native.rs](../../../../cli/src/native.rs), [cli/README.md](../../../../cli/README.md#native-executables), and
    [proposals/native-frames.md](../../../proposals/native-frames.md#removed-experiments): packaging and the storage
    alternatives that were tried and removed.

## 8. Checkpoint Questions

1. Which two products can one assembly lowering run publish, and what selects the second?
   → [C10](../../../references/compiler.md#c10-zasm-stack-analysis-and-local-representation-choices), [pipeline.rs:36](../../../../lang/assembly/src/pipeline.rs#L36).
2. What does `NativeProgram::prepare` check that `StackAnalyzer` cannot?
   → [C11 opening](../../../references/compiler.md#c11-native-preparation-activation-frames-and-amd64-emission), [frames.rs:118](../../../../lang/assembly/src/frames.rs#L118).
3. Why can a dead write still interfere with a live binding, and when may a result reuse a dead slot?
   → [C11 activation lifetime](../../../references/compiler.md#activation-lifetime), [frames.rs:285](../../../../lang/assembly/src/frames.rs#L285).
4. Which frame transitions become static descriptors, and who executes them?
   → [C12 environment actions and roots](../../../references/compiler.md#environment-actions-and-roots), [emit.rs:345](../../../../lang/amd64/src/emit.rs#L345).
5. Why do block entries start with unknown `rsp` parity, and what does the emitter do when parity is unknown?
   → [C11 emission paragraph](../../../references/compiler.md#c11-native-preparation-activation-frames-and-amd64-emission), [emit.rs:140](../../../../lang/amd64/src/emit.rs#L140).
6. How do the emitter and the stub agree on closure offsets, transfer offsets, and action words?
   → [DESIGN shared model](../../../../DESIGN.md#shared-rust-runtime-model), [native.rs:63](../../../../lang/machine/src/native.rs#L63).
7. Where does the runtime's copy of the machine model come from, and why can a stale pair fail to link?
   → [C11 artifacts](../../../references/compiler.md#compilation-unit-preparation-and-artifacts), [build.rs:32](../../../../lang/machine/build.rs#L32), [native.rs:76](../../../../cli/src/native.rs#L76).
8. Why is `Int64` boxed while `Int` fits in a word, and why may raw scalar homes not be traced?
   → [C12 scalar value boundaries](../../../references/compiler.md#scalar-value-boundaries), [L13](../../../references/language.md#13-primitive-values-and-capabilities).
9. What does `--representation` change, and which backend does it not reach?
   → [C10 policy selection](../../../references/compiler.md#policy-selection), [cli guide](../../../../cli/README.md#representation-experiments).
10. Which frame storage experiments were removed, and why is frame planning still the default?
    → [native-frames proposal](../../../proposals/native-frames.md#removed-experiments), [C11 activation lifetime](../../../references/compiler.md#activation-lifetime).

## 9. Redundancy and Cleanup Signals

Statuses follow the evaluation gates: `open` = removable or outstanding with no approved-design conflict,
`decision needed` = changes approved reference text or rests on an unfixed design choice, `resolved` = removed or
deliberately settled. The shared inventory is [appendix-cleanup-candidates.md](../appendix-cleanup-candidates.md).

| # | Signal | Evidence | Status | Assessment |
| --- | --- | --- | --- | --- |
| 1 | Explicit frame planning versus fixed slots | [size analysis backend shape](../../../evaluations/2026-10-25-size-analysis/README.md#design-decisions-for-the-artifact) (≈1.1k; `wasm-am` fixed-slot precedent); [appendix D4](../appendix-cleanup-candidates.md) | decision needed | Frame planning is explicitly [retained after review](../../../evaluations/2026-10-21-cleanup-audit/README.md#retained-after-review), so fixed slots are a design decision about the SPS implementation claim, not housekeeping. |
| 2 | Representation policies selected by `build --representation` | [audit post-update](../../../evaluations/2026-10-21-cleanup-audit/README.md) dropped the finding; [representation.rs:80](../../../../lang/assembly/src/representation.rs#L80); my boxed/shared measurement in §6 | resolved | Kept: four policies, default `Local`; the CLI translates its own spelling at entry ([lib.rs:40](../../../../cli/src/lib.rs#L40)) and the option reaches ZASM, asm, exe, Wasm, and — through `CommandCompiler` — library/unit lowering. |
| 3 | Duplicate runtime digest and platform computation on every `build -t exe` | [main.rs:497](../../../../cli/src/main.rs#L497)–[514](../../../../cli/src/main.rs#L514) hashes the runtime at lines 503 and 512 and builds `LibraryPlatform` twice; [audit defect](../../../evaluations/2026-10-21-cleanup-audit/README.md#defects-found-during-the-audit) | open | Compute both once before the two `validate_*` calls; code-only change. Still absent from `docs/todos`, although the audit says it belongs there before repair. |
| 4 | Host ABI written once per consumer; `runtime/stub.rs` is 1,264 lines with 59 `export_name` sites | [size analysis builtins](../../../evaluations/2026-10-25-size-analysis/README.md#builtins-written-once-per-consumer) (≈2k for one table); [audit parallel implementations](../../../evaluations/2026-10-21-cleanup-audit/README.md#parallel-implementations); appendix B3 | decision needed | The direction is one table generating role signatures and stubs for interpreter, stub, and Node host; it changes the [C14](../../../references/compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls) contract, and giving arithmetic an intrinsic body is the prerequisite. |
| 5 | Product-suffix layouts lack end-to-end coverage | [todos: suffix layouts](../../../todos/README.md); emitter branches [emit.rs:1005](../../../../lang/amd64/src/emit.rs#L1005) and [emit.rs:1047](../../../../lang/amd64/src/emit.rs#L1047) | open | Source lowering emits only full-arity layouts, so the `elements < arity` branches are reachable only through constructed programs; add backend fixtures before any source producer. |
| 6 | `--target-arch` accepts one supported value | [cli.rs:107](../../../../cli/src/cli.rs#L107)–[111](../../../../cli/src/cli.rs#L111); rejection at [main.rs:370](../../../../cli/src/main.rs#L370); [audit test-only](../../../evaluations/2026-10-21-cleanup-audit/README.md#test-only-generality) and [post-audit](../../../evaluations/2026-10-21-cleanup-audit/README.md#post-audit-verification) | open | `Aarch64` parses and is rejected before compiling; HEAD `e2dd8979` moved the rejection earlier and documented the requirement. Removing the option(s) changes `cli/README.md` text, so it is a reference-gated cleanup. |
| 7 | The AMD64-only gate is repeated at three layers | [main.rs:370](../../../../cli/src/main.rs#L370), [native.rs:103](../../../../cli/src/native.rs#L103), [library.rs:56](../../../../cli/src/library.rs#L56) (`for_target`), plus host selection at [execution.rs:62](../../../../cli/src/execution.rs#L62) | open | Mechanical dedupe once signal 6 settles the CLI surface; today each layer guards its own public entry point. |
| 8 | Removed frame experiments and the ZASM interpreter | `9ef854f2` (compact/moving environment experiments, 697 deletions) and `36a60594` (ZASM interpreter); [native-frames removed experiments](../../../proposals/native-frames.md#removed-experiments); [evaluation update](../../../evaluations/2026-10-21-cleanup-audit/README.md) | resolved | Do not look for `frames/fragments.rs`, `frames/moving.rs`, or a ZASM evaluator; `Frames` retains only the `Storage` split `Fixed`/`Growable` ([storage.rs:26](../../../../lang/machine/src/frames/storage.rs#L26), [storage.rs:64](../../../../lang/machine/src/frames/storage.rs#L64)). |

## 10. Uncertainties and Drift

- **Undocumented export-name prefix.** `runtime/stub.rs` names its exports `\x01zydeco_*` and
  `lang/machine/build.rs` generates the matching `\x01`-prefixed declarations; the
  leading SOH is load-bearing because it suppresses the Mach-O underscore, and `nm target/study-native/build/hello.exe`
  shows bare `zydeco_frame_step` and `zydeco_entry_frames_<fingerprint>`. No reference section explains it; either
  document it where C12 owns the model contract or treat it as a hazard for new host symbols.
- **Unguarded host-call arity.** The emitter fills SysV registers up to six and hits `todo!()` beyond that
  ([emit.rs:936](../../../../lang/amd64/src/emit.rs#L936)); the current role table tops out at five arguments
  ([syntax/lib.rs:738](../../../../lang/syntax/src/lib.rs#L738)), so it is unreachable today but would panic during
  emission if a seventh host argument appeared. No validation rejects such a role.
- **Library/unit representation scope.** The code applies the selected strategy through
  `CommandCompiler::lower_export`/`lower_unit` ([compile.rs:142](../../../../cli/src/compile.rs#L142),
  [compile.rs:163](../../../../cli/src/compile.rs#L163)), while C10's sentence names ZASM, assemblies/executables, and
  Wasm ([C10](../../../references/compiler.md#policy-selection)). I did not build a C library or native unit to
  confirm the end-to-end effect, so the reference's intended scope is unverified.
- **Evaluation figures are dated.** The evaluations describe `7e5ee4c1` and `6aae43d0`; this card re-measured current
  counts only (for example `runtime/stub.rs` is 1,264 lines now, matching appendix B3, not the size analysis's 1,267
  at its revision). All ≈ estimates remain estimates.
- **Not run here.** Full `cargo test-all`, `scripts/bench-native.sh` timings, C-library/native-unit link commands,
  and direct execution of the product-suffix layout branches. The first nested native build took about a minute;
  later builds reused the packaged runtime. Expect these omissions to matter only for performance claims, not for
  the contracts above.
- **Drift process gap.** The duplicate digest (signal 3) and the undocumented `\x01` convention are undocumented in
  the references or todos; the former is called out as unrecorded drift by the audit, and neither is fixed by the
  working-tree proposals (`native-frames.md` only adds a cross-reference).
