# Bug-hunt evidence, 2026-09-07

On 2026-09-07 a five-probe bug hunt audited the native backend and collector, the compiler and interpreter,
the WebAssembly backends, the formatter and CLI, and diagnostics.
The target was `e00b91f0` on `main` (v0.3.0), built as a release binary.
The findings led to a separate fix pass; this record retains the audit's method, host,
historical measurements, and probe limits where later work still depends on them.
It describes the measured revision and is not a current backlog or performance ranking.
Current status and next work live in [outstanding discrepancies](../../todos/README.md),
and measurements whose findings were replaced are not retained here.

## Target and method

The host was macOS arm64. Native AMD64 executables ran under Rosetta because the host lacks native x86_64 execution.
The release-only `pop_stack` bug, fixed in `d54ec327`, was already known and excluded.

Five probes produced the findings:

- semantic probes of newly added language features;
- a targeted review of the 2026-09-04 through 2026-09-06 commits;
- frontend fuzzing: 53,827 mutants of 122 seeds through `check`, about 4,000 through `run`,
  about 18,000 through `fmt` twice, and 310 hand-written cases;
- differential testing of the interpreter, AMD64, wasm-am, and wasm-sps backends over 136 corpus runs
  and targeted programs;
- a full audit of [runtime/gc.rs](../../../runtime/gc.rs), [runtime/stub.rs](../../../runtime/stub.rs),
  and [lang/amd64/src/emit.rs](../../../lang/amd64/src/emit.rs) with stress programs.

Every finding was reproduced by hand after the discovering probe reported it.
The malformed frontend inputs produced no parser or lexer panic,
and the runtime stress programs produced no segfault, bus error, or memory corruption.
The reproduction programs were hand-written probes
in the job-scoped directory `~/.claude/jobs/17fab1ae/tmp/{sem,review,fuzz,diff,gc}/` and were not copied out,
so this record has no fixtures, commands, or manifest: its numbers are documentary, not directly reproducible.

## Retained measurements

These measurements belong to work that remains open.
They were taken at `e00b91f0` on the stated host with the release binary, and later revisions are not compared.

### Interpreter value lookup

[Semantic values](../../../lang/dynamics/src/syntax.rs) recursively own constructor payloads and product components,
and environment lookup clones them, so sharing one source value can copy its reachable tree.
The audit measured a 5,000-cell list at 2.4 seconds and 15,000 cells at 22 seconds,
and a 20,000-cell value aborted with exit status 134 after a host-stack overflow near 16,000 levels of data.
The same 5,000-cell workload completed in under 0.2 seconds on the Wasm path;
that comparison crosses execution surfaces rather than comparing implementations.
The interpreter's allocation behavior does not predict native or Wasm costs
in general ([C7](../../references/compiler.md#c7-linking-and-the-reference-interpreter)).
These thresholds are platform-dependent and were not rerun.
Owning work: [revisit interpreter value sharing and destruction](../../todos/README.md#work-that-needs-a-new-baseline).

### Checker host stack

The [checker driver](../../../lang/statics/src/check/driver.rs) and its recursive judgments run on the main thread.
The audit measured host-stack exhaustion with exit status 134 at about 1,500 nested `let`s,
about 2,000 nested functions or thunks, and about 8,000 nested type applications on an 8 MiB main thread;
parsing succeeded because the parser uses an explicit stack.
These approximate thresholds are platform-dependent and were not rerun for this record.
Owning work: [checker host-stack strategy](../../todos/README.md#work-that-needs-a-new-baseline).

### Long-running host retention

At `e00b91f0`, native string and byte results were boxed and leaked through `HostString::leak` and `HostBytes::leak`.
The audit measured about 215 MB maximum resident memory for 2,000 string appends of 100 characters on the native path
against about 16 MB in the interpreter, and about 524 MB for 4,000 byte appends against about 17 MB.
These figures describe that revision and that workload.

The current runtime instead gives host strings and buffers stable storage owned
by the [runtime instance](../../references/compiler.md#runtime-instances),
and normal entry teardown releases those resources; the older leak claim does not describe that boundary.
Whether a long-running instance should reclaim unreachable host storage remains a separate question,
tracked under the [memory extension questions](../../proposals/bytes.md#remaining-questions)
and [long-running host-resource retention](../../todos/README.md#work-that-needs-a-new-baseline).
The historical RSS figures are not evidence about current entry teardown.

## Retained structural finding

### Product-suffix layouts

[ProductLayout](../../../lang/assembly/src/syntax.rs) distinguishes physical arity from logical stack elements:
when `elements < arity`, the final logical element points into an existing product suffix,
and [C10](../../references/compiler.md#c10-zasm-stack-analysis-and-local-representation-choices) owns that contract.
Code inspection during the audit found that source lowering emitted only full-arity layouts,
so the suffix branches in the ZASM interpreter
and [AMD64 emitter](../../../lang/amd64/src/emit.rs) had no end-to-end coverage.
Collector interior-pointer forwarding was covered separately
by [GC regressions](../../../lang/tests/tests/native_gc.rs).
No execution probe or measurement was retained for this finding.
Owning work: [suffix-layout coverage](../../todos/README.md#work-that-needs-a-new-baseline).

## Documented behavior retained from the audit

The audit flagged `pack ... is Int where (0 : X)`
because a payload annotation naming the abstract binder does not make the synthesized body abstract.
In current syntax the same probe is `pack (X : VType) is Int where 0 : X end`,
which synthesizes `exists (X : VType) . Int`; an expected `exists (X : VType) . X` rejects with `tyck.type-mismatch`.
[L9](../../references/language.md#9-polymorphism-and-packed-values) documents this sealing rule
as sound and directs an expected existential annotation when the abstract interface must be prescribed,
with the mechanism in [Packed Value Introduction](../../../DESIGN.md#packed-value-introduction).
No warning or different annotation policy was adopted, so the diagnostic choice remains
with [compiler boundaries](../../todos/compiler-boundaries.md#concrete-payload-annotations-during-package-introduction).
The probe was rechecked on 2026-10-20 with the repository's debug CLI.

## Resolved findings

The audit's argument-fold and host-allocation findings were resolved after the fix pass.
Their durable contracts live in [L13](../../references/language.md#13-primitive-values-and-capabilities),
the [argument-contract regressions](../../../lib/tests/builtin/argument-contract.zy),
and the [runtime-instance contract](../../references/compiler.md#runtime-instances);
the [todos](../../todos/README.md) no longer list it.
The other fixed findings' contracts and limits live in the owning references and regression tests,
and this record does not restate their fix list.

## Limits of this record

The measurements and thresholds above were recorded on one host at `e00b91f0`,
with native AMD64 code translated by Rosetta and with different execution surfaces compared where noted.
They are platform-dependent, they were not rerun for this record, and the full workspace suite
and the audit's fuzzing campaign were not repeated.
The results describe the measured revision, not the current compiler;
the [references](../../references/README.md) own current contracts
and the [todos](../../todos/README.md) own current status and next work.
