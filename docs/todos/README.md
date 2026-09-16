# Outstanding discrepancies

Keep observed implementation and documentation drift here, with its governing reference,
current evidence, and a bounded repair.
Recording a discrepancy does not authorize a language-design change.
Remove resolved entries once their repair and durable rationale have reached the owning document or regression.

- [Reference drift](reference-drift.md): remaining formal-calculus disagreements and duplicate documentation accounts.
- [Compiler boundaries](compiler-boundaries.md): product inference, computation-witness diagnostics,
  source-load error locations, unsupported ZASM execution, and documentation exposure collisions.

## Documentation follow-up, reviewed 2026-09-15

- [ ] Repair [documentation exposure collisions](compiler-boundaries.md#documentation-exposure-collisions):
  valid `std` interfaces fail all four documentation commands with duplicate `()/()` paths.
  The [documentation proposal](../proposals/documentation.md) owns the publication redesign; shared selection
  follows the [package resolution reference](../references/language.md#shared-selection-and-documentation),
  with frontend integration in the [package plan](../proposals/package-management.md).

## Next pass, reviewed 2026-09-14

The implementation review used `3b4dd665` and a newly built debug CLI, including the local,
uncommitted 16 MiB CLI worker-stack change.
These priorities distinguish reproduced failures from proposed extensions and performance investigations;
they do not change the references.

| Order | Work | Evidence and completion criterion |
| --- | --- | --- |
| 1 | [Repair product-shape inference](compiler-boundaries.md#n-ary-product-inference-refinement) | Inferred triples reject while pairs and explicitly typed triples pass. Infer the source arity and retain all three cases. |
| 2 | [Bound unsupported ZASM execution](compiler-boundaries.md#zasm-execution-panics) | A valid exit-only program panics under `build -t zasm -x`. Return a deliberate unsupported-operation error before execution, or complete the advertised execution boundary. |
| 3 | [Repair witness diagnostics](compiler-boundaries.md#nested-witness-diagnostic) and [load locations](compiler-boundaries.md#source-load-diagnostic-locations) | Explain the unsupported witness route; render imports and cycles against their owning source. Preserve the supported counterparts and independent failures. |
| 4 | [Finish package diagnostic collection](../proposals/traversals.md#diagnostic-collection-and-recovery) | Two malformed discovered files currently require two correction cycles. Report both while withholding the project and execution. |
| 5 | [Complete SPSLow recovery](../proposals/traversals.md#diagnostic-collection-and-recovery) | Structural, free-variable, entry, and protocol checks still stop at the first failure. Collect independent failures only where prerequisite structure remains valid. |
| 6 | [Repair reference drift](reference-drift.md) | Reconcile the formal calculus with supported source cases and retire obsolete accounts after preserving their evidence. |

The CLI stack mitigation is already in progress.
Its focused Wasm build regression passes with `RUST_MIN_STACK` removed from the child process;
the worker-stack implementation was still uncommitted at review time.
A larger stack is a bounded mitigation; deep checker recursion, library/editor entry points,
and [residual-code growth](../ideas/residual-code-sharing.md) remain separate questions.

## Work that needs a new baseline

- Measure [compiler retention and query costs](../proposals/arena-gc.md#next-measurement-pass)
  and [traversal costs](../proposals/traversals.md#performance-validation) before another broad representation rewrite.
  Include repeated edits, root switching, inference-region scans, nested closures, and cold versus warm checks.
- Measure native preparation separately from lowering.
  The [execution runner](../../cli/src/execution.rs) now shares one lowering
  across targets, while [native linking](../../cli/src/native.rs) still packages
  and builds runtime support in each temporary build directory.
  A cache proposal must preserve compiler/runtime pairing, target options, and changed-library relinking.
- Revisit interpreter value sharing and destruction.
  [Semantic values](../../lang/dynamics/src/syntax.rs) still recursively own constructor payloads
  and product components; [environment lookup](../../lang/dynamics/src/eval.rs) clones them.
  Reproduce lookup cost and deep teardown separately before choosing shared or arena-owned storage.
- Evaluate long-running host-resource retention
  under the current [runtime-instance lifetime](../references/compiler.md#runtime-instances)
  and [memory extension questions](../proposals/bytes.md#remaining-questions).
  Normal native entry teardown now releases its host resources; the older claim
  of permanently leaked `HostString::leak` allocations no longer describes that boundary.

## Changes to earlier findings

The monadic-literal lowering failure is resolved:
the existing `monadic_block_lifts_builtin_literals_with_trivial_value_structures` regression passes,
following `406fa554`'s empty-comatch protocol-hint repair.
High SPS ownership and free-variable analysis now share a traversal,
closure conversion reuses its variable facts, and the selected-pass regressions pass.
The shared CLI/test execution runner also replaces the earlier duplicated execution path.
Further generic traversal machinery needs a demonstrated consumer.

The [September 7 follow-up](../logs/2026-09-07-bug-hunt-follow-up.md) is historical:
its six-open-findings count is not today's backlog.
The raw `ArgumentFold` host-root mechanism is gone, and native strings have entry-owned teardown.
Interpreter recursive cloning, long-running resource retention,
and suffix-layout coverage still need their own evidence.

## Verification for this review

The debug CLI was rebuilt with `cargo build -p zydeco-cli --bin zydeco`.
Fourteen CLI invocations checked the accepted and rejected counterparts recorded
in [compiler boundaries](compiler-boundaries.md)
and [package recovery](../proposals/traversals.md#diagnostic-collection-and-recovery).
Thirteen focused Rust tests passed:

```sh
cargo test -p zydeco-session --lib monadic_block_lifts_builtin_literals_with_trivial_value_structures
cargo test -p zydeco-stackir --lib high::traverse::tests
cargo test -p zydeco-stackir --lib passes::tests
cargo test -p zydeco-cli --test source_build the_wasm_targets_write_distinct_valid_core_modules
cargo test -p zydeco-cli --lib library::tests
```

This was a focused review, with no full workspace suite, native execution benchmark, or new timing/RSS comparison.
The performance items above remain investigation priorities.

Concrete unimplemented designs belong in [proposals](../proposals/README.md);
exploratory questions and evaluation directions belong in [design ideas](../ideas/README.md).
[CONTRIBUTING](../../CONTRIBUTING.md#maintain-documentation) owns the writing, consolidation,
and verification workflow; the [reference index](../references/README.md#rule-ownership) locates current rule owners.
