# Outstanding discrepancies

Keep observed implementation and documentation drift here, with its governing reference,
current evidence, and a bounded repair.
Recording a discrepancy does not authorize a language-design change.
Remove resolved entries once their repair and durable rationale have reached the owning document or regression.

- [Reference drift](reference-drift.md): remaining formal-calculus disagreements and duplicate documentation accounts.
- [Compiler boundaries](compiler-boundaries.md): product inference, computation-witness diagnostics,
  higher-order value-function inference, source-load error locations, monadic closure validation,
  partial-annotation scope during formatting, and package-introduction payload annotations.

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
  The representation decision must cover destruction as well as lookup, including recursive destruction,
  closure environments, and cycles; list construction and traversal regressions should keep repeated references
  to one value and check deep-value teardown separately.
  At `e00b91f0` a 5,000-cell list took 2.4 seconds and 15,000 cells 22 seconds,
  and a 20,000-cell value overflowed the host stack; those thresholds were not rerun.
- Evaluate long-running host-resource retention
  under the current [runtime-instance lifetime](../references/compiler.md#runtime-instances)
  and [memory extension questions](../proposals/bytes.md#remaining-questions).
  Normal native entry teardown now releases its host resources; the older claim
  of permanently leaked `HostString::leak` allocations no longer describes that boundary.
  Any reclamation account must preserve live aliases, host callbacks, and the borrowed buffers
  of the [C FFI contract](../proposals/c-ffi.md), treat strings and bytes separately,
  and start from allocation and reclamation accounting plus workloads whose live result stays bounded.
  Allocator retention makes immediate RSS reduction an unreliable sole assertion.
- Choose a checker host-stack strategy that covers the CLI, REPL, and editor entry points rather than relying
  on a launcher's stack size, and run its regressions in child processes so an abort cannot take down the test harness.
  At `e00b91f0` an 8 MiB main thread overflowed at about 1,500 nested `let`s, 2,000 nested functions or thunks,
  and 8,000 nested type applications, while parsing succeeded on its explicit stack;
  those thresholds are platform-dependent and were not rerun.
  Deep checker recursion and [residual-code growth](../ideas/README.md#residual-code-sharing) remain separate questions.
- Exercise product-suffix layouts through the backend boundary.
  [ProductLayout](../../lang/assembly/src/syntax.rs) distinguishes physical arity
  from logical stack elements, so a final element may point into an existing product suffix
  ([C10](../references/compiler.md#c10-zasm-stack-analysis-and-local-representation-choices)).
  Source lowering currently emits only full-arity layouts, leaving the suffix branches
  in the [AMD64 emitter](../../lang/amd64/src/emit.rs) without end-to-end coverage even
  though [GC regressions](../../lang/tests/tests/native_gc.rs) cover interior pointers.
  Construct the layouts explicitly in backend regressions before adding a source producer: cover field order,
  packing and unpacking, a suffix retained after its original pointer is dropped,
  collection while that suffix is live, interpreter/native agreement, and the corresponding WebAssembly lowering.

Concrete unimplemented designs belong in [proposals](../proposals/README.md);
exploratory questions and evaluation directions belong in [design ideas](../ideas/README.md).
[CONTRIBUTING](../../CONTRIBUTING.md#maintain-documentation) owns the writing, consolidation,
and verification workflow; the [reference index](../references/README.md#rule-ownership) locates current rule owners.
