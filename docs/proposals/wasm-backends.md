# WebAssembly backend strategy

The `wasm-sps` target keeps its qualified name while the unqualified `wasm` target is undecided.
[C13](../references/compiler.md#c13-webassembly-backend-and-embedding) owns the current emitter,
module ABI, representations, and embedding limits.
This proposal retains the criteria for naming the default target, optional target features,
reclamation choices, and the open runtime questions.
The structured SPSLow path replaced an abstract-machine prototype over ZASM after a 2026-08-30 comparison
of `lib/tests/compile/fact.zy` showed it emitting about 69 percent fewer bytes and 96 percent fewer functions;
[the 2026-09-08 runtime study](../evaluations/2026-09-08-cbpv-runtime/README.md#webassembly-memory) retains
the later bounded memory evidence for both prototypes.

## Validation Evidence

The [source harness](../references/compiler.md#source-fixtures-and-runtime-oracles) supports declared stdin,
captured output, and expected exits on the backend, and the committed Node.js host executes the compile tests,
examples, the `exec` suite, OOPSLA artifacts, effects, packs, stack/control cases, tutorial programs,
and standard-library collection, text, numeric, argument-list, and filesystem tests.
These cases cover direct calls, higher-order closures, tuples and product suffixes,
match/comatch, cloned continuations, string and byte operations, every numeric width, host control transfers,
persistent-stack allocation stress, and signed and unsigned tagged-word boundaries.
Multi-argument process folds use the [source argument library](../../lib/std/system/arguments.zy)
and execute in the focused [argument regressions](../../lang/tests/tests/builtin.rs).
Emission sorts arena-derived identifiers, so repeated builds of one source produce identical bytes.
Harness support alone does not establish coverage of every embedding edge case,
and the allocation stress results demonstrate growth rather than an acceptable long-running memory policy.

## Alternatives Considered

### Require tail calls or typed function references

WebAssembly proposals could express dynamic tail transfer more directly.
Making them mandatory would narrow the set of usable engines before measurements show
that the block trampoline is a bottleneck.
They remain a possible optional target feature rather than a baseline requirement.

### Choose an unqualified `wasm` target now

Aliasing the prototype now would turn an architectural experiment into a compatibility promise.
The explicit name makes scripts state which lowering they rely on.
When a default is selected, the transition should be direct: add `wasm`, update callers,
and retire `wasm-sps` without retaining an ambiguous legacy alias.

## Criteria for selecting `wasm`

First establish semantic parity with declared input, output, successful and failing exits,
numeric boxing boundaries, products, closures, continuations, and the shared host calling modes.
Use [C16's runtime oracles](../references/compiler.md#source-fixtures-and-runtime-oracles)
and identify the cases actually exercised; harness support alone is insufficient evidence.
Continued conformance work should include reuse and abandonment of ordinary source continuations.

Then measure module size and function count, compile and validation time, execution time,
peak and retained linear memory, unsupported-form diagnostics, and host-interface stability.
Workloads must include computation, allocation, and control-heavy programs and long-running phases.
A structural size advantage alone cannot choose the default.

## Open questions

- How should persistent SPS frames and heap values be reclaimed: tracing, regions,
  destructive reuse, a shadow stack, or another continuation representation?
  Establish lifetime and sharing requirements before selecting the collector or storage discipline.
- Which product-unboxing decisions belong at a shared SPSLow boundary, and which need target-specific analysis?
  Compare per-block local plans with the current whole-program plan.
- Which word layouts and spare-box conventions should external hosts rely on, and how should that ABI be versioned?
- Should control transfers use a typed record instead of the current four results and two-argument limit?
- Should a WASI adapter own process startup and exit, and which layouts belong in shared Wasm support?

Prioritize a reproducible current baseline and long-running reclamation measurements.
Account separately for live values, persistent/control storage, cached capacity, and host resources.
The [runtime evidence](../evaluations/2026-09-08-cbpv-runtime/README.md#evidence-and-limits) retains the commands,
hashes, and limits needed to rerun the earlier experiments; it does not prescribe the answers here.
