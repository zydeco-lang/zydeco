# WebAssembly backend strategies

The prototypes remain separately named `wasm-am` and `wasm-sps` while the default target is undecided.
[C13](../references/compiler.md#c13-webassembly-backends-and-embedding) owns their current emitters,
module ABI, representations, and embedding limits.
This proposal retains default-target criteria, optional target features,
reclamation choices, and historical selection evidence.
The numerical prototype record below dates to 2026-08-30;
[the 2026-09-08 runtime study](../evaluations/2026-09-08-cbpv-runtime/README.md#webassembly-memory) supplies
later bounded memory evidence.

## Prototype Comparison

The following historical measurement, recorded in commit `ba70305b` on 2026-08-30,
compiled `lib/tests/compile/fact.zy` with both backends.
It is one directional size measurement, not a runtime benchmark, but it exposes the structural difference clearly.

| Property | `wasm-am` | `wasm-sps` |
| --- | ---: | ---: |
| Backend input | ZASM `AssemblyProgram` | `SpsLowProgram` |
| Defined WebAssembly functions | 13,935 | 525 |
| Module size | 713,838 bytes | 222,451 bytes |
| Assembly construction required | yes | no |
| Lexical bindings | memory environment | WebAssembly locals |
| Control stack | fixed one-megabyte region | persistent heap frames |
| Product representation | ZASM-guided, with local unboxing | uniformly boxed |

For this input, preserving SPS block structure reduces the emitted module by about 69 percent
and reduces the number of defined functions by about 96 percent.
These figures motivate the higher-level path, but they do not yet establish execution speed, peak memory,
or behavior on a representative program corpus.

## Validation Evidence

The 2026-08-30 prototype record reported the following evidence; these counts and hashes are historical:

- both emitted modules pass `wasmparser` validation in focused backend tests;
- focused CLI tests verify the distinct target names, suffixes, module validity, and relative function counts;
- the committed Node.js host runs 126 existing source cases on each backend, for 252 WebAssembly executions;
- that corpus includes the compile tests, examples, the `exec` suite, OOPSLA artifacts, effects, packs,
  stack/control cases, tutorial programs, and standard-library collection, text, numeric,
  argument-list, and filesystem tests;
- the cases cover direct calls, higher-order closures, tuples and product suffixes, match/comatch,
  cloned continuations, string and byte operations, every numeric width, host control transfers,
  and persistent-stack allocation stress;
- signed and unsigned tagged-word boundary cases execute successfully;
- the 4,096-iteration allocation stress program passes on both backends; an earlier SPS measurement grew
  to 85 WebAssembly pages, approximately 5.5 MiB; and
- two independent SPS builds
  of `fact.zy` produced identical bytes (`84ca8e63a78995214b941649570695fdf6a3ffd9796d20eca14efd47340b24cf`).

The current [source harness](../references/compiler.md#source-fixtures-and-runtime-oracles) also supports
declared stdin, captured output, and expected exits across registered backends.
That capability does not establish coverage of every embedding edge case.
Multi-argument process folds now use the [source argument library](../../lib/std/system/arguments.zy)
and execute in the focused argument regressions on both backends.
The historical stress result demonstrates growth rather than an acceptable long-running memory policy.

## Alternatives Considered

### Require tail calls or typed function references

WebAssembly proposals could express dynamic tail transfer more directly.
Making them mandatory would narrow the set of usable engines before measurements show
that the core trampoline is a bottleneck.
They remain a possible optional target feature rather than a baseline requirement.

### Choose an unqualified `wasm` target now

Aliasing one prototype now would turn an architectural experiment into a compatibility promise.
Explicit names make scripts state which lowering they rely on and let both artifacts coexist
until the evidence supports a default.
When a default is selected, the transition should be direct: add `wasm`, update callers,
and document whether the non-default implementation remains supported, without retaining an ambiguous legacy alias.

## Criteria for selecting `wasm`

First establish semantic parity with declared input, output, successful and failing exits,
numeric boxing boundaries, products, closures, continuations, and the shared host calling modes.
Use [C16's runtime oracles](../references/compiler.md#source-fixtures-and-runtime-oracles)
and identify the cases actually exercised on each backend; harness support alone is insufficient evidence.
Multi-argument process folds now pass the shared [argument regressions](../../lang/tests/tests/builtin.rs);
continued conformance work should include reuse and abandonment of ordinary source continuations.

Then compare module size and function count, compile and validation time, execution time,
peak and retained linear memory, unsupported-form diagnostics, and host-interface stability.
Workloads must include computation, allocation, and control-heavy programs and long-running phases.
A structural size advantage alone cannot choose the default.

If the direct SPS path preserves parity and its advantages survive those measurements, it is a candidate
for the default; the AM path can remain supported or serve as an oracle if that role justifies its maintenance.
Make that support decision explicit when introducing `wasm`.

## Open questions

- How should persistent SPS frames and heap values be reclaimed: tracing, regions,
  destructive reuse, a shadow stack, or another continuation representation?
  Establish lifetime and sharing requirements before selecting the collector or storage discipline.
- Which product-unboxing decisions belong at a shared SPSLow boundary, and which need target-specific analysis?
  Compare per-block local plans with the current whole-program plan.
- Which word layouts and spare-box conventions should external hosts rely on, and how should that ABI be versioned?
- Should control transfers use a typed record instead of the current four results and two-argument limit?
- Should a WASI adapter own process startup and exit, and which layouts belong in shared Wasm support?
- What maintained role should `wasm-am` have if `wasm-sps` becomes the default?

Prioritize a reproducible current baseline and long-running reclamation measurements.
Account separately for live values, persistent/control storage, cached capacity, and host resources.
The [runtime evidence](../evaluations/2026-09-08-cbpv-runtime/README.md#evidence-and-limits) retains the commands,
hashes, and limits needed to rerun the earlier experiments; it does not prescribe the answers here.
