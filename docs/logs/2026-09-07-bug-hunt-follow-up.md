# Worklog: bug-hunt fixes and remaining work

Status after `3d458223`, following the [2026-09-07 bug report](2026-09-07-bug-hunt.md).
The original report is preserved as received: its source paths, line numbers,
measurements, and descriptions refer to the reported checkout at `e00b91f0`.
This worklog records the subsequent implementation pass and the work still needed.
It does not establish new language rules; links below identify the existing semantic owners.

Of the report's 36 findings, 30 have a fix, a bounded failure, a documented limit,
or verification that the current implementation already handles the case.
Six remain open: A4, A5, A7, B6, D2, and G9.
A7's collector coverage is already addressed; suffix layout emission still needs coverage.
The timings and failure thresholds quoted below come from the original report,
not from new benchmark runs during this documentation change.

## Remaining work, in priority order

| Finding | Current status | Next work |
| --- | --- | --- |
| A4 | Native argument-fold tails retain host roots and allocations when abandoned. | Establish ownership and reclamation for pending host continuations. |
| A5 | Native strings and byte buffers are allocated outside the managed heap and leaked. | Connect host allocation lifetime to managed reachability. |
| D2 | Interpreter value lookup still deep-clones recursive data. | Choose shared value storage and a safe destruction strategy. |
| B6 | Deep source nesting can exhaust the checker's host stack. | Choose a stack-safety strategy and retain subprocess regressions. |
| A7 | GC interior pointers are tested; suffix layouts remain unproduced and emitter coverage is missing. | Exercise explicit suffix layouts through the backend boundary. |
| G9 | The reported loss of an abstraction annotation has no new warning or policy decision. | Pin down the exported classifier, then decide the diagnostic behavior. |

### A4: abandoned argument-fold tails

In [runtime/stub.rs](../../runtime/stub.rs), `ArgumentFold::into_thunk` converts a boxed host object
to a raw pointer and registers its `when_empty` and `when_item` fields as two managed roots.
`from_environment` unregisters those fields and recovers the box when the tail is resumed.
An abandoned tail never reaches that cleanup path.
The report's 300 calls that consume only the first argument exhaust the 256-slot host root table.

The next change needs an answer to who owns an unresumed tail and how its loss
of reachability releases both the object and its registered roots.
Increasing the root-table capacity would postpone exhaustion while retaining the leak.
Review this together with A5: both concern Rust-owned objects whose lifetimes are invisible
to the [native collector](../../DESIGN.md#native-garbage-collection).
The collector's pointer index fixes lookup cost, but cannot reclaim an object kept alive by a permanent host root.

Retain a self-contained version of the abandoned-tail reproducer before changing the representation.
Validation should also cover fully consumed tails, intentionally retained tails,
and a collection between creating and resuming a tail.
Completion means abandoned objects and root registrations are reclaimed, while reachable callbacks still contain valid,
GC-updated references.

### A5: native String and Bytes lifetime

`HostString::leak` and `HostBytes::leak` in [runtime/stub.rs](../../runtime/stub.rs) return `Box::into_raw` pointers.
These addresses lie outside the two managed semispaces, so forwarding leaves them unchanged
and never releases their Rust allocations.
The report measured about 215 MB RSS for 2,000 string appends and 524 MB for 4,000 byte appends.

Choose how managed reachability will own and release these external allocations.
That choice must account for live aliases, host callbacks, and the borrowed buffers described
by the [C FFI contract](../proposals/c-ffi.md).
The compiler's [arena reclamation proposal](../proposals/arena-gc.md) concerns checked compiler data;
it does not supply a runtime host-object ownership solution.

Start with allocation and reclamation accounting plus workloads whose live result stays bounded.
Test strings and bytes separately, including slicing, conversion, literals, and foreign-call borrows.
Completion means unreachable host allocations are released without invalidating live values.
RSS trends are supporting evidence; allocator retention makes immediate RSS reduction an unreliable sole assertion.

The fixed 1 MiB semispace is a separate capacity constraint.
A3 is closed as a documentation omission, but heap growth or configuration has not been implemented.
Lifting that capacity limit would not resolve either host-object leak.

### D2: interpreter sharing and destruction

[SemValue](../../lang/dynamics/src/syntax.rs) still contains boxed constructor payloads
and vectors of nested semantic values, with a derived `Clone` implementation.
[Evaluation](../../lang/dynamics/src/eval.rs) clones the value retrieved from the environment.
Consequently, sharing a source value can copy the entire reachable data tree.
The report's list workload grew from 2.4 seconds at 5,000 cells to 22 seconds at 15,000,
then overflowed the host stack at 20,000 cells.

The representation decision must cover both lookup and destruction.
Reference-counted or arena-backed storage may make lookup cheap, but recursive destruction,
closure environments, and cycles need an explicit account as well.
Retain list construction and traversal regressions with repeated references to one value,
then check deep-value teardown separately.
Completion means lookup shares existing immutable data and large values can be evaluated and released safely.

### B6: checker host-stack exhaustion

The report gives approximate failure thresholds of 1,500 nested `let`s, 2,000 nested functions or thunks,
and 8,000 nested type applications on an 8 MiB main-thread stack.
Those thresholds are platform-dependent and were not rerun for this worklog.
The [checker driver](../../lang/statics/src/check/driver.rs) and its recursive judgments remain the starting points
for this work.

Decide between making the affected traversals iterative, imposing an explicit checked nesting limit,
or using a larger worker stack as a bounded mitigation.
A larger stack alone would move the failure threshold.
The choice should cover CLI, REPL, and editor entry points rather than relying on the launcher's stack size.

Run these regressions in child processes so a stack abort cannot take down the test harness.
Include valid programs within the supported range and assert either successful processing
or a deliberate diagnostic beyond it, with no host panic or signal termination.
B2's static-reduction bound and F3's formatter sharing do not solve general source nesting.

### A7: dormant product-suffix layouts

This finding is partially addressed. [runtime/gc.rs](../../runtime/gc.rs) tests interior-product forwarding
and lookup work independent of block size and preceding dead blocks.
A2's index preserves interior offsets and does not depend on the report's observation
that current source lowering never produces them.

The remaining gap is `ProductLayout` with `elements < arity`:
the final logical element represents a suffix of a larger product.
Current source producers use full-arity layouts, leaving the suffix branches
in the [ZASM interpreter](../../lang/assembly/src/interp.rs) and [AMD64 emitter](../../lang/amd64/src/emit.rs)
without direct end-to-end coverage.

Construct these layouts explicitly in backend regressions before enabling a source producer.
Cover field order, packing and unpacking, a suffix retained after the original pointer is dropped,
and collection while that suffix is live.
Compare the interpreter and emitted native behavior, and cover the corresponding WebAssembly lowering.
Removing the separate stack-allocation path for A6 did not remove suffix layouts.

### G9: abstraction annotations during package introduction

The report flags `pack ... is Int64 where (0 : X)` because the resulting package can expose a concrete payload type
despite the source annotation naming the abstract binder.
No warning or new annotation policy was implemented in the fix pass. The current account
of concrete and abstract payloads is in [Package Introduction](../../DESIGN.md#package-introduction).

First retain the exact reproducer and assert its synthesized classifier,
then compare checking against an explicitly abstract package contract.
The existing [existential tests](../../lang/tests/tests/existential.rs) include accepted dependent payload annotations,
so a change must preserve intended uses as well as diagnose misleading ones.
Decide whether to warn about a concrete exported payload or revise the annotation behavior.
Any semantic decision belongs in the existing package-introduction section, with a linked summary
in the [package proposal](../proposals/package-modularization.md).

## Results against the original report

The other 30 findings are listed briefly below.
Documented limits and verified behavior are marked explicitly.

| Finding | Result | Commit |
| --- | --- | --- |
| A1 | Native rebuilds relink changed archives. | `cc500d6c` |
| A2 | Constant-time GC pointer lookup. | `5a7badbc` |
| A3 | Fixed heap limit documented; capacity unchanged. | `5a7badbc` |
| A6 | Dormant stack-allocation path removed; unboxing retained. | `4e9f7c6d` |
| A8 | Native signals produce failing CLI status. | `961d478e` |
| B1 | Discarded computation results retain required dependencies. | `89a75001` |
| B2 | Existing static-reduction bound verified by regressions. | `58d83c10` |
| B3 | Existing partial-application alias handling verified by regressions. | `dc64c963` |
| B4 | Refutable computation binders require local `@[partial]`. | `dc45989e` |
| B5 | Value-producing binders require irrefutable patterns. | `e2795b89` |
| C1 | Invalid fixpoint binders report type errors. | `4b04c8d3` |
| C2, G3 | Binding cycles rejected before checking annotations. | `fedeb4e1` |
| C3 | Invalid codata parameters report desugaring errors. | `8d1680bf` |
| C4 | Malformed monadic operation contracts diagnosed. | `83d70ddf` |
| C5 | Typed holes remain inspectable; executable holes rejected. | `2a26849b` |
| C6 | Integer zero divisors report runtime errors. | `a002efb4` |
| D1 | Interpreter stderr uses a separate stream. | `fb095078` |
| E1 | WebAssembly float rendering matches Rust at each width. | `a7491271` |
| E2 | WebAssembly machine-stack exhaustion diagnosed; capacity unchanged. | `b65ec0c3` |
| F1 | Overflowing float literals rejected. | `30def6c5` |
| F2 | Five reported files and existing formatter exit status verified. | Existing `fc55bf3f` |
| F3 | Shared formatter documents eliminate exponential layout work. | `222cc638` |
| G1 | Expected/found diagnostic ordering corrected. | `a7578b16` |
| G2 | Distinct abstract identities distinguished in diagnostics. | `3d458223` |
| G4 | Unknown escapes rejected; null and Unicode supported consistently. | `7aae19a9`, `a35a5199` |
| G5 | Inference holes render without cascading diagnostics. | `8ce60358` |
| G6 | Non-executable roots display source types. | `c6066c1a` |
| G7 | Parse diagnostics preserve source snapshots and accurate snippets. | `ef690e02` |
| G8 | Malformed numeric spellings rejected lexically. | `23fc59a1` |

C2's reproducer was a named binding cycle, rather than the anonymous definition described in the report.
A6's future implementation remains
in the [escape-analysis proposal](../proposals/escape-unboxing.md#implementation-status).

## Validation status

The implementation pass used focused regressions and finished with formatting checks
and `cargo clippy-all --offline -- -D warnings` passing.
The full workspace suite and complete fuzzing and benchmark campaign were not rerun.
This follow-up changes documentation only and does not rerun implementation tests.

Remaining items need retained reproducers, design decisions where noted, focused validation,
and separate implementation commits when work resumes.
