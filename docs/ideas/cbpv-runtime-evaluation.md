# Evaluating CBPV runtime representations

## Material Passport

- Origin Skill: academic-research-suite / experiment-agent
- Origin Mode: engineering experiment plan and evaluation
- Origin Date: 2026-09-08
- Verification Status: VERIFIED for the bounded experiments and source audits recorded below
- Version Label: runtime_study_v3

## Question and scope

Which runtime representations let Zydeco express its computation protocols directly
while controlling allocation, copying, root discovery, and retained space?
The goal is to compare concrete mechanisms, including the newly implemented retained frames,
without choosing a winner from the word “stack”.
The [second round](#second-round-static-layouts-and-heap-environments) extends the initial comparison
with static slot packing, growable storage, and a collector-integrated managed-frame prototype.
The [third round](#third-round-reusable-active-storage-and-compact-suspensions) evaluates compact snapshots
behind the same native action contract.

The semantic starting point is
[computation types as stack protocols](../../DESIGN.md#computation-types-as-stack-protocols).
An arrow consumes an argument and continues with another computation protocol;
`Ret A` consumes an `A`-return continuation; codata selects a protocol by an observation tag.
A thunk owns suspended code and its lexical captures.
These are distinct operations, and neither an arrow nor a block label necessarily marks an activation boundary.
The [native frame proposal](../proposals/native-frames.md) owns the current frame lifetime and GC rules;
this study evaluates their costs and alternatives.

CBPV separates decisions that a conventional call-by-value function interface packages together.
For example, `Thk (A -> B -> Ret C)` describes two argument consumptions before a return,
whereas `Thk (A -> Ret (Thk (B -> Ret C)))` explicitly returns a function value between them.
This distinction matters even when both can implement an ordinary curried source function.
Levy's [CBPV account](https://pblevy.github.io/cbpv.html) makes the value/computation
and CBV/CBN decompositions explicit.
The repository's [stack-manipulating computation paper](https://arxiv.org/html/2502.15031v1#S2) extends
that view to optional and variadic arguments, stack-walking computations, and relative monads.

The comparison with C concerns the programming and compilation boundary, not expressibility. C can host an explicit
machine, a trampoline, or a CPS translation. Its usual ABI does not automatically give a CBPV compiler arbitrary typed
residual stacks, proper tail space, managed closure lifetimes, or resumable control. The implementation must account for
those separately. The
[Koka evidence-passing work](https://www.microsoft.com/en-us/research/publication/generalized-evidence-passing-for-effect-handlers/)
is a concrete counterexample to the claim that flexible control requires abandoning C as a target.

## Independent choices

| Question | Alternatives | What must stay explicit |
| --- | --- | --- |
| How does execution transfer? | Native jumps, native calls with a restricted protocol, trampoline, CPS | Arguments, returned values, codata observations, host boundaries |
| Where is the residual control stack? | Machine stack, separate contiguous stack, linked persistent nodes, segments | Consumption, nesting, any permitted capture or duplication |
| Where do suspended locals live? | Compact capture tuples, flattened stack captures, retained slots, managed heap frames | Owner, initialized bindings, entry aliases, reclamation |
| What is one activation? | Thunk invocation, a group of compatible blocks, finer suspension regions | A semantic lifetime justified by transfers rather than syntax alone |
| How are values represented? | Tagged words, explicit boxes, typed unboxed fields, representation-polymorphic layouts | Pointer maps, alignment, width, ownership, host adapters |
| How are roots found? | Cleared ranges, sparse maps, frame descriptors, handle tables | Active and suspended liveness, relocation, temporary values |

The native machine stack can hold a protocol word without creating a C call frame.
Conversely, keeping environment words in a Rust allocation can implement a stack discipline;
the lifetime and access rules determine that discipline, not the allocator that supplied the bytes.
An escaping closure and a return continuation may therefore deserve different representations.

## Candidates and evidence boundaries

| Candidate | Concrete anchor | Principal benefit | Principal obligation or cost |
| --- | --- | --- | --- |
| Reusable environment and heap captures | Zydeco `a3df0fb8` | Compact suspended state; simple active environment reuse | Capture allocation and two-way value copying |
| Retained environment frames | Zydeco `062d5ce7` | Existing caller slots survive without capture copying | Reserved frame extent, metadata, precise suspended roots |
| Retained frames with deferred root enumeration | Experimental change to the current collector boundary | Root discovery is paid when collection actually needs it | Publish the same complete roots at every collection |
| Flattened captures on the control stack | Synthetic layout accounting | Removes the tuple allocation while retaining compact captures | Copies values at suspension and resumption; changes continuation entry shape |
| Heap activation frames | Collector-integrated prototype in the second round; no native backend | Potential for independently retained environments | Reclamation, precise field liveness, and frame retention |
| Persistent linked stacks and lexical locals | Zydeco `wasm-sps` | Explicit residual stacks and structured block code | Per-push nodes; current bump allocator never reclaims them |
| Contiguous abstract-machine stack and reusable environment | Zydeco `wasm-am` | Direct operational reference; bounded stack | Per-instruction dispatch and capture products |
| Segmented or copied resumable stacks | OCaml/MLton literature and implementation survey | Storage for detached or later-reentered continuations | Ownership or copying on capture/resumption; GC and FFI integration |

Detached heap frames and resumable stacks provide capabilities beyond the current native `Ret` tokens.
CBPV alone does not make those tokens unrestricted first-class continuations,
nor does it force a destructive stack implementation.
Library-encoded control and primitive machine-stack capture must be evaluated separately.

## What other implementations actually choose

### CBPV implementations

[Fiddle](https://github.com/zydeco-lang/fiddle/tree/9a8941c224635ccfa1955c4ecaaf3de561e1fde2) is a Scheme-like CBPV
implementation hosted by Racket. In the inspected revision, the explicit stack is a mutable box containing either an
empty return boundary, an argument cons, or a method record with arguments and a residual stack. The `bind` translation
saves that stack, hides it while evaluating the bound computation, then restores it; the returned value and lexical
bindings use Racket's execution mechanism. A thunk becomes a Racket procedure. See
[`initialize.rkt`](https://github.com/zydeco-lang/fiddle/blob/9a8941c224635ccfa1955c4ecaaf3de561e1fde2/fiddle/initialize.rkt)
and
[`fiddle.rkt`](https://github.com/zydeco-lang/fiddle/blob/9a8941c224635ccfa1955c4ecaaf3de561e1fde2/fiddle/fiddle.rkt).
This is a useful hybrid: the host manages lexical environments and returns, while an explicit structure represents the
remaining argument/method protocol. Its global mutable state and foreign adapters would need their own reentry and
concurrency contracts. No Fiddle performance claim is made here.

The inspected
[Riddle implementation](https://github.com/UMjoeypeng/riddle_compiler/tree/54f9d9221af412b850172916b5ffacd63f403128)
provides a different reference point.
Despite the repository name,
its [`eval.rs`](https://github.com/UMjoeypeng/riddle_compiler/blob/54f9d9221af412b850172916b5ffacd63f403128/src/eval.rs)
implements a recursive evaluator: bindings substitute cloned syntax, `To` recursively evaluates a returner,
and `Push` evaluates its computation to a `Pop` terminal before substitution.
It has no independently planned activation layout in this evaluator.
It is useful for understanding semantic cases, but its syntax copying
and Rust recursion make it an inappropriate direct performance baseline for native frame allocation.
Neither external implementation was built or timed in this study; these are pinned source audits.

Zydeco's own three compiled paths make a stronger controlled comparison
because they share source checking and observable test programs.
The previous native implementation copies lexical captures at suspension; the current one retains their owning slots;
the two WebAssembly backends choose different granularities of dispatch and explicit stack storage.
Their differences already refute the idea that CBPV determines one environment representation.
They also show why comparing “stack versus heap” alone misses the question:
both native schemes use a machine control stack and managed heap objects,
and the retained word stack itself is allocated by Rust.

### Related runtimes with stronger control facilities

OCaml's effect-handler implementation is a concrete guide to detached control lifetimes.
The [OCaml 5 design account](https://github.com/ocaml-multicore/docs/blob/main/ocaml_5_design.md#stack-layout)
describes runtime-managed fiber stacks, allocated at entry and handler installation,
with cached freed stacks and explicit growth checks.
Foreign calls switch to the system stack; relocation must repair internal stack references such as the exception chain.
Its [one-shot continuation interface](https://ocaml.org/manual/5.2/effects.html) also
makes resumption ownership explicit.
The implication for Zydeco is conditional: a primitive operation that detaches control would need a segment owner
and a policy for destruction, relocation, and host crossings.
The current nested native tokens provide none of those capabilities by themselves.

[MLton's continuation interface](https://www.mlton.org/MLtonCont) documents a copying alternative:
`callcc` copies the stack and `throw` copies the saved stack on invocation.
Its `isolate` operation constructs an independent continuation in constant time,
while [the implementation note](https://www.mlton.org/MLtonContIsolateImplementation) explains why
a seemingly equivalent construction can accidentally retain a larger context.
This makes two evaluation criteria concrete: count copying over repeated resumptions,
and inspect everything kept reachable by a captured context.
Copying is not automatically wrong, and sharing is not automatically space efficient.

Koka offers a complementary compilation route:
generalized evidence passing and a monadic translation compile effect handlers to C. The
[Xie–Leijen paper](https://www.microsoft.com/en-us/research/wp-content/uploads/2021/08/genev-icfp21.pdf) is relevant
to a future C or portable backend because control can be represented explicitly
by the translation while ordinary execution uses the host.
It does not imply that arbitrary machine stack capture is a C ABI operation,
or that Zydeco should adopt Koka's representation without comparing its allocation and host-boundary costs.

### Representation and calling convention are further choices

Marlow and Peyton Jones compare push/enter and eval/apply for unknown-arity higher-order calls
in [Making a Fast Curry](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/eval-apply-icfp.pdf).
Their argument for eval/apply concerns Haskell's runtime arity handling; it does not select a CBPV backend.
A known CBPV protocol can describe multiple argument consumptions without an intermediate returned closure.
It can also expose effects between those consumptions.
That protocol must survive lowering even when a particular known call is implemented using registers
or a conventional call instruction.

Paul Downen's [Call-by-Unboxed-Value](https://pauldownen.com/publications/cbuv.pdf),
ICFP 2024, separates atomic from complex representations in addition to separating values from computations.
Its explicit boxing, closures, register-shaped arguments/results,
and stack operations make it a useful guide for a representation-aware IR.
Plain CBPV leaves questions such as a returned pair's physical storage open.
Consequently, a future multiword or unboxed Zydeco scheme needs representation and pointer-map evidence
at the compiler/runtime boundary; replacing the current `Word` alias alone would not establish the contract.

The [native frame proposal's literature](../proposals/native-frames.md#literature-pointers) adds closure space safety,
structured continuation use, join points, and C-- runtime coordination.
Together these sources suggest a progression: preserve protocol and lifetime information,
then select storage and calling conventions, then measure the costs induced by that selection.

## Reading the memory layouts

For a return entry that needs `x` and `y`, the alternatives can be illustrated independently of argument
and observation frames that may temporarily sit above them:

```text
compact capture:   control [code, tuple pointer, ...S]
                  heap    [header, x, y]
                  active  reusable slots, overwritten by the callee

flat capture:      control [code, x, y, ...S]
                  active  reusable slots, overwritten by the callee

retained frame:    control [code, token, ...S]
                  words   [caller: ...x...y...][callee: ...]
                  metadata token -> owner + suspended live-slot map

heap frame:        control or continuation object refers to a frame
                  heap    [layout/links, ...x...y...]
                  lifetime follows reachability or explicit ownership
```

The compact and flat schemes copy `x` and `y` into saved storage and subsequently into active slots.
The retained scheme changes the active base and preserves the original slots.
The heap-frame scheme can keep the same access strategy when lifetimes cease to nest,
but must prevent escaping references from retaining dead fields and must participate
in managed collection or another reclamation discipline.
These are alternative representations of required state, not alternative meanings of `Ret`.

A local join, a general thunk entry, and a return entry have different obligations.
A join may inherit its owner's initialized bindings.
A thunk entry obtains an independently usable captured environment.
A return entry restores suspended state.
Lowering all three to anonymous closed blocks too early loses information needed to choose among these layouts.
The current native metadata recovers the return-entry relationship; it is not yet a general scheme
for arbitrary context-bearing joins or detached stacks.

## How far a shared Rust model can go

The existing approach is feasible and useful: typed layouts, serialized actions, runtime transitions,
and source-fingerprinted packaging already form an executable contract.
This study's root-source refinement does not change the generated action record at all;
it changes when the runtime asks that record for roots.
The same generated assembly can therefore compare eager and deferred discovery.

Future designs should share the questions that remain stable while giving each representation its own answers.
A bounded nested-frame trace can compare enter, suspend, tail replacement, resume, and root discovery.
A flat-capture model would instead own field ordering, capture shape, restoration,
and root offsets in the control stack.
A segment model would own segment identity, links, saved cursors, relocation, and detachment.
The emitter should consume each model's typed descriptors and the stub should execute or decode those same descriptors.
That is a stronger boundary than two independent sets of numeric offsets.

There should not yet be one universal `Runtime` trait pretending these schemes have identical capabilities.
In particular, destructive nested resumption, detached one-shot resumption,
and reusable captured control are different semantic states.
A future experiment can introduce owned ticket types or capability-specific traits when both sides actually need them.
Conformance traces should then compare observable results and failure invariants only
over the capabilities shared by those models.

Rust still cannot prove that emitted AMD64 instructions obey a descriptor,
that every safepoint's live set is complete, or that the platform ABI preserves the intended registers.
Those require compiler validation and integration checks.
Likewise, a source fingerprint detects packaging mismatch, not an incorrect emitter.
This distinction gives the methodology a practical scope: share executable representation rules,
validate their compiler-side evidence, and test the generated boundary.

## Experiments decided before measurement

### Native historical comparison

Compare identical generated source programs using the heap-capture compiler at `a3df0fb8`
and retained-frame compiler at `062d5ce7`.
Each executable receives the runtime and embedded machine model belonging to its compiler.
Preserve the eager-root runtime as a separate build input
so a root-enumeration refinement can use identical generated assembly.
Do not retain the old native lowering as a production compatibility path.

Workloads must vary tail-chain length, suspension depth, and capture width separately.
Include computation protocols with argument and codata frames, an escaping closure held by a suspended caller,
and a depth/capture case where active allocation repeatedly runs beneath unchanged suspended roots.
Check each result against an exact exit/output oracle before timing it.

Record all successful and failed configurations, compiler revisions, build flags, runtime and source hashes,
OS, CPU, architecture, and whether AMD64 runs through translation.
Build outside timed intervals.
Warm each executable, rotate run order, retain individual samples, and report medians and ranges.
Use a per-process timeout and record failures rather than silently dropping them.
These measurements describe the tested compiler and hardware; they cannot rank CBPV against C as languages.

### Root enumeration experiment

At `062d5ce7`, `ManagedHeap::allocate` calls `Action::root_slots` before asking the collector to allocate.
`Frames::roots` rebuilds, sorts, and deduplicates the active and suspended slot addresses.
The collector only uses those roots when the active semispace lacks room.
The hypothesis is that eagerly enumerating a deep, stable suspended root set can dominate allocation time.

Test a deferred root-source boundary with paired cases: no collection, collection preserving moved pointers,
an oversized allocation rejected before collection, and a live set that still leaves too little space after collection.
Count root-source invocations independently of execution time.
Changing the root boundary must preserve the existing suspended/dead-slot and host-root regressions.

### Space and layout accounting

Keep logical live words, reserved environment capacity, control words, managed heap cells, and Rust metadata separate.
For `k` saved value words, a flat capture uses `k` control words plus its code entry;
a compact heap capture also needs a heap header and a pointer on the control stack.
Retained frames replace those capture words with a token but retain an extent whose size can exceed `k` substantially.
These are layout costs to check against concrete plans, not measured execution times.

For WebAssembly, compare emitted module size and post-execution linear memory under increasing loop counts.
The existing [backend proposal](../proposals/wasm-backends.md) owns the two implementations and ABI.
Trampolining avoids host-stack growth; it does not by itself reclaim explicit stack nodes or capture objects.

## Decision criteria

1. Preserve typed protocol behavior and observable outcomes, including nested returns and library control.
2. Preserve bounded tail space under a fixed set of retained callers.
3. Preserve precise liveness: one live slot must not retain a dead object in another slot.
4. Keep representation changes expressible through a shared Rust contract and reject mismatched artifacts.
5. Compare total costs, including metadata, root work, code size, and reserved space.
6. Promote a runtime change only after its paired correctness checks and a reproducible experiment support it.

## Findings and next decisions

### Configuration and evidence

The native runs used an Apple M5 Pro with 48 GB memory, macOS 26.6.2,
and AMD64 executables running through Rosetta on an ARM64 host.
The toolchain reported Rust 1.98.0 / LLVM 22.1.8, NASM 3.02, and Node 24.15.0.
The initial `sysctl` CPU query was denied; a filtered `system_profiler` query supplied the hardware description.
The committed data retains failed metadata queries as well as successful measurements.

Both compiler binaries were optimized. Native linking normally builds the runtime in Cargo's dev profile,
so the optimized-runtime experiment explicitly selected opt-level 3, no debug information,
no debug assertions, no overflow checks, and `lto=off`.
The CLI also selected `panic=abort` on macOS. All three variants used these same settings.
The dev-profile comparison is a separate run with one tenth as many loop iterations and three samples,
not a direct timing comparison with the optimized table.

Each final native configuration passed a warmup oracle before measurement;
variant order rotated within each workload and workload order alternated.
Seven process-wall-time samples follow the optimized warmup.
The first run includes substantial startup/translation overhead and is recorded separately.
Process startup remains part of every sample, so small differences should not be overinterpreted.
There were no benchmark builds running during the timed intervals.

The exact answer is reduced to exit status 0 or 1 by a full-width equality check.
A deliberately wrong expected answer differing by 256 exits with status 1 on both WebAssembly backends;
this guards against treating truncated POSIX exit codes as an arithmetic oracle.
The final configurations all passed their expected outcomes.

### Native execution

The initial five workloads isolate a fixed suspended prefix from allocation beneath it.
“Capture width” describes additional source-level scalar bindings deliberately used after return;
it is not the compiler's total root count, which may also include closure captures and administrative values.
The repeated-call extension separately creates and uses wide captured sets on every iteration.

| Workload | Optimized-run construction |
| --- | --- |
| Retained closure | One escaping closure held across 1,000,000 tail iterations |
| Zero captures | Depth 128, no extra scalar captures, 300,000 leaf iterations |
| Shallow captures | Depth 8, four extra scalar captures, 400,000 leaf iterations |
| Deep captures | Depth 128, sixteen extra scalar captures, 100,000 leaf iterations |
| Mixed protocol | 200,000 iterations consuming `.more value .more value .done` above a return continuation |
| Repeated 16 | 50,000 calls, sixteen fresh scalar bindings used after a three-step callee |
| Repeated 64 | 20,000 calls, sixty-four fresh scalar bindings used after a three-step callee |

Times are milliseconds, shown as median [minimum, maximum].
The first five rows come from the initial optimized study; the last two come from its repeated-call extension
on the same host and toolchain.

| Workload | Heap captures | Frames, eager roots | Frames, deferred roots |
| --- | ---: | ---: | ---: |
| Retained closure | 77.5 [76.3, 77.9] | 644.9 [638.3, 647.3] | 80.3 [79.6, 81.6] |
| Zero captures | 39.3 [38.7, 40.6] | 273.0 [269.6, 324.4] | 39.7 [39.4, 39.9] |
| Shallow captures | 49.2 [48.7, 50.3] | 1,161.4 [1,153.1, 1,181.2] | 50.7 [49.9, 52.6] |
| Deep captures | 18.5 [18.0, 19.4] | 5,764.0 [5,744.3, 5,799.0] | 19.7 [19.4, 20.0] |
| Mixed protocol | 43.8 [43.0, 44.7] | 267.1 [264.5, 269.7] | 45.2 [44.0, 46.5] |
| Repeated 16 | 97.9 [97.4, 99.1] | 736.7 [730.9, 747.7] | 90.9 [89.5, 91.5] |
| Repeated 64 | 179.9 [179.2, 181.8] | 1,087.8 [1,084.3, 1,097.5] | 115.0 [113.7, 115.5] |

Deferring root discovery improves the constructed deep-capture workload from 5.764 seconds
to 19.7 milliseconds, approximately 293 times faster.
That is evidence of an avoidable per-allocation root-enumeration cost in this implementation,
not a general speedup from stack allocation.
With that cost removed, retained frames remain about 1–7% slower than heap captures in the first five cases.
In the repeated-call extension, they use about 7% and 36% less elapsed time for widths 16 and 64 respectively.
This supports the intended benefit when capture copying is repeated,
while leaving a smaller transition/metadata cost visible elsewhere.
The experiment does not isolate each remaining cost or establish statistical significance
across machines or applications.

The shipped dev-profile comparison uses the first five workloads at one tenth of the iteration counts above:

| Workload | Heap captures, median ms | Eager frames, median ms | Deferred frames, median ms |
| --- | ---: | ---: | ---: |
| Retained closure | 46.7 | 1,032.4 | 72.5 |
| Zero captures | 25.4 | 436.8 | 39.5 |
| Shallow captures | 31.0 | 1,753.1 | 49.3 |
| Deep captures | 14.6 | 23,813.6 | 22.1 |
| Mixed protocol | 28.3 | 429.1 | 47.1 |

The linked data contains all three samples and their ranges.
Deferred frames remain approximately 52–66% slower than heap captures in this configuration,
so runtime profile selection materially affects the engineering conclusion.
Optimizing the compiler executable alone does not address that overhead.

The first five optimized executables with deferred roots are approximately 20–23 KiB larger
than their heap-capture counterparts, including linked runtime code and data.
Eager and deferred frame variants have byte-identical generated assembly for each workload;
their runtime implementations account for the difference.
Executable size is neither live memory nor a measurement of instruction-cache misses.

### WebAssembly memory

The same arithmetic tail loop was instantiated in a fresh Node process for each count.
These figures measure exported linear-memory capacity after execution,
including static regions and rounding to 64 KiB pages.
They do not measure GC live bytes, committed resident pages, or the JavaScript engine's total memory.
The optional host probe reported zero retained host values for these arithmetic programs.

| Iterations | `wasm-am` initial → final bytes | `wasm-sps` initial → final bytes |
| ---: | ---: | ---: |
| 100 | 1,179,648 → 1,179,648 | 131,072 → 131,072 |
| 10,000 | 1,179,648 → 2,359,296 | 131,072 → 4,194,304 |
| 100,000 | 1,179,648 → 13,893,632 | 131,072 → 41,615,360 |

At 100,000 iterations, the SPS module is smaller: 9,502 bytes versus 12,203 bytes for AM.
It nevertheless consumes substantially more linear memory in this workload.
Between 10,000 and 100,000 iterations the growth is approximately 128 bytes per iteration
for AM and 416 for SPS, subject to page rounding.
The source audit explains the direction: both allocate unreclaimed products/closures,
while SPS also allocates linked protocol-stack nodes.
The experiment does not attribute every byte to an individual instruction kind.

Reclamation is therefore a more pressing long-running-workload question than the module-size advantage alone suggests.
A tracing collector must account for products' interior suffix pointers and opaque host handles.
A destructive stack or region alternative must establish that the relevant residual stacks cannot outlive
or share reclaimed nodes; the name “SPS” is not a proof of that property.

### Storage accounting with the production Rust model

[`frame_layouts.rs`](../../lang/machine/examples/frame_layouts.rs) drives the actual `Frames` implementation
through nested suspension, 10,000 tail replacements beneath a fixed prefix, and reverse-order resumption.
It verifies every saved value. It also checks a frame-capacity boundary with a fitting case
and an overflowing counterpart whose failure preserves the retained caller and its token.

| Scenario | Frame words | Suspensions | Live slots per suspension | Retained high-water words | Suspended root slots |
| --- | ---: | ---: | ---: | ---: | ---: |
| Empty maps | 256 | 32 | 0 | 8,448 | 0 |
| Compact | 8 | 32 | 4 | 264 | 128 |
| Sparse | 256 | 32 | 4 | 8,448 | 128 |
| Capacity fits | 1,024 | 127 | 4 | 131,072 | 508 |
| Capacity exceeded | 1,024 | 128 | 4 | Rejected before overwrite | — |

For the compact and sparse 32-suspension scenarios, a flat-capture layout would hold 160 continuation control words:
32 code entries and 128 saved values.
Compact heap captures would use 64 control words and 192 heap words, including two-word cell headers.
Both copying layouts would move 256 value words across suspension and resumption.
The retained scheme uses 64 continuation control words and performs no such capture copying,
but preserves the frame extents shown above.

Only the retained trace executes a production transition model here.
The other counts are explicit layout arithmetic, not implemented flat-backend timings.
They exclude argument/observation words, closure allocations, allocator metadata,
and Rust activation/suspension vector storage.
Their reusable active environment is also excluded, so the counts must not be added
into a supposed whole-process memory ranking.

The first-round retained model allocates and zero-initializes the entire fixed 1 MiB word buffer on first entry.
Its high-water counter measures consumption of that capacity, not how many bytes the allocator reserves.
Precise liveness prevents dead slots from retaining managed objects,
but does not reduce this reservation or the frame extent that counts against the limit.
The sparse and empty-map cases make frame packing and empty suspension treatment worthwhile next experiments even
when managed-heap retention is already precise.

Bounded native frame and managed-heap space is also narrower than whole-program space safety.
The current stub's `HostString::leak` and `HostBytes::leak` transfer Rust allocations through raw pointers;
Cheney collection does not reclaim those host allocations.
The arithmetic workloads avoid that lifetime question.
A broader memory evaluation must include host values and resource ownership separately
from frame roots and managed semispaces.

### Changes justified by the study

The native runtime now defers enumeration of frame and registered host roots until collection needs them.
The shared action layout and the model's live-slot rule are unchanged.
Paired collector checks establish zero enumeration on the allocation fast path
or oversized rejection, exactly one on collection, preservation of relocated pointers,
and preservation of the live graph when collection still leaves insufficient room.
Existing suspended/dead-slot and host-root cases continue to pass.

Optimized native builds also exposed an independent linkage failure:
runtime exports referenced only by assembly disappeared with local ThinLTO.
The supplied runtime manifest now uses `lto=off` in dev and release profiles, and the integration test links
and executes the same packaged program in release mode before checking rejection of an incompatible model symbol.
Merely making the exports public did not repair the observed failure; disabling local ThinLTO did.
This is a toolchain-boundary finding, not an advantage of any frame layout.
The benchmark applies the same setting to every historical runtime through Cargo's environment.

The test host gains an opt-in `ZYDECO_WASM_MEMORY_REPORT=1` probe.
Ordinary source arguments and default output remain unchanged.
The native and WebAssembly proposals remain authoritative for implementation rules;
this document owns the exploratory comparisons and their evidence limits.

### First-round follow-ups

The first round justified retaining native frames with deferred root enumeration,
while treating performance as an open comparison.
Its suggested native experiments were frame slot packing, treatment of empty suspension maps,
and a flat-capture model on the same source workloads.
Flat captures can isolate capture allocation from copying; they need a shared entry-shape
and root-layout contract before code emission.
Slot packing must preserve every pending continuation's live bindings,
including overlapping suspensions of one activation.
Changing only frame size estimates would be insufficient.
The second round below implements packing and compares two storage policies; flat captures remain unimplemented.

For WebAssembly, keep the backend choice explicit and prioritize a bounded or reclaiming allocation scheme.
Rust-generated module layout and the JavaScript embedding are currently a different sharing boundary
from the native Rust stub.
A future Rust runtime module, generated host schema, or binding generator can make that contract shared;
simply using the same conceptual word tag in handwritten Rust and JavaScript does not do so.

Explore segments or copied continuations when a proposed language or IR operation requires detached control,
not merely because CBPV exposes the control stack conceptually.
Specify single-use, repeated-use, cancellation, root relocation,
and foreign-call behavior before selecting a segment layout.
Library-encoded continuations can remain ordinary heap closures under the current native discipline.

The unmeasured areas are native execution on physical AMD64 hardware, a broader application corpus,
RSS and cache behavior, allocation/collection counters in generated programs, a working flattened backend,
heap-frame alternatives, and primitive detached-stack operations.
No results for those areas are implied by this study.

## Reproducing and inspecting the experiments

The runner is [`lang/tests/runtime-study.py`](../../lang/tests/runtime-study.py);
its only Python dependency is the standard library.
It writes generated sources, build outputs, exact commands, source/runtime/compiler hashes, warmups,
individual samples, failures, and summaries into the selected output directory.
Recorded repository paths use `${REPO}` as a portable placeholder.
A nonzero script exit indicates failed builds or result checks; failures are retained instead
of silently excluding them from the report.

The recorded datasets are [optimized roots](runtime-study-2026-09-08/optimized.json),
[repeated captures](runtime-study-2026-09-08/wide.json), [dev profile](runtime-study-2026-09-08/debug.json),
and [layout accounting](runtime-study-2026-09-08/layouts.csv).
The [setup-failure record](runtime-study-2026-09-08/setup-failures.json) separates harness/compiler setup problems
from runtime outcomes.
The earlier pilots used smaller inputs; their timings are not pooled with final samples.
The runner gained explicit workload selection and filtered hardware collection for the repeated-call extension;
the dataset records the runner hash used for each run, and the generated workload hashes remain reproducible.

Start from a checkout of this study's commit so the deferred runtime matches the recorded implementation.
Use separate archived source trees and Cargo target directories for the two compiler revisions.
Sharing one target directory between historical workspaces produced a stale-model artifact in the initial setup.
For example, from the repository root:

```sh
mkdir -p build/runtime-study/baseline build/runtime-study/retained build/runtime-study/bin
git archive a3df0fb8 | tar -x -C build/runtime-study/baseline
git archive 062d5ce7 | tar -x -C build/runtime-study/retained
cargo build --manifest-path build/runtime-study/baseline/Cargo.toml --release --bin zydeco -j 2
cargo build --manifest-path build/runtime-study/retained/Cargo.toml --release --bin zydeco -j 2
cp build/runtime-study/baseline/target/release/zydeco build/runtime-study/bin/zydeco-captures
cp build/runtime-study/retained/target/release/zydeco build/runtime-study/bin/zydeco-frames
python3 lang/tests/runtime-study.py \
  --capture-compiler build/runtime-study/bin/zydeco-captures \
  --capture-runtime build/runtime-study/baseline/runtime \
  --frame-compiler build/runtime-study/bin/zydeco-frames \
  --eager-runtime build/runtime-study/retained/runtime \
  --deferred-runtime runtime \
  --output build/runtime-study/reproduction --samples 7 --scale 10
cargo run --quiet -p zydeco-machine --example frame_layouts
```

Unset an inherited `CARGO_TARGET_DIR` before those compiler builds.
The runner clears it for native linking. The combined command runs all seven native workloads.
Use `--workloads retained-closure zero-captures shallow-captures deep-captures mixed-protocol`
to reproduce the initial subset, or `--workloads repeated-16 repeated-64` for the extension.
For the dev-profile comparison, select the initial subset and use `--profiles debug --samples 3 --scale 1`.
`--wasm-only` repeats just the memory and oracle checks.
Each executable has a 30-second timeout and each build a 300-second timeout by default.

The focused checks for the implemented changes were `native_gc` (14 tests),
`native_model` (3, including optimized execution and incompatible-model rejection),
and the AMD64 builtin cases (11, including process arguments).
The model accounting example also passed.
`cargo fmt --all`, standalone runtime formatting, `cargo clippy-all -- -D warnings`,
and standalone-runtime Clippy for `x86_64-apple-darwin` passed.
The CPU-intensive full workspace test suite was not run.


## Second round: static layouts and heap environments

### Goal and distinctions

This round asks how far static knowledge and different environment allocators can be combined,
while preserving CBPV's explicit argument, observation, and return protocols.
The requested directions were heap-resident environments and statically allocated local frames.
The investigation separates three interpretations:

1. The previous environment was already a `Vec` allocated on Rust's heap, with a fixed 1 MiB extent.
   Moving the allocation there again would change nothing.
2. Its owner layouts were already statically sized.
   The missing optimization was reusing storage across definitions whose lifetimes do not overlap,
   including lifetimes extended by pending continuations.
3. Putting environments inside the **managed value heap** changes when their addresses can move,
   which values collection must retain, and which compiler operations can collect.

The implemented native combination is static slot packing plus a geometrically growing contiguous environment.
A separate executable prototype puts environment cells through the actual Cheney collector.
The [native frame proposal](../proposals/native-frames.md#boundary-with-compiler-and-runtime)
owns the production packing and entry contracts,
and its [managed-environment section](../proposals/native-frames.md#experimental-managed-environments)
owns the experimental root boundary.
This section evaluates their consequences.

Static frame size is compatible with dynamic storage allocation.
The compiler fixes offsets and the required extent; the runtime reserves that extent when an activation begins.
Knowing the local size does not determine recursive depth, the number of suspended activations,
or whether an environment can escape.
An arrow consumption or observation branch need not establish a new activation.
These distinctions follow the CBPV transfer protocol and checked ownership, rather than treating every source binder
or code label as a C function entry.

### Static packing and measured reservations

The new allocator combines ordinary backward liveness with a forward may-analysis of pending captures.
That second analysis makes an otherwise tempting reuse unsafe: an inner continuation can stop reading `x`
while an outer continuation still needs its original value.
The regression constructs exactly that case, as well as a dead write that would clobber a live slot
and a consumed capture whose storage can safely be reused.
Aliases keep their source slot; static maps and generated loads/stores all use the resulting assignment.

The added `sequential-locals` workload performs 128 successive additions inside one activation,
then repeats that computation 10,000 times through returning calls.
It isolates the difference between the total number of local definitions
and the maximum storage needed over their lifetimes.
The other seven workloads and scale-10 parameters are those of the first round.
All use the full-width arithmetic oracle described above.

| Workload | Largest frame, before → packed | High-water words, before → packed | Growable reservation, bytes |
| --- | ---: | ---: | ---: |
| Retained closure | 8 → 3 | 13 → 6 | 48 |
| Zero extra captures | 7 → 4 | 10 → 5 | 64 |
| Shallow captures | 14 → 8 | 123 → 72 | 1,024 |
| Deep captures | 38 → 20 | 4887 → 2580 | 40,960 |
| Mixed protocol | 10 → 4 | 13 → 5 | 64 |
| Repeated 16 | 40 → 20 | 50 → 25 | 336 |
| Repeated 64 | 136 → 68 | 146 → 73 | 1,104 |
| Sequential locals | 135 → 4 | 138 → 5 | 64 |

The previous fixed store reserves 1,048,576 environment bytes in every row.
The table measures `Vec` word capacity, excluding allocator rounding, Rust frame/token metadata,
the machine control stack, and the managed heap.
It is not RSS or whole-program memory. Packing cuts logical high-water usage roughly in half in several rows;
geometric capacity rounding means that reservation does not fall by exactly the same ratio.
The zero-extra-captures case has bounded high-water usage: its source return path allows continuation elimination,
so its source recursion depth is not its retained frame depth.

A separate source case retains 16 scalar captures through 7,000 recursive levels,
then checks the complete result on return.
Both the previous fixed store and the packed fixed store report environment capacity exhaustion;
the packed growable store completes with exit status zero.
This is an end-to-end capacity result in addition to the model trace that successfully addresses 200,003 words
and preserves nested tokens across growth.

The growable store caches capacity after a deep call.
A tail chain remains bounded by the largest historical reservation,
but a large completed phase does not automatically return those bytes to the host allocator.
Shrinking or segmented reclamation remains a separate policy decision.

### Controlled native timing

An initial 120-sample comparison showed a small slowdown despite the space reduction.
A 280-sample factorial comparison then used the same revised Rust model with both distinct
and packed slot assignment, each combined with fixed and growable storage.
It also retained the old fixed implementation as an anchor.
Packing and growth were close to neutral within that comparison; the shared-storage refactor accounted
for most of the observed 2–10% regression relative to the old implementation.
Disassembly showed out-of-line `Storage::reserve` and `Storage::base` calls at the new boundary.
Adding inline hints to those concrete methods removed most of this overhead without changing their semantics.

The final comparison below uses the inlinable implementations.
Numbers are median milliseconds from seven samples; all builds precede timing,
variant order rotates, and separate probe executables measure reservations afterwards.
The hardware, Rosetta translation, optimized runtime settings,
and fresh-process timing limitations remain those recorded for the first round.
In particular, this is not a measurement on a physical AMD64 processor.

| Workload | Previous fixed | Packed fixed | Packed growable |
| --- | ---: | ---: | ---: |
| Retained closure | 79.35 | 82.31 | 82.38 |
| Zero extra captures | 39.00 | 39.18 | 39.84 |
| Shallow captures | 50.01 | 52.75 | 50.48 |
| Deep captures | 18.86 | 18.89 | 19.27 |
| Mixed protocol | 45.24 | 45.35 | 45.04 |
| Repeated 16 | 87.41 | 87.74 | 87.98 |
| Repeated 64 | 109.95 | 110.03 | 110.50 |
| Sequential locals | 52.75 | 52.67 | 52.26 |

Packed growable differs from the previous fixed implementation by approximately −1% to +4% in this run.
This supports a space and capacity improvement with modest throughput differences, not a general speed claim.
The packed fixed and growable variants have byte-identical generated assembly within each comparison;
storage policy changes their Rust runtime.
The earlier factorial data remains available so the initial regression and the reason
for adding inline hints are reviewable.
Compiler preparation time and worst-case growth latency have not been benchmarked systematically.

### Managed heap environments: what actually ran

[`environment_layouts.rs`](../../lang/tests/examples/environment_layouts.rs) exercises three concrete mechanisms:
the fixed store, the growable store, and opaque environment cells allocated by the production Cheney heap.
The managed trace registers frame handles and uses the shared `MovingRoots` adapter at collection.
Captured fields point to managed values; several fields share a value so relocation must preserve aliasing.
The trace checks every retained value after collection, then drops younger handles and continues allocating.
This is a collector-integrated model trace, not a second AMD64 backend or a timing comparison with native code.

The sparse and packed cases have the same 32 suspended environments with four live fields each,
plus one active environment.
The sparse layout reserves 256 words per frame; the compact synthetic layout uses four.
This pair isolates layout density and does not claim that every real 256-word owner can be packed to four words.
The managed cases execute 100,000 one-word heap allocations beneath the retained prefix, reusing the active frame.
The linear cases execute 100,000 tail entries and check constant storage; they do not instantiate a value heap.

| Trace | Environment payload words | Growable reservation words | Managed collections | Managed frame payload words copied | Live value words lifted |
| --- | ---: | ---: | ---: | ---: | ---: |
| Sparse | 8,448 | 16,384 | 2 | 16,896 | 256 |
| Packed | 132 | 256 | 2 | 264 | 256 |
| Empty captures, large frames | 8,448 | 16,384 | 2 | 16,896 | 0 |
| Packed, depth 128 | 516 | 1,024 | 2 | 1,032 | 1,024 |

These counts expose two different space questions. Precise roots prevent dead fields from retaining values,
but the copying collector still moves each live frame's entire opaque payload.
Static compaction reduces that work 64-fold in the sparse/packed pair,
while the number of published live values is unchanged.
Lifted values are also written back once after collection; that restoration count equals the lifting count.
Frame copy counts omit cell headers and the separately copied captured objects.
Empty capture maps avoid value tracing but still retain frame bytes.

With 1,024-word frames, 127 suspended environments plus an active frame exactly fill the old fixed environment region.
The managed variant fails when entering the active frame: 1,045,464 bytes already remain live
in its 1 MiB semispace, including environment cells, headers, and captured values.
At depth 128 the fixed store rejects the next frame, while the growable store succeeds
with 132,096 used words and a 262,144-word reservation.
Managed frame allocation competes with value allocation;
removing a separate environment buffer is not automatically an improvement under the same semispace budget.
The managed trace reserves two semispaces and their indices; blank reservation fields in its CSV mean
that it has no separate environment allocation, not zero total memory.

Allocating the whole environment stack as one managed slab is another possible composition.
It would reduce per-frame cell headers, but this collector copies complete cells:
retaining one old frame would still keep the slab's payload extent, even after the logical stack frontier retreats.
That is an inference from the collector's copying granularity, not an additional native measurement.
Individual managed cells, which this trace implements, can become unreachable independently after return.

The managed-root regression also forces collection during an allocation that subsequently fails.
It checks repaired frame handles, repaired live fields, sharing, and the exclusion of a large dead referent.
An invalid slot-map counterpart is rejected before invoking the collector.
Together these checks make the root contract executable; they do not establish arbitrary escaping frame references
or detached continuation semantics.
A generational collector, frame-specific tracing descriptors,
and immutable compact continuation environments could change the tradeoffs substantially.

### Further directions and useful combinations

The two implemented changes separate *how much storage an activation needs* from *where that storage lives*.
Several further directions follow from that separation:

| Direction | What it could improve | Contract or experiment needed next |
| --- | --- | --- |
| Active scratch area plus compact suspended fragments | Avoid retaining a large owner for a few saved values | A shared capture/entry map and a policy comparing copy work with retained extent; flattening on the control stack remains a useful baseline |
| Promotion only when an environment escapes | Keep ordinary nested activations cheap while allowing longer lifetimes | An escape/lifetime proof and a transition that redirects every permitted reference; ordinary `Ret` tokens alone do not prove escape behavior |
| Small growable regions that switch to segments | Bound large relocation work and release completed deep regions | Frame-to-segment addressing, spare-segment reuse, and peak/valley workloads that detect allocation thrashing |
| Individually allocated nonmoving frames | Stable bases with independent reclamation | Per-frame addressing and precise value roots; account for allocator metadata and allocation frequency |
| Reserved virtual address space with incremental commitment | Stable contiguous addressing without copying the high-water buffer | An OS-specific reservation/guard-page contract and separate virtual, committed, and resident measurements |
| Region allocation from inferred lifetimes | Reclaim groups of environments and related values together | An IR representation of ownership and region capabilities across CBPV transfers, including host borrows and escaping thunks |
| Representation-aware slots and registers | Avoid uniformly spilling and tagging every local word | Width/alignment and pointer maps shared with the collector, plus spill/root contracts at host calls and safepoints |

A particularly promising next combination is a small reusable active region plus compact immutable suspended fragments,
allocated in a nursery or retained segment.
The choice should depend on capture density and lifetime, not simply on whether the allocator is called a heap.
The sparse trace motivates this experiment, while the native packing result establishes a stronger baseline
than one slot per definition.

Sharing a mutable, packed heap frame with an escaping closure is a different proposal from moving its bytes.
The present slot allocator may overwrite a consumed continuation's old slot.
A closure that still refers to that slot would observe the wrong binding, even if the collector keeps the frame alive.
Such sharing requires extending the preservation analysis to those references,
copying the relevant fragment, or making shared storage immutable.
This is where CBPV's separation between thunk values, returned values,
and residual computation protocols helps state the right ownership boundary.
Allocating everything through `malloc` or a collector does not establish it.

The growing-buffer policy can also be combined with a shrink threshold after sufficiently large completed phases.
That experiment should alternate deep and shallow calls and measure allocation churn as well as resident memory.
A resize-on-every-return implementation would lose the reuse benefit established by the tail traces.
No segmented, virtual-memory, region-inference, or new register backend is claimed by this round.

### What this establishes about the Rust contract

There are now two concrete implementations of a narrow common capability:
contiguous word storage that may relocate at entry.
`Frames<Storage>` shares ownership, tokens, actions, and root-map interpretation between them.
The emitter continues to serialize the same `Action` type that the stub consumes.
Inlineable concrete methods allow that modular boundary without the initial out-of-line-call cost.
The production stub selects `Growable`; `Fixed` remains a controlled alternative with the same continuation convention.

A moving managed environment does not implement this storage capability.
It can relocate during ordinary value allocation, and the native entry operation currently cannot collect.
Its separate `MovingRoots` contract expresses the relocation obligation without pretending
that selecting an allocator updates code generation automatically.
Likewise, a segmented store cannot supply the current trait's single contiguous base;
it needs a different address-resolution boundary.
Shared Rust types make each chosen contract explicit, while integration tests still check register reloads,
safepoints, serialized descriptors, and emitted accesses.

This is a feasible modular methodology: share representations and transitions where concrete schemes agree,
and give a new scheme a new typed boundary when it adds a capability.
The source fingerprint rejects mismatched model artifacts;
it does not replace the compiler's initialization/preservation proofs or prove handwritten assembly.

### Literature added in this round

[Farvardin and Reppy, PLDI 2020](https://kavon.farvard.in/papers/pldi20-stacks.pdf) compare six strategies
within one compiler and runtime, including resizing, segmented, mutable linked frames, and immutable CPS continuations.
Their distinction between mutable frame reuse and immutable continuation closures is especially useful here.
Their stack models also include control and call/return behavior that differs from Zydeco's separate environment stack,
so their performance rankings should guide experiments rather than select a Zydeco winner.

[Appel and Shao](https://www.cs.princeton.edu/~appel/papers/stack2.pdf), especially sections 2–5,
separate creation, access, copying/sharing, and space safety.
Their generational-heap assumptions matter: the present two-space trace is not a reproduction
of their collector or closure optimizer.
The older paper and the newer controlled comparison should be read together.

[Tofte and Talpin, *Region-Based Memory Management*, 1997](https://researchprofiles.ku.dk/en/publications/region-based-memory-management/)
provides a region-inference direction. The
[Capability Calculus](https://www.cs.cornell.edu/talc/papers/capabilities-abstract-tr.html) makes safe reclamation and
non-lexical region lifetimes explicit in a typed compiler language. The application to CBPV environments above is an
inference and future design question, not an existing Zydeco region system.

### Evidence and reproduction

The second-round data lives beside the first-round records:

- [Pilot](runtime-study-2026-09-08/environment-pilot.json): 120 timed samples and 24 warmups.
- [Factorial comparison before inline hints](runtime-study-2026-09-08/environment-factorial.json):
  280 timed samples, 40 warmups, and 40 separate environment probes.
- [Final comparison](runtime-study-2026-09-08/environment-inlined.json): 168 timed samples, 24 warmups,
  and 24 separate environment probes.
- [Capacity case](runtime-study-2026-09-08/environment-capacity.json): two expected fixed-store rejections
  and one successful growable execution.
- [Collector/storage traces](runtime-study-2026-09-08/environment-layouts.csv)
  and [data/source hashes](runtime-study-2026-09-08/environment-manifest.json).

All 568 timed samples, 88 warmups, 64 probes, and three capacity expectations passed.
The runners record source, compiler, runtime, and model hashes, exact commands, build failures, and output oracles.
The pilot runner predates probe support; the final timing runner predates only the capacity-mode addition.
An unused runtime import was removed after the factorial run; it changes no generated behavior.
These differences are recorded rather than silently assigning final-file hashes to earlier executions.
The fixed-runtime copies change only the concrete `Frames` storage selection.
The original anchor uses the first-round frame compiler and the `88329154` runtime.
The unpacked factorial control combines the `88329154` compiler sources with the revised machine sources
before inline hints; packed controls use the working compiler at that same model revision.

To reproduce the final comparison from this revision, build the current compiler and an isolated baseline:

```sh
mkdir -p build/environment-study/baseline
git archive 88329154 | tar -x -C build/environment-study/baseline
CARGO_TARGET_DIR=build/environment-study/current-target cargo build --release --bin zydeco
CARGO_TARGET_DIR="$PWD/build/environment-study/baseline-target" cargo build --release \
  --manifest-path build/environment-study/baseline/Cargo.toml --bin zydeco
```

Copy `runtime` to `build/environment-study/fixed-runtime` without its `target` directory.
In that copy, replace the `storage::Growable` import with `storage::Fixed`, and replace `Frames<Growable>`
with `Frames<Fixed<{ zydeco_machine::native::ENVIRONMENT_BYTES / zydeco_machine::native::WORD_BYTES }>>`.
The compiler supplies the matching machine crate to every build; do not copy a historical model over it.
Then run:

```sh
python3 lang/tests/environment-study.py \
  --variant original-fixed build/environment-study/baseline-target/release/zydeco build/environment-study/baseline/runtime \
  --variant packed-fixed build/environment-study/current-target/release/zydeco build/environment-study/fixed-runtime \
  --variant packed-growable build/environment-study/current-target/release/zydeco runtime \
  --output build/environment-study/results --samples 7 --scale 10
cargo run --release -p zydeco-tests --example environment_layouts
```

For the capacity comparison, use the same three `--variant` arguments with a separate output directory
and add `--capacity-only --growable-variant packed-growable`.
The historical factorial controls can be reconstructed by omitting the four inline annotations in a copy
of the new storage module and combining that module with the baseline or packed planner.
Keep each compiler build's target directory separate.

Focused verification covers the machine and assembly tests, `native_gc`, `native_model`, native builtin/core/control
and FFI cases, both executable model traces, workspace Clippy, and standalone-runtime Clippy for AMD64.
The full workspace test suite was not run. The implementation promotes packing and growable storage;
managed-frame root publication remains an explicitly bounded prototype.


## Third round: reusable active storage and compact suspensions

### Hypothesis and controlled boundary

Static packing improved active layouts, but a frame's peak local demand can still exceed what survives a call.
This round implements a compact suspension buffer alongside one reusable active environment.
The [compact environment contract](../proposals/native-frames.md#experimental-compact-environments) owns its transitions
and roots.
The compiler's preservation analysis, slot assignments, control-stack convention, managed collector,
and generated action descriptors stay fixed between retained and compact selections.
The two engines share the sealed Rust `Environment` capability;
selecting compact storage changes only the stub's concrete model type through `compact-environments`.

The first candidate snapshots captures eagerly at Suspend.
Unlike the earlier heap-capture backend, it allocates no managed tuple per continuation
and needs no capture-unpacking instructions in generated entries.
One geometrically growing buffer stores nested fragments, while another supplies active slots.
This isolates capture copying and compact storage from collector-integrated frame allocation.
It remains a nested continuation implementation, not a new first-class control capability.

The literature suggests measuring creation, copying, sharing, and retention separately:
[Appel and Shao](https://www.cs.princeton.edu/~appel/papers/stack2.pdf) provide those cost categories,
while [Farvardin and Reppy](https://kavon.farvard.in/papers/pldi20-stacks.pdf) motivate comparing concrete strategies
within one compiler.
The application to Zydeco's separate environment and control stacks is our inference.
Their implementations and collectors do not predict the measurements below.

### Eager snapshot results

The pilot ran the eight existing workloads with three samples per variant.
Disassembly showed out-of-line calls from `Action::apply` to the compact transition methods.
Inline hints removed those transition calls; the follow-up uses seven samples and adds two sparse wide-frame workloads.
Inlining alone did not remove the compact engine's execution penalty.
The previous commit, `77b9c14e`, remains an external anchor, while the retained engine in the new model controls
for the introduction of the `Environment` interface.

Optimized median wall times in milliseconds, on the same M5 Pro/Rosetta host as earlier rounds:

| Workload | Previous commit | Retained through shared interface | Eager compact |
| --- | ---: | ---: | ---: |
| Retained closure | 78.046 | 78.270 | 86.097 |
| Zero source captures | 37.478 | 37.378 | 41.708 |
| Shallow captures | 48.490 | 47.846 | 54.292 |
| Deep captures | 17.628 | 17.879 | 19.178 |
| Mixed protocol | 42.871 | 42.731 | 50.777 |
| Repeated 16 | 83.278 | 83.298 | 99.563 |
| Repeated 64 | 105.886 | 105.531 | 147.348 |
| Sequential locals | 49.933 | 50.045 | 56.439 |
| Sparse repeated 64 | 106.506 | 106.053 | 147.472 |
| Sparse deep 64 | 17.588 | 17.659 | 19.564 |

The shared interface adds no clear measurable cost in this bounded comparison.
Eager compaction saves substantial suspended storage when local demand is wide and captures are sparse:
`sparse-deep-64` peaks at 8,581 retained words versus 260 compact words,
with word-buffer reservations of 8,704 versus 451 words.
The reduction keeps the same source result and collector root counts.
For `repeated-64`, compact peak storage instead rises from 73 to 135 words and reservation rises from 138 to 164:
copying a nearly complete environment temporarily keeps both its active words and snapshot.
Metadata, allocator rounding, control-stack words, and managed heap reservation are excluded from these figures.
Cached capacities remain allocated after returning to shallow execution.

Separate probe binaries count the actual action stream and collection root visits.
`repeated-64` executes 2,740,003 suspensions but only 200,004 fresh entries,
saving and restoring 89,980,000 capture words in each direction with eager snapshots.
The sparse repeated variant still has 88,780,000 words in each direction: reducing locals
before the recursive call does not remove the many earlier primitive-return continuations.
Even the workload named zero captures performs 600,131 lowered suspensions with 1,200,256 captured words;
its name describes the source-level nesting parameter, not every lowered host return.
None of these ten workloads had multiple pending suspensions of the same dynamic activation.

This explains why capture density at one selected call is insufficient as a policy.
A host primitive can resume within the same activation without any fresh Enter in between.
The eager engine pays copying on those suspensions even though the active region was never reused.
The next candidate should defer materializing snapshots until Enter actually overwrites active storage.
That distinction follows the CBPV machine operations: consuming a return continuation
and establishing another closure activation are separate events.

### Sharing and collection traces

The executable `environment_fragments` example runs both engines with the production Cheney collector,
100,000 tail entries, and exact value-survival checks after reverse resumption.
It varies layout size, capture offsets, nesting depth, and overlapping suspensions of one activation.
Each trace forces 36 or 37 collections.
Word reservations are observed; the copy counts below are derived exactly from the executed sequence in
which every suspension is made and consumed once.

| Trace | Retained peak / reserved words | Compact peak / reserved words | Retained / compact suspended root locations | Compact copied words |
| --- | ---: | ---: | ---: | ---: |
| 32 sparse 256-word owners, four captures each | 8,448 / 16,384 | 384 / 384 | 128 / 128 | 256 |
| 32 dense four-word owners | 132 / 256 | 132 / 132 | 128 / 128 | 256 |
| 32 empty-capture 256-word owners | 8,448 / 16,384 | 256 / 256 | 0 / 0 | 0 |
| 32 suspensions sharing one eight-word owner | 16 / 16 | 264 / 264 | 8 / 256 | 512 |
| 32 suspensions sharing four slots in one 256-word owner | 512 / 512 | 384 / 384 | 4 / 128 | 256 |

Sparse captures at low or scattered high offsets give the same compact size.
The shared-owner cases show a limit absent from the native workload set: snapshots duplicate both storage
and physical root locations, while retained frames share the union of slots.
For the eight-word shared owner, root visits across 36 collections rise from 288 to 9,216.
All aliases still point to correctly relocated objects after resumption.
The regression tests also cover a collecting allocation failure: every live copy is repaired before returning the error.

### Evidence and reproduction

- [Pilot](runtime-study-2026-09-08/fragments-pilot.json): 72 samples, 24 warmups, and 24 separate probes.
- [Eager comparison after inline hints](runtime-study-2026-09-08/fragments-eager.json): 210 samples,
  30 warmups, and 30 separate probes including transition counts.
- [Executable collector traces](runtime-study-2026-09-08/fragments-eager-layouts.csv)
  and [data/source hashes](runtime-study-2026-09-08/fragments-eager-manifest.json).

All samples, warmups, and probes passed their output oracles.
Builds precede every timed run; variant order rotates and workload order alternates between samples.
Probe binaries are built and run separately after timing.
Their counters do not affect the timed executables.
The JSON records source/runtime/model hashes, exact commands, output, and failures.
The pilot predates the sparse workload and transition-probe additions; its recorded runner hash is retained.

Use `lang/tests/environment-study.py --fragments --samples 7` with explicit `--variant NAME COMPILER RUNTIME` arguments
for each compiler/runtime pair.
The compact runtime copy changes only its Cargo feature defaults to `["compact-environments"]`.
Build historical compilers in isolated source and target directories to preserve their bundled model identities.
Run `cargo run --quiet -p zydeco-tests --example environment_fragments` for the collector/storage traces.
Focused validation includes model and assembly tests, `native_gc`, `native_model`,
workspace Clippy, and standalone AMD64 runtime Clippy.
The compact integration case compares host-return, GC-stress, escaping-closure,
callback, and control-library programs against interpreter outputs.
The full workspace test suite was not run.
