# CBPV runtime findings and further work

This report consolidates the runtime explorations conducted on 2026-09-08.
The main finding is that explicit entry contexts and environment lifetimes support several storage strategies
without changing CBPV's computation protocols.
Static packing, growable storage, and deferred root discovery are established improvements.
Retained frames remain the native default; deferred compact environments are a competitive experimental alternative.

The [native frame proposal](../proposals/native-frames.md) owns the entry,
preservation, reclamation, and collection contracts.
This report owns their evaluation and the priorities for further exploration.
All measurements below precede the arithmetic normalization in `77ee657f`; they establish historical costs
and mechanisms, rather than a performance ranking of the current compiler.

## Findings

### Preserve known contexts through lowering

CBPV distinguishes argument consumption, observation, return, and thunk entry.
Those events need not each create an activation.
A local entry can inherit available bindings, a return entry can recover suspended bindings,
and an escaping closure needs captures with an adequate lifetime.
The useful compilation boundary preserves these distinctions before choosing where the bindings live.
The [entry-context contract](../proposals/native-frames.md#entry-contexts) establishes this for native frames;
a general treatment of context-bearing joins remains further work.

This is flexibility in the programming and compilation model. C can host an explicit machine or CPS translation,
but its usual call ABI does not supply the required residual-stack and environment-lifetime rules automatically. The
semantic starting point is [computation types as stack protocols](../../DESIGN.md#computation-types-as-stack-protocols).

### Combine static layouts with dynamic storage

The environment was already allocated on Rust's heap, and its owner layouts were already statically sized.
The productive changes were reusing slots across nonoverlapping lifetimes and growing storage as activations accumulate.
Packing must account for bindings needed by older pending continuations, including across an inner resumption.

The sequential-locals workload's largest frame shrank from **135 to 4 words**.
A separate 7,000-level recursive case exceeded both fixed-store variants but completed with growable storage.
Static local size therefore combines naturally with dynamic activation counts.
The current growable store caches capacity after return; bounded tail usage does not imply releasing an earlier peak.

### Delay preservation work until storage reuse requires it

A suspension establishes a preservation obligation.
An immediate host return can consume that obligation while the bindings remain in their original slots.
Eager snapshots copied those values unnecessarily.
Deferred snapshots materialize pending captures only when a new activation needs to reuse the active region.
In the repeated-64 trace, required capture/restore copying fell from **179,960,000 to 2,680,000 words**.

Root discovery showed the same pattern: enumerating roots at every allocation dominated the early retained-frame costs.
Deferring enumeration until an actual collection removed that work from ordinary allocation.
Both results favor identifying the event that makes runtime work necessary before optimizing the work itself.

### Retention and compaction favor different workloads

| Representation | Evidence | Current decision |
| --- | --- | --- |
| Retained packed frames | Avoid capture copying and share slots among pending continuations of one activation; retain full frame extents. | Keep as the default. |
| Deferred compact captures | Save sparse suspended state while preserving immediate returns in place; overlapping snapshots can duplicate values and root locations. | Keep as an experimental native option. |
| Managed environment cells | Survive relocation in a collector-integrated trace; collection still copies each retained cell's full payload. | Keep as a model prototype, without native code generation. |

The sparse-deep-64 workload used **88,152 bytes** of persistent environment storage
with retained frames and **16,024 bytes** with deferred captures, including metadata.
The reduction is about 5.5-fold; counting only word-buffer reservation would misleadingly suggest about 45-fold.
Metadata dominates the remaining compact footprint in this case.
Dense captures can instead increase peak words, and shared-activation traces expose duplicated snapshots and roots.
These results motivate selective retention or shared saved captures, rather than establishing a universal winner.

### Managed allocation changes the relocation and lifetime contract

Putting environments in the managed value heap is a different experiment from allocating a stack-shaped Rust buffer.
The prototype repairs moved frame handles and live fields, including after a collecting allocation fails.
Precise root maps exclude dead referents, but do not reduce the frame payload copied by the current Cheney collector.
Managed frame allocation also competes with ordinary values for semispace capacity.

A native implementation would need collecting-entry rules and base reloads after potentially collecting operations,
as specified by the [managed-environment boundary](../proposals/native-frames.md#experimental-managed-environments).
Copied captures alone do not provide detachable or reusable continuations; those need their own ownership semantics.
Ordinary escaping closures still own captures independently of the current nested frame tokens.

### A shared executable Rust contract works

Retained and deferred compact engines ran **byte-identical generated assembly** for all ten final native workloads.
The compiler supplies the same layouts and actions, and the runtime executes the selected environment model.
This demonstrates that storage policy can vary behind a shared contract without duplicating representation decisions.

The useful boundaries are capability-specific: contiguous storage, nested environment transitions,
and moving-frame roots have different guarantees.
The [shared Rust model](../../DESIGN.md#shared-rust-runtime-model) owns executable representation rules;
compiler checks and integration tests still establish initialized bindings, preservation,
root completeness, and agreement with emitted instructions.
Source fingerprints check artifact pairing, not compiler correctness.

### WebAssembly memory

The historical 100,000-iteration tail loop ended with about **13.9 MB** of exported linear-memory capacity
in `wasm-am` and **41.6 MB** in `wasm-sps`, despite the smaller SPS module.
Both paths allocated unreclaimed products and closures; SPS additionally allocated linked protocol-stack nodes.
This motivates reclamation work, while arithmetic normalization requires rerunning the workload
before reusing its numbers.
Bounded native environments likewise establish only part of whole-program space behavior:
Rust-owned host strings and bytes have a separate reclamation problem.

## Further work

1. **Refresh the baseline and broaden the workloads.** Rerun after `77ee657f`,
   which already turns known arithmetic returns into ordinary bindings.
   Include callbacks, escaping closures, overlapping suspensions of one activation,
   host values, and alternating deep and shallow phases.
   Physical AMD64 runs and representative programs should precede any default change;
   arithmetic-heavy Rosetta microbenchmarks are insufficient.
2. **Remove remaining provably local control transitions.** Preserve known entry contexts
   through the relevant IR boundaries and eliminate suspension bookkeeping
   when the transfer cannot reuse the environment.
   Pair local cases with host callbacks and indirect returns that must retain the full protocol.
   [Primitive call normalization](../references/compiler.md#primitive-calls) is the implemented starting point.
3. **Compare shared captures with selective frame retention.** Try an activation-level union
   of saved bindings or retention for dense, overlapping captures.
   Check that older values survive inner resumptions and slot reuse.
   Measure copying, duplicate root locations, metadata, and total reservation together.
4. **Reclaim words and metadata together.** Compare segments, regions, or shrink policies under deep-to-shallow phases.
   Demonstrate that reservation falls after the retained state is released, while tail usage remains bounded.
   A segmented suspended store can implement the environment capability without promising one contiguous storage base.
5. **Extend capabilities when a program needs them.** Integrate moving environments or detached control only
   with an explicit relocation, ownership, safepoint, and host-boundary contract.
   Force collection and allocation failure at those boundaries.
   Flattened control-stack captures, generational environments, and region allocation remain candidates,
   not completed native comparisons.
6. **Evaluate whole-program reclamation.** Add long-running WebAssembly and native host-value cases.
   Account separately for live values, environment/control storage, cached capacity, and external resources.
   Select tracing, regions, or destructive reuse only after establishing the required lifetime and sharing discipline.

## Evidence and limits

Native timing used optimized Rust runtimes and AMD64 executables translated by Rosetta on an Apple M5 Pro.
In the final ten-workload comparison, deferred captures ranged from approximately 1% slower
to 6% faster than retained frames.
Small process-wall-time differences do not establish a general throughput ranking.
Runtime optimization and inlining materially affected the results;
compiler `--release` alone does not optimize the linked runtime.
See the [native build workflow](../../CONTRIBUTING.md#compile-programs).

Persistent environment figures include word-buffer capacity, Rust owner state, and metadata-vector capacity.
They exclude allocator overhead, the control stack, managed heap, host roots,
and temporary collection vectors; they are not process RSS.
Copy counts are derived from executed action traces; GC root visits are separately observed.
The native timing workloads each had at most one pending suspension per dynamic activation.
Separate shared-activation traces establish why their space results do not generalize to overlapping suspensions.

Focused model, compiler, GC, native ABI, callback, control-library, and FFI checks passed,
including pointer relocation and state preservation on rejected transitions.
The full workspace suite was not run. The managed-frame results are executable collector traces;
flattened and segmented alternatives have no native measurements in this study.

The [evidence directory](runtime-study-2026-09-08/) retains samples, commands,
source hashes, probes, pilots, and setup failures.
The principal records are:

| Question | Records |
| --- | --- |
| Capture copying and root discovery | [Optimized comparison](runtime-study-2026-09-08/optimized.json), [repeated captures](runtime-study-2026-09-08/wide.json), [dev profile](runtime-study-2026-09-08/debug.json), [layout trace](runtime-study-2026-09-08/layouts.csv) |
| Packing, growth, and moving frames | [Final comparison](runtime-study-2026-09-08/environment-inlined.json), [capacity checks](runtime-study-2026-09-08/environment-capacity.json), [collector/storage traces](runtime-study-2026-09-08/environment-layouts.csv), [manifest](runtime-study-2026-09-08/environment-manifest.json) |
| Compact suspensions | [Eager comparison](runtime-study-2026-09-08/fragments-eager.json), [final deferred comparison](runtime-study-2026-09-08/fragments-final.json), [collector/storage traces](runtime-study-2026-09-08/fragments-final-layouts.csv), [C-boundary check](runtime-study-2026-09-08/fragments-ffi.json), [manifest](runtime-study-2026-09-08/fragments-final-manifest.json) |

Historical implementation anchors are `a3df0fb8` (heap captures), `062d5ce7` (retained frames),
`88329154` (deferred roots), `77b9c14e` (packing and growth), `26bc3132` (eager compact captures),
and `a12d087a` (deferred compact captures).
Recorded source hashes identify intermediate variants.
Build historical compilers in separate source and target directories and use the model bundled by each compiler.

[`runtime-study.py`](../../lang/tests/runtime-study.py) compares the original capture/root variants;
[`environment-study.py`](../../lang/tests/environment-study.py) accepts explicit `--variant NAME COMPILER RUNTIME`
pairs, with `--fragments` for compact comparisons.
Enable `compact-environments` in a separate runtime copy's default Cargo features for that variant.
Both runners expose `--help` and record exact commands and output oracles.
The executable layout checks are `frame_layouts` in `zydeco-machine`, and `environment_layouts`
and `environment_fragments` in `zydeco-tests`; run them with `cargo run -p PACKAGE --example NAME`.

## Literature pointers

- [Levy's CBPV account](https://pblevy.github.io/cbpv.html)
  and [Zydeco's stack-manipulating computation paper](https://arxiv.org/html/2502.15031v1#S2):
  semantic protocols and their flexibility.
- [Appel and Shao, 1996](https://www.cs.princeton.edu/~appel/papers/stack2.pdf)
  and [Farvardin and Reppy, 2020](https://kavon.farvard.in/papers/pldi20-stacks.pdf): creation, access,
  copying, sharing, and controlled comparisons of stack/continuation implementations.
  Their collector and calling-convention assumptions differ from these experiments.
- [Tofte and Talpin, 1997](https://researchprofiles.ku.dk/en/publications/region-based-memory-management/)
  and [the Capability Calculus](https://www.cs.cornell.edu/talc/papers/capabilities-abstract-tr.html):
  region lifetimes and safe reclamation.
- [Downen's Call-by-Unboxed-Value](https://pauldownen.com/publications/cbuv.pdf):
  a further direction for explicit value representations and calling conventions, beyond choosing environment storage.

The [frame proposal's bibliography](../proposals/native-frames.md#literature-pointers) adds closure space safety,
join points, structured continuation use, and compiler/runtime coordination.
The pinned [Fiddle](https://github.com/zydeco-lang/fiddle/tree/9a8941c224635ccfa1955c4ecaaf3de561e1fde2)
and [Riddle](https://github.com/UMjoeypeng/riddle_compiler/tree/54f9d9221af412b850172916b5ffacd63f403128)
source audits supplied implementation alternatives; neither was built or timed in this study.
