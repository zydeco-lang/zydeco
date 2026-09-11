# Native activation frames and return continuations

## Status and scope

Retained activation frames are the current AMD64 environment representation.
[C11](../references/compiler.md#c11-native-preparation-activation-frames-and-amd64-emission) owns native preparation,
slot assignment, and the lifetime justification;
[C12](../references/compiler.md#environment-actions-and-roots) owns the shared model and root contract.
Portable ZASM and WebAssembly retain capture-based lowering.

This record owns the reasons for retaining frames, the lifetime requirements any alternative must meet,
and the experimental storage choices.
[Escape and unboxing](escape-unboxing.md) owns individual value representations.
The [reachability and regions formalization](reachability-regions.typ) proposes general support typing
for values and captured storage.
Relating that proposal to physical activation lifetimes remains a lowering obligation.
The [runtime study](../ideas/cbpv-runtime-evaluation.md) keeps dated evidence and measurement limits;
source computation protocols belong to [L6](../references/language.md#6-computations-and-control).

## Motivation and the previous implementation

There are two different meanings of environment at runtime.
The current environment stores bindings available to the executing code.
A captured environment stores the values needed when suspended code starts or resumes.
The previous AMD64 backend implemented the former with a reusable buffer and the latter with ordinary products.

At native entry, `rbp` was initialized to the buffer supplied by the Rust stub.
Generated Zydeco code left that base unchanged.
Binding a value stored it at the next compiler-assigned slot; reading it loaded from the corresponding offset.
An SPSLow jump staged its outgoing control stack and target, then started the destination with an empty local context.
`AllocContext` emitted no pointer adjustment or allocation.

Closure conversion makes this reuse safe. It gives each closure a capture tuple and code label,
and each return continuation a code label followed by its capture tuple and residual control stack.
If a caller needs `x` and `y` after calling `f`, the continuation captures their value words
before the callee overwrites the current environment.
Heap pointers in those words preserve the referenced objects; they do not point back
into the reusable environment buffer.

With the control-stack top on the left, the usual layout is:

```text
closure f:            [E_f, L_f]
return continuation: L_k :: E_k :: S       where E_k = (x, y)

entry to f:          E_f :: argument :: L_k :: E_k :: S
entry to k:          result :: E_k :: S
```

The generated continuation entry bound the result and unpacked `E_k` into its newly assigned environment slots.
Local unboxing eliminated some product allocations,
but a nonempty capture product crossing an ordinary continuation entry generally remained boxed.
Portable lowering still uses this representation.

This representation has a useful property: suspended code retains an explicit set of captures,
and the current environment can always be reused.
Its cost is transferring surviving locals out to a capture tuple and back into local storage.
Retained frames preserve their storage location across the call.

## Preferred representation

Use an environment stack of activation frames, initially separate from the existing machine control stack.
An activation is a dynamic invocation with local storage; it may execute several blocks
and resume at several continuation labels.
A block label alone does not identify a fresh activation.

An active frame has a base, a known layout, and a description of its initialized live slots.
`rbp` addresses the active frame.
An allocation frontier identifies the end of reserved environment storage independently of that base.
A suspended return continuation retains a reference to its owner's frame and enough entry metadata to resume it.
The implementation keeps the frontier and ownership metadata in the shared model, separately from frame words.

```text
environment stack, older to newer:

    older retained frames
    caller frame F: [x, y, ...]     <- retained by continuation k
    callee frame G: [...]           <- current rbp

control stack at entry to f, top first:

    E_f :: argument :: L_k :: token(F) :: S
```

The continuation label identifies a resumption layout: where its captured bindings already reside,
where to bind the result, and which environment extent to retain on resumption.
The implementation attaches static descriptors to generated entries and uses checked tokens on the control stack.
No heap tuple of `x` and `y` is needed for this continuation.

Keeping the environment stack separate preserves the existing argument and continuation push/pop convention
while frame management is evaluated.
A single machine stack could hold both locals and control frames later, but would need
to coordinate local reservations with consumption of arbitrary SPS stack protocols.
Choosing separate storage here does not prescribe an eventual unified stack ABI.

## Frame lifetime and entry invariants

### Entry contexts

A continuation entry must be compiled under the layout of the frame it resumes.
Every incoming transfer must establish that layout and initialize all slots the entry can read.
The available bindings are an explicit entry contract, even when the corresponding frame reference is passed
in a hidden machine location.
This gives a concrete runtime meaning to a block having a known context.

Three entry roles need distinct treatment:

- A local branch continues in the current activation and preserves its applicable slot bindings.
- A closure entry establishes an activation from its explicit closure environment and incoming arguments.
- A return-continuation entry restores a retained activation and binds the returned value in its resumption layout.

Physical slot availability and knowledge of a slot's contents are different properties.
This proposal preserves the former and provides a place to attach the latter;
it does not introduce a general static fact system or assume that a known frame makes every captured value constant.

### Suspension and preservation

Before entering a callee, construct the return continuation and publish the caller frame state it requires.
In the retained engine, the callee's frame must occupy disjoint storage.
Slots needed by any pending continuation cannot be overwritten or reused while that continuation remains live.
The [compact alternative](#experimental-compact-environments) preserves those bindings
in separate snapshots before reusing their original active storage.
Outgoing arguments and closure captures must also be read before their source storage becomes reusable.

Several continuations can refer to the same activation.
Their entry descriptors may require different slots or extents.
Preservation and root tracking must account for all pending uses,
rather than assuming one saved frame reference per activation.
Resuming an inner continuation cannot invalidate an outer continuation's slots.

Logical frame references must remain valid while frames are retained.
The retained engine saves word offsets in activation metadata and indices in suspension records.
Entry may relocate the contiguous allocation; it returns the new active base,
which generated code loads into `rbp` before accessing any environment slot.
Earlier raw bases and root-slot addresses expire at entry.
No managed value, escaping closure, foreign borrow, or saved continuation can contain an environment-slot pointer.
Suspend, Resume, root enumeration, and managed collection do not relocate this storage.
A nonmoving store satisfies the same protocol with a stronger physical-address guarantee.

### Return and reclamation

A return stages its result and continuation target, identifies the retained frame
and its resumption extent, reclaims the younger environment storage that is no longer retained,
restores the active base and frontier, and transfers control.
The result remains rooted if any step can allocate or trigger collection.
The resumed code reads existing captured slots and establishes the result binding without unpacking a capture tuple.

Reclamation follows proven nesting and lifetime relationships.
A frame may be reclaimed only after its active use has ended and no pending continuation
or permitted borrow can reach it.
Resumption metadata must describe the actual retained extent; restoring only `rbp` is insufficient
when allocation also has a separate frontier.

### Tail transfers

Machine jumps do not by themselves determine frame lifetime: both source tail transfers
and calls with explicit return continuations currently become jumps.
Allocating a frame at every jump would destroy bounded-space tail recursion.

A tail transfer that leaves no continuation retaining the current activation reuses
or reclaims that activation's storage before entering the destination.
If a continuation retains it, the activation becomes suspended, and the callee uses other storage.
A tail-call chain beneath a fixed set of suspended continuations must have environment usage bounded by
that retained storage plus its largest active frame, independent of chain length.

### Escaping values and control

An ordinary closure that outlives an activation must own captures with a sufficient lifetime.
It cannot retain a raw reference to that activation's reclaimed slots.
Existing heap capture environments remain a valid representation for such closures.
Sharing a frame with a closure requires a separate lifetime justification; it is not a consequence
of knowing the closure's code label.

The native backend already realizes control through destructive stack operations,
but the source classifier `Ret A` is not itself a linearity or non-escape proof.
The compiler must justify the nesting and lifetime of the concrete continuations that use retained frames.
Lexical single occurrence of an IR node also does not prove a dynamic one-shot property.
Library encodings of control must be checked through their actual lowered operations,
rather than classified by their names.

An implementation that supports detached, duplicated,
or later-reentered machine continuations would need a corresponding ownership and storage model,
such as copied stack segments or heap frames.
Such behavior cannot be obtained by retaining unchecked pointers into a reclaimed environment stack.
This proposal does not add a source restriction to make the representation fit.

## Collection and space behavior

The native collector receives the active control-stack range and a sparse list of mutable root addresses.
That list includes live slots in active and suspended frames, together with registered host roots.
The [existing word and collector contracts](../../DESIGN.md#native-garbage-collection) still determine how
those values are traced and updated; frame references are control metadata, not managed heap objects.

At every collecting operation, the active frame, all suspended live slots,
staged arguments, and temporary results must be discoverable.
Publishing a new frame and changing the active base must either be a sequence
with no intervening safepoint or expose a complete intermediate root state.
Reserved but uninitialized frame capacity must not be scanned as runtime values.

Retaining a frame can retain more than compact capture tuples do.
A dead slot pointing to a large object must not keep that object reachable just
because another slot in the same frame is needed.
Suspension descriptors can identify live slot sets, or generated code can clear dead slots before exposing a scan range.
If multiple continuations retain a frame, the required roots are the union of their live slots.
Word tags answer whether a live word is pointer-shaped; they do not answer whether a slot is live.

Evaluation must measure both frame capacity and retained heap data.
Avoiding capture allocation is not a sufficient space result if large frame reservations
or dead references remain live through deep calls.
Slot reuse, smaller retained extents, or compact continuation captures may be preferable for some activations.

## Boundary with compiler and runtime

The common [preparation contract](../references/compiler.md#c11-native-preparation-activation-frames-and-amd64-emission)
provides initialized bindings, ownership, packed slots, and suspension maps.
An alternative storage engine must preserve every pending capture,
not merely the currently executing block's live locals.
The [environment capability](../references/compiler.md#environment-actions-and-roots) permits suspended values
to move between transitions but constrains the active base and published root addresses.
This is the boundary against which the following experiments are compared.

For example, an inner resumption cannot reuse `x`'s slot while an older continuation still captures `x`,
even if the inner block never reads it.
Ordinary backward liveness is therefore insufficient for slot reuse.
Likewise, implementing a copied source-level control closure does not grant permission
to copy or detach a machine token.
A new machine-stack operation would require revisiting the lifetime proof before selecting storage.

### Experimental compact environments

[`frames::fragments::Fragments`](../../lang/machine/src/frames/fragments.rs) implements the same
`Environment` capability with one reusable active region and a separate contiguous buffer of captured values.
Suspend records a pending use of active slots.
A host operation can consume that token through Resume without moving either its environment or its captures.
Only Enter, which will reuse the active region, materializes the still-active suspensions as compact fragments.
The generated transfer has already staged its outgoing arguments before this boundary.
A saved fragment records the layout, slot map, and token; Resume copies its current values back
to those offsets and reestablishes the owner's layout.
Slots outside that entry's declared captures are unavailable.
The compiler's preservation analysis remains unchanged, so both engines accept the same generated code.

Saved suspensions form a prefix of the pending-token vector, followed by a suffix referring to active slots.
Entry visits only that suffix. A tail entry with no newly pending active suspensions does not walk the saved prefix.
Consuming an active token performs no capture copy; consuming a saved token restores its fields
and retracts the compact frontier.
Older snapshots remain saved after an inner resumption.
This permits a resumed activation to publish new active suspensions above an older saved prefix.

Several active suspensions share their original slots, and their root locations are deduplicated by address.
After entry, their independent snapshots can contain copies of overlapping captures.
The collector must update every physical copy of a live pointer.
Deduplicating equal pointer values would leave some resumption copies stale; only duplicate addresses can be removed.
Roots combines the ordinary active map with the active suspension maps and every materialized snapshot word.
Dead active slots are still excluded.
The root source remains deferred until collection actually needs it.

Both buffers use the selected `Storage` policy and cache capacity independently.
The snapshot frontier follows token nesting; consuming a token reclaims its fragment
without a heap allocation or free operation per continuation.
Enter first reserves and fills an unpublished snapshot suffix, then reserves the destination active extent,
and finally publishes the new locations.
If either reservation fails, logical captures, pending tokens, and the current active base remain usable.
Snapshot reservation may already have increased capacity or moved that buffer on a later active-reservation failure;
previously returned root addresses expire at the attempted transition, and fresh enumeration resolves the new addresses.
No managed collection occurs during staging or publication.

Enter may grow the active region, and previously reserved scratch extents remain addressable on Resume.
With a fixed pending continuation set, tail entries remain bounded by the largest active region plus its snapshots.
The high-water statistic includes the temporary overlap between outgoing active slots and completed snapshots.
Whole-buffer reservation can exceed that logical bound because capacities retain earlier peaks and geometric slack.
The separate metadata statistic includes the Rust owner and allocated control-record capacities; word buffers,
temporary root vectors, and allocator bookkeeping have distinct accounting boundaries.

The standalone runtime's experimental `compact-environments` Cargo feature selects this engine;
retained frames remain the default.
Both selections consume the compiler's bundled model and identical descriptors.
This experiment keeps captures in ordinary Rust storage, not in the moving value heap.
It therefore isolates capture copying and compact storage without introducing the collecting-entry boundary below.
It also retains the nested token discipline: copied payloads do not make machine continuations detachable or duplicable.

### Experimental managed environments

[`frames::moving`](../../lang/machine/src/frames/moving.rs) supplies a collector-integrated experimental contract
for environments stored as opaque managed cells.
Each registered live frame has a mutable handle and a slot map covering all its live uses.
Before collection, `MovingRoots` lifts those values into stable temporary root storage
and publishes the frame handles alongside them.
After collection it restores the updated values through the relocated handles.
Restoration also occurs when the collecting allocation returns an error.
Dead fields remain untraced, while the collector still copies each retained frame's complete cell.

This capability is outside `Storage`: a managed allocation can move the active environment,
so generated code would need to reload its base after every potentially collecting operation.
Entering a managed frame would also require publishing the caller, staged arguments,
and intermediate results before that entry can allocate.
The current native Enter action does not provide such a collecting boundary.
The executable trace and collector regressions exercise the root contract;
they do not implement a second native backend.
The explicit handle table covers registered environments, not arbitrary frame references hidden in escaping closures.
Supporting those references, detached control,
or shared immutable continuation environments requires its own reachability and ownership account.

## Alternatives to compare

| Scheme | Suspended caller state | Work at suspension and resumption | Main tradeoff |
| --- | --- | --- | --- |
| Previous reusable environment | Heap capture tuple referenced from control stack | Pack captures, then unpack into reused slots | Compact captures, allocation and copying |
| Flattened continuation captures | Capture words directly on control stack | Push captures, then bind them into reused slots | Removes the tuple allocation, retains copying |
| Retained environment frames | Existing slots in a suspended frame | Save and restore frame state | Avoids capture copying, needs lifetime and liveness management |
| Heap activation frames | Heap frame referenced by continuations | Change active frame reference | Flexible lifetimes, collection and retention costs |

Flattened captures are a smaller experiment that isolates allocation cost from copying cost.
For example, `L_k :: pointer_to_tuple(x, y) :: S` becomes `L_k :: x :: y :: S`.
The producer and continuation entry must agree on field count and order; this is a calling-convention change,
not just omitting `PackProduct` locally.

The preferred candidate is retained frames for continuations with the required stack lifetime.
The previous implementation and flattened captures are useful measurement baselines,
not permanent compatibility paths required by this proposal.
Heap frames remain a distinct candidate when lifetime flexibility or closure sharing outweighs stack reclamation.
The experiments should determine which combinations are worth keeping.

## Representative checks and evaluation

The following paired checks guide validation at the compiler and runtime boundaries.

| Accepted behavior | Rejected counterpart or failure invariant |
| --- | --- |
| Resume with the owner's declared frame layout | Reject an incompatible frame or a read of an uninitialized slot before executing the entry |
| Callee overwrites its own locals while caller values survive | Reject slot reuse that overlaps any pending continuation's live bindings |
| Return through several nested activations | No reference to younger reclaimed storage remains usable |
| Long tail recursion under a fixed outer continuation | Environment high-water usage does not grow with the tail-call count |
| Escaping closure owns valid captures after return | Reject an escaping raw reference to a reclaimed activation |
| Collection updates pointers held only by suspended frames | No reachable value is lost and no resumed code observes a stale moved pointer |
| Dead large values become collectible while another local remains live | Reserved or dead slots do not become roots merely through frame retention |
| Checked frame growth and host transitions | Overflow cannot overwrite retained frames; host return restores the declared entry state |

Both compiler validation and runtime model checks can enforce these conditions at their appropriate boundary;
`FramePlanError` and `FrameError` report violations at those boundaries.
Existing native programs, including control-library examples and returning C imports, must keep their source behavior.
Focused tests should compare interpreter results with each candidate native scheme and check heap pointer survival
under forced collection.

The focused regressions live in the `frames` modules of `zydeco-machine` and `zydeco-assembly`,
SPSLow continuation-conversion tests, and the `native_model` and `native_gc` integration targets.
They cover mismatched metadata, missing initialized bindings, invalid layouts and slot indices,
stale or out-of-order resumption, overflow without corrupting a caller,
and a 100,000-transfer tail chain with bounded environment usage.
The collector regression keeps a moved object solely through a suspended slot
while collecting a dead object in another slot of the same frame.
A source regression compares interpreter and native results for an escaping closure held
across a 100,000-call tail chain.
Emission checks confirm that returning calls with captures no longer allocate continuation products.
Existing core, builtin, control-library, and C-boundary cases exercise the integrated ABI.

Measure execution time, capture allocations, value words copied, collection work,
environment and control-stack high-water usage, peak live heap, and generated code size.
Useful workloads include repeated non-tail calls with many live locals, nested recursion, long tail-call chains,
returning callbacks, escaping closures, and calls retaining one small value beside a large dead value.
Report the compiler settings and hardware, and separate empty captures from calls with substantial saved state.
The [runtime evaluation](../ideas/cbpv-runtime-evaluation.md) records bounded experiments,
including root-enumeration timing and reserved-space accounting; it does not establish a universal winner.

## Literature pointers

These sources provide background and evaluation criteria.
Their results do not establish that the proposed Zydeco representation is correct or faster.

- Andrew W. Appel and Zhong Shao, *An Empirical and Analytic Study of Stack vs. Heap Cost for Languages with Closures*,
  Journal of Functional Programming,
  1996 ([author-hosted preprint](https://www.cs.princeton.edu/~appel/papers/stack2.pdf)).
  Sections 2-5 separate frame creation, access, copying, sharing, and space safety;
  later sections discuss collection and first-class continuations.
  The study motivates measuring total representation cost instead of treating stack allocation
  as an automatic improvement.
  Its historical machine measurements are not predictions for today's AMD64 backend.
- Zhong Shao and Andrew W. Appel, *Space-Efficient Closure Representations*,
  LFP 1994 ([author publication page](https://flint.cs.yale.edu/shao/papers/closure.html)).
  Uses compile-time control and data flow to choose closure representations while preserving space behavior.
  This is relevant to choosing compact captures versus shared retained storage and to the danger
  of keeping dead data reachable through an environment.
- Kavon Farvardin and John Reppy, *From Folklore to Fact: Comparing Implementations of Stacks and Continuations*,
  PLDI 2020 ([author-hosted paper](https://kavon.farvard.in/papers/pldi20-stacks.pdf)).
  Compares fixed, resizing, segmented, hybrid, linked, and immutable CPS strategies in one compiler and runtime.
  Its methodology motivates controlling layout, allocator, and calling convention separately.
  Its call-stack and concurrency measurements do not directly predict Zydeco's separate environment/control stacks.
- Josh Berdine, Peter W. O'Hearn, Uday S. Reddy, and Hayo Thielecke, *Linearly Used Continuations*,
  CW 2001 ([paper](https://www.microsoft.com/en-us/research/wp-content/uploads/2000/12/linuc.pdf)).
  Gives target-language accounts of structured continuation use, including call/return.
  It helps distinguish a justified control discipline from unrestricted first-class continuations.
  Linearity alone is not the frame-nesting proof required by this proposal.
- Luke Maurer, Paul Downen, Zena M. Ariola, and Simon Peyton Jones, *Compiling without Continuations*,
  PLDI 2017 ([paper](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/join-points-pldi17.pdf)).
  Makes join points explicit in a functional IR. It is a useful comparison for distinguishing local control entries
  from general function values and preserving surrounding context.
  Join points are not by themselves an implementation of returning activation frames.
- Simon Peyton Jones and Norman Ramsey,
  *Machine-independent support for garbage collection, debugging, exception handling, and concurrency*, MSR-TR-1998-1,
  1998
  ([report](https://www.microsoft.com/en-us/research/publication/machine-independent-support-for-garbage-collection-debugging-exception-handling-and-concurrency/)).
  Its C-- design coordinates compiler metadata, runtime interfaces, safepoints, and alternate continuations. It is
  relevant to the shared contract needed for frame restoration and root discovery, rather than evidence for a particular
  frame layout.

## Remaining decisions

The [runtime study](../ideas/cbpv-runtime-evaluation.md) motivates these comparisons without choosing a new default:

- Remove provably local control transitions when known entry contexts establish
  that no suspension bookkeeping is needed.
  Preserve the full protocol for indirect returns and host callbacks;
  [primitive normalization](../references/compiler.md#primitive-calls) is the implemented starting point.
- Compare activation-level shared captures with selective retention for dense, overlapping suspensions.
  Measure copying, duplicate roots, metadata, and total reserved space; older captures must survive inner resumptions.
- Reclaim word storage and metadata together during deep-to-shallow phases.
  Compare segments, regions, and shrink policies while preserving bounded tail usage.
  Segmented suspended storage need not promise one contiguous base for every inactive frame.
- Integrate moving environments or detached control only with explicit relocation, ownership, safepoint,
  and host-boundary contracts, including collection and reservation failure at those boundaries.
  Flattened control-stack captures, generational environments, and regions remain candidates.
- Include native host text and byte storage in long-running reclamation studies,
  accounting separately for live values, frame/control storage, cached capacity, and external resources.

Rerun after changes to normalization and include overlapping suspensions, callbacks,
escaping closures, host values, and alternating deep and shallow phases.
Physical AMD64 measurements and representative programs should precede a default change;
Rosetta arithmetic microbenchmarks alone do not establish a general ranking.
