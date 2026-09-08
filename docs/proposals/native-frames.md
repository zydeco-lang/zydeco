# Native activation frames and return continuations

## Status and scope

This proposal describes an alternative AMD64 environment representation: preserve the locals needed
by a return continuation in an activation frame while another computation runs.
The continuation resumes against that frame, avoiding a separate heap capture tuple
and the reconstruction of its local environment.
The first candidate is implemented for AMD64 through the shared `zydeco-machine` model.
It replaces native continuation capture tuples with retained slots; portable ZASM
and WebAssembly keep their existing capture-based lowering.
Performance comparison remains future work.

The independently reviewable question is the lifetime of an activation and the context available
when its continuation resumes.
This document owns the frame lifetime, entry, reclamation, and root invariants.
[Escape analysis and unboxing](escape-unboxing.md) owns the representation choices for individual values;
it can use these frame lifetimes without redefining them.
The current implementation remains documented
in [Runtime Representations](../../DESIGN.md#runtime-representations), and the source meaning of `Ret` remains
in [Computation Types as Stack Protocols](../../DESIGN.md#computation-types-as-stack-protocols).

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
The callee's frame must occupy disjoint storage.
Slots needed by any pending continuation cannot be overwritten or reused while that continuation remains live.
Outgoing arguments and closure captures must also be read before their source storage becomes reusable.

Several continuations can refer to the same activation.
Their entry descriptors may require different slots or extents.
Preservation and root tracking must account for all pending uses,
rather than assuming one saved frame reference per activation.
Resuming an inner continuation cannot invalidate an outer continuation's slots.

Frame references must remain valid while frames are retained.
A nonmoving allocation strategy, such as stable segments, can supply that property directly.
Moving a contiguous environment allocation would instead require relocatable references
or updating every saved reference.
An unchecked growable buffer with interior raw pointers does not satisfy the contract.

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

### Checked native preparation

SPSLow still represents a lexical tree of closed executable blocks.
Continuation conversion additionally records `ContinuationEntry` provenance: the returned-value pattern,
the body after the portable environment preamble, and the source-to-captured-binding relation.
Validation checks this metadata against the actual package, capture product, and entry preamble.
It also checks that the entry body needs only its result bindings and declared captures.
Metadata references describe existing nodes; they do not introduce additional executable occurrences.

Native lowering uses this checked relation to omit the capture product and its unpacking preamble.
It introduces a `RetainFrame` instruction and a resumption entry whose captured bindings alias the source slots.
The root and closure entries establish fresh activations; ordinary branches stay in their current activation.
The native frame analysis follows local control edges and suspension-to-resumption edges to assign owners.
It verifies initialized bindings by forward dataflow, independently of ZASM's context annotations,
and requires suspension captures to agree with the corresponding resumption aliases.

Every distinct local definition reserves one slot within its activation.
Captured aliases resolve to the original definition's slot, including through several nested continuations.
Backward liveness supplies the active slot set at each allocation;
suspension descriptors supply the slots required by pending continuations.
A checked, immutable `NativeProgram` is the AMD64 emitter's input.
The first layout deliberately avoids packing or reusing different locals' slots within one activation.

The lifetime justification comes from the lowered machine-stack operations.
SPSLow values cannot contain a residual machine stack, and the checked continuation code cannot refer
to its own label from its body.
The package pushes code and a token onto the ambient stack; opening it consumes that stack destructively.
Recursive closure invocation establishes another activation instead of reentering a suspended one.
Source control-library encodings can copy ordinary heap closures, but cannot detach or copy these machine tokens.
The runtime also checks that resumption consumes the most recently pending token.
A future IR operation that captures machine stacks must revisit this justification.

### Shared executable frame model

[`zydeco-machine::frames`](../../lang/machine/src/frames.rs) owns `Layout`, `Token`, `Action`, and `Frames`.
The emitter serializes `Action<u64>` followed, where applicable, by static slot indices.
One declaration generates both the Rust header and its serialization order;
AMD64 data sections guarantee word alignment.
The runtime reads the same record as `Action<usize>`, with target-side size and alignment assertions.
The following actions invoke the model's transition methods:

| Action | Model behavior |
| --- | --- |
| Enter | Check capacity, then replace an unretained active frame or append above a retained one; return its base. |
| Suspend | Validate the layout and slots, retain the active frame, and return a fresh tagged token. |
| Resume | Validate the most recent token and owner layout, release that suspension, reclaim younger frames, and return the restored base. |
| Roots | Validate the active slot map and return addresses for its union with every pending suspension's slot map. |

The stub owns `Frames`, which allocates its fixed, nonmoving 1 MiB word region on first entry.
That allocation never resizes, and moving the Rust owner cannot move its words.
Slot access uses raw pointers without constructing Rust references over words addressed by generated code.
Activation and suspension metadata use separate Rust vectors; saved references are indices and checked tokens,
so metadata growth cannot invalidate frame references.
Frame overflow is checked before changing existing state.
Slots need no physical clearing: the compiler proves initialization, and sparse maps select the live roots.
The backing storage's initial zeroes do not count as initialized source bindings.

Frame transitions may allocate Rust metadata but never collect the managed heap.
A suspension leaves `L_k :: token(F) :: S` on the machine control stack.
At `k`, the prologue removes the token while preserving `result :: S`, invokes Resume,
restores `rbp`, and executes the returned-value pattern.
The model restores the allocation frontier as well as the base.
Before a managed allocation, generated code passes its active Roots descriptor and the control-stack cursor;
the stub adds registered host roots and invokes the existing collector.

The [shared model packaging](../../DESIGN.md#shared-rust-runtime-model) pairs these definitions
with their emitter through a source fingerprint in the entry symbol.
Frame operations share executable Rust semantics across code generation and the stub;
instruction selection and SysV register placement still need integration checks.
Returning host and C calls preserve the continuation already on the control stack
and reach the same resumption prologue.
The [C import contract](c-ffi.md) continues to own borrowing, unwinding, and reentry restrictions.

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
No performance result is asserted here.

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

- Should the fixed region grow through stable segments, or should frame references become relocatable?
- How much reserved space can slot packing and smaller resumption extents save without disturbing pending aliases?
- When should compact captures win over retaining a large frame, and how should that decision preserve space behavior?
- Can root enumeration avoid rebuilding and sorting a sparse address vector at every managed allocation?
- Which frame operations should code generation inline while preserving the shared transition contract?
- What is the smallest experiment that fairly compares the previous scheme, flattened captures, and retained frames?

## Implementation references

- [Closure and continuation conversion](../../lang/stackir/src/low/convert.rs) constructs explicit captures and entries.
- [SPSLow syntax](../../lang/stackir/src/low/syntax.rs)
  and [validation](../../lang/stackir/src/low/check.rs) define the present closed block boundary.
- [Native preparation](../../lang/assembly/src/frames.rs) checks ownership and entries and assigns slots and root maps.
- [Assembly lowering](../../lang/assembly/src/lower.rs) uses continuation provenance to construct native resumptions.
- [Native emission](../../lang/amd64/src/emit.rs) emits frame descriptors, transitions, and host bridges.
- [Runtime stub](../../runtime/stub.rs) provides the environment storage and allocation entry points.
- [Native collector](../../runtime/gc.rs) consumes root ranges and updates managed pointers.
