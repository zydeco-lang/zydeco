# Native environment alternatives and evaluation

Retained activation frames are implemented and remain the AMD64 default.
[C11](../references/compiler.md#activation-lifetime) owns their preparation and lifetime justification;
[C12](../references/compiler.md#environment-actions-and-roots) owns default environment transitions and roots.
This proposal keeps the alternatives that remain to be compared and the decisions that would precede a new default.

The remaining work compares storage choices and integrates stronger lifetime or relocation mechanisms.
[Escape analysis](escape-unboxing.md) owns individual value representation selection;
[reachability regions](reachability-regions.typ) proposes support typing and explicit retirement.
The [runtime study](../evaluations/2026-09-08-cbpv-runtime/README.md) keeps dated evidence and measurement limits.
No new default or source control restriction follows from these experiments.

## Removed experiments

Two storage experiments ran beside retained frames until 2026-10-29 and were removed without becoming defaults.
Compact suspension fragments reused one active region and, at Enter, copied the captures
of still-active suspensions into a separate snapshot buffer.
They consumed the compiler's descriptors unchanged, so both engines ran byte-identical generated code,
but overlapping snapshots duplicated values and root locations.
A moving-root adapter stored environments as opaque managed cells and restored their live values
through relocated handles around collection.
Generated code would have needed to reload its base after every potentially collecting operation,
and Enter would have needed a collecting boundary that the native action does not provide.
The [runtime evaluation](../evaluations/2026-09-08-cbpv-runtime/README.md) keeps their measurements and traces.

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

Retained frames are the current baseline for continuations with the required stack lifetime.
The previous implementation and proposed flattened captures can isolate capture costs in comparisons;
the experiment does not require keeping them as compatibility paths.
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
Existing native programs, including effect-library examples and returning C imports, must keep their source behavior.
Focused tests should compare interpreter results with each candidate native scheme and check heap pointer survival
under forced collection.

Use the existing [native model and GC regressions](../references/compiler.md#activation-lifetime) as the baseline
when introducing another engine; extend them with its distinct failure and relocation cases.

Measure execution time, capture allocations, value words copied, collection work,
environment and control-stack high-water usage, peak live heap, and generated code size.
Useful workloads include repeated non-tail calls with many live locals, nested recursion, long tail-call chains,
returning callbacks, escaping closures, and calls retaining one small value beside a large dead value.
Report the compiler settings and hardware, and separate empty captures from calls with substantial saved state.
The [runtime evaluation](../evaluations/2026-09-08-cbpv-runtime/README.md) records bounded experiments,
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

The [runtime study](../evaluations/2026-09-08-cbpv-runtime/README.md) motivates these comparisons
without choosing a new default:

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
