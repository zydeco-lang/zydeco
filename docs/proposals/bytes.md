# Memory representation and ownership extensions

The implemented [memory capabilities and views](../references/language.md#checked-memory-capabilities),
[immutable bytes](../references/language.md#immutable-owners-and-source-bytes),
[storage laws](../references/language.md#explicit-storage-contracts),
and [stored calls](../references/language.md#stored-call-interfaces) live in the language reference.
[C14](../references/compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls) owns host storage.
This proposal contains the remaining representation, construction, and ownership work.

## Alternatives and decision criteria

Tree or rope representations could reduce repeated-concatenation costs, at the price of traversal,
node ownership, and possible flattening at the C boundary.
Their appeal depends on the distribution of concatenation, slicing, indexed reads, and foreign calls.
Measure retained storage as well as execution time before replacing the contiguous representation.
No particular tree complexity or default representation is selected here.

Incremental effectful construction has one separate home:
the [memory-backed Writer and byte builder](filesystem.md#memory-backed-writer-and-byte-builder).
A builder can yield an immutable result without adding mutation to `Bytes`.
The proposed [functional update path](#functional-updates-with-allocation-reuse-proposed) would preserve
that interface's immutable observations while permitting justified allocation reuse.
The current implementation has no update or thaw operation for published bytes.

## Functional updates with allocation reuse (proposed)

Repeatedly rebuilding a byte sequence can copy its entire payload for a small change.
Functional updates should preserve every retained input's contents while allowing an implementation to reuse storage
when the [compiler reuse obligations](escape-unboxing.md#functional-allocation-reuse-proposed) are satisfied.
That section owns the common exclusivity, layout, rooting, and control requirements.
The following is a proposed application to this memory interface, not an implemented extension.

A first operation could replace one octet without changing the visible length:

```text
set : forall R. Bytes -> Int64 -> UInt8 -> Thk (Fault -> R) -> Thk (Bytes -> R) -> R
```

The result has the requested octet and otherwise the input's contents.
Any retained input or slice continues to observe its original sequence.
The initial source implementation can use `build` and `copy_to`, validate the index
before writing, and freeze the completed result.
A batch of validated replacements can amortize this copying over many edits.
An arbitrary editor callback would need an additional failure and escape contract before promising the same costs.

There are two different allocations to optimize.
Reusing the source `Window` constructor saves its descriptor; it does not reuse the payload owned by `BufferArena`.
Payload reuse needs ownership evidence for that backing allocation, separate from access permission.
The present `Access` is a freely copied handle, so counting grant-table entries cannot establish uniqueness.
Even one owner reference inside one shared source object can have several surviving observers.
Dynamic ownership accounting would have to cover those aliases, slices, captures, and escaped grants;
a conservative static proof could instead restrict the fast path to a closed ownership flow.

The first payload fast path should cover a complete, exclusively owned allocation and a same-length update.
Shared inputs, partial windows, insufficient layout evidence, or untracked exported grants select the copy path.
In particular, `with_window` exposes an `Access` that its callback can retain.
Its callback shape does not prove a scoped borrow; an implementation must track the escaped authority
or conservatively keep that allocation ineligible for reuse.
A synchronous C borrow likewise holds the owner stable and excludes reuse until the call finishes.

After preflight succeeds, an internal ownership transition could consume the old immutable owner,
temporarily expose its storage to checked writes, and publish a new immutable owner.
Reusing physical storage must not revive any old allocation or grant identity;
the proposed transition should issue a fresh logical identity even when the address stays the same.
No public `thaw : Access -> Buffer` follows from this design.
Revoking an observable old grant would violate functional update semantics by making an earlier value fail.
The current `freeze` contract, including address identity within that transition, remains unchanged.

All fallible preflight and result-bookkeeping reservations must precede mutation for this initial fixed-size path.
Use the existing checked writes so initialization and pointer-slot provenance remain consistent.
Failure must preserve observable input bytes and grants; broader callback editing would need a separate account
of failure after partial work.
Reuse may preserve the input's physical alignment, but does not make arbitrary future operations preserve it
or prove spare capacity for append.

The proposed sequence is source updates with copy fallback, ownership evidence for a restricted reuse path,
and then dynamic accounting if broader workloads justify it.
The general tracing collector need not be replaced to test statically justified byte reuse.
Reference counting only the retained byte owners is insufficient unless aliases
through the managed heap are included or conservatively excluded from reuse.
Reclamation and reuse should share that ownership account.
The representation of this evidence remains open; it introduces no new source kind or `Bytes` intrinsic here.

Validation should pair an unshared update with a retained original, shared slice,
captured original, saved continuation, and escaped `Access`.
Old observations must remain valid in every shared case.
Invalid indices and failed reservations must preserve owner state and contents.
Measure payload copies and reuse through runtime instrumentation rather than exporting an address merely
to test identity, since that export itself affects eligibility.
Count owner/grant bookkeeping and source allocations separately:
avoiding a payload copy alone does not establish the stronger FIP bound on total allocation and stack use.

## Related work

[Perceus](https://doi.org/10.1145/3453483.3454032)
and [FP²](https://doi.org/10.1145/3607840) motivate preserving functional observations while reusing consumed storage.
The [compiler proposal](escape-unboxing.md#related-work) gives the bibliographic records and their distinct guarantees.
Their application here is ownership-aware reuse of a backing allocation; read permission alone cannot authorize it.
The [region proposal](reachability-regions.typ) addresses the separate lifetime and retirement obligations.

## Remaining questions

- Can direct destination codecs avoid intermediate buffers and repeated concatenation
  while preserving canonical padding, exact-width decoding, and failure before destination mutation?
  Compare the runtime and static-plan C fixtures using executed allocations and copied bytes, not only generated sites.
- What evidence could relate a parent layout, child cell, and offset as a typed field path?
  Current inspected offsets are ordinary integers;
  numerical equality does not equate independently opened stored carriers.
- How should borrowed regions express lexical lifetime and cleanup?
  Freely reusable thunks and completion continuations supply neither affinity nor guaranteed invocation.
  A stronger boundary needs an affine protocol or an explicitly handled dynamic scope.
- Should ordered collections receive a three-way primitive comparison, or derive a library `Order` value?
- Which workloads justify a non-contiguous representation?
- How should native text and byte storage be reclaimed alongside, or separately from, the managed heap?
  [Native memory](../references/compiler.md#activation-lifetime) supplies the surrounding lifetime constraints.
- Which ownership representation can justify byte reuse across source aliases and exported grants
  without retaining dead owners or imposing per-copy overhead on every managed value?
