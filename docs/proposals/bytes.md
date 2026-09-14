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
The next memory extension should use [CPS destination construction](#cps-destination-construction)
to compose writes before publishing an immutable result.
The optional [functional update path](#functional-updates-with-allocation-reuse-proposed) can build
on that construction while permitting justified allocation reuse.
The current implementation has no update or thaw operation for published bytes.

## CPS destination construction

The implemented [destination capabilities](../references/language.md#mutable-destination-capabilities)
already support repeated writes followed by an explicit freeze.
Use that sequence as the foundation for further construction APIs.
A write invokes a completion continuation without constructing an intermediate immutable result;
freeze delivers the completed bytes to their consumer.
This separates advancing a construction from publishing its contents.

The interface combines continuation-passing style (CPS), which makes the next computation explicit,
with destination-passing style (DPS), which makes result storage explicit.
DPS can also use ordinary returns; continuation representation and result placement are independent decisions.
The [local CPS compilation proposal](escape-unboxing.md#local-cps-continuations-proposed) owns callback lowering.

The [Ret and CPS convention](../references/language.md#ret-and-explicit-cps) governs these public interfaces.
Choosing explicit successors does not by itself remove allocations or make a thunk single-use.

For a selected codec and memory provider, the proposed computation shape after forcing an encoder is:

```text
WriteInto A R = A -> Access -> Addr -> Thk (Fault -> R) -> Thk R -> R
```

This schematic shape describes a direct destination codec; it is not an implemented extension to `Storage`.
Its destination is explicit, so several encoders can write successive fields before their enclosing builder freezes.
The existing `Bytes.build` supplies a private destination and a completion that freezes it.
Caller-owned `Buffer` construction instead leaves the caller responsible for choosing when to freeze or close.
The general memory provider accepts `R`; the current `buffer` and `access/write_to` conveniences use `OS`.
Generalizing a source adapter must retain the protocol required by its dependencies.

### Direct destination codecs

The current [storage access path](../references/language.md#access-through-existing-representation-contracts) encodes
into temporary immutable storage and copies that encoding into a destination.
A direct codec could remove those temporaries and the repeated concatenation within compound encoding.
Start with the scalar and product codecs whose complete placement is already known from their representation.
Write fields, gaps, and tail padding into one destination, then invoke completion once the encoding is established.
Exact widths, alignment, canonical padding, and the abstract storage boundary remain governed
by the [storage laws](../references/language.md#layout-laws).

Preserve the existing guarantee that a failed encoding or destination check leaves
that operation's destination unchanged.
Preflight must validate the entire footprint, field encodings, offset arithmetic,
and required reservations before writing.
The write phase must then exclude later recoverable failures and reentry that could revoke its access.
An extensible codec whose behavior cannot establish this separation must retain a temporary encoding or other staging.
Invoking success after several writes does not make those writes transactional.
Earlier successful operations in a sequence remain committed if a later operation fails;
an atomic batch would need its own whole-batch preflight or private staging contract.

Pair accepted nested encodings with invalid destinations, overflow,
and a failing late field that leaves all bytes unchanged.
Compare the completed bytes with the existing encoder, including padding and scalar bit patterns.
Measure executed payload allocations and copied bytes in the runtime and static-plan C construction fixtures,
separately from the [callback and frame costs](escape-unboxing.md#local-cps-continuations-proposed).
CPS alone is not evidence that any of these costs decreased.

### Composed workers and result destinations

The existing [stored-call protocol](../references/language.md#stored-call-interfaces) already delivers results by CPS.
A further source adapter could let a worker assemble output in a caller-supplied destination
and invoke completion without transporting a newly encoded `Stored` value at each stage.
The outer construction would freeze and establish the complete representation before publishing its stored result.
Keep the shared representation witness and its codec with the participating workers:
a writable range alone is not a `Stored` value or proof that a field offset has the required alignment.
This is a source composition extension over the existing word convention; different native result layouts
still require the separate [call-boundary evidence](escape-unboxing.md#remaining-machine-call-boundary).

## Functional updates with allocation reuse (proposed)

Repeatedly rebuilding a byte sequence can copy its entire payload for a small change.
Functional updates should preserve every retained input's contents while allowing an implementation to reuse storage
when the [compiler reuse obligations](escape-unboxing.md#functional-allocation-reuse-proposed) are satisfied.
That section owns the common exclusivity, layout, rooting, and control requirements.
The following is a proposed application to this memory interface, not an implemented extension.

A functional update helper is an optional composition of destination writes and publication.
Its copying implementation can copy the input into a private destination, apply validated edits, and freeze once.
It delivers the final `Bytes` through a success continuation; each internal write needs only completion.
Any retained input or slice continues to observe its original sequence.
A finite batch can amortize copying over many edits.
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

The proposed sequence is direct destination construction, optional functional helpers with copy fallback,
ownership evidence for a restricted reuse path, and then dynamic accounting if broader workloads justify it.
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

- Amir Shaikhha, Andrew Fitzgibbon, Simon Peyton Jones, and Dimitrios Vytiniotis.
  [*Destination-Passing Style for Efficient Memory Management*](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/dps-fhpc17.pdf).
  FHPC 2017. Caller-provided result storage and compositional shape calculation support efficient allocation in a
  restricted functional array language. This motivates starting with known-layout codecs; its stack-like allocation
  guarantees do not extend automatically to arbitrary Zydeco callbacks.
- OCaml's [tail-modulo-constructor transformation](https://ocaml.org/manual/tail_mod_cons.html)
  generates destination-passing workers that initialize constructor fields through private mutation.
  It supplies an implemented precedent for constructing immutable results directly in their final storage.
- MLIR's [bufferization](https://mlir.llvm.org/docs/Bufferization/#destination-passing-style) uses destination operands
  and alias/use analysis to choose between reusing a buffer and allocating another.
  Immutable tensor semantics are preserved when an earlier value remains observable.
  This supports separating destination placement from the ownership evidence required for reuse.

These precedents support the components; Zydeco's checked grants, freeze transition,
and failure-before-mutation contract still need their own validation.

[Perceus](https://doi.org/10.1145/3453483.3454032)
and [FP²](https://doi.org/10.1145/3607840) motivate preserving functional observations while reusing consumed storage.
The [compiler proposal](escape-unboxing.md#allocation-reuse) gives the bibliographic records
and their distinct guarantees.
Their application here is ownership-aware reuse of a backing allocation; read permission alone cannot authorize it.
The [region proposal](reachability-regions.typ) addresses the separate lifetime and retirement obligations.

## Remaining questions

- Which extensible codecs can establish the [preflight and write separation](#direct-destination-codecs),
  and which require staging to preserve failure before destination mutation?
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
