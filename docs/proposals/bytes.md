# Immutable-byte representation and ownership extensions

The implemented [manual memory](../references/language.md#manual-memory),
[retained immutable bytes](../references/language.md#immutable-owners-and-source-bytes),
and [direct layout recipes](../references/language.md#layout-laws) live in the language reference.
This proposal contains optional immutable updates and future storage choices.

## Alternatives and decision criteria

Tree or rope representations could reduce repeated-concatenation costs at the price of traversal,
node ownership, and flattening for contiguous foreign inputs.
Measure concatenation, slicing, indexed reads, foreign calls, and retained storage before selecting a replacement.
The [stream proposal](filesystem.md#memory-backed-writer-and-byte-builder) owns growable incremental construction.

## CPS destination construction

Scalar and product recipes now write directly into caller-provided storage.
[Byte construction](../references/language.md#immutable-owners-and-source-bytes) publishes a retained result only
after its fill callback completes.
This combines continuation-passing style, which specifies the next computation,
with destination-passing style, which specifies result storage.
Neither implies affinity or automatic cleanup.
The [local CPS compilation proposal](escape-unboxing.md#local-cps-continuations-proposed) owns callback lowering.

Extensible fallible codecs would need a separate partial-progress contract.
A no-write-on-failure promise requires complete preflight before writes or private staging.
Several CPS writes are not automatically transactional, and C may report errors after partial writes.
The current fixed scalar/product recipes have no recoverable failure once valid storage is supplied.

## Functional updates with allocation reuse (proposed)

A functional update must preserve the contents observed through every retained input and slice.
An initial copying implementation can allocate a private destination, copy the input,
apply validated edits, and publish one retained result through CPS.
A finite batch can amortize that copy over several edits.
An arbitrary editor callback requires an additional failure and escape contract.

Reusing the input payload needs the
[compiler reuse obligations](escape-unboxing.md#functional-allocation-reuse-proposed):
exclusive access to the backing allocation, compatible layout, valid roots, and a control-flow account.
The manual `Ptr L S` state protocol provides none of those uniqueness proofs.
`Bytes` currently retains storage until teardown and has no reference counts, thaw, or functional update operation.
Counting retained allocations cannot determine how many source values, slices, closures,
or saved continuations still observe one of them.

Start any reuse experiment with a complete, exclusive allocation and a same-length update.
Shared inputs, partial slices, unknown ownership, and escaped raw pointers require a copying fallback.
`bytes/unsafe/with_window` can expose an address to a callback; its ordinary thunk type does not prove non-escape.
A trusted C consumer's retention contract must also be included in the ownership evidence.
Fallible reservations and bounds checks should precede mutation so failure preserves every input observation.
Reusing a source pair allocation and reusing the backing byte allocation are separate optimizations.

Pair candidate reuse with retained originals, shared slices, captured aliases, saved continuations, and foreign exports.
Measure payload copies, retention bookkeeping, source products, closures, and frames separately.
Avoid exporting an address merely to test identity: that export changes the ownership assumptions.
Avoiding one payload copy does not establish the stronger FIP bounds on total allocation and stack usage.

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

These precedents motivate destination placement and ownership analysis as separate components.
[Perceus](https://doi.org/10.1145/3453483.3454032)
and [FP²](https://doi.org/10.1145/3607840) motivate preserving functional observations while reusing consumed storage.
The [compiler proposal](escape-unboxing.md#allocation-reuse) gives their bibliographic records and distinct guarantees.
Manual typestate is not an implementation of those reuse proofs.

## Remaining questions

- Which fallible codecs can preflight completely, and which need staging?
- Which workloads justify non-contiguous storage?
- Which ownership evidence can justify reclamation and reuse across managed aliases and raw exports
  without imposing per-copy overhead on every value?

Typed field paths
and partial initialization follow the [memory reference](../references/language.md#typed-records-and-field-paths).
