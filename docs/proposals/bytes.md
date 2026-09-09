# Immutable byte representation

Binary formats and foreign calls need indexed octets and contiguous borrowed buffers.
The current [library interface](../../lib/std/README.md#text-model) supplies those operations;
[C14](../references/compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls)
owns their backend storage.
This record concerns representation choices, not a second API specification.

## Design constraints

A buffer denotes its octet sequence. Substituting an equal-content buffer must preserve every observation,
so storage sharing cannot create a mutation channel.
This permits backends to share or copy a slice while keeping one source interface.
A start-and-length window matches the explicit pointer-and-length foreign borrow.
`UInt8` makes singleton construction total; library-defined options and booleans stay outside the host ABI.

Contiguous storage gives constant-time indexing and avoids flattening before each foreign borrow.
Shared windows also make decomposition cheap, but a small retained window can keep a large parent alive.
Native host allocation currently copies slices and does not participate in the collector;
sharing there needs an ownership and reclamation account, not just a changed slice operation.
The [cost table](../../lib/std/README.md#byte-operation-costs) makes these target differences explicit.

## Alternatives and decision criteria

Tree or rope representations could reduce repeated-concatenation costs, at the price of traversal,
node ownership, and possible flattening at the C boundary.
Their appeal depends on the distribution of concatenation, slicing, indexed reads, and foreign calls.
Measure retained storage as well as execution time before replacing the contiguous representation.
No particular tree complexity or default representation is selected here.

Incremental effectful construction has one separate home:
the [memory-backed Writer and byte builder](filesystem.md#memory-backed-writer-and-byte-builder).
A builder can yield an immutable result without adding mutation to `Bytes`.
Functional update would need its own measured use case; in-place byte mutation is outside this interface.

## Remaining questions

- Should ordered collections receive a three-way primitive comparison, or derive a library `Order` value?
- Which workloads justify a non-contiguous representation or native shared-window ownership?
- How should native text and byte storage be reclaimed alongside, or separately from, the managed heap?
  [Native memory](native-frames.md#collection-and-space-behavior) supplies the surrounding lifetime constraints.
