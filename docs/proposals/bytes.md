# Immutable byte representation

Binary formats and foreign calls need indexed octets and contiguous borrowed buffers.
The current [library interface](../../lib/std/README.md#text-model) supplies those operations;
[C14](../references/compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls)
owns their backend storage.
This record owns the source-level layout laws for explicit storage and the rationale for byte representation.
The public classifiers live in the library sources; C14 owns host storage and ABI mechanics.

## Design constraints

A buffer denotes an immutable octet sequence.
Content operations compare that sequence, and storage sharing cannot create a mutation channel.
Foreign borrowing can also observe an address; equality of contents does not promise pointer identity
or the same allocation behavior.
This permits backends to share or copy a slice while keeping one source interface.
A start-and-length window matches the explicit pointer-and-length foreign borrow.
`UInt8` makes singleton construction total; library-defined options and booleans stay outside the host ABI.

Contiguous storage gives constant-time indexing and avoids flattening before each foreign borrow.
Shared windows also make decomposition cheap, but a small retained window can keep a large parent alive.
Native host allocation currently copies slices and does not participate in the collector;
sharing there needs an ownership and reclamation account, not just a changed slice operation.
The [cost table](../../lib/std/README.md#byte-operation-costs) makes these target differences explicit.

## Explicit storage contracts

A `Bytes` value alone does not say where fields live, how many bytes a scalar occupies,
or which alignment a borrowed address satisfies.
An ordinary Zydeco product also leaves its physical layout to the compiler.
The implemented [memory library](../../lib/std/memory/package.zy) supplies a first explicit representation boundary:
a logical type `A` has a source-authored `Layout A`, which can be realized into a storage contract.
The concrete stored payload is one contiguous immutable buffer, without retaining the original logical product.
The surrounding runtime value remains a host handle.

### Descriptions, computations, and abstract storage

`Layout A` is abstract. The library constructs layouts with ordinary total value functions:
`product A B left right`, `padding count`, and `align A boundary layout`.
Their bodies construct thunks; applying these value functions does not execute numeric arithmetic during type checking.
Forcing a layout through `realize A R layout no yes` calculates
and validates its metadata using ordinary returning computations, then selects one of the supplied `R` continuations.
The choice of `R : CType` belongs to the caller, so construction requires no `OS` stack.
[Value functions](../references/language.md#8-value-functions-and-views) retain their existing static boundary.

A successful realization supplies [Representation A](../../lib/std/memory/representation.type.zy),
an existential package with an abstract `Stored : VType`, `size`, `alignment`, and four operations:

| Operation | Contract |
| --- | --- |
| `store R value no yes` | Encode `A`, establish backing-buffer alignment, and deliver `Stored` on success. |
| `load R stored no yes` | Decode `Stored` into `A`. A valid stored value satisfies this decoder; its interface retains the explicit failure branch. |
| `bytes stored` | Return the immutable byte buffer for observation or foreign borrowing. |
| `from_bytes R buffer no yes` | Check exact size and canonical contents, establish alignment, and deliver `Stored` on success. |

Only `store` and `from_bytes` introduce `Stored` through this interface.
Arbitrary `Bytes` cannot be passed to `load`, and different existential openings cannot exchange stored values even
when their logical types coincide.
A caller can transfer storage between contracts by explicitly extracting bytes
and validating them at the second contract.
This scopes representation evidence with ordinary package abstraction;
layouts are not runtime indices in the type system.
The [module signature](../../lib/std/memory/package.type.zy) uses an expected existential annotation to prescribe
that abstraction rather than attempting to infer it from the concrete byte implementation.

The current checker cannot derive a total byte decoder from size and layout evidence.
Consequently `load` retains a failure continuation even though these constructors establish its input invariant.
Likewise, two independently opened contracts have no type-level proof that their runtime layouts agree.
These are remaining expressiveness limits: the nominal storage boundary is enforced,
while its byte-level laws are implemented and tested by the library rather than represented as value-dependent proofs.

### Layout laws

All sizes and offsets are nonnegative `Int64` values.
Alignment is a positive power of two. Arithmetic checks the `Int64` bound before adding or rounding;
invalid inputs and overflow select `no` during realization, without allocating a payload.
A representable size does not guarantee that storage can be allocated.
These are dynamic checks; type checking enforces the logical type and abstract storage boundary.

The ten scalar leaves use explicit little-endian storage.
Integers occupy their exact declared width, with signed integers using two's complement.
`Float32` and `Float64` occupy their IEEE bit patterns, including signed zero and NaN payloads.
Scalar size and alignment are both the width in bytes.
Each scalar decoder accepts exactly that many bytes.
The [scalar primitives](../../lib/std/builtin/numeric) implement only these leaves.

`unit` has size zero and alignment one.
`padding n : Layout Unit` has size `n` and alignment one, and stores exactly `n` zero octets.
It can occur as a field in an ordinary product layout.
For a product with metadata `(left_size, left_alignment)` and `(right_size, right_alignment)`:

```text
right_offset = round_up(left_size, right_alignment)
alignment    = max(left_alignment, right_alignment)
size         = round_up(right_offset + right_size, alignment)
```

The left field begins at zero; the right field begins at `right_offset`.
Every gap and trailing byte is zero.
Nested products obey the same rule, so grouping is significant: `A * (B * C)` contains a nested aggregate.
A standalone padding layout may have any nonnegative size; product composition establishes its own aligned stride.

`align A boundary layout` preserves field offsets, raises alignment to the maximum of the requested
and existing alignment, and rounds size up to that alignment with zero tail padding.
It never weakens a field's requirement. For example:

```zydeco
let record = memory/align (UInt8 * UInt32) 16
  (memory/product UInt8 UInt32 memory/uint8 memory/uint32) in
...
```

This description has size 16 and alignment 16.
Its stored bytes are:

| Byte offsets | Contents |
| --- | --- |
| 0 | `UInt8` field |
| 1–3 | Zero gap |
| 4–7 | `UInt32` field, little endian |
| 8–15 | Zero tail padding |

`from_bytes` validates the complete canonical representation.
It rejects truncated or oversized buffers and nonzero padding.
The current implementation decodes and re-encodes to check canonical contents before aligning the supplied buffer.
This deliberately distinguishes an accepted storage contract from arbitrary C struct bytes:
C code must initialize padding to the required value before importing a complete object through this interface.

### Address realization and FFI

The sole allocation primitive added for composition is `bytes/aligned`, described
in [L13](../references/language.md#13-primitive-values-and-capabilities).
Numeric size calculation, power-of-two validation, field placement,
and zero-padding construction remain library computations.
No layout annotation or special compiler interpretation of `product`, `padding`, or `align` is involved.

Interpreter and native realizations preserve the buffer's contents at a borrowed address divisible
by the contract's alignment.
Exporting the returned `Bytes` to the existing C pointer-and-length argument preserves that address and length.
Subsequent byte transformations produce ordinary buffers and carry no stored-type proof;
re-import them through the contract to reestablish its invariants.
The Wasm test host has opaque host-owned bytes and no native C pointer export; see C14 for that target limit.

The [C example](../../lib/tests/ffi/representation.zy) constructs an over-aligned record
and passes it to a [C fixture](../../lib/tests/ffi/boundary.c) that checks address alignment,
`sizeof`, field offsets, contents, and zero padding.
This exercises byte borrowing through the existing FFI, not C aggregate argument classification.
The example's native scalar layout matches the supported little-endian targets;
this is not a portable derivation of every platform's C ABI.
C reads through `memcpy` to avoid assigning an effective C type to the byte allocation.

### Costs and the next representation boundary

Layout realization allocates ordinary closure metadata.
Storage construction currently creates intermediate buffers and concatenates them; native field decoding copies slices.
Deeply nested composition can therefore copy a payload repeatedly.
Import validation also re-encodes. This implementation establishes the semantics needed
to justify a later builder or offset-based codec without embedding layout policy in the compiler.
It provides physical scalar and product storage, but does not change the tagged word convention for ordinary values,
inline `Stored` in call frames, or offer arbitrary field pointers and mutation.
Native host byte objects still live outside the managed collector and are not reclaimed by it.
Allocation is still a host operation; a computation type does not yet express allocator choice or buffer lifetime.

The next bounded step should make allocation and a borrowed region explicit
in the computation protocol while retaining these layout laws.
That would permit construction into caller-provided storage and field reads by verified offsets,
followed by representation-aware native argument/result classification.
Automatically changing all products or adding representation-polymorphic calls first would conflate storage,
ownership, and the native calling convention before their boundaries are expressible.

The [focused tests](../../lang/tests/tests/representation.rs) cover all four execution backends,
exact integer and float bits, padding and size rejection, and abstract-type rejection.
Shared buffer tests check address alignment and retained windows;
the native C test additionally checks the actual foreign borrow.

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
