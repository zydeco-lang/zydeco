# Foreign exports, callbacks, and ABI extensions

Returning C imports are implemented.
Their source obligations belong to [L14](../references/language.md#14-foreign-interfaces),
and their validated call plan and target adapters to [C14](../references/compiler.md#foreign-calls).
The [xxHash binding](../../lib/ffi/xxhash.zy) demonstrates the current pointer-and-length borrow.
This record owns the next independently reviewable foreign boundaries.

## Following boundary

A C caller entering Zydeco must establish a runtime and a CBPV return continuation.
An export returning an ordinary C scalar could initialize that context and resume the caller after `Ret`,
but the entry's allocation roots, failure behavior, and repeated-call lifetime must be specified first.
An `OS` export instead needs a root-stack protocol and an explicit answer about process termination.
Neither follows merely from reversing the current import marshalling.

## Closures, callbacks, and reentry

A callback needs both executable code and its captured environment.
Choose who owns that environment, how a foreign caller releases it,
and whether the callback may outlive the import call.
Captured managed values must remain rooted while foreign code retains the callback.
The current transient byte borrow does not provide an ownership protocol for such values.

Reentry must define which runtime and stack context it enters, whether an earlier call is suspended,
and how nested calls preserve roots and return continuations.
Unwinding and nonlocal exits need an explicit boundary protocol before they can be admitted.
A callback's source computation type alone does not establish thread safety, purity, termination, or single invocation.
These decisions should precede syntax and be exercised with nested calls and retained environments.

## Additional ABI shapes

Fixed-width signed and unsigned integer parameters/results and `void` results now extend the typed call plan.
They reuse integer registers without requiring a new source representation or lifetime protocol;
[L14](../references/language.md#14-foreign-interfaces) owns their accepted classifiers.
The [integer fixture](../../lib/tests/ffi/integers.zy) checks extrema, mixed widths in all six registers,
unit continuation resumption after an observable C operation, and deliberately dirty upper return bits on AMD64.

The remaining shapes are deferred with distinct prerequisites:

| Direction | Required extension before implementation |
| --- | --- |
| Floating-point scalars | Classify and allocate SSE argument/result registers independently of integer registers; preserve payload bits across marshalling and mixed calls. |
| More than six integer components | Plan stack arguments, their alignment, and cleanup together with the existing temporary frame and return continuation. |
| Aggregates by value | Derive target ABI classes from an explicit storage contract, including register splitting, memory arguments, and hidden result pointers. |
| Raw or mutable pointers | Specify bounds, mutability, ownership, validity duration, and alias behavior; an integer address supplies none of this evidence. |
| Callbacks and exports | Establish runtime entry, retained roots, completion, and reentry as described above. |

The [storage access extension](bytes.md#access-through-existing-representation-contracts) already constructs
an aligned C record in a caller-provided buffer and passes the frozen result through the existing immutable borrow.
That supplies a useful aggregate-pointer path while aggregate calling conventions remain open.
`Layout A` is presently a runtime recipe, so its size and alignment are not static ABI classification evidence.
Keep argument order and flattening in one validated representation consumed by every target as these shapes are added.

Header-based validation could check some declaration mistakes but would introduce a separate source of ABI evidence.
Specify how it interacts with the trusted declaration and platform configuration.
Native imports remain outside the Wasm and ZASM-interpreter execution profiles until a separate adapter is designed.

## Validation criteria

[Signature tests](../../lang/statics/tests/foreign.rs)
and [integration fixtures](../../lang/tests/tests/ffi.rs) cover the current subset.
New boundaries need accepted and rejected shapes plus observable lifetime checks:
argument order, full-width results, empty buffers, loader failure, collection with live captures,
and invalid entry or release without partial runtime mutation.
Installed-library and native-toolchain runs remain opt-in under the repository's testing workflow.
