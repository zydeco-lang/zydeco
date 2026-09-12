# Bytes, explicit storage, and memory views

Binary formats and foreign calls need indexed octets and contiguous borrowed buffers.
The current [library interface](../../lib/std/README.md#text-model) supplies those operations;
[C14](../references/compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls)
owns their backend storage.
This record owns the source-level layout laws for explicit storage, the rationale for byte representation,
and the proposed address, cell, and memory-view interfaces below.
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
The implemented memory libraries supply an explicit representation boundary:
a logical type `A` has a source-authored layout, which can be realized into a storage contract.
The concrete stored payload is one contiguous immutable buffer, without retaining the original logical product.
The surrounding runtime value remains a host handle.

### Descriptions, computations, and abstract storage

The [runtime builder](../../lib/std/memory/package.zy) has an abstract `Layout A`.
It constructs layouts with ordinary total value functions: `product A B left right`,
`padding count`, and `align A boundary layout`.
Their bodies construct thunks; applying these value functions does not execute numeric arithmetic during type checking.
Forcing a layout through `realize A R layout no yes` calculates
and validates its layout information using ordinary returning computations,
then selects one of the supplied `R` continuations.
The choice of `R : CType` belongs to the caller, so construction requires no `OS` stack.
The static builder below performs the layout calculation
within [value functions](../references/language.md#8-value-functions-and-views).

A successful realization supplies [Representation A](../../lib/std/memory/representation.type.zy),
an existential package `exists (= Stored : VType) . Storage A Stored`.
[Storage A Stored](../../lib/std/memory/storage.type.zy) names its dictionary independently of the opening,
so a consumer can receive the shared carrier and its operations as explicit parameters.
The dictionary contains `size`, `alignment`, and four operations:

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
The [call interface](escape-unboxing.md#stored-call-interfaces) uses the same opening to share stored argument
and result types across separately checked workers, with explicit logical conversion between different carriers.
The [module signature](../../lib/std/memory/package.type.zy) uses an expected existential annotation to prescribe
that abstraction rather than attempting to infer it from the concrete byte implementation.

The current checker cannot derive a total byte decoder from size and layout evidence.
Consequently `load` retains a failure continuation even though these constructors establish its input invariant.
Likewise, two independently opened contracts have no type-level proof that their runtime layouts agree.
These are remaining expressiveness limits: the nominal storage boundary is enforced,
while its byte-level laws are implemented and tested by the library rather than represented as value-dependent proofs.
The public `Storage A Stored` type also permits caller-authored dictionaries.
Its classifier specifies the operations; implementing one carries the same layout-law obligations as a builder.
Reusing a carrier while silently changing its interpretation is not ruled out by a dependent proof.

### Static layout plans

Foreign records and fixed buffer operations often need placement before execution.
The [static builder](../../lib/std/memory/static-layout.zy) answers that requirement using ordinary value functions.
Its [signature](../../lib/std/memory/static-layout.type.zy) hides `Plan A`
and discloses `Layout A = Result (Plan A) Error`.
Constructors have the same composition syntax as the runtime builder: scalar leaves,
`unit`, `padding`, `product`, and `align`.
Each successful plan contains validated placement and the codecs derived from that placement.
Callers cannot introduce a successful plan from a layout-information record.

```zydeco
let make_memory = @(import("memory/static-layout.zy")) in
let (= Plan, = Layout, memory) = builtin |> make_memory in
let record = memory/align (UInt8 * UInt32) 16
  (memory/product UInt8 UInt32 memory/uint8 memory/uint32) in
match record
| +Err(error) => ...
| +Ok(plan) =>
  let shape = memory/inspect (UInt8 * UInt32) plan in
  let (= Stored, repr) = memory/realize (UInt8 * UInt32) plan in
  ...
end
```

`inspect` and `realize` are value functions.
`inspect` returns a [Shape](../../lib/std/memory/shape.zy) with `size`, `alignment`, and `form`.
A scalar form retains its original byte width, padding is a leaf,
and a product form records the right `offset` and both child shapes.
The left offset is zero.
Raising alignment preserves that form, so an over-aligned scalar still exposes its original width
and an over-aligned product retains its field offsets.
For the example, the shape exposes size 16, alignment 16, and right offset 4.
`realize` constructs the usual `Representation A`; it needs no failure continuation
because placement has already been checked.
Its `store` and `from_bytes` operations still perform fallible backing allocation.

[Size calculations](../../lib/std/memory/size.zy) are source-defined value functions returning `Result Int64 Error`.
They check the nonnegative signed range, power-of-two alignment, and overflow,
using only [L8's total integer leaves](../references/language.md#8-value-functions-and-views).
`NegativeSize`, `InvalidAlignment`, and `SizeOverflow` are ordinary constructors
in [Error](../../lib/std/memory/layout-error.zy); callers can handle them with value matches.
Product construction propagates the left error before the right error;
alignment validates the requested boundary before inspecting its input layout.
No allocation or byte operation runs to calculate a plan, including a representable but impractically large plan.

Both builders use the same [descriptor](../../lib/std/memory/descriptor.type.zy)
and [codec implementation](../../lib/std/memory/codec.zy).
They pass completed offsets, gaps, and tails to codecs, so byte writes do not repeat placement arithmetic.
These internal codec constructors assume validated placement; only the public builders expose opaque successful plans.
The common descriptor remains an internal implementation interface, not an independently checked proof
of arbitrary user-supplied codecs.

The [static elimination contract](../references/language.md#10-static-elimination) determines
when a value calculation must resolve.
A runtime size cannot supply a static `padding` calculation; the runtime builder supports that use.
Validated plans themselves can be transported or selected at runtime,
and `inspect` can forward their layout information as ordinary values.
A runtime-selected plan does not thereby supply known integers to a later static calculation.
Neither API executes `Ret` computations during checking.

This is source-level construction evidence, with practical limits.
All plans for one logical `A` have the same `Plan A` type; the type does not distinguish two different placements.
`Shape` is an inspection result, not a dependent proof or a compiler calling-convention descriptor.
It contains no managed-reference map or target register classification.
The [call-boundary proposal](escape-unboxing.md#representation-contracts-at-call-boundaries) owns the
additional evidence required before compiler policies may choose among source-constrained call layouts.
The current Rust representation policies continue to govern only locally justified word representations.

### Layout laws

All sizes and offsets are nonnegative `Int64` values.
Alignment is a positive power of two.
Arithmetic checks the `Int64` bound before adding or rounding; invalid inputs and overflow select `no`
during runtime realization or return a static-construction `Err`, without allocating a payload.
A representable size does not guarantee that storage can be allocated.
Type checking enforces the logical type and abstract storage boundary; the builders implement the arithmetic laws.

The ten scalar leaves use explicit little-endian storage.
Integers occupy their exact declared width, with signed integers using two's complement.
`Float32` and `Float64` occupy their IEEE bit patterns, including signed zero and NaN payloads.
Scalar size and alignment are both the width in bytes.
Each scalar decoder accepts exactly that many bytes.
The [scalar primitives](../../lib/std/builtin/numeric) implement only these leaves.

`unit` has size zero and alignment one.
`padding n : Layout Unit` has size `n` and alignment one, and stores exactly `n` zero octets.
It can occur as a field in an ordinary product layout.
For a product with field sizes and alignments `(left_size, left_alignment)` and `(right_size, right_alignment)`:

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
Alternatively, a [foreign input decoder](c-ffi.md#valid-foreign-output-need-not-be-canonical-storage) can
read the meaningful fields and use `store` to construct canonical storage.

### Address realization and FFI

The sole allocation primitive added for composition is `bytes/aligned`, described
in [L13](../references/language.md#13-primitive-values-and-capabilities).
Numeric size calculation, power-of-two validation, and field placement remain library code,
using value functions or returning computations according to the builder.
Zero-padding construction remains a suspended computation.
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
The [static-plan variant](../../lib/tests/ffi/static-layout.zy) exercises the same C checks
with placement calculated before execution.

### Costs and the next representation boundary

The paired C examples perform the same 64-byte record store, import, and foreign checks.
The [representation comparison tool](../../cli/examples/representations.rs) reports 138 product/closure allocation sites
for `ffi/representation.zy` and 75 for `ffi/static-layout.zy` under the default `Local` policy (137
and 74 under `Shared`).
Placement calculations and their continuation structure disappear in the static variant.
These are generated-code counts, not runtime allocation or speed measurements.
Reproduce them with:

```sh
cargo run --example representations -- lib/tests/ffi/representation.zy lib/tests/ffi/static-layout.zy
```

Layout realization allocates ordinary closure environments.
Storage construction currently creates intermediate buffers and concatenates them; native field decoding copies slices.
Deeply nested composition can therefore copy a payload repeatedly.
Import validation also re-encodes. This implementation establishes the semantics needed
to justify a later builder or offset-based codec without embedding layout policy in the compiler.
It provides physical scalar and product storage, but does not change the tagged word convention for ordinary values,
inline `Stored` in call frames, or offer arbitrary field pointers and mutation.
Native host byte objects still live outside the managed collector and are not reclaimed by it.
The original `store` operation still chooses host allocation internally.
The allocator protocol below makes that choice explicit for buffer construction;
lexical buffer lifetimes remain unexpressed.

Allocator selection and checked mutable destination capabilities are implemented below.
A borrowed region with a statically scoped lifetime remains a separate boundary.
Checked offset access into caller-provided storage is implemented below;
statically verified field paths remain deferred here.
[Representation-aware argument/result contracts](escape-unboxing.md#representation-contracts-at-call-boundaries)
are deferred at their owning boundary, with static evidence, shared entry contracts, and tracing as prerequisites.
Automatically changing all products or adding representation-polymorphic calls first would conflate storage,
ownership, and the native calling convention before their boundaries are expressible.

The [focused tests](../../lang/tests/tests/representation.rs) cover all four execution backends,
exact integer and float bits, padding and size rejection, and abstract-type rejection.
Shared buffer tests check address alignment and retained windows;
the native C test additionally checks the actual foreign borrow.
[Static-plan tests](../../lang/tests/tests/static_layout.rs) check signed-size limits, typed construction errors,
plan abstraction, normalized placement, canonical bytes, and all four execution backends.
The [buffer example](../../lib/tests/std/static-storage-access.zy) uses the inspected size, alignment,
and right offset to allocate and write caller-provided storage, then validates and decodes it through the whole plan.
The offset remains an ordinary integer; checked access does not yet constitute a statically typed field path.

## Mutable destination capabilities

Fixed-capacity destination storage extends the representation boundary with an explicit resource protocol.
The host-owned `Buffer` capability and its operations are declared
in [the Builtin buffer interface](../../lib/std/builtin/system/buffer.zy).
They run in `OS`, whereas immutable byte observations remain returning computations.
A source integer or immutable `Bytes` cannot stand in for a buffer handle.

`allocate size alignment error success` creates zero-initialized storage
with the requested nonnegative size and positive power-of-two alignment.
`write handle offset bytes error done` replaces a checked range without resizing.
`read handle offset length error success` returns a detached immutable snapshot of that range.
An empty range at the end is valid.
Negative or overflowing ranges are rejected before any byte is changed.

`freeze handle error success` produces aligned immutable bytes and closes the handle on success.
`close handle error done` frees mutable storage without producing bytes.
Both transitions invalidate every alias.
Closed handles are never reused, and read, write, freeze, or close through an old alias report `Closed`.
A failed freeze leaves the handle open.
Snapshots and frozen bytes remain immutable after later writes or close.
These are resource-state guarantees, not a static uniqueness or lexical-lifetime claim.

The stable error codes are `InvalidLayout = 0`, `Closed = 1`, `Bounds = 2`,
`AllocationFailed = 3`, and `Uninitialized = 4`.
The last applies to buffers created by the uninitialized memory allocator described below.
Operations on a closed handle report `Closed` before inspecting their range.
Detected allocation and layout failures create no handle; failed writes preserve all bytes.
General host allocator aborts remain outside this fallible protocol, as for immutable storage.
Native/interpreter buffers use real aligned allocations; the Wasm test host retains its opaque-address limitation.
Reads and freeze currently copy, so no mutable foreign alias is introduced.

### Choosing an allocator on the computation stack

[allocation.zy](../../lib/std/memory/allocation.zy) defines an ordinary codata `Allocator`
with an `.allocate` observation.
`Allocate A` is a computation accepting that service, an error continuation, and a result continuation.
`allocate size alignment : Allocate Buffer` requests storage from the supplied service rather
than selecting an allocator inside the compiler.
The heap provider delegates to Builtin; the `limited maximum parent` value function intercepts requests larger
than its per-allocation ceiling and delegates the rest.
This is a size policy, not a cumulative quota or a distinct physical allocator.
A negative ceiling rejects every nonnegative request.

Consumers may supply other source-defined services without changing the host ABI or the layout language.
This makes allocator choice explicit at participating call sites; it does not prevent a program
with Builtin access from calling the heap operation directly.
[The checked example](../../lib/tests/std/buffer.zy) exercises the service, a restrictive provider,
alias invalidation, detached snapshots, and failure without mutation on all backends.

Lexically scoped borrowing and automatic cleanup are deferred.
A thunk may be retained, invoked twice, or invoke its completion continuation zero or multiple times.
A scope-shaped helper cannot derive single invocation or cleanup from these types.
The current extension provides checked close and freeze; an affine completion protocol
or an explicitly handled dynamic scope is needed before a stronger borrow-lifetime claim can be made.

## Access through existing representation contracts

[access.zy](../../lib/std/memory/access.zy) is an extension consuming the existing existential
`Representation A` interface.
It introduces no compiler rule and does not couple the immutable layout builder to `OS` or `Buffer`.

`read_at A R representation source offset no yes` checks a window of the representation's exact size,
validates its canonical contents, and decodes an `A`.
It accepts dynamically supplied representation packages: unpacking occurs inside the body
because the result does not depend on their hidden stored type.
It returns through the caller's `R` stack and can inspect a field of a larger immutable buffer.
Invalid ranges or representations select `no`.

`write_to A representation destination offset value error done` encodes an `A`
and writes it into a caller-provided `Buffer`.
Destination bounds and closed-handle errors are the buffer protocol's errors;
detected temporary storage allocation failure uses `AllocationFailed`.
No destination byte changes unless encoding and bounds checking succeed.
The operation still constructs a temporary encoding, but repeated field writes reuse the destination allocation rather
than reconstructing the whole record.
The caller chooses its capacity and base alignment through an allocator.

A successful write proves that the bytes fit; it does not turn the destination
into `Stored` or prove that the chosen offset is aligned for a field.
Freeze and import through a complete representation when that proof boundary is needed.
Typed field paths relating parent and child layouts remain deferred:
the current types carry no value-dependent offset or layout-equality evidence.
These explicit checked offset operations remain useful without claiming those proofs.

[Access tests](../../lib/tests/std/storage-access.zy) cover field decoding, wrong ranges,
and failed writes preserving other fields on all backends.
[The C construction example](../../lib/tests/ffi/storage-access.zy) creates the existing 64-byte-aligned record
by writing fields into one destination and freezing it before foreign borrowing.
Reads may copy windows, and writes currently allocate temporary encodings;
a direct destination codec is a later optimization that must preserve these failure and canonical-padding contracts.

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

## Addresses, cells, and views

A pointer to a record, a pointer paired with a length, and a pointer whose length lives just
before its payload should share memory operations.
Their differences are source-defined representation choices: which value crosses a boundary,
where its runtime metadata lives, and how that information is obtained.
Length and capacity are examples of runtime metadata; so are strides, tags, allocator handles, and vtable pointers.
There is no fixed compiler record of optional fields.

The [source library](../../lib/std/memory/views.zy) implements these types and constructors
with the existing `VType` and `CType` kinds.
The [native provider](../../lib/std/memory/native.zy) binds them to checked host memory;
[examples](../../lib/tests/std/memory-views.zy) run on the interpreter, AMD64, and both WebAssembly backends.
The [bounded model](../../lib/tests/ffi/views/model.zy) supplies a deterministic alternative provider
for [source composition and phase tests](../../lang/tests/tests/memory_views.rs).
The WebAssembly test host models addresses in its own virtual address space and exposes no C pointer.
The current `Bytes` and `Storage` interfaces still use their existing host-owned immutable storage.

### The primitive boundary

The Builtin provider exposes two abstract value types:

| Type | Meaning | Runtime responsibility |
| --- | --- | --- |
| `Addr : VType` | An opaque data address. It carries no element type, length, capacity, ownership, or permission. | The implemented native address cell occupies one 8-byte pointer slot. Copying an address does not keep its allocation alive; foreign-call transport is a separate extension. |
| `Access : VType` | Authority to access a live allocation or granted range with particular permissions. | The checked implementation identifies an owned allocation and a revocable range grant, checks liveness and bounds, and rejects invalid operations. |

Keep `Access` separate from `Addr`, so a thin external handle can remain one pointer.
A source wrapper can retain both when it should own or retain the resource.
The implemented grants come from `Buffer` owners.
Extending grants to foreign storage will require a trusted binding's explicit extent, permissions, and release contract.
A length read from an arbitrary address cannot grant authority to read that address or its surrounding allocation.
Revocation invalidates every alias of a grant; copying the handle does not duplicate ownership or release rights.
The checked runtime record stores a shared live/revoked grant, an allocation identity
and range, and read/write permissions.
The buffer owner controls allocation lifetime; a future foreign owner must supply its release policy.
Those fields belong to the provider's abstract grant representation, not to every raw pointer.

The provider takes an explicit `Access` on every memory operation.
It checks the addressed range, required alignment, initialization and leaf representation before exposing a value.
Offsetting checks signed displacement without overflow and stays within the granted allocation,
including its one-past address; a subsequent nonempty load rejects one-past access.
A negative offset is therefore valid when the grant includes the header before the payload.
Loading an address from a pointer slot checks that slot; accessing its target requires a suitable target grant.
Addresses remain pointer values through native loads and stores; generic integer casts
or byte codecs do not manufacture pointer validity or a foreign grant.

The source read interface is:

```zydeco
let Memory (R : CType) = codata
| .check : Access -> Addr -> Int64 -> Int64 -> Thk (Fault -> R) -> Thk R -> R
| .load_u8 : Access -> Addr -> Thk (Fault -> R) -> Thk (UInt8 -> R) -> R
| .offset : Access -> Addr -> Int64 -> Thk (Fault -> R) -> Thk (Addr -> R) -> R
| .load_i64 : Access -> Addr -> Thk (Fault -> R) -> Thk (Int64 -> R) -> R
| .load_addr : Access -> Addr -> Thk (Fault -> R) -> Thk (Addr -> R) -> R
end in
let Mem (A : VType) (R : CType) =
  Thk (Memory R) -> Thk (Fault -> R) -> Thk (A -> R) -> R in
...
```

`Mem A R` abbreviates a computation supplied with a memory provider and failure/success continuations.
Its answer protocol `R` belongs to the caller.
It does not promise purity, termination, or one invocation.
The provider must validate a primitive access before performing it and reporting success.
The faults are ordinary source constructors `Closed`, `Bounds`, `Permission`, `Overflow`,
`Alignment`, `Uninitialized`, `InvalidValue`, `Unavailable`, and `AllocationFailed`.
The integer model implements only a bounded read space and uses `Bounds` for displacements outside that space.
The native provider checks initialization for every loaded leaf.
Its `check` operation validates the complete footprint and alignment without reading padding
or requiring padding bytes to be initialized.

The native module also exports `allocate`, `grant`, `base`, `revoke`,
and the typed operations `store_i64`, `store_u8`, and `store_addr`.
Allocation returns a `Buffer` owner and starts uninitialized; the existing `buffer/allocate` remains zero-initializing.
`grant R owner offset length permission no yes` creates a range grant
with ordinary source permissions `Read`, `Write`, or `ReadWrite`.
A grant does not embed itself in an address. `base` obtains the grant's first address,
and `revoke` invalidates every copy of that grant without closing the allocation or independent grants.
Closing or successfully freezing the buffer invalidates all its grants and addresses.
Failed writes leave bytes, initialization information, and pointer slots unchanged.
Freezing an incompletely initialized buffer reports buffer error 4 and preserves the live owner.

Integer and byte stores initialize their footprint.
Pointer stores additionally record the target's allocation identity and offset
and write the native pointer into the slot.
Any overlapping byte or scalar store removes that pointer information, even when it writes identical bits.
Loading such a slot as an address reports `InvalidValue`;
loading a properly stored address preserves the target identity, including after that target has closed.
A later target access checks its own grant and reports `Closed`.
The shared [runtime model](../../lang/machine/src/memory.rs) owns these checks for the interpreter and AMD64.
Generic runtime address values are opaque handles into that model;
an explicit address cell has the separate 8-byte native storage representation.
No new kind or compiler rule recognizes thin, fat, or header views.

This is a checked capability design.
It makes no claim that current typing proves pointer lifetimes or that all checks erase.
Static region retirement and transitive support have their separate owner
in [reachability regions](reachability-regions.typ);
adopting that system would change how access evidence is discharged.
`Ret A` remains an installed continuation accepting `A`; it supplies neither a memory lifetime nor cleanup scope.

### Cells describe storage; views interpret handles

`Cell A` describes a fixed memory representation of an `A` and a computation that reads it:

```zydeco
let Cell (A : VType) =
    (#size :: Int64)
  * (#alignment :: Int64)
  * (#read :: Thk (forall (R : CType) . Access -> Addr -> Mem A R)) in
let Fat (RuntimeMetadata : VType) =
  (#address :: Addr) * (#runtime_metadata :: RuntimeMetadata) in
let View (Handle : VType) (RuntimeMetadata : VType) =
    (#carrier :: Cell Handle)
  * (#open :: Thk (forall (R : CType) . Access -> Handle -> Mem (Addr * RuntimeMetadata) R)) in
...
```

The three questions have different answers: `Handle` is the value being passed,
`Cell Handle` describes its explicit stored form, and `open` obtains a payload address
and runtime metadata from that handle.
A view descriptor is an ordinary reusable dictionary; it need not be stored inside each handle.
`Fat M` permits any representable `M`.
Its fields are logical source fields until a `Cell (Fat M)` or call adapter supplies physical placement.
A source product alone does not promise adjacent native words.

Cell construction follows the existing layout laws: nonnegative size,
power-of-two alignment, checked rounding, and checked addition.
The complete cell size includes tail padding and is its array-element stride.
Product construction places the second cell at `round_up(left.size, right.alignment)`
and rounds the complete size to the larger alignment.
The library reuses the existing [size value functions](../../lib/std/memory/size.zy) for those calculations.
Before reading any field, a product validates its complete size and alignment through `Memory.check`.
It then reads fields through their cells, leaving padding uninterpreted.
`padding count` constructs a `Cell Unit` with size `count` and alignment one;
its read checks the footprint and returns unit without loading bytes.
`align A boundary cell` raises alignment to the larger of `boundary` and the cell's current alignment,
rounds its size accordingly, and preserves every field offset.
It validates that new footprint before delegating to the original cell.
Thus a small field cannot make an out-of-bounds padded or misaligned enclosing cell succeed.
All three constructors return `Result (Cell A) LayoutError` using source value calculations.
The read interface does not grant mutation permission; native stores require a writable access grant.

As with `Storage`, the public dictionary type does not prove its size, alignment, and decoder agree.
Caller-authored cells must satisfy those laws; checked builders can hide successful layouts behind package abstraction.
All cells for `A` share `Cell A`; this is not a type index distinguishing their placements.
The supplied native leaves are `UInt8` (size/alignment 1), `Int64` (8),
and an address cell (8) for the current 64-bit native target.
The integer model uses that same sample format.
Additional primitive formats can extend the provider without changing the view constructors.
Native address and code-pointer cells cannot be obtained by serializing an integer through `Bytes`:
the existing portable byte contract has a different carrier and validity boundary.

### Concrete view forms

The source constructors use the following representations.
`p` is the supplied handle address, and offsets are byte displacements checked by the provider.

| Form | `Handle` | `RuntimeMetadata` | `open` behavior |
| --- | --- | --- | --- |
| Thin | `Addr` | `Unit` | Return `(p, ())`; no memory operation. |
| Fat length | `Fat Int64` | `Int64` | Project `(handle/address, handle/runtime_metadata)`; no memory operation. |
| Fat length and capacity | `Fat ((#length :: Int64) * (#capacity :: Int64))` | The named pair | Project the carried record; no memory operation. |
| Prefix header | `Addr` | Any `M` with a `Cell M` | Read `M` at `p - header_delta`, return payload `p`. |
| Inline header | `Addr` | Any `M` with a `Cell M` | Read `M` at `p`, return payload `p + payload_offset`. |
| Object header | `Addr` | `Addr`, for a vtable slot | Read the slot at `p`, return the original object address and the vtable address. |

The last three are applications of one source constructor:

```zydeco
indirect M pointer_cell runtime_cell runtime_offset data_offset
```

It offsets to the runtime metadata, reads it through `runtime_cell`, offsets to the payload,
and invokes success only after these operations succeed.
A runtime metadata cell can itself be a product; following more
than one indirection is another ordinary `open` computation.
The caller supplies the original allocation grant, so prefix recovery does not attempt
to validate itself using the header it is about to read.

On the model's chosen 64-bit format, a thin handle occupies 8 bytes,
a fat length handle 16, and a fat length/capacity handle 24.
A prefix or inline-header handle still occupies 8 bytes; the runtime metadata resides in the referenced allocation.
Header size, payload alignment, and the position of an embedded pointer are choices of the source cell plan.
COM-style object access first loads a vtable address, then uses a separate table cell and access grant
to load a method pointer; invocation is a foreign-call operation, not a data load.

These layouts cover familiar external formats without baking their conventions into `View`.
A [BSTR](https://learn.microsoft.com/en-us/previous-versions/windows/desktop/automat/bstr) has a
four-byte byte-length prefix before its character pointer; its length excludes the terminating character.
A BSTR binding must also preserve its allocation/release convention and distinguish byte counts from character counts.
A [COM interface](https://learn.microsoft.com/en-us/office/client-developer/outlook/mapi/implementing-objects-in-c)
starts with a vtable pointer and supplies the interface pointer as the method's first argument.
The model uses 8-byte sample headers and does not implement either ABI.

### Typed pointers, slices, and immutable bytes

The typed operation layer receives a cell for the element it accesses:

```text
read_at : forall A R. Cell A -> Access -> Addr -> Mem A R
index  : forall H A R. View H Int64 -> Cell A -> Access -> H -> Int64 -> Mem A R
```

`read_at` validates the selected cell's complete footprint and then delegates to its reader.
`index` opens the handle, checks `0 <= index < length`, checks multiplication by the element stride for overflow,
offsets within the grant, and reads through the element cell.
Thus a slice length counts elements, and its stride comes from `Cell A`.
For byte slices the element is `UInt8` with stride one.
Runtime checks and numeric arithmetic are computations; the length does not become a dependent integer index such
as `Slice A n`.

The source factories `pointer A carrier element` and `slice H A view element` export an abstract `Ptr`
or `Slice` together with operations specialized to the chosen element cell.
The pointer factory provides `from_address`, `address_of`, `pointer_cell`, and `get`;
the slice factory provides `from_handle`, `handle_of`, `slice_cell`, and indexed `get`.
Their expected existential signatures hide the selected handle representation
while sharing its witness with the returned operations.
For example, `let (= Slice, slices) = views/slice H A view element in ...` opens the slice factory once,
and `slices/get` receives that opening's `Slice`.
Constructing a wrapper only preserves the handle; access still validates the grant when `get` runs.
This binds the chosen representation to an API without a compiler builtin for `Slice`.
For a concrete instance, `Handle` may be `Addr`, `Fat Int64`, or a retained pair containing an owner.
Clients that need to select different handle types dynamically package the handle
with its matching operations: `exists (= H : VType) . H * View H Int64 * Cell A`.
Clients sharing one `H` can select a view at runtime directly.

Capacity has a separate meaning from length. A growable container's source API validates `0 <= length <= capacity`,
manages initialized elements, and supplies a writable grant for mutations.
Copying its runtime metadata proves none of those facts and does not authorize a write.
The same separation supports runtime strides, allocator records, and application-specific tags.

`Bytes` should be an ordinary std abstraction combining a byte-slice handle with a retained immutable owner.
Its length, bounds checks, slicing, comparison, singleton construction, concatenation,
and collection conversions are source algorithms over these layers.
Compiler recognition of a universal `Bytes` layout is unnecessary; an FFI adapter should choose its pointer
and length transport explicitly.

The remaining primitive question is how an allocation becomes an immutable owner.
A read-only grant over a mutable buffer is insufficient: another grant or owner alias can still write or close it.
A freeze transition must invalidate writable aliases and transfer the storage
to an immutable owner retained by every shared slice.
Copying foreign mutable storage is another valid way to establish this invariant.
Initialization checks remain at the primitive boundary, and no wrapper can establish pointer provenance
by decoding ordinary octets.

Moving the current builtin implementation therefore requires one coordinated migration:
define the std type and algorithms; have codecs, UTF-8 conversion, and I/O exchange generic storage windows;
and replace the special `Bytes -> (pointer, length)` foreign classifier with an explicit view adapter.
Remove the old byte roles and compiler primitive in that same change.
The present memory provider supplies checked owned mutable storage; it does not
yet supply the retained immutable-owner transition.
Existing builtin `Bytes` remains implemented until that ownership and caller migration is complete,
with no parallel public std replacement.

### Compile-time and runtime behavior

The existing [value-function](../references/language.md#8-value-functions-and-views)
and [static-elimination](../references/language.md#10-static-elimination) rules remain authoritative.
The following table applies those rules to memory views:

| Expression or information | During checking | At runtime |
| --- | --- | --- |
| `Addr`, `Access`, `M`, and `H` | Check ordinary kinds, types, and package witnesses. | Types, witnesses, and field labels erase; their values remain as needed. |
| Fixed cell size, alignment, and product offsets | Value arithmetic requires known operands and checks its ordinary error result. | A retained descriptor may carry those calculated integers and read thunks. |
| Fat-handle construction and runtime metadata projection | A value function may forward unknown runtime fields inside known structure. | The residual program constructs or projects ordinary values; it contains no value-function closure. |
| Thin/fat `open` | Check the suspended computation; never force it to discover static information. | Invoke success with carried fields, without accessing memory. |
| Header recovery and element indexing | Check types and operation protocols. Runtime lengths cannot drive static arithmetic. | Execute checked offsets, loads, and numeric computations through the supplied provider. |
| Runtime-selected view or cell | Check that the selected values have a common type, or open an existential package. | Keep required dictionaries, offsets, and captured values. Selection does not make their integers statically known. |
| Explicit native ABI layout | Require a known target leaf layout and argument/result transport plan. | Apply the validated marshalling plan to runtime payloads. |

Runtime metadata names the information's role in a representation, not a requirement that it be unknown during checking.
A literal length may fold away while still describing that representation's runtime metadata.
Conversely, ordinary layout descriptions may be constructed and selected at runtime.
Neither is a [meta annotation](../references/language.md#meta-annotations-compile-time-metadata).

An `open` result is a snapshot of the observations its computation made.
It neither freezes the referenced allocation nor promises an atomic snapshot of a mutable multifield header.
Shared mutable runtime metadata needs its own synchronization protocol, and later accesses recheck their grants.

### Implementation boundary and next steps

The source library implements thin/fat projection, prefix and inline-header recovery, object-header loads,
padding, alignment, product cells, typed indexing, and abstract pointer/slice factories.
Source tests pair successful construction and access with layout, bounds, phase, and type rejections.
[Native view](../../lib/tests/std/memory-views.zy)
and [fault](../../lib/tests/std/memory-faults.zy) examples exercise checked owned memory on all four backends.
The Rust model also verifies allocation identity, revocation, initialization, pointer-slot invalidation,
and failure-before-mutation invariants.

The next ownership boundary is the immutable owner needed to move `Bytes` into std, described above.
The [foreign adapter](c-ffi.md#source-defined-views-at-foreign-boundaries) separately specifies whether
an address-bearing handle supplies one pointer, several scalar arguments, or an aggregate by value.
Foreign grants and native `Addr` arguments are not yet part of that implemented ABI.
Data addresses and callable code require different leaves: a proposed `Code S : VType` is indexed
by an abstract static foreign-signature witness `S : VType`, and uses the matching call adapter.
A code address is not a `Thk`, which may capture an environment,
and a computation classifier alone does not determine the target calling convention.
Code-pointer loading and callbacks follow the FFI design.

## Remaining questions

- Should ordered collections receive a three-way primitive comparison, or derive a library `Order` value?
- Which workloads justify a non-contiguous representation or native shared-window ownership?
- How should native text and byte storage be reclaimed alongside, or separately from, the managed heap?
  [Native memory](native-frames.md#collection-and-space-behavior) supplies the surrounding lifetime constraints.
