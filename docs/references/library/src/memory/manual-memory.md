# Manual Memory and Storage

## Manual Memory

Systems code needs to choose storage, initialize it, and release it explicitly.
[std/memory](../../../../../lib/std/memory/package.zy) provides that interface using ordinary packed values,
abstract type witnesses, and [CPS](../../../language/src/computations-and-control.md#ret-and-explicit-cps).
It introduces no lifetimes, borrow checker, affine types, or special continuation kind.

The compiler supplies `Addr`, existing scalars, `Ret`, `Thk`, and the primitive memory operations.
`Addr` is an unmanaged data address; it carries no allocation identity, extent, permission, or initialization flag.
The library supplies the other types:

| Type | Purpose | Runtime payload |
| --- | --- | --- |
| `Uninit`, `Init`, `Fields S T` | Whole-object and partial-record initialization states | None |
| `Ptr L S` | An address interpreted with layout witness `L` and state `S` | One `Addr`; no wrapper or state tag |
| `Slice L S` | Contiguous elements with an explicit dynamic extent | Pointer and element count |
| `Field P C`, `DynamicField P C` | A parent/child layout path | Fixed recipe erases; dynamic path carries one offset |
| `views/View P H A`, `views/DynamicView P H A` | Handle interpretation | Fixed recipe erases; dynamic operation is an ordinary thunk |
| `fixed/Layout A` | A static `Either LayoutError (Plan A)` | Construction evidence eliminates before fixed realization |
| `Representation A` | An existential layout witness with separate storage and codec components | Fixed recipes erase; dynamic counterparts carry geometry and operation thunks |
| `dynamic/Layout A` | Runtime layout computation | Captured dynamic inputs and operations |
| `Storage L`, `DynamicStorage L` | Validated geometry independent of logical values | Fixed recipe erases; dynamic geometry carries size/alignment |
| `Codec L A`, `DynamicCodec L A` | Initialization and observation recipes | Fixed recipes erase; dynamic codecs carry ordinary thunks/context |
| `StaticAlloc Context`, `Alloc` | Fixed and runtime allocation services | Explicit context; runtime interface carries ordinary operations |
| `Buffer` | Incremental byte construction | Address, capacity, initialized-prefix length |

The table describes payload requirements. Products, closures, and runtime-selected dictionaries still
use the [ordinary representation policy](../../../compiler/src/representation-and-scalar-regions.md).
CPS alone does not guarantee allocation-free callbacks or stack frames.
`Buffer` builds bytes; the [array factory](layouts-and-aggregates.md#array-storage-and-element-builders)
also provides generic element builders.
No `Frozen` state is needed for this manual interface.

### Unsafe Obligations

A library field named `unsafe` documents caller obligations; it is not a language keyword or a checked effect.
Raw addresses and typed pointers may be copied freely.
The caller establishes lifetime, allocation extent, initialized fields, valid scalar
or pointer representations, alias discipline, and synchronization before each access.
Use of a freed pointer or stale state alias is a contract violation, with no required recoverable runtime fault.
The native and interpreter accesses perform no grant lookup or initialized-byte tracking.

An ordinary `Thk R` may escape, be dropped, or be forced repeatedly.
CPS successors therefore establish neither unique ownership nor guaranteed cleanup.
After invoking a completion that releases or publishes storage, a callback must not use an old mutable alias.
A caller that abandons cleanup leaks its manually allocated storage.
There are no implicit destructors.
Raw storage must not contain movable managed references without a separate rooting protocol;
the supplied layouts cover scalars and unmanaged addresses.

## Independent Storage and Codecs

[Storage](../../../../../lib/std/memory/storage.zy) validates geometry
without selecting a logical value type or allocator.
`storage/constant/create size alignment` returns `Either LayoutError (exists L. Storage L)`;
its runtime counterpart returns `Ret (Either LayoutError (exists L. DynamicStorage L))`.
Both reject negative sizes and nonpositive or non-power-of-two alignments.
The fixed constructor requires static operands. Both representations are sealed against unchecked construction;
`unsafe/for_layout L` validates the same geometry but lets the caller associate it with an existing witness.
`query L` exposes `SizeAlign = (#size :: Int) * (#alignment :: Int)`.
`constant/materialize L` explicitly obtains dynamic geometry from fixed storage.

A witness relates geometry, pointers, and codecs, not an allocation instance.
Independently opened witnesses remain distinct even with equal geometry.
For dynamic instances, the caller retains the matching geometry until release.
Size and indexing remain bounded by nonnegative `Int`; full pointer-width sizes remain proposed work.

[Codec](../../../../../lib/std/memory/codecs.zy) supplies value functions `init pointer value : Thk (Cps (Ptr L Init))`
and `read pointer : Thk (Cps A)`.
The [dynamic form](../../../../../lib/std/memory/codecs.zy) carries ordinary thunks:
`init : Thk (Ptr L Uninit -> A -> Cps (Ptr L Init))` and `read : Thk (Ptr L Init -> Cps A)`.
`codecs/materialize L A` selects that form explicitly.
`codecs/unsafe/scalar L A write read` associates scalar accesses with a caller-established layout.
`codecs/unsafe/take` reads through a fixed codec before yielding an uninitialized pointer
and the value; `dynamic_take` does the same through a dynamic codec.
Neither clears bytes nor invalidates aliases. A codec carries no implicit allocator.
Its stored format is independent of the compiler's representation of `A`.

[StaticAlloc](../../../../../lib/std/memory/allocation.zy) selects allocator code using value functions,
with explicit `Context`, byte size, and alignment; its `free` also takes the original base.
`allocation/static_heap` uses `Unit` context, and `materialize Context allocator context` produces `Alloc`.
`reserve L Context allocator context storage` produces a CPS allocation recipe yielding `Ptr L Uninit`;
`unsafe/release` requires matching geometry, provider/context, and an uninitialized pointer.
`dynamic_reserve` and `unsafe/dynamic_release` use `Alloc` and `DynamicStorage` at runtime.
Fixed and dynamic layout conveniences expose these components separately under the same layout witness.
Record and array factories compose storage; callers select logical conversion only when needed.
`codecs/product` combines child codecs through matching field paths.
Its lower-level `unsafe/product_at` and `unsafe/relabel` recipes require the caller
to establish valid nonoverlapping offsets and a compatible parent layout; they perform no geometry validation.

## Allocation and Release

The [allocator interface](../../../../../lib/std/memory/allocation.zy) receives byte size and alignment explicitly.
`allocation/heap` uses the host allocator; `allocation/limited maximum parent` rejects oversized requests
before calling its parent.
Runtime selection of an allocator is ordinary source dispatch.
There is no hidden allocator field on a pointer.

The raw [memory adapter](../../../../../lib/std/memory/native.zy) exposes allocation and an `unsafe` operation group.
Allocation validates nonnegative size, positive power-of-two alignment, and a representable rounded extent;
invalid requests report `InvalidLayout`, and reservation failures report `AllocationFailed`.
The resulting storage is uninitialized.
Zero-sized allocations require no backing bytes and cannot be dereferenced.
Release receives the original base, exact requested size and alignment, and matching allocator.
It must occur exactly once, after the caller has settled the initialized contents and all uses of aliases.

`null` and signed wrapping `offset` are pure address calculations returning through `Ret`.
Dereference validity is a separate obligation; an offset calculation does not prove bounds.
`load_addr` and `store_addr` access unmanaged pointer slots.
Scalar `load_le` and `store_le` access the exact little-endian width without requiring natural alignment.
`copy source destination count` permits overlapping ranges; `fill address count octet` writes a byte pattern.
Valid raw loads, stores, copies, and fills use only a success/completion continuation.
There is no runtime initialization check and no implicit initialization of padding.

## Typed Pointers and Slices

Selecting a fixed representation produces `exists L. (#storage :: Storage L) * (#codec :: Codec L A)`.
Its dynamic counterpart substitutes `DynamicStorage` and `DynamicCodec` in that same packed value shape.
The [representation alias](../../../../../lib/std/memory/codecs.zy) is parameterized by these component families.
[Ptr](../../../../../lib/std/memory/types.zy) privately aliases `Addr` and has no runtime wrapper.
Allocate and release using the selected storage and provider; initialize, observe, or take using the selected codec.
The [independent interfaces](#independent-storage-and-codecs) give the exact call order and protocols.

`init` writes directly into the supplied destination.
`read` copies a logical value without changing its state.
`take` reads the value and supplies a pointer interpreted as uninitialized; it does not clear the storage.
These types reject reading an uninitialized pointer, initializing an initialized one,
freeing through the wrong state, and interchanging independently opened layouts.
They do not invalidate the old pointer or consume any aliases.
In particular, copying a pointer before a transition can leave a well-typed stale alias.

`pointer/unsafe/address L S` exposes the address; `from_address L S` asserts an interpretation.
The latter requires the caller to establish layout, initialization state, extent, and lifetime.
It supplies the explicit boundary for foreign memory and manually calculated field pointers.
No typed field-path or partial-record tracker is implied by an integer offset.

The [slice operations](../../../../../lib/std/memory/slice.zy) share these pointer and state types.
`from_parts` checks a nonnegative count and wraps a caller-validated pointer and extent.
`slices/for_layout L storage` selects fixed geometry; `for_dynamic_layout L storage` selects dynamic geometry.
Each supplies `unsafe/at`, derives stride by rounding size up to alignment,
checks the index against the count, and rejects rounding or multiplication overflow before offsetting,
using the shared [counted-view access](views-and-buffers.md#memory-views) implementation.
Zero-sized elements have zero stride. The resulting pointer keeps the same `L` and `S`.
The caller supplies an allocation covering the elements; bounds checks do not verify that assertion.
A raw slice does not retain its allocation.
See the [standard library guide](../../../../../lib/std/README.md#explicit-storage) for worked storage examples.
