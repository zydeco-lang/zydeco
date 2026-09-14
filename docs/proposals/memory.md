# Static layouts and general memory views

The [implemented memory interface](../references/language.md#manual-memory) supplies manual allocation,
typed pointers, and fixed or dynamic layout recipes.
General views should let those recipes serve thin, fat, header-based,
and indirect handles while making each representation's costs explicit.
This proposal restores that generality and adds typed field access and partial construction.
It uses ordinary source packages and value functions, without lifetimes, affine types, or a new effect system.
The interfaces below are proposed; the checked example verifies their underlying language forms only.

## Storage and handles answer different questions

Keep three choices independently composable:

| Choice | Description | Where information belongs |
| --- | --- | --- |
| Storage layout | Field placement, padding, alignment, and typed reads/writes | Fixed source recipe, or an explicitly dynamic description |
| Handle `H` | The value passed by a caller | Exactly the selected address and runtime metadata |
| View | How an `H` yields a payload pointer and observations | Fixed source recipe, or explicitly retained operations |

`Representation A` continues to introduce the layout witness `L` used by `Ptr L S`.
A view commonly produces `Ptr L S * M`, where `M` is runtime metadata such as length, capacity, or a vtable address.
Its output may also be another handle, allowing several indirections to compose.
The handle's own storage representation is a separate layout:
a logical pointer/length product does not specify adjacent native words or a C calling convention.

A view neither owns storage nor supplies the allocator, original allocation base, or exact release layout.
Those remain explicit at the allocation boundary, especially when the handle points inside an allocation.
The existing [unsafe obligations](../references/language.md#unsafe-obligations) apply to every memory observation.

## Fixed views and computation protocols

A fixed view is a value function that produces the computation for a particular handle.
Its result protocol is an ordinary type argument: `Ret` for pure calculations and `Cps` for memory observations.
This makes recipe selection subject to existing [static elimination](../references/language.md#10-static-elimination).
The following complete example defines those types and the pure-to-CPS adapter:

```zydeco check
begin
  let VType = @(intrinsic(vtype)) in
  let CType = @(intrinsic(ctype)) in
  let Thk = @(intrinsic(thk)) in
  let Ret = @(intrinsic(ret)) in
  let Int64 = @(intrinsic(i64)) in
  let Cps (A : VType) = forall (R : CType) . Thk (A -> R) -> R in
  let View (P : VType -> CType) (H : VType) (A : VType) =
    val pi (handle : H) . Thk (P A)
  in
  let val as_cps (H : VType) (A : VType) (view : View Ret H A) : View Cps H A =
    val handle => { fn R yes => do value <- ! (view handle); ! yes value }
  in
  let val identity (value : Int64) : Thk (Ret Int64) = { ret value } in
  ! ((as_cps Int64 Int64 identity) 7) (Ret Int64) { fn value => ret value }
end
```

`! (view handle)` executes the specialized operation; a CPS view then receives `R` and its successor.
Applying the value function never performs a load.
Runtime arguments remain inside the residual computation.
The view itself cannot survive as an unknown runtime function under this interface.
Fixed selection does not make runtime handle fields or captured context constant.
The fixed geometry constructors separately require known placement operands;
metadata read by the resulting operation remains runtime data.

An unknown fixed recipe cannot be passed through an ordinary computation argument:

```zydeco reject=tyck.static-elimination at=6:3
let Thk = @(intrinsic(thk)) in
let Ret = @(intrinsic(ret)) in
let Int64 = @(intrinsic(i64)) in
let View = val pi (value : Int64) . Thk (Ret Int64) in
let use : Thk (View -> Int64 -> Ret Int64) = {
  fn view value => ! (view value)
} in
! use (val value => { ret value }) 7
```

For deliberate runtime selection, `dynamic/View P H A = Thk (H -> P A)` retains an ordinary operation
and any captured offsets or context.
A `materialize` adapter converts a fixed view to that interface explicitly.
Both forms use the same handle; neither appends a dictionary to it automatically.
Different dynamic handle types can be packaged as `exists H. H * dynamic/View P H A`.

Provide identity, input adaptation, result mapping, and sequential composition as source combinators.
Pure composition keeps `Ret`; composition containing a memory read uses CPS, lifting pure steps with `as_cps`.
There is no public adapter that labels an arbitrary CPS view pure.
As in the [Ret/CPS convention](../references/language.md#ret-and-explicit-cps), programmers remain responsible
for hidden effects; these interfaces do not authorize compiler reordering based on `Ret` alone.

Fallible observations use an explicit protocol `Checked E A = forall R. Thk (E -> R) -> Thk (A -> R) -> R`.
For example, a checked header view can reject a negative length before yielding a slice.
Valid raw loads use only success; no view promises to recover from a dangling or otherwise invalid address.

## General handle forms

| Constructor | Handle payload | Observations | Protocol |
| --- | --- | --- | --- |
| `thin` | Typed pointer | Same pointer and `Unit` | `Ret` |
| `fat` | Typed pointer and arbitrary `M` | Carried pointer and `M` | `Ret` |
| `inline` | Header address | Load metadata fields; offset to payload | CPS |
| `prefix` | Payload address | Offset backwards and load metadata; preserve payload | CPS |
| `indirect` | Address of a pointer slot | Load the pointer, then apply another view | CPS |

Inline and prefix forms specialize one constructor with a metadata layout,
signed metadata displacement, and payload displacement.
An object-header view is the same operation with an unmanaged address as metadata;
invoking a code pointer remains a separate [FFI extension](c-ffi.md).
Metadata may be a product, and composition can follow any finite statically described chain of pointer loads.
Runtime-dependent traversal uses ordinary computation recursion.

Thin and fat views preserve an existing pointer's `L` and `S`.
A header factory exports an abstract handle family `H S`, privately backed by an address,
with an `unsafe` constructor establishing initialized header fields and the payload interpretation `S`.
Header initialization is independent of payload initialization:
finding an uninitialized destination must not require claiming the entire object is initialized.
An `Init` slice interpretation covers only its stated element extent, not unused capacity.
Reading a numeric header cannot establish allocation validity or turn arbitrary bytes into initialization evidence.

An opening is a snapshot. Later header changes do not update an earlier fat handle or returned length.
Reopen when a fresh observation is required; synchronization and alias discipline remain caller obligations.
Release uses the original allocation description, rather than trusting a possibly changed length header.

## Compile-time placement and typed fields

Extend the fixed layout constructors with named records, explicit field offsets,
fixed arrays, and header/payload composition.
Reuse the existing checked [layout laws](../references/language.md#layout-laws):
size and alignment arithmetic must resolve during source elaboration.

- Record fields expose typed paths as ordinary named package fields.
  An explicit-offset builder validates alignment, non-overlap, and the complete enclosing extent;
  gaps and tail padding reserve bytes without causing reads or writes.
- A `Field Parent Child` describes a path between witnesses from the same layout construction.
  Its pure projections preserve whole-object `Init` or `Uninit`; composing paths adds their constant offsets.
  The recipe is selected statically and is not an offset field stored on each pointer.
- A fixed array factory receives a known element layout and count and introduces a fresh array-layout witness.
  Its handle is one address; its bound and stride specialize the access operations.
  This needs neither integer-indexed types nor a materialized tuple of all elements.
  A runtime index still needs arithmetic and, for checked access, a bounds test against the constant bound.
- Header composition derives metadata and payload paths from the finalized enclosing layout.
  Prefix recovery uses that same placement, including alignment padding.

Field-producing constructors must export parent and child witnesses, paths,
and matching operations through shared package openings.
The current opaque `Plan A` alone does not expose this relationship;
extend constructor results with the necessary field packages.
Concretely, a product factory takes opened child layout packages with witnesses `Left` and `Right`
and returns a fresh `Parent`, its representation operations, `left : Field Parent Left`,
`right : Field Parent Right`, and its partial-state operations.
The field recipes use those same child operations; realizing another child package is not a substitute.
An untyped `inspect` result must not be used to forge a typed field path,
and independently realized equivalent layouts do not acquire interchangeable witnesses.
Nested source records provide names and structure without compiler reflection or a new row system.
Paths establish a layout relationship, not the identity or validity of an individual allocation.

Fixed constants belong to the recipe: a static array bound uses a thin view with `M = Unit`.
A runtime count belongs in a fat handle, a header, or explicitly supplied context.
If placement depends on a runtime value, use `dynamic` layout realization and retain the resulting facts only
in the operations or handle components that need them.
Fixed realization must reject unknown placement operands instead of silently retaining them.
Neither fixed nor dynamic selection requires repeating the layout on every element pointer.

## Partial initialization

A product factory also exposes the erased state constructor `Fields LeftState RightState`.
For a parent `P` whose left value type is `A`, representative operations have these schematic shapes:

```text
left/init : Ptr P (Fields Uninit SR) -> A
         -> Thk (Ptr P (Fields Init SR) -> R) -> R
left/take : Ptr P (Fields Init SR)
         -> Thk (Ptr P (Fields Uninit SR) -> A -> R) -> R
finish    : Ptr P (Fields Init Init) -> Ptr P Init
```

`R` and the untouched sibling state `SR` are universally quantified.
`finish` is an erased value conversion; corresponding conversions open whole `Init`/`Uninit`
as uniformly initialized/uninitialized fields and close all-uninitialized fields for release.
Field initialization and taking perform direct memory operations before invoking the successor.
Nested records use nested `Fields`; a field operation preserves the states of every unaffected sibling.
Padding contributes no field state and does not become initialized when `finish` succeeds.

The field-specific accessors project the appropriate child state.
An arbitrary path must not copy a parent's entire `Fields` state onto one child.
This is ordinary polymorphism over explicitly named state parameters, not type-level integer arithmetic.
Copied parent or child pointers can still become stale; no operation consumes aliases or guarantees cleanup.

For arrays with runtime-dependent partial progress, use a builder with an initialized-prefix count.
`init_each` can deliver whole-array `Init` only after completing all elements;
its explicit failure successor supplies the completed prefix so the caller can settle those elements
and release storage.
Before reporting failure, an element callback must settle its current element and return it as `Uninit`;
that element does not advance the prefix.
Dropping or abandoning a continuation provides no automatic notification.
The builder retains capacity and progress only where they are dynamic. Growth and the memory-backed writer remain
in the [stream proposal](filesystem.md#memory-backed-writer-and-byte-builder).

## Example: two handles for the same allocation

Consider a record containing an `Int64` visible length and four `UInt32` elements, with the payload aligned to 16 bytes.
Its fixed recipe calculates:

| Component | Offset | Extent |
| --- | --- | --- |
| Length | 0 | 8 bytes |
| Padding | 8 | 8 bytes |
| Four elements | 16 | 16 bytes, element stride 4 |
| Complete allocation | 0 | 32 bytes, alignment 16 |

An inline handle is the allocation base. A prefix handle is `base + 16`.
The two views load the same length field, validate `0 <= length <= 4`,
and supply the same payload pointer and visible element count through CPS.
The displacement, capacity, stride, allocation size, and alignment are compile-time facts;
only the chosen address and observed length are runtime values.
The caller establishes that the exposed elements are initialized.
Typed operations never inspect or initialize the padding implicitly.

A fixed full-array view can instead expose all four initialized elements from one pointer without a length field.
A dynamic-capacity version performs checked extent calculation at runtime
and preserves the original allocation facts for release.
These choices do not change the element representation or require a different allocator API.

## Implementation and acceptance

Implement the fixed/dynamic view interfaces and thin, fat, header, and indirect constructors first.
Next extend layout factories with typed fields and fixed arrays, then add partial-field and element-builder protocols.
The present `Slice L S` becomes the counted fat specialization of the shared slice-access factory;
its representation stays pointer plus count, and its indexing derives stride from the selected element layout.
Migrate its callers and remove the superseded explicit-stride indexing implementation in that change.
Retained `Bytes` keep their existing ownership contract; exposing their storage uses the existing unsafe boundary.

All new types and combinators are std definitions over existing primitives and package witnesses.
Additional targets still need compiler-supplied pointer size/alignment facts.
Managed-reference storage needs a separate rooting contract,
and stack/arena storage needs explicit release rules compatible with reusable continuations.
Functional reuse of published bytes remains
in the [byte proposal](bytes.md#functional-updates-with-allocation-reuse-proposed).

Pair accepted examples with rejected counterparts: unknown fixed recipes or placement operands; misaligned,
overlapping, or overflowing placement; unrelated field witnesses; and invalid initialization transitions.
Test thin/fat/header equivalence, pointer indirection, zero-size elements,
metadata bounds, and rejection before any element access.
Invalid raw addresses are caller violations, not recoverable test cases.

Inspect residual code for constant offsets, absent runtime recipe selection, and the specified loads and stores.
Source specialization must remove the fixed view functions and layout evidence from the handle representation.
Measure product, thunk, and frame allocation separately:
CPS and staging alone do not establish that all ordinary runtime allocations disappear;
that work belongs to [local CPS compilation](escape-unboxing.md#local-cps-continuations-proposed).
