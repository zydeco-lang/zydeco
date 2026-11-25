# Memory Views, Bytes, and Buffers

## Memory Views

Storage layout, handle representation, and handle interpretation are independent choices.
A handle `H` is the value passed by the caller; a view describes how it leads to a payload pointer and observations.
A logical fat handle does not itself prescribe native field placement or a C argument layout.
Its own storage representation is selected separately.

The [view library](../../../../../lib/std/memory/view.zy) defines these ordinary source types:

```text
Cps A             = forall R. Thk (A -> R) -> R
Checked E A       = forall R. Thk (E -> R) -> Thk (A -> R) -> R
View P H A        = val pi (handle : H). Thk (P A)
DynamicView P H A = Thk (H -> P A)
```

`View` selects a recipe statically and produces a computation for its handle.
`! (view handle)` executes that operation; a CPS operation then receives `R` and its successor.
Typical results are `Ptr L S * M`, where `M` is runtime metadata, but a view can yield another handle for composition.
`Ret` serves pure calculations, while `Cps` and `Checked` sequence memory observations
under the [Ret and explicit CPS convention](../../../language/src/computations-and-control.md#ret-and-explicit-cps).
Applying a value function never executes a load.

`identity`, `precompose`, `map`, and `compose` adapt and combine pure views.
`as_cps` lifts a pure view; `map_cps` and `compose_cps` preserve the CPS protocol.
`as_checked`, `map_checked`, and `compose_checked` propagate explicit failure without running a later step.
No adapter asserts that arbitrary CPS code is pure.
The fixed recipe itself must disappear under [static elimination](../../../language/src/static-elimination.md);
runtime handle fields and captured context can remain.

For runtime selection, `materialize` explicitly produces `DynamicView` with its ordinary thunk and captured data.
Different handle types can be paired with their matching operations in an existential value.
Neither form inserts a view dictionary into each handle.

```zydeco
param (/Addr = _; memory_builtin) : @(import("../../lib/std/builtin/memory.zy")) in
let (/views) = memory_builtin |> (@(import("../../lib/std/memory/package.zy"))) in
let (/Cps; /View; /identity; /as_cps) = views in
let run : Thk (Int -> Ret Int) = {
  fn value => ! ((as_cps Int Int (identity Int)) value) (Ret Int) { fn result => ret result }
} in
! run 7
```

The supplied constructors support these handle forms:

| Constructor | Handle | Observation |
| --- | --- | --- |
| `thin` | `Ptr L S` | Same pointer and `Unit`, through `Ret` |
| `fat` | `Ptr L S * M` | Carried pointer and arbitrary runtime metadata, through `Ret` |
| `unsafe/header` | Abstract `H S`, backed by `Addr` | Read through a metadata codec at a fixed signed displacement; calculate payload address through CPS |
| `unsafe/inline` | Header address in `H S` | Metadata at the handle; payload at a fixed displacement |
| `unsafe/prefix` | Payload address in `H S` | Metadata at a signed displacement; preserve payload address |
| `unsafe/indirect` | Address of an unmanaged pointer slot | Load its pointer, then run another CPS view |

A `Header M Payload` packed value supplies the handle family `H S` and its unsafe wrapping,
address, and opening operations.
The caller establishes initialized metadata fields and payload state `S` independently;
finding an uninitialized destination does not require a fully initialized enclosing object.
For a counted initialized element interpretation, the obligation covers the stated extent, not spare capacity.
The fixed header constructors receive `Codec Metadata M` without requiring its allocation geometry.
`unsafe/dynamic_header` receives `DynamicCodec Metadata M`, runtime displacements,
and a raw address under these same obligations.
A header containing a vtable address is supported as data;
calling code pointers has its separate [FFI boundary](../../../language/src/foreign-interfaces.md).

[headers/from_fields](../../../../../lib/std/memory/header.zy) derives inline and prefix recipes
from metadata and payload paths sharing one parent layout.
Prefix recovery includes the padding chosen by that layout.
For example, an `Int` length followed by four `UInt32` elements with payload alignment 16 has offsets 0
and 16, total size 32, and alignment 16.
The inline handle is the allocation base; the prefix handle is `base + 16`.

`bounded` wraps a counted CPS view with checks for nonnegative capacity and `0 <= length <= capacity`.
`counted` and `counted_checked` receive `Storage L`, derive rounded element stride,
validate index and multiplication bounds, and produce an element pointer.
`dynamic_counted` and `dynamic_counted_checked` use `DynamicStorage L` and compute checked stride at runtime.
These checks cannot establish allocation validity or initialized contents from an arbitrary header.
A fixed array can instead use a thin interpretation, keeping its bound in the recipe without a carried length.

An opening is a snapshot: later metadata changes do not update earlier observations or copied handles.
Reopening performs fresh loads.
Synchronization, alias validity, and exact allocation release remain the caller's task;
release uses the original base and allocation layout, not a mutable length header.
Fixed recipes and state witnesses erase, while ordinary products, thunks,
and frames retain the compiler's representation policy.
Staging and CPS alone do not guarantee that all runtime allocations disappear.

## Immutable Owners and Source Bytes

[Bytes](../../../../../lib/std/text/bytes.zy) is a source abstraction with a private address and byte count.
Its backing allocations are explicitly retained until the runtime instance is destroyed.
Copying and slicing share storage without per-copy reference counting; a small slice can retain a large allocation.
Text conversion and I/O reads import their results into that retained storage.
No mutable operation is available through the immutable content interface.

`bytes/unsafe/build R count alignment fill no yes` allocates a private destination.
The fill callback receives its address, a failure successor, and completion.
It must initialize every published byte and invoke exactly one successor.
Failure frees the allocation before forwarding the error; completion transfers ownership
to runtime retention before publishing `Bytes`.
A retention reservation failure also frees the allocation.
After success, no alias may mutate or free the storage.
Escaped aliases and repeated completions violate this caller contract; the callback type does not prevent them.

`bytes/unsafe/from_retained` wraps an address and nonnegative length whose retention,
immutability, and initialized extent the caller establishes.
`with_window` exposes the address and byte count for a trusted read-only consumer.
`copy_to` copies initialized contents into a caller-provided destination.
Retaining an allocation with unused spare capacity is permitted when only its initialized prefix is published.
Retention transfers ownership, without scanning bytes or tracking initialization.

`empty`, `length`, content comparison, slicing, concatenation, scalar codecs,
and UTF-8 conversion preserve immutable-content semantics.
Bounds and UTF-8 failures remain explicit branches.
Concatenation allocates once and copies each input; alignment conversion allocates a suitably aligned copy.
These pure-content wrappers may use internal allocation
while following the [Ret convention](../../../language/src/computations-and-control.md#ret-and-explicit-cps).

## Byte Builders

The source [Buffer](../../../../../lib/std/memory/buffer.zy) is a manually managed byte builder.
`allocate` takes an explicit allocator and capacity and starts with an empty initialized prefix.
`unsafe/push` and `unsafe/extend` check capacity before writes and deliver an updated handle through CPS.
A capacity failure leaves storage and the supplied handle's prefix unchanged.
Copying a handle does not keep its prefix metadata synchronized with later updates; callers retire stale handles.

`unsafe/snapshot` copies the initialized prefix into retained `Bytes`.
`unsafe/finish_heap` transfers a builtin-heap allocation to retention and publishes its initialized prefix
without copying its payload; the caller must not reuse any old handle.
It requires builtin-heap allocation, since retention eventually frees through that allocator.
`unsafe/close` instead releases storage through the original allocator.
Neither repeated finish nor access after close is a checked operation. Growth and integration
with `Writer` remain an [open idea](../../../../ideas/README.md#growable-byte-builders-and-incremental-streams).

## Stored-Call Interfaces

Source modules share pointer types and CPS operations from the same layout opening.
A worker can accept `Ptr L Init`, take its fields, initialize the same destination,
and deliver `Ptr L Init` without allocating another payload.
There is no separate storage-conversion or call-adapter module.
A different layout needs an explicit allocation and field conversion, with both allocations' lifetimes settled.
Equal numerical placement does not equate independent witnesses.
These calls use the existing source word convention; managed logical values and continuations may still allocate.
See the [standard library guide](../../../../../lib/std/README.md#explicit-storage) for view, byte,
buffer, and stored-call examples.
