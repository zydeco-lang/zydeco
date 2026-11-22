# Layouts, Records, and Arrays

## Static Layout Plans

Fixed placement belongs to source value calculation.
The [fixed builder](../../../../../lib/std/memory/layout.zy) exposes `Layout A = Either LayoutError (Plan A)`,
with an abstract successful plan and [inspectable shape](../../../../../lib/std/memory/shape.zy).
Its constructors support scalars, unmanaged addresses, unit, padding, products, and increased alignment.
Invalid alignment, negative size, and overflow produce typed `Left` results during static construction;
only a `Right` plan can be realized.
The caller handles that static result explicitly. Unknown runtime inputs to placement or fixed realization produce
a [`tyck.static-elimination`](../../../language/src/static-elimination.md#reduction-rules-and-decidability) diagnostic.

`realize A plan` introduces `L` with the [representation interface](../../../../../lib/std/memory/codecs.zy).
Size and alignment are known constants at this boundary.
The source recipes specialize field offsets and selected accesses; the pointer carries none of this evidence.
Explicit queries can materialize size or alignment as an `Int`, and allocation receives those constants.
Transporting a plan through an unknown runtime argument does not make its placement static.
Use `dynamic` when execution chooses sizes or alignment.

### Layout Laws

Supported scalar widths are 1, 2, 4, or 8 bytes, with matching natural alignment.
The current interpreter/native 64-bit profiles and Wasm's virtual-address profile use an 8-byte address slot.
This is an explicit storage format; it does not select a C aggregate ABI or the compiler's logical product layout.

For a product, the second field starts at the first field's size rounded up to the second field's alignment.
Aggregate alignment is the maximum field alignment; total size rounds the field end up to that alignment.
All size arithmetic is checked against nonnegative `Int` bounds before any wrapping primitive arithmetic.
Raising alignment preserves internal field offsets and rounds the final size to the greater boundary.
Unit has size zero and alignment one.
`padding count` reserves bytes and contributes only a logical `Unit`.

For example, `UInt8 * UInt32` has offsets 0 and 4, size 8, and alignment 4.
Raising alignment to 16 retains offsets 0 and 4 and gives size 16.
Typed initialization writes the fields directly; reading ignores the intervening and trailing padding.
No temporary `Bytes` encoding or canonical-zero-padding check participates in these operations.
Serialization and foreign functions that inspect every byte require the caller to initialize padding explicitly,
for example with `raw/unsafe/fill` before field initialization.

## Dynamic Layouts

The [dynamic builder](../../../../../lib/std/memory/dynamic-layout.zy) supports the same layout combinators
when their inputs are runtime values.
Its `realize A R layout no yes` validates sizes, alignment, and overflow at execution,
then supplies a `Representation A` with a witness, `storage : DynamicStorage L`, and `codec : DynamicCodec L A`.
Invalid placement selects `no` before allocation or writes.
Dynamic size, alignment, and offsets remain captured where needed; they are not appended to each typed pointer.

## Typed Records and Field Paths

The [record factories](../../../../../lib/std/memory/record.zy) compose child storage geometry
while preserving its witnesses.
`records/product Left Right left_storage right_storage` calculates ordinary product placement;
`records/at` additionally accepts the left and right byte offsets, total size, and alignment.
Both produce a static `Either LayoutError (Record Left Right)`; neither takes a logical type or codec.
Explicit placement checks nonnegative extents, alignment, field overlap, and the enclosing size before construction.
Zero-sized fields occupy no bytes; padding remains uninterpreted.

Opening the result yields a fresh parent `L`, its `storage : Storage L`,
`left` and `right` field groups, and state conversions.
Names come from ordinary named values: a record module may expose `#length = left` and `#payload = right`.
Nested records require neither reflection nor a compiler row system.

[Field](../../../../../lib/std/memory/field.zy) is an abstract source family `Field Parent Child`.
Its implementation is a static value function carrying the constant displacement;
`fields/offset` and path composition must resolve before execution.
`fields/initialized` and `fields/uninitialized` specialize pure pointer projections for whole-object states.
A path relates layouts, without identifying a particular allocation.
Ordinary clients cannot exchange unrelated parent or child witnesses.
`fields/unsafe/from_offset` is the explicit assertion boundary for caller-authored layout relationships;
its caller establishes a nonnegative offset and a child footprint fitting and aligned within the parent.
A raw shape inspection supplies no such evidence automatically.

`fields/materialize` deliberately produces `DynamicField Parent Child`, carrying one runtime byte offset.
The `runtime_fields` operations query, compose, and project these paths through `Ret`.
`runtime_fields/unsafe/from_offset` accepts a runtime displacement under the same caller obligations.
Neither path form appends a descriptor to its resulting pointer.

Records track partial construction with the erased `Fields LeftState RightState` constructor.
A field's accessors select a logical type and matching codec when used.
After supplying `A`, `Codec Left A`, and sibling state `S` to a left accessor,
its remaining value-function arguments produce these suspended protocols:

```text
left/init pointer value : Thk (Cps (Ptr L (Fields Init S)))  -- pointer has Fields Uninit S
left/read pointer       : Thk (Cps A)                      -- pointer has Fields Init S
left/take pointer       : Thk (forall R. Thk (Ptr L (Fields Uninit S) -> A -> R) -> R)
```

These accessors live under `unsafe`, which slash projection can omit when unambiguous.
For example, `! (left/init A codec S pointer value) R yes` initializes the left field.
The right field preserves the left state analogously.
`states/empty` opens `Uninit` as `Fields Uninit Uninit`, and `states/full` opens `Init` as `Fields Init Init`.
`states/finish` and `states/vacate` perform the inverse conversions for completely initialized or vacant records.
These are erased value conversions, with no flag writes or padding initialization.

A field's `unsafe/project` selects its own state from a partially initialized parent.
For a nested transition, `unsafe/replace` accepts the original parent and the same projected child
after its update, and returns the parent with that child state changed.
It performs no writes or address-equality check: the caller must supply that exact child and retire stale aliases.
An arbitrary path cannot propagate a parent's entire `Fields` state onto one child.

## Array Storage and Element Builders

The [array factory](../../../../../lib/std/memory/array.zy) composes element storage
without selecting a logical value type.
`arrays/make Element storage capacity boundary` takes `Storage Element` and static inputs,
returning `Either LayoutError (Array Element)`.
`arrays/realize Element R storage capacity boundary no yes` takes `DynamicStorage Element`,
validates runtime inputs, and supplies `DynamicArray Element`.
Both calculate `stride = round_up(element_size, element_alignment)`
and `size = round_up(stride * capacity, max(element_alignment, boundary))`, checking all arithmetic before allocation.
An eight-byte element aligned to 64 therefore has stride 64; a codec still touches only its meaningful bytes.
Fixed arithmetic uses [size.zy](../../../../../lib/std/memory/size.zy),
dynamic arithmetic uses [dynamic-size.zy](../../../../../lib/std/memory/dynamic-size.zy).
No integer-dependent type is required.

Opening either packed value introduces an array-layout witness `L`, construction handle `Build`, and family `Values A`.
The packed value supplies `storage`, `capacity`, `stride`, and element/builder operations.
`Ptr L S` is one address.
Fixed recipes specialize their geometry; dynamic packed values retain the geometry and ordinary operation thunks.
Dynamic packed values contain no static value-function fields.
`elements/unsafe/at` checks an index against capacity and produces `Ptr Element S`;
zero-sized elements have zero stride and require no backing bytes.
`elements/unsafe/base` is a pure address interpretation and supplies no dereference bounds.

`elements/unsafe/init_each` writes directly into an uninitialized destination through a caller-supplied callback.
A successful callback supplies that same element as `Init`; a failure callback must first settle its current contents
and supply that same element as `Uninit`.
Completed elements advance the initialized prefix.
The operation's failure successor receives the fault and a `Build` describing that prefix;
success receives whole-array `Init` only after every element completes.
Abandoning a continuation produces neither cleanup nor failure notification.

The `buffer/unsafe` group manages this prefix explicitly.
`start` begins with zero initialized elements; `resume` reopens a fully initialized array at capacity.
`length` and `prefix` expose the current count and a counted initialized element pointer.
`push` checks capacity before writing; `pop` reads the last element and decreases the count.
Fixed calls select `A` and `Codec Element A` as value arguments: `! (buffer/push A codec build value) R no yes`
and `! (buffer/pop A codec build) R no yes`.
Dynamic calls explicitly dispatch through `DynamicCodec Element A`:
`! buffer/push A R codec build value no yes` and `! buffer/pop A R codec build no yes`.
`finish` requires a complete prefix, while `free` requires an empty one and the original allocator.
A rejected operation preserves the supplied handle and storage.
Copying handles still permits stale aliases.

Whole-value conversion is optional.
For a fixed array, `array/codecs A element_codec` produces `Codec L (Values A)`.
A dynamic array uses `! array/codecs A dynamic_element_codec`,
returning a materialized `DynamicCodec L (Values A)` through `Ret`.
`values/generate A` builds exactly the capacity of logical elements through a pure callback;
`values/at A` observes them with a checked index.
`Values` is sealed, preserving that length contract.
Whole-value reads and takes construct a managed list behind `Values A`.
Direct access, prefix building, and `init_each` need no such list.
Embedding array storage in a record requires only the array's `storage` component.
A typed no-read discard operation remains an [open idea](../../../../ideas/README.md#further-storage-boundaries);
manual address reinterpretation remains available under its existing obligations.
See the [standard library guide](../../../../../lib/std/README.md#explicit-storage) for layout,
record, and array examples.
