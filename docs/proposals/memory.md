# Further memory boundaries

Static and dynamic views, typed fields, partial record states, and array builders are implemented.
Their design belongs to [L13](../references/language.md#manual-memory),
including [memory views](../references/language.md#memory-views),
[typed records](../references/language.md#typed-records-and-field-paths),
and [array storage](../references/language.md#array-storage-and-element-builders).
This proposal retains the unfinished storage boundaries.
The [memory compilation proposal](memory-compilation.md) owns the compiler gaps,
cost criterion, and first end-to-end acceptance target.
Its opening states the division between ordinary values and programmer-selected storage contracts;
the [codec boundary](memory-compilation.md#cross-between-ordinary-values-and-explicit-storage) explains how reads
and initialization connect them.
It now follows a [32-byte header and array](memory-compilation.md#1-start-with-the-bytes)
through the [source interfaces](memory-compilation.md#2-give-each-source-type-one-job), initialization,
memory IR, and target instructions.

## Independently selectable storage operations

Storage-only record/array composition and optional codecs are implemented
in [L13](../references/language.md#independent-storage-and-codecs).
The [header-array example](../../lib/tests/std/header-array.zy) composes only geometry and selected field access;
[array conversion](../../lib/tests/std/array-memory.zy) requests logical `Values A` explicitly.
The remaining choices are independent indexing and discard contracts.

Provide explicit typed checked and unchecked indexing choices.
The current `elements/unsafe/at` still checks bounds and displacement overflow;
`unsafe` marks the remaining memory obligations, not absence of checks.
Unchecked access can currently be assembled with raw address operations,
but a typed interface should state the bounds and arithmetic obligations it transfers to the caller.
Known redundant checks should disappear under specialization; genuinely dynamic checked access retains them.

Provide a no-read discard/forget transition for initialized storage whose logical value is no longer needed.
The current `take` reads that value, and raw pointer reinterpretation can assert another state.
A dedicated operation should express the transition without invoking a codec, clearing bytes, or destroying resources.
The caller must settle owned contents and aliases before forgetting them;
this operation must not imply ownership checking or automatic destruction.

Retain the storage-only and checked-indexing regressions while adding these interfaces;
pair new indexing successes with bounds/overflow failures that preserve storage.
Verify that discard performs no read or write; unchecked tests must supply valid caller-established bounds.

## Additional control and storage boundaries

The header-array example fixes nonoverlapping byte placement and uses ordinary scalar accesses.
The following extensions change a separate part of that contract:

| Boundary | Concrete next example | Representation or lowering obligation |
| --- | --- | --- |
| Byte packing | `UInt8` at offset 0 and `UInt32` at offset 1: extent 5, alignment 1, versus natural extent 8/alignment 4 | Keep field byte offsets; access the second field with alignment one. Bit fields additionally need bit offsets, masks, and read/modify/write rules |
| Overlapping storage | `UInt32` and `UInt64` alternatives at offset 0: extent 8, alignment 8 | Candidate source states `LeftActive S`/`RightActive S` identify a statically known active member; a runtime choice needs an explicit tag/observation protocol |
| Runtime field placement | An element array following a header whose extent is read at runtime | Validate extent, offset, alignment, and nonoverlap before returning `DynamicField Parent Child`; preserve the descriptor associated with the allocation |
| Target facts | The width/alignment of an unmanaged pointer slot on a new execution profile | Supply static address facts to factories and include that profile in compilation/cache identity; do not infer them from arbitrary logical `A` or the build host |
| Stack/static bytes | A 32-byte, 16-aligned scratch object or writable static buffer | A frame slot or data symbol; explicit placement requires valid lifetime/initialization evidence, and cannot silently become heap allocation |
| Buffer/arena allocation | A backing range with a bump cursor and an explicit reset operation | `StaticAlloc Context` selects code; context carries actual cursor/range state; alignment/capacity checks precede cursor mutation |
| Managed references | A stored raw counter beside a movable managed reference | A registered root/trace description distinguishes raw bits from references and keeps the reference valid through collection |
| Pointer operations | Compare two addresses, subtract pointers within an allowed range, or expose address bits | Dedicated operations with target width and provenance rules; an integer bit pattern alone does not prove dereference validity |
| Observable/synchronized access | A volatile device register or an acquire load from an atomic `UInt32` slot | Distinct primitive access semantics and static ordering/alignment requirements; ordinary scalar loads cannot stand in for them |

The candidate union states are ordinary std type constructors whose implementations can erase,
like `Fields`; they do not add a runtime tag automatically or track stale aliases.
Packed-field projection must retain justified access alignment even
when the child codec's standalone allocation requests stronger alignment.
Explicit C struct layout and aggregate transport remain separate FFI decisions.
These examples bound further designs; their factories, state protocols, and target support are not yet implemented.

- Checked record composition requires aligned, nonoverlapping fields.
  Packed records and unions currently need custom operations or raw access.
  Typed factories need explicit placement, overlap, active-field, and state-transition contracts;
  ordinary scalar byte accesses already permit unaligned addresses.
- Runtime layout construction lacks a checked typed field-path factory matching fixed records.
  `DynamicField` carries a runtime displacement, but its raw constructor asserts validity.
  A checked builder must validate placement before exposing related parent/child witnesses and states.
- Additional target profiles need compiler-supplied pointer size and alignment facts;
  the current layouts use native 64-bit and Wasm virtual 64-bit addresses.
- Stack or arena allocation needs an explicit release convention compatible with reusable continuations.
  Abandoning a callback does not automatically run cleanup.
  Static storage also needs a placement, initialization, and lifetime contract.
- Storing movable managed references requires explicit rooting and scanning rules.
  Unmanaged pointer slots provide neither.
- The raw interface lacks dedicated pointer equality/difference and pointer/integer conversion contracts.
  Define their target widths, allocation/provenance obligations, and foreign-address behavior instead
  of relying on byte-level encodings as substitutes.
  Current sizes and offsets use `Int`; exposing the full pointer-width numeric domain needs an explicit interface,
  independently of whether the compiler boxes a temporary scalar.
- Atomics, volatile access, and memory ordering need dedicated primitive semantics and target support.
  Ordinary loads/stores and caller-managed aliases do not supply synchronization.
- Growable arrays and a memory-backed writer belong
  to the [stream proposal](filesystem.md#memory-backed-writer-and-byte-builder).

These extensions introduce no source lifetimes without a separate design decision.
Control over ordinary compiler-managed products, closures, strings,
and frames remains the [representation](escape-unboxing.md) and [native environment](native-frames.md) boundary.
The current Wasm host uses virtual 64-bit addresses and host range lookup; native pointer lowering
and Wasm embedding costs require separate acceptance criteria under the [backend proposal](wasm-backends.md).
Code pointers, callbacks, and C aggregate transport belong to the [FFI proposal](c-ffi.md).

Immutable `Bytes` deliberately retains shared storage until runtime teardown.
Earlier reclamation, functional reuse, and alternative ownership policies require additional evidence,
owned by the [byte proposal](bytes.md#functional-updates-with-allocation-reuse-proposed).
The retained policy is an accepted choice for this rewrite, not an accidental cost to remove implicitly.
