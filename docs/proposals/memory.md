# Further memory boundaries

Static and dynamic views, typed fields, partial record states, and array builders are implemented.
Their design belongs to [L13](../references/language.md#manual-memory),
including [memory views](../references/language.md#memory-views),
[typed records](../references/language.md#typed-records-and-field-paths),
and [array storage](../references/language.md#array-storage-and-element-builders).
This proposal retains the unfinished storage boundaries.
The [memory compilation proposal](memory-compilation.md) owns the compiler gaps, cost criterion,
and first end-to-end acceptance target.

## Independently selectable storage operations

`Operations L A` currently combines storage size/alignment and allocation/release with a logical codec for `A`.
Separate storage geometry and placement from optional whole-value codecs so a record can contain array storage
without selecting its managed-list `Values` representation.
Preserve shared layout identity across the components;
the [existing direct-element and builder interfaces](../references/language.md#array-storage-and-element-builders)
already avoid whole-array conversion.

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

Validate storage-only composition without intermediate logical containers,
and pair checked indexing successes with bounds/overflow failures that preserve storage.
Verify that discard performs no read or write; unchecked tests must supply valid caller-established bounds.

## Additional control and storage boundaries

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
