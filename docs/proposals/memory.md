# Further memory boundaries

Static and dynamic views, typed fields, partial record states, and array builders are implemented.
Their design belongs to [L13](../references/language.md#manual-memory),
including [memory views](../references/language.md#memory-views),
[typed records](../references/language.md#typed-records-and-field-paths),
and [array storage](../references/language.md#array-storage-and-element-builders).
This proposal retains the unfinished storage boundaries.

- Additional target profiles need compiler-supplied pointer size and alignment facts;
  the current layouts use native 64-bit and Wasm virtual 64-bit addresses.
- Stack or arena allocation needs an explicit release convention compatible with reusable continuations.
  Abandoning a callback does not automatically run cleanup.
- Storing movable managed references requires explicit rooting and scanning rules.
  Unmanaged pointer slots provide neither.
- Growable arrays and a memory-backed writer belong
  to the [stream proposal](filesystem.md#memory-backed-writer-and-byte-builder).

These extensions introduce no source lifetimes without a separate design decision.
Measure ordinary product, thunk, and frame allocation
under [local CPS compilation](escape-unboxing.md#local-cps-continuations-proposed).
Functional reuse of published immutable bytes needs additional ownership evidence,
owned by the [byte proposal](bytes.md#functional-updates-with-allocation-reuse-proposed).
