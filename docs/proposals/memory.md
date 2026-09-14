# Manual-memory extensions

The implemented manual-memory interface has its sole design home
in [L13](../references/language.md#manual-memory), with host implementation
in [C14](../references/compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls).
It uses ordinary type witnesses and CPS without source lifetimes, affine values, or a borrow checker.
This proposal retains the unfinished extensions.

## Partial construction and generic buffers

Whole-object `Uninit` and `Init` are sufficient for the implemented scalar and product recipes.
A future `Fields<S...>` encoding could track which record fields have been initialized.
It must associate field witnesses with their parent layout and offset, preserve the other field states,
and establish whole-object `Init` only when all required fields are valid.
Padding contributes no initialization obligation until an operation exposes those bytes.
These are library state protocols; they must not claim that copied aliases are consumed.

The current `Buffer` builds an initialized byte prefix.
A generic element builder could reuse that protocol with explicit element layout, capacity, and prefix count.
Its failure contract must state whether the current element remains uninitialized or needs cleanup.
Dynamic partial progress needs runtime metadata; a static type parameter cannot remember a runtime-dependent count.
Growth and a memory-backed `Writer` belong
to the [stream proposal](filesystem.md#memory-backed-writer-and-byte-builder).

## Further storage boundaries

- Typed field paths could preserve the parent/child relationship currently asserted by raw address conversion.
- Additional target profiles need compiler-supplied pointer size and alignment facts;
  the current layouts explicitly target native 64-bit and Wasm virtual 64-bit addresses.
- Stack or arena allocation needs an explicit release convention compatible with ordinary reusable continuations.
  Abandoning a callback does not automatically run cleanup.
- Storing managed closures or other movable GC references in manual memory requires rooting and scanning rules.
  Unmanaged pointer slots provide neither.

None of these extensions introduces source lifetimes without a separate design decision.
Measure required payload work separately from source product, closure, and frame allocation
under the [local CPS compilation proposal](escape-unboxing.md#local-cps-continuations-proposed).
Functional reuse of published immutable bytes needs additional ownership evidence,
owned by the [byte proposal](bytes.md#functional-updates-with-allocation-reuse-proposed).
