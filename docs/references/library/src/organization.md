# Standard Library Organization

## Module Layout and Boundaries

The standard library separates value representations, reusable algebraic dictionaries, and effect protocols.
This keeps a data type such as `Option` independent of a computation carrier that uses it,
and lets a dictionary such as `Monoid` serve more than one effect.
The directories under `lib/std/` have these responsibilities:

| Directory | Responsibility |
| --- | --- |
| `primitive/` | Provider-independent pure operations (`numeric` and `text`) built over `@(intrinsic(...))` |
| `data/` | Concrete value representations and operations, including `Bool`, `Option`, `Either`, and `List` |
| `algebra/` | General-purpose algebraic dictionaries, currently `Monoid` |
| `effect/` | Concrete effect interfaces, carriers, transformers, handlers, and relative-monad machinery |
| `numeric/` | Concrete width modules and capability dictionaries, plus byte codecs (`codecs.zy`) |
| `text/` | Concrete text utilities (`package.zy`, `bytes-ops.zy`) and the `Bytes` capability module (`bytes.zy`) |
| `system/`, `memory/` | Concrete runtime capabilities and capability-parameterized interfaces (`builtin/{memory,system}.zy`) |

`Algebra M R` belongs to `effect/`: it describes an algebra for a particular relative monad,
whereas `Monoid W` describes an operation and identity on a value type independently of a monad.
Standard-library modules define related type constructors and operations together.
A reusable structural contract is a manifest export alongside the factory or operations that use it;
a type-only package carries its exports with a `()` payload.
Consumers open these packages
with [projection patterns](../../language/src/polymorphism-and-packed-values.md#field-search-and-projection-patterns),
including before defining local type aliases.
A consumer that exports only a derived type packages that result as a manifest field as well.
The Builtin ABI declarations remain type terms because they prescribe the host-supplied interface.

Explicit signatures remain at representation boundaries: colocating a signature
and its implementation must preserve the abstract witnesses and disclosed equations of the public contract.
A separate interface-only package is appropriate when it has an independent dependency boundary,
as with the numeric dictionaries shared by both numeric builders.
Each effect's operation interface, concrete carrier, and transformer stay in the same module.
The Writer effect is distinct from the host stream `Writer` capability.

The root `builtin.zy` bridges the split `builtin/memory.zy` (`Addr`, `memory`,
`scalar`) and `builtin/system.zy` (`Reader`, `Writer`, `OS`, I/O, streams, arguments,
random, process) contracts for complete executables, and `std.zy` remains the assembled public value.
Pure modules (`primitive/`, `data/`, `numeric/package.zy`, `text/package.zy`,
`effect/`) require no `Builtin` parameter and share their `def` identities through package merging.
The `algebra/` and `effect/` modules are imported explicitly rather than added to that aggregate.
Directory ownership does not assign [source-package names](../../language/src/sources/packages.md#source-packages).
The [library guide](../../../../lib/std/README.md#source-layout) inventories the entry points and imports.

Modules that hide representations while sharing abstract types across independent callers (`memory/storage.zy`,
`memory/types.zy`, `memory/field.zy`, `memory/slice.zy`, `memory/buffer.zy`, `memory/layout.zy`,
`memory/dynamic-layout.zy`, `text/bytes.zy`, and `system/package.zy`) bind private local signatures and seal
with [applicative sealing](../../language/src/polymorphism-and-packed-values.md#applicative-sealing).
As a result, dependent modules accept only their capability root (`MemoryBuiltin` / `Addr` or `Builtin`) instead
of forwarding intermediate contract packages, while `std/memory` and `std` re-export the resulting sealed types
as manifest witnesses (`(= Type as Type)`).

## Either Values

The data library's shared [`Either L R`](../../../../lib/std/data/either.zy) represents a
choice between `Left` carrying an `L` and `Right` carrying an `R`.
It is a transparent type, so data operations, exception carriers, I/O, and memory interfaces share its representation.
The constructors do not intrinsically mean failure or success; fallible library APIs use `Either E A`,
with errors on the left and successful values on the right.
The public `std` value and the data package both export `Either` and its `either` operation bundle.

Operations take the input branch types `L` and `R` first, select only the callback
for the branch present in the input, and pass computation callbacks as thunks.
`map` and `and_then` are right-biased and skip their callback on `Left`; `map_left` leaves right values untouched,
and `fold` takes its left handler before its right handler.
The [library guide](../../../../lib/std/README.md#either-operations) lists the `either` operation signatures.
