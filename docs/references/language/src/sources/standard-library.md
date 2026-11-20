# Standard Library Organization

The standard library separates value representations, reusable algebraic dictionaries, and effect protocols.
This keeps a data type such as `Option` independent of a computation carrier that uses it,
and lets a dictionary such as `Monoid` serve more than one effect.
The directories under `lib/std/` have these responsibilities:

| Directory | Responsibility |
| --- | --- |
| `data/` | Value representations and their operations, including `Bool`, `Option`, `Either`, and `List` |
| `algebra/` | General-purpose algebraic dictionaries, currently `Monoid` |
| `effect/` | Effect interfaces, carriers, transformers, handlers, and shared relative-monad machinery |
| `numeric/` | Numeric operations and their domain-specific dictionaries |
| `text/` | Text and byte-sequence operations |
| `system/`, `memory/` | Concrete runtime capabilities and their library interfaces |

`Algebra M R` belongs to `effect/`: it describes an algebra for a particular relative monad,
whereas `Monoid W` describes an operation and identity on a value type independently of a monad.
Standard-library modules define related type constructors and operations together.
A reusable structural contract is a manifest export, alongside the factory or operations that use it;
a type-only package carries its exports with a `()` payload.
Consumers open these packages with projection patterns (§9), including before defining local type aliases.
A consumer that exports only a derived type packages that result as a manifest field as well.
The Builtin ABI declarations remain type terms because they prescribe the host-supplied interface.

Explicit signatures remain at representation boundaries: colocating a signature
and its implementation must preserve the abstract witnesses and disclosed equations of the public contract.
A separate interface-only package is appropriate when it has an independent dependency boundary,
as with the numeric dictionaries shared by both numeric builders.
Each effect's operation interface, concrete carrier, and transformer stay in the same module.
The Writer effect is distinct from the host stream `Writer` capability.

The root `builtin.zy` remains the host contract, and `std.zy` remains the assembled public value.
The `algebra/` and `effect/` modules are imported explicitly rather than added to that aggregate.
Directory ownership does not assign [source-package names](packages.md#source-packages).
The [library guide](../../../../../lib/std/README.md#source-layout) inventories the entry points and imports.

## Either Values

The data library's shared [`Either L R`](../../../../../lib/std/data/either.zy) represents a
choice between `Left` carrying an `L` and `Right` carrying an `R`.
It is a transparent type, so data operations, exception carriers, I/O, and memory interfaces share its representation.
The constructors do not intrinsically mean failure or success; fallible library APIs use `Either E A`,
with errors on the left and successful values on the right.
The public std value and the data package both export `Either` and its `either` operation bundle.

All operations take the input branch types `L` and `R` first.
Transformations then take their new payload type `S`; `fold` instead takes an arbitrary computation result `B`.
The operations select only the callback for the branch present in the input:

| Operation | Contract |
| --- | --- |
| `left L R value`, `right L R value` | Construct the corresponding branch under `Ret` |
| `fold L R B value on_left on_right` | Apply the handler for the selected branch, returning `B` |
| `map L R S value transform` | Transform a right payload through `R -> Ret S`, returning `Ret (Either L S)` |
| `map_left L R S value transform` | Transform a left payload through `L -> Ret S`, returning `Ret (Either S R)` |
| `and_then L R S value next` | Sequence a right payload through `R -> Ret (Either L S)`; preserve a left payload |
| `unwrap_or L R value fallback` | Return the right payload, or the fallback for a left value |
| `is_left L R value`, `is_right L R value` | Return the corresponding branch test as `Ret Bool` |

`map` and `and_then` are right-biased; their callbacks are not run for a left value.
`map_left` leaves right values untouched, and `fold` takes its left handler before its right handler.
Computation callbacks are passed as thunks.
