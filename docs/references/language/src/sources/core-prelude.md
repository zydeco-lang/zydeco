# Core Prelude

Ordinary source terms can use the core kind and type vocabulary without importing or opening a prelude.
The governing rule is **implicit vocabulary, explicit providers, preserved witness identity**.
Closed names such as `Int`, `Thk`, and `Option` come from the standard view;
`OS` refers to an explicitly opened provider's witness, never to a global host type.
For example:

```zydeco
param (/process; builtin) : @(import(/std/builtin)) in
let stop : Thk (Int -> OS) = process/exit in
! stop 0
```

The parameter selects the provider and opens its packed value once.
That opening supplies both `process` and the witness used by `OS`.
The whole-value alias `builtin` retains it for further composition, but that spelling has no special status.
A provider-independent source needs no such parameter: `Option Int`, or `fn (value : Int) => (ret value : Ret Int)`,
is already a complete source term.

## Vocabulary and Lookup

The default vocabulary is fixed:

| Names | Default meaning |
| --- | --- |
| `VType`, `CType` | Manifest value and computation kinds |
| `Thk`, `Ret` | Manifest intrinsic constructors of kinds `CType -> VType` and `VType -> CType` |
| `OS` | The unique visible witness carrying the `builtin(os)` role, of kind `CType` |
| `Unit` | Manifest intrinsic unit value type |
| `Int8`, `Int16`, `Int32`, `Int64`, `Int` | Manifest signed primitive value types |
| `UInt8`, `UInt16`, `UInt32`, `UInt64`, `UInt` | Manifest unsigned primitive value types |
| `Float32`, `Float64`, `Char`, `String` | Manifest floating-point and text primitive value types |
| `Bool`, `Option`, `Either` | Manifest re-exports of the transparent data definitions, with kinds `VType`, `VType -> VType`, and `VType -> VType -> VType` |

Authored lexical and block-wide bindings take precedence over the default names.
This includes forward mobile definitions: `let Int = Unit that` selects that local meaning throughout its block.
The prelude does not export fields from the source's result or import operations.
`Addr`, `Reader`, `Writer`, `builtin`, stream and memory operations, numeric dictionaries and arithmetic intrinsics,
`List`, `Bytes`, `Monad`, `Algebra`, and effect instances remain explicitly selected.
Data constructors follow ordinary data typing.
Changing the core set changes lexical scope and requires review as a language change.

Naming a computation type supplies neither an action nor an effect handler.
`OS` permits signatures such as `Thk OS`; it does not introduce `exit` or acquire a provider.
`Ret` retains its [ordinary protocol meaning](../computations-and-control.md#ret-and-explicit-cps),
rather than asserting purity.
The default concerns common names, not the purity of the computations they describe.

## Explicit Providers and Shared Openings

An unresolved `OS` in a default-enabled scope selects the unique visible abstract witness
with the `@[builtin(os)]` role.
The role, rather than a signature path or binder name, identifies it.
A signature alias or another provider contract carrying that role works by the same rule.
A packed-value parameter that has not been opened supplies no visible witness.
No visible witness produces `tyck.missing-builtin-type-role`;
multiple witnesses produce `tyck.ambiguous-builtin-type-role` instead of silently choosing the nearest
or first provider.

Select an ordinary lexical `OS` explicitly when several providers are open:

```zydeco
param (/OS; /process = left; first) : @(import(/std/builtin)) in
param (/process = right; second) : @(import(/std/builtin)) in
let stop : Thk (Int -> OS) = left/exit in
! stop 0
```

The explicit field binding chooses the first opening's witness.
Replacing `left/exit` with `right/exit` is rejected: opening the same contract twice does not establish equality.
Reusing an already retained opening preserves identity without a second opening or a fresh witness.
Forward the provider and dependent operations together across composition boundaries,
following the [shared-opening rules](../polymorphism-and-packed-values.md#module-interfaces-and-shared-openings).
`std/builtin/system` exposes the same `@[builtin(os)]` role for system-only programs,
whereas `std/builtin/memory` carries no `OS` witness.
Builtin's abstract `SystemOS` witness, role, and manifest `OS` export remain unchanged;
there is no canonical `intrinsic(os)`.

A mobile binding using contextual `OS` must see independent provider openings in its block,
including openings written later.
Its ordinary dependencies are collected first.
Bindings that neither use contextual `OS` nor depend transitively on a binding that does are checked first;
the remaining bindings retain their ordinary dependency order.
This adds no back-edge through an `OS` user's dependents and does not remove authored cycles.
Use an explicit `OS` field binding when provider selection itself needs ordinary lexical dependencies.

## Opt-Out and Replacement

Meta annotations describe exceptions rather than activating the normal vocabulary.
`@[prelude(none)] e` disables default lookup within `e`, including contextual `OS`.
Authored bindings remain available, including names explicitly bound outside the annotation;
sibling terms retain their own policy.
A leading file-level opt-out also applies to declarations independently selected from that file.

```zydeco
@[prelude(none)]
let Int = @(intrinsic(int)) in
let value : Int = 42 in
value
```

`@[prelude(from(p))] e` replaces the vocabulary with fields from an ordinary lexical packed view `p`.
It abbreviates exactly this opening:

```zydeco
let (
  /VType; /CType; /Thk; /Ret; /OS; /Unit;
  /Int8; /Int16; /Int32; /Int64; /Int;
  /UInt8; /UInt16; /UInt32; /UInt64; /UInt;
  /Float32; /Float64; /Char; /String;
  /Bool; /Option; /Either
) = p in
e
```

Every field comes from `p`, including the kinds; spelling does not assign a compiler identity.
The bindee resolves before the new bindings, so `from(Int)` can refer to an outer value named `Int`.
Missing or ambiguous fields receive ordinary projection diagnostics.
Bind imports, provider selections, and custom adapters explicitly before the annotation.
An override can be used inside an opted-out region because its bindings are ordinary lexical definitions.
It must preserve the witness identity expected by dependent operations; it cannot canonicalize
or reseal a provider's `OS`.

Exactly one argument is accepted: `none` or `from(identifier)`.
Missing, duplicate, combined, unknown, or malformed arguments are rejected.
`from` accepts a lexical variable, not a string, keyword, package path, or arbitrary expression.

## Standard View and Bootstrap

[`prelude.zy`](../../../../../lib/std/prelude.zy), package `/std/prelude`,
exports the closed vocabulary directly as a packed value, excluding provider-relative `OS`.
It imports [`primitive.zy`](../../../../../lib/std/primitive.zy), package `/std/primitive`,
and the three transparent data definitions.
Those data sources import only the primitive leaf, which uses existing intrinsic roles and has no imports,
abstract witnesses, host capabilities, operations, or parameters.
The view exports each closed core name once with a unit payload and no hidden factory parameter.
Manifest kind witnesses use annotated introductions;
manifest type witnesses use [`pack`](../polymorphism-and-packed-values.md#existentials-and-packed-values).

The loader supplies that closed view as an ordinary tracked dependency for each default-enabled source.
The primitive leaf, three bootstrap data definitions, prelude itself, and raw Builtin contracts opt out explicitly
with `@[prelude(none)]`, keeping the dependency graph acyclic.
Other provider-independent libraries use the implicit vocabulary rather than repeating primitive imports.
The default requires neither a project declaration nor a runtime provider.
Ordinary dependency refresh and source sharing apply; the compiler bundles the closed bootstrap for installations
without the source checkout.

## Source Boundaries and Results

Imports, companions, independently selected packages, and numbered REPL inputs receive their own defaults.
They do not inherit a caller's overrides, provider bindings, or opt-out; the file-level opt-out
for selected declarations is the explicit exception described above.
Each source still synthesizes independently before comparison with import-site expectations.
Annotations remain at their authored positions and do not move a monadic basis across a binder
or change monadic capture.

Default vocabulary is available in kind and type roots as well as value and computation roots.
This follows the ordinary [static-only opening rule](../bindings-and-scope.md#static-only-openings),
not a special retry or an inserted `pack`.
An abstract provider witness still cannot escape its opening in a raw type result;
pack the result with an appropriate dependent interface when necessary.
A `.zyi` source must still denote a type.
Checking eliminates the wholly manifest closed opening, leaving no prelude work for interpreter or backend lowering.
No initialization computation, runtime dictionary, extra provider parameter, or runtime metadata is introduced.

Compiler-canonical primitives retain their identities, and transparent data definitions share identity according
to their resolved definitions and the [source sharing rules](packages.md#copy-resolve-merge-analyze).
Neither fact makes a provider-specific witness canonical.
