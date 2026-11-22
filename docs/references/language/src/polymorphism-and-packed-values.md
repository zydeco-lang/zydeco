# Polymorphism and Packed Values

Polymorphism lets code use a type supplied by its caller.
A packed value lets a provider supply types together with values whose classifiers may depend on them.
These arrangements determine who chooses a type and where its identity is available.

## Universal Polymorphism and Dependent Classifiers

`forall (X : K) . B` classifies computation polymorphism, introduced by `fn (X : K) => M`
and eliminated by type application.
Type arguments erase.
At computation-type level, `pi` uses the binder's sort: a type binder yields a universal computation,
and a value binder yields a computation arrow, possibly dependent on opened type witnesses.
With a kind body, `pi (X : K) . L` forms the kind arrow `K -> L`.
`sigma` similarly yields an existential for a type binder or a product for a value binder.
These forms do not provide general dependence on runtime values or quantification over kinds.

Dependency is judged after elaboration: `pi (value : Int) . (@[typeof] ret value)` forms `Int -> Ret Int`.
The same erasure permits classifier queries in `val pi`, `sigma`, and kind-arrow bodies.

## Existentials and Packed Values

An **existential type** `exists (X : K) . A` hides a type witness used by its payload;
an **existential value** inhabits that type.
The broader term **packed value** also covers structures with manifest type components
or a mixture of abstract and manifest witnesses.
For example, a library may expose an element type together with operations on that type.
Its interface determines which type equations a consumer can use.

An **abstract witness** exposes its kind and identity while keeping its defining equation opaque.
A **manifest witness** is transparent: its interface exposes an equation, such as `Element = Int`.
An exposed equation `Element = T` can still refer to an abstract `T`,
so manifest does not imply that the underlying representation is known.
Visibility belongs to each exposed witness; a packed value can contain both abstract and manifest witnesses.
The [static elimination rules](static-elimination.md) determine which components erase.

The telescope may mix abstract witnesses, manifest equations `(X as T : K)`,
and manifest kind entries such as `(VType as @(intrinsic(vtype)))`.
Later entries may refer to earlier ones. Naming and punning compose with these binder forms.
Static and value entries may be interleaved through nested existentials and products;
packed value formation does not require all existentials to precede all products.
A manifest binder may omit its classifier when its definition synthesizes it, including a kind classified by `Set`.
An explicit classifier checks that same definition.

Against an expected existential, `(T, v)` supplies the witness and payload.
`pack` synthesizes an existential type from explicit evidence:

```zydeco
let Int = @(intrinsic(int)) in
let package =
  pack (= Item as Int)
  where #value = 42 end
in
let (/Item; /value) = package in
ret (value : Item)
```

A manifest entry discloses its witness with `as`; an abstract entry uses `(X : K) is T`.
The payload must synthesize its type. Sealing abstracts occurrences of a recoverable witness identity;
it does not infer an arbitrary abstract interface from concrete values.
For example, `pack (X : VType) is Int where 42 end` synthesizes `exists (X : VType) . Int`.
Use an expected existential annotation when its abstract payload interface must be prescribed.
`pack` currently introduces type witnesses, not kind witnesses.

## Opening and Witness Scope

Opening a packed value introduces fresh identities for its abstract witnesses, with scope over its payload bindings.
Those witnesses cannot escape an ordinary binding's result type.
A **witness-dependent** function classifier binds the abstract witnesses opened from its argument,
allowing its result classifier to refer to them: `pi ((X, x) : exists (X : K) . A) . B`.
Application requires corresponding witness evidence available in the caller's static scope.
The runtime implementation may be an unknown thunk; its signature carries the dependency.
Opening a manifest entry substitutes the disclosed equation and creates no fresh witness.
Introduction checks the supplied witness against that equation.
Manifest entries erase and contribute no abstract witnesses to a witness-dependent arrow.
Disclosed equations substitute transparent provider-local names;
normalization cannot recover an equation hidden by sealing.
At computation application, witness instantiation currently traverses a leading existential prefix;
it does not recover abstract components beneath preceding value products.
An ordinary value `sigma` introduces no witness telescope, so witnesses opened by its pattern must be absent
from the resulting component type, even when mentioned only through `typeof`.

## Field Search and Projection Patterns

Field selection `v/field` recursively searches named wrappers, product components,
and witness telescopes for exactly one matching public name.
Functions, thunks, and data payloads are opacity boundaries.
Explicit named fields and public punned binder names, including manifest kind entries, share this search namespace.
Missing and ambiguous names are different errors, and an explicit path performs a new search at each slash.
Selection can traverse manifest entries directly; crossing an abstract witness requires a projection pattern.
Fields behind an abstract witness still count when checking ambiguity.

This term is rejected with `tyck.missing-named-field`:

```zydeco
(#value = 42)/missing
```

`/field = p` selects and binds the payload; `/field` puns that binding.
Payload patterns are irrefutable and may carry annotations.
Projection patterns associate to the right: `/outer = /inner = p` selects `outer`, then `inner` from its payload.
A group such as `(/Item; /value; whole)` opens each selected packed value occurrence once,
so related fields share witnesses and `whole` can forward the same packed value.
Type projection selects the payload of a named kind.
Module factories and explicit dictionaries use these ordinary packed value and function forms.

## Module Interfaces and Shared Openings

A module interface exposes the names a client needs without requiring the client
to unpack the provider's entire product layout.
Recursive field search permits grouping operations for organization; explicit paths resolve ambiguity.
The opacity boundaries above prevent selection from forcing a thunk or inspecting an arbitrary data payload.
Changing grouping is compatible only where existing selections remain unique and preserve their witness evidence.

An abstract carrier and the operations consuming it must share the witness from one opening of a packed value.
Projection groups and whole-value aliases preserve that opening for forwarding through a composition root.
Independent openings introduce distinct abstract witnesses even
when the packed values have identical field names or layouts.
This relationship uses existential elimination and ordinary bindings; it introduces no separate module object.

An inferred `pack` interface is useful when its witnesses and payload already express the intended contract.
An explicit annotation, including a [source companion](sources.md#source-boundaries),
prescribes the interface when details must be hidden.
`typeof` can reuse an inferred contract but couples it to the inspected implementation.
No companion is required merely because a term is used as a module.

[Value functions](value-functions-and-views.md) compose modules statically.
A provider selected at runtime instead uses an ordinary thunk or codata protocol;
a witness-dependent computation arrow carries any witness dependency needed by its result.
Explicit adapters can expose curried universal interfaces when that is the desired public protocol.

A consumer can open a provider inside a scoped existential elimination and pass its carrier
and operations to a polymorphic callback.
The hidden identity stays within that elimination and cannot escape through its result.
The callback's classifier supplies neither purity nor single invocation nor a native continuation lifetime.
Remaining questions concern [kind-witness introduction](../../../ideas/README.md#kind-witness-introduction),
[computation witness routes](../../../ideas/README.md#computation-witness-routes),
and [companion generation](../../../ideas/README.md#companion-interface-generation).
