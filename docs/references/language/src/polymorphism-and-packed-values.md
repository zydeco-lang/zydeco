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
let packed =
  pack (= Item as Int)
  where #value = 42 end
in
let (/Item; /value) = packed in
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

## Applicative Sealing

Under the shared-opening rule, an abstract type takes its identity from the opening that introduced it.
That rule suits a provider selected once at a composition root.
It becomes a burden for a library module that many sources use independently:
two clients that apply the same module constructor to the same arguments open two packed values
and obtain two incompatible abstract types.
In a diamond-shaped import graph, the shared dependency would have to be opened once at the top and forwarded
through every intermediate source.

`seal term : Sig` gives the abstract types of `Sig` an identity determined by the sealing site and its static arguments.
The sealing is *applicative* in the sense of ML module systems: equal arguments produce equal abstract types.
Every use of the sealed value sees the same types, and their representation stays hidden.

```zydeco
let Counter = exists (= Count : VType) . (#zero :: Count) * (#read :: Thk (Count -> Ret Int)) in
let counter = seal (#Count = Int, #zero = 0, #read = { fn (count : Int) => ret count }) : Counter in
let (/zero) = counter in
let (/read) = counter in
! read zero
```

The two openings are independent, and both see one `Count`, so `read` accepts `zero`.
With the ordinary annotation `(... : Counter)` in place of `seal`, each opening introduces its own `Count`,
and the last line is rejected with `tyck.type-mismatch`.
Sealing still hides the representation.
This term is rejected with `tyck.type-mismatch`:

```zydeco
let Counter = exists (= Count : VType) . (#zero :: Count) * (#read :: Thk (Count -> Ret Int)) in
let counter = seal (#Count = Int, #zero = 0, #read = { fn (count : Int) => ret count }) : Counter in
let (/zero) = counter in
ret (zero : Int)
```

### Static Arguments and Shared Identity

A sealed type is indexed by the static context of its sealing site.
When `seal` appears under a value function or a type abstraction, the type parameters in scope
and the witnesses opened by enclosing binders become arguments of the sealed type.
Instantiations that agree on those arguments agree on the sealed type:

```zydeco
let val cell (Item : VType) =
  seal (
    #Cell = Item,
    #put = { fn (item : Item) => ret item },
    #get = { fn (cell : Item) => ret cell }
  ) : exists (= Cell : VType) . (#put :: Thk (Item -> Ret Cell)) * (#get :: Thk (Cell -> Ret Item))
in
let (/put) = cell Int in
let (/get) = cell Int in
do stored <- ! put 7;
! get stored
```

Both applications yield the same `Cell`, so a value stored through one is read through the other.
The same holds when the two applications sit in different sources that import the builder:
[source copies that merge](sources/packages.md#copy-resolve-merge-analyze) share one sealing site,
which resolves the diamond without a common opening.
Applying `cell` at `Char` produces a different `Cell`, even for a representation that ignores `Item`.
An opened witness indexes the sealed type in the same way.
A builder applied to witnesses from two openings of one provider contract, such as two `Addr` openings,
produces two incompatible types, while forwarding a single opening preserves the identity.

Because a sealed type mentions the witnesses that index it,
the [witness-scope rule](#opening-and-witness-scope) applies to it unchanged.
A witness opened by an ordinary binding indexes the sealed type and cannot leave that binding with it.
This term is rejected with `tyck.escaping-existential`:

```zydeco
let Item = exists (Elem : VType) . (#item :: Elem) in
let item : Item = (Int, #item = 42) in
let (Elem, /item = element) = item in
seal (Elem, #item = element) : exists (Sealed : VType) . (#item :: Sealed)
```

Bind the packed value as a value-function parameter when the sealed type should depend on its witness;
the function's witness-dependent classifier then carries the dependency.

### Checking a Seal

Checking `seal term : Sig` reads `Sig` at kind `VType` and checks `term` against it.
`Sig` must begin with an existential prefix that binds at least one abstract witness,
which gives the seal something to hide.
A signature with no such prefix, or with a wholly manifest one, is rejected with `tyck.type-expected`.

The checker must also see the witnesses that `term` supplies for that prefix.
A packed value written in place provides them, as does one whose witnesses follow
from enclosing value-function, universal, and packed-value binders.
The witnesses of a packed value delivered at runtime are unknown during checking,
so this term is rejected with `tyck.package-witnesses-unavailable`:

```zydeco
let Item = exists (Elem : VType) . (#item :: Elem) in
let chosen : Thk (Ret Item) = { ret ((Int, #item = 1) : Item) } in
do item <- ! chosen;
let sealed = seal item : Item in
ret ()
```

The result is a packed value whose classifier makes each sealed binder manifest:
the binder equals a canonical opaque type applied to the static arguments of the sealing site.
Entries of the prefix that were already manifest keep their equations.
Type equality treats the opaque type as abstract and never unfolds its representation.

Sealing covers the leading prefix of `Sig` only.
An `exists` nested inside a field, a thunk, or a function codomain keeps its generative meaning,
so a sealed module can still export an operation that returns a fresh abstract type on every call.

### Erasure and Staging

`seal` is a static form with no runtime trace. [Static elimination](static-elimination.md) passes through it
to its operand and substitutes each sealed type's representation in residual types.
A sealed value therefore has the runtime layout of its representation, with no additional boxing.
[Residual representability](static-elimination.md#residual-representability-and-erasure) is judged
on that representation: a sealed type standing for a `val pi` classifier is rejected
with `tyck.static-elimination` wherever a runtime value of that type is required.

The [compiler reference](../../compiler/src/checking-and-inference.md#applicative-seals) describes the
sealed type family and where its representation is unfolded.

A [call-by-value block](translations/call-by-value-and-call-by-name.md#call-by-value-blocks) has no translation
for `seal` and rejects it with `tyck.expressivity`.
[Standard Library Organization](../../library/src/organization.md#module-layout-and-boundaries) describes
how library modules choose among `let` imports, parameters, and `seal`.
