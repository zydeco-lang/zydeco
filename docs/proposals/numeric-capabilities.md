# Numeric capabilities and explicit instances

The fixed-width integer and floating-point types expose similar operations through separate standard-library modules.
Generic numeric code should reuse that common structure without introducing a language-level inheritance relation
or flattening every operation into one record.
A flat record makes names such as `add` and `eq` easy to project ambiguously once capabilities are combined.

A capability dictionary describes operations over a known carrier type.
A manifest instance package can additionally group that dictionary with a disclosed carrier,
so a consumer obtains both the operations and their representation equation from one opening.
Both use ordinary package composition and explicit value flow.

## Capability composition

This proposal represents each operation family by a type-indexed package.
Larger interfaces contain smaller packages as named fields:

```zydeco
Additive A =
  (#zero :: A) *
  (#add :: Thk (A -> A -> Ret A)) *
  (#sub :: Thk (A -> A -> Ret A)) *
  (#negate :: Thk (A -> Ret A))

Multiplicative A =
  (#one :: A) *
  (#mul :: Thk (A -> A -> Ret A))

PartialEquality Bool A =
  (#eq :: Thk (A -> A -> Ret Bool)) *
  (#ne :: Thk (A -> A -> Ret Bool))

PartialOrder Bool A =
  (#equality :: PartialEquality Bool A) *
  (#lt :: Thk (A -> A -> Ret Bool)) *
  (#le :: Thk (A -> A -> Ret Bool)) *
  (#gt :: Thk (A -> A -> Ret Bool)) *
  (#ge :: Thk (A -> A -> Ret Bool))

Numeric Bool A =
  (#additive :: Additive A) *
  (#multiplicative :: Multiplicative A) *
  (#order :: PartialOrder Bool A) *
  Unit
```

The nesting is the composition rule.
A function that needs addition accepts `Additive A`; a function that also needs multiplication accepts `Numeric Bool A`
and projects `numeric/additive/add` or `numeric/multiplicative/mul`.
The terminal `Unit` keeps the final nested package from merging with the enclosing associative product representation.
No subtyping judgment or superclass elaboration is required.

These packages describe available operations rather than proving algebraic laws.
In particular, IEEE floating-point equality is not reflexive in the presence of NaN,
which motivates the name `PartialEquality`.
Lawful refinements may be introduced later as separate interfaces without changing the operational dictionaries.

## Standard-library integration

The standard package exports these five capability type constructors
and a `dictionaries` module containing one dictionary for each of `Int8` through `Int64`,
`UInt8` through `UInt64`, `Float32`, and `Float64`.
The dictionaries are named after their widths, such as `int64_dictionary` and `float32_dictionary`.
Generic functions receive the selected dictionary as an ordinary argument.
The specialized width modules remain the interfaces for division, integer remainder,
rendering, extrema, and other representation-specific operations.
The host-facing operations live in the Builtin contract's `numeric` group, one plain operation module per width.
The [standard-library guide](../../lib/std/README.md) records the current exports and representation-specific behavior.

Literal types are selected by context, defaulting to `Int64` and `Float64` when no expected type is available.
The capability encoding does not add implicit conversions, checked arithmetic, or instance search;
it establishes explicit, reusable packages over the operations provided by each representation.

## Manifest instance packages

The current library exports dictionaries over already known carrier types.
When a library also needs to expose the carrier through the selected instance,
it can wrap a dictionary in a manifest package.
For example, an instance for `Int64` has this signature:

```zydeco
param Bool : VType in
exists (= Int64 as @(intrinsic(i64)) : VType) .
  Numeric Bool Int64
```

The manifest binder names the field after its disclosed carrier.
A generic family such as `NumericInstance Bool Representation` would instead force one role label onto every carrier,
requiring a rename when the consumer wants the concrete type's name.
Per-carrier package signatures let `Int64` or `Float64` serve directly as the public field name.

An integer instance packages the manifest `Int64` field with a dictionary checked at `Numeric Bool Int64`:

```zydeco
let int64_instance =
  pack (= Int64 as @(intrinsic(i64)) : VType) where
    dictionaries/int64_dictionary
  end
in
let (= Int64, operations) = int64_instance in
  use_numeric Int64 operations
```

Opening this wrapper supplies both `operations` and the equality between `Int64` and its concrete representation.
The [manifest type rules](normalization.md#manifest-types) establish that equality and erase the static field;
the remaining dictionary uses ordinary product projections.
This wrapper is a library construction, not an additional instance primitive.

An instance wrapper follows the shared
[static elimination contract](normalization.md#static-elimination-and-residual-code) for package transport,
while its dictionary's ordinary runtime fields may remain.
A library can also adapt its operations
to an [explicit runtime contract](package-modularization.md#explicit-runtime-contracts).
The disclosed carrier remains a static equality in either case.
The current package occurrence checker is more restrictive than this revised design.

## Explicit selection and scope

Selection is ordinary value flow: a caller chooses `dictionaries/int64_dictionary`
or constructs and passes a manifest wrapper such as `int64_instance`.
When exposing several wrappers together, distinctive field names such as `int64_instance`, `uint8_instance`,
and `float32_instance` also avoid ambiguity in recursive package projection.
Manifest normalization establishes type equality after selection; it does not search the lexical environment,
choose among overlapping dictionaries, or enforce laws.
The choice of dictionary is determined by lexical binding and explicit arguments,
so multiple implementations for the same carrier require no global coherence mechanism.

The same representation can later express associated types by adding abstract
or manifest static fields before the dictionary body.
Such an extension must respect the current package-dependent-arrow restriction that abstract witnesses occur
in the leading static telescope, as recorded in the [normalization account](normalization.md#manifest-types).
No associated type or implicit-resolution mechanism is introduced by this proposal.
