# Manifest packages as explicit typeclass instances

A capability dictionary such as `Numeric Bool A` gives a direct dictionary encoding
of a typeclass-like constraint when the carrier `A` is already known.
Libraries also need a first-class value that packages a carrier with its dictionary
while preserving a disclosed concrete representation.
Manifest existential fields provide precisely that equality.

The standard library gives each width its own instance shape, named after the disclosed carrier:

```zydeco
param Bool : VType in
exists (= Int64 as @(intrinsic(i64)) : VType) .
  Numeric Bool Int64
```

A single generic family `NumericInstance Bool Representation` would force one role label onto every carrier,
reintroducing the renaming step at each open, so the family was dissolved into per-width instance packages
introduced inline in `numeric/package.zy`.

An integer instance therefore packages one manifest field `Int64`, defined as the primitive `i64` intrinsic,
with the dictionary as the package body, checked at `Numeric Bool Int64`.
The floating-point instance similarly discloses `Float64`.
The manifest binder is a pun on the field name, so no intermediate role variable stands between the field
and its public name.
Opening the package substitutes the manifest definition, so results at the bound name remain definitionally
equal to the concrete representation:

```zydeco
let (= Int64, operations) = numeric/int64_instance in
  use_numeric Int64 operations
```

The manifest field is erased.
Only the dictionary value remains at runtime, and its nested projections lower through the ordinary product machinery.
This makes an instance package a first-class module with a transparent carrier,
rather than a new runtime object or a new form of type evidence.

Instance selection remains explicit.
The `numeric` module gives the packages globally distinct field names such as `int64_instance`,
`uint8_instance`, and `float32_instance`,
and a generic function receives the selected dictionary as an ordinary parameter.
The longer names also avoid ambiguity because unchained projection searches recursively through nested packages.
Manifest normalization establishes type equality after selection; it does not search the lexical environment,
choose among overlapping dictionaries, or enforce laws.
Consequently, coherence follows from ordinary lexical binding and explicit value flow.

The same representation can later express associated types by adding abstract
or manifest static fields before the dictionary body.
Such an extension must respect the current package-dependent-arrow restriction
that abstract witnesses occur in the leading static telescope.
No associated type or implicit-resolution mechanism is introduced by this proposal.

This design complements [numeric capability packages](numeric-capabilities.md):
capability types describe the required operations, while manifest instance packages bind those operations
to a disclosed carrier.
