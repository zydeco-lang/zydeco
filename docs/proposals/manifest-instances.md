# Manifest packages as explicit typeclass instances

A capability dictionary such as `Numeric Bool A` gives a direct dictionary encoding
of a typeclass-like constraint when the carrier `A` is already known.
Libraries also need a first-class value that packages a carrier with its dictionary
while preserving a disclosed concrete representation.
Manifest existential fields provide precisely that equality.

The standard library uses the following instance shape:

```zydeco
NumericInstance Bool Representation =
  exists (#Scalar = ScalarType as Representation : VType) .
    (#dictionary :: Numeric Bool ScalarType) *
    Unit
```

An integer instance therefore contains a static field `Scalar` with the equation `Scalar ≡ Int64`
and a dynamic dictionary checked at `Numeric Bool Scalar`.
The floating-point instance similarly discloses `Scalar ≡ Float64`.
Opening either package substitutes the manifest definition, so generic results at `Scalar` remain definitionally equal
to the concrete representation:

```zydeco
let (
  #Scalar = A,
  #dictionary = operations,
  ()
) = numeric/int64_instance in
  use_numeric A operations
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
or manifest static fields before the dictionary value.
Such an extension must respect the current package-dependent-arrow restriction
that abstract witnesses occur in the leading static telescope.
No associated type or implicit-resolution mechanism is introduced by this proposal.

This design complements [numeric capability packages](numeric-capabilities.md):
capability types describe the required operations, while manifest instance packages bind those operations
to a disclosed carrier.
