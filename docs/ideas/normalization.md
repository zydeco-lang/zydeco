# Compile-Time Normalization

Consider a library that exports a transparent type together with values using that type:

```zydeco
-- counter.zy
begin
  let Counter = Int that
  let zero : Counter = 0 that
  let inc : Thk (Counter -> Ret Counter) =
    { fn n => ! add n 1 }
  that

  (= Counter, = zero, = inc)
end
```

The library author determines the package signature assigned to the resulting term:

```text
exists (= Counter as Int : VType).
  (zero :: Counter) *
  (inc :: Thk (Counter -> Ret Counter))
```

A consumer binds the library as a parameter without repeating that package signature:

```zydeco
begin
  param (= Counter, = zero, = inc) that

  let one : Counter = 1 that
  ! inc one
end
```

The consumer may annotate the `param` pattern explicitly.
When the annotation is omitted, the compiled provider package signature supplies the expected type
from which it is inferred.
Opening the parameter therefore contributes `Counter ≡ Int`, `zero`, and `inc` to the block context.
The consumer is checked using the package signature produced by the library, and linking applies the consumer
to the library implementation.

## Package Signatures

A package signature is an ordered telescope whose later entries may refer to earlier type entries.
In the following metalanguage, `type` and `val` distinguish the sorts of entries;
they do not introduce a declaration sort into the source language.

```text
Θ ::= ·
    | static X as D : S; Θ
    | type X : K; Θ
    | type X as A : K; Θ
    | val x : A; Θ
```

`static X as D : S` binds a transparent compile-time component,
where the classifier `S` may be `Set` or an ordinary kind.
`type X : K` is abstract. `type X as A : K` is manifest and contributes the definitional equality `X ≡ A`.
The `as` keyword states an equation on an otherwise ordinary existential type binder.
A value entry carries an ordinary runtime value whose type may mention preceding type entries.
Its source-level classifier is `x :: A`, while `x = v` introduces the corresponding named value.

The package signature can be represented by nested package and product types:

```text
⟦ type X : K; Θ ⟧      = exists (X : K). ⟦ Θ ⟧
⟦ type X as A : K; Θ ⟧ = exists (X as A : K). ⟦ Θ ⟧
⟦ static X as D : S; Θ ⟧ = exists (X as D : S). ⟦ Θ ⟧
⟦ val x : B; Θ ⟧       = (x :: B) * ⟦ Θ ⟧
```

This translation preserves the order of the telescope and permits type and value entries to be interleaved.
It does not require a normal form in which every existential precedes every product.

The second line is the package representation of a manifest type.
Its witness is packaged like an existential type component, but its defining equation is disclosed
by the package signature.
Here `X` is an ordinary type variable. Manifest existential types add only the equation `X ≡ A`.
The `as` decoration belongs to the binder payload and composes with ordinary pattern structure rather
than introducing a separate naming form:

```text
exists (field = ((X as A) : K)) . B
exists (= X as A : K) . B  ≡  exists (X = ((X as A) : K)) . B
```

The second line uses ordinary named-pattern punning.
Its leading `=` derives the field from the head binder `X`, while `as A` and `: K` remain decorations of that payload.

## Manifest Static Fields

The classifier of a manifest field may be omitted when the definition determines it:

```text
Γ ⊢ D : S
Γ, X ≡ D : S ⊢ B : VType
────────────────────────────────
Γ ⊢ exists (X as D). B : VType
```

This judgment applies uniformly when `D` is a kind classified by `Set` and
when `D` is a type classified by an ordinary kind.
The explicit form `exists (X as D : S). B` remains available and checks the stated classifier.

The canonical Builtin signature uses the inferred form to introduce the core CBPV vocabulary once:

```zydeco
exists
  (VType as @[intrinsic(vtype)] _)
  (CType as @[intrinsic(ctype)] _)
  (Thk as @[intrinsic(thk)] _)
  (Ret as @[intrinsic(ret)] _)
  (Unit as @[intrinsic(unit)] _)
.
  ...
```

Opening this signature binds `VType`, `CType`, `Thk`, `Ret`, and `Unit` by ordinary package-pattern name resolution.
The first two fields bind kinds; the remaining three bind types or type constructors.
All five are transparent and erased.
They neither create fresh identities nor contribute witnesses to a package-dependent arrow.
Abstract host types later in the signature, such as `Int` and `OS`, retain the ordinary existential semantics.

## Manifest Types

A manifest type in a package signature has the following formation rule:

```text
Γ ⊢ A : K
Γ, X : K, X ≡ A ⊢ B : VType
────────────────────────────────────────
Γ ⊢ exists (X as A : K). B : VType
```

Introduction checks that the supplied witness agrees with the disclosed type:

```text
Γ ⊢ W : K
Γ ⊢ W ≡ A : K
Γ ⊢ v : B[W/X]
──────────────────────────────────────────────────
Γ ⊢ (W, v) : exists (X as A : K). B
```

Eliminating the package binds `X` as a transparent alias of `A` and checks the payload at `B[A/X]`.
It does not create a fresh skolem, so the result may mention `X` wherever it may mention `A`.
The type component is erased after checking.

Ordinary existential elimination remains unchanged.
Opening `exists (X : K). B` creates a fresh abstract identity and applies the usual non-escape check.
When the result of a parameter abstraction depends on that identity, `PackPi` records it.
Manifest types do not add `PackPi` witnesses.

The two forms may occur in one telescope:

```text
exists
  (Key : VType)
  (Map as Tree Key : VType)
.
  API Key Map
```

Opening this package creates an abstract `Key` and then binds `Map ≡ Tree Key`.
Only `Key` belongs to the `PackPi` witness telescope.
For now, `PackPi` opens only a leading existential prefix.
Supporting abstract existential components nested beneath preceding value products is deferred;
this implementation limit does not impose a normal form on package signatures.

## Package Signatures as Expected Types

A package is an ordinary term checked against an expected signature built from existential and product types.
A transparent type component may inhabit a manifest existential, whereas a nominal
or explicitly sealed component inhabits an abstract existential.

An explicitly annotated parameter checks its annotation against the provider package signature.
An unannotated parameter is accepted when that package signature is available as its expected type.
If neither an annotation nor an expected provider package signature is available, the parameter type cannot be inferred.
This is expected-type propagation from the dependency's package signature, rather than inference from uses in the body.

For now, the design assumes one global compilation context in which the provider signature is available
before the consumer is checked.
Separate compilation and persistent interface artifacts are deferred.

Just to be clear, package checking is independent of block elaboration.
The `begin ... end` block only arranges the bindings contributed by `that`;
it neither determines external visibility nor constructs a package interface.

## Sealing and Erasure

Sealing explicitly forgets a manifest equation:

```text
exists (X as A : K). B
    ⟶
exists (X : K). B
```

Opening the sealed package creates a fresh abstract identity.
The reverse conversion is unavailable because an abstract package signature carries no representation equation.
Sealing has no runtime behavior.

Manifest types are also erased:

```text
| (W, v) | = | v |
```

The compiled package signature retains `X ≡ A`, while the executable package retains only its value fields.
Before publishing the package signature, normalization substitutes provider-local transparent names and ensures
that every disclosed right-hand side is closed over the public static context.
Normalization may simplify an equation the provider exposes, but it must not recover an equation hidden
by ordinary existential sealing.
