# Numeric capability packages instead of inheritance

`Int` and `Float` expose similar operations through separate standard-library modules.
Generic numeric code should reuse that common structure without introducing a language-level inheritance relation
or flattening every operation into one record.
A flat record makes names such as `add` and `eq` easy to project ambiguously once capabilities are combined.

This proposal represents each operation family by a type-indexed package.
Larger interfaces contain smaller packages as named fields:

```zydeco
Additive A =
  (zero :: A) *
  (add :: Thk (A -> A -> Ret A)) *
  (sub :: Thk (A -> A -> Ret A)) *
  (negate :: Thk (A -> Ret A))

Multiplicative A =
  (one :: A) *
  (mul :: Thk (A -> A -> Ret A))

PartialEquality Bool A =
  (eq :: Thk (A -> A -> Ret Bool)) *
  (ne :: Thk (A -> A -> Ret Bool))

PartialOrder Bool A =
  (equality :: PartialEquality Bool A) *
  (lt :: Thk (A -> A -> Ret Bool)) *
  (le :: Thk (A -> A -> Ret Bool)) *
  (gt :: Thk (A -> A -> Ret Bool)) *
  (ge :: Thk (A -> A -> Ret Bool))

Numeric Bool A =
  (additive :: Additive A) *
  (multiplicative :: Multiplicative A) *
  (order :: PartialOrder Bool A) *
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

The standard package exports these five type constructors and a `numeric` module containing `int_instance`
and `float_instance`.
Their specialized `int` and `float` modules remain the interfaces for division, remainder, rendering,
extrema, and other representation-specific operations.

This proposal does not add numeric widths, overloaded literals, implicit conversions,
checked arithmetic, or instance search.
It only establishes explicit, reusable capability packages over the operations already present.
