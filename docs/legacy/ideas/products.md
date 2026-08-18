## N-ary Products and Existential Packages

Parentheses and commas have one surface representation.
The parser does not decide whether a sequence is a value product or an existential package;
that decision remains type-directed.

- `()` becomes `Triv` and checks at `Unit`.
- `(term)` is a parenthesized term, not a unary product.
- `(a, b, ...)` remains one `Cons(ConsN<_, _>)` through surface syntax and name resolution.
- In synthesis mode, a non-empty `Cons` synthesizes a product.
- Against a product type, its terms or patterns are product components.
- Against an existential type, its leading terms or patterns are witnesses and the remaining component
  or components form the package body.

For example, against

```text
exists (X : VType) (Y : VType) . X * Y
```

the term `(Int64, Char, 0, 'z')` contains two witnesses and a two-component product body.
The pattern `(Left, Right, number, letter)` eliminates the same package.
Sort checking therefore happens in the type checker, not in the parser.

## Typed Representation

Product types remain binary because `*` is the binary type operator, and `Unit` remains the nullary product type.
A right-associated product spine gives the component types for an n-ary value:

```text
(a, b, c) : A * (B * C)
```

The shared structural nodes are:

```rust
struct Triv;
struct Cons<S, T>(S, T);
struct ConsN<S, T>(Vec<S>, T);
```

`Cons` remains the representation for structures that are genuinely binary.
`ConsN` is nonempty: the vector is the possibly empty initial sequence and `T` is the distinguished final element.
Source tuples contain at least two elements, but compiler-generated structures such
as a singleton closure environment may use `ConsN(vec![], value)`.

Typed product values and patterns use `VCons(ConsN<Value, Value>)`.
Typed existential packages use `TCons(ConsN<Type, Value>)`, with the witness prefix
in the vector and the package body as the final element.
The corresponding type-pattern form has the same shape.
Explicit grouping is preserved, so `(a, b, c)` and `(a, (b, c))` are different term trees even
when they have the same type.

The empty value and pattern are explicit `Triv` nodes throughout the compiler.
They inhabit `Unit`; they are not empty `ConsN` values.

## Canonical Backend Layout

Stack IR records the logical `ConsN` elements inside `VCons` alongside the physical arity derived from the product type.
Assembly packing then uses a canonical layout for each binary product type:

```text
A * (B * C)  => [a, b, c]
(A * B) * C  => [pointer-to-[a, b], c]
```

Only the right product spine is flattened.
A nested product in any other position remains a pointer.

Logical grouping need not match physical arity.
When a two-element pattern `(a, rest)` eliminates a three-field `A * (B * C)`,
`rest` is an interior pointer to `[b, c]`.
Conversely, packing `(a, (b, c))` copies the final suffix into the canonical three-field allocation.
This keeps representation type-directed and stable without reintroducing binary tuple desugaring.

Every `VCons` layout is nonempty. `Triv` travels through Stack IR and assembly separately,
where it is emitted as the backend's zero immediate and does not allocate a heap product.
