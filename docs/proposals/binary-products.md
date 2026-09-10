# Binary products with right-spine layout

Status: proposed alternative, not implemented or accepted as the language's product semantics.
The [language reference](../references/language.md#5-values-products-and-data) owns the current n-ary rule:
`A * B * C`, `A * (B * C)`, and `(A * B) * C` are distinct types with explicit component counts.
The [compiler reference](../references/compiler.md#c10-zasm-stack-analysis-and-local-representation-choices) owns
current representation rules.

This proposal preserves the alternative previously described in `docs/legacy/ideas/products.md`
and assumed by the tutorial and formal calculus.
Those active accounts now follow the implementation.
Adopting this design would require a separate language and representation change.

## Motivation and proposed typing

A binary product calculus could give a tuple suffix its own product type while retaining convenient n-ary syntax.
The proposed choice is to make `*` binary and right-associative:

```text
A * B * C = A * (B * C)
(a, b, c) : A * (B * C)
(a, (b, c)) : A * (B * C)
```

The two values would retain distinct term trees but share a type.
A pattern `(a, rest)` could bind the suffix of the same three-field value.
Left nesting would remain significant: `(A * B) * C` would be a different type.
This is right-association, not unrestricted associativity.

The alternative to this change is the current n-ary model, where arity and explicit nesting determine typing.
Its direct correspondence between source components, typed vectors,
and coverage heads avoids a separate equivalence between tuple grouping and product type shape.

## Surface syntax and packages

The proposal would retain the shared comma syntax and classifier-directed interpretation of products and packages.
`()` would remain `Triv : Unit`, `(term)` would remain grouping, and a source tuple would have at least two terms.
A nonempty tuple would synthesize a product; against an existential classifier,
its leading entries would supply static witnesses and the rest would form the payload.
For example:

```text
exists (X : VType) (Y : VType) . X * Y
```

would accept `(Int64, Char, 0, 'z')`, opened by `(Left, Right, number, letter)`.
These package operations already have their own [current rules](../references/language.md#9-polymorphism-and-packages);
the proposed change concerns how the remaining value product is typed and represented.

## Proposed representation

To preserve source grouping while distinguishing a suffix, the earlier design used these structural shapes:

```rust
struct Triv;
struct Cons<S, T>(S, T);
struct ConsN<S, T>(Vec<S>, T);
```

`Cons` would remain binary. `ConsN` would hold a possibly empty initial sequence and a distinguished final element;
compiler-generated structures such as singleton closure environments could use `ConsN(vec![], value)`.
Surface syntax and name resolution would retain one `Cons(ConsN<_, _>)` per comma sequence.
Typed product values and patterns would use `VCons(ConsN<Value, Value>)`.
Typed packages would use `TCons(ConsN<Type, Value>)`, separating the witness prefix
from the payload; their patterns would mirror that shape.
`Triv` would remain separate from every nonempty product.

Stack IR would record both the logical components and the physical arity derived from the binary product type.
The proposed canonical layout would flatten only the right product spine:

```text
A * (B * C)  => [a, b, c]
(A * B) * C  => [pointer-to-[a, b], c]
```

For `(a, rest)` at `A * (B * C)`, `rest` would point into the `[b, c]` suffix.
Packing `(a, (b, c))` would copy that suffix into a three-field allocation,
making layout depend on the type despite different logical grouping.
A nested product in another position would remain a separate aggregate.
`Unit` would retain its zero-immediate representation without allocating a product.

## Decisions before adoption

- Confirm that suffix binding and binary typing justify changing the current source-level arity rule.
  Include flat and explicitly nested values, patterns, and rejected left-nested counterparts.
- Specify how equality, local inference, coverage, static elimination,
  and package payload checking agree on right-spine expansion.
  Update them together rather than allowing each phase to flatten independently.
- Reconcile suffix copying and interior pointers with representation policies,
  storage contracts, and allocation lifetimes.
  A type equation alone cannot justify the lifetime of a shared suffix.
- Validate the same grouping and layout choices across the interpreter, SPS, native,
  and Wasm paths before replacing the current reference account.
