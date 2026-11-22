# Patterns and Coverage

## Pattern Forms

Variables and holes match any input.
Unit, products, named wrappers, and existential openings are irrefutable when their components are.
A constructor pattern is irrefutable only for a single-constructor data type with an irrefutable payload.
Integer literals check against an expected primitive integer type and must fit its representation.
They are always refutable and select an arm by integer equality.
A match may produce a value or a computation under the rules
in [Value Matches and Integer Intrinsics](value-functions-and-views.md#value-matches-and-integer-intrinsics).
Float, string, and character literal patterns are rejected.

## Irrefutability and Partial Binders

Ordinary parameters and `let`, `def`, `do`, and `param` binders require irrefutable patterns.
`@[partial]` permits failure in the annotated computation binding or function header:

```zydeco
@[partial] let 0 = 0 in ret ()
```

This term is rejected with `tyck.refutable-binding`:

```zydeco
let 0 = 0 in ret ()
```

The annotation covers that construct's own binders, including the parameters in one function header.
It does not propagate into bodies, binding tails, or nested functions, and cannot annotate an entire block.
Value functions and value-producing bindings remain total even under `@[partial]`.
A failed partial binder terminates execution unsuccessfully.

## Pattern Aliases

`(p; q; ...)` applies each member to the same original input, extending scope left to right.
The group requires at least two members and can nest wherever a pattern is accepted.
It neither constructs a product nor passes one member's result to the next.
General aliases require an expected value type and irrefutable members, even under `@[partial]`.
Type and kind pattern aliases are rejected.
Direct field-projection groups additionally support the shared opening of a packed value
([Field Search and Projection Patterns](polymorphism-and-packed-values.md#field-search-and-projection-patterns)).

```zydeco
let ((left, right); whole) = (1, 2) in ret whole
```

This term is rejected with `tyck.refutable-pattern-alias`:

```zydeco
@[partial] let (0; whole) = 0 in ret whole
```

## Exhaustiveness and Coverage

Matches must be exhaustive over their typed input, including correlations between nested patterns.
Integer literal arms require a catch-all even for a finite-width integer type.
[View patterns](value-functions-and-views.md#view-patterns) contribute coverage only
through an irrefutable result pattern; the checker does not prove properties of a view's image.
Generalized comatches must cover arguments and destructors along every observation path.
Duplicate integer arms are not currently rejected as redundant.
