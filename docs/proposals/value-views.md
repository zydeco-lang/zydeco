# Value Views

## Abstract

A value view is a pattern that observes a value through a total value function before matching it.
If `f` has classifier `val pi (_ : A) . B` and `p` is a pattern for `B`, then `f ~> p` is a pattern for `A`.

```zydeco
let val first ((left, _) : A * B) : A = left that
let first ~> selected = pair in
...
```

The construct is an active pattern in a deliberately narrow sense:
it precomposes an existing pattern with a second-class, effect-free map.
It does not introduce a declaration class or a namespace.
Value functions and their classifiers are supplied by the separate [ValPi proposal](value-pi.md);
this proposal adds only their use in patterns.

## Pattern Formation

The surface extension is

```text
p ::= ... | f ~> p
```

where `f` ranges semantically over checked value terms.
The current surface grammar accepts a value variable followed by optional bracketed type arguments;
this keeps erased application visibly separate from the nested pattern.
The operator associates to the right, so

```zydeco
let first_view ~> second_view ~> result = input in ...
```

applies `first_view`, then `second_view`, and finally matches `result`.
Naming a computed function before the pattern still permits dynamic selection and lexical capture
without making the pattern grammar ambiguous.

The typing rule is ordinary value-function elimination followed by pattern checking:

```text
Delta; Gamma |-v f : val pi (_ : A) . B
Delta; Gamma |-p p <= B -| Gamma'
------------------------------------------------ VIEW
Delta; Gamma |-p f ~> p <= A -| Gamma'
```

Only the nested pattern contributes binders.
The function expression is checked in the lexical environment at the pattern site,
so a view may be selected dynamically or may close over ambient values.
Its application remains total because `ValPi` introduction admits only value terms.

The initial rule requires a single runtime value binder.
A polymorphic function must be instantiated before it is used as a view:

```zydeco
let val first (A : VType) (B : VType) ((left, _) : A * B) : A = left that
let first[Int64, String] ~> selected = pair in
...
```

Square brackets are pattern syntax for erased type application.
They make the boundary between static arguments and the nested pattern explicit;
the term-level spelling remains ordinary value application.

## Meaning

A view pattern is defined by expansion through a fresh intermediate value:

```text
let f ~> p = V in N  ==  let p = (V |> f) in N
```

Here `==` is a source-language equation and `|>` is the value-function application syntax from the ValPi proposal.
Equivalently, if a pattern denotes a partial binding map, then

```text
match_(f ~> p) = match_p o F_f
```

where `F_f : Value(A) -> Value(B)` is the total map denoted by `f`.
The view therefore changes how a value is presented to a pattern; it does not change the effect theory
or add a second notion of function.

For a fresh variable `result`, term and pattern uses agree:

```text
V |> f  ==  let f ~> result = V in result
```

This equation is the central coherence condition.
It ensures that a value function has one meaning whether its result is retained as a term
or immediately decomposed by a pattern.

## Refutability and Coverage

Refutability belongs to the result pattern:

```text
irrefutable(f ~> p) iff irrefutable(p)
```

Applying `f` cannot fail, diverge, or perform effects.
A partial observation must expose failure in its result type, for example by returning `Option B`,
and the nested pattern may then choose which result to accept.

Coverage is necessarily conservative for arbitrary functions.
Arms that use the same syntactic function and the same static arguments may be analysed as patterns over its codomain.
Exhaustiveness over that codomain implies exhaustiveness over the domain,
although the converse need not hold when the function is not surjective.
Arms with unrelated functions require an ordinary exhaustive fallback.

## Implementation Boundary

The elaborated pattern stores a checked value expression and its nested pattern.
Matching applies the function with the same unfolding semantics as `|>` and continues with the result.
There is no `ViewId`, static view signature, view declaration, view dependency graph, or separate resolver namespace.

An implementation may share a transformed result between adjacent arms after proving
that their function expressions are equivalent and pure.
Such sharing is an optimisation, not part of name resolution or the source semantics.

## Open Questions

The implementation restricts a view head to a variable plus explicit type arguments.
Whether to admit arbitrary value terms remains a pattern-syntax question,
not a reason to return to second-class declarations.
A later pattern elaborator could also expose a typed equivalence key for safe sharing across arms.
