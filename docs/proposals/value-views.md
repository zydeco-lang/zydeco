# Value Functions as Value-Level Cut

## Abstract

This proposal replaces first-class pure value functions with statically named value transformations.
A value function does not inhabit a source function type. It names a derivation of the value judgment

```text
x : A ⊢ᵥ W : B
```

and is eliminated by value-level cut. The term `V |> w`, equivalently `w <| V`, substitutes the value `V`
for the subject of `w`. The pattern `w ~> p` performs the same cut and matches `p` against its result.
Thus pipelines and view patterns are not two unrelated conveniences: they are the direct and contravariant
actions of one non-first-class transformation.

This account places the feature in the equational theory of CBPV complex values rather than in a pure function
calculus. It explains why value functions need neither a runtime function object nor an internal function type,
and why pipeline fusion is cut elimination.

## Surface Language

A definition has optional static parameters and one runtime subject:

```zydeco
view first (A : VType) (B : VType)
  ((left, _) : A * B)
: A =
  left
that
```

The transformation has three use forms:

```zydeco
let selected = pair |> first Int64 String in
...

let selected = first Int64 String <| pair in
...

let first Int64 String ~> selected = pair in
...
```

Their grammar is deliberately asymmetric:

```text
V ::= ...
    | V |> view-head
    | view-head <| V

p ::= ...
    | view-head ~> p

view-head ::= view-name static-argument*
```

The open side of a pipeline accepts an ordinary value, while the other side must resolve statically to a complete
view head. A bare view name, partial view application, or `value |> runtime_function` is rejected. Ordinary
computation functions retain their existing CBPV syntax.

`|>` associates to the left; `<|` and `~>` associate to the right. Consequently,

```zydeco
input |> first_view |> second_view
second_view <| first_view <| input
let first_view ~> second_view ~> result = input in ...
```

all apply `first_view` before `second_view`. Mixed pipeline directions require parentheses.

## Static Account

View identities inhabit a static context `Ξ`, separate from the ordinary value context `Γ`.
A simplified signature is

```text
Ξ(w) = ∀δ. A ⇒ᵥ B
```

where `⇒ᵥ` is metalanguage notation for a named value derivation, not a source classifier.
There is no source type `View A B`.

For a definition with an irrefutable subject pattern `P : A`, checking proceeds as follows:

```text
Δ ⊢ P ⇐ A ⊣ Γ_P
Ξ; Δ; Γ_P ⊢ᵥ W : B
irrefutable(P)     free-runtime(W) ⊆ dom(Γ_P)
acyclic(w, dependencies(W))
--------------------------------------------------------- VIEW-DEF
Ξ, w : ∀δ. (P : A) ⇒ᵥ B
```

The body must be a value derivation. It may construct products, constructors, existential packages, literals,
and thunks, and it may cut other views. It may not force a thunk, run a computation, recur, or capture an ambient
runtime value. Static parameters and static dependencies remain available.

The capture restriction is stronger than CBPV itself requires. With ambient context `Γ`, the definition would
denote a morphism `⟦Γ⟧ × ⟦A⟧ → ⟦B⟧`. Requiring capture freedom ensures that the advertised domain `A`
is the complete runtime dependency of the transformation. A required ambient value must therefore be included in
the subject, usually as a product or package field.

Both pipeline spellings use one typing rule:

```text
Ξ(w) = ∀δ. A ⇒ᵥ B
Δ ⊢ S : δ
Ξ; Δ; Γ ⊢ᵥ V : A[S/δ]
------------------------------------------------ VIEW-CUT
Ξ; Δ; Γ ⊢ᵥ V |> w S : B[S/δ]
Ξ; Δ; Γ ⊢ᵥ w S <| V : B[S/δ]
```

The rule refers directly to `w` in `Ξ`; it never synthesizes a value for the view head.
Both forms elaborate to `ViewCut(ViewId, S, V)`.

A view pattern checks its nested pattern against the result classifier:

```text
Ξ(w) = ∀δ. A ⇒ᵥ B
Δ ⊢ S : δ
Ξ; Δ; Γ ⊢ p ⇐ B[S/δ] ⊣ Γ'
------------------------------------------------ VIEW-PAT
Ξ; Δ; Γ ⊢ w S ~> p ⇐ A[S/δ] ⊣ Γ'
```

Only `p` contributes binders to the caller. Existential subjects may open witnesses on which `B` depends;
the view signature records that telescope without introducing a first-class dependent arrow.

## Complex Values and Equational Theory

The central equation expands a view cut into a complex-value binding. If `w` is defined by subject pattern `P`
and body `W`, then, after instantiating static arguments,

```text
V |> w  ≡  w <| V  ≡  let P = V in W
```

Here `≡` denotes source equality, not a Zydeco operator. The right-hand side remains in the value judgment because
`P` is irrefutable and `W` is a value. For a variable subject, the equation is ordinary substitution:

```text
let x = V in W  ≡  W[V/x]
```

Zydeco already represents this administrative form as `Value::Let`. Value functions give selected value
derivations stable names; pipelines cut those derivations without introducing value closures.

The required equational theory is the corresponding fragment of the complex-value theory:

```text
let x = V in x                              ≡ V
let x = V in W                              ≡ W        if x is not free in W
let y = (let x = V in W) in U               ≡ let x = V in let y = W in U
```

The last equation is understood up to alpha-renaming. The second law is sound because value formation is total and
effect-free and because allocation identity is not source-observable. These laws supply identity, dead-cut
elimination, and associativity. A definition

```zydeco
view composed (x : A) : C = x |> first |> second that
```

therefore satisfies

```text
V |> composed  ≡  V |> first |> second
```

without reifying a composite function. Categorically, a value judgment denotes a morphism in the CBPV value
category:

```text
⟦V⟧       : ⟦Γ⟧ → ⟦A⟧
⟦w⟧       : ⟦A⟧ → ⟦B⟧
⟦V |> w⟧  = ⟦w⟧ ∘ ⟦V⟧
```

Composition is available in the category even when its hom-sets are not internalized as value objects.
This is the semantic content of non-first-classness.

The pattern equation is induced by the same cut:

```text
let w ~> p = V in N  ≡  let p = (V |> w) in N
```

Equivalently, if a pattern denotes a partial binding map, then

```text
match_(w ~> p) = match_p ∘ F_w
```

where `F_w : Value(A) → Value(B)` is the total map denoted by `w`. Thus `w ~> p` is the pullback of `p`
along `F_w`. In particular, for a fresh `result`, term and pattern use are coherent:

```text
V |> w  ≡  let w ~> result = V in result
```

Nested view patterns consequently agree with the corresponding pipeline chain. Refutability belongs to the
result pattern:

```text
irrefutable(w ~> p) iff irrefutable(p)
```

There is no source equation comparing two view identities because views are not terms. Distinct `ViewId`s may
denote extensionally equal maps while remaining nominally distinct for resolution, coverage, and separate
compilation. The equational theory compares their cuts, not the static names themselves.

## The CBPV Boundary

In ordinary CBPV, a function type `A -> B` is a computation type. An effectful value-to-value operation has the
shape `A -> F B`, and its abstraction must be thunked to become a first-class value. The current Zydeco `VArrow`
instead internalizes a value-to-value map as a positive type and evaluates it through `EnvValueClosure`.
This proposal removes that additional function space.

A view occupies a different position. It is admissible precisely when its body is derivable in the value judgment.
Constructing a thunk is allowed; forcing one is not. Rearranging or constructing represented data is allowed;
performing I/O, invoking a recursive computation, or dynamically selecting an operation is not. Higher-order and
effectful behavior continues to use ordinary computation functions.

Pure branching is not fundamentally excluded by this account. A richer complex-value calculus may admit exhaustive,
terminating case analysis into values. The initial proposal nevertheless requires an irrefutable input and no
value-level case expression. Branching active views should be added only together with such a rule and its
equational theory. Partial recognition remains explicit by returning `Option B` or another sum; matching failure is
not a hidden effect of applying the view.

## Compilation

The initial compiler should elaborate a definition to a typed, target-independent plan and inline that plan at each
`ViewCut` and view-pattern site. Acyclicity makes expansion terminate; capture freedom supplies every runtime input.
Complex-value associativity then becomes the formal justification for fusing a pipeline and eliminating intermediate
aggregates. The result of a cut remains an ordinary first-class value and is materialized when it escapes. The view
identity itself never occupies a runtime slot and never creates an environment closure.

Coverage remains conservative. Arms sharing the same `ViewId` and static arguments may be checked as result patterns
over `B`, and the transformed result may be shared. Exhaustiveness over `B` implies exhaustiveness over `A`, although
the converse need not hold when `F_w` is not surjective. Arms using unrelated views require an ordinary exhaustive
fallback.

## Transition

The implementation should replace the pure value-function path directly:

```text
VArrow, VForall, VPackPi
Value::VAbs, Value::VApp
EnvValueClosure
```

Their responsibilities move to `ViewSignature`, static parameter and witness telescopes, typed view plans, and
`ViewCut`. Computation `Arrow`, `Forall`, `PackPi`, abstraction, and application remain unchanged. Package-producing
pure functions migrate to views; consumers choose a pipeline when the result is retained and `~>` when it is
immediately decomposed. Higher-order pure-value programs have no view translation and must use computation
functions when dynamic function choice is essential.

The unresolved extensions are branching complex values, the concrete syntax for exported root views, and whether
measurements justify shared first-order view blocks after the inlining implementation. None requires a source view
type or a compatibility layer for first-class pure value functions.

## References

- Paul Blain Levy.
  [Call-by-push-value: Decomposing call-by-value and call-by-name](https://www.cs.bham.ac.uk/~pbl/papers/hosc05.pdf).
- Paul Blain Levy.
  [Call-by-push-value: A functional/imperative synthesis](https://www.cs.bham.ac.uk/~pbl/papers/thesisqmwphd.pdf),
  Chapter 4: Complex Values.
