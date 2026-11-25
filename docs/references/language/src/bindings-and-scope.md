# Bindings and Scope

## Lexical Bindings

`param p in e` introduces a type or computation abstraction; `param val p in v` introduces a value function.
`let p = e in body` binds a type or value transparently.
`def p = e in body` additionally seals a type definition with a nominal identity.
Binders scope over the tail; ordinary lexical bindings may shadow enclosing names.

A binding form determines transparency or nominal identity;
its `in` or `that` connective determines placement and scope.
These choices are independent: moving a definition before or after a use does not turn a transparent alias
into a nominal type.
`let` supports composition of packed values with disclosed equations, while `def` establishes an identity
that clients can share without equating it with its implementation.

## Mobile Bindings and Blocks

A `that` binding contributes to the nearest `begin ... end` block.
Its names are visible throughout that block, and dependencies from bodies and annotations determine its placement.
Source order breaks ties. Lexical binder identities are resolved before dependency scheduling;
reordering must preserve capture, shadowing, and nominal identity.
A dependency on a lexical binder must still be available at the block boundary;
a nested `begin` provides a nearer boundary.

```zydeco
begin
  let answer = seed that
  param (seed : Int) that
  ret answer
end
```

The example elaborates to a function taking `seed` before binding `answer`.
An unannotated `begin` adds no effect or existential boundary.

## Binding Header Abbreviations

Binding headers abbreviate abstractions:

| Header | Binding value or type |
| --- | --- |
| `let F (X : K) = T in e` | `let F = fn (X : K) => T in e` |
| `let val f p : A = v in e` | `let f = (val p => v : val pi p . A) in e` |
| `let ! f p : B = M in e` | `let f = ({ fn p => M } : Thk (pi p . B)) in e` |
| `let fix f p : B = M in e` | A thunk containing `fix f => fn p => M` |

## Mutual Recursion

Recursive block components must consist of sealed type definitions whose kinds are available before their bodies.
Parameters, values, and transparent aliases cannot form such cycles. Runtime recursion is explicit through `fix`;
refutable binding patterns follow [Patterns and Coverage](patterns-and-coverage.md#irrefutability-and-partial-binders).

## Static-Only Openings

A value binding normally has a value or computation continuation.
A static-only opening may also return a kind or type: its checked pattern selects static components
from a [packed value](polymorphism-and-packed-values.md#existentials-and-packed-values)
and discards the runtime payload, introducing no runtime value binder or view pattern.
For example, `let (/Int) = @(import("../../lib/std/primitive.zy")) in Int` returns the manifest type.
The ordinary [witness-escape check](polymorphism-and-packed-values.md#opening-and-witness-scope)
still rejects a result containing an abstract witness introduced by that opening.
An unrelated value binding such as `let value = 1 in Int` does not become a type-level binding.

When every selected witness is manifest, the opening introduces no runtime binding or payload evaluation.
An opening that introduces abstract witnesses retains the evidence needed for static specialization.
