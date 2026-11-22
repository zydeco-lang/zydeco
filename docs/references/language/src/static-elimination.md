# Static Elimination

## Reduction Rules and Decidability

Before execution, value-function applications, views, and static packed value structure must reduce
to a representable residual program.
Reduction follows lexical bindings, type and value application, known constructors,
projections, construction and opening of packed values, value matches,
and the [integer value operations](value-functions-and-views.md#value-matches-and-integer-intrinsics).
It preserves runtime value sharing and the order and multiplicity of effects.
It never runs a computation, force, or general recursion to discover static information.

Integer value operations require known integer operands.
Value matches select the first matching arm without reducing unselected arm bodies.
Every tag or literal needed to decide that choice must be known;
the reducer cannot skip an undecidable earlier row to choose a later catch-all.
Irrefutable patterns can forward unknown runtime payloads inside known structure.
An unresolved integer operand or value-match choice reports `tyck.static-elimination` at a source site.
Ordinary computation matches can still inspect runtime data.

## Residual Representability and Erasure

A library may export an unapplied value function.
A residual runtime value cannot contain one:

```zydeco
val (x : @(intrinsic(unit))) => x
```

This term is rejected with `tyck.static-elimination`:

```zydeco
ret (val (x : @(intrinsic(unit))) => x)
```

Explicit computation thunks and representable packed value payloads may remain.
Types, witnesses, labels, and resolved static routes erase; erasure does not reveal an abstract representation.
Putting a value function inside a thunk does not exempt the thunk's residual body from the rule.

## Typed Holes

Typed value and computation holes can be inspected during checking.
Execution and lowering reject any hole reachable in the residual values, thunks, or computations before effects occur.
Holes confined to eliminated static definitions do not block execution; wildcard patterns are not runtime holes.

## Resource Bounds

The current reducer bounds a request to 128 nested applications and 65,536 applications in total, including views.
Exceeding a bound during residualization reports `tyck.static-elimination`.
An unfinished witness inspection supplies no evidence to dependent checking.
These are implementation resource limits, not a termination theorem.
Optional backend simplification does not relax this source acceptance boundary.
