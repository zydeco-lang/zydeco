# Value Functions and Views

## Total Value Functions

`val (x : A) => v` introduces a total value transformation.
If `v : A'`, its classifier is `val pi (x : A) . A'`.
Type parameters express polymorphism within this value-function space.
Value binders may also open type witnesses used in the result type; arbitrary runtime values cannot index types.

```zydeco
let val duplicate (x : @(intrinsic(int))) = (x, x) in
ret (4 |> duplicate)
```

`f v`, `v |> f`, and `f <| v` are the same application; the directional spellings also apply to type functions.
Functions can be partially applied, passed to other value functions, returned, and stored
in intermediate structures, subject to [Static Elimination](static-elimination.md).
They may capture runtime values, but cannot execute computations to form their result.
A thunk in the result keeps its computation suspended.

## Value Matches and Integer Intrinsics

`match` also produces a value when every arm produces a value of the same type.
It uses the patterns and exhaustive coverage of [Patterns and Coverage](patterns-and-coverage.md);
computation-producing arms keep their usual meaning.
The four integer value intrinsics below have classifier `val pi (left : Int) (right : Int) . Int`,
where `Int` denotes `@(intrinsic(int))`:

| Intrinsic | Result |
| --- | --- |
| `int_add` | Addition wrapping within the `Int` payload |
| `int_sub` | Subtraction wrapping within the `Int` payload |
| `int_and` | Bitwise conjunction |
| `int_compare` | Signed comparison: −1, 0, or 1 |

```zydeco
let Int = @(intrinsic(int)) in
let add = @(intrinsic(int_add)) in
let compare = @(intrinsic(int_compare)) in
let val maximum (left : Int) (right : Int) : Int =
  match compare left right | -1 => right | _ => left end
in
ret (maximum 16 (add 5 11))
```

These total leaves allow checked arithmetic and layout construction in ordinary source functions.
They obey [static elimination's](static-elimination.md#reduction-rules-and-decidability) decidability requirements;
returning numeric computations remain available for runtime inputs.

## View Patterns

A view pattern `f ~> p` applies the total value function `f` to its input, then matches `p` against the result.
Only `p` introduces bindings.
The source head is a variable, optionally with bracketed type arguments, as in `f[T] ~> p`.
Its result pattern may be refutable in a match or partial computation binding.

```zydeco
let Int = @(intrinsic(int)) in
let val first ((x, _) : Int * Int) = x in
let (first ~> x; whole) = (3, 4) in
ret (x, whole)
```

Views associate to the right: `f ~> g ~> p` transforms with `f`, then `g`, before matching `p`.

## Equational Theory

For complete, well-typed total values, the usual capture-avoiding equations explain application and binding:

```text
V |> (val p => W)               = let p = V in W
let x = V in x                  = V
let x = V in W                  = W                     (x not free in W)
let y = (let x = V in W) in U   = let x = V in let y = W in U
val x => f x                    = f                     (x not free in f)
let f ~> p = V in W             = let p = (V |> f) in W
```

The reassociation requires `x` fresh for `U`; binders may be renamed to satisfy it.
The binding equations use irrefutable patterns.
Type abstraction/application has the corresponding beta and eta equations.
These equations concern value transformations; execution still requires the bounded reduction
in [Static Elimination](static-elimination.md).
An explicitly authored thunk is the runtime callable form when an implementation must remain dynamically selectable.
