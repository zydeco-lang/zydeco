# Zydeco Guide

This guide introduces current Zydeco source syntax, CBPV control, library composition, and monadic blocks.
It assumes familiarity with typed functional programming.
The [language reference](../references/language.md) gives the precise rules; implementation details belong
to the [compiler reference](../references/compiler.md).

Each `zydeco check` example is a complete, independently checked source term.
Imports are relative to this guide's directory, `docs/tutorial/`; adjust them when copying an example elsewhere.
There is no implicit prelude. Blocks marked `text` describe syntax or signatures with schematic names.

## 1. The core idea

Zydeco separates **values** from **computations**.
A value may contain data or suspended code; using it does not run a computation's effects.
Computations perform control transfers such as forcing a thunk, returning a value, or invoking an operation.
Total value functions also assemble and transform values, subject to static elimination as described in §4.

`VType` classifies value types, and `CType` classifies computation types.
The basic forms connect the two:

| Form | Reading |
| --- | --- |
| `Thk B` | Value type of a suspended computation with protocol `B` |
| `{ M }` | Suspend the computation `M` as a thunk value |
| `! thunk` | Force a thunk to run its computation |
| `Ret A` | Computation protocol for returning an `A` to a continuation |
| `ret value` | Return a value to that continuation |
| `do pattern <- M; N` | Run `M : Ret A`, bind its returned value, then run `N` |
| `A -> B` | Computation protocol accepting an `A` before continuing at `B` |
| `fn pattern => M` | Introduce a function computation |

A computation type describes the stack a computation can consume.
`Ret A` describes an installed return continuation; it does not imply a fixed physical frame or stack extent.
A thunk suspends code compatible with its protocol.
Forcing it again runs that code again.

A runnable file accepts the host's Builtin value and ends in that provider's `OS` protocol:

```zydeco check
param (/stdio; /process) : @(import("../../lib/std/builtin.zy")) in
! stdio/write_line "hello" { ! process/exit 0 }
```

`stdio/write_line` has type `Thk (String -> Thk OS -> OS)`.
Its last argument suspends the next `OS` action; `process/exit : Thk (Int -> OS)` terminates with the supplied code.
A returning computation such as `ret 1` checks successfully but needs an executable wrapper for file execution.

### Running programs

Save the complete example above as `docs/tutorial/hello.zy` to keep its relative import valid, then run:

```sh
zydeco check docs/tutorial/hello.zy
zydeco run --dry docs/tutorial/hello.zy
zydeco run docs/tutorial/hello.zy
zydeco repl
```

`check` accepts types, values, and computations.
`run --dry` also selects an executable root, without executing it.
To use the checkout's development binary, replace `zydeco` with `cargo run --quiet --bin zydeco --`.

The REPL accepts commands such as `@[type] ret 1`, `@[run] ret 1`, `@(help)`, and `@(quit)` as separate inputs.
An earlier input can be imported by number with `@(import(1))`.
The [editor guide](../../editor/README.md) describes Cajun language-server support.

## 2. One source file, one term

A `.zy` or `.zydeco` file contains one complete term.
Definitions and parameters are ordinary term forms; there is no distinguished `main` name.
`@(import("path"))` abbreviates `@[import("path")] _` and imports an independently checked source term.
Repeated imports share its checked definition and nominal identities;
imported computations still execute at each dynamic occurrence.
An importer's context cannot supply missing names or solve the imported source's inference variables.

Bindings use either a local syntactic scope or an enclosing block:

| Form | Purpose |
| --- | --- |
| `param P in/that N` | Introduce a parameter, classified from its annotation or expected type |
| `param val P in/that V` | Introduce a total value-function parameter |
| `let P = N in/that N'` | Bind transparently; suitable for type aliases and ordinary values |
| `def P = N in/that N'` | Introduce a sealed binding; type definitions acquire nominal identity |

`in` scopes over the written tail. Inside `begin ... end`, `that` makes a binding mobile:
its name is visible throughout the nearest block, and dependency order determines its placement.
Source order breaks ties between independent bindings.

```zydeco check
begin
  let result = answer that
  let answer = 42 that
  ret result
end
```

A nested `begin` starts a new mobile-binding boundary.
Use `let` for a carrier alias such as `State S A = S -> Ret (A * S)` when clients need its arrow equation.
Use `def` for nominal `data` or `codata` types.
A `foo.zy` implementation may have an independently checked `foo.zyi` type companion;
[the source rules](../references/language.md#12-sources-imports-and-entry) describe that boundary.

### The `!` in a binding is part of the copattern

A binding header describes how its name is eliminated at a use site.
`def ! identity (x : Int) : Ret Int = ret x in ...` binds a thunk that is forced and then applied.
The explicit thunk spelling puts the parameter inside the suspended computation:

```zydeco check
param (/Int; /Thk; /Ret) : @(import("../../lib/std/builtin.zy")) in
def ! identity (x : Int) : Ret Int = ret x in
let explicit : Thk (Int -> Ret Int) = { fn (x : Int) => ret x } in
do first <- ! identity 1;
do second <- ! explicit 2;
ret (first, second)
```

Both names can also be passed directly wherever their thunk type is expected.
A packed value uses plain `let` or `def`; adding `!` would require its body to be a computation.

Documentation uses `--|` Markdown blocks attached to a following `@[doc]` annotation.
Use `--` for ordinary comments; see [the documentation reference](../references/language.md#source-documentation)
for checked examples and links.

## 3. Kinds, types, and type-level terms

Kinds classify type-level terms.
Their base forms are `VType` and `CType`; `Set`, which classifies kinds in the metatheory, has no source term spelling.
Type constructors use kind arrows, and type-level functions use `fn` and application:

```zydeco check
param (/VType; /Int; /Char) : @(import("../../lib/std/builtin.zy")) in
let Pair = fn (A : VType) (B : VType) => A * B in
ret ((1, 'x') : Pair Int Char)
```

Value types include `Unit`, products, data, thunks, existential types, and total value-function types.
Computation types include `Ret A`, `A -> B`, `forall`, witness-dependent `pi`, codata, and the provider's `OS`.
Names such as `Int`, `Char`, and `Thk` come from explicit bindings; the examples obtain them from Builtin.

Unconstrained integer and decimal literals default to `Int` and `Float64`.
An expected fixed-width type selects another width, and the literal must fit that type.
There are no implicit numeric conversions.
Integer arithmetic wraps at the selected width; floating-point arithmetic uses IEEE 754 at the selected width.

### Reusing an expression's type

`@[typeof] expression` yields its type without executing it; applied to a type, it yields its kind.
The result can name a signature or appear in an annotation:

```zydeco check
let Int = @(intrinsic(int)) in
let identity = { fn (value : Int) => ret value } in
let Signature = @[typeof] identity in
let replacement : Signature = { fn (value : Int) => ret value } in
! replacement 0
```

For example, `@[typeof] ret 1` is `Ret Int`, and `@[typeof] Int` is `VType` in this context.
The operand must still check. Use `@[typeof] (1 : Int8)` when the queried literal should have type `Int8`.
Queries preserve abstract identities and cannot extract `Set` from a kind.

### Quantifiers and witness boundaries

`forall` introduces computation-level polymorphism:

```zydeco check
param (/VType; /Thk; /Ret; /Int) : @(import("../../lib/std/builtin.zy")) in
let identity : Thk (forall (A : VType) . A -> Ret A) = {
  fn (A : VType) (value : A) => ret value
} in
! identity Int 42
```

Existential types describe type witnesses together with a value payload.
The [language reference](../references/language.md#9-polymorphism-and-packed-values) defines existential values,
the broader term packed value, and abstract and manifest witnesses.
The abstract and manifest forms are:

```text
exists (X : K) . A          -- hide the witness's definition
exists (X as T : K) . A     -- disclose the equation X = T
```

A named binder such as `exists (#Item = X : VType) . A` exposes the public name `Item`.
`exists (= Item : VType) . A` puns the public and local names.
Manifest entries use `as`, for example `exists (= Item as Int : VType) . A`.
An existential may also disclose a kind equation, with its inferred classifier omitted.

`pi` abstracts over a parameter whose pattern may open type witnesses used in the result type.
It supports dependency on those static witnesses, not arbitrary runtime values:

```zydeco check
param (/VType; /Thk; /Ret; /Int) : @(import("../../lib/std/builtin.zy")) in
let Box = exists (T : VType) . T in
let reveal : Thk (pi ((T, value) : Box) . Ret T) = {
  fn ((T, value) : Box) => ret value
} in
! reveal ((Int, 42) : Box)
```

## 4. Products, named fields, and packed values

`A * B * C` is one three-component product type.
Parentheses preserve nesting: `A * (B * C)` is a two-component product with a product in its second slot.
`*` has no associativity equation.
Tuple introductions and product patterns must have the same arity and nesting as their product type.
`()` inhabits `Unit`; `(value)` is grouping, not a unary product.

```zydeco check
let Int = @(intrinsic(int)) in
let flat : Int * Int * Int = (1, 2, 3) in
let nested : Int * (Int * Int) = (1, (2, 3)) in
let (a, b, c) = flat in
let (x, (y, z)) = nested in
ret ((a, b, c), (x, (y, z)))
```

The flat value cannot check against the nested type:

```zydeco reject=tyck.type-expected at=2:2
let Int = @(intrinsic(int)) in
((1, 2, 3) : Int * (Int * Int))
```

A two-component pattern likewise cannot bind a suffix of a three-component product:

```zydeco reject=tyck.type-expected at=3:5
let Int = @(intrinsic(int)) in
let flat : Int * Int * Int = (1, 2, 3) in
let (first, rest) = flat in
ret (first, rest)
```

Against an expected existential, a comma sequence instead supplies leading witnesses and the remaining payload.
For `exists (X : VType) (Y : VType) . X * Y`, `(Int, Char, 0, 'z')` supplies two witnesses and two payload fields.
This classifier-directed opening of a packed value does not make ordinary products associative.

### Named fields and projection groups

`#field :: A` classifies a named payload; `#field = value` introduces it.
Use `term/field` to select a public field, or a projection-pattern group to open several fields together:

```zydeco check
let record = (#initial = 7, #read = { ret 7 }) in
let (/initial = seed; /read) = record in
do value <- ! read;
ret (seed, value)
```

`/field = local_name` renames; `/field` puns that binding.
A final ordinary member, as in `(/initial; whole)`, retains the whole value.
Search follows named wrappers, product components, and witness telescopes.
Functions, thunks, and data payloads stop the search.
Missing and ambiguous names are distinct static errors.

Manifest components publish equations and support direct selection.
Selecting fields across an abstract witness requires a projection pattern;
selecting related fields in one group shares one opening and the same witnesses.
This is why module examples retain `builtin` and forward it to their builders.

`pack` introduces explicit type witnesses and synthesizes the existential type from its payload:

```zydeco check
let Int = @(intrinsic(int)) in
let package = pack (= Item as Int : @(intrinsic(vtype))) where #value = 42 end in
let (/Item; /value) = package in
ret (value : Item)
```

For an abstract interface, write an expected `exists` type and supply its witness and payload.
An abstract `pack (X : K) is T where value end` also exists,
but cannot infer an arbitrary hidden interface from concrete payload values.
Kind witnesses currently require an annotated packed value rather than `pack`.
[The packed value reference](../references/language.md#9-polymorphism-and-packed-values) covers these boundaries.

Type-level named projection uses the same slash: if `T : (#field :: K)`, then `T/field : K`.
For a manifest named type, `(#field = A)/field` reduces to `A`.

### Projection and prefix precedence

Projection binds tighter than `!`, `ret`, and constructor introduction; application follows those prefixes:

```text
! cap/get argument     = (! (cap/get)) argument
ret cap/initial        = ret (cap/initial)
+Some package/value    = +Some(package/value)
```

To project from a record returned by a computation, bind the returned value first:

```zydeco check
let thunk = { ret (#value = 42) } in
do record <- ! thunk;
ret record/value
```

### Total value functions and views

`val` introduces a total value function, classified by `val pi`.
Its irrefutable parameter may contain a product or open a packed value.
`f value`, `value |> f`, and `f <| value` are the same application.
A view pattern `f ~> pattern` applies such a function before matching its result:

```zydeco check
let Int = @(intrinsic(int)) in
let val first ((x, _) : Int * Int) = x in
let (first ~> value; whole) = (3, 4) in
ret (value, whole)
```

Value functions can construct packed values, return thunks, and compose other value functions.
They undergo [static elimination](../references/language.md#10-static-elimination) before execution;
a runtime-selectable callable should have an explicit thunked computation type.

`match` may produce a value when its arms produce values of the same type.
The total integer intrinsics `int_add`, `int_sub`, `int_and`, and `int_compare` support static calculations:

```zydeco check
let Int = @(intrinsic(int)) in
let add = @(intrinsic(int_add)) in
let compare = @(intrinsic(int_compare)) in
let val maximum (left : Int) (right : Int) : Int =
  match compare left right | -1 => right | _ => left end
in
ret (maximum 16 (add 5 11))
```

The comparison returns -1, 0, or 1.
Value-level branch selection and these integer operations must reduce at the static boundary;
use computation-level matching and returning numeric operations for runtime choices.
A value function may still forward an unknown runtime value through an irrefutable pattern.

## 5. Data and codata

A `data` term forms a value type. Each constructor has one payload; a nullary constructor takes `Unit`.
`match` eliminates the value with exhaustive alternatives:

```zydeco check
let Unit = @(intrinsic(unit)) in
def Bool = data | +False : Unit | +True : Unit end in
let condition : Bool = +True() in
match condition
| +False() => ret 0
| +True() => ret 1
end
```

Constructor nesting may use a whitespace-guided spine such as `+Some +Pair(left, right)`.
`let` and ordinary function parameters require irrefutable patterns;
use `match` when choosing between constructors or literals.
Partial computation binders require explicit `@[partial]`;
[the pattern rules](../references/language.md#7-patterns-and-coverage) describe its scope and failure behavior.

A `codata` term forms a computation type, describing observable residual protocols.
`comatch` implements its destructors, and `M .field` selects one.
A recursive observation can continue directly at the same codata protocol:

```zydeco check
param (/CType; /Ret; /Int) : @(import("../../lib/std/builtin.zy")) in
begin
  def Counter : CType = codata
    | .value : Ret Int
    | .next : Counter
  end that
  def fix counter : Counter = comatch
    | .value => ret 0
    | .next => ! counter
  end that
  ! counter .next .next .value
end
```

`Counter : CType`, so `.next` continues at `Counter`;
returning suspended counter code would instead use `Ret (Thk Counter)`.
`Ret Counter` is ill-kinded because `Ret` expects a value type.
The `fix` binding introduces recursive computation code behind a thunk, invoked here as `! counter`.
The recursive type uses a mobile sealed binding with an explicit kind, so its identity is available in its own body.

## 6. The Builtin interface

The host supplies a packed value implementing the [Builtin signature](../../lib/std/builtin.zy).
Opening that value introduces identities for its abstract system capabilities.
Open it once and retain the value to pass the same capabilities into library builders.

| Group | Contents |
| --- | --- |
| Manifest prefix | `VType`, `CType`, `Thk`, `Ret`, `Unit`, fixed-width integer and floating-point types, `Char`, `String` |
| `numeric` | Operations grouped by numeric type, such as `numeric/int` |
| `text` | `char` and `string` operations |
| `system` | Public `Addr`, `Reader`, `Writer`, and `OS` names; `memory`, `io`, `fs`, `stdio`, `args`, `random`, and `process` operations |

Recursive field selection reaches these public names from the whole packed value:

```zydeco check
param (/numeric) : @(import("../../lib/std/builtin.zy")) in
do answer <- ! numeric/int/add 20 22;
ret answer
```

The numeric groups provide returning arithmetic and continuation-based comparisons.
The larger [standard facade](../../lib/std/std.zy) adds ordinary data types,
numeric capabilities, and source-level text and system helpers.
It is a value-function builder applied to an existing Builtin packed value.

The standard library supplies immutable `Bytes`;
[the memory library](../../lib/std/README.md#explicit-storage) builds allocator protocols and source-level `Storage`,
`Representation`, and static `Plan` recipes over Builtin's address and I/O capabilities.
Those explicit storage interfaces coexist with compiler-managed ordinary values.
`args/at` supports indexed access; the standard argument fold composes it in ordinary CBPV.
The [primitive reference](../references/language.md#13-primitive-values-and-capabilities) lists current contracts.

Returning [C imports](../references/language.md#14-foreign-interfaces) are another explicit boundary.
Current signatures support fixed-width integer scalars, borrowed `Bytes`, and `Ret Unit` for a C `void` result.
Use the documented ABI subset when writing a binding.

## 7. Relative monads

A relative monad has a carrier `M : VType -> CType` and a codata dictionary `Monad M`.
The [monadic basis](../../lib/std/control/monad.zy) exports `Monad` and `Algebra`:

```text
Monad M = codata
  | .return : forall (A : VType) . A -> M A
  | .bind : forall (A : VType) (B : VType) .
      Thk (M A) -> Thk (A -> M B) -> M B
end

Algebra M R = forall (A : VType) . Thk (M A) -> Thk (A -> R) -> R
```

An algebra continues at the chosen computation protocol `R`.
`bind` takes thunks because computations passed as arguments must be suspended.
These are library interfaces; their types do not enforce the monad laws.
The `Ret` instance implements the operations with ordinary `ret` and `do`:

```zydeco check
param (/Ret; /Int; builtin) : @(import("../../lib/std/builtin.zy")) in
let make_monad = @(import("../../lib/std/control/monad.zy")) in
let (/Monad) = builtin |> make_monad in
let ! mo_ret : Monad Ret = comatch
  | .return A value => ret value
  | .bind A B computation continuation =>
    do value <- ! computation;
    ! continuation value
end in
! mo_ret .return Int 42
```

`mo_ret` is already a thunk. Pass it directly where `Thk (Monad Ret)` is required;
force it when invoking an observation.

## 8. Monadic blocks

`@[monadic]` translates a computation using the lexically visible `Monad` and `Algebra` constructors.
`Ret A` becomes the ambient `M A`; `ret` and `do` use the supplied dictionary's `.return` and `.bind`.
Open the basis in the annotation's scope:

```zydeco check
param (/Ret; /Thk; builtin) : @(import("../../lib/std/builtin.zy")) in
let make_monad = @(import("../../lib/std/control/monad.zy")) in
let (/Monad; /Algebra) = builtin |> make_monad in
let mo_ret : Thk (Monad Ret) = {
  comatch
  | .return A value => ret value
  | .bind A B computation continuation =>
    do value <- ! computation;
    ! continuation value
  end
} in
let ! translated = @[monadic] begin
  do value <- ret 1;
  ret value
end in
! translated Ret mo_ret
```

The translated computation first accepts the chosen carrier, then its thunked monad instance.
Parameters inside the annotation follow those two arguments.
Parameters outside the annotation stay outside: for a binding `program S E = @[monadic] ...`,
its use begins `! program S E M mo`, followed by the annotated term's parameters.

### Effect operations inside a block

Write generic effect-operation signatures using `Ret`; translation lifts them into the selected carrier:

```text
get   : Thk (Ret S)                         becomes Thk (M S)
put   : Thk (S -> Ret Unit)                 becomes Thk (S -> M Unit)
raise : Thk (forall (A : VType) . E -> Ret A)
      becomes Thk (forall (A : VType) . Thk Top -> E -> M A)
```

Here `Top` is empty codata. The extra `Thk Top` supplies the structure needed for the quantified value type.
The control library's monadic-block-ready `raise` and `catch` accept that argument already.
More general polymorphic translation is described
in [the relative-monad reference](../references/language.md#11-relative-monads).

## 9. Effect modules

The control modules are ordinary value-function builders from Builtin to packed values:

| Module | Exports |
| --- | --- |
| `monad.zy` | `Monad`, `Algebra` |
| `state.zy` | `State`, `MonadState`, `mo_state`, `state`, `get`, `put`, `modify`, `run_state`, `eval_state` |
| `exception.zy` | `Exception`, `MonadThrow`, `mo_exception`, `throw_ops`, `raise`, `handle_exception`, `try_exception` |
| `state-exn.zy` | `StateExn`, `mo_state_exn`, `state_ops`, `throw_ops`, `get`, `put`, `raise`, `catch`, `run_state_exn` |

Apply a builder and open the selected fields directly:

```zydeco check
param (/Int; builtin) : @(import("../../lib/std/builtin.zy")) in
let make_state = @(import("../../lib/std/control/state.zy")) in
let (/State; /get; /eval_state) = builtin |> make_state in
! eval_state Int Int 7 { ! get Int }
```

This evaluates the state read with initial state 7.
`run_state` additionally passes the final state and answer, in that order, to a continuation of a chosen protocol.

These modules export manifest carrier equations with `as`.
For example, `State S A` exposes `S -> Ret (A * S)`, so it can be applied directly.
The runners provide convenient interfaces; their necessity depends on the exposed type, not the module's name.
`Exception` and `StateExn` disclose carriers containing a private nominal `Either` type;
the handler operations expose its alternatives to clients.

```zydeco check
param (/String; /Int; /Ret; builtin) : @(import("../../lib/std/builtin.zy")) in
let make_exception = @(import("../../lib/std/control/exception.zy")) in
let (/raise; /handle_exception) = builtin |> make_exception in
! handle_exception String Int (Ret Int)
  { ! raise String Int { comatch end } "stop" }
  { fn _ => ret 0 }
  { fn value => ret value }
```

A new module can use `val (dependency : Signature) => value`, or `param val ... that` inside a block,
then finish with `pack`.
Its classifier is a `val pi` inferred from the annotated parameter and result.
See [packed value composition](../../lib/std/README.md#packed-value-composition) for reusable patterns.

## 10. Capability dictionaries

Group the operations required by generic user code as named fields with `Ret` signatures.
Monadic translation lifts the fields together, and the caller chooses the concrete operations and carrier.
This complete example reads the old state, replaces it, and returns both the old and final state:

```zydeco check
param (/VType; /Thk; /Ret; /Unit; /Int; builtin) : @(import("../../lib/std/builtin.zy")) in
let make_monad = @(import("../../lib/std/control/monad.zy")) in
let (/Monad; /Algebra) = builtin |> make_monad in
let make_state = @(import("../../lib/std/control/state.zy")) in
let (/State; /mo_state; /get; /put; /run_state) = builtin |> make_state in
let StateCapability (S : VType) =
  (#get :: Thk (Ret S)) * (#put :: Thk (S -> Ret Unit))
in
let ! program (S : VType) = @[monadic] fn (cap : StateCapability S) (next : S) =>
  do old <- ! cap/get;
  do _ <- ! cap/put next;
  ret old
in
let capability = (#get = { ! get Int }, #put = { ! put Int }) in
! run_state Int Int (Ret (Int * Int)) 7
  { ! program Int (State Int) { ! mo_state Int } capability 9 }
  { fn final_state old => ret (old, final_state) }
```

The capability is a product value and is passed directly.
`{ ! mo_state Int }` suspends the specialized dictionary computation,
while an already-bound dictionary thunk such as `mo_ret` needs no wrapper.
The `S` parameter precedes the annotation, so `Int` precedes the translated carrier and instance arguments.

The [State and Exception example](../../lib/tests/effects/state-exception-stack.zy) extends this pattern
with polymorphic `raise` and `catch`, their structure arguments, and an executable `OS` runner.

## 11. Pitfall checklist

1. A runnable file accepts Builtin and ends in its `OS`; `check` also accepts non-executable terms.
2. Flat and explicitly nested products have different types.
   Patterns must preserve that shape.
3. Projection binds inside force: `! record/field x` parses as `(! (record/field)) x`.
   Bind a returned record with `do` before projecting from it.
4. `do` eliminates `Ret`. Sequence an `OS` action through its explicit continuation,
   and an arbitrary relative monad through its dictionary or a monadic block.
5. A thunk is already a value. Pass its name directly when the expected argument is a thunk.
6. Use transparent aliases when an equation is part of the interface; manifest witnesses publish that equation.
   Open a packed value with abstract witnesses once to keep its related fields at the same witnesses.
7. Monadic blocks need lexical `Monad` and `Algebra`.
   Write generic operation signatures with `Ret` and account for structure arguments in lifted polymorphic operations.
8. Value functions and value-level choices must satisfy static elimination.
   Use thunked computation protocols for runtime-selected behavior.

## 12. Quick grammar cheat sheet

```text
-- comment                     line comment
/- block -/                    block comment

VType  CType                   base kinds; Set is meta-level only
Thk B   Ret A   A -> B          core CBPV type constructors
Unit    A * B * C   data        value types; the product has three components
codata                         computation type constructor
forall (X : K) . B             computation-level polymorphism
pi (pattern : A) . B           computation parameter, possibly opening witnesses
val pi (pattern : A) . A'      total value-function type
exists (X : K) . A             existential type with an abstract witness
exists (X as T : K) . A        existential type with a manifest equation

{ M }                          thunk value
! V                            force
ret V                          return
do P <- M; N                   Ret sequencing
fn P => M                      computation abstraction; also type-level abstraction
val P => V                     total value abstraction
fix P => M                     computation fixed point
V |> f    f <| V               value application, equivalent to f V
f ~> P                         view pattern
match V | P => N ... end       value or computation match
comatch | .d => M ... end      codata introduction
M .d                           codata destructor
V / field                      named projection
+Constructor payload           data constructor
#field :: A                    named payload classifier
#field = term                  named payload introduction/pattern
(/field; /other; whole)        projection-pattern group
pack (X as T : K) where V end  packed value with a manifest witness
pack (X : K) is T where V end  packed value with an abstract witness

begin term end                 mobile-binding block
param P in/that term           parameter
param val P in/that value      value-function parameter
let P = term in/that term      transparent binding
def P = term in/that term      sealed binding
let ! f P : B = M in term      thunk-pattern binding; primary use is ! f
let val f P = V in term        total value-function binding
@[monadic] term                algebra translation
@[typeof] term                 extract a type or kind without execution
@(import("path"))              import sugar for @[import("path")] _
```

## 13. Where to look next

- [Language reference](../references/language.md) — source rules and execution boundaries.
- [Standard library guide](../../lib/std/README.md) — current capabilities, storage, and packed value composition.
- [Effect examples](../../lib/tests/effects/) — complete programs using control modules and capability dictionaries.
- [OOPSLA artifact overview](../../lib/tests/oopsla/README.md) — paper examples, transformers,
  and evaluation instructions.
- [Compiler reference](../references/compiler.md) — phase contracts and maintenance entry points.
