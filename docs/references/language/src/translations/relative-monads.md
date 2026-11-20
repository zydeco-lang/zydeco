# Relative Monads and Monadic Blocks

## Relative Monads

A relative monad has carrier `M : VType -> CType`.
The library's `Monad M` is codata with operations `.return : forall (A : VType) . A -> M A`
and `.bind : forall (A : VType) (B : VType) . Thk (M A) -> Thk (A -> M B) -> M B`.
An `Algebra M R` is the computation type `forall (A : VType) . Thk (M A) -> Thk (A -> R) -> R`.
These are ordinary library interfaces; their types do not enforce algebraic laws.

## Relative Monad Laws

A lawful dictionary preserves both answers and observable effects when sequencing is reassociated.
Write `return_M a` for its return operation and `bind_M m f` for its bind,
omitting type arguments and writing braces for thunks.
The two unit equations and associativity are:

```text
bind_M { return_M a } f                         ≃ f a
bind_M m { x => return_M x }                    ≃ force m
bind_M { bind_M m f } g                         ≃ bind_M m { x => bind_M { f x } g }
```

Linearity additionally preserves the effects of producing the first computation thunk:

```text
tt : Thk (Ret (Thk (M A)))

do t <- force tt; bind_M t f
  ≃ bind_M { do t <- force tt; force t } f
```

This equation concerns bind's first computation argument, not a global linear-use restriction on thunks.
The equalities are observational, including ambient effect order rather than only returned values.
The [relative-monad paper, §4.2](https://arxiv.org/html/2502.15031v2#S4.SS2), gives the semantic account.
Checking a `Monad` dictionary verifies its classifier, not these equations.

## Monadic Blocks

`@[monadic] e` selects the lexically visible `Monad` and `Algebra` constructors
and performs sort-preserving algebra translation.
Write `L_M` for translation under carrier `M` and its monad implementation.
The annotation synthesizes its operand once, then supplies an outer interface appropriate to that operand's sort:

| Operand | Generated interface | Result classifier |
| --- | --- | --- |
| Type `T : K` | `fn (M : VType -> CType) => L_M(T)` | `(VType -> CType) -> K` |
| Value `v : A` | `val (M : VType -> CType) (mo : Thk (Monad M)) => L_M(v)` | `val pi (M : VType -> CType) (mo : Thk (Monad M)) . L_M(A)` |
| Computation `e : B` | `fn (M : VType -> CType) (mo : Thk (Monad M)) => L_M(e)` | `forall (M : VType -> CType) . Thk (Monad M) -> L_M(B)` |

Type operands include value types, computation types, and higher-kinded type constructors.
Their translation needs the carrier but no runtime dictionary argument.
Value operands include thunks, value functions, and ordinary value packages used as modules;
translation preserves them as values rather than inserting a `ret` or forcing suspended computations.
Kinds have no operand translation and are rejected with `tyck.sort-mismatch`.
The operand receives no expected classifier from outside the annotation; an ascription inside it applies normally.

Source `Ret`, `ret`, and `do` are reinterpreted through the supplied carrier and its operations.
Parameters outside the annotation remain outside; translated parameters inside follow the generated interface.
Polymorphic translation may require additional structure arguments describing how a type supports the translation.
For example, `let MonadicMonad = @[monadic] (Monad Ret)` derives the dictionary classifier:
`MonadicMonad M` has the translated operations, including a `Thk Top` argument after each value-type quantifier.
Likewise, `(@[monadic] { ret 1 }) M mo` is a value of type `Thk (M Int)`.

```zydeco
param (/Ret; /Thk; builtin) : @(import("../../lib/std/builtin.zy")) in
let basis = @(import("../../lib/std/effect/monad.zy")) in
let (/Monad; /Algebra) = basis in
let mo : Thk (Monad Ret) = {
  comatch
  | .return A value => ret value
  | .bind A B computation continuation =>
    do value <- ! computation;
    ! continuation value
  end
} in
(@[monadic] begin do x <- ret 1; ret x end) Ret mo
```

## Packed Effect Instances

An effect-polymorphic client needs a carrier, its monad dictionary, and operations at that same carrier.
The [effect library](../../../../../lib/std/effect) separates carrier-indexed operations
from existential packaging so several effects can share one carrier.
`IState S M` contains `state : Thk (MonadState S M)`; `IException E M` contains `exception : Thk (MonadThrow E M)`
and a `catch` operation at `M`.
Neither operation bundle introduces a carrier or a monad dictionary of its own.

The carrier and its required dictionary form one packed argument.
The shared `MonadInstance` interface carries the dictionary as its unnamed payload,
without additional effect operations:

```zydeco
exists (M : VType -> CType) . Thk (Monad M)
```

`WithMonad I` packages one leading positional witness `M : VType -> CType`,
a named `monad : Thk (Monad M)` dictionary, and an operation payload of type `I M`.
Standalone clients use `WithMonad (IState S)` or `WithMonad (IException E)`.
The combined signature `IStateExn S E` composes the operation bundles before packaging them:

```zydeco
WithMonad <| fn (M : VType -> CType) =>
  IState S M * IException E M
```

Both bundles therefore use the same `M` and the surrounding package's single monad dictionary.
A product of independently packaged interfaces would instead introduce independent witnesses.
These signatures constrain carrier agreement, not the algebraic laws of the supplied operations.

A client whose result uses that carrier has a [witness-dependent signature](../polymorphism-and-packed-values.md),
such as `pi ((M, _) : WithMonad (IState Int)) . M Int`.
Its parameter pattern opens the carrier and operation payload once;
repacking `(M, ops)` forwards the same witness to other clients.
The packed boundary is an ordinary library interface.
An adapter inside the client supplies the carrier, monad dictionary, and operations
to the existing curried interface generated by `@[monadic]`.

Each concrete `instance` value function exposes a manifest carrier equation.
Open that result and explicitly reconstruct the argument expected by an abstract instance signature:
`let (M, ops) = instance Int in ! client (M, ops)`.
This keeps the concrete equation available to the caller's runner while the client sees only its declared operations.
The generic helpers `modify`, `raise`, and `catch` use these packed boundaries;
the runners observe the concrete carriers and remain outside the effect interfaces.
The [library walkthrough](../../../../tutorial/zydeco-guide.md#10-capability-dictionaries) shows a complete adapter.

The operation interfaces leave effect interaction to the chosen carrier and handlers.
The [State and Exception composition contract](effects-and-transformers.md#state-and-exception-composition)
specifies the standard combined instance and contrasts it with a state-preserving failure stack.
