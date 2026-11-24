# Derived Transformers and Elementary Effects

## Derived Relative Monad Transformers

A transformer should preserve the source effect's sequencing rather than introduce a second implementation of it.
The standard library derives transformers from closed ordinary relative-monad definitions with `@[monadic]`,
following the [relative-monad paper, §5.4, Figures 13–14](https://arxiv.org/html/2502.15031v2#S5.SS4).
The source dictionary and effect operations use ordinary `Ret`, `ret`, and `do`.
Algebra translation supplies their interpretation over a base carrier `M` and its `Monad M` dictionary.
`Ret` itself remains the bootstrap instance.

The [shared transformer interface](../../../../../lib/std/effect/transformer.zy) separates a transformer family
from its result at one base:

```text
Transformer M T = codata
  .monad : Monad T
  .lift  : forall A. Thk (M A) -> T A

MonadTrans F = pi ((M, _) : MonadInstance) . Transformer M (F M)
```

Here `F : (VType -> CType) -> VType -> CType` maps a base carrier to a transformed carrier.
Each effect exports `transformer`, with effect configuration
before one [`MonadInstance`](relative-monads.md#packed-effect-instances) argument `(M, base)`.
For example, `{ ! reader/transformer R }` has type `Thk (MonadTrans (ReaderT R))`,
and `{ ! writer/transformer W output }` has type `Thk (MonadTrans (WriterT W))`.
The witness-dependent arrow opens the caller's chosen base once and retains that identity in both `M`
and `F M` in the result; the implementation may remain an unknown runtime thunk.
The interface constrains the carriers of the dictionary and lift; lawfulness remains a semantic obligation.
For lawful source and base monads, lift preserves return and bind:

```text
lift (return_M a)       ≃ return_T a
lift (bind_M m f)       ≃ bind_T { lift (force m) } { x => lift (f x) }
```

The common [source helper](../../../../../lib/std/effect/transformer-source.zy) pairs a source dictionary
with `lift A computation = do a <- ! computation; ! source_monad .return A a`.
Import that helper inside the closed translated payload together with the source implementation.
Translation acts on both, generating the transformed dictionary and lift at once.
It does not inspect an opaque dictionary passed from outside the annotation.
Dependencies must be bound as parameters inside the payload, as required
by the [closed translation boundary](../../../language/src/translations/monadic-blocks.md#closed-translation-boundary).

The adapter's input classifier is itself derived by translating `fn (T : VType -> CType) => Transformer Ret T`
as a type root.
This keeps the translated monad and lift signatures synchronized with their ordinary definitions.
Each standard effect similarly shares one closed, module-local source carrier,
such as `ReaderSource` in [Reader](../../../../../lib/std/effect/reader.zy)
or `NonDetSource` in [NonDet](../../../../../lib/std/effect/nondet.zy),
between its public type derivation and its implementation translation.
Configured effects reorder the generated type arguments to keep configuration before the base carrier.

`from_monadic` supplies the trivial `Top` arguments introduced for value-type quantifiers
on the generated monad and lift.
Each effect similarly adapts its generated operation interface.
These adapters retain meaningful structure evidence: in particular, a computation-type quantifier
in the source introduces an `Algebra M` argument that cannot be discarded.
The NonDet source quantifies over a computation answer `R` without an algebra parameter;
translation produces the algebra-bearing [public carrier](nondet.md#carrier-and-search-operations).
The public concrete instance specializes the derived carrier to `Ret`, which need not be definitionally identical
to the untransformed source carrier.

A `transformer_instance` value function exposes the manifest transformed carrier, one `monad` dictionary, its `lift`,
and the effect's own operations. `Lift M T` abbreviates the lift classifier above; `ITransformer M T` contains only
`lift : Thk (Lift M T)`. It can combine with effect bundles under `WithMonad`, without another carrier or monad
dictionary. An ordinary effect client receives an explicit projection of the fields its interface requests; for example,
pass `(T, #monad = t/monad, #state = t/state)` to `WithMonad (IState S)`. A lift-aware client instead uses
`WithMonad (ITransformer M)` or a composition of that bundle with effect operations. Concrete `instance` builders
specialize to `Ret` and project the ordinary effect interface. The computation-level `transformer` is a view of the same
derived instance, not a separate dictionary implementation. It opens its packed base and forwards the curried `M base`
arguments to `transformer_instance`. The generated `@[monadic]` interface also remains curried; packaging belongs to the
library boundary, with no definitional equality between the packed and curried classifiers
([witness-dependent classifiers](../../../language/src/polymorphism-and-packed-values.md#universal-polymorphism-and-dependent-classifiers)).

External dependencies remain explicit at the translation boundary.
Writer supplies its source with an empty value and an append operation;
the module adapts the existing `Ret`-returning append by running it and returning its answer through the base monad.
This retains the [Writer output contract](#writer-output-contract),
rather than accepting arbitrary effectful base-monad append operations.
NonDet imports the concrete data and primitive packages directly through `let`,
preserving one shared `List` identity across consumers without requiring a `DataPackage` parameter.

## Elementary Effect Instances

The [effect library](../../../../../lib/std/effect) exports elementary effects independently of OS capabilities.
`ret.zy`, `reader.zy`, `writer.zy`, `option.zy`, `state.zy`, `exception.zy`,
`state-exn.zy`, and [`nondet.zy`](../../../../../lib/std/effect/nondet.zy) are provider-independent
concrete packages requiring no `Builtin` argument.
Except for the bootstrap `Ret` instance, each concrete instance specializes its derived transformer
to the reusable `Monad Ret` dictionary.
`Ret` is the ambient computation carrier, not a separate pure identity effect.
These modules are imported explicitly rather than aggregated by `std.zy`.

| Module | Concrete carrier | Instance entry point | Carrier-indexed operation bundle |
| --- | --- | --- | --- |
| [`ret.zy`](../../../../../lib/std/effect/ret.zy) | `Ret` | `instance` | Only `monad` |
| [`reader.zy`](../../../../../lib/std/effect/reader.zy) | `Reader R A = R -> Ret A` | `instance R` | `IReader R M`: `reader : Thk (MonadReader R M)` |
| [`writer.zy`](../../../../../lib/std/effect/writer.zy) | `Writer W A = Ret (A * W)` | `instance W output` | `IWriter W M`: `writer : Thk (MonadWriter W M)` |
| [`option.zy`](../../../../../lib/std/effect/option.zy) | `Optional A = Ret (Option A)` | `instance` | `IOption M`: `option : Thk (MonadOption M)` |
| [`state.zy`](../../../../../lib/std/effect/state.zy) | `State S A = S -> Ret (A * S)` | `instance S` | `IState S M`: `state : Thk (MonadState S M)` |
| [`exception.zy`](../../../../../lib/std/effect/exception.zy) | `Exception E A = Ret (Either E A)` | `instance E` | `IException E M`: throw and catch at `M` |

The value `Option` is the existing [data-library type](../../../../../lib/std/data/option.zy).
`Optional` names its computation carrier, so a client does not confuse a value datatype with a relative monad.
Concrete builders disclose their carriers.
As with State and Exception, open an instance and repack `(M, operations)` for a client taking `WithMonad`
of the corresponding bundle.

Reader's `.ask` returns the current environment.
Its `.local A replacement computation` runs only the protected computation with a replacement environment.
The surrounding environment is restored after that computation.
The replacement is a value, not an environment-mapping function.
`run_reader R A environment computation` supplies the initial environment and returns `Ret A`.

Writer's `.tell output` emits an output with a Unit answer.
Its `.listen A computation` returns the protected computation's answer and output together,
while retaining that output in the surrounding Writer computation.
Bind combines the first computation's output before the continuation's output.
`run_writer W A computation` returns the answer and accumulated output as `Ret (A * W)`.

Optional's `.none A` produces an absent answer.
Bind runs its continuation on `Some` and propagates `None` without running it.
Its `.or_else A computation fallback` executes the fallback only after `None`.
`run_option A computation` returns the existing `Option A` observation.
Short-circuiting and fallback do not undo effects already performed by the protected computation.
These operations make no fairness or distributivity promise.

### Writer Output Contract

[`Monoid W`](../../../../../lib/std/algebra/monoid.zy) contains `empty : W` and `append : Thk (W -> W -> Ret W)`.
For Writer, append must be total and observationally pure, associative, and have `empty` as a two-sided identity.
Commutativity is unnecessary: output order is significant. The computational operation supports runtime-selectable
and recursive implementations beyond the current total value-function fragment,
but its classifier cannot establish the purity or algebraic obligations.

Purity matters because reassociating Writer bind changes when intermediate outputs are combined.
An append that prints can move those prints relative to user computations even
if its returned values obey the ordinary monoid equations.
Such a dictionary is well-typed but outside the Writer contract.
String concatenation with the empty string is a useful noncommutative instance.
No complexity guarantee is imposed on a caller's append implementation.

### Elementary Transformers

The transformer carriers retain the underlying base computation:

```text
ReaderT R M A    = R -> M A
WriterT W M A    = M (A * W)
OptionT M A      = M (Option A)
StateT S M A     = S -> M (A * S)
ExceptionT E M A = M (Either E A)
```

Exception uses the data library's shared [`Either E A`](../organization.md#either-values),
with `Left E` for errors and `Right A` for successful values.
The builders share the [derived transformer interface](#derived-relative-monad-transformers):

| Module | Packed builder | Base-aware observation |
| --- | --- | --- |
| Reader | `transformer_instance R M base` | `run_reader_t R M A environment computation : M A` |
| Writer | `transformer_instance W output M base` | `run_writer_t W M A computation : M (A * W)` |
| Option | `transformer_instance M base` | `run_option_t M A computation : M (Option A)` |
| State | `transformer_instance S M base` | `run_state_t S M A initial computation : M (A * S)` |
| Exception | `transformer_instance E M base` | `run_exception_t E M A computation : M (Either E A)` |

Here `base : Thk (Monad M)` and `output : Monoid W`.
Each builder supplies its own effect operations; it does not automatically lift other interfaces from the base.
The instance's `lift A computation` ignores Reader's environment, attaches Writer's empty output,
wraps Optional's answer in `Some`, preserves State's state, or wraps Exception's answer in `Right`, respectively.
The runners expose the carrier equations without introducing additional sequencing policies.

Base effects preceding `None` remain observable in OptionT.
For example, `OptionT (Writer W) A` can retain output with an absent answer,
whereas `WriterT W Optional A` has no returned output when its base computation produces `None`.
This difference follows from the carrier equations, not from a common rollback policy.

### State and Exception Composition

StateT's `.get` and `.put` use the current state, and bind passes the state returned
by the first computation to its continuation.
ExceptionT's `.throw` returns `Left`; bind skips its continuation on that case.
Its `catch` runs a handler only for `Left` and preserves a successful `Right` without invoking the handler.
Ambient and base effects already performed remain observable through recovery.

The standard combined carrier is `StateExn S E A = StateT S (Exception E) A`, or `S -> Ret (Either E (A * S))`.
It composes the derived State transformer with Exception rather than maintaining another bind implementation.
Failure discards the updated state, and its catch handler restarts from the state at entry to the protected computation.

Reversing the stack gives `ExceptionT E (State S) A = S -> Ret (Either E A * S)`.
The state remains observable on failure, and a catch handler continues with that updated state.
Neither policy follows from the combined operation interface alone.
See the [standard library guide](../../../../../lib/std/README.md#relative-monads-and-effect-examples)
for worked examples and law tests.
