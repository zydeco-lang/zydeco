# Effect Instances and Transformers

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
by the [closed translation boundary](algebra-translation.md#closed-translation-boundary).

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
translation produces the algebra-bearing [public carrier](#ordered-depth-first-nondeterminism).
The public concrete instance specializes the derived carrier to `Ret`, which need not be definitionally identical
to the untransformed source carrier.

A `transformer_instance` value function exposes the manifest transformed carrier,
one `monad` dictionary, its `lift`, and the effect's own operations.
`Lift M T` abbreviates the lift classifier above; `ITransformer M T` contains only `lift : Thk (Lift M T)`.
It can combine with effect bundles under `WithMonad`, without another carrier or monad dictionary.
An ordinary effect client receives an explicit projection of the fields its interface requests;
for example, pass `(T, #monad = t/monad, #state = t/state)` to `WithMonad (IState S)`.
A lift-aware client instead uses `WithMonad (ITransformer M)` or a composition of that bundle with effect operations.
Concrete `instance` builders specialize to `Ret` and project the ordinary effect interface.
The computation-level `transformer` is a view of the same derived instance, not a separate dictionary implementation.
It opens its packed base and forwards the curried `M base` arguments to `transformer_instance`.
The generated `@[monadic]` interface also remains curried; packaging belongs to the library boundary,
with no definitional equality between the packed and curried classifiers (§9).

External dependencies remain explicit at the translation boundary.
Writer supplies its source with an empty value and an append operation;
the module adapts the existing `Ret`-returning append by running it and returning its answer through the base monad.
This retains the [Writer output contract](#writer-output-contract),
rather than accepting arbitrary effectful base-monad append operations.
NonDet keeps its shared data package and list observations outside the translated search core,
preserving the caller's existing `List` witness.

The [uniform-interface fixture](../../../../../lib/tests/effects/transformer-operations.zy) exercises
all six transformer families through `MonadTrans` and a packed lift-aware client.
The [classifier tests](../../../../../lang/tests/tests/transformers.rs) pair closed source derivation
and matching carrier witnesses with rejection of captured dictionaries, mismatched lifts,
and missing answer-type algebras.

## Elementary Effect Instances

The [effect library](../../../../../lib/std/effect) exports elementary effects independently of OS capabilities.
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

Exception uses the data library's shared [`Either E A`](../sources/standard-library.md#either-values),
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

The [operation fixture](../../../../../lib/tests/effects/effect-operations.zy) exercises scoped environments,
noncommutative output, lazy fallback, and transformer order.
The [law fixture](../../../../../lib/tests/effects/effect-laws.zy) checks the four monad equations
and lift laws using an ambient mutable trace.
Its [counterparts](../../../../../lib/tests/effects/effect-law-counterexamples.zy) detect repeated execution
and effectful append despite successful typechecking.
The [translated clients](../../../../../lib/tests/effects/effect-monadic.zy) demonstrate closed adapters,
including the structure arguments required for polymorphic operations.
These finite tests are regression evidence, not proofs for arbitrary dictionaries.

### State and Exception Composition

StateT's `.get` and `.put` use the current state, and bind passes the state returned
by the first computation to its continuation.
ExceptionT's `.throw` returns `Left`; bind skips its continuation on that case.
Its `catch` runs a handler only for `Left` and preserves a successful `Right` without invoking the handler.
Ambient and base effects already performed remain observable through recovery.
The [derived-law fixture](../../../../../lib/tests/effects/transformer-laws.zy) checks both ordinary instances,
their transformers over Writer, and their lifts with exact ambient traces.

The standard combined carrier is `StateExn S E A = StateT S (Exception E) A`, or `S -> Ret (Either E (A * S))`.
It composes the derived State transformer with Exception rather than maintaining another bind implementation.
Failure discards the updated state, and its catch handler restarts from the state at entry to the protected computation.
The [combined example](../../../../../lib/tests/effects/state-exception-stack.zy) tests this policy.

Reversing the stack gives `ExceptionT E (State S) A = S -> Ret (Either E A * S)`.
The state remains observable on failure, and a catch handler continues with that updated state.
The [translated stack client](../../../../../lib/tests/effects/transformer-monadic.zy) distinguishes these policies
through the same packed operations;
the [short-circuit fixture](../../../../../lib/tests/monadic/nominal-short-circuit.zy)
also admits the state-preserving carrier.
Neither policy follows from the combined operation interface alone.

## Ordered Depth-First Nondeterminism

A search can produce an answer before exploring its remaining alternatives.
The [NonDet library](../../../../../lib/std/effect/nondet.zy) represents that remaining work as a continuation,
so bind can explore the descendants of one answer before requesting another.
The standard instances preserve branch order and duplicates and use depth-first traversal.
They provide neither fair interleaving nor implicit pruning or deduplication.

The module exports a `make` value-function factory taking Builtin
and then the caller's shared [`DataPackage`](../../../../../lib/std/data/package.zy).
Its list inputs and observations use that package's `List` witness and operations;
the factory does not create a separate list type.
The carriers are:

```text
NonDetT M A = forall (R : CType) .
  Thk (Algebra M R) -> Thk (A -> Thk R -> R) -> Thk R -> R
NonDet A = NonDetT Ret A
```

The observer chooses an answer protocol `R` and supplies its base-monad algebra.
The success continuation receives one answer and a thunk for the remaining search at `R`;
the failure continuation describes exhaustion.
Bind replaces success with a continuation that explores the selected answer's descendants,
passing the remaining alternatives as their failure continuation.
The rest is an ordinary reusable thunk, not a linear or memoized cursor.
Repeated forcing repeats its computation and effects; the standard observers force each visited rest once.

`MonadNonDet M` has `.empty : forall (A : VType) . M A`
and `.choice : forall (A : VType) . Thk (M A) -> Thk (M A) -> M A`.
`INonDet M` contains `nondet : Thk (MonadNonDet M)` and combines with the shared monad dictionary
under `WithMonad INonDet`.
`instance` exposes `NonDet`; `transformer_instance M base` exposes `NonDetT M`, its dictionary,
lift, and search operations through the [common derivation](#derived-relative-monad-transformers).
The shared constructor takes `base : Thk (Monad M)` even though search sequencing and choice compose continuations.
The observation supplies the algebra used to interpret lifted base computations.

The standard `.choice A left right` starts with `left` and suspends `right`
until the left search and its descendants are exhausted.
`.empty A` invokes failure without producing an answer.
`choose instance A values` visits the supplied list in order, including duplicates;
`guard instance condition` produces one Unit answer for true and no answer for false.
Choice is associative with empty as a two-sided identity.
Bind distributes over choice in its first argument and skips its continuation after empty.
These laws do not imply commutative or idempotent choice, or that discarding all answers undoes effects.

The concrete observers are:

| Operation | Observation | Exploration boundary |
| --- | --- | --- |
| `observe_first A computation` | `Ret (Option A)` | Stops at the first answer; returns `None` on exhaustion |
| `observe_many A limit computation` | `Ret (List A)` | Stops after at most `limit` answers; a nonpositive limit does not force the computation |
| `observe_all A computation` | `Ret (List A)` | Explores to exhaustion, retaining order and duplicates |

The bound in `observe_many` limits answers, not execution steps.
An infinite or diverging left branch can starve later branches; even `observe_first` may therefore diverge.
`observe_all` requires the explored search to terminate to return its list.
Stopping after an answer does not force the rest merely to determine whether another answer exists.

Each `_t` observer takes `M base` before the concrete observer's arguments
and returns the corresponding `M (Option A)` or `M (List A)`.
Here `base : Thk (Monad M)` constructs the algebra for that observer's protocol.
The instance's `lift A computation` uses the algebra passed to the search
to execute the base computation and yield its answer.
With lawful base dictionaries and algebras it preserves return, bind, and base-effect order.
Custom observers that call the carrier directly must supply a lawful `Algebra M R`.

Explored ambient and base effects remain observable when a branch later produces no answers.
For example, `NonDetT (Writer W)` accumulates the output of visited branches, including failed branches,
whereas `WriterT W NonDet` attaches output to each surviving answer.
A failure in the base carrier is distinct from search exhaustion: lifting `None`
from Optional propagates that failure rather than trying the next search alternative.
No operation automatically rolls back state or other effects.

### Lists and Search Observations

Collecting answers is an observation, not the representation used by search bind.
The eager candidate `Ret (List A)` executes a continuation on every element before yielding the combined list.
For input `[1, 2]`, let `f i` emit `F_i` and return one result, and let `g` emit `G_i` on that result.
Left-associated bind emits `F_1, F_2, G_1, G_2`; right-associated bind emits `F_1, G_1, F_2, G_2`.
Equal lists therefore do not establish associativity in the presence of ambient effects.
The [relative-monad paper, §5.4](https://arxiv.org/html/2502.15031v2#S5.SS4),
identifies the commutativity restriction on this returned-list representation.
NonDet imposes no such commutativity restriction on its base,
and collecting its answers is not generally a monad-preserving conversion to returned lists.

The [operation fixture](../../../../../lib/tests/effects/nondet-operations.zy) checks ordering,
duplicate preservation, repeated observation, lazy branches, and bounded observation of a productive infinite search.
The [law fixture](../../../../../lib/tests/effects/nondet-laws.zy) checks branching monad
and choice laws, linearity, and lifts over Reader and Writer.
The [returned-list counterexample](../../../../../lib/tests/effects/nondet-law-counterexamples.zy) is well-typed
but distinguishes the associativity traces above.
[Transformer interactions](../../../../../lib/tests/effects/nondet-transformers.zy) and
[translated clients](../../../../../lib/tests/effects/nondet-monadic.zy) exercise the other boundaries described here.
