# Extending the Relative Monad Library

Status: ongoing library programme. The authorized Ret, Reader, Writer, and Option batch is implemented;
its settled design lives in the [language reference](../references/language.md#elementary-effect-instances).
The directions below require separate implementation decisions.
This document proposes library work, not new language semantics.

## Abstract

Zydeco already supports relative monads, algebra translation, and carrier-indexed effect interfaces,
but its reusable effect library covers fewer effects than its examples demonstrate.
This proposal asks which familiar monadic abstractions can become ordinary Zydeco libraries,
which representations preserve the relative-monad laws, and which require additional semantic or runtime support.
It compares established libraries in Haskell, Scala, TypeScript, F#, and OCaml with the existing Zydeco codebase.
The resulting programme separates elementary environment, output, and absence effects from search,
continuation control, parsing, effect syntax, diagnostics, randomness, resource management, and concurrency.
Its principal constraint is that a carrier must compose with ambient computations
without changing their observable order.
In particular, eagerly collecting nondeterministic results
into a returned list does not provide an unrestricted relative monad.
The initial implementation establishes reusable dictionaries, transformer conventions,
and law-oriented tests without introducing a scheduler, a resource runtime, or compiler changes.

## 1. Motivation and Research Question

An effect library should let a program state the operations it needs independently of their implementation.
The current [packed effect interfaces](../references/language.md#packed-effect-instances)
already support this separation for State and Exception.
Their shared `WithMonad` boundary can also accommodate an environment, an accumulated output, or an absent result.
Extending that boundary is more useful than adding isolated examples whose dictionaries,
runners, and composition conventions differ.

The design question is therefore representational rather than taxonomic:
**for each familiar monadic effect, what computation carrier and operational contract make it a lawful, composable relative monad in Zydeco?**
A familiar name supplies evidence of utility, but does not establish that its usual value-level representation survives
an effectful host language.

The proposal has three intended outcomes: a reusable elementary library, an explicit account
of the difficult effect interactions, and an implementation order governed by semantic dependencies.
It does not claim a new categorical construction, a complete survey of functional libraries,
or measured performance improvements.
The source comparison is a purposive sample of documented APIs, not a popularity study.

## 2. Evidence and Existing Implementation

The theoretical starting point is Jiang, Xue,
and New's [*Notions of Stack-Manipulating Computation and Relative Monads*](https://arxiv.org/html/2502.15031v2),
published in PACMPL 9, OOPSLA1, article 100 (2025), [DOI 10.1145/3720434](https://doi.org/10.1145/3720434).
This proposal uses the extended version dated 4 March 2025, especially §§4–6 and its appendices.
The paper supplies the relative-monad and algebra account.
The admission criteria, module design, test plan, and prioritization below are engineering proposals derived
from it and from the repository.

The following sources establish precedents for the candidate families.
API presence is the relevant evidence, not an assertion that the libraries share operational semantics
or implementation costs.

| Family | Representative primary sources | Relevance to Zydeco |
| --- | --- | --- |
| Reader, Writer, State, optional failure, exceptions, RWS, continuations | Haskell [`transformers`](https://hackage.haskell.org/package/transformers) | A compact baseline of effect carriers and explicit transformer composition |
| Reader-like functions, Writer, OptionT, EitherT, Free | Scala Cats [`Kleisli`](https://typelevel.org/cats/datatypes/kleisli.html), [`Writer`](https://typelevel.org/cats/datatypes/writer.html), [`OptionT`](https://typelevel.org/cats/datatypes/optiont.html), [`EitherT`](https://typelevel.org/cats/datatypes/eithert.html), [`Free`](https://typelevel.org/cats/datatypes/freemonad.html) | Dictionary-oriented APIs and reusable effect operations |
| Collections, Writer, inclusive disjunction | TypeScript fp-ts [`Array`](https://gcanti.github.io/fp-ts/modules/Array.ts.html), [`Writer`](https://gcanti.github.io/fp-ts/modules/Writer.ts.html), [`These`](https://gcanti.github.io/fp-ts/modules/These.ts.html) | Familiar application-level uses beyond Haskell |
| Fair search | Haskell [`logict`](https://hackage.haskell.org/package/logict) | Search scheduling and observation deserve an interface beyond list concatenation |
| Parsing | Haskell [`megaparsec`](https://hackage.haskell.org/package/megaparsec), F# [FParsec](https://www.quanttec.com/fparsec/) | Input state, failure, commitment, and diagnostics interact |
| Warning accumulation and validation | Cats [`Ior`](https://typelevel.org/cats/datatypes/ior.html) and [`Validated`](https://typelevel.org/cats/datatypes/validated.html) | Distinguishes dependent sequencing from independent error accumulation |
| Randomness | Haskell [`MonadRandom`](https://hackage.haskell.org/package/MonadRandom) | Separates generator threading from access to ambient entropy |
| Resource scopes and asynchronous computation | Cats Effect [`Resource`](https://typelevel.org/cats-effect/docs/std/resource), OCaml [`Lwt`](https://github.com/ocsigen/lwt) | Demonstrates useful APIs whose guarantees require execution machinery |

Zydeco's current implementation provides a stronger starting point than its exported module inventory suggests.
[State](../../lib/std/effect/state.zy), [Exception](../../lib/std/effect/exception.zy),
and [StateExn](../../lib/std/effect/state-exn.zy) are ordinary libraries over Builtin.
The standard data library already owns [Option](../../lib/std/data/option.zy),
[Result](../../lib/std/data/result.zy), and list operations.
[Transformer examples](../../lib/tests/monadic/trans.zy) implement ReaderT, OptionT,
and a continuation-based optional transformer.
Other examples cover [backtracking](../../lib/tests/monadic/backtrack.zydeco),
[free effects](../../lib/tests/oopsla/free.zydeco), and [delimited control](../../lib/tests/delimcc/reset-shift-k.zy).
These examples establish feasibility of particular encodings, not a stable API or general law certification.

## 3. Semantic Admission Criteria

### 3.1. Carriers, Observations, and Laws

The [language reference](../references/language.md#11-relative-monads) owns `Monad M`,
`Algebra M R`, algebra translation, and the [relative-monad laws](../references/language.md#relative-monad-laws).
For each later candidate, the design must identify a computation carrier, its observation boundary,
and why the dictionary satisfies both unit equations, associativity, and linearity.
A value datatype alone does not answer those questions.

Tests with returned answers establish less than observational equivalence in an effectful host.
An admission review should also account for traces, failure, divergence, and continuation use.
Each effect needs its own operation equations and an explicit runner or handler.
These are semantic obligations that the current dictionary types do not enforce.

### 3.2. Transformer Construction

For closed, lawful relative-monad definitions, the paper derives transformers
by algebra translation ([§5.4](https://arxiv.org/html/2502.15031v2#S5.SS4)).
This is more than replacing the spelling `Ret` with `M`: computation-type parameters require algebra evidence,
and the whole definition must respect the translation boundary.
In particular, importing an effectful function from the surrounding scope is not an implicit lift.
The repository's [closed translation boundary](../references/language.md#closed-translation-boundary) continues
to govern implementation.

For elementary carriers, direct dictionary implementations make the order of operations easy to inspect.
A transformer should provide an explicit lift satisfying the
[monad-morphism contract](../references/language.md#elementary-transformers).
The implemented elementary dictionaries have been tested through translated clients as well as explicit bind.
A general transformer typeclass remains unnecessary for the elementary batch.
Consistent constructors and lift functions can establish the actual common interface before it is abstracted.

### 3.3. The Returned-List Counterexample

The naive candidate `N A = Ret (List A)` first executes its input, then executes a continuation
for each element and concatenates the results.
Suppose the input contains `1` and `2`.
Let `f i` emit `F_i` before returning one result, and let `g` emit `G_i` on that result.
Left-associated bind emits `F_1, F_2, G_1, G_2`, whereas right-associated bind emits `F_1, G_1, F_2, G_2`.
The final list agrees, but the ambient trace differs.

Consequently, the original suggestion to add this carrier as an unrestricted relative monad must be withdrawn.
The paper explicitly identifies the commutativity restriction in [§5.4](https://arxiv.org/html/2502.15031v2#S5.SS4).
A pure finite-list API remains useful, and a commutative-base specialization may be lawful.
Neither justifies exporting a general `Monad` dictionary for returned lists over arbitrary ambient effects.
The more promising unrestricted direction represents search as a protocol whose bind installs continuations
before results are observed.

## 4. Implemented Foundation

The initial batch implements Ret, Reader/ReaderT, Writer/WriterT, and Optional/OptionT.
Their approved carriers, operation contracts, instance packaging, and lift equations now have one home
in the [elementary effect reference](../references/language.md#elementary-effect-instances).
The [library guide](../../lib/std/README.md#relative-monads-and-effect-examples) points
to importable modules and executable clients.
The [standard library organization](../references/language.md#standard-library-organization) assigns these modules
to `effect/` and their general-purpose monoid dictionary to `algebra/`.
Search, StateT, ExceptionT, and the remaining directions below are outside that batch.

This foundation makes the later comparisons concrete: a candidate can be tested over an existing effectful base,
through a closed monadic block, and with the same carrier-indexed packaging.
The shared [Monoid classifier](../../lib/std/algebra/monoid.type.zy) is available for subsequent output
and warning abstractions, subject to the [Writer output contract](../references/language.md#writer-output-contract).

## 5. Subsequent Library Directions

### 5.1. StateT and ExceptionT

The existing concrete instances make these natural follow-ups:

```text
StateT S M A       = S -> M (A * S)
ExceptionT E M A   = M (Result A E)
```

The second equation is a proposed observation format, not permission
to silently replace Exception's current private result representation.
An implementation should reconcile that interface deliberately and update callers in the same change
if it replaces the design.

Transformer order chooses behavior.
`S -> M (Result (A * S) E)` loses the updated state on failure, whereas `S -> M (Result A E * S)` preserves it.
The current StateExn policy is already specified
in the [language reference](../references/language.md#packed-effect-instances).
Future stacking helpers should expose their chosen policy and test handlers
against it instead of treating the two shapes as interchangeable.
Writer/Exception and Writer/Option combinations similarly determine whether an output remains observable on failure.

### 5.2. Continuations and Delimited Control

A continuation carrier `Kont B A = Thk (A -> B) -> B`, with `B : CType`,
expresses a computation by what it does with its result continuation.
Return calls that continuation. Bind passes the first computation a continuation that executes the second.
This fits the computation-kind boundary without wrapping the answer in Ret.

The existing reset/shift examples provide implementation material,
but a public library needs a chosen answer-type discipline and tests for nested delimiters,
repeated continuation invocation, and interaction with failure.
Fixed-answer continuation passing, answer-type polymorphism, and answer-type modification are separate API choices.
A continuation transformer whose answer is `M R` also differs from a Church encoding quantifying
over arbitrary computation answers and their `Algebra M` dictionaries.

The paper's continuation
and exception encodings support exploring these alternatives ([§4.1](https://arxiv.org/html/2502.15031v2#S4.SS1)).
They do not license unrestricted inspection of a continuation stack:
an exposed protocol can admit inhabitants that violate the monad equations.
Before standardization, identify which constructors clients may use and how the representation enforces
or documents the necessary canonicity invariant.
No ambient native stack-capture primitive is proposed.

### 5.3. Finite Nondeterminism, Backtracking, and LogicT

The first search library should build on the existing Church-style backtracking example,
with success and failure continuations or a related demand-driven protocol.
Bind can then compose the search before a runner decides how many results to observe.
Collecting a finite prefix or all finite results becomes an observation, rather than the definition of effectful bind.

Fair search is a further design decision. The documented `logict` interface includes mechanisms
for interleaving and observing search ([package documentation](https://hackage.haskell.org/package/logict)).
A depth-first choice operator should not be called fair merely because it has the same monad operations.
Acceptance tests should distinguish an infinite left branch from a finite right branch
and specify whether each operation interleaves, commits, or exhausts a branch.

This direction needs a bounded observer, ordering tests with ambient effects, and a written account of pruning.
A general collection `flat_map` may remain in the data library without being promoted
to an unrestricted relative-monad dictionary.
Search is explicitly outside the initial implementation goal.

### 5.4. Parser and ParsecT

A minimal parser can combine an input position with failure, but a useful parser library must specify more:
whether a failed branch consumed input, whether choice may retry, and which error is reported.
Megaparsec and FParsec are precedents for treating these as parser semantics rather
than incidental state updates ([Megaparsec](https://hackage.haskell.org/package/megaparsec),
[FParsec](https://www.quanttec.com/fparsec/)).

An initial carrier could thread an immutable input view and cursor through a base monad,
returning either a success with the next cursor or a structured diagnostic with consumption information.
Parser bind is admissible only once those result cases and their propagation satisfy the laws.
Choice, commitment, and error merging need additional equations beyond Monad.

Tests should include an empty parser under repetition, a consumed failure,
explicit backtracking, and a bounded diagnostic comparison.
A tokenizer or parser for configuration files would be a useful consumer.
This proposal does not select byte versus Unicode indexing or a final diagnostic datatype.

### 5.5. Free and FreeT

Free effects separate a program's requested operations from the interpreter that handles them.
Cats provides a familiar value-level precedent ([Free](https://typelevel.org/cats/datatypes/freemonad.html)),
while Zydeco already has effect-specific examples.
The design choice is between inspectable syntax and an elimination-oriented, Church-encoded protocol.

A Church representation can quantify over a result computation and accept an operation handler.
For a transformer, quantifying over an arbitrary computation answer also requires its base-monad algebra.
A tree representation instead needs a signature functor, explicit recursive structure,
and a policy for the effects in interpreting its nodes.
Neither should be admitted merely by analogy with returned lists.

The next experiment should use one shared operation signature and two interpreters,
such as an executable handler and a trace-producing handler.
It should test fold preservation of return and bind, continuation use,
and deep composition before promising generic stack safety.
Inspectability and runtime costs can then guide the public representation rather than being assumed from the name Free.

### 5.6. Ior/These and Validation

Inclusive disjunction represents an error or warning, a successful value, or both.
With an associative warning combination, its bind can preserve earlier warnings while continuing on a value.
A returned carrier `Ret (Ior E A)` is a candidate relative monad,
with the same observational-purity obligation on warning combination as Writer.
Cats and fp-ts document this family ([Ior](https://typelevel.org/cats/datatypes/ior.html),
[These](https://gcanti.github.io/fp-ts/modules/These.ts.html)).

Validation serves a different purpose: combining independent checks to accumulate their errors.
The standard accumulating applicative cannot generally be obtained from the dependent sequencing
of a compatible fail-fast monad ([Cats Validated](https://typelevel.org/cats/datatypes/validated.html)).
It therefore belongs in the broader algebra library, not in this programme's monad count.
The shared warning or error algebra may be reusable, but the sequencing contracts must stay distinct.

### 5.7. Random and RandT

A deterministic random computation can be represented as explicit generator state, `Generator -> M (A * Generator)`.
This is a StateT specialization with domain-specific operations, not a new sequencing mechanism.
A fixed seed then makes tests reproducible.
The Haskell [MonadRandom package](https://hackage.haskell.org/package/MonadRandom) supplies a precedent
for separating random operations from a particular client.

The open decisions concern generator selection, seed encoding, bounded sampling
without bias, and any splitting operation.
Acquiring entropy is a separate runtime capability.
No cryptographic suitability should be claimed without selecting and reviewing an appropriate implementation.
This direction should follow StateT and use golden sequences only with a documented stability policy.

### 5.8. RWS and RWST

Reader, Writer, and State may be combined into `R -> S -> M (A * S * W)`.
Its return and bind reuse the elementary environment, state-threading, and output-combination rules.
Haskell's [transformers](https://hackage.haskell.org/package/transformers) includes RWS variants,
which supports considering a convenient combined API.

The first implementation should establish semantic equivalence with an explicit stack of the component transformers.
A fused representation is justified only if a real client benefits or measurements identify an avoidable cost.
The name RWS alone does not justify duplicating interfaces or assuming improved performance.
The existing carrier-indexed packaging can expose all three operations over one carrier.

### 5.9. Resource Management

Resource composition needs a scope in which acquisition is paired with finalization.
Cats Effect's [Resource](https://typelevel.org/cats-effect/docs/std/resource) documents nested release order
and interactions with failure and cancellation.
These guarantees require more than an accumulated list of cleanup thunks: cleanup itself may fail,
and control may leave a scope without returning normally.

A Zydeco design must first choose whether scope is continuation-based, explicitly bracketed,
or represented by a restricted resource syntax.
It must specify release on normal return and exceptions, partial acquisition,
finalizer failures, and repeated or escaped continuations.
Cancellation requirements depend on the later asynchronous runtime.
A synchronous bracket experiment is reasonable, but it should advertise only the exit modes for
which cleanup is actually guaranteed.

### 5.10. Async and Task

An asynchronous carrier needs an interpreter or scheduler that determines when continuations resume.
OCaml's [Lwt](https://github.com/ocsigen/lwt) illustrates why a usable asynchronous library includes execution
and I/O integration as well as combinators.
A callback-shaped computation type can express part of the protocol, but does not establish single completion,
cancellation behavior, fairness, or structured lifetimes.

The prerequisite design should specify ownership of pending operations, the treatment
of multiple callback invocations, cancellation propagation, and shutdown.
Parallel composition also needs an explicit policy for failure and unfinished siblings.
Until those questions have operational answers, Async/Task should remain a runtime research direction rather
than a thin Monad wrapper in the standard library.

## 6. Validation and Implementation Sequence

### 6.1. Acceptance Criteria

Every admitted module needs source checking, a representative executable client,
and a runner that reveals the promised observation.
The implemented elementary fixtures establish the following pattern for later admission tests:

| Obligation | Positive observation | Counterpart that must be distinguished or rejected |
| --- | --- | --- |
| Shared carrier | Operations and dictionary agree under `WithMonad` | A dictionary or operation from another carrier fails with the intended type diagnostic |
| Monad equations | Both units, associativity, and linearity agree in answers and traces | Reordered, duplicated, or skipped first-computation effects change the expected trace |
| Reader scope | Nested local replacement restores the surrounding environment | A replacement must not persist after its protected computation |
| Writer order | Noncommutative output is combined left to right | Reversed concatenation and duplicated output fail exact observation |
| Optional failure | `Some` continues and `None` skips the continuation | A continuation or fallback that should be skipped must not emit its marker |
| Transformer lift | Lift preserves return, bind, and base-effect order | A lifted computation must not execute twice or lose effects preceding `None` |
| Translation | Explicit bind and a closed `@[monadic]` client agree | Unsupported capture is not used as an implicit effect bridge |

Negative classifier cases should assert the structured error and rejection before lower-stage execution.
Runtime tests should use deterministic traces and exact results rather than only successful termination.
Tests with pure inputs are necessary but insufficient for the returned-list and Writer hazards discussed above.

Focused tests should cover the interpreter while iterating and the affected end-to-end fixtures
on the supported native and WebAssembly backends before claiming cross-backend support.
The full workspace test suite is outside this task unless explicitly requested.
The elementary batch required no dependency or compiler-pass change.

### 6.2. Stages and Decision Gates

1. **Elementary library, implemented:** Ret, Reader/ReaderT, Writer/WriterT,
   and Optional/OptionT provide the [reference baseline](../references/language.md#elementary-effect-instances).
   Finite-list nondeterminism remains excluded.
2. **Composition:** add StateT and ExceptionT, then exercise ordered combinations with Writer and Option.
   Consider RWST after the component behavior is stable.
3. **Protocol-based control:** settle continuation abstraction and backtracking observation.
   Compare inspectable and Church-encoded Free representations, then build parser experiments on explicit state,
   failure, and choice policies.
4. **Domain libraries:** develop diagnostic accumulation and deterministic randomness with real consumers.
   Validation remains an applicative companion.
5. **Runtime-backed effects:** design resource scopes and asynchronous execution only with their supporting failure,
   cancellation, and lifetime contracts.

Each later stage is a proposal, not authorization to expand the initial implementation goal.
Implemented, approved rules should move to their owning reference section, leaving only unresolved work here.
Example-only representations should remain labelled as such until their advertised laws
and target support have been exercised.

## 7. Limitations and Expected Contribution

The paper's semantic development provides the conceptual basis for algebra translation,
but its model theorem is formulated for a restricted core ([§6](https://arxiv.org/html/2502.15031v2#S6)).
It should not be read as a machine-checked proof of every feature in the current compiler
or of arbitrary user-supplied dictionaries.
Likewise, derivability of a transformer does not prove its chosen runtime representation efficient
or its resource operations safe.

The main practical contribution of this programme is a repeatable admission rule:
choose a computation carrier, identify its observable effects, justify the relative-monad laws,
and specify the interaction policy before standardizing operations.
Reader, Writer, and Option provide a small setting in which to establish that discipline.
The same rule then explains why search requires a protocol, why accumulating validation is separate,
and why resource management and concurrency cannot be completed by naming a carrier alone.
