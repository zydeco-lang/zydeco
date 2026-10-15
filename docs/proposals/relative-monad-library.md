# Extending the Relative Monad Library

Status: ongoing library programme.
Ret, Reader, Writer, Option, State, Exception, and ordered depth-first NonDet are implemented. The six effect
transformers share the [derived construction](../references/language.md#derived-relative-monad-transformers);
settled effect contracts live
in the [elementary effect reference](../references/language.md#elementary-effect-instances)
and [search reference](../references/language.md#ordered-depth-first-nondeterminism).
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
The implemented libraries establish reusable dictionaries, transformer conventions, ordered depth-first search,
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
[Either](../../lib/std/data/either.zy), and list operations.
[Transformer examples](../../lib/tests/monadic/trans.zy) implement ReaderT, OptionT,
and a continuation-based optional transformer.
The [backtracking example](../../lib/tests/monadic/backtrack.zydeco) now uses the standard NonDet library.
Other examples cover [free effects](../../lib/tests/oopsla/free.zydeco)
and [delimited control](../../lib/tests/delimcc/reset-shift-k.zy).
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

The [derived transformer reference](../references/language.md#derived-relative-monad-transformers) owns the approved
construction and shared interface, replacing the earlier recommendation to handwrite elementary transformers.
New effects should supply a closed, lawful source implementation and make their external dependencies explicit.
Review must justify the source operations and their interaction contracts;
translating them does not discharge those obligations or certify arbitrary supplied dictionaries.

### 3.3. Search Observations

The [search reference](../references/language.md#lists-and-search-observations) owns the returned-list counterexample
and the distinction between search sequencing and collecting its answers.
Future search representations must preserve that distinction or state the additional assumptions they require.
A finite-list collection API remains useful independently of a relative-monad dictionary.

## 4. Implemented Foundation

The initial batch implements Ret, Reader/ReaderT, Writer/WriterT, and Optional/OptionT.
Their approved effect contracts live
in the [elementary effect reference](../references/language.md#elementary-effect-instances);
the [transformer reference](../references/language.md#derived-relative-monad-transformers) owns shared construction,
packaging, and lift equations.
The [library guide](../../lib/std/README.md#relative-monads-and-effect-examples) points
to importable modules and executable clients.
The [standard library organization](../references/language.md#standard-library-organization) assigns these modules
to `effect/` and their general-purpose monoid dictionary to `algebra/`.
StateT and ExceptionT now use the same shared derivation as the initial transformers.
The standard StateExn instance composes StateT with Exception and retains its established recovery policy.

The subsequent NonDet/NonDetT implementation provides the
[ordered depth-first baseline](../references/language.md#ordered-depth-first-nondeterminism).
Its backtracking client replaces the former eager-list example; that example was list-backed,
not Church-encoded as an earlier version of this proposal claimed.
The unsuitable eager-list strategy remains represented
by a [negative law fixture](../../lib/tests/effects/nondet-law-counterexamples.zy).

This foundation makes the later comparisons concrete: a candidate can be tested over an existing effectful base,
through a closed monadic block, and with the same carrier-indexed packaging.
The shared [Monoid classifier](../../lib/std/algebra/monoid.type.zy) is available for subsequent output
and warning abstractions, subject to the [Writer output contract](../references/language.md#writer-output-contract).

## 5. Subsequent Library Directions

### 5.1. Stack Construction and Effect Lifting

The implemented StateT and ExceptionT provide the component carriers;
the [composition reference](../references/language.md#state-and-exception-composition) owns their interaction policy.
What remains is convenience for constructing stacks and forwarding base operations.
Such helpers should preserve one carrier and dictionary while making any scoped-operation policy explicit.
For example, lifting a throwing operation does not by itself determine how a state-transforming catch restores state.

A future combined helper should eliminate repeated client wiring without concealing which stack it constructs.
Its tests should compare it with explicit composition, including effects
that precede failure and state visible to a handler.
RWST remains a separate candidate below rather than an assumed optimization of every stack.

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

### 5.3. Fair Search, Pruning, and LogicT

Ordered depth-first search and bounded-result observation are implemented;
the [search reference](../references/language.md#ordered-depth-first-nondeterminism) specifies their limits.
Fair search remains a separate design decision. The documented `logict` interface includes mechanisms
for interleaving and observing search ([package documentation](https://hackage.haskell.org/package/logict)).
A depth-first choice operator should not be called fair merely because it has the same monad operations.
Acceptance tests should distinguish an infinite left branch from a finite right branch
and specify whether each operation interleaves, commits, or exhausts a branch.

The next search design needs scheduling and pruning contracts beyond the existing depth-first operations.
Its tests should preserve ambient effect order within each documented traversal policy
and distinguish bounded result counts from termination guarantees.

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
The implemented fixtures establish the following pattern for later admission tests:

| Obligation | Positive observation | Counterpart that must be distinguished or rejected |
| --- | --- | --- |
| Shared carrier | Operations and dictionary agree under `WithMonad` | A dictionary or operation from another carrier fails with the intended type diagnostic |
| Monad equations | Both units, associativity, and linearity agree in answers and traces | Reordered, duplicated, or skipped first-computation effects change the expected trace |
| Reader scope | Nested local replacement restores the surrounding environment | A replacement must not persist after its protected computation |
| Writer order | Noncommutative output is combined left to right | Reversed concatenation and duplicated output fail exact observation |
| Optional failure | `Some` continues and `None` skips the continuation | A continuation or fallback that should be skipped must not emit its marker |
| Search order | Depth-first traversal preserves order and duplicates | Reversed, deduplicated, or eagerly collected branches change answers or traces |
| Search observation | First and bounded observers stop without forcing unused alternatives | A zero bound or an unneeded right branch must not emit its marker |
| Transformer lift | Lift preserves return, bind, and base-effect order | A lifted computation must not execute twice or lose effects preceding `None` |
| Translation | Explicit bind and a closed `@[monadic]` client agree | An opaque captured dictionary is rejected rather than implicitly lifted |
| Generated evidence | NonDet retains the algebra introduced for its answer computation | An algebra-free target classifier is rejected |
| Stack order | StateT/Exception and ExceptionT/State expose their respective failure observations | A handler must not silently switch between entry state and updated state |

Negative classifier cases should assert the structured error and rejection before lower-stage execution.
Runtime tests should use deterministic traces and exact results rather than only successful termination.
Tests with pure inputs are necessary but insufficient for the returned-list and Writer hazards discussed above.

Focused tests should cover the interpreter while iterating and the affected end-to-end fixtures
on the supported native and WebAssembly backends before claiming cross-backend support.
The full workspace test suite is outside this task unless explicitly requested.
The implemented effects required no dependency or compiler-pass change.

### 6.2. Stages and Decision Gates

1. **Elementary library, implemented:** Ret, Reader/ReaderT, Writer/WriterT,
   and Optional/OptionT provide the [reference baseline](../references/language.md#elementary-effect-instances).
   An unrestricted returned-list monad remains excluded.
2. **Ordered search, implemented:** NonDet/NonDetT supplies the
   [depth-first baseline](../references/language.md#ordered-depth-first-nondeterminism).
3. **Derived transformers, implemented:** the common construction covers all six effect families,
   including StateT and ExceptionT with ordered composition tests.
   Stack-construction helpers and RWST remain open.
4. **Protocol-based control:** settle continuation abstraction, fair search, and pruning.
   Compare inspectable and Church-encoded Free representations, then build parser experiments on explicit state,
   failure, and choice policies.
5. **Domain libraries:** develop diagnostic accumulation and deterministic randomness with real consumers.
   Validation remains an applicative companion.
6. **Runtime-backed effects:** design resource scopes and asynchronous execution only with their supporting failure,
   cancellation, and lifetime contracts.

The remaining stages are proposals, not authorization for further implementation.
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
Reader, Writer, Option, State, Exception, and NonDet provide concrete settings in which to establish that discipline.
The same rule explains why search uses a protocol, why accumulating validation is separate,
and why resource management and concurrency cannot be completed by naming a carrier alone.
