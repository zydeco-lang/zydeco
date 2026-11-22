# Ordered Depth-First Nondeterminism

A search can produce an answer before exploring its remaining alternatives.
The [NonDet library](../../../../../lib/std/effect/nondet.zy) represents that remaining work as a continuation,
so bind can explore the descendants of one answer before requesting another.
The standard instances preserve branch order and duplicates and use depth-first traversal.
They provide neither fair interleaving nor implicit pruning or deduplication.

## Carrier and Search Operations

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
`instance` exposes `NonDet`; `transformer_instance M base` exposes `NonDetT M`,
its dictionary, lift, and search operations
through the [common derivation](transformers-and-instances.md#derived-relative-monad-transformers).
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

## Observers and Transformer Interactions

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

## Lists and Search Observations

Collecting answers is an observation, not the representation used by search bind.
The eager candidate `Ret (List A)` executes a continuation on every element before yielding the combined list.
For input `[1, 2]`, let `f i` emit `F_i` and return one result, and let `g` emit `G_i` on that result.
Left-associated bind emits `F_1, F_2, G_1, G_2`; right-associated bind emits `F_1, G_1, F_2, G_2`.
Equal lists therefore do not establish associativity in the presence of ambient effects.
The [relative-monad paper, §5.4](https://arxiv.org/html/2502.15031v2#S5.SS4),
identifies the commutativity restriction on this returned-list representation.
NonDet imposes no such commutativity restriction on its base,
and collecting its answers is not generally a monad-preserving conversion to returned lists.
See the [standard library guide](../../../../../lib/std/README.md#ordered-depth-first-search)
for worked search examples and law tests.
