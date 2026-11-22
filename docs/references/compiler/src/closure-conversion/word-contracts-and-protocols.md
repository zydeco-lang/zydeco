# Word Entry Contracts and Partial Source Protocols

## Word Entry Contracts

Closure conversion introduces words that have no corresponding source argument:
a closure's captured environment and a continuation's saved environment.
Making their roles explicit lets callers and code entries agree before assembly lowering chooses stack operations.
A returned source value also has an explicit entry role.
The remaining source arguments and effects still use the residual stack.

Each `Block` declares [EntryParameters](../../../../../lang/stackir/src/low/entry.rs),
and each `Jump` supplies an `EntryArgument` before its residual stack:

| Entry kind | Parameters in consumption order | Word supplied by the jump | Residual stack at the jump |
| --- | --- | --- | --- |
| Closure | Environment | Environment from the closure record | Ordinary argument/effect stack |
| Continuation | Result, environment | Returned result | Saved environment followed by the caller's stack |

Every listed parameter occupies one ordinary target value word, including a product or buffer handle.
`EntryParameters::words` fixes their order for both ZASM lowering and direct SPS Wasm emission.
The block's patterns bind these parameters before its body executes;
ordinary `LetArg` nodes in the body consume subsequent user arguments.
`OpenContinuation` restores the residual stack, so returning supplies only the result word.
The explicit entry forms replace administrative `LetArg` prologues and `Arg` prefixes in SPSLow.
Their lowering preserves the existing physical word convention.

The [entry verifier](../../../../../lang/stackir/src/low/contracts.rs) checks a code value's origin
and entry kind at each record construction and jump.
A block and its recursive self label carry the declared kind.
Known unit/product environments must agree with a direct block's outer environment arity;
unknown shapes remain subject to upstream source typing.
This is a partial shape check, not reconstruction of the erased source types or their field classifiers.

Opening a closure establishes a local association between its code and its whole environment.
An indirect jump or repackaging must preserve that association.
Opening a continuation similarly associates its code with the restored residual stack;
returning or repackaging must use that same residual stack.
Code from one opening cannot consume another opening's environment or continuation stack,
even if both records happen to have the same shape.
The verifier propagates evidence through whole-value aliases and projections of known complete products.
Destructuring an opaque environment does not establish permission to reconstruct an equivalent one.
Stack operations retain evidence when their known pushes and pops cancel; a remaining prefix or a pop
into the opaque restored stack loses the required agreement.

These checks assume source typing and closure conversion establish the shapes of dynamically obtained records.
The administrative checks are supplemented by the partial source protocol checks below.
Neither establishes continuation lifetime or reconstructs complete source typing.
Host and C external calls retain their own upstream signatures and transfer contracts.
Local representation policies must preserve the ordinary word transport at this boundary;
source storage alignment and padding alone cannot select a different call layout.

For native retained-frame lowering, `ContinuationEntry` additionally records the result pattern,
body, and ordered capture bindings.
The verifier compares that metadata with the explicit continuation entry and its record
before C11 replaces portable captures with retained slots.
There is no second executable entry prologue to infer or keep synchronized.

## Partial Source Protocols

The compiler retains a partial description of the source stack protocol so
that known call components can be checked after normalization.
This description follows [Ret and stack extent](../../../language/src/computations-and-control.md#ret-and-stack-extent):
an installed continuation hides its saved residual stack, and there is no end-of-frame constructor.
In particular, an argument prefix followed by `cont(A)` does not bound the stack extent or allocation lifetime.

[ValueProtocol and StackProtocol](../../../../../lang/stackir/src/protocol.rs) are owned IR data extracted
from checked classifiers.
Values retain unit, primitive, product, and thunk structure when known.
Codata interfaces retain observation alternatives in an owned `ProtocolGraph` shared by the SPS and SPSLow arenas.
Disclosed seals and applied type functions can lead back to existing graph instances.
Unresolved first-order value and computation witnesses retain program-local parameter names and their kinds.
Universal binders retain their scope in the descriptor, while captured parameters can occur freely in local entries.
Unsupported higher-kinded applications, existential carriers, and other unsupported forms contribute unknowns.

| Stack descriptor | Interpretation |
| --- | --- |
| `?` | Unknown protocol; no statement about whether the stack is empty or how large it is |
| `aN` | A named computation parameter; repeated occurrences retain a relationship |
| `forall aN . S` | A source type binder around `S`, consuming no runtime stack component |
| `A :: S` | One argument classified by `A`, followed by protocol `S` |
| `cont(A)` | An installed continuation accepting `A`, with its saved stack hidden |
| `pN` | A program-local reference to a codata interface's observation alternatives |
| `.d#i :: S` | A producer has supplied observation `.d`, with runtime tag `i`, above remainder `S` |

Each codata instance receives a graph reference before its observations are translated.
Recursive occurrences with the same captured arguments reuse that reference,
including cycles through argument prefixes or thunk components.
This keeps a finite description even when executions accumulate an unbounded number of observations and arguments.
The graph records no physical stack extent.

The [source extractor](../../../../../lang/stackir/src/protocol/source.rs) interprets disclosed type-function
applications against the frozen checked arena, without allocating substituted types or rerunning the checker.
A source expression carries lexical bindings for the witnesses that its supported structure uses freely.
Applying a checked type abstraction binds the argument to its witness;
a named binder projects its payload, while a plain binder keeps the whole argument.
Nested abstractions capture the outer arguments they use.
Known named projections, value products, thunk protocols, and computation arguments use these bindings as well.
For example, `Stream A R = codata .item : A -> Stream A R; .done : R end` retains an `Int` payload
and `cont(Int)` result when applied to `Int` and `Ret Int`.
Applying the same family to `Char` preserves a separate instance.

Instance keys contain the source codata identity and its captured source expressions and bindings.
They do not use partial protocol compatibility to equate arguments; distinct unresolved witnesses remain distinct keys.
Only an existing exact instance closes a recursive edge.
When a source codata is already being translated under different arguments,
a new application contributes unknown instead of starting another specialization.
For `Growing A` whose next observation requires `Growing (A * A)`,
the current observation retains its known payload and the changing tail remains opaque.
The same guard covers growth through returned thunks.
Repeated source nodes along unguarded reduction or value/argument paths also stop conservatively.
These guards impose no unfolding-depth limit, but can lose evidence for finite nested applications too.
General nonregular recursion remains outside this instantiation procedure.

Observation indices are the zero-based ranks of complete destructor names in lexicographic order within the interface.
SPS lowering obtains both pushed tags and case tags from the descriptor's canonical ordering.
Consequently, structurally equal codata interfaces agree on runtime indices even
when their source declarations list observations in different orders.
Both the name and index participate in low protocol checking.

SPS lowering records value and pattern protocols, recursive entry protocols, and the interface of each codata case.
Normalization copies facts for retained whole values and patterns into the fresh arena;
discarded values and pruned product fields do not retain stale whole-value facts.
Surviving cases keep their interface, and rebuilding shares the immutable graph while preserving its references.
Primitive results can recover their scalar protocol from the typed operation itself.
Closure conversion transfers these facts to the low arena and records an `EntryProtocol` for each block.
Closures retain their incoming source stack protocol, and continuation entries retain the accepted value protocol.
Recursive self labels inherit the same entry descriptor as their block.
Localization keeps a procedure's stack protocol from the closure it replaces, and conversion records it
for the procedure's label, under an unknown argument for each variable that a closed label's jumps push.
These descriptions come from checked classifiers, not a count of leading `LetArg` nodes.

The [protocol verifier](../../../../../lang/stackir/src/low/protocols.rs) checks known components
before SPSLow is published.
It compares a closure's protocol, and a label's, with the supplied argument stack,
and a continuation's accepted value with the result delivered to it.
Entry kinds agree with administrative roles; known incoming parameter protocols agree
with their retained source pattern classifiers.
Bindings, aliases, product projections, and record openings propagate local evidence.
An opened thunk supplies its code's protocol; an opened continuation supplies the code's accepted value protocol
while its restored stack becomes opaque to this analysis.
The separate provenance check still associates that code with the exact restored stack.

For a supplied observation, the verifier selects that alternative from the expected codata descriptor
and checks its remainder.
A retained case checks its supplied stack against the declared interface, validates branch coverage and tag indices,
and checks each branch against the selected observation's protocol.
This branch evidence remains available even when the ambient stack became opaque at a continuation opening.
SPSLow publication validates graph and parameter references, including parameter kinds, before examining transfers.

The [agreement checker](../../../../../lang/stackir/src/protocol/agreement.rs) compares the whole transfer
with a fresh constraint set.
Each side has its own parameter namespace, and each universal occurrence gets a fresh scope.
Within a scope, repeated occurrences of a parameter share constraints.
For example, `forall A . A -> Ret A` agrees with `Int :: cont(Int)` but rejects `Int :: cont(Char)`.
Two separately quantified thunk components can each instantiate the same source binder differently.
A computation parameter can relate a callback's required protocol to the stack supplied after it.
The SPS verifier skips leading universal binders when inspecting the next runtime stack component;
these binders never add a physical stack word.

Parameter constraints retain every known partial shape rather than selecting one representative.
For example, observations of `(?)`, `(Int)`, and `(Char)` for one value parameter must still reject:
the unknown field cannot discard the later concrete conflict.
Ordinary unknown occurrences remain independent gaps, including repeated visits to an unknown codata payload.
For a computation parameter, different supplied tags can select different alternatives of the same interface;
a known complete interface must admit each supplied observation with its proper index and remainder.

Comparison builds finite local graphs keyed by codata reference and relevant captured parameter bindings.
Already compared graph components close recursive comparisons, while every other observation remains checked.
Parameters bound outside a recursive interface stay related across its observations.
A universal binder introduced inside a codata interface is retained in the published descriptor,
but its parameter positions are treated as unknown during agreement.
Fresh instantiation at each observation needs a further comparison extension;
reusing one inferred argument for every visit would reject valid polymorphic observations.
Fixed components and recursive observation coverage remain checkable under this conservative treatment.
Changing captured bindings along a recursive comparison also leaves an opaque component.
There is no unfolding-depth limit or runtime iteration bound.

Unknown components are compatible with the existing word transport, and only known conflicts are rejected.
This relation is nontransitive; successful agreement cannot establish type equality or authorize a different ABI.
Abstract storage-carrier identity remains an upstream source typing property; agreement checks consistent known shapes,
without reconstructing the explicit type arguments erased before SPS or proving parametricity of a generic body.
Those remain upstream typing obligations. Layout/reference-map evidence is not reconstructed.
Host and C external transfers keep their existing signature checks.
No frame-size, frame-lifetime, or stack-scanning plan is derived from these descriptors.

`zydeco build --target zir` displays entries such as `closure[Int :: cont(Int)]`
and `continuation[Thk(Int :: cont(Int))]` alongside their administrative word parameters.
It also prints finite definitions such as:

```text
[protocol:p0] codata { .done#0: cont(Int); .item#1: Int :: p0; }
```

Parameter declarations print their kinds, for example `[parameter:a0] VType` and `[parameter:a1] CType`.
A polymorphic relay can retain an entry such
as `closure[forall a0 . forall a1 . Int :: a0 :: a0 :: Thk(a0 :: a0 :: a1) :: a1]`.
These names describe source relationships and convey no representation size or allocation policy.

[Protocol regressions](../../../../../lang/tests/tests/suite/stack_protocols.rs) check those surviving descriptions,
rejected argument/result and observation conflicts, graph integrity, and a source computation
that accumulates a runtime-dependent number of argument/tag frames before its installed continuation.
That program dynamically selects and returns its codata consumer, and also calls a returned arithmetic worker
through a recursive computation-polymorphic forwarder on all backends.
The [declaration-order regression](../../../../../lib/tests/core/codata-order.zy) exercises equal structural interfaces
with reversed source ordering on all backends.
The [parameterized regression](../../../../../lib/tests/core/parameterized-protocols.zy) instantiates one
recursive family with both `Int` and `Char`, including a dynamically selected and returned stream thunk.
Low mutation tests reject an incompatible payload after the first recursive observation.
The [growing-family regression](../../../../../lib/tests/core/growing-protocols.zy) checks
that conservative extraction still permits successive observations with larger product types on all backends.
The [symbolic relay](../../../../../lib/tests/core/symbolic-protocols.zy) calls one recursive worker
at different value and computation types on all backends.
Its low mutation test rejects conflicting arguments for one parameter.
