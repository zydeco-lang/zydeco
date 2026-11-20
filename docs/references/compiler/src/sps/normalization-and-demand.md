# Normalization and Consumer Demands

## Normalization Reconstruction

After residual lowering, the unoptimized lexical tree can still contain long binding and closure chains.
The [normalization folder](../../../../../lang/stackir/src/high/normalize/fold.rs) reconstructs values, computations,
and stacks through the shared [resumable folder driver](../compiler-data.md#resumable-folder-execution),
which also runs pattern copying and [pattern decisions](operations-and-patterns.md#pattern-decisions-and-validation).
Frames retain unfinished consumers and their producer environments,
while typed result stacks retain completed syntax and its consumer demands.
Product and branch cursors request one child at a time; split-binding iterators resume their components
in the order required by demand propagation.
Tail calls transfer reductions to surviving bodies without retaining the eliminated wrapper.
Pattern-fact distribution uses an explicit stack; discardability and movable-stack checks iterate
through their relevant children.

The folder preserves the [consumer-demand schedule](#consumer-demands).
A binding installs producer facts before visiting its tail, then resumes with the tail's demands
to discard, split, or rebuild the producer.
Split components consume those demands from last to first, so reconstructed bindings execute in their original order.
Unknown branches retain separate scopes and contribute completed demands before their shared scrutinee is rebuilt.
Primitive continuations follow the same schedule, retaining potentially trapping operations even without a value demand.
Suspended bodies return capture demands without executing their code.

Delayed ambient stacks are stored in a flat table whose IDs refer to both the stack node and its original scope.
This preserves substitutions across nested closure and argument reductions while allowing long substitution chains
to be resolved and destroyed without recursive Rust calls.
The existing environment table and occurrence counts retain their distinct roles
in forwarding and exclusive body movement.
Reconstruction preserves definition identities, source sites, applicable protocol evidence,
and the lexical ownership and branch-join validation boundary.

The bounded normalization behavior fixtures check generated syntax, source provenance, and protocol evidence.
They cover sharing, branch demands, primitive folding, and preservation of potentially trapping operations,
along with empty and wide product, match, and comatch child sequences.
[Depth tests](../../../../../lang/stackir/src/high/normalize/depth_tests.rs) use a 512 KiB stack for discarded
and retained bindings, suspended bodies, ambient substitutions, argument stacks, alias parameters, and unknown branches.
The [CLI regression](../../../../../cli/tests/source_build.rs) builds the factorial source through the Wasm backend
without a compiler worker or Cargo's `RUST_MIN_STACK` environment setting.
These tests cover reconstruction depth; recursively structured semantic facts, demands, and protocol evidence,
as well as later compiler passes, remain separate concerns.

## Local Reductions

The [normalizer](../../../../../lang/stackir/src/high/normalize.rs) records lexical facts
for aliases, literals, products, constructors, and suspended code.
Its principal reductions are:

```text
force (closure • => M) S          ==> M[S/•]
let arg(p) :: • = arg(v) :: S in M ==> let p = v in M[S/•]
return v to (kont p => M)         ==> let p = v in M
kont x => return x to •          ==> •
closure • => force f •           ==> f
```

Forwarding uses a variable `f` or a plain continuation binder.
Known destructor tags select their comatch arm; constructor introductions select their match arm and bind the payload.
Knowing only the tag of a stored value does not expose its payload, so normalization retains the explicit match
and its complete constructor table in that case.
Matching product introductions and patterns split into bindings in evaluation order.
A selected value branch can drop its join guard; an unknown branch retains the shared stack.

Substitution preserves the producer's lexical environment and the captured ambient stack.
It stops at stack binders, including closures, recursion, argument consumers, comatch arms, and branch joins.
Moving a remaining stack requires its argument values to be discardable;
tags recurse through the stack, and continuation bodies stay suspended.
A trapping argument retains its evaluation boundary.
The popped head argument is bound before the consumer executes.

## Sharing and Discardability

A directly forced closure literal reduces. A variable-bound general closure can move into its force only
with one syntactic occurrence; counts conservatively include dead code.
Shared closures stay bound, and aliases do not establish exclusive ownership.
Recursion is never unfolded. Value aliases forward without copying compound values;
nontrivial literals substitute only through singly used bindings, while trivial values can forward freely.
A returned value used repeatedly keeps one binding.

Discardability determines whether an unused producer may disappear.
Variables, literals, trivial values, and suspended closures are discardable;
products and constructors inherit it from their contents.
Arithmetic is discardable only when total and its operands are discardable.
Integer division, remainder, and holes retain possible traps unless literal folding proves successful evaluation.
Executed external calls and `Fix` remain even if their result is unused.
A dead suspended thunk can disappear without inspecting its recursive body.
These conditions preserve the order and multiplicity of effects while local reductions expose further consumers.

## Second-Class Procedures

Sharing keeps a closure that several calls use bound, so each call still opens a closure package
and copies the environment it captured.
The stack-passing-style paper's second-class procedures share such code without a package.
`letproc p • = N in M` binds `N` as code over the stack that each call supplies,
bound to its own `•`, and `p ! S` calls it with a stack built over the ambient one.
A procedure is not a value: it can only be called, every call lies in the tail of its binding,
and a closure or fixed-point body hides the procedures bound outside it, because that body runs at a later call.
The implementation has the paper's minimal, non-recursive form: `p` is not in scope in `N`, so a `fix` stays a closure.

The [fragment validator](../../../../../lang/stackir/src/high/check.rs) checks this scope alongside ownership
and placement.
It rejects a call outside its procedure's tail, including a call from the procedure's own body,
a call under a closure or fixed-point body, and a procedure used as a value.
A call consumes its stack, so under the full scheme it receives `•`.

The source language has no procedures.
The [`localize` pass](../../../../../lang/stackir/src/high/localize.rs) introduces them
by reading the paper's localize equation, `let x = λ_c •. N in M = letproc x • = N in M`, from left to right.
A value let of a closure literal is eligible when its binder is called in the tail
and every occurrence there is the head of a call outside any closure and fixed-point body.
A bottom-up analyzer decides inner lets first: once a closure is localized, its body no longer hides the calls in it,
so a binding whose only obstacle was that closure becomes eligible too.
The rewrite replaces the eligible lets and their calls in place, and the result is revalidated in the program's scheme.
The default plan localizes after normalizing, so a closure called once is inlined rather than localized;
`localize,normalize` hands procedures to the normalizer instead.

Normalization keeps procedures as it keeps shared closures.
A procedure called exactly once moves its body to that call, the paper's procedure β at a single site.
A procedure whose rebuilt tail no longer calls it is dropped, and the others stay bound with their bodies rebuilt
over a fresh `•`; under the full scheme a call names the frames it receives, as other consumers do.
Closure conversion lowers procedures to [labels](../closure-conversion.md#correspondence-with-sps_l).

## Consumer Demands

[Demand analysis](../../../../../lang/stackir/src/high/demand.rs) is the backward component of the same traversal:

| Demand | Observation |
| --- | --- |
| `Absent` | No surviving consumer needs the value. |
| `Fields` | Consumers need specified physical product positions and nested demands. |
| `Used` | An unknown consumer requires the complete value. |

Join unions field positions recursively, and `Used` absorbs other demands.
An empty `Fields` map still observes product shape and differs from `Absent`.
At `let p = V in M`, producer facts first normalize `M`; its free-definition demands are translated
through `p` into a demand on `V`.
The rebuilt producer contributes its own free demands, and demands for bound definitions leave the map.
Independent branches join before the producer is rebuilt.
There is no global demand fixed point or recursive-call specialization.

A known call exposes argument and return bindings before demands are read,
allowing a consumer to prune a passed module value.
Unknown calls observe arguments whole.
Escaping thunk bodies contribute demands on their captures while remaining suspended.
A surviving constructor match observes its scrutinee whole; known selection removes demands from discarded arms.
Forcing a thunk observes that thunk, not a projection of its eventual result.

Logical suffix patterns translate into physical product positions by shifting their nested demands.
A rebuilt suffix spread retains the required product arity even when its individual fields are absent.
Alias members join demands on the same scrutinee; a whole-value use
through one alias defeats selective pruning by another.
Eliminating an unpack can also eliminate its shape demand.
An unobserved field becomes trivial only when its producer is discardable.

Source value and parameter classifiers also contribute the
[partial protocol evidence](../closure-conversion/word-contracts-and-protocols.md#partial-source-protocols) retained
across rebuilding.
Discarded producers lose their evidence; whole retained values and parameters carry it to closure conversion.
Codata tag producers and consumers use the canonical observation numbering defined at that same boundary.
