# Demand Analysis

Every backend lowers one checked root computation on demand, so intermediate
representations never contain code that is unreachable from the root. Two size sources
survive that property. First, a Zydeco program is one term: every top-level definition of
every transitively imported file sits on the root's binding spine and is therefore
syntactically reachable, so unused user definitions are lowered and emitted. Second, and
much larger, the host Builtin package is materialized from its *signature*: the plan walks
the package type and turns every operation entry into a closure program plus an extern
declaration, regardless of what the program uses. The echo-sum example, which calls five
operations, emitted 135 labeled closure programs and 131 externs before this analysis
existed, because the numeric towers of the standard library all ride along inside the
package.

Plain reference liveness does not fix the second source. The package construction
references every field, so every operation is reachable from the root through the package
value; nothing is dead in the liveness sense. What distinguishes the five used operations
is not reachability but *how* the package is consumed: the program projects only certain
positions, at certain depths. The pass described here therefore computes demands — for
each binding, which product positions of its value are ever observed — and uses them to
skip dead bindings and to materialize only the demanded part of the host package.

The analysis lives in the stack IR crate (`lang/stackir/src/sps/demand.rs`) and runs once
per root when the lowerer is constructed. This placement is deliberate. The interpreter is
the reference semantics and keeps linking the whole program unchanged; the analysis
session used by the editor needs the full checked program for completion and diagnostics.
Compilation is the only consumer, and its two decision points — the `Let` arms of the
SPS lowerer and the Builtin package materializer — already read the checked arena
through the lowerer. A table shared through the statics crate would make the incremental
check pay for a compilation-only concern, and rewriting the spine to drop dead bindings is
awkward against an immutable, shared checked arena. By the time lowering has run, a later
sweep of any IR is also too late: dead bindings are already inlined into the root program
body, and eliminating there would require conservative reasoning about code flowing as
data. Deciding at the boundary where purity is still syntactic keeps the rule exact.

## The demand lattice

A demand describes what live code needs from one bound value. `Absent` means nothing
references the binding, so its bindee never evaluates. `Fields` maps product positions to
the demands placed on those positions; positions absent from the map are never observed
and may hold trivial values. `Used` means the value flows into an unknown context, such as
a function argument or a returned closure, and must be kept whole. Join takes the union of
what either context needs, with `Used` absorbing everything.

Elimination is sound because values are pure: in call-by-push-value, evaluating a value
performs no effects, and effect sequencing goes through `Do`, which the analysis never
drops. Replacing an unobserved product position with `Triv` preserves the layout and is
unobservable, and skipping the bindee of an `Absent` binding removes no evaluation.
Runtime structure that observes a value — forcing a thunk, matching a constructor tag —
counts as a use and is modeled as one.

## One backward pass

The traversal starts at the root with `Used` and works backwards through the spine,
visiting each computation with the demand on its result. At a `Let` it visits the tail
first, reads the accumulated demand of the binder's definitions, and only then visits the
bindee with that demand; a binder whose demand is absent skips its bindee entirely. `Do`
always evaluates both parts and passes the binder's demand as the return demand of its
bindee, so `Ret` and application nodes propagate it onto their values. Projections run
backwards by nesting: the resolved projection chain of `numeric/int64` turns a demand on
the field into `Fields{int64-position: demand}` on the receiver.

This single pass is exact, with no fixed-point iteration, because of two structural facts.
Nested binding scopes are lexical: an inner bindee may demand an outer binder, which the
pass decides later, but nothing can demand an inner binder from outside its scope.
And recursion arrives contained: a recursive definition elaborates as a `Let` whose bindee
is a thunk over `Fix`, so self- and mutual references live inside the bindee and are only
visited after the binding-site decision has been made. A dead recursive definition
therefore dies with its enclosing `Let`, and a self-reference never resurrects it.

A bare `Fix` node — one elaborated outside any `Let` bindee — is visited but never
eliminated. Such a shape would let an outer fixpoint body reference an inner fixpoint's
parameter across sibling bindings, and that reference is only discovered after the inner
binding-site decision; dropping by local decision could remove a definition that later
code reaches. No elaboration producing this shape has been observed, so the guard is
conservative rather than load-bearing.

## Positions in patterns and plans

Reading a binder's demand out of a pattern requires knowing how pattern shapes map to
positions. `VCons` concatenates positionally. `Alias` does not: every member of a pattern
alias matches the *same* scrutinee, so the demanded structure is the join of the members'
demands. The checker's own witness arithmetic records the same distinction — product
patterns sum their components' arities while an alias picks one member's — and the lowered
code confirms it by unpacking one package value once per alias member. `SCons` contributes
nothing from its static components; type fields have no runtime position.

The same alignment lets the Builtin plan consume the analysis. The plan walks the package
type, stripping existential wrappers, manifests, and named wrappers; the root's parameter
pattern strips the same layers, with `SCons` static components carrying the witnesses.
Pattern positions therefore index plan positions, and the root's parameter demand reads
off exactly which operations must be materialized. Undemanded positions still receive
`Triv` to preserve the product arity, since the parameter pattern destructures the whole
package at runtime.

## Uses that are not projections

Three consumption forms do not project, and each one had to be learned the hard way
during bring-up; they are the invariants most worth reviewing.

Forcing is call position. `Force` demands its thunk whole, because a forced projection
such as `int64/add` is *entered*, and entering an operation needs the closure at that
position regardless of what is demanded of the result. The first version of the pass
propagated the result demand through `Force` and pruned every used operation down to
`Triv`. Only a literal `Thunk` node could forward a result demand to its suspended body,
and even then a thunk can be forced from several sites, so bodies conservatively receive
`Used`.

Matching consumes the scrutinee. A constructor pattern observes the tag even when every
payload binder is ignored, so constructor patterns demand the scrutinee whole. And when
the join of all arms' demands is empty — every binder a hole — the match still evaluates
and destructs its scrutinee, so the demand escalates to `Used` rather than staying absent.
The version that ignored this dropped the scrutinee's binding and produced an open root;
the SPS well-formedness check caught it as a free variable before any backend could
misbehave. That check remains the safety net for any future under-approximation in this
pass.

View patterns carry embedded value functions, which are live code whenever the enclosing
binding or match is live; the analysis visits them at binding sites rather than during
demand reads, so reading a pattern's demand stays pure.

## Measured effect and review status

On the add example, which calls `int64_add` and `process/exit`, the emitted assembly fell
from 934 to 152 lines with two externs instead of 131. On echo-sum, 1059 lines and 131
externs became 299 lines and the five externs it actually uses. All end-to-end suites,
interpreter included, pass unchanged, and the interpreter is untouched by design.

What remains deliberately unexploited: function bodies are visited with `Used`, so
demands do not flow interprocedurally and return positions inside closures are not
slimmed, though local dead bindings inside bodies still are. The analysis computes
`Fields` demands for user-level product constructions, but only the Builtin plan
materialization currently acts on them; the lowerer still emits full constructions. The
all-absent match escalation is conservative. And the complement of the live set is
precisely an unused-definition warning, which the editor could surface without touching
the compiler again.
