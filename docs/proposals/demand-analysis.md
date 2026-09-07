# Demand Analysis

A program can reach a package without observing every field of that package.
Ordinary reference liveness retains the entire construction, including all the closures it contains.
Demand analysis asks how the surviving consumer uses each value, so unused operations
and their dependencies can disappear before closure conversion allocates environments or emits code.

The analysis is part of [residual SPS normalization](normalization.md#residual-sps-normalization).
`lang/stackir/src/sps/demand.rs` defines consumer demands and their translation through patterns;
`sps::normalize` uses those demands while rebuilding high SPS.
The checked residual program lowers structurally, including the complete Builtin package plan.
Host package fields and user product fields then follow the same elimination rules.
The interpreter retains its reference semantics, and the editor retains the full checked program.

This placement relies on high SPS preserving lexical bindings, explicit sharing, and suspended computations.
The former checked-AST analysis and demand-dependent lowering have been removed.
Lowering temporarily constructs unused high-SPS nodes, but normalization removes them before closure conversion.
Avoiding that temporary construction would be a compile-time optimization to justify with measurements.

## The demand lattice

A demand describes what a consumer observes:

| Demand | Observation |
| --- | --- |
| `Absent` | No use of the value survives. |
| `Fields` | Only the listed physical product positions and their nested demands are observed. |
| `Used` | An unknown consumer requires the whole value. |

Join combines observations from all consumers. `Used` absorbs every other demand;
joining field demands takes their union and recursively joins demands on common positions.
An empty `Fields` map differs from `Absent`: a surviving unpack still requires the product's shape,
even when it reads none of the fields.
Such a product can contain trivial values in every position.

Demand describes observation, not permission to suppress evaluation.
The normalizer applies its [discardability and stack-movement rules](normalization.md#residual-sps-normalization)
before removing a binding or replacing an unobserved field with `Triv`.
A trapping field remains evaluated even if no consumer observes its result.

## Producer facts and consumer demands

At `let p = V in M`, the normalizer first records facts about `V` in the lexical environment used for `M`.
Known callees, product fields, and constructor tags can expose local reductions in that consumer.
The normalized consumer returns a map from its free definitions to their demands.
The binder pattern translates those entries into a demand on `V`, which is then rebuilt under that demand.
Demands on the bound definitions leave the map; demands from the surviving producer join the remaining entries.
This makes alias forwarding, field pruning, and dead-binding elimination parts of one traversal.

A return to a visible continuation becomes an ordinary binding before demand is read.
Likewise, reducing a known application exposes its parameter and argument as a binding.
The consumer's field demands can therefore prune a package passed to a locally reduced computation.
A call whose implementation remains unknown conservatively observes its argument values whole.
Escaping thunks remain suspended, and their normalized bodies contribute demands on their captures.

Lexical structure makes these demands local.
A binder's entire surviving scope is visited before deciding whether to keep its producer,
and independent branches join their demands before that decision.
An executed `Fix` remains, with its self reference treated as an unknown callable;
the normalized body contributes all demands on its outer captures.
A dead thunk containing recursion can disappear without inspecting its body.
There is no recursive-call specialization and no global demand fixed point.

## Physical positions and pattern aliases

High SPS records the physical arity of every product construction and pattern.
Ordinary items demand individual positions. If the last logical item represents the remaining suffix,
its nested field positions are shifted into the containing product's physical positions.
Rebuilding a suffix spread preserves a product of the required arity even when all its fields are absent,
because constructing the enclosing product still reads that suffix.
Static witnesses and named field routes have already erased or become structural patterns during lowering.

Every member of an `Alias` pattern observes the same scrutinee.
Their demands join at the same positions; they do not concatenate.
A whole-value use through one alias therefore keeps fields that another alias only projects selectively.
Product construction and elimination can cancel when their logical components and physical layout align.
Once that unpack disappears, its shape no longer contributes demand.

## Observations beyond projections

An indirect force observes its thunk whole.
The result demand is not a demand on the closure package.
A direct primitive call can remove that closure use through normalization, while retaining the external call
and every value needed by its supplied stack.

A constructor pattern observes the tag even if it ignores every payload binder.
Surviving constructor matches conservatively demand their scrutinees whole.
Known-constructor selection can remove the other arms, so dependencies used only in those arms become absent.
If selection cannot be established, all potentially live arms remain consumers.

The normalizer's tests pair nested and suffix field pruning with whole-product escape,
and single-use closure reduction with shared alias patterns.
They also retain trapping payloads and stack arguments, unknown branches with shared continuations,
and unknown or recursive calls.
The demand and core integration tests cover unused definitions, unused Builtin operations, package applications,
and execution on the compiled backends.
