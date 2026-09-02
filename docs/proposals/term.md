# Uniform Term Composition

Zydeco uses one term language for kinds, types, values, and computations.
`param`, `let`, and `def` are ordinary term constructors for abstractions, transparent bindings,
and nominal definitions. Their `in` forms establish binders at the written position, while their `that` forms
contribute binders to the nearest `begin ... end` block. A block dependency-orders those contributions
and elaborates them into an ordinary heterogeneous telescope.

A source file stores one complete term. The file contributes no context, parameters, declarations,
namespace, or runtime structure around that term. Every dependency must occur in the term itself,
through an ordinary binder or an import that is replaced by another independently checked term.

## Blocks and Mobile Bindings

In the grammar below, `e` ranges over a source term before sorting determines whether it is a kind,
type, value, or computation.
The common metavariable records the shared surface syntax while leaving the CBPV categories intact.
The constructs can therefore be stated once at the surface level and sorted according to their use.

The surface grammar includes a block and three binding forms:

```text
e ::= ...
    | begin e end
    | param p in e
    | param p that e
    | param val p in e
    | param val p that e
    | let p = e in e
    | let p = e that e
    | def p = e in e
    | def p = e that e
```

The concrete syntax of `begin ... end` marks a region with explicit delimiters, giving nested syntax a visible boundary.
Metadata can attach to the whole region, as in `@[monadic] begin ... end`,
while an unannotated `begin` organizes only the static context surrounding a term.
When its body contributes no mobile bindings, `begin e end` elaborates to `e` much like parentheses.
Its additional purpose is to provide a visible destination for bindings contributed from within the term.

The block supplies a boundary, but each binding still needs to say how it relates to that boundary.
Each binding combines two choices.
The form says what enters the context: `param` adds a parameter, `let` adds a transparent binding
that the type checker may unfold during equality checking, and `def` gives the source binder a stable identity.
The `val` modifier on `param` records that block reconstruction must introduce a total value function;
without it, the parameter retains the existing type-function or computation-abstraction discipline.
The connective says where that binding is established.
Keeping these choices separate allows the same kind of binding to be either lexical or block-wide.

With `in`, the binding stays where it is written and its following term is its scope.
Thus `param p in e` forms a local type or computation abstraction, `param val p in e` forms a local value abstraction,
and the `in` variants of `let`
and `def` form transparent and nominal local bindings.
These forms support the familiar left-to-right reading of lexical scope.
They provide the baseline against which the mobility of `that` can be understood.

With `that`, the binding belongs to the nearest enclosing `begin`.
The block places it according to its dependencies, and the resulting block-level position makes the names introduced
by its pattern visible throughout the block, including text before the binding
and text outside its syntactic continuation.
The term following `that` supplies the residual expression at the original site.
A nested `begin` starts a new closure region, so mobile bindings settle at the closest visible boundary.
The elaborator allocates every block-wide binder before resolving occurrences in the block.

Whole-block visibility makes forward references possible, but it also determines how far a binding may travel.
Movement is valid when the binding's dependencies remain available at the block boundary.
For example, a mobile definition whose right-hand side refers to an enclosing local `in` binder must settle
at a boundary within that binder's scope.
The programmer can make the dependency mobile, keep the dependent definition local,
or place a nested block inside the local scope.

With the boundary and scope rules in place, a small example shows how dependency ordering completes the picture.
The following block is well scoped:

```zydeco
begin
  let answer = seed that
  param seed that
  answer
end
```

The reference to `seed` induces an ordering constraint, and the block elaborates to

```zydeco
fn seed => let answer = seed in answer
```

The example writes `answer` before its dependency `seed`.
The corresponding graph edge leads the block to place the parameter before the transparent binding.

The first example contains a single dependency.
The compositional benefit becomes clearer when several mobile parameters occur inside a larger expression:

```zydeco
begin
  let x =
    (param a that param b that a)
      + (param c that c)
  in param d that d
end
```

The block closes as

```zydeco
fn a b c d => let x = a + c in d
```

Here the four `param` forms become parameters of the resulting abstraction.
Reading the source from left to right orders the otherwise independent parameters as `a`, `b`, `c`, and `d`.
Dependency edges may still interleave definitions when their types or bodies require it.
The unused parameter `b` remains part of the result because `param` constructs an explicit binder
instead of asking the compiler to infer one from free variables.

The examples above focus on placement. The other choice carried by a binding form concerns identity.
The pairing of `def` and `let` is deliberate.
`let` names a type or value transparently, so the surrounding term may use its defining equation.
`def` establishes an abstraction boundary by giving the source binder its own identity.
This distinction concerns identity and equality.
The connectives `in` and `that` separately determine placement and scope.

These surface rules explain what a block means to the programmer.
The next question is how the elaborator turns freely placed contributions into one well-scoped term.

## Dependency-Directed Elaboration

Whole-block visibility separates the availability of a name from the eventual position of its binder.
Elaboration therefore begins by assigning source identities to the binders in every block-wide pattern
and resolving occurrences against the complete block context.
Each mobile form then contributes a candidate that records its pattern and binder identities,
an optional right-hand side, its binding mode, and its source position.
At the end of this collection step, every block-wide name is available,
but the binders have intentionally been left unordered.

Dependency analysis supplies that missing order.
The block builds a dependency graph over these candidates.
An edge records that one candidate must be established before another can be formed.
Occurrences in right-hand sides, pattern annotations, and parameter types all create such edges.
If a parameter type mentions a local type definition, the definition precedes the parameter.
When a definition uses a parameter, the parameter precedes the definition.
The resulting order is a heterogeneous telescope, since parameters and definitions may alternate
and each entry may refer to earlier entries.
Source order breaks ties between independent candidates, which keeps elaboration and diagnostics deterministic.

The graph determines where binders are placed.
Their meaning, however, was already fixed when names were resolved.
Scheduling preserves the identities chosen during name resolution.
An occurrence continues to refer to the same source binder after that binder moves,
and a nominal type receives its identity from its source `def`.
This separation between resolution and placement prevents capture and makes nominal identity independent
of the particular topological order selected by the graph algorithm.

For an acyclic block, this scheduling step completes the plan.
Recursive types require one further distinction, because their dependency cycles are intentional.
Cycles in the graph are analyzed as strongly connected components (SCCs), groups in
which every member depends, directly or indirectly, on every other member.
A recursive component is admissible when all of its members define types, their kinds are available
before their bodies are checked, and their recursive occurrences satisfy the guardedness
or positivity discipline chosen for well-formed recursive types.
The checker allocates the nominal identities for such a component together and then checks its equations.
Parameters, values, and transparent type aliases follow acyclic dependency order.
A cycle involving one of them receives a focused diagnostic.

This account of recursive components also gives the named type forms a natural place in the design.
Named `data` and `codata` forms enter a block as specialized nominal definitions.
A named `data` form elaborates to a nominal definition whose right-hand side abstracts its parameters
over an anonymous `data ... end` term.
The `codata` form supplies the computation-type dual.
Their parameters and constructor or destructor signatures contribute dependency edges,
so mutually recursive named types pass through the same SCC analysis as other type definitions.
Constructors and destructors remain arms of their respective type terms,
and the enclosing type serves as the scheduling candidate.
Anonymous `data ... end` and `codata ... end` remain ordinary terms.

Type recursion is accounted for by the block graph.
Computation recursion continues to use the explicit introduction form already present in Zydeco.
A value may suspend a computation that uses `fix`, while the value's static dependency component stays acyclic.
This keeps recursive types, recursive computations, and dependency cycles as three distinct ideas in the language.

The separation between static and computational recursion also explains two restrictions on mobile bindings.
The scheduler accepts only types and values on the right of `def` and `let`.
Its choices change static nesting, while effect order continues to follow computation syntax.
Mobile patterns should be irrefutable for the same reason that ordinary binding patterns are:
forming a context should produce bindings directly.
Refutable decomposition belongs in `match`, where the source program states its branches explicitly.

Block elaboration supplies both a scope and an order for every contributed binder.

## Nominal Identity

Within a term, nominal identity distinguishes `def` from `let`.
A type introduced by `def` is lexically generative: the source binder receives a stable abstract identity
for the lifetime of that term occurrence. Repeated evaluation of the occurrence reuses the identity.
For a recursive type component, the checker allocates all member identities together
before checking their defining equations. By contrast, `let` preserves its defining equation
and therefore supports transparent equality.

Copying a term freshens its bound identities. Two copies of a term containing `def` consequently contain
distinct nominal definitions, while two uses of one bound copy share the same definitions.

## Source Terms and Imports

A source root is resolved and type checked under an empty context. After its own imports and optional companion
annotation have been assembled, the complete root must synthesize its classifier. An expected classifier at a use
site may be compared with that result, but it does not participate in elaborating the source root.

An implementation source `foo.zy` may have a companion `foo.zyi`. The companion contains one type term,
which must itself synthesize a type. The pair is elaborated as the ordinary annotated term
`(contents-of-foo.zy : contents-of-foo.zyi)`. The companion supplies no declarations or context.

An import is metadata on a hole:

```zydeco
@[import("library.zy")] _
```

The spelling `@(import("library.zy"))` abbreviates the same term. Source assembly replaces the hole with a fresh,
capture-avoiding copy of the independently checked source term. A source boundary prevents free names and mobile
bindings from crossing between the two terms. Each occurrence is fresh; sharing requires binding one imported copy
and using that binding more than once.

This operation is ordinary term substitution. Its stability claim concerns well-typed, scope-respecting
substitutions: substitution preserves typing and freshens bound identities where necessary. It does not make
duplicated computations execute once or make nominal definitions from distinct copies identical.
