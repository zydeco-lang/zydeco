# Uniform Term Composition

Zydeco uses a common term language for kinds, types, values, and computations.
Its former source language reserved a separate declaration sort for top-level definitions,
which gave local terms and compilation units different rules for scope, dependency ordering, and abstraction.
Uniform term composition removes this split by making context formation part of the term language.
The guiding idea is that a term may contribute the binders it needs before their final nesting is known.
Its central construct is `begin ... end`, a block that closes its body over binders contributed by nested terms.
The forms `param`, `let`, and `def` contribute parameters, transparent bindings, and nominal definitions.
With `in`, a binder keeps its lexical position and scope.
With `that`, it belongs to the nearest block and becomes visible throughout that block.
Together, these connectives let the programmer choose between local binding and block-level context formation.
Once binders may arise in different parts of a term, however, textual order is no longer enough.
The block resolves dependencies among these contributions and elaborates them into a heterogeneous telescope:
an ordered sequence in which later binders may depend on earlier ones.
Most contributions form an acyclic order.
Recursive types are handled as strongly connected components, while value bindings remain acyclic
and computation recursion remains explicit through `fix`.
The scheduling stays within the static layer because movable right-hand sides are types or values,
so the order of CBPV computations is preserved.
An outer block can therefore represent a source unit, and libraries can use the same functions
and existential packages that compose ordinary terms.

## 1. Motivation

Zydeco already presents kinds, types, values, and computations through a shared term syntax.
At the boundary of a source file, however, the programming model changes.
A local binding participates in ordinary term structure, whereas a top-level definition enters a declaration context
with its own rules for name resolution, dependency analysis, and linking.
Moving a definition between these settings can therefore change more than indentation:
it changes how the definition composes with its surroundings.
The discrepancy is easy to overlook in a small file, but it becomes visible as soon as code is factored
into local helpers, reusable components, and separate compilation units.

At library scale, the same split affects the meaning of composition itself.
Within a program, functions, products, and existential packages express abstraction and composition.
Across source units, declarations instead produce environments for an external linker.
Programmers therefore use one vocabulary to compose terms and another to compose programs.
Uniform term composition extends the expression-oriented character of Zydeco to definitions and source units,
so a library becomes a term with an explicit interface.

Reaching that goal requires a term to describe more than its immediate result.
It must also be able to describe the context in which that result becomes well formed.
The proposal is organized around block closure.
A subterm may contribute a parameter, type definition, or value definition to its nearest enclosing block,
and the block arranges all such contributions into a dependency-correct telescope around its body.
At the outermost level, the same operation closes a source file.
From the programmer's perspective, a definition can remain near the expression that motivates it.
The enclosing block later assembles the context needed by the whole term.
Placement also determines visibility: a binder assigned to a block boundary receives the scope of that block.
This connection makes mobility predictable from the syntax.

The remaining concern is whether this assembly can change program behavior.
Zydeco's phase distinction makes dependency-directed placement semantically stable.
Types and values are effect-free at introduction,
while computations carry and sequence effects through relative monads.
Reordering the contributed binders can therefore change their nesting without changing computation order.
This separation allows `that` to express binder mobility while computation syntax continues to express evaluation order.

## 2. Blocks and Mobile Bindings

In the grammar below, `e` ranges over a source term before sorting determines whether it is
a kind, type, value, or computation.
The common metavariable records the shared surface syntax while leaving the CBPV categories intact.
The new constructs can therefore be stated once at the surface level and sorted according to their use.

The surface language adds a block and three binding forms:

```text
e ::= ...
    | begin e end
    | param p in e
    | param p that e
    | let p = e in e
    | let p = e that e
    | def p = e in e
    | def p = e that e
```

The concrete syntax of `begin ... end` takes its cue from Zydeco's `monadic ... end` blocks.
Both forms mark a region with explicit delimiters, giving nested syntax a visible boundary.
A monadic block organizes computation, whereas `begin` organizes the static context surrounding a term.
When its body contributes no mobile bindings, `begin e end` elaborates to `e` much like parentheses.
Its additional purpose is to provide a visible destination for bindings contributed from within the term.

The block supplies a boundary, but each binding still needs to say how it relates to that boundary.
Each binding combines two choices.
The form says what enters the context: `param` adds a parameter,
`let` adds a transparent binding that the type checker may unfold during equality checking,
and `def` gives the source binder a stable identity.
The connective says where that binding is established.
Keeping these choices separate allows the same kind of binding to be either lexical or block-wide.

With `in`, the binding stays where it is written and its following term is its scope.
Thus `param p in e` forms a local abstraction, while the `in` variants of `let` and `def`
form transparent and nominal local bindings.
These forms support the familiar left-to-right reading of lexical scope.
They provide the baseline against which the mobility of `that` can be understood.

With `that`, the binding belongs to the nearest enclosing `begin`.
The block places it according to its dependencies, and the resulting block-level position makes
the names introduced by its pattern visible throughout the block,
including text before the binding and text outside its syntactic continuation.
The term following `that` supplies the residual expression at the original site.
A nested `begin` starts a new closure region, so mobile bindings settle at the closest visible boundary.
The elaborator allocates every block-wide binder before resolving occurrences in the block.

Whole-block visibility makes forward references possible, but it also determines how far a binding may travel.
Movement is valid when the binding's dependencies remain available at the block boundary.
For example, a mobile definition whose right-hand side refers to an enclosing local `in` binder
must settle at a boundary within that binder's scope.
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
fn seed -> let answer = seed in answer
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
fn a b c d -> let x = a + c in d
```

Here the four `param` forms declare the block's interface.
Reading the source from left to right orders the otherwise independent parameters as `a`, `b`, `c`, and `d`.
Dependency edges may still interleave definitions when their types or bodies require it.
The unused parameter `b` remains part of the result because `param` states an interface
instead of asking the compiler to infer one from free variables.

This freedom of placement is useful, although block-wide scope can occasionally outrun
the visual cues of indentation.
The compiler should warn when a reference relies on a binder moving beyond its syntactic continuation,
especially across match arms or another abstraction.
A diagnostic can suggest moving the binding to the block spine, choosing `in`, or introducing a nested block.
The warning addresses readability while preserving the block-wide meaning of `that`.

The examples above focus on placement.
The other choice carried by a binding form concerns identity.
The pairing of `def` and `let` is deliberate.
`let` names a type or value transparently, so clients may use its defining equation.
`def` establishes an abstraction boundary by giving the source binder its own identity.
This distinction concerns identity and equality.
The connectives `in` and `that` separately determine placement and scope.

These surface rules explain what a block means to the programmer.
The next question is how the elaborator turns freely placed contributions into one well-scoped term.

## 3. Dependency-Directed Elaboration

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
This separation between resolution and placement prevents capture and makes nominal identity
independent of the particular topological order selected by the graph algorithm.

For an acyclic block, this scheduling step completes the plan.
Recursive types require one further distinction, because their dependency cycles are intentional.
Cycles in the graph are analyzed as strongly connected components (SCCs),
groups in which every member depends, directly or indirectly, on every other member.
A recursive component is admissible when all of its members define types,
their kinds are available before their bodies are checked,
and their recursive occurrences satisfy the guardedness or positivity discipline
chosen for well-formed recursive types.
The checker allocates the nominal identities for such a component together and then checks its equations.
Parameters, values, and transparent type aliases follow acyclic dependency order.
A cycle involving one of them receives a focused diagnostic.

This account of recursive components also gives the existing named type forms a natural place in the design.
The named `data` and `codata` forms formerly handled as declarations
enter a block as specialized nominal definitions.
A named `data` form elaborates to a nominal definition whose right-hand side
abstracts its parameters over an anonymous `data ... end` term.
The `codata` form supplies the computation-type dual.
Their parameters and constructor or destructor signatures contribute dependency edges,
so mutually recursive named types pass through the same SCC analysis as other type definitions.
Constructors and destructors remain arms of their respective type terms,
and the enclosing type serves as the scheduling candidate.
Anonymous `data ... end` and `codata ... end` remain ordinary terms.

Type recursion is now accounted for by the block graph.
Computation recursion continues to use the explicit introduction form already present in Zydeco.
A value may suspend a computation that uses `fix`, while the value's static dependency component stays acyclic.
This keeps recursive types, recursive computations, and dependency cycles as three distinct ideas in the language.

The separation between static and computational recursion also explains two restrictions on mobile bindings.
The scheduler accepts only types and values on the right of `def` and `let`.
Its choices change static nesting, while effect order continues to follow computation syntax.
Mobile patterns should be irrefutable for the same reason that ordinary binding patterns are:
forming a context should produce bindings directly.
Refutable decomposition belongs in `match`, where the source program states its branches explicitly.

Block elaboration has now supplied both a scope and an order for every contributed binder.
The stable identities produced by this process become especially important when a block represents a library.

## 4. Nominality and Existential Libraries

Within a block, nominal identity distinguishes `def` from `let`.
At a library boundary, the same distinction determines whether clients can observe a representation.
A type introduced by `def` is lexically generative:
the source binder receives a stable abstract identity for the lifetime of the program.
Repeated evaluation of the enclosing term reuses that identity.
For a recursive type component, the checker allocates all member identities together
before checking any defining equation.
By contrast, `let` preserves the defining equation and therefore supports transparent equality.

Lexical generativity is the appropriate default for source definitions.
A future construct such as `fresh` could express dynamic generativity by creating a new identity
at each runtime introduction.
Giving that behavior its own term former would make the stronger lifetime discipline visible
and provide a clear point at which generative package rules apply.

The stable identity supplied by `def` is precisely what allows a package to hide an implementation type
while sharing that type among all of its operations.
Once a source unit denotes a term, external dependencies also acquire a language-level interface.
The current `extern` form introduces a typed name whose implementation is supplied later by the linker.
Under uniform term composition, an existential library collects those assumptions into one package.
The provider chooses the package's abstract types and supplies values implementing the operations over them.
The consumer unpacks the package with a pattern that brings both types and operations into its block context.
The compiler or launcher can construct the package for built-in facilities,
so every external requirement appears in the type of the consuming term.

For an ordinary dependency, the package discipline has a familiar function shape.
A reusable library may accept one package and return another:

```text
Library D = D -> exists (T : K). API T
```

Here the existential witness `T` represents the implementation's central abstract type,
and `API T` contains the operations that expose its permitted behavior.

This type explains how a library hides its representation.
The consumer side becomes more interesting when its own result type must refer to the hidden witness.
The host interface follows the same pattern.
Suppose `Core` packages an abstract computation type `OS` together with the standard operations that use it:

```text
Core = exists (OS : CType). API OS
binary : pi ((OS, api) : Core). OS
```

The package pattern binds the opaque `OS` witness directly, and the binary's result type refers to that witness.
This dependency is represented in the statics language by `PackPi`,
a computation-valued package-dependent arrow whose codomain may mention abstract types bound by its parameter
pattern. A value-valued library uses the corresponding `ValuePackPi`; its application instantiates the same witness
telescope without introducing a computation.
The package value carries a stable witness identity wherever it is used.
At launch, the compiler instantiates `Core`, passes the package to the binary,
and executes the resulting computation while the witness remains in scope.
The user sees an abstract interface.
The launcher supplies its concrete implementation.

The library account therefore follows directly from block elaboration:
nominal binders provide stable abstract identities, existential packages collect them into interfaces,
and the two package-dependent arrows keep those identities in scope across value and computation calls.

## 5. Consequences and Open Work

At the source level, the result is one composition discipline.
Inside the compiler, declaration planning becomes an elaboration task with its own explicit structures.
The elaborator can represent block plans, dependency graphs, recursive type components,
and package interfaces as explicit intermediate structures.
These structures now arise from ordinary terms and their block boundaries,
while programmers use one composition language at every scale, from local definitions to source units.
This division is deliberate: source programs gain a uniform language,
while intermediate representations expose the graph and telescope that make elaboration precise.

With that implementation boundary in place, the remaining work is chiefly formal.
A formal account should make the elaboration invariants precise.
Resolved identities and sorts must survive every permitted movement,
and valid schedules that differ only in the order of independent candidates should be equivalent.
Readability diagnostics must preserve the scope assigned by `that`,
so a recommendation about layout leaves the meaning of a block-wide reference unchanged.
Recursive type components need an explicit admissibility judgment,
and `PackPi` needs an elimination rule that tracks the existential witness into the result type.

These obligations give the organizing idea a precise metatheory.
Uniform term composition extends Zydeco's expression-oriented design to binding structure.
`in` gives a binder a lexical home.
`that` assigns it to the surrounding block.
The programmer may place a definition near the syntax that motivates it,
while dependency analysis recovers the telescope required by the type system.
At larger scales, the same mechanism turns a source unit into a closed term
and expresses libraries through ordinary abstraction, application, and existential packaging.
