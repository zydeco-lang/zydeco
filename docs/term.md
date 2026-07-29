# Uniform Term Composition

Zydeco represents kinds, types, values, and computations in a common term language,
but definitions and compilation units still inhabit a separate declaration sort.
The split makes dependency resolution a privilege of the top level and leaves libraries
outside the language's ordinary abstraction mechanisms.
This note proposes eliminating declarations in favor of a term former, `begin ... end`.
A block closes an open term: it collects static binders contributed by its subterms,
resolves their dependencies, and elaborates them into a heterogeneous telescope.
The connective `in` preserves lexical placement and scope.
The connective `that` moves a binding to the nearest enclosing block,
and this movement grants block-wide visibility.
Parameters, transparent bindings, and nominal definitions,
including named `data` and `codata` definitions, are scheduled in one dependency graph.
Its strongly connected components admit recursive types but reject recursive values;
computation-level recursion remains explicit in `fix`.
Because mobile definitions range only over types and values,
scheduling cannot reorder effects, which remain confined to CBPV computations and relative monads.
The proposal recovers the capabilities of the declaration-based design
without retaining declarations as a source-language category.
The result is a declaration-free source calculus in which local binding,
compilation units, and library composition differ only in their term structure.

## 1. Motivation

Uniform term composition extends the expression-oriented ideal from computational constructs to binding structure.
Definitions and compilation units should not form a second layer above terms;
they should be introduced and composed within the term language itself.
Zydeco already represents kinds, types, values, and computations as terms,
yet the bindings that introduce them obey special rules at the root of a file.
A definition consequently changes its scoping and dependency discipline
when moved between a local context and a compilation unit.

The declaration sort also obstructs a uniform account of libraries.
Abstraction, application, products, and existential packages can describe a library interface,
but a compilation unit still denotes an environment to be linked rather than a term to be applied.
The source language thus duplicates composition:
terms compose inside programs, while declarations and an external linker compose programs themselves.

The proposed replacement rests on a closure principle:
a block closes its body under the static binders contributed by that body.
It arranges those binders into a well-scoped, dependency-correct telescope,
and an outermost block can therefore serve as a source unit.
A binder moved to a block boundary acquires exactly the visibility of that boundary.
This is why mobility and visibility are not independent properties in the design.

This principle relies on Zydeco's existing phase distinction.
Types and values are inert. Effects occur in computations and are mediated by relative monads.
A block may therefore reorder static bindings without silently reordering computation.

## 2. Blocks and Mobile Bindings

In the grammar below, `e` ranges over surface terms before sorting;
each occurrence is subsequently classified as a kind, type, value, or computation.
The surface language introduces a delimited block together with three binding forms:

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

The concrete syntax of `begin ... end` is inspired by Zydeco's `monadic ... end` blocks.
The resemblance is syntactic: a monadic block organizes computation,
whereas `begin` delimits static binder mobility and closure.

In the absence of a mobile binding, `begin e end` elaborates to `e`;
like parentheses, the block does not itself introduce evaluation.
Its additional role is to delimit how far a mobile binding may move.

The difference between `in` and `that` is semantic rather than stylistic.
An `in` binding remains at the position where it is written, and its pattern is visible only in the following body.
Thus `param p in e` is a local abstraction, `let p = v in e` is a transparent local binding,
and `def p = v in e` introduces a local nominal definition.

A `that` binding is instead contributed to the nearest enclosing block.
The block moves the binding to a position consistent with its dependencies.
This movement determines scope:
because the binding is established at the block boundary, its pattern is visible throughout the block,
including in text that precedes the binding or lies outside its syntactic continuation.
Block-wide binders are consequently collected before occurrences in the block are resolved.
The term written after `that` supplies the residual syntax to be elaborated; it is not the scope of the moved binder.
A nested `begin` establishes a new boundary and prevents its bindings from escaping to an outer block.
A mobile binding must not capture a local binder that it crosses.
If its right-hand side or annotation depends on an enclosing `in` binder,
the program must make that dependency mobile, keep the dependent binding local,
or introduce a nested block within the local scope.

For example,

```zydeco
begin
  let answer = seed that
  param seed that
  answer
end
```

is well scoped. The reference to `seed` induces an ordering constraint, and the block elaborates to

```zydeco
fn seed -> let answer = seed in answer
```

The same rule applies when mobile parameters occur inside larger expressions:

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

Here the written positions do not by themselves determine the final nesting
and do not delimit the parameters' visibility.
They identify four block parameters and determine the order among otherwise independent candidates.
The unused parameter `b` is retained because `param` specifies an interface, not an inferred set of free variables.

Block-wide visibility can make a legal program difficult to read from its indentation alone.
The compiler should warn when a `that` binder is used outside its syntactic continuation,
across match arms, or across another abstraction.
It can recommend moving the binding to the block spine, replacing `that` with `in`, or introducing a nested block.
These diagnostics repair presentation, not semantics: `that` remains uniformly block-wide.

The keywords `def` and `let` expose the distinction between nominal and transparent binding.
`def` introduces a stable abstract identity, whereas `let` preserves definitional equality.
The familiar `let` form applies naturally to both types and values
without suggesting a separate module or resource discipline.
This distinction remains orthogonal to `in`/`that`, which controls placement and scope.

The named `data` and `codata` forms formerly classified as declarations
are retained as block-context contributions.
Each contributes a nominal type definition to the same candidate set as `def`;
its parameters and constructor or destructor signatures determine its dependencies.
Conceptually, a named `data` form is a specialized `def` of an anonymous `data ... end` term,
and `codata` is its computation-type dual.
Their recursive components are therefore checked by the block's SCC analysis
rather than by a separate declaration pass.
Constructors and destructors remain arms of their respective types, not independent binding candidates,
while anonymous `data ... end` and `codata ... end` remain ordinary type terms.

## 3. Dependency-Directed Elaboration

Elaborating a block produces a residual term together with a collection of binding candidates.
A candidate records its source identity, binder pattern, optional right-hand side, source position,
and binding mode. `param` contributes an abstraction, `let` contributes a transparent binding,
and `def` contributes a nominal binding whose type-level representation is sealed.
Named `data` and `codata` forms contribute the same nominal binding mode as `def`.
All of these forms are ordered together.
There is no preliminary phase that places every parameter before every definition.

Dependencies arise from occurrences in right-hand sides, pattern annotations, and parameter types.
If a parameter type mentions a local type definition, the definition precedes the parameter.
If a definition uses a parameter, the opposite order is required.
The resulting sequence is therefore a heterogeneous telescope rather than a pair of parameter and definition lists.
Independent candidates retain source order, which makes elaboration and diagnostics deterministic
without assigning semantic importance to an arbitrary graph traversal.

Name resolution and scheduling are separate phases.
The elaborator first allocates identities for every block-wide binder and resolves occurrences to those identities.
It then constructs the dependency graph and computes its strongly connected components.
Scheduling never re-resolves names after a binder moves,
and nominal type identity is derived from the source binder rather than from its eventual position.
These conditions prevent both capture and order-dependent type identity.

The condensation graph of strongly connected components is acyclic and can be topologically ordered.
Acyclic components may contain parameters and well-sorted type or value definitions.
Cyclic components, however, are restricted to recursive types.
Their members must provide enough kind information to register all binders before checking their bodies,
and their recursive occurrences must satisfy the appropriate guardedness or positivity condition.
Transparent type-binding cycles are rejected because they provide no nominal boundary for finite unfolding.
A component containing a parameter or a value definition is likewise rejected.

This restriction does not remove general recursion from programs.
Recursive computation is already represented by the computation-level fixed point `fix`;
it need not be inferred from a cycle among block bindings.
A value may contain a suspended computation whose body uses `fix`,
while the value's own block dependency remains acyclic.
Recursive types and recursive computations therefore retain distinct introduction forms.

Only types and values may appear on the right of `def` and `let`.
Consequently, topological scheduling has no effect on evaluation order.
Effectful sequencing remains explicit in computation terms.
Patterns in mobile bindings should also be irrefutable;
a refutable decomposition is an elimination and belongs in `match`, where failure and branching are explicit.

## 4. Nominality and Existential Libraries

A type introduced by `def` is lexically generative.
Its abstract identity is attached to the source binder and is allocated once,
even when the enclosing term is evaluated repeatedly.
For a recursive type component, all identities are allocated before any defining equation is checked.
The distinction between `def` and `let` is thus semantic:
`let` preserves definitional equality, whereas `def` establishes a nominal abstraction boundary.

Dynamic generativity is not implied by movement to a block boundary.
If Zydeco later admits a construct such as `fresh`,
it should denote a distinct operation that creates a new abstract identity at each dynamic introduction.
Such a construct requires an explicit account of generative packages
and should not alter the lexical meaning of `def`.

`extern` is not retained as a fourth binding mode.
A former `extern` declaration introduced a typed name without a source definition
and left the linker to provide its implementation.
Under uniform term composition, such assumptions are collected into existential library interfaces.
A library package hides the types chosen by its provider
and carries the values implementing operations over those types.
Eliminating the package with a pattern brings both into the program's block context.
The compiler or launcher may construct and supply this package,
but the source calculus contains no ambient bodyless definitions.

Once compilation units are terms, libraries can be represented by the same abstraction mechanisms.
A library implementation is a function from its dependency packages
to an existential package containing an abstract central type and the operations defined over it:

```text
Library D = D -> exists (T : K). API T
```

The host interface can be represented similarly.
Let `Core` package an abstract computation type `OS`
together with the standard operations that produce and consume it.
A binary consumes a core package and returns a computation
of the abstract `OS` type bound by that package's elimination pattern.
Its conceptual type is

```text
pi ((OS, api) : Core). OS
```

The occurrence of `OS` in the result refers to the opaque witness bound by the parameter pattern,
not to a projected or globally distinguished type.

This interface requires package-dependent elimination rather than ordinary System F existential elimination:
the result type may mention the abstract witness introduced by the parameter pattern.
In the statics language, this dependency is represented by the dedicated `PackPi` type.
The same package value must yield the same `OS` identity wherever it is referenced,
and the launcher must execute the binary while that witness remains in scope.
Package-dependent elimination is therefore part of the module encoding, not an incidental convenience.

## 5. Consequences and Open Work

Removing the declaration sort changes the source calculus, not the need for structured intermediate representations.
A compiler may still construct block plans, binding graphs, recursive type components, and package interfaces.
What disappears is the requirement that programmers select a different syntactic category
in order to obtain those structures.
A program denotes a term, and its block boundaries account for every binding needed to close it.

A formal account must now establish two invariants.
First, block elaboration must preserve resolved identities and sorting
while remaining independent of incidental choices among valid topological orders.
Second, a readability diagnostic must never affect scope:
if `that` moves a binder, the binder is block-wide whether or not the compiler considers the use idiomatic.
The admissibility condition for recursive type components and the elimination rule for existential packages
with dependent results also require explicit typing rules.

The central claim is structural.
Local definitions, compilation units, and libraries need not be composed by separate language mechanisms.
Named data and codata forms elaborate into block bindings,
while external requirements elaborate into abstractions over existential libraries.
`in` means that a binder remains where it is written; `that` means that it belongs to the surrounding block.
Once movement determines visibility and purity makes static scheduling unobservable,
dependency analysis can recover a legal binding order,
realizing the expression-oriented ideal through uniform term composition.
