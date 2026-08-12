# Scoped (Name Resolution)

`scoped` resolves names in one desugared source term.
It replaces `VarName` occurrences with `DefId`s, checks lexical scope, and elaborates context-forming blocks
into ordinary term structure.

## Role in the pipeline

```markdown
textual -> bitter -> scoped -> statics
```

Every source file has already become one complete term before this pass begins.
Name resolution therefore has no separate root environment or executable-body case:
it resolves the root under an empty lexical context and returns that term identity alongside the scoped arena.

## Data model

- `ScopedArena` stores resolved patterns and `Term<DefId>` nodes.
- `ScopedArena::blocks` retains the contextual plan for every `begin ... end` term.
  A block body records both its residual source term and its dependency-ordered elaboration.
- A block binding is a `Parameter` or a `Definition`.
  Its `BindingId` is the `TermId` of the contributing `param`, `let`, or `def` form.
- `ContextNode::Acyclic` represents a singleton strongly connected component without a self edge.
  `ContextNode::Recursive` represents a self-recursive singleton or a mutually recursive group.
  These nodes form the condensation DAG of the block's binding dependencies.
- `Context` records variables available at each term site; `CoContext` records free variables used at that site.

## Resolution process

Ordinary lexical binders are introduced as their patterns are traversed.
A `SourceBoundary` begins with an empty lexical environment, preventing an imported term
from capturing names in its importer.

For a `begin` block, resolution first collects every mobile `that` contribution up to the next block or source boundary.
It installs all contributed pattern binders before resolving occurrences, then records dependency edges
from each binding's right-hand side and pattern annotations.
References inside nested syntax propagate to the active binding of the same block,
so the resulting DAG captures the dependencies required to move each binder safely.

SCC analysis classifies each component and constructs the condensation DAG.
Source order is retained within recursive groups and breaks ties between independent nodes.
Parameters become `Abs`, acyclic definitions become `Let`, and recursive type components become `RecGroup`.
A `Residual` indirection at each original mobile site preserves tree ownership after its binder moves
to the block boundary.

## Context collection

After elaboration, `Collector` traverses the resolved root term directly.
The ordinary term structure already contains the dependency-ordered abstractions and bindings,
so no separate root-context traversal is required.
The traversal records:

- `ctxs_*`: variables available at a site.
- `coctxs_*`: variables used at a site.

## Errors and formatting

`ResolveError` reports unbound variables, duplicate block definitions, unenclosed mobile forms,
illegal recursive parameters, and dependency cycles that cannot be represented by the supported recursive type form.
The `fmt` module provides an ugly formatter for resolved terms used in diagnostics.
