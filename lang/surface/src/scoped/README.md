# Scoped (Name Resolution)

`scoped` performs name resolution on the desugared surface syntax (`bitter`).
It replaces `VarName` occurrences with `DefId`s, checks scoping rules, and turns
the source declaration sequence into one contextual term.

## Role in the pipeline

```markdown
textual -> bitter -> scoped -> statics
```

This pass is where variable names become bound identifiers (ABT-style), so later
phases operate directly on definition IDs. Textual and bitter declarations remain
useful for parsing and desugaring, but the declaration sort does not cross the
scoped boundary.

## Data model

- `ScopedArena` stores resolved patterns and `Term<DefId>` nodes.
- Its root is a `ContextualTerm<BindingContext>`, consisting of a context and an
  optional executable body. The absent-body case permits checking a library.
- A context binding is a `Parameter`, a `Definition` with a right-hand side, or
  an `External` with an optional classifier. Root contexts currently use the
  latter two forms; nested `begin` contexts use parameters and definitions.
  Metadata is attached directly to a binding or body rather than represented by
  wrapper nodes.
- `ScopedArena::blocks` retains the contextual term for every nested
  `begin ... end`. Its body records both the residual source term and a
  dependency-ordered elaboration into ordinary abstractions and bindings.
- `ContextNode::Acyclic` represents a singleton SCC without a self edge.
  `ContextNode::Recursive` represents a self-recursive singleton or a mutually
  recursive group. The nodes form the condensation DAG of the binding dependency
  graph.
- `Context` tracks variables available at each term site; `CoContext` tracks free
  variables used at that site.
- `PrimDefs` records definitions of primitives such as `VType`, `CType`, `Thk`,
  and `Ret`, and validates that they were provided.

## Resolution process

`Resolver` first collects all global binders, then resolves each source item with
a `(Local, Global)` lookup:

- Local bindings come from patterns and shadow global names.
- Global references contribute dependency edges between context bindings.
- Internal terms inserted by desugaring are redirected to their primitive
  definitions and contribute the same dependency edges.
- An executable body may refer to the complete global context, but it is not a
  binding and therefore does not become a graph node.

For a nested `begin`, resolution first collects every `that` contribution up to
the next nested block and installs all of its pattern binders. It then resolves
each candidate under the complete block scope. Active context bindings form a
stack: a reference can add an edge to the block-local DAG and, when relevant,
to an enclosing block or root definition. This propagation preserves outer
dependency ordering without treating already available outer bindings as nodes
of the inner DAG.

After resolution, SCC analysis classifies each binding component and constructs
its condensation DAG. Source order is retained within recursive groups and breaks
ties between independent nodes.

The block elaboration follows the resulting topological order. Parameters become
`Abs`, definitions become `Let`, and a recursive type component becomes
`RecGroup`. A `Residual` indirection remains at each original mobile site, which
preserves tree ownership of term IDs after its binder moves to the block context.

## Context collection

`Collector` traverses the context DAG before the executable body. An acyclic
definition checks its right-hand side before introducing its binder. A recursive
node introduces all binders before visiting any right-hand side. During this
traversal the collector records:

- `ctxs_*`: variables available at a site.
- `coctxs_*`: variables used at a site.

## Errors and formatting

`ResolveError` reports unbound variables, duplicate definitions, duplicate
executable bodies, and missing or duplicate primitives. The `fmt` module provides
an "ugly" formatter for resolved terms and context bindings used in diagnostics.
