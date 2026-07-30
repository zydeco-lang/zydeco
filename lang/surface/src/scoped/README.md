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
- A context binding is either a `Definition` with a right-hand side or an
  `External` with an optional classifier. Metadata is attached directly to the
  binding or body rather than represented by wrapper nodes.
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

After resolution, SCC analysis classifies each binding component and constructs
its condensation DAG. Source order is retained within recursive groups and breaks
ties between independent nodes.

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
