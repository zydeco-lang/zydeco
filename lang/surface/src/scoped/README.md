# Scoped (Name Resolution)

`scoped` performs name resolution on the desugared surface syntax (`bitter`).
It replaces `NameRef` occurrences with `DefId`s, checks scoping rules, and
builds a `ScopedArena` enriched with dependency graphs and local contexts.

## Role in the pipeline

```
textual -> bitter -> scoped -> statics
```

This pass is where variable names become bound identifiers (ABT-style), so later
phases no longer need to inspect name paths.

## Data model

- `ScopedArena` mirrors the bitter arenas but stores `Term<DefId>` and
  `Declaration` with resolved identifiers.
- `Context` tracks variables available at each term site; `CoContext` tracks
  free variables used at that site.
- `PrimDefs` records the definitions of primitives like `VType`, `CType`, `Thk`,
  `Ret`, etc., and validates they were provided.

## Resolution process

`Resolver` walks the top-level declarations, collects global binders, and then
resolves terms and patterns with a `(Local, Global)` lookup:

- Local bindings come from patterns and shadow earlier names.
- Global bindings are introduced once per top-level declaration.
- Internal terms inserted by desugaring (e.g., `VType`) are redirected to the
  matching primitive definitions and recorded as dependencies.

The pass also builds a dependency graph of top-level declarations that later
drives SCC analysis and context collection.

## Context collection

`Collector` runs after resolution to annotate each term and pattern with:

- `ctxs_*`: variables available at a site.
- `coctxs_*`: variables used at a site.
- `unis`: which declarations are non-recursive.

This uses an `SccGraph` to process strongly connected components in dependency
order, so recursive groups are handled consistently.

## Errors and formatting

`ResolveError` reports unbound variables, duplicate definitions, and missing or
duplicate primitives. The `fmt` module provides an "ugly" formatter for scoped
syntax for debugging.
