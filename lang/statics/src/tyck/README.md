# Type Checker (tyck)

`tyck` implements the static semantics of Zydeco. It consumes the scoped surface
syntax, elaborates it into typed syntax (kinds, types, values, computations),
and records the resulting annotations in a `StaticsArena`.

## Role in the pipeline

```markdown
textual -> bitter -> scoped -> tyck -> wf
```

The type checker is the first phase that separates term categories and assigns
explicit kinds and types.

## Core data structures

- `Tycker` is the driver that walks the scoped program, accumulates errors, and
  builds the `StaticsArena`.
- `StaticsArena` stores typed nodes (kinds/types/values/computations) along with
  annotations and auxiliary tables (sealed abstract types, data/codata, globals).
- `AnnId`/`TermAnnId` carry the inferred or checked annotation for each node.
- `FillId` and `Fillable<T>` represent type/kind holes that are solved during
  checking and resolved at the end of the pass.

## Type checking flow

The top level is processed in SCC order (from the resolver’s dependency graph).
Declarations are checked in either synthesis or analysis mode, and the checker
keeps a task stack (`TyckTask`) to enrich error reports.

Primitive definitions (e.g., `VType`, `CType`, `Thk`, `Ret`) are registered
early so internal surface terms can be linked to their typed equivalents.

## Monadic blocks and algebra translation

Monadic blocks are elaborated during type checking via the algebra translation
implemented in `monadic`. This translation uses a monadic construction API and
specialized environments to lift terms into a user-supplied monad.

## Helper modules

- `env`: typing environments, substitution maps, and monadic environments.
- `alloc`: typed arena allocation with annotation tracking.
- `construct`/`moncons`: HOAS-style builders for internal transformations.
- `destruct`: destructors and helpers for inspecting typed nodes.
- `syntactic`: syntactic checks for annotations, seals, and usage.
- `lub`, `norm`: least-upper-bound and normalization/substitution utilities.
- `err`, `fmt`, `span`: error reporting, formatting, and span lookup.
