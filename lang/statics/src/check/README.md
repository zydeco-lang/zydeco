# Type Checking

`check` implements Zydeco's bidirectional kind and type checking. It consumes scoped surface
syntax, elaborates it into typed syntax (kinds, types, values, computations),
and records the resulting annotations in a `StaticsArena`.

## Role in the pipeline

```markdown
textual -> bitter -> scoped -> check -+-> dynamics
                                      +-> stackir -> assembly -> amd64 / llvm
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

## Neighboring statics modules

- `syntax` and `arena`: the durable typed representation and its annotation tables.
- `environment`: typing, substitution, structure, and monadic environments.
- `alloc`, `construct`, and `destruct`: typed allocation, construction, and inspection APIs.
- `normalize`: substitution, hole solving, scope support, and definitional normalization.
- `elaborate::monadic`: the algebra translation and its specialized construction API.
- `validate`: whole-program checks that consume typed syntax after local checking.
- `fmt` and `source_span`: source-aware formatting and span lookup; the latter remains crate-private.

The `check` module itself retains the checking rules, structured errors, least-upper-bound operations,
syntactic queries needed by those rules, and diagnostic dump helpers.
