# Well-Formed Program (wf)

`wf` post-processes the output of the type checker into a compact,
well-formed program representation. It assumes holes are solved and focuses
on producing a clean arena layout for downstream consumers.

## Role in the pipeline

```markdown
textual -> bitter -> scoped -> tyck -> wf
```

## What it produces

`WellFormedProgram` bundles:

- Typed arenas for kinds, types, values, and computations (with holes removed).
- A single entry computation assembled from top-level value bindings.
- Mapping tables back to the original textual entities for span reporting.
- Name-resolution metadata (contexts, usage, externals, recursion info).
- Type-checking metadata (sealed abstract types, data/codata, globals).

## Assembly strategy

The constructor walks the type-checked declarations in dependency order and:

- Collects value bindings into a let-chain that feeds the program entry.
- Records external definitions and the (single) entry point.
- Filters `Fillable` kinds/types to keep only concrete entries.
- Rewrites usage/context tables through the typed/untyped bijections.
