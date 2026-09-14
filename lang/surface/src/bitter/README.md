# Bitter (Desugared Surface Syntax)

`bitter` lowers parsed surface syntax to the shared syntax family consumed by name resolution.
The [compiler reference](../../../../docs/references/compiler.md#c4-parsing-desugaring-and-name-resolution)
owns the phase contract,
and its [rebuilding section](../../../../docs/references/compiler.md#surface-structural-rebuilding) owns allocation,
structural folding, and freshening.

```text
textual -> bitter -> scoped -> statics
```

## Implementation map

| Module | Responsibility |
| --- | --- |
| [`syntax`](syntax.rs) | Desugared syntax and IDs, with term references parameterized for resolution. |
| [`arena`](arena.rs) | Node storage, textual origins, and source binder annotations. |
| [`alloc`](alloc.rs) | `BitterBuilder`, allocation with provenance, and frozen publication. |
| [`desugar`](desugar/mod.rs) | `DesugarFolder`, recursive lowering, memoization, and diagnostic collection. |
| [`telescopes`](desugar/telescopes.rs), [`bindings`](desugar/bindings.rs) | Telescope rules and paired term/classifier construction. |
| [`cbpv`](desugar/cbpv.rs) | CBPV introductions and primitive construction through the builder. |
| [`freshen`](freshen.rs) | Fresh occurrence copies through the shared [`Folder`](../fold.rs). |
| [`err`](err.rs), [`fmt`](fmt.rs), [`span`](span.rs) | Diagnostics, debug formatting, and source locations. |

The [folder contract](../../../../docs/references/compiler.md#desugaring-folders) owns the implemented lowering rules.
The [traversal proposal](../../../../docs/proposals/traversals.md) collects the remaining meta annotation,
source analysis, and resolution work.
