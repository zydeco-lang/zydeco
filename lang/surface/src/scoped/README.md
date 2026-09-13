# Scoped (Name Resolution)

`scoped` resolves names in desugared surface syntax and supplies the term graph consumed by static checking.
The [compiler reference](../../../../docs/references/compiler.md#c4-parsing-desugaring-and-name-resolution)
owns resolution, block scheduling, source boundaries, and their phase contracts.

```text
textual -> bitter -> scoped -> statics
```

## Implementation map

| Module | Responsibility |
| --- | --- |
| [`syntax`](syntax.rs) | Scoped syntax aliases, contextual bindings, and block plans. |
| [`arena`](arena.rs) | Resolved storage, source origins, and dependency graphs. |
| [`resolver`](resolver.rs) | Name lookup, lexical environments, and resolution entry points. |
| [`binders`](binders.rs), [`blocks`](blocks.rs) | Mobile binding discovery and dependency-directed elaboration. |
| [`completion`](completion.rs) | Exact cursor scope capture and completion results. |
| [`traverse`](traverse.rs) | Shared structural traversal and visitor composition. |
| [`context`](context.rs) | Free-variable analysis and the term summaries retained for checking. |
| [`err`](err.rs), [`fmt`](fmt.rs), [`span`](span.rs) | Diagnostics, debug formatting, and source locations. |

`ContextCollector` can share a traversal with other analysis visitors.
The [structural traversal contract](../../../../docs/references/compiler.md#scoped-structural-traversal)
specifies ordering, sharing, stopping, and the lifetime of its summaries.
Remaining folder migrations are collected in the [traversal proposal](../../../../docs/proposals/traversals.md).
