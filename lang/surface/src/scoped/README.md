# Scoped (Name Resolution)

`scoped` resolves names in desugared surface syntax and supplies the term graph consumed by static checking.
It covers resolution, block scheduling, and source boundaries.

```text
textual -> bitter -> scoped -> statics
```

## Implementation map

| Module | Responsibility |
| --- | --- |
| [`syntax`] | Scoped syntax aliases, contextual bindings, and block plans. |
| [`arena`] | Resolved storage, source origins, and dependency graphs. |
| [`resolver`] | Semantic scheduling, structural rebuilding, recovery, and publication. |
| [`scope`] | Shared name lookup and lexical scope enumeration. |
| `alloc` | Scoped syntax construction and provenance. |
| [`observers`] | Reference indexing and exact cursor capture. |
| `dependencies` | Required block graphs with begin, finish, and abort boundaries. |
| [`binders`], `blocks` | Mobile binding discovery and dependency-directed elaboration. |
| `completion` | Recovered resolution results for completion. |
| [`traverse`] | Shared structural traversal and visitor composition. |
| [`context`] | Free-variable analysis and the term summaries retained for checking. |
| [`err`], [`fmt`], `span` | Diagnostics, debug formatting, and source locations. |

`ContextCollector` can share a traversal with other analysis visitors.
