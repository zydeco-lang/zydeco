# Bitter (Desugared Surface Syntax)

`bitter` lowers parsed surface syntax to the shared syntax family consumed by name resolution.

```text
textual -> bitter -> scoped -> statics
```

## Implementation map

| Module | Responsibility |
| --- | --- |
| [`syntax`] | Desugared syntax and IDs, with term references parameterized for resolution. |
| [`arena`] | Node storage, textual origins, and source binder annotations. |
| [`alloc`] | `BitterBuilder`, allocation with provenance, and frozen publication. |
| [`desugar`] | `DesugarFolder`, recursive lowering, memoization, and diagnostic collection. |
| `telescopes`, `bindings` | Telescope rules and paired term/classifier construction. |
| `cbpv` | CBPV introductions and primitive construction through the builder. |
| `meta` | Read-only annotation inspection and typed lowering actions. |
| [`freshen`] | Fresh occurrence copies through the shared [`crate::fold::Folder`]. |
| [`err`], [`fmt`], `span` | Diagnostics, debug formatting, and source locations. |
