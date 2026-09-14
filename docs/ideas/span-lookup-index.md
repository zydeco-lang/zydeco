# Span lookup indexing

Profile the linear `SpanArena` cursor/range lookup before introducing an interval index.
Keep file-local editor coordinates separate from merged global spans.

[C2](../references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance) owns current source coordinates
and provenance.
