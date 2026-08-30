# Worklog: Span Source Map

Chronological record of shrinking `Span` to an 8-byte position pair and introducing
the session source map (design account: `docs/ideas/span-source-map.md`).

## What changed

- `Span` is now `{ lo: BytePos, hi: BytePos }` (`u32` pair, `Copy`, `Ord`, 8 bytes).
  Per-span `Arc<PathBuf>` paths, the eagerly computed `CompactSpan2` line/col cache,
  `LocationCtx`, and `under_loc_ctx` are gone; the lalrpop grammar emits plain
  `Span::new(l, r)` and every `parse` signature lost its location parameter.
- `FileMap` (path, shared `Arc<str>`, base, `u32` line table) and `SourceMap`
  (files sorted by base, bases from 1 so position 0 stays dummy-only) live in
  `zydeco_utils::span`. Line/column resolution is lazy and counts **characters**,
  not bytes; UTF-16 conversion moved onto `FileMap` for the editor boundary.
- `SourceGraph::parse`'s merge builder assigns each file a global base in provider
  order, rebases cloned spans, and attaches the resulting `SourceMap` to the merged
  `SpanArena`. Later phases (desugar, resolve, statics) render spans through
  `SpanArena::source_map()` rather than per-span paths.
- Consumers: parse errors and the formatter use per-template local `FileMap`s;
  statics errors and `ResolveError::to_report` resolve through the arena's map;
  the CLI hole-solution printer uses `SourceMap::display`; cajun (LSP) localizes
  merged spans to file ranges before comparing offsets and keys its UTF-16
  conversions off per-file local `FileMap`s.

## Decisions made during implementation

- **The merged `SpanArena` carries the map** (`attach_map`/`source_map`) instead of
  threading an `Arc<SourceMap>` through salsa `ScopedData`. The `Tycker` already
  borrows the arena, so this avoids touching any tracked-struct signature; template
  arenas keep `None` and resolve through their own `FileMap`.
- **`BitterProgram::resolve` returns `ResolveFailure { error, spans }`** when it
  fails, handing the merged arena back with the error so `AnalysisError::Resolve`
  can still render diagnostics; without it the consumed program would take the
  only map-owning arena with it.
- cajun's span containment checks now localize spans via the map before comparing
  against file-local offsets — the first draft compared global offsets to local
  offsets and silently dropped every token and hover; the session tests passed
  because they only checked file attribution, not offset arithmetic.

## Known degradations, accepted

- `SourceLoadError::ImportPath`/`ImportInput` and `SourceCycleStep` print raw byte
  offsets (`at 12-30`) where they used to print `path:line:col`; these template-local
  spans have no map at hand and the message already names both files. Rendering them
  through the importer template's `FileMap` is a small follow-up if the messages matter.

## Verification

`cargo build --workspace`; focused tests pass for `zydeco-utils` (5 span tests incl.
base assignment, char columns, UTF-16 round trips), `zydeco-surface` (150),
`zydeco-session` (151), `zydeco-statics` (26), `cajun` (31), `zydeco-tui`, `zydeco-cli`.
End-to-end: `let a = "λλ" in in` reports `1:17` (character column; the old byte-column
rendering would say `1:18`), and the ariadne label underlines the right text.
