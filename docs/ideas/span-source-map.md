# Spans and a Session Source Map

Status: implemented 2026-08-30; see `docs/logs/2026-08-30-span-source-map.md`.
Two deltas from this text as implemented: the merged `SpanArena` carries the
`SourceMap` instead of threading it through salsa `ScopedData`, and template-local
load errors (import resolution, import cycles) render byte offsets for now.

## The problem

A span should answer exactly one question: *which bytes of which program does this node cover?*
Everything else — which file those bytes live in, what line and column they sit at, how to print
them — is derived data that belongs to the compilation, not to the node. The current
implementation inverts that split, and its costs compound at every phase.

Today `Span` (`lang/utils/src/span.rs`) is a 32-byte self-contained value:

```rust
pub struct Span {
    span1: (Cursor1, Cursor1),        // file-relative byte offsets (usize pair)
    span2: Option<CompactSpan2>,      // eagerly computed line/col, bit-packed into 4 bytes
    path: Option<Arc<PathBuf>>,       // per-span clone of the file path
}
```

The parser decorates every node at construction: each grammar production calls
`Span::new(l, r).under_loc_ctx(loc)`, which binary-searches the file's line table twice and
clones the path `Arc`. The consequences:

1. **Eager work where almost none is needed.** Line/column is computed for every AST node,
   but only a handful of spans ever reach a diagnostic. Parse time should pay `O(1)` per node.
2. **Duplicated file ownership.** Every span from one file carries its own `Arc<PathBuf>`;
   every `Span` clone — and desugaring copies spans constantly — performs an atomic refcount
   increment and decrement. `Span` cannot be `Copy`, and it is paid inline in every `Sp<T>`.
3. **A packing cliff.** `CompactCursor2` gives 18 bits to lines (262,142) and 14 bits to
   columns (16,383 bytes). Beyond either bound, error display silently degrades to raw byte
   offsets (`path:12345-12399`). A long generated or minified line already trips the column
   bound. The compression exists only to make the eager design affordable.
4. **Byte-based columns.** `Cursor2`'s column is a byte offset within the line, so diagnostics
   disagree with what a user counts on any line containing multibyte characters. Correct
   UTF-16 converters already exist in `FileInfo` but have no production callers.
5. **No positional identity across files.** Spans embed a path instead of living in an address
   space, so they cannot be ordered (`Span` has no `Ord`), and `SpanArena::lookup_cursor`
   linear-scans with `PathBuf` equality per span.

## Prior art

The compilers with the best diagnostics converge on one shape — a strict split between the
small ordered value and the lazily-consulted map:

- **rustc**: `Span` is an 8-byte compressed handle interning `SpanData { lo, hi: BytePos(u32),
  ctxt }`. Positions are global across all files in the crate's `SourceMap`; the file is
  recovered by binary search over file bases, line/col by binary search over per-file line
  tables, computed only when a span is displayed. Columns count characters.
- **Roslyn (C#)**: `TextSpan` is two `int`s relative to one `SourceText`; green tree nodes
  store only a *width*, absolute spans computed on traversal. `SourceText.Lines` is built
  lazily; columns are UTF-16 code units, which is what editors and LSP consume.
- **Swift**: `SourceRange` is two pointers into the file buffer; line tables are lazy.
- **Go**: `token.Pos` is one `int` in a global `FileSet` address space; `Position()` converts
  lazily (Go counts byte columns — a known wart, not a model to copy).
- **TypeScript**: `pos`/`end` ints per node, per-file line map.

The shared invariants: positions are small ordered integers in a single address space; file
identity, line, and column are derived on demand through a source map owned by the
compilation; the human-facing column unit is characters (or UTF-16 at an editor boundary),
never bytes.

## Why a global address space fits Zydeco

The pipeline already funnels every span through one merge point.
`SourceGraph::parse` (`lang/session/src/source/program.rs`) re-emits each template's AST
through a single `t::Parser`, cloning every per-file span into one program-wide `SpanArena`.
That builder is exactly where file-relative offsets can be rebased into one program-wide
address space at no extra traversal cost — the per-span `path` field exists today only to
compensate for the absence of this address space in the merged arena.

A per-file alternative (Roslyn-style spans plus a `FileId` side array in the arena) would also
work, but it keeps cross-file queries dispatching per file forever and gives up total ordering.
The global space subsumes the merge, matches rustc and Go's track record, and leaves room for
interval-based span lookup later.

## The design

### Types (`zydeco_utils::span`)

```rust
/// A byte position in one program's global address space.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BytePos(pub u32);

/// A byte range in one program's global address space. Exactly 8 bytes, `Copy`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span { lo: BytePos, hi: BytePos }

/// A resolved human-facing position. 0-based line, 0-based *character* column.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct LineCol { pub line: u32, pub column: u32 }

/// One file's contribution to the address space.
pub struct FileMap {
    path: Option<Arc<PathBuf>>,   // `None` for synthetic sources
    source: Arc<str>,             // shared with the `SourceTemplate`
    base: BytePos,                // global offset of the file's first byte
    line_starts: Vec<u32>,        // file-relative, starts with 0
}

/// The address space of one merged program.
pub struct SourceMap { files: Vec<FileMap> }  // sorted by base
```

`Span`'s API shrinks to construction (`new`, `dummy`), accessors (`lo`, `hi`, `range`),
predicates (`is_dummy`), and joining (`Span::join(a, b)` replaces ad-hoc span merging).
Its `Display` shows the offset pair only — debug identity, not user-facing text.

### Address space and dummy spans

The merge builder assigns file bases in provider order, starting at **1**; position 0 is
reserved so that `Span::dummy() == (0, 0)` is unambiguous — a real position can never be 0.
`debug_assert` guards the `u32` ceiling (a merged program over 4 GiB of source is not a case
this compiler has).

Per-template parsing keeps 0-based local offsets, exactly as the lexer emits them. A
template's own `FileMap` (base 0) serves parse-error rendering and the formatter's
`capture_source_information`, which today construct a throwaway `FileInfo` for the same
purpose. Dummy-vs-real ambiguity does not arise there because template-local spans are never
tested for dummy-ness.

### The merge (`TextualProgramBuilder`)

When the builder clones a span out of a template arena, it adds that file's base:

```rust
fn span(&self, source: SourceId, entity: t::EntityId) -> Span {
    self.graph.sources[&source].spans[&entity].rebase(self.bases[&source])
}
```

The builder collects `(path, Arc<str>, len)` per file as it goes and finishes by constructing
the `SourceMap`. The map rides along the existing ownership path — `TextualProgram` →
`ScopedProgram` → `ScopedData` → `Tycker` — replacing nothing, adding one `Arc<SourceMap>`
field per hop.

### Lazy resolution

```rust
impl SourceMap {
    fn file_of(&self, pos: BytePos) -> &FileMap;          // binary search over bases
    fn line_col(&self, pos: BytePos) -> LineCol;          // + binary search over line_starts,
                                                           //   column counted in characters
    fn range(&self, span: Span) -> (&FileMap, Range<usize>);  // file-local, for ariadne
    fn display(&self, span: Span) -> impl Display;        // "path:2:1 - 2:5"
    fn offset_utf16(&self, pos: LineCol) -> Option<BytePos>;  // editor boundary only
    fn line_col_utf16(&self, pos: BytePos) -> Option<LineCol>;
}
```

Character columns are counted from the retained `Arc<str>` at display time — `O(column)`, paid
only per diagnostic label. If that ever shows up in profiles, the follow-up is rustc's
per-file `multibyte_chars` table; it is deliberately not built now.

### Rendering

Both ariadne frontends already receive a file cache at their boundary (`SourceCaches`), so
they are the natural place to hold the `SourceMap`:

- `ParseError` (`textual/err.rs`) renders through the template's own `FileMap` instead of
  `FileInfo::trans_span2`, printing character columns for the first time.
- Statics errors (`statics/check/error.rs`) call `source_map.range(span)` instead of
  `span.to_ariadne_span()`; the `Tycker` holds the map alongside its span store.
- The hole-solution printer in `cli/src/diagnostics.rs` prints `source_map.display(span)`
  instead of the span's own `Display`.
- The cycle-error path (`SourceCycleStep`) keeps the span plus the file's map, both already
  reachable from the graph.

### Parser signatures

`LocationCtx` disappears. The grammar emits `Span::new(l, r)` — no decoration step — and the
`loc: &LocationCtx` parameter leaves `parse` signatures; roughly forty test call sites drop
their `&LocationCtx::Plain` argument. `under_loc_ctx`, `set_info`, `CompactSpan2`,
`CompactCursor2`, `FileInfo::trans_span1*`, and the ariadne conversion methods on `Span` are
deleted outright. `SpanArena::lookup_cursor`/`lookup_span` lose their path matching: in the
merged arena offsets alone identify containment.

### What stays

`Sp<T> = With<Span, T>` is unchanged in shape but its metadata becomes `Copy`, deleting the
refcount churn in every `make`/`mk`. The `SpanView`/`TextualBack`/`SpanStore` chain is
untouched by this change — inlining 8-byte spans into later-phase arenas to shorten that
chain is a separate, compatible follow-up.

## Migration order

One direct transition, in four reviewable commits:

1. `zydeco_utils::span`: add `BytePos`, `LineCol`, `FileMap`, `SourceMap` with unit tests
   (base assignment, binary search, char columns, UTF-16 round trips).
2. Shrink `Span`; update the grammar, parser, and every `under_loc_ctx`/`get_path` consumer;
   delete the eager machinery and the commented-out `Sp` block; parse errors render through
   the template's `FileMap`.
3. Merge rebasing and `SourceMap` construction; thread through `ScopedData` into the
   `Tycker`; switch both ariadne frontends and the hole-solution printer to map-based
   rendering.
4. Cleanup and docs: naming (`Cursor1`/`Cursor2` and `trans_span*` vanish with `FileInfo`),
   `DESIGN.md` note, full test pass.

## Open questions

- **Salsa identity.** The `SourceMap` is derived per merge and travels inside arenas already
  marked `no_eq`; it needs no memo identity of its own. Confirm no query keys on it.
- **Column unit for the CLI.** Characters (rustc) is chosen for display, UTF-16 reserved for
  the LSP boundary. If a TUI ever needs editor-aligned columns, it uses the UTF-16 accessors.
- **Interval lookup.** With ordered global spans, `lookup_cursor` can become a sorted
  structure when an LSP needs it; the linear scan stays until then.
- **Hygiene headroom.** rustc's interner exists to carry macro provenance (`SyntaxContext`,
  `ExpnId`). Zydeco has no macro system; if one arrives, a span interner can wrap this design
  without changing the address space.

## Representative outcome

A span on line 2 of an imported file, after a `λ` on the same line, currently renders as
`file.zy:2:5 - 2:9` where the 5 counts bytes including `λ`'s two; afterwards it renders
`file.zy:2:4 - 2:8` counting characters, the parse of a 300-node file performs 300 `Span::new`
calls with no line-table traffic, and `size_of::<Span>() == 8`.
