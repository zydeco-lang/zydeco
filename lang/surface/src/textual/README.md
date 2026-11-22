# Textual (Parsed Surface Syntax)

`textual` defines the concrete surface syntax and produces the first AST from source text.
It owns lexing, parsing, token-stream `--|` text-block capture, and span collection,
and stores parsed nodes in arenas keyed by lightweight IDs.

## Role in the pipeline

```markdown
textual -> scoped -> statics
```

This phase is responsible for turning source text into structured syntax with accurate span information,
without performing desugaring, name resolution, or formatting trivia capture.

## Data model

- `TextArena` stores parsed definitions, patterns, copatterns, metadata, and terms keyed by `DefId`, `PatId`,
  `CoPatId`, `MetaId`, and `TermId`.
- `EntityId` is a tagged enum over those categories; it is not an unchecked shared raw index.
- `SourceUnit` identifies the single complete term in one source file.
  It decodes typed metadata directives attached to holes, collects `@[doc]` attachments for arbitrary terms,
  and validates `@[literal]` splices without assigning them a presentation.
- `text_block` captures contiguous `--|` blocks directly from the Logos token stream and attaches them
  to entities by span.
- Parsed metadata is an arena-backed tree of `MetaNode` values.
  Every nested value has a `MetaId`, so it participates in span lookup
  and formatting presentation like other textual syntax.
  The textual-to-scoped boundary lowers and resolves metadata into the span-free `Meta` representation
  and typed actions used by later phases.
  Concrete interpretations live in `zydeco_surface::metadata`, implement `SpecializeMeta`,
  and are requested explicitly by the phases that consume them.
- `SpanArena` stores `Span` values for every textual entity so later phases can report precise locations;
  it is storage-only and retains no ID allocator.
- `Parser` combines `TextArena` and `SpanArena` and is passed through the LALRPOP-generated parser.
  When invoked through `Parser::for_formatting()`, it also retains structural prefix ranges consumed
  by `zydeco-fmt`; otherwise formatting trivia and layout intentions (`SurfaceTrivia`,
  `SurfaceIntentions`) live strictly in `lang/fmt/` and are never captured during compilation.
  It owns the `KeySpace` only while nodes are being parsed, then `finish` returns the two durable arenas
  and drops the issuer.

## Lexing and parsing

- [`lexer`] supplies Logos tokens, typed lexical errors, and generated `TokenMetadata`.
- [`parser`] exposes `StrictParser`, `RecoveringParser`, and their source-bound outcomes.
  Its private generated parser consumes `parser/grammar.lalrpop` through the arena allocator.
- [`escape`] expands string and character escape sequences after literal parsing.

Both parsers share one grammar.
Recovery preserves strict acceptance, typed expectations, cursor replacement ranges, and exact recovery identities.
Recovery tests in `parser/tests/recovery.rs` compare retained structure with strict repairs
and exercise token mutations, Unicode prefixes, lexical failures, and abandoned recovery events.

## Spans and lookup helpers

`span` implements `SpanView` for textual IDs, including each nested `MetaId`,
and provides helpers on `SpanArena` for cursor/region lookup and for ordering entities by precision.
Directive diagnostics can therefore select an invalid metadata argument or payload instead
of highlighting the whole annotation.

## Errors and formatting

- [`err`] formats every retained parser issue with its source context.
- Source layout formatting lives in the separate `zydeco-fmt` crate (`lang/fmt/`), consuming parsed `TextArena`,
  `SpanArena`, and `FormatPresentation` while parser/formatter corpus tests live under `tests/corpus`.

Formatting follows shared laws over source intentions, boundary algebra, canonical layout families, and directives.
Source analysis decodes documentation and literal attachments and reports their diagnostics.
