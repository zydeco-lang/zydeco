# Textual (Parsed Surface Syntax)

`textual` defines the concrete surface syntax and produces the first AST from source text.
It owns lexing, parsing, and span collection, and stores parsed nodes in arenas keyed by lightweight IDs.

## Role in the pipeline

```markdown
textual -> bitter -> scoped -> statics
```

This phase is responsible for turning source text into structured syntax with accurate span information,
but it does not perform desugaring or name resolution.

## Data model

- `TextArena` stores parsed definitions, patterns, copatterns, metadata, and terms keyed by `DefId`, `PatId`,
  `CoPatId`, `MetaId`, and `TermId`.
- `EntityId` is a tagged enum over those categories; it is not an unchecked shared raw index.
- `SourceUnit` identifies the single complete term in one source file.
  It decodes typed metadata directives attached to holes, collects `@[doc]` attachments for arbitrary terms,
  and validates `@[literal]` splices without assigning them a presentation.
- Parsed metadata is an arena-backed tree of `MetaNode` values.
  Every nested value has a `MetaId`, so it participates in span lookup, source-layout retention,
  and comment attachment like other textual syntax.
  The textual-to-bitter boundary lowers the tree to the span-free `Meta` representation used by later phases.
  Concrete interpretations live in `zydeco_surface::metadata`, implement `SpecializeMeta`,
  and are requested explicitly by the phases that consume them.
- `SpanArena` stores `Span` values for every textual entity so later phases can report precise locations;
  it is storage-only and retains no ID allocator.
- `SurfaceIntentions` is an auxiliary arena keyed by `EntityId`.
  It records layout choices such as whether a parsed entity crossed a line boundary
  without adding presentation-only variants to the syntax tree.
- `SurfaceTrivia` retains source content outside the syntax tree.
  Text, line, and nested block comments are kept as typed values in one source-ordered sequence and anchored
  to stable textual entities, so formatters can move the surrounding syntax without discarding comment content.
- `Parser` combines `TextArena` and `SpanArena` and is passed through the LALRPOP-generated parser.
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
- The [`fmt`] module re-exports the pretty printer's entry points and options.
- `pretty` constructs documents from textual syntax, trivia, and intentions.
  Its private `context`, `punning`, and `config` modules supply grammar contexts, punning, and options;
  corpus checks in `corpus` compare preservation and idempotence.

Formatting follows shared laws over source intentions, boundary algebra, canonical layout families, and directives.
Source analysis decodes documentation and literal attachments and reports their diagnostics.
