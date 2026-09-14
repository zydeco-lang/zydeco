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

- [lexer.rs](lexer.rs) supplies Logos tokens, typed lexical errors, and generated `TokenMetadata`.
- [parser.rs](parser.rs) exposes `StrictParser`, `RecoveringParser`, and their source-bound outcomes.
  Its private generated parser consumes [grammar.lalrpop](parser/grammar.lalrpop) through the arena allocator.
- [escape.rs](escape.rs) expands string and character escape sequences after literal parsing.

[C4's recovery contract](../../../../docs/references/compiler.md#recovering-parsing) owns the shared grammar,
strict acceptance, typed expectations, cursor replacement ranges, and exact recovery identities.
[Recovery tests](parser/tests/recovery.rs) compare retained structure with strict repairs and exercise token mutations,
Unicode prefixes, lexical failures, and abandoned recovery events.

## Spans and lookup helpers

`span` implements `SpanView` for textual IDs, including each nested `MetaId`,
and provides helpers on `SpanArena` for cursor/region lookup and for ordering entities by precision.
Directive diagnostics can therefore select an invalid metadata argument or payload instead
of highlighting the whole annotation.

## Errors and formatting

- [err.rs](err.rs) formats every retained parser issue with its source context.
- [ugly.rs](ugly.rs) implements debug spelling; the `fmt` module
  in [lib.rs](../lib.rs) re-exports rendering entry points.
- [pretty.rs](pretty.rs) constructs documents from textual syntax, trivia, and intentions.
  [Grammar contexts](pretty/context.rs), [punning](pretty/punning.rs),
  and [options](pretty/config.rs) supply its reusable policies; [corpus checks](pretty/corpus.rs) compare preservation
  and idempotence.

[C15](../../../../docs/references/compiler.md#formatting-and-typed-rendering) owns formatter laws,
source intentions, boundary algebra, canonical layout families, and directives.
The [language reference](../../../../docs/references/language.md#meta-annotations-compile-time-metadata) provides
meta annotation syntax, while [source analysis](../../../../docs/references/compiler.md#shared-source-analysis)
owns documentation/literal attachment decoding and diagnostics.
`NamedTermPunningAudit` remains a formatter test helper; its eventual retirement is tracked
in [deferred work](../../../../docs/ideas/formatter-punning-audit.md).
