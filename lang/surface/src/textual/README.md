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
- Parsed metadata is an arena-backed tree of `MetaNode` values. Every nested value has a `MetaId`,
  so it participates in span lookup, source-layout retention, and comment attachment like other textual syntax.
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

- `lexer` uses `logos` to tokenize the input. One shared token stream defines comment and literal boundaries for
  parsing, source tooling, and completion. The parser-facing `Lexer` skips trivia and returns malformed lexemes as
  source-located `LexicalError` values; a stray `-/` or unfinished block comment never silently ends the stream.
  `TokenMetadata` derives payload-free `TokenKind` values, fixed spellings, and grammar labels from `Tok` and its
  Logos attributes. Explicit metadata selects canonical aliases and excludes trivia and malformed tokens from
  parser expectations. Fixed-token formatting reuses these spellings.
- `parser` exposes strict and recovering entry points over one LALRPOP grammar, `parser/grammar.lalrpop`.
  Its private `parser::generated` implementation consumes tokens and builds the textual AST by calling into the
  arena-allocating `Parser`.
- `StrictParser` is the compilation, source-loading, and formatting entry point. It rejects a source whenever
  LALRPOP used recovery, even if recovery retained a complete root, so tooling support cannot widen the language's
  ordinary acceptance boundary accidentally.
- `RecoveringParser` retains a partial root and structured `ParseIssue` values for editor queries. With a validated
  `CompletionCursor`, its token adapter removes the active source token and inserts a parser-only, zero-width marker.
  Minimal recovery points at term and pattern atoms turn that marker into a typed `CompletionHole`; the same points
  produce typed `RecoveryHole` values for genuine malformed input. Both remain ordinary semantic holes in the arena,
  while the parse result keeps their distinct tooling origin outside canonical syntax.
- `RecoveringParser::new(source)` and `RecoveringParser::at(source, offset)` borrow one immutable source snapshot.
  Their `source`, `term`, and `pattern` methods take only an arena allocator, so a validated cursor cannot be reused
  against different text. Comments and unfinished quoted literals own their EOF insertion cursor.
- Grammar semantic values carry recovery handles until their allocation rule records the exact typed AST ID.
  No span lookup identifies a recovery hole. A later recovery can pop an already allocated node, so a
  `CompletionHole` is exposed only when it is reachable from the returned root. Ordinary issue-to-hole links record
  allocation history and can refer to abandoned nodes; they do not establish usable completion context.
- `CompletionSite` reports the full replacement range and typed `TokenKind` values when the cursor instead
  occupies a fixed grammar position such as `in`, `that`, `=>`, or `end`. Raw LALRPOP terminal strings are converted
  at the parser boundary and are not part of the public tooling contract.
- The parser adapter turns lexical errors into a grammar-known `Invalid` terminal, allowing LALRPOP's existing
  recovery points to handle them. Numeric conversions use typed fallible actions. In particular, metadata integer
  overflow returns a source-located `LiteralError` and no root instead of panicking. Fallible-action failures stop
  parsing; they do not invoke `!` recovery, and earlier recovery issues are retained.
- `escape` expands string and char escape sequences after parsing literals.

The [completion proposal](../../../../docs/proposals/completion.md#trust-boundary-and-recovery-contracts) records
the recovery contracts. Tests compare the two modes on the shared repository corpus, compare recovered shapes with
explicit repairs accepted by the strict parser, and exercise deterministic token mutations and edit prefixes.
The grammar remains the sole syntax specification and generated reference parser; these are integration and
regression checks, not a separate parser or a formal proof of optimal recovery.

## Spans and lookup helpers

`span` implements `SpanView` for textual IDs, including each nested `MetaId`, and provides helpers on `SpanArena`
for cursor/region lookup and for ordering entities by precision. Directive diagnostics can therefore select an
invalid metadata argument or payload instead of highlighting the whole annotation.

## Errors and formatting

- `err` formats parser errors with file path and location context.
- `fmt::ugly` renders textual syntax back into a safe surface form for debugging and diagnostics.
- `fmt::PrettyFormatter` renders the same textual arenas through compositional documents.
  `PrettyOptions` configures width, indentation, how much recorded line layout is retained (every break,
  blank lines only, or none), and whether redundant singleton grouping parentheses are preserved.
  A meaningful multiline group remains an indentation boundary,
  while an application's own compact-or-hanging layout subsumes an extra singleton wrapper.
  A `@[format(...)]` directive overrides the options for its annotated expression,
  nested directives override enclosing ones, a width change pre-renders its payload as an embedded block,
  and `@[format(verbatim)]` copies the annotated source text unchanged.

The [surface formatting design](../../../../docs/proposals/formatting.md) states the printer's preservation laws,
boundary algebra, and canonical layout families.

Line comments are canonicalized as `--` or `--|` lines, nested block comments retain their delimiters
and relative indentation, and all comment kinds are always printed.
Arm-boundary anchors keep comments on the intended side of `|` and its header.
Recorded blank separators remain blank separators after formatting.
Only an uninterrupted adjacent `--|` block attaches to a following annotation: `@[doc]` renders the block
as repository prose, while `@[literal]` replaces its hole payload with the block text as a string literal.
An ordinary comment continues to separate text from the annotation.
Source analysis warns about every `--|` block without such an attachment because that block contributes no text
to any annotation.

Parenthesized metadata is sugar for the bracket form with a hole payload: `@(meta)` parses as `@[meta] _`.
The pretty printer renders a metadata annotation in its parenthesized form whenever its payload is a hole,
so `@[intrinsic(i64)] _` and `@(intrinsic(i64))` are indistinguishable and both format as `@(intrinsic(i64))`.
Metadata applications use the same compact-or-expanded delimiter layout as term groups. Their layout is decided
from the metadata itself, so a long following payload cannot expand a short annotation; nested calls, retained
source rows, and comments between arguments remain independently structured. A comment before an annotation's
`@` remains outside its brackets, while a comment after the opening bracket remains inside the metadata wrapper.

The pretty printer treats concise puns as canonical syntax rather than author intent.
Named terms, named patterns, and projection patterns
therefore use their punned spelling whenever their payload is the same-named variable, including the annotated forms.
`NamedTermPunningAudit` can inventory explicit term fields that will become puns;
the standard-library regression currently records 96 such fields without rewriting those source files.

Minimal parenthesis formatting follows the parser's typed precedence contexts.
A singleton group is removed only when its child is accepted in that exact grammar position,
so right-associated arrows may lose redundant grouping while the left side of an arrow
or an atomic force operand retains grouping when required.
The regression suite also compares the original and formatted standard library after desugaring,
which checks that these presentation changes preserve the language-level structure.
