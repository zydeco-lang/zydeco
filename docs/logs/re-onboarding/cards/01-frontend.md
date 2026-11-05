# 01 — Front End (syntax, surface, parsing through resolution, formatter)
> Scratch study note for re-onboarding. Not canonical: contracts below point to their authoritative references.

## 1. Responsibility

`lang/syntax` supplies the shared vocabulary: interned names, `Meta`, primitive/scalar/memory bases, spans,
and the `Pretty`/`Ugly` formatter traits, where `Ugly` projects `Pretty` at unbounded width
(`lang/syntax/src/lib.rs:1075`, `:1087`; `lang/syntax/src/fmt.rs:6-22`). `lang/surface` owns the three
front-end representations plus the formatter that consumes the parsed form:

- **textual** — Logos tokens, the LALRPOP grammar, strict and recovering parse modes, spans, retained trivia
  and layout intentions (`lang/surface/src/textual/`).
- **bitter** — sugar removal with names still unresolved: binding headers, telescopes, nominal sealing,
  CBPV introductions, and retained special boundaries (`lang/surface/src/bitter/`).
- **scoped** — names replaced by `DefId`s, plus block collection, dependency scheduling, observers, and
  completion observations (`lang/surface/src/scoped/`).
- **formatter** — a semantics-preserving normalizer over the same textual arena, shared by `zydeco fmt` and
  Cajun (`lang/surface/src/textual/pretty.rs`).

[C4](../../../../docs/references/compiler.md#c4-parsing-desugaring-and-name-resolution) owns parsing,
desugaring, and resolution; the formatting half of
[C15](../../../../docs/references/compiler.md#c15-diagnostics-formatting-documentation-and-interactive-tooling)
owns source layout. [L2](../../../../docs/references/language.md#2-lexical-structure-and-syntax) and
[L3](../../../../docs/references/language.md#3-bindings-and-scope) own surface syntax and binding scope.

Out of scope: source graph, packages, overlays, session assembly, and CLI driver — see the
[tooling card](06-tooling.md) and [C3](../../../../docs/references/compiler.md#c3-source-loading-sessions-queries-and-memory-retention);
typed syntax, checking, and typed arenas — see the [statics card](02-statics.md) and
[C5](../../../../docs/references/compiler.md#c5-typed-representation-judgments-and-inference). This card
ends at the `ScopedProgram` product.

## 2. Position in the Pipeline

Upstream, the session parses each file (`SourceTemplate::parse` calls `StrictParser::source` and
`SourceInventory::scan`, `lang/session/src/source/loader.rs:46-71`) and assembles one merged
`TextualProgram` (`lang/session/src/source/program.rs:15-21`). Loading, package context, and assembly are
[C3's boundary](../../../../docs/references/compiler.md#source-loading-and-package-selection) and
[L12](../../../../docs/references/language.md#12-sources-imports-and-entry); the [tooling card](06-tooling.md)
covers them.

The phase seams are three concrete products in `lang/session/src/source/pipeline.rs:16-62`:
`TextualProgram::desugar` → `BitterProgram` → `BitterProgram::resolve` → `ScopedProgram` → statics.
[C1's product table](../../../../docs/references/compiler.md#c1-architecture-and-a-programs-path-through-the-compiler)
is the canonical version of that chain.

Formatting takes a different route: `zydeco fmt` reads one file, runs `StrictParser::source`, and renders
with `PrettyFormatter::with_source` (`cli/src/format.rs:67-74`); it builds no program and resolves no names
([C15 formatting](../../../../docs/references/compiler.md#formatting-and-typed-rendering)). Editor completion
uses the recovering parser plus a request-local resolution run instead
(`lang/session/src/source/query/completion.rs:124`;
[C4 recovering parsing](../../../../docs/references/compiler.md#recovering-parsing)).

Assembly rebuilds syntax into a fresh `Parser` arena (`lang/session/src/source/program.rs:50-53`) and never
calls `capture_source_information`; source-fidelity data stays on each per-file `SourceTemplate`
(`lang/session/src/source/graph.rs:30-46`). Formatter and editor presentation therefore consume a per-file
parse, not the merged program arena.

## 3. Entry Points and Key Types

| What | Where | Why it matters |
| --- | --- | --- |
| Token model and generated terminal metadata | `lang/surface/src/textual/lexer.rs:10-11`, `:44-47`, `:69-75` | One lexer feeds parser, diagnostics, and tooling; `TokenMetadata` carries canonical spellings such as `def`. |
| Grammar as the single syntax specification | `lang/surface/src/textual/parser/grammar.lalrpop:428-433`, `:512`, `:527`, `:268` | Precedence, backward application, `sigma`, and view patterns live here; strict and recovering modes share it. |
| `StrictParser::source` | `lang/surface/src/textual/parser.rs:430-455` | Compilation and formatting entry point; accepts only a root produced with zero recovery issues. |
| `RecoveringParser` and `CompletionCursor` | `lang/surface/src/textual/parser.rs:463-536`, `:365-427` | Editor entry point: partial root, typed issues, completion hole, expected `TokenKind`s. |
| `Parser`, `TextArena`, `SpanArena` | `lang/surface/src/textual/syntax.rs:396-427`; `lang/surface/src/textual/arena.rs:34-49` | Durable arenas keyed by `EntityId`; presentation layers (`SurfaceIntentions`, `SurfaceTrivia`) sit beside the syntax. |
| `Parser::capture_source_information` | `lang/surface/src/textual/syntax.rs:498-604`; called from `lang/surface/src/textual/parser/grammar.lalrpop:578-583` | Fills the presentation layer on every accepted parse, without adding layout variants to the AST. |
| `SourceUnitDesugarer` / `DesugarFolder` | `lang/surface/src/bitter/desugar/mod.rs:27-81` | Memoized textual→bitter lowering; publishes a bitter arena only when no diagnostic was recorded. |
| `MetaRules` | `lang/surface/src/bitter/desugar/meta.rs:19-121` | Typed annotation actions; validates the same catalog entries that the source scanner validates. |
| `ResolveFolder` and observers | `lang/surface/src/scoped/resolver.rs:14-22`, `:47-76`, `:91-151` | Replaces names with `DefId`s, schedules blocks, and publishes strict or completion-local results. |
| `BlockCandidateCollector`, `DependencyAnalyzer` | `lang/surface/src/scoped/blocks.rs:69`; `lang/surface/src/scoped/dependencies.rs:6` | Forward references and dependency-directed elaboration of `begin ... end` blocks. |
| `MetadataKind`, `MetadataCatalog` | `lang/surface/src/metadata.rs:38-68`, `:149-161` | Closed compiler catalog over the open structural `Meta` tree; drives scanner, desugarer, formatter, editor. |
| `PrettyFormatter`, `SourceFormatter` | `lang/surface/src/textual/pretty.rs:22-30`, `:360-421`; `cli/src/format.rs:37-74` | Canonical formatting; the CLI adapter adds file IO and `fmt --check`. |
| `TextualProgram`, `BitterProgram`, `ScopedProgram` | `lang/session/src/source/pipeline.rs:16-62` | The phase seams this module publishes to statics. |

## 4. Contracts and Invariants

- **One grammar, two explicit outcomes.** Both modes run the same generated parser; strict mode accepts
  only a returned root with an empty issue list (`lang/surface/src/textual/parser.rs:430-455`). Recovery
  policy lives in the grammar's `!` productions, not in a second parser
  ([C4 trust boundary](../../../../docs/references/compiler.md#trust-boundary-and-recovery-contracts)).
- **Recovery identity is typed.** A completion hole and a recovery node are distinct values carrying their
  own entity IDs and spans; a completion is exposed only if its allocated hole stays reachable from the
  returned root (`lang/surface/src/textual/parser.rs:124-151`, `:492-533`;
  [C4 recovering parsing](../../../../docs/references/compiler.md#recovering-parsing)).
- **Terminals and diagnostics share generated metadata.** Logos attributes generate canonical spellings and
  parser terminal names; `define` and `def` both lex as `Define` with canonical spelling `def`
  (`lang/surface/src/textual/lexer.rs:44-47`; test
  `lang/surface/src/textual/parser/tests.rs:424-441`).
- **Presentation is captured outside the syntax tree.** Every accepted parse records line extents, boundary
  break intents, and comment anchors keyed by `EntityId`; the AST gains no layout-only variants, and a
  formatter may ignore the intentions but not the trivia
  (`lang/surface/src/textual/syntax.rs:498-604`, `lang/surface/src/textual/intention.rs:125-138`,
  `lang/surface/src/textual/trivia.rs:13-25`;
  [C15 retained source information](../../../../docs/references/compiler.md#retained-source-information)).
- **Desugaring is all-or-nothing and provenance-preserving.** A memo table prevents duplicate reporting of a
  reused rejected source term; a rejected source publishes no bitter arena, and every allocation stores its
  textual origin (`lang/surface/src/bitter/desugar/mod.rs:33-36`, `:62-72`;
  `lang/surface/src/bitter/alloc.rs:31-64`;
  [C4 desugaring folders](../../../../docs/references/compiler.md#desugaring-folders)).
- **Resolution owns scope and scheduling, and publishes strictly.** Block candidates are collected before
  bodies are visited, dependency graphs order bindings, and any strict-policy diagnostic rejects the whole
  program while observations remain available to tooling (`lang/surface/src/scoped/blocks.rs:69-78`,
  `lang/surface/src/scoped/dependencies.rs:6-35`, `lang/surface/src/scoped/resolver.rs:99-126`;
  [C4 name resolution](../../../../docs/references/compiler.md#name-resolution),
  [L3](../../../../docs/references/language.md#3-bindings-and-scope)).
- **Formatting is a semantics-preserving normalizer with source-driven policy.** Reparse, desugared-shape
  equality, comment retention, and idempotence are checked over the repository corpus
  (`lang/surface/src/textual/pretty/corpus.rs:61-88`); options come from `@[format(...)]` in the source, not
  frontend settings ([C15 formatter laws](../../../../docs/references/compiler.md#formatter-laws),
  [components and policy](../../../../docs/references/compiler.md#components-and-policy)). A live exception to
  the identity law is recorded in §10.

## 5. Data Flow

Source text is lexed into Logos tokens and parsed by the generated LALRPOP grammar into a textual arena;
spans are stored beside every node. A successful parse immediately captures presentation data into two
side layers, then either compilation (desugar → resolve → statics) or the formatter consumes the same arena.
The session has already assembled per-file arenas into one program DAG before desugaring; formatting and
editor presentation start from a per-file parse instead.

```text
file text
  │ Lexer (Logos tokens, TokenMetadata spellings)
  ▼
tokens ──► LALRPOP grammar (strict │ recovering + completion cursor)
  │                  └─ issues/holes/expected ──► CompletionSite (editor only)
  ▼
TextArena + SpanArena  (DefId/PatId/CoPatId/MetaId/TermId, EntityId tag)
  ├─► SurfaceIntentions (line extents, boundary BreakIntents)
  └─► SurfaceTrivia     (comments, --| blocks, attachment)
  ▼
session assembly: copy per-file syntax into TextualProgram (C3; tooling card)
  ▼
DesugarFolder ──► BitterArena  (origins textual→bitter; names remain textual)
  ▼
ResolveFolder ──► ScopedArena  (Var→DefId, binder environments, block DepGraphs)
  ▼
statics (C5; statics card)

formatting path:  file text → StrictParser::source → PrettyFormatter::with_source → Doc → width render
```

## 6. Verification Commands

Run from the repository root with `cargo run --quiet --bin zydeco --` because the installed `zydeco` may
lag; `target/debug/zydeco` is equivalent once built. Commands write scratch input only under `/tmp`.
The formatter corpus test takes about 75 s on this host.

| Purpose | Command | Expected observable outcome |
| --- | --- | --- |
| Accept views and backward application | `cargo run --quiet --bin zydeco -- check lib/tests/core/value-views.zy` | Exit 0, silent (view patterns and backward application parse, desugar, resolve, check). |
| Accept `sigma` | `cargo run --quiet --bin zydeco -- check lang/tests/cases/quantifiers/accepts_multiple_parameters_for_dependent_quantifiers.zy` | Exit 0, silent. |
| See text-block warnings | `cargo run --quiet --bin zydeco -- check lib/tests/oopsla/core.zydeco 2>&1 \| grep -c "Warning: text block is not attached"` | Prints `10`; exit 0 despite warnings. |
| Strict parse rejection | `printf 'fn x =>\n' > /tmp/frontend-malformed.zy && cargo run --quiet --bin zydeco -- check /tmp/frontend-malformed.zy` | Exit 1; `Error: Unrecognized EOF`; the expectation list spells `def`, not `define`. |
| Resolution rejection | `printf 'fn x => y\n' > /tmp/frontend-unbound.zy && cargo run --quiet --bin zydeco -- check /tmp/frontend-unbound.zy` | Exit 1; `Error: Unbound variable` with `variable 'y' is not defined`. |
| Canonical convergence of a synonym | `printf 'define answer = 1 in answer\n' > /tmp/frontend-define.zy && cargo run --quiet --bin zydeco -- fmt /tmp/frontend-define.zy && cat /tmp/frontend-define.zy` | File becomes `def answer = 1 in` / `answer`. |
| Abbreviation normalization | `printf 'comatch (value : Int) => ret value end\n' > /tmp/frontend-comatch.zy && cargo run --quiet --bin zydeco -- fmt /tmp/frontend-comatch.zy && cat /tmp/frontend-comatch.zy` | File becomes `fn (value : Int) => ret value`. |
| Repository formatting baseline | `cargo run --quiet --bin zydeco -- fmt --check lib/std/builtin.zy lib/tests/core/value-views.zy` | Exit 0, silent (checked-in sources are canonically formatted). |
| `verbatim` escape hatch | `printf '@[format(verbatim)] (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)\n' > /tmp/frontend-verbatim.zy && cargo run --quiet --bin zydeco -- fmt --check /tmp/frontend-verbatim.zy` | Exit 0; the long payload is not reflowed. |
| Width-driven layout | `printf 'fn (value : Int) => ret (value, value, value, value, value, value, value, value, value, value, value, value, value, value)\n' > /tmp/frontend-long.zy && cargo run --quiet --bin zydeco -- fmt /tmp/frontend-long.zy && wc -l /tmp/frontend-long.zy` | `17`; the overflowing product expands one item per line. |
| Formatter laws over the corpus | `cargo test -p zydeco-surface --lib -- textual::pretty::corpus::repository_programs_preserve_formatter_laws` | `1 passed; 0 failed; 288 filtered out` after ≈75 s. |
| Recovery contract | `cargo test -p zydeco-surface --lib -- textual::parser::tests::recovery::recovery_matches_explicit_strict_repairs_and_retains_later_syntax` | `1 passed; 0 failed; 288 filtered out`. |
| Companion `.zyi` load | `mkdir -p /tmp/frontend-pair && printf '@(intrinsic(unit))\n' > /tmp/frontend-pair/library.zyi && printf '()\n' > /tmp/frontend-pair/library.zy && cargo run --quiet --bin zydeco -- check /tmp/frontend-pair/library.zy` | Exit 0 for the matching companion; changing the `.zyi` root to `@(intrinsic(int))` instead exits 1 with `[tyck.type-mismatch]`. |

## 7. Reading Order

1. [C4](../../../../docs/references/compiler.md#c4-parsing-desugaring-and-name-resolution) whole chapter, then
   [C15 formatting and typed rendering](../../../../docs/references/compiler.md#formatting-and-typed-rendering)
   with its retained-source and formatter-law subsections.
2. [L2](../../../../docs/references/language.md#2-lexical-structure-and-syntax) and
   [L3](../../../../docs/references/language.md#3-bindings-and-scope) for the surface the code must accept.
3. `lang/surface/src/textual/README.md`: the textual model in one page.
4. `lang/surface/src/textual/lexer.rs` with `lang/derive/src/lib.rs` (the `TokenMetadata` macro), then
   `lang/surface/src/textual/parser/grammar.lalrpop:428-583` (precedence summary and `SourceUnit`).
5. `lang/surface/src/textual/parser.rs`: strict versus recovering entry points and recovery values.
6. `lang/surface/src/textual/syntax.rs:396-604` beside `intention.rs` and `trivia.rs`: the parser arena and
   the presentation capture it runs.
7. `lang/surface/src/bitter/README.md`, then `lang/surface/src/bitter/desugar/mod.rs:27-81` and
   `lang/surface/src/bitter/desugar/meta.rs:19-121`: lowering strategy and annotation actions.
8. `lang/surface/src/scoped/README.md`, then `lang/surface/src/scoped/resolver.rs:14-151`,
   `lang/surface/src/scoped/blocks.rs:69-120`, and `lang/surface/src/scoped/dependencies.rs:6-55`:
   names, block scheduling, publication.
9. `lang/surface/src/textual/pretty.rs:22-120` (`PrettyFormatter`, `LayoutFragment`, `GroupLayout`),
   `lang/surface/src/textual/pretty/config.rs:103-153`, then the corpus check in
   `lang/surface/src/textual/pretty/corpus.rs:61-88`.
10. `lang/surface/src/metadata.rs:20-161` plus the two evaluation findings in §9; the
    [appendix](../appendix-cleanup-candidates.md#f-surface-form-counts-at-e2dd8979) reproduces the counts.

## 8. Checkpoint Questions

1. What exactly separates strict from recovering parsing, and why can no strict caller accept a recovered root?
   → `lang/surface/src/textual/parser.rs:430-455`;
   [C4 strict and recovering modes](../../../../docs/references/compiler.md#strict-and-recovering-modes).
2. Where does `define` become `def`, and which layer owns that decision?
   → `lang/surface/src/textual/lexer.rs:44-47`; test at `lang/surface/src/textual/parser/tests.rs:424-441`;
   [C4 terminal metadata](../../../../docs/references/compiler.md#trust-boundary-and-recovery-contracts).
3. What is the difference between a completion hole and a recovery node, and when is a hole visible to tooling?
   → `lang/surface/src/textual/parser.rs:124-151`, `:492-533`;
   [C4 recovering parsing](../../../../docs/references/compiler.md#recovering-parsing).
4. Which information lives outside the textual syntax tree, and who may ignore it?
   → `lang/surface/src/textual/intention.rs:27-47`, `:125-138`; `lang/surface/src/textual/trivia.rs:13-25`;
   [C15 retained source information](../../../../docs/references/compiler.md#retained-source-information).
5. Why does the formatter need the raw source string as well as the arena?
   → `PrettyFormatter::with_source` and `format_verbatim` in `lang/surface/src/textual/pretty.rs:378-404`,
   `:1772-1797`; [C15 formatter directives](../../../../docs/references/compiler.md#formatter-directives).
6. What does the desugarer memoize, and why does a rejected source produce no bitter program?
   → `lang/surface/src/bitter/desugar/mod.rs:33-36`, `:62-72`;
   [C4 desugaring folders](../../../../docs/references/compiler.md#desugaring-folders).
7. When does resolution publish a `ScopedProgram`, and what remains observable after rejection?
   → `lang/surface/src/scoped/resolver.rs:24-30`, `:99-126`;
   [C4 name resolution](../../../../docs/references/compiler.md#name-resolution).
8. Which module makes forward references inside `begin ... end` resolve before their definitions are visited?
   → `lang/surface/src/scoped/blocks.rs:69-120`, `lang/surface/src/scoped/dependencies.rs:6-35`;
   [C4 name resolution](../../../../docs/references/compiler.md#name-resolution).
9. Which spellings does the formatter guarantee to converge, and what is the only frontend option?
   → `lang/surface/src/textual/pretty/config.rs:103-121`;
   [C15 components and policy](../../../../docs/references/compiler.md#components-and-policy); the sweep in
   §6 shows current convergence for `define` and `comatch`.

## 9. Redundancy and Cleanup Signals

Statuses follow the card template: `open` = removable without an owner design decision; `decision needed` =
owner approval or a design choice gates it (the appendix calls the same gate `open — reference gate`);
`resolved` = removed or settled. Appendix IDs refer to the
[candidate inventory](../appendix-cleanup-candidates.md); the
[cleanup lens](../04-cleanup-lens.md#verification-protocol-before-proposing-a-removal) defines the
verification protocol. Both evaluations call their findings proposals, not decisions.

| # | Signal | Evidence | Status | Assessment |
| --- | --- | --- | --- | --- |
| 1 | `define` alias of `def` | `lang/surface/src/textual/lexer.rs:44-47`; [L2](../../../../docs/references/language.md#2-lexical-structure-and-syntax); [appendix F](../appendix-cleanup-candidates.md#f-surface-form-counts-at-e2dd8979) | decision needed | 0 uses in 629 tracked sources at `e2dd8979`; a documented synonym, so deletion updates L2 and the parser expectation list. Appendix F `open — reference gate`. |
| 2 | `comatch p => M end` abbreviation of `fn` | `lang/surface/src/textual/parser/grammar.lalrpop:447`; the parsed `Abs` prints as `fn` (`lang/surface/src/textual/pretty.rs:188-200`) | decision needed | 0 uses and undocumented; the grammar is C4's syntax specification, so the gate is narrow. The arm form `comatch \| ... end` is separate, documented in [L6](../../../../docs/references/language.md#6-computations-and-control), and stays. |
| 3 | `sigma` binder | `lang/surface/src/textual/parser/grammar.lalrpop:527`; [L4](../../../../docs/references/language.md#4-classification-and-inference) | decision needed | 4 uses, all compiler cases; equals `exists` for type binders and `*` for value binders. Documented, so removal changes L4. |
| 4 | `<\|` backward application | `lang/surface/src/textual/parser/grammar.lalrpop:512`; [L2](../../../../docs/references/language.md#2-lexical-structure-and-syntax); [style guide](../../../../docs/style.md) | decision needed | 7 uses in 6 files, but `docs/style.md:26-27` endorses it for multiline abstractions; it costs a precedence level and a formatter case. |
| 5 | `~>` view patterns | `lang/surface/src/textual/parser/grammar.lalrpop:268`; [L8](../../../../docs/references/language.md#8-value-functions-and-views); [audit §surface forms](../../../../docs/evaluations/2026-10-21-cleanup-audit/README.md#surface-forms) | decision needed | 16 uses in 6 files plus about 130 arms/references in 9 crates (audit estimate); `match f v` expresses the same matching. Documented in L8. |
| 6 | `param ... that` hoisting | [L3](../../../../docs/references/language.md#3-bindings-and-scope); `lang/surface/src/textual/parser/grammar.lalrpop:359-362`, `:546-551` | decision needed | 45 uses in 30 files (std 15); the hoisting form overlaps `param ... in` plus block placement. Distinct from `begin ... end` with `that`, which the audit retains as the only recursive-sealed-type form. |
| 7 | `.zyi` companions | `lang/session/src/source/graph.rs:97-108`; [L12](../../../../docs/references/language.md#source-boundaries); [appendix F](../appendix-cleanup-candidates.md#f-surface-form-counts-at-e2dd8979) | decision needed | 0 tracked `.zyi` files; the machinery is live (checked via §6) and also motivates the separate `.zydeco` extension. Session-owned, so the [tooling card](06-tooling.md) must carry the decision. |
| 8 | `@[doc]`, `@(literal)`, and `--\|` text blocks | [L2](../../../../docs/references/language.md#2-lexical-structure-and-syntax), [source documentation](../../../../docs/references/language.md#source-documentation); `lang/surface/src/textual/source/scan.rs:200-246` | decision needed | 0 `@[doc]` uses, 1 `@(literal)` splice, 53 text blocks; no documentation tooling consumes the retained text ([C15](../../../../docs/references/compiler.md#source-documentation-annotations)), and `check` warns per unattached block (10 on OOPSLA `core.zydeco`). |
| 9 | Duplicate builtin/intrinsic annotation validation | scanner: `lang/surface/src/textual/source/analyzers.rs:147-215`, `lang/surface/src/textual/source.rs:380-470`; desugarer: `lang/surface/src/bitter/desugar/meta.rs:60-80`, `:108-121`; [audit §parallel implementations](../../../../docs/evaluations/2026-10-21-cleanup-audit/README.md#parallel-implementations) | open | About 420 audit lines; the scanner and the desugarer decode the same catalog entries into separate error types (`BuiltinDirectiveError`/`IntrinsicDirectiveError` versus `DesugarError`). Folding into one typed decode is behavior-preserving for source (appendix B3). |
| 10 | Metadata catalog decoding sites | `lang/surface/src/metadata.rs:38-68`, `:149-161`; directive types `lang/surface/src/textual/source.rs:113-206`; prelude decodes at `lang/surface/src/bitter/desugar/meta.rs:32-44`, `lang/surface/src/scoped/blocks.rs:141`, `lang/surface/src/scoped/resolver.rs:213`, `lang/session/src/source/program.rs:154`; [size §structural simplifications](../../../../docs/evaluations/2026-10-25-size-analysis/README.md#structural-simplifications) | open | The size analysis notes `intrinsic` alone has six types across catalog/directive/analyzer and `prelude` is decoded at four sites (≈0.9k–1.2k). Decoding once into a typed annotation node during desugaring is a structural simplification, not a source change. |
| 11 | Three front-end syntax families | [C4](../../../../docs/references/compiler.md#c4-parsing-desugaring-and-name-resolution); [size §representation multiplication](../../../../docs/evaluations/2026-10-25-size-analysis/README.md#representation-multiplication); shared mitigations `lang/surface/src/fold.rs:1-27`, `lang/surface/src/debug.rs:1-30` | decision needed | ≈2k for merging desugar and resolution into one representation. The shared `fold.rs` family and the shared debug formatter already remove the worst duplication; a merge rewrites C4's phase contracts, so it is design work. |
| 12 | Formatter source-fidelity cost | [size §formatter fidelity](../../../../docs/evaluations/2026-10-25-size-analysis/README.md#formatter-fidelity); `lang/surface/src/textual/pretty.rs:2599` (inline tests start); `lang/surface/src/textual/pretty/corpus.rs:61-88` | decision needed | 4,587 production lines with fidelity support at the measured revision; `pretty.rs` is 5,489 lines now (2,598 before inline tests). The size analysis says a canonical printer without retention would be "a fraction"; C15's retention laws would change. |
| 13 | Reserved formatter options | `lang/surface/src/metadata.rs:954-965`; `lang/surface/src/textual/pretty/config.rs:103-153`; [C15](../../../../docs/references/compiler.md#formatter-directives) | resolved | `width`, `indent`, `layout`, and `parentheses` stay as reserved knobs; the audit's removal was declined ([update paragraph](../../../../docs/evaluations/2026-10-21-cleanup-audit/README.md#post-audit-verification)). Only `verbatim` appears in tracked sources (`lib/tests/core/gc-stress.zy:29`). |
| 14 | Test-only parser entry points and the textual ugly printer | commit `0f0f311f`; [audit update](../../../../docs/evaluations/2026-10-21-cleanup-audit/README.md#post-audit-verification); `lang/surface/src/textual/parser.rs:430`, `:463` | resolved | Removed; `textual/ugly.rs` is gone and the only public parse entries are strict and recovering. |
| 15 | Parser diagnostics spell canonical `def` | `lang/surface/src/textual/lexer.rs:44-47`; test `lang/surface/src/textual/parser/tests.rs:424-441`; commit `b652ff4e`; [C4](../../../../docs/references/compiler.md#trust-boundary-and-recovery-contracts) | resolved | `define` still parses, but expectation lists and messages print `def`; the test asserts the message contains `def` and not `define`. |

## 10. Uncertainties and Drift

- **Dated figures.** The audit measured `7e5ee4c1` and the size analysis `6aae43d0`; the manual and this card
  are at `e2dd8979` with a dirty working tree. I re-ran `docs/evaluations/2026-10-21-cleanup-audit/usage.py`
  on a clean `git archive HEAD` extraction (629 sources: 601 `.zy`, 0 `.zyi`, 28 `.zydeco`) and it matches the
  [appendix F](../appendix-cleanup-candidates.md#f-surface-form-counts-at-e2dd8979) surface-form rows cited
  here. Line-count splits (production versus test, `≈` deltas) were not re-derived.
- **Live formatter identity defect.** Reproduced at `e2dd8979`: formatting
  `@[partial] fn (0 : Int) => fn (1 : Int) => ret 0` prints
  `@[partial] fn (0 : Int) (1 : Int) => ret 0`; the original rejects with `tyck.refutable-binding` and the
  formatted text is accepted, because both binders enter the annotated header. This is tracked, unchecked, in
  [compiler boundaries](../../../../docs/todos/compiler-boundaries.md#partial-annotation-scope-during-formatting)
  and violates C15's semantic-identity law; a formatter round-trip regression is still missing.
- **Nested checkouts poison repository-wide searches.** Untracked copies exist at
  `editor/zed/grammars/zydeco` (an old checkout at `6f4f7ab5`, excluded by `editor/zed/.gitignore`) and
  `.claude/worktrees/` (excluded by `.git/info/exclude`). They are not part of the snapshot; restrict
  `grep`/`wc` to tracked paths or to the crate directory as done here.
- **Editor path not exercised.** Recovery and completion have no CLI surface; I verified one focused recovery
  regression and read the session completion entry points, but did not run Cajun or an editor loop.
- **Assembly drops presentation data — inferred, not fully traced.** I confirmed that
  `TextualProgramBuilder` builds a fresh `Parser` and never calls `capture_source_information`; I did not
  audit every session/Cajun consumer to prove that none expects merged trivia.
- **Companion behavior scope.** The `.zyi` probe used a direct CLI path; package-selected or imported
  companion resolution is session-owned and belongs to
  [C3](../../../../docs/references/compiler.md#source-loading-and-package-selection) and the
  [tooling card](06-tooling.md).
- **Corpus size drift.** `pretty.rs` is 5,489 lines now against 5,273 in the size analysis; I did not bisect
  the difference or re-classify production versus test lines with the evaluation's rule.
- **Docs versus formatter.** The README hello-world snippet is not canonical formatter output
  (`fmt --check` would change it); whether documentation samples must be formatted is a docs decision, not a
  front-end contract, and I did not measure it repository-wide.
