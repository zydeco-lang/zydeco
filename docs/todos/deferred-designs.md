# Deferred design work

These questions survived documentation consolidation on 2026-09-08 against `14e0410a`.
They are follow-ups, not additions to the current language or implementation contract.

## Value views

- [ ] **Refutable-view coverage.** The checker currently treats a view with a refutable nested pattern
  as opaque for exhaustiveness, even though it may occur in matches and partial computation bindings.
  Investigate grouping arms that apply the same resolved function with the same static arguments,
  then checking their result patterns together.
  Exhaustiveness over the whole codomain is sufficient; exhaustiveness only over the image needs additional evidence.
  Define equivalence and conservative fallback before accepting new programs.
  Pair collectively exhaustive result arms with a missing-result case and with distinct views that must not be grouped.
- [ ] **Shared view evaluation.** Decide whether such grouped arms should evaluate the transformation once.
  Preserve arm order, bindings, and runtime value sharing; purity alone does not define an equivalence key
  or cost policy.
- [ ] **Broader view heads.** Consider arbitrary value terms instead of the current variable
  with optional type arguments.
  Review parsing, lexical scope, diagnostics, and the static-elimination boundary together.

Current rules remain in [L7–L8](../references/language.md#8-value-functions-and-views);
[coverage conversion](../../lang/statics/src/validate/coverage.rs) maps refutable view results to `Opaque`.

## Residual code sharing

- [ ] Measure code growth from repeated static value-function applications before adding residual-code factoring.
  Compare direct block sharing with inlining while preserving captured runtime values and effect multiplicity.
  This is an optimization of the residual program, not a new runtime representation for `val pi`.
  [C6](../references/compiler.md#static-elimination) owns elimination;
  [C8](../references/compiler.md#sharing-and-discardability) owns residual sharing constraints.

## REPL history and replay

- [ ] Define persistence and replay around immutable numbered sources, including working-directory-relative imports.
  An edited historical entry should receive a new identity; replay must state which effects it executes again.
- [ ] Prune retained history only with a rule that preserves every still-visible numbered import.
  Transcript clearing alone is not evidence that a source is unreachable.
- [ ] Revisit argument-bearing or stateful commands when a concrete interaction needs them.
  [C15](../references/compiler.md#interactive-engine) owns the current submission and retry model.

## Source-location storage and lookup

- [ ] Profile the linear `SpanArena` cursor/range lookup before introducing an interval index.
  Keep file-local editor coordinates separate from merged global spans.
- [ ] Measure whether cached multibyte column information improves location rendering enough to justify its memory cost.
- [ ] Compare embedding compact spans in later arenas with following provenance back to the surface map.
  Preserve rejected-program diagnostics and revision ownership in either design.
  [C2](../references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance) owns the implemented model;
  [arena retention](../proposals/arena-gc.md) owns broader session memory questions.

If macro provenance becomes a concrete requirement, review whether a span interner should add hygiene context
around the compact address-space model.
No macro expansion or hygiene design is selected by this storage cleanup.

## Classifier and pattern affordances

- [ ] Decide whether editors should offer an action constructing `@[typeof]` expressions.
  This convenience would not identify the source construct with the REPL's display-only `@[type]` command.
- [ ] Review type and kind pattern aliases separately if pursued: their classification and scope remain unspecified.
  Value-pattern usefulness work does not implicitly extend the admitted pattern language.

## Formatter migration helper

- [ ] Review whether `NamedTermPunningAudit` still serves the formatter corpus tests
  after the library's punning migration.
  The helper remains exported and used in [the formatter](../../lang/surface/src/textual/pretty.rs);
  retiring the migration prose does not remove that code or establish that the test no longer needs it.
