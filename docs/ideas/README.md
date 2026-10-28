# Design ideas

These exploratory questions and research directions do not extend the current language or implementation contract.
Each idea names its implemented starting point in the [references](../references/README.md),
which own approved, implemented contracts.
Develop an idea into a [proposal](../proposals/README.md) when it has a concrete design,
alternatives, and validation criteria.
Keep observed implementation discrepancies in [todos](../todos/README.md).
Dated experimental reports and their evidence live in [evaluations](../evaluations/README.md).

The one developed idea has its own document:
[CBPV as a universal FFI](cbpv-universal-ffi.md) asks whether typed CBPV adapters can compose foreign data
and control protocols in either direction.
The remaining questions are short enough to live here.

## Language

### Refutable view coverage

The checker currently treats a view with a refutable nested pattern as opaque for exhaustiveness,
even though it may occur in matches and partial computation bindings.
Investigate grouping arms that apply the same resolved function with the same static arguments,
then checking their result patterns together.
Exhaustiveness over the whole codomain is sufficient; exhaustiveness only over the image needs additional evidence.
Define equivalence and conservative fallback before accepting new programs.
Pair collectively exhaustive result arms with a missing-result case and with distinct views that must not be grouped.
Current rules remain in [L7–L8](../references/language.md#8-value-functions-and-views);
[coverage conversion](../../lang/statics/src/validate/coverage.rs) maps refutable view results to `Opaque`.

### Shared view evaluation

Grouping view arms also raises an evaluation question: should grouped arms evaluate the transformation once?
Preserve arm order, bindings, and runtime value sharing; purity alone does not define an equivalence key or cost policy.
[L8](../references/language.md#8-value-functions-and-views) owns current value-function and view rules.

### Broader view heads

Consider arbitrary value terms as view heads instead of the current variable with optional type arguments.
Review parsing, lexical scope, diagnostics, and the static-elimination boundary together.
[L8](../references/language.md#8-value-functions-and-views) owns current value-function and view rules.

### Classifier pattern aliases

Review type and kind pattern aliases separately if pursued: their classification and scope remain unspecified.
Value-pattern usefulness work does not implicitly extend the admitted pattern language.
[L7](../references/language.md#7-patterns-and-coverage) owns current pattern aliases and their rejection boundary.

### Kind-witness introduction

Design introduction of kind witnesses separately from the implemented type-witness `pack` form.
Pair accepted packed values with ill-scoped or mismatched evidence.
[L9](../references/language.md#module-interfaces-and-shared-openings) owns current module composition.

### Computation witness routes

Extend computation application beyond its leading existential prefix only
with an explicit witness route through preceding products.
Preserve identity and non-escape checks;
value-function projection evidence alone does not implement this computation boundary.
[L9](../references/language.md#module-interfaces-and-shared-openings) owns current module composition.
The [compiler boundary issue](../todos/compiler-boundaries.md#nested-witness-diagnostic) tracks the current
diagnostic limitation; improving that rejection does not implement broader routes.

### Companion interface generation

Decide whether companion interfaces need generation or synchronization support.
Preserve the distinction between an inferred `typeof` contract and an explicitly authored abstraction boundary.
[L9](../references/language.md#module-interfaces-and-shared-openings) owns current module composition.

### Recursive admissibility

Specify stronger guardedness or positivity only after identifying the intended accepted recursive values and types,
their rejected cycles, and the property being established: normalization, representation, or logical consistency.
Dependency scheduling is not that proof.
[L3](../references/language.md#3-bindings-and-scope) owns current recursive component acceptance.

## Compiler

### Residual code sharing

Measure code growth from repeated static value-function applications before adding residual-code factoring.
Compare direct block sharing with inlining while preserving captured runtime values and effect multiplicity.
This is an optimization of the residual program, not a new runtime representation for `val pi`.
[C6](../references/compiler.md#static-elimination) owns elimination;
[C8](../references/compiler.md#sharing-and-discardability) owns residual sharing constraints.

### Demand-directed construction

Measure temporary construction of unused SPS nodes before changing demand-directed construction.
Preserve diagnostic and sharing behavior; allocation counts alone do not establish an equivalent replacement.
[C8](../references/compiler.md#consumer-demands) owns the current demand schedule.

### Span lookup indexing

Profile the linear `SpanArena` cursor/range lookup before introducing an interval index.
Keep file-local editor coordinates separate from merged global spans.
[C2](../references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance) owns current source coordinates
and provenance.

### Source-location column caching

Measure whether cached multibyte column information improves location rendering enough to justify its memory cost.
[C2](../references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance) owns current source coordinates
and provenance.

### Source locations in later phases

Compare embedding compact spans in later arenas with following provenance back to the surface map.
Preserve rejected-program diagnostics and revision ownership in either design.
[C2](../references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance) owns the implemented model;
[arena retention](../proposals/arena-gc.md) owns broader session memory questions.

### Macro source provenance

If macro provenance becomes a concrete requirement, review whether a span interner should add hygiene context
around the compact address-space model.
This conditional question does not select a macro expansion or hygiene design.
[C2](../references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance) owns current source coordinates
and provenance.

## Tooling

### REPL history and replay

Define persistence and replay around immutable numbered sources, including working-directory-relative imports.
An edited historical entry should receive a new identity; replay must state which effects it executes again.
[C15](../references/compiler.md#interactive-engine) owns the current submission and retry model.

### REPL history pruning

Prune retained history only with a rule that preserves every still-visible numbered import.
Transcript clearing alone is not evidence that a source is unreachable.
[History and replay](#repl-history-and-replay) is a separate persistence question.

### REPL command extensions

Revisit argument-bearing or stateful commands when a concrete interaction needs them.
[C15](../references/compiler.md#interactive-engine) owns the current submission and retry model.

### An editor action for typeof

Decide whether editors should offer an action constructing `@[typeof]` expressions.
This convenience would not identify the source construct with the REPL's display-only `@[type]` command.
[L4](../references/language.md#4-classification-and-inference) owns classifier extraction;
[C15](../references/compiler.md#interactive-engine) owns REPL inspection.

### Elaborated type layout

Break long quantifier telescopes, consider folding adjacent same-form quantifiers,
and extend source-shaped term labels beyond type operands while preserving grammar and classifier identity.
[C15](../references/compiler.md#elaborated-type-rendering) owns the separate renderers and their current layout support.

### Source generation from elaborated terms

Survey which diagnostics need elaborated identities and which interactions need source-shaped output.
Readable names such as `Int` do not reproduce an intrinsic source expression by themselves.
Reassess a typed-to-textual bridge or shared precedence vocabulary when a source-faithful consumer needs it,
such as inserting a reparseable elaborated annotation.
Specify fills, witness names, intrinsic spellings, and synthetic provenance before promising round trips.
A reifier would add another textual AST producer while diagnostic-only forms still need a typed renderer;
a shared precedence layer would add a third vocabulary alongside the two grammars.
Evaluate those costs against the consumer.
[C15](../references/compiler.md#elaborated-type-rendering) owns the separate renderers and their current layout support.

### Floating comment anchors

If new syntax admits floating comments, give them an explicit typed boundary and preserve one stored comment record.
The current formatter uses entity anchors, typed arm/delimiter boundaries, and exclusion ranges.
[C15](../references/compiler.md#formatting-and-typed-rendering) owns current textual formatting and comment retention.

### Source fixture directive extensions

The directive parser accepts `backends`, `input`, `output`, `args`, and `setup`
for the [standard-library directive runner](../../lang/tests/tests/directives.rs).
Those directives require `stage: run`; the inline case runner rejects them.
An inapplicable expectation must continue to fail the trial.
Expected exit codes and stable desugaring-rejection spellings remain deferred;
add them only when a concrete case needs those contracts.
The [fixture guide](../../lang/tests/cases/README.md) owns current directives;
[C16](../references/compiler.md#source-fixtures-and-runtime-oracles) owns discovery and runtime oracles.
