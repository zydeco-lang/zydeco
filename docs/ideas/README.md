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

### Further storage boundaries

Checked record composition requires aligned, nonoverlapping fields; packed records, unions, runtime field placement,
and target-specific pointer facts currently need custom operations or raw access.
Each would need its own factory, state protocol, and target support: byte packing keeps field offsets
and accesses with alignment one; overlapping storage needs a statically known or tagged active member;
runtime placement must validate extent, offset, alignment, and nonoverlap before exposing a dynamic field;
stack, static, and arena storage need placement, lifetime, and release conventions;
stored managed references need rooting and scanning rules; pointer comparison and conversion need width
and provenance rules; and atomics and volatile access need dedicated primitive semantics.
Two smaller library additions are typed unchecked indexing that states the obligations it transfers
and a no-read discard transition for initialized storage.
None of these introduces source lifetimes without a separate decision.
[L13](../references/language.md#manual-memory) owns the implemented layouts, fields, arrays, and views;
[memory compilation](../proposals/memory-compilation.md) owns the compiler gaps.

### Functional byte updates with allocation reuse

A functional update of immutable bytes must preserve every retained input and slice.
A copying implementation can allocate a private destination, apply validated edits,
and publish one retained result through CPS; reusing the input allocation additionally needs the
[compiler reuse obligations](../proposals/escape-unboxing.md#functional-allocation-reuse-proposed): exclusive access,
compatible layout, valid roots, and a control-flow account that the manual `Ptr` state protocol does not provide.
Start any reuse experiment with a complete, exclusive allocation and a same-length update,
and keep a copying fallback for shared inputs, slices, and escaped raw pointers.
Fallible codecs that cannot preflight completely need staging,
and non-contiguous storage needs a workload that justifies it.
Destination-passing style, OCaml's tail-modulo-constructor transformation,
and MLIR bufferization are the precedents for separating destination placement from ownership evidence.
[Retained bytes](../references/language.md#immutable-owners-and-source-bytes) own the current observations.

### Growable byte builders and incremental streams

Repeated immutable append is quadratic. A memory-backed writer could reuse the capability model
for amortized pushes and publish one immutable `Bytes` result through CPS destination construction,
with growth reserving capacity before any observable change and failure preserving committed chunks.
A chunk-processing driver could pass each read chunk to its consumer and request the next read only on completion,
distinguishing EOF, read failure, consumer failure, and explicit stop.
Buffered and seekable streams, asynchronous protocols, and native paths are further extensions,
each with its own position, cancellation, or aliasing contract.
[L13](../references/language.md#streams-and-process-arguments) owns the current stream observations;
the [fixed-capacity buffer](../references/language.md#byte-builders) is the implemented core.

### Further effect libraries

Ret, Reader, Writer, Option, State, Exception, and ordered depth-first NonDet are implemented
as relative monads with a shared transformer derivation.
Further families would each need an implementation decision: delimited control,
fair search and pruning, parsers, free monads, validation accumulators, randomness,
reader–writer–state bundles, resource management, and asynchronous tasks.
The admission constraint is that a carrier must compose with ambient computations
without changing their observable order; eagerly collecting nondeterministic results
into a list is not an unrestricted relative monad.
[Elementary effects](../references/language.md#elementary-effect-instances)
and [derived transformers](../references/language.md#derived-relative-monad-transformers) own the implemented library.

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
[compiler memory retention](#compiler-memory-retention) owns broader session memory questions.

### Macro source provenance

If macro provenance becomes a concrete requirement, review whether a span interner should add hygiene context
around the compact address-space model.
This conditional question does not select a macro expansion or hygiene design.
[C2](../references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance) owns current source coordinates
and provenance.

### Compiler memory retention

A checked root elaborates into many more nodes than its source, so retaining every typed arena
in a long-lived session makes memory depend on root history.
Extending reclamation to interactive sessions needs an account of active projects, overlays,
and outstanding snapshots, and must keep keyed facts after arena eviction paired with the right revision.
Measurement targets, none yet measured: judgment-producer query bookkeeping during cold and warm checks,
inference-region fill scans as local binders grow, repeated root switching against full-arena residency,
and editor lookups that still route through the session.
The producer-query migration measured on 2026-08-14 reported about 30% end-to-end checking overhead.
Open questions are root-aware reclamation versus bounded generations, sharing retained classifier annotations,
sharing location structure across parsed-source lifetimes, and consumer-driven normalization of closed types.
[Analysis facts and materialization](../references/compiler.md#analysis-facts-and-materialization)
owns the current split.

### Stronger typed-arena verification

Parent/child checks, product reconstruction, and definition-reference scope need use-site instantiations
or equivalent derivation witnesses, whose retention cost must be measured against the retained-fact boundary.
Checking a shared node under every scope needs a scope-sensitive traversal,
and a finer skolem policy must distinguish deliberate export from accidental escape.
Remaining checks to establish before asserting them: that every `Type::Label` payload has kind `VType`,
the kinded shapes of `ManifestKind` and `SCons`, whether a finished arena may retain `Type::Var`,
declaration telescopes for orphan `Data` and `CoData` arms, and how the lint should treat deliberate source holes.
[The typed-arena lint](../references/compiler.md#typed-arena-lint) owns the current checks and limits.

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

### Structural completion and last-successful state

Visible definitions, metadata, and source paths are implemented.
Contextual forms, constructors, destructors, and named fields would come from the parser expectation,
the expected or scrutinee type, and product structure respectively, as type-checker queries rather than name lists;
each snippet needs a representative parsing test, and raw LALRPOP diagnostic strings must never reach Cajun.
If last-successful analysis is retained after a failed revision, keep it separate from current-revision state:
source edits and semantic ranges need current facts, and stale results must not claim exact scope.
[Recovering parsing](../references/compiler.md#recovering-parsing)
and [completion](../references/compiler.md#completion) own the implemented behavior.

### Project context inspection

Make the active project context inspectable: selected roots, entry declaration locations,
discovery origins, namespace substitutions, import routes, and resulting bindings.
Define refresh responsibilities for directory membership, disk changes, overlays, and root changes,
a user-facing syntax for project registration, and the scope of relationship validation,
including which tests a relationship-driven suite selection runs.
[Source packages](../references/language.md#source-packages) own the implemented resolution model.
