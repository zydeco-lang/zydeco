# Reference drift

These are follow-ups from reference drafting on 2026-09-08: the language account was inspected against `289f2a14`,
the compiler account against `fdc4eeee`, and the broader consolidation against `14e0410a`.
Keep the evidence and proposed repair together here; the reference chapters should contain the resulting account,
not an editorial history.
Checked entries record completed documentation repairs; unchecked entries remain follow-ups.

## Language accounts

- [ ] **Product shape.** The [tutorial](../tutorial/zydeco-guide.md)
  and [formal calculus](../../lang/statics/type-system.typ) describe binary right-associated product typing.
  The current [typed syntax](../../lang/statics/src/syntax.rs)
  and [product checker](../../lang/statics/src/check/pattern/product.rs) preserve n-ary arity and explicit nesting.
  Revise the active accounts together, using `A * B * C` versus `A * (B * C)` as the distinguishing example.
  Keep the [older product design](../legacy/ideas/products.md) historical.

- [ ] **Recursive-type admissibility.** The earlier term proposal required a chosen guardedness
  or positivity discipline.
  Its [narrowed design](../proposals/term.md#dependency-directed-elaboration) now keeps
  that requirement as an open question.
  The [binding checker](../../lang/statics/src/check/binding.rs) checks sealed groups and available kinds;
  the formal calculus explicitly records the absence of a positivity judgment.
  Decide whether to specify that current boundary or add a stronger restriction.
  A new restriction needs accepted and rejected recursive examples before the reference can state it.

- [x] **Provider-directed inference.** Retired `normalization.md` described omitted parameter annotations
  receiving a compiled provider signature and assumed a global checking context.
  The [source checker](../../lang/statics/src/check/source.rs) closes each source independently.
  [L12](../references/language.md#12-sources-imports-and-entry)
  and [C5](../references/compiler.md#inference-and-reuse) now distinguish local expected-type propagation
  from import-site reconciliation.
  The obsolete importer-driven account was removed; rejection of underconstrained imported sources remains part
  of the boundary.

- [x] **Numeric package rationale.** Retired `numeric-capabilities.md` justified terminal `Unit`
  by associative product flattening and described a more restrictive package occurrence checker.
  The current [Numeric type](../../lib/std/numeric/numeric.type.zy) already has no terminal `Unit`,
  and [static elaboration](../references/compiler.md#static-elimination) replaced the occurrence validators.
  The [library guide](../../lib/std/README.md#numeric-capabilities-and-explicit-instances)
  now uses the current interfaces and checked examples.
  The obsolete claims were not transferred.

- [ ] **Tutorial examples and notation.** The [guide](../tutorial/zydeco-guide.md) includes `exists (X = def as X : K)`
  and an older monadic-basis product opening.
  Compare them with the [grammar](../../lang/surface/src/textual/parser/grammar.lalrpop),
  [monadic basis](../../lib/std/control/monad.zy), and current examples.
  Correct obsolete forms and make complete guide examples opt into documentation checking.
  Rejoin the Builtin table's `surface` row, which is split across two Markdown lines.

- [x] **FFI example grouping.** The earlier FFI proposal's unparenthesized metadata-hole annotation failed parsing
  at `:` as a complete term.
  The retired import account now points to the grouped, checked example
  in [L14](../references/language.md#14-foreign-interfaces) and the [working binding](../../lib/ffi/xxhash.zy);
  the remaining proposal concerns extensions.

## Superseded implementation accounts

These entries track superseded implementation descriptions and their replacement homes.
Unchecked entries still need consolidation.

- [x] **Alias lowering.** Retired `aliasing.md` described normalization expanding ordinary alias assignments.
  The current [SPS normalizer](../../lang/stackir/src/high/normalize.rs)
  and [closure conversion](../../lang/stackir/src/low/convert.rs) retain structural aliases.
  [Assembly lowering](../../lang/assembly/src/lower.rs)
  and the [direct SPS WebAssembly emitter](../../lang/wasm-sps/src/emit.rs) preserve the bindee in a temporary or local.
  The [transferred pattern account](../references/compiler.md#pattern-decisions-and-validation) describes
  these current boundaries.

- [x] **Coverage product shape.** The earlier coverage proposal described converting typed products
  to a right-associated binary spine.
  The [matrix conversion](../../lang/statics/src/validate/coverage.rs) retains each typed component vector
  and uses its length as the head arity.
  The [compiler coverage account](../references/compiler.md#coverage) preserves explicit nesting;
  the tutorial and formal-calculus follow-up above remains open.

- [x] **Primitive package layout.** Retired `primitive-packages.md` marked its five-group layout as superseded
  but retained `core`, `representations`, carrier-bearing numeric children, and an obsolete `Unit`-tail rationale.
  Its identity rationale now lives
  in [package modularization](../proposals/package-modularization.md#primitive-identity-and-package-boundaries);
  the [Builtin guide](../../lib/std/README.md#builtin-packages) describes the current contract.
  Incoming links were updated when the proposal was removed.

- [x] **Remaining package-design summaries.** Package modularization retained older core/representation groups,
  mismatched selection prose, and a claim that bare records cannot synthesize a principal type.
  The narrowed [package design](../proposals/package-modularization.md) now links to current rules and inventories;
  [the library recipes](../../lib/std/README.md#package-composition) use one Builtin opening and checked examples.
  Named products synthesize types; kind-witness introduction still needs its explicit annotation.

- [x] **Query ownership.** Retired `query-owned-statics.md` retained an all-pure-query target
  and migration instructions alongside its achieved checker-owned core.
  [C3](../references/compiler.md#query-and-checker-ownership) now describes producer queries and stateful inference
  from the [checker](../../lang/statics/src/check) and [query inputs](../../lang/statics/src/query/input.rs).
  The obsolete migration instructions were removed; historical overhead is qualified in the memory design.

- [x] **Arena retention.** The earlier memory proposal named `strip_occurrence_payload`
  and claimed that long-running sessions no longer grow per root.
  Current [analysis](../../lang/session/src/source/query.rs) uses `clone_keyed_indexes`,
  while fine-grained memos can outlive a full arena.
  [C3](../references/compiler.md#analysis-facts-and-materialization) states the current API
  and revision pairing requirement.
  The [memory design](../proposals/arena-gc.md) preserves the measurements as historical evidence
  and keeps root-aware memo reclamation open; it makes no general bounded-memory claim.

- [x] **Normalization architecture.** Retired `normalization.md` described eager finalization
  as a temporary step toward fully query-driven semantics.
  [C5](../references/compiler.md#finalization) describes the implemented shared pass without promising that migration.
  Consumer-driven normalization remains an evaluation question
  in the [memory design](../proposals/arena-gc.md#open-questions).

- [x] **Wasm validation claims.** The [Wasm design](../proposals/wasm-backends.md) now labels the 126-case counts,
  size comparison, and hashes as historical evidence.
  [C16](../references/compiler.md#source-fixtures-and-runtime-oracles) owns the current harness's stdin,
  captured-output, and exit oracles.
  The revised selection criteria distinguish that capability from actual case coverage;
  the multi-argument process-fold embedding gap remains open.

- [x] **Escape-analysis environment premise.** The [escape design](../proposals/escape-unboxing.md) now
  uses the retained native activation model and links delivered local unboxing to C10.
  Stack cells and interprocedural constraints remain proposed,
  and a retained frame alone still cannot justify an escaping raw pointer.

- [ ] **Lint and deliberate holes.** The earlier lint account conflated a successful source check
  with complete runtime terms.
  [Ordinary checking](../references/language.md#10-static-elimination) supports hole inspection,
  while [lint](../../lang/statics/src/validate/lint.rs) rejects non-foreign term placeholders.
  C6 now states the stricter gate.
  Decide whether the verifier should accept explicitly incomplete artifacts using a separate typed mode;
  preserve detection of unresolved type fills and executable holes.

- [x] **Source-map layout.** The retired exploration mixed an old 32-byte span with the implemented eight-byte model.
  [C2](../references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance) now owns compact positions,
  merged `SpanArena` ownership, lazy locations, and local/global coordinate conversion.
  Remaining lookup and storage choices live in [todos](deferred-designs.md#source-location-storage-and-lookup).

- [x] **REPL wrapper behavior.** The retired proposal described one wrapper and no fallback.
  The [engine](../../tui/src/engine.rs) analyzes a direct observation, then a returned-value wrapper on rejection,
  reporting the original direct diagnostic if both reject.
  [C15](../references/compiler.md#interactive-engine) records that boundary and numbered-source identity.

- [x] **Byte construction costs.** The earlier byte proposal called `from_list` linear
  and grouped `concat` with a single append.
  [The library implementation](../../lib/std/text/package.zy) folds repeated append, copying growing tails.
  The [cost table](../../lib/std/README.md#byte-operation-costs) now records quadratic `from_list`
  and length-sum copying for `concat`, as well as native copying slices.

- [x] **Formatter corpus scope.** The earlier design excluded CLI fixtures from its corpus account.
  The [current fixture architecture](../references/compiler.md#source-fixtures-and-runtime-oracles) is the owner;
  the narrowed formatting design retains layout laws and extension criteria.
  The still-used punning audit helper remains a [separate todo](deferred-designs.md#formatter-migration-helper).

- [x] **Filesystem conversion protocols.** The earlier filesystem proposal omitted `Ret` from the path helpers.
  [The implementation](../../lib/std/system/package.zy) exports thunks
  for `String -> Ret Path` and `Path -> Ret String`.
  [The stream guide](../../lib/std/README.md#streams-and-files) now states these protocols
  and makes the omitted outer thunk explicit.

- [ ] **Template-local load diagnostics.** The retired source-map exploration records import-resolution
  and cycle errors still rendering local byte offsets.
  Audit these early failures against the template's `FileMap`, separately from merged checker diagnostics,
  before promising consistent line/column rendering.
  Include an imported file with non-ASCII text in the diagnostic checks.

## Compiler boundary probes

- [ ] **Nested package witness diagnostic.** Computation binders can collect witnesses beneath product patterns,
  but [application instantiation](../../lang/statics/src/check/functions/application.rs)
  traverses a leading existential prefix.
  This complete term rejects at the call with `tyck.package-witness-arity-mismatch`,
  reporting “expected 1 witness(es), found 1”:

  ```zydeco
  let VType = @(intrinsic(vtype)) in
  let Unit = @(intrinsic(unit)) in
  let reveal = { fn ((_, (T, x)) : Unit * (exists (T : VType) . T)) => ret x } in
  ! reveal ((), (Unit, ()))
  ```

Removing the outer product from both binder and argument passes: use `(T, x) : exists (T : VType) . T`
and `! reveal (Unit, ())` in the same context.
Give unsupported witness routes a diagnostic explaining their shape,
or review generalized computation instantiation as a separate extension.
Preserve this pair when implementing either change.

## Drafting corrections

- [x] **Algebra classification.** The original language outline grouped `Monad` and `Algebra` as codata.
  [Monad](../../lib/std/control/monad.type.zy) is codata;
  [Algebra](../../lib/std/control/algebra.type.zy) is a universally quantified computation type.
  The language draft now uses those classifications.

Broader extraction and incoming-link work stays in the [reference plan](reference-plan.md).
