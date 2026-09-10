# Reference drift

These are follow-ups from reference drafting on 2026-09-08: the language account was inspected against `289f2a14`,
the compiler account against `fdc4eeee`, and the broader consolidation against `14e0410a`.
The follow-up audit on 2026-09-10 inspected `e7a5a1cf`, including the intervening storage,
static-calculation, argument-fold, FFI, and stack-protocol work.
Product authority and the tutorial were repaired in this pass; the other findings below remain open.
Keep the evidence and proposed repair together here; the reference chapters should contain the resulting account,
not an editorial history.
Checked entries record completed documentation repairs; unchecked entries remain follow-ups.

## Language accounts

- [x] **Product shape and proposal status.** The
  [tutorial](../tutorial/zydeco-guide.md#4-products-named-fields-and-packages)
  and [formal calculus](../../lang/statics/type-system.typ) had described binary right-associated product typing.
  The current [typed syntax](../../lang/statics/src/syntax.rs)
  and [product checker](../../lang/statics/src/check/pattern/product.rs) preserve n-ary arity and explicit nesting.
  The implementation remains canonical: both active accounts now distinguish `A * B * C` from `A * (B * C)`.
  The binary design was a proposal, not an implemented historical semantics; it has been moved
  from the legacy ideas directory into the [binary-product proposal](../proposals/binary-products.md),
  retaining its suffix-pattern and layout alternatives.
  The tutorial checks flat and nested introductions and rejects both a flat value at a nested type
  and a suffix pattern at a flat type.

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

- [x] **Tutorial examples and notation.** The [guide](../tutorial/zydeco-guide.md) now uses manifest `as` syntax,
  complete terms with guide-relative imports, and 21 accepted examples plus two specified rejections.
  Its Builtin table is repaired and includes the current Buffer capability.
  The review also corrected the executable entry boundary, import sugar, thunk-binding expansion,
  the ill-kinded `Ret Counter`, projection from returned records, and unnecessary rewrapping of dictionary thunks.
  Recursive codata uses a mobile sealed definition with its required explicit kind.
  State, Exception, and StateExn publish manifest carrier equations; describing all three as abstract was incorrect.
  The positional monadic-basis opening remains valid; the refreshed examples use projection groups
  to select their dependencies without enumerating the package layout.
  The guide now covers total value functions, value matches and integer calculations, current storage interfaces,
  and the fixed-width/void C import subset, with links to their rule owners.

- [ ] **Formal calculus beyond products.** The [calculus](../../lang/statics/type-system.typ) still
  gives `field` an immediate-component search and explicitly excludes deeper traversal;
  [field lookup](../../lang/statics/src/check/projection/field.rs) recursively searches named wrappers,
  products, and package telescopes.
  The function section also says witnesses beneath products or constructors are rejected at the binder,
  whereas the nested-package probe below reaches an application failure.
  Finally, the value grammar and rules omit value-producing matches
  and integer value operations already covered by [L8](../references/language.md#8-value-functions-and-views)
  and [static elimination](../references/compiler.md#static-elimination).
  Reconcile these accounts separately; the product repair does not establish whole-calculus conformance.

- [ ] **Literal-match result summary.** [L7](../references/language.md#7-patterns-and-coverage)
  still says integer-literal matching remains a computation.
  [L8](../references/language.md#8-value-functions-and-views)
  and the tutorial's checked `maximum` example demonstrate a value-producing integer match.
  Update the pattern summary to permit either result sort under the shared match and static-elimination rules.

- [ ] **Execution-profile storage wording.** [L15](../references/language.md#15-execution-profiles)
  still says there are no source layout annotations or manual allocation.
  The [current storage interfaces](../../lib/std/README.md#explicit-storage) expose Buffer allocation and `close`,
  allocator protocols, storage recipes, and static plans.
  Scope the limitation to the compiler-managed representation of ordinary values,
  and distinguish it from explicit source-managed storage.

- [ ] **Import-sugar explanation.** [DESIGN.md](../../DESIGN.md#source-terms-and-imports)
  explains the parenthesized metadata abbreviation by repeating `@(import("library.zy"))` on both sides.
  The corresponding tutorial explanation now expands it to `@[import("library.zy")] _`.
  Correct the remaining design summary against the grammar.

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
  the tutorial and formal-calculus product repair above now agrees with that account.

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
  The revised selection criteria distinguish that capability from actual case coverage.
  At this repair's original baseline, multi-argument process folds still had an embedding gap;
  the later status reconciliation is tracked separately below.

- [ ] **Wasm argument-fold status.** `c6bacd1c` introduced indexed `args/at`
  and the ordinary CBPV [argument fold](../../lib/std/system/arguments.zy);
  the [Wasm proposal's validation criteria](../proposals/wasm-backends.md#criteria-for-selecting-wasm)
  now report passing multi-argument regressions.
  Its open questions still ask whether to choose an indexed API or a host-closure bridge.
  Remove that superseded choice and audit related summaries; retain the independent continuation-reuse,
  lifetime, and backend-selection questions.

- [ ] **C ABI evidence and static plans.** The [C FFI proposal](../proposals/c-ffi.md#additional-abi-shapes)
  still frames `Layout A` as a runtime recipe without explaining the later source-calculated `Plan A` path.
  Distinguish static storage-plan evidence from runtime layouts and from target-specific ABI classification.
  Static size and alignment calculations alone do not implement aggregate register splitting, memory arguments,
  or hidden result pointers; those extensions remain open.

- [ ] **Contribution-guide prelude references.** [CONTRIBUTING.md](../../CONTRIBUTING.md#check-and-run-source-terms)
  checks `lib/std/prelude.zy`, which is absent at the audit baseline, and refers to its annotated kind prefix.
  Replace the command with an existing independently checked source and link the package explanation
  to the current [kind-prefix example](../../lib/std/README.md#package-composition).

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

- [ ] **N-ary product inference refinement.** The [refinement helper](../../lang/statics/src/normalize/inference.rs)
  fills an unknown expected product with exactly two fresh component types.
  The formal calculus retains this implementation detail in REFINE-PROD; it is not an association law.
  This three-component use rejects with `tyck.type-expected` (expected matching components,
  found `_ * _`) and a secondary `tyck.missing-solution`:

  ```zydeco
  begin
    let first = { fn triple => let (head, _, _) = triple in ret head } that
    ! first ((), (), ())
  end
  ```

The two-component counterpart, using `(head, _)` and `((), ())`, passes.
The three-component program also passes when `Unit = @(intrinsic(unit))` is bound
and the function parameter is annotated `(triple : Unit * Unit * Unit)`.
Review arity-directed refinement and retain all three probes when implementing a change.
The product documentation repair leaves this inference behavior unchanged.

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

## Verification of the product and tutorial repair

The 2026-09-10 pass ran the tutorial through the documentation checker: all 23 guide examples passed (21 accepted terms
and two expected product rejections), alongside two examples from the documentation fixture.
The formal calculus compiled with Typst.
The three product-inference probes above were checked separately; they remain evidence for an open implementation issue.

```sh
cargo run --quiet --bin zydeco -- doc check docs/examples/documentation/counter.zy --guide docs/tutorial/zydeco-guide.md
typst compile --root . lang/statics/type-system.typ /tmp/zydeco-type-system.pdf
```

These checks validate example acceptance and document construction, not execution of every tutorial example
or conformance of the remaining formal rules.

## Drafting corrections

- [x] **Algebra classification.** The original language outline grouped `Monad` and `Algebra` as codata.
  [Monad](../../lib/std/control/monad.type.zy) is codata;
  [Algebra](../../lib/std/control/algebra.type.zy) is a universally quantified computation type.
  The language draft now uses those classifications.

Broader extraction and incoming-link work stays in the [reference plan](reference-plan.md).
