# Reference drift

These are follow-ups from reference drafting, inspected against `289f2a14` on 2026-09-08.
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

- [ ] **Recursive-type admissibility.** The [term proposal](../proposals/term.md#dependency-directed-elaboration)
  requires a chosen guardedness or positivity discipline.
  The [binding checker](../../lang/statics/src/check/binding.rs) checks sealed groups and available kinds;
  the formal calculus explicitly records the absence of a positivity judgment.
  Decide whether to specify that current boundary or add a stronger restriction.
  A new restriction needs accepted and rejected recursive examples before the reference can state it.

- [ ] **Provider-directed inference.** The
  [normalization proposal](../proposals/normalization.md#package-signatures-as-expected-types)
  describes omitted parameter annotations receiving a compiled provider signature.
  The [source-boundary rules](../proposals/term.md#source-terms-and-imports)
  and [source checker](../../lang/statics/src/check/source.rs) require independent source synthesis.
  Clarify where an expected type is locally available and remove any implication
  that an importer can solve a provider's inference variables.
  Preserve rejection of underconstrained imported sources.

- [x] **Numeric package rationale.** Retired `numeric-capabilities.md` justified terminal `Unit`
  by associative product flattening and described a more restrictive package occurrence checker.
  The current [Numeric type](../../lib/std/numeric/numeric.type.zy) already has no terminal `Unit`,
  and [static elaboration](../proposals/normalization.md#implementation-status) replaced the occurrence validators.
  The [library guide](../../lib/std/README.md#numeric-capabilities-and-explicit-instances)
  now uses the current interfaces and checked examples.
  The obsolete claims were not transferred.

- [ ] **Tutorial examples and notation.** The [guide](../tutorial/zydeco-guide.md) includes `exists (X = def as X : K)`
  and an older monadic-basis product opening.
  Compare them with the [grammar](../../lang/surface/src/textual/parser/grammar.lalrpop),
  [monadic basis](../../lib/std/control/monad.zy), and current examples.
  Correct obsolete forms and make complete guide examples opt into documentation checking.
  Rejoin the Builtin table's `surface` row, which is split across two Markdown lines.

- [ ] **FFI example grouping.** The [FFI proposal](../proposals/c-ffi.md) shows a metadata hole followed
  by an unparenthesized expression annotation.
  Used as a complete term, this fails parsing at `:`.
  Add the grouping used in the [working FFI library](../../lib/ffi/xxhash.zy) and check the example
  with its imports and parameter context supplied explicitly.

## Superseded implementation accounts

These entries track superseded implementation descriptions and their replacement homes.
Unchecked entries still need consolidation.

- [x] **Alias lowering.** Retired `aliasing.md` described normalization expanding ordinary alias assignments.
  The current [SPS normalizer](../../lang/stackir/src/high/normalize.rs)
  and [closure conversion](../../lang/stackir/src/low/convert.rs) retain structural aliases.
  [Assembly lowering](../../lang/assembly/src/lower.rs)
  and the [direct SPS WebAssembly emitter](../../lang/wasm-sps/src/emit.rs) preserve the bindee in a temporary or local.
  The [transferred pattern account](../proposals/exhaustiveness.md#literal-and-alias-patterns) describes
  these current boundaries.

- [x] **Coverage product shape.** The [coverage proposal](../proposals/exhaustiveness.md#the-internal-pattern-language)
  described converting typed products to a right-associated binary spine.
  The [matrix conversion](../../lang/statics/src/validate/coverage.rs) retains each typed component vector
  and uses its length as the head arity.
  The receiving proposal now preserves explicit nesting in its account; the tutorial
  and formal-calculus follow-up above remains open.

- [x] **Primitive package layout.** Retired `primitive-packages.md` marked its five-group layout as superseded
  but retained `core`, `representations`, carrier-bearing numeric children, and an obsolete `Unit`-tail rationale.
  Its identity rationale now lives
  in [package modularization](../proposals/package-modularization.md#primitive-identity-and-package-boundaries);
  the [Builtin guide](../../lib/std/README.md#builtin-packages) describes the current contract.
  Incoming links were updated when the proposal was removed.

- [ ] **Remaining package-design summaries.** The opening
  and [checker-constraint discussion](../proposals/package-modularization.md#checker-constraints-on-the-topic-layout)
  in package modularization still describe older core/representation groups.
  The paragraph following `param (/Bytes; /Reader; /io; builtin)` also describes a different set of selections.
  Audit those summaries and positional examples against [Builtin](../../lib/std/builtin.zy)
  and the [shared field-selection rules](../proposals/field-projection.md) during that proposal's consolidation.

- [ ] **Query ownership.** The [query-owned-statics proposal](../proposals/query-owned-statics.md)
  records both its target architecture and the achieved checker-owned mutable core.
  Base the compiler reference on the [achieved form](../proposals/query-owned-statics.md#achieved-form-2026-08-14),
  [checker](../../lang/statics/src/check), and [query inputs](../../lang/statics/src/query/input.rs).
  Retire the completed migration instructions after extracting their rationale.

- [ ] **Source-map layout.** The [source-map exploration](../ideas/span-source-map.md) opens with implementation deltas
  but retains a present-tense account of the old 32-byte span and a different map ownership path.
  The current [span](../../lang/utils/src/span.rs) is eight bytes
  and the [merged span arena](../../lang/surface/src/textual/span.rs) carries its map.
  Preserve the motivation and alternatives; replace the obsolete implementation and migration account.

## Drafting corrections

- [x] **Algebra classification.** The original language outline grouped `Monad` and `Algebra` as codata.
  [Monad](../../lib/std/control/monad.type.zy) is codata;
  [Algebra](../../lib/std/control/algebra.type.zy) is a universally quantified computation type.
  The language draft now uses those classifications.

Broader extraction and incoming-link work stays in the [reference plan](reference-plan.md).
