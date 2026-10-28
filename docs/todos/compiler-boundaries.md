# Compiler boundary discrepancies

These bounded implementation issues were retained from the reference audit and subsequent focused reviews.
Except where a section gives a later baseline, the relevant code was inspected at `3b4dd665` on 2026-09-14;
the accepted and rejected source probes below were rechecked with a newly built debug CLI.
The checkout included the local 16 MiB CLI worker-stack mitigation; none of these probes depends on deep nesting.
They record current limitations and intended repairs, without promoting a wider language feature.

## N-ary product inference refinement

[L5](../references/language.md#5-values-products-and-data) preserves n-ary products;
[L4](../references/language.md#4-classification-and-inference) describes expected-shape refinement.
The [refinement helper](../../lang/statics/src/normalize/inference.rs) fills an unknown expected product
with exactly two fresh component types.
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

## Nested witness diagnostic

[L9](../references/language.md#9-polymorphism-and-packed-values)
and [C5](../references/compiler.md#witness-evidence-and-field-lookup) govern witness evidence.
Computation binders can collect witnesses beneath product patterns,
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

A generalized computation witness route remains
in the [computation witness-route idea](../ideas/README.md#computation-witness-routes).
Improving the rejection diagnostic does not require accepting that extension.

## Monadic closure validation

The approved [closed translation boundary](../references/language.md#closed-translation-boundary)
requires external types to enter through type parameters inside the monadic annotation.
The implementation's [environment filter](../../lang/statics/src/environment.rs) retains lexical type bindings,
and the translator starts with an empty structure environment.
Consequently an external abstract computation type may work until an operation needs its algebra,
at which point checking reports `tyck.missing-structure`.

At `a12537a0`, this complete term passes checking even though `B` is free in the annotated payload:

```zydeco
param (/CType; /Thk; builtin) : @(import("../../lib/std/builtin.zy")) in
let basis = @(import("../../lib/std/effect/monad.zy")) in
let (/Monad; /Algebra) = basis in
fn (B : CType) => @[monadic] fn (k : Thk B) => ! k
```

Moving `fn (B : CType) =>` inside the annotation gives the intended type-parameter interface
and causes translation to introduce its structure argument.
The retained lexical witness is implementation drift, not an invitation to infer or register an outer algebra.

- [ ] Validate closure over terms and classifiers after resolving intrinsic constants and admissible closed definitions.
  Distinguish parameters bound in the payload from enclosing witnesses; do not reject closed intrinsic aliases just
  because their source names were resolved outside it.
- [ ] Diagnose a free external witness at the block boundary, with a parameter-passing suggestion,
  before partial algebra construction.
  Choose and retain a stable diagnostic code in the implementation change.
- [ ] Pair the rejected outer binder above with the accepted inner binder, including `VType`,
  `CType`, higher-kinded types, and existential witnesses.
  Keep existing runtime-capture rejection and closed-definition cases.
- [ ] Migrate examples whose type parameters precede the annotation, including the effect examples
  under `lib/tests/effects/`, to the explicit interface and update argument order and structure arguments together.

Implemented [nominal translation](../references/language.md#applicative-nominal-translation) rejects external
nominal declarations and free external witnesses in reachable local nominal definitions.
It and [value-function translation](../references/language.md#polymorphic-value-functions) do not establish
the complete closure validation still required above.

## Inferred higher-order value-function witnesses

[L4](../references/language.md#4-classification-and-inference) governs inferred classifiers,
and [L8](../references/language.md#8-value-functions-and-views) permits higher-order value functions.
Observed on 2026-09-25 while implementing monadic value functions, this ordinary program rejects
at `specialize make` with `tyck.type-mismatch`, reporting distinct identities for the two bound `B` witnesses:

```zydeco
param (/VType; /CType; /Thk; /Ret; /Int; builtin) : @(import("../../lib/std/builtin.zy")) in
let val make (B : CType) = {
  fn (operation : Thk B) => do _ <- ret (); ! operation
} in
let val specialize (factory : val pi (B : CType) . Thk (Thk B -> B)) =
  factory (Ret Int)
in
! (specialize make) { ret 1 }
```

An explicit classifier on `make` passes:
`let make : val pi (B : CType) . Thk (Thk B -> B) = val (B : CType) => { ... }`.
The failure precedes algebra translation and occurs with no monadic annotation.
Investigate inference-hole reconciliation under paired binder scopes,
retaining both versions as regressions when repaired.
The monadic higher-order tests currently use the explicit signature.

## Source-load diagnostic locations

[C2](../references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance) keeps source coordinates
with their owning file,
and [C15](../references/compiler.md#c15-diagnostics-formatting-documentation-and-interactive-tooling)
owns source-facing diagnostics.
The [source errors](../../lang/session/src/source/err.rs) retain local byte ranges,
but import-path and cycle `Display` implementations render spans directly;
the [CLI renderer](../../cli/src/diagnostics.rs) delegates those load errors to `Display`.

The fresh probe imports `child.zy` from `root.zy`.
The child starts with `-- λ雪🙂`, a blank line, then `@(import("missing.zy"))` on line 3.
The CLI reports the raw range `14-37`, without a line/column location or source excerpt.
Replacing the target with `root.zy` reports a cycle with ranges `0-21` and `14-34`.
Replacing the child's import with `()` passes.

- [ ] Resolve these early locations through the source template's file map before rendering line/column locations.
  Keep template-local ranges distinct from assembled checker spans and preserve all independent load failures.
  Pair missing imports and cycles with an imported source containing non-ASCII text;
  compare each reported location with its owning source.

## Partial annotation scope during formatting

The [formatter's semantic-identity law](../references/compiler.md#semantic-identity)
and [partial-binder scope](../references/language.md#7-patterns-and-coverage) govern this boundary.
Observed during the 2026-10-13 fixture migration, formatting this source fragment merges the two function headers:

```zydeco
@[partial] fn (0 : Int) => fn (1 : Int) => ret 0
```

The original rejects with `tyck.refutable-binding`; the printed `@[partial] fn (0 : Int) (1 : Int) => ret 0` is accepted
because both binders now belong to the annotated header.
The [partial-binding tables](../../lang/tests/tests/partial.rs) retain this rejected nested header
and accepted single-header counterparts as inline Rust sources.

Preserve nested headers when flattening would extend a meta annotation's scope.
Add a formatter round-trip regression for this pair.

## Concrete payload annotations during package introduction

Sealing in `pack` rewrites the disclosed witness's occurrences in the payload type
into the abstract binder, so a payload that speaks only in concrete primitive types produces
a witness-independent body ([L9](../references/language.md#9-polymorphism-and-packed-values);
[Packed Value Introduction](../../DESIGN.md#packed-value-introduction) gives the mechanism account).
The payload is checked against the disclosed witness, so an explicit annotation naming the abstract binder is accepted
without abstracting the synthesized body.
Rechecked with the debug CLI on 2026-10-20,
`pack (X : VType) is Int where 0 : X end` synthesizes `exists (X : VType) . Int`,
while an expected `exists (X : VType) . X` rejects with `tyck.type-mismatch`.
The [2026-09-07 audit](../evaluations/2026-09-07-bug-hunt/README.md#documented-behavior-retained-from-the-audit)
observed that this difference is not reported.

- [ ] Retain both probes and decide whether a concrete exported body should warn about the annotation
  or whether the expected-existential spelling needs clearer documentation.
  Accepted dependent payload annotations must keep working,
  so the [existential cases](../../lang/tests/cases/existential) establish intended uses.
  Any semantic change belongs in the package-introduction account, not in this item.
