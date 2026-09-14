# Compiler boundary discrepancies

These bounded implementation issues were retained from the reference audit.
The relevant code was inspected at `3b4dd665` on 2026-09-14; the accepted
and rejected source probes below were rechecked with a newly built debug CLI.
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

## Nested package witness diagnostic

[L9](../references/language.md#9-polymorphism-and-packages)
and [C5](../references/compiler.md#package-evidence-and-lookup) govern witness evidence.
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
in the [computation witness-route idea](../ideas/computation-witness-routes.md).
Improving the rejection diagnostic does not require accepting that extension.

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

## ZASM execution panics

[C10](../references/compiler.md#c10-zasm-stack-analysis-and-local-representation-choices) owns ZASM;
the [CLI](../../cli/src/main.rs) exposes its interpreter through `build -t zasm -x`.
The [interpreter](../../lang/assembly/src/interp.rs) still has `todo!()` branches
for `Terminator::Extern` and `Instruction::AllocContext`.
The first is reachable from a valid process exit, without foreign libraries or native tools.

This source uses a path relative to `lib/std/`:

```zydeco
param (/process) : @(import("builtin.zy")) in
! process/exit 0
```

The audit placed the program in a temporary directory and used the absolute path to `lib/std/builtin.zy`.
With `PROGRAM` naming that file and the commands run from the repository root,
the freshly built CLI produces these results:

| Command | Result |
| --- | --- |
| `target/debug/zydeco run "$PROGRAM"` | Exits 0. |
| `target/debug/zydeco build "$PROGRAM" -t zasm` | Exits 0 and renders `<extern:exit/1:Control>`. |
| `target/debug/zydeco build "$PROGRAM" -t zasm -x` | Panics at the unimplemented external terminator and exits 101. |

The bounded repair is to reject unsupported ZASM execution explicitly before running the program.
Completing host operations and context allocation is a larger alternative.
Retain the successful interpreter/rendering counterparts and a subprocess rejection regression;
check that unsupported execution neither panics nor performs earlier program effects.
