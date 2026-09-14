# Stronger verification of the typed arena

The optional [typed-arena lint](../references/compiler.md#typed-arena-lint) owns current artifact integrity,
constructor checks, ambient witnesses, and their limits.
This proposal concerns the evidence needed to strengthen them.
The checker's transient environments are absent from the finished artifact; adding checks must account for
that boundary and preserve valid generic instantiations and package openings.

## Use-site derivation evidence

General parent/child checks, product reconstruction,
and definition-reference scope need explicit use-site instantiations or equivalent derivation witnesses.
Identify which downstream assumption needs that information and measure its retention cost.
Reintroducing per-node environments would work
against the [retained-fact boundary](../references/compiler.md#analysis-facts-and-materialization).

Checking a shared node under every scope needs a scope-sensitive traversal or recorded occurrence evidence.
A finer policy for skolems and named witnesses must distinguish deliberate export from accidental escape.
Retain accepted package and recursive-group cases while detecting a seeded leak.
An independently executed verifier still cannot establish separate soundness
if it shares the checker's faulty derivation rule.

## Remaining checks

- Confirm whether every `Type::Label` payload has kind `VType` across all elaborations before asserting it.
- Establish the precise kinded shapes of `ManifestKind` and the static package prefix `SCons`.
- Determine whether a finished arena may legitimately retain `Type::Var`.
  Bound variables usually become abstract witnesses and definition-backed variables are substituted;
  an absence invariant needs evidence across source and generated roots.
- Reconstruct parameter telescopes for `Data` and `CoData` heads; current integrity checks establish their existence.
- Decide how optional lint should interact with deliberate source holes.
  Ordinary checking supports their inspection, while the current lint permits only recorded foreign placeholders.
  A broader interactive gate needs a typed distinction between intentional incompleteness and corrupt finalization.

The implementation lives in [lint.rs](../../lang/statics/src/validate/lint.rs)
and [rederive.rs](../../lang/statics/src/validate/rederive.rs);
[mutation tests](../../lang/tests/tests/tyck_lint.rs) establish which corruption each check actually detects.
