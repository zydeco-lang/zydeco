# Stronger verification of the typed arena

Staged checking and elaboration can publish an incoherent artifact even when each local source judgment succeeds.
A stale annotation, missing reference, or escaped witness can survive until a backend relies on it.
The optional [typed-arena lint](../references/compiler.md#typed-arena-lint) checks the finished artifact;
that reference owns its implemented passes, gate, and failure behavior.
This record explains the limits of independent verification and the information needed to strengthen it.

A verifier sharing derivation rules with the checker establishes consistency rather than a separate soundness proof.
A faulty rule may reproduce the same wrong answer twice.
Its practical value is detecting corruption introduced by mutation, normalization, retries, and generated syntax,
with [seeded-defect tests](../references/compiler.md#c16-validation-debugging-and-extending-the-implementation) paired
with clean programs.

## The artifact limits re-derivation

Every allocated node needs a well-formed annotation, including abandoned allocations from inference retries.
Only reachable nodes have a meaningful structural binder context.
This is why arena-wide integrity and root-based scope reconstruction are different obligations.
The checker's transient environments have already been stripped when the verifier runs.

A shared typed node can appear under several instantiations of an enclosing universal
or package member while carrying one node-keyed annotation.
Its recording sites may legitimately differ: generic versus instantiated, or labeled versus plain.
The finished artifact does not distinguish those differences from arbitrary cross-node annotation corruption.
Re-deriving every let, application, declaration, and product by directly comparing child annotations would
therefore reject valid programs.

The current constructor-shape checks cover thunk, return, named-value, unit, and literal introductions.
For an operand-dependent shape, the guard excludes abstract identities and type applications other than `Thk` and `Ret`.
That guard follows data and codata payloads. Closed data/codata shapes compare their named arms structurally;
distinct arena identities alone do not imply distinct types.
Nominal identities still compare as abstract witnesses.
Product shapes and general parent/child comparisons remain deferred
because their components can carry distinct instantiations.
Definition-reference scope also remains deferred across import and alias boundaries;
reference existence is still checked.

Stronger checks require explicit use-site instantiations or an equivalent derivation witness.
The design should first identify which downstream assumption needs that information and measure its retention cost.
Reintroducing per-node environments would work
against [the session's retained-fact boundary](../references/compiler.md#analysis-facts-and-materialization).

## Witness scope policy

A reachable abstract witness must be structurally bound or belong to the ambient set.
That set currently includes seals, existential skolems, definition-denoted identities, and named witnesses.
Recursive components allocate identities together, and package openings distribute their bindings through elaboration;
neither necessarily has one enclosing structural binder in the finished tree.
Names currently allow exported identities to survive this reconstruction.

This policy catches an unbound anonymous witness even when its table entry exists,
but it does not reconstruct every source-level non-escape proof.
The visited-node cache also checks a shared node only under its first encountered scope;
checking every use requires a scope-sensitive traversal or recorded use-site evidence.
A finer policy for skolems and named witnesses needs provenance that distinguishes deliberate export
from accidental escape.
Any change must retain accepted package and recursion cases while detecting a seeded leak.

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
