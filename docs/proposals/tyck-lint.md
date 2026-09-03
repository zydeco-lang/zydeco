# A Post-Check Type Lint for the Typed Arena

Type checking is the last phase that reasons about the source program as a whole, and
everything after it — linking, Stack IR, native and WebAssembly backends — consumes its
output on faith.

The artifact is built by staged mutation, and each stage can miss a site.
Types and kinds are allocated as `Fillable` cells whose solutions are substituted only in
the finish phase, after which normalization rewrites every kind and type in place;
a fill or rewrite that misses a site leaves a stale neighbor.
Type-directed elaborations — the monadic algebra translation, copattern elaboration —
rebuild parts of the term through a separate construction API.
The annotations themselves are split across node-keyed tables, surface-term-keyed facts,
and co-located kind indexes, whose provenance entries rechecking and recursion retries
overwrite.
Every local judgment can be individually correct while the whole is incoherent.

This document specifies the *type lint*: an optional pass that re-validates the finished
arena after a successful check.
The name follows GHC's `-dcore-lint`, which re-typechecks the compiler's internal Core
language after every transforming pass and has been one of its most productive bug-finding
tools.
The lint is two passes: an arena-wide well-formedness pass over the annotation tables,
and a re-derivation pass that reconstructs kinds, constructor shapes, and witness scope
from structure.

## What the Lint Can and Cannot Establish

A verifier that shares derivation rules with the checker verifies *consistency*, not
*soundness*.
If the checker assigns a wrong but self-agreeing type through a faulty rule, the lint
re-derives the same wrong answer and stays silent.
What the lint does establish is that the published artifact is internally coherent:
every recorded annotation agrees with the structure of the node it describes,
every reference resolves, and every binder–reference pair is well-scoped.
Those are exactly the assumptions downstream phases make, and exactly the properties
that staged mutation can silently break.

Concretely, the lint targets four bug classes:

1. **Witness escape.** An abstract-type witness (`AbstId`) referenced outside the binder
   that introduced it. With existentials, packages, and package-dependent arrows, witness
   identity is the classic soundness-bug class in this language.
2. **Stale or desynchronized annotations.** A recorded type that disagrees with the
   structure of the node it describes — for example, a type that hole resolution or
   normalization rewrote incompletely.
3. **Well-formedness failures.** Unfilled holes surviving the finish phase, annotations
   pointing at nodes of the wrong sort or at nothing, kinds that disagree with the
   structure of their type.
4. **Elaboration corruption.** Nodes rebuilt by the monadic algebra translation or
   copattern elaboration carrying annotations that no longer match their structure.

## The Shape of the Typed Artifact

The lint operates on the arena *after* the checker has finished and stripped its transient
state (`strip_checker_state` drops the per-node typing environments), so it may rely only on:

- the node stores `kinds_pre`, `kpats`, `tpats`, `types_pre`, `vpats`, `values`, `compus`;
- the node-keyed annotation tables (`annotations_value`, `annotations_compu`,
  `annotations_vpat`, `annotations_tpat`, `annotations_abst`, `annotations_var`), plus the
  co-located kind of each type node;
- the definition tables (`datas`, `codatas`, `seals`, `type_definitions`, `value_aliases`,
  `inlinables`, `generated_defs`);
- the normalized forms recorded by the finish phase (`kinds_normalized`, `types_normalized`).

Two facts about the artifact shape the design.

First, *every allocated node carries an annotation*.
The `Alloc` trait takes the annotation at allocation, and the salsa judgment queries
insert node and annotation together, so annotation presence is checkable arena-wide.

Second, *the arena legitimately contains orphaned nodes*.
Rechecking a site allocates fresh derived identifiers rather than reusing old ones, so
abandoned attempts from inference retries remain allocated but unreachable from the final
tree.
The finish phase nevertheless normalizes *every* node, successful or orphaned, and hole
resolution reports a missing solution whenever any allocated type still cannot be resolved.
Orphaned nodes must therefore satisfy the arena-wide invariants, but they have no stable
binder context, and scope discipline is only meaningful for nodes reachable from a root.

This yields a two-level specification, and the lint implements it as two passes:
arena-wide well-formedness, and structural re-derivation from the annotation roots.

## Arena-Wide Well-Formedness

The well-formedness pass iterates the whole arena, orphaned nodes included, like the coverage
validator iterates every computation.
Each check is local to one node or one table row; no traversal, no equality up to
normalization.

- **Hole absence.** Every kind and type cell is `Fillable::Done`, and a residual hole
  *term* node must be a foreign-import placeholder (`foreign_imports` records it),
  because a checked external value is materialized as a typed hole.
  A successful check implies the finish phase resolved every hole it needed to;
  a surviving `Fill` means the finish guarantee failed.
- **Annotation presence.** Every `ValueId`, `CompuId`, `VPatId`, `TPatId` has an entry in
  its annotation table; every `TypeId` has a co-located kind.
  Abstract-type witnesses are deliberately exempt: their kind may live in
  `annotations_abst`, on the co-located kind of the `Type::Abst` node denoting them
  (as for recursive-group identities), or in the pattern of an enclosing binder, so
  arena-wide presence is not an invariant the checker maintains for them.
- **Annotation sorts.** The recorded type of a value node has kind `VType`, of a
  computation node kind `CType`; the same for value patterns.
  A `TermAnnId::Type(ty, kind)` pair agrees with the co-located kind of `ty` *up to
  normalized kind structure*: annotation reconciliation replaces the synthesized kind
  with a least-upper-bound result, and `VType`/`CType` leaves are not canonicalized
  across the arena, so kind identity must be compared by resolving every leaf through
  its normalized form rather than by identifier or derived equality.
- **Paired-view agreement.** For every `term_facts` entry and for the root annotation,
  the pair's node exists in the arena of the right sort and the pair's type equals the
  node-keyed annotation of that node.
  This is the desync check between the surface-keyed and node-keyed views.
- **Reference existence.** Every `DefId` mentioned by a variable reference, pattern
  binder, or `annotations_var` row resolves in `annotations_var` or `generated_defs`;
  every `AbstId`, `DataId`, `CoDataId`, `FillId` mentioned anywhere resolves in its
  owning table; data and codata arm types and sealed definitions resolve to existing
  nodes; and every child reference inside an allocated kind, type, value, computation,
  or pattern resolves in its arena.

The pass needs no type-level equality reasoning — only the leaf-resolving kind equivalence
above — and runs in one linear pass over the arena.

## Structural Re-Derivation from the Roots

The re-derivation pass reconstructs annotations the way the checker built them, but over
the typed representation rather than the surface, and synthesizing only — bidirectional
checking is irrelevant once every binder is explicit.
The traversal starts from the annotation roots: the root `TermAnnId`, every recorded
definition body (`value_aliases`, `inlinables`, `type_definitions`), every data and codata
arm, and every sealed type.
Orphaned nodes are simply never visited, which is the correct treatment of retries.

### Kinding rules

Write `Ψ ⊢ A : K` for "type node `A` has co-located kind `K`, derivable from its
children", where `Ψ` supplies kinds for type variables (`annotations_var`) and abstract
witnesses (`annotations_abst`).
All comparisons are between *normalized* forms, because definitional equality, not
syntactic equality, is what the checker guarantees.
Every allocated type node is kinded this way, orphaned nodes included; child-sort
violations (an arrow whose domain is not a value type, a product component that is not)
report against the child.

```text
Var(d)        : Ψ(d)
Abst(w)       : Ψ(w)
Thk           : CType -> VType          Ret : VType -> CType
Unit, Opaque,
Primitive(_)  : VType                   OS  : CType
Named(f, A)   : #f :: Ψ⊢A               Label(f, A) : VType, with A : VType
App(F, A)     : K2, where Ψ⊢F : K1 -> K2 and Ψ⊢A : K1
Proj(T, f)    : K, where Ψ⊢T : #f :: K
Abs(b, B)     : Kp -> KB, where binder pattern b : Kp and Ψ, b ⊢ B : KB
Arrow(A, B)   : CType, with A : VType and B : CType
Forall(b, B)  : CType, with Ψ, b ⊢ B : CType
PackPi(...)   : CType, domain : VType, codomain : CType under its witnesses
ValPi(b, C)   : VType, with Ψ, b ⊢ C : VType
Prod(A*)      : VType, with each Ai : VType
Exists(b, B)  : VType, with Ψ, b ⊢ B : VType
```

The binder of `Abs`, `Forall`, `Exists`, and `ValPi`'s type case is a `TypeBinder` that
introduces both a pattern and an abstract witness; the witness receives the pattern's
kind, and the body is kinded under both.
Parameter telescopes of `Data` and `CoData` heads are deferred; their existence is the
well-formedness pass's check.

### Constructor shapes

For terms, the pass judges constructor shapes — the judgments whose conclusion is a fixed
type former around the node's single operand:

```text
Γ ⊢c C ⇓ B                    Γ ⊢v V ⇓ A
─────────────────────         ─────────────────────
Γ ⊢v {C} ⇓ Thk B              Γ ⊢c ret V ⇓ Ret A

Γ ⊢v V ⇓ A                    Γ ⊢v () ⇓ Unit          Γ ⊢v lit ⇓ its primitive
──────────────────────
Γ ⊢v #f = V ⇓ #f :: A
```

A shape judgment fires only when its operand is free of abstract identities and of type
applications other than `Thk` and `Ret` themselves; the guard is stated below.

### The deferral principle

Every other typing judgment compares the recorded annotation of one node against the
recorded annotation of another — tail against let, codomain against body, result against
declaration, pattern against domain — and no such judgment can fire on this artifact.
A shared node used at several instantiations of an enclosing universal, or inside a
package member elaborated per import, carries a single annotation, and its recording
sites legitimately disagree: generic against instantiated, labeled against plain.
The finished arena cannot distinguish those from corruption, so these judgments are
deferred rather than made noisy.
Product shapes are deferred for the same reason — composite operands multiply the
sensitivity — and definition *scoping* is deferred with them: references cross import and
alias boundaries ("repeated imports share that root"), and the arena records no
per-reference binding context.
Definition existence remains the well-formedness pass's check.
The deferred judgments could return only if the arena recorded, per use site, which
instantiation a shared node was checked under.

### Witness scope

Every abstract-type witness reachable from the annotation roots must be bound by an
enclosing structural binder or be ambient.
Ambient witnesses are sealed types, existential skolems, definition-denoted identities,
and named witnesses: recursive type components allocate their identities together, and
package openings bind theirs through the elaborated program, so neither lives under one
structural binder; a name makes a witness an exported identity rather than an accidental
leak.
Unbound witnesses are reported even when their table rows exist, which is what
distinguishes this from the well-formedness pass's existence check.

The scoping account this implements is `docs/proposals/term.md`: a file is one term,
blocks dependency-order mobile bindings into ordinary telescopes, and the elaborated form
obeys strict lexical scope.
Computation recursion is the explicit `fix` form and stays structural.

## Failure Semantics and Gating

A lint failure is a compiler bug, never a user diagnostic.
The pass returns typed `LintError` values carrying the offending identifiers and both the
recorded and expected forms; the gated entry point reports them as an internal compiler
error and aborts, the way `debug_assert` failures abort.

The gate is a typed option, not a global flag: `CommandCompiler` grows a builder flag
exposed as `--lint-types` on the CLI, and the lint runs in `analyze` after a `Checked`
outcome, over the arena that `checked_program` re-materializes from the memoized check.
Because the lint is a pure function of the finished arena and runs outside the salsa
query graph, enabling it never invalidates memoized work, and editor sessions are
unaffected unless they opt in.

## Testing the Verifier

A lint that has never been shown to catch a seeded defect has no credibility, so every
check ships with mutation tests: check a program, clone the finished arena, corrupt
exactly one fact, and assert the matching error variant fires.
Clean-program counterparts assert silence over the corpus, which guards against false
positives — the property that makes the lint trustworthy enough to run in CI.
The corpus should exercise the elaboration-heavy paths (monadic blocks, generalized
comatches, packages) because those rebuild the most structure after checking.
The tests live in `lang/tests/tests/tyck_lint.rs`.

## Status and Remaining Uncertainty

Both passes have landed: well-formedness in `lang/statics/src/validate/lint.rs`,
re-derivation in `lang/statics/src/validate/rederive.rs`, gated behind
`CommandCompiler::with_lint_types` and the `--lint-types` CLI flag.
Every checkable program under `lib/` passes the gate with zero findings.

Questions that remain open:

- **Skolem scope discipline.** Skolems are currently ambient, like every named witness;
  whether a finer discipline is recoverable from the finished arena is open.
- **`Label` payloads.** The design restricts named values to value types; whether every
  `Type::Label` in a finished arena accordingly wraps a `VType`-kinded payload should be
  asserted, but the invariant needs confirmation across the standard library first.
- **`ManifestKind` and `SCons` shapes.** The precise kinded/typed shapes of manifest kind
  components and package witness prefixes remain to be pinned down.
- **Surviving `Type::Var`.** Definition-heavy arenas contain none: bound type variables
  are referenced through abstract witnesses, and definition-backed variables are
  substituted during normalization. Whether any legitimate arena retains one is open;
  if none does, a surviving `Var` can become an immediate finding.

Related documents: the coverage pass this proposal sits beside is described in
[`exhaustiveness.md`](exhaustiveness.md); the arena's retained/transient split in
[`arena-gc.md`](arena-gc.md); query-owned intrinsics in
[`query-owned-statics.md`](query-owned-statics.md); normalization in
[`normalization.md`](normalization.md); the scoping account in [`term.md`](term.md).
