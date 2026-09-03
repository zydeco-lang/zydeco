# A Post-Check Type Lint for the Typed Arena

Type checking is the last phase that reasons about the source program as a whole,
and everything after it — linking, Stack IR, native and WebAssembly backends — consumes
its output on faith.
That output is not produced in one pass, however: the checker allocates typed nodes with
provisional hole annotations, solves the holes in a finish phase, rewrites every kind and
type by normalization in place, and type-directed elaborations (monadic blocks, generalized
comatches) rebuild parts of the term through a separate construction API.
A missed site in any of these mutations leaves an artifact that is internally inconsistent
while every local judgment was individually correct.

This document proposes a *type lint*: an optional pass that re-validates the finished arena
after checking succeeds.
The name follows GHC's `-dcore-lint`, which re-typechecks the compiler's internal Core
language after every transforming pass and has been one of its most productive bug-finding
tools.
Zydeco's lint plays the same role for `StaticsArena`: it re-derives kinds and types
bottom-up over the typed representation and compares them with the recorded annotations.

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

1. **Scope escape.** An abstract-type witness (`AbstId`) or definition (`DefId`) referenced
   outside the binder that introduced it. With existentials, packages, and package-dependent
   arrows, witness identity is the classic soundness-bug class in this language.
2. **Stale or desynchronized annotations.** A recorded type that disagrees with a
   bottom-up re-synthesis of the node's children — for example, a type that hole
   resolution or normalization rewrote incompletely.
3. **Structural well-formedness failures.** Unfilled holes surviving the finish phase,
   annotations pointing at nodes of the wrong sort or at nothing, kinds that disagree
   with the structure of their type.
4. **Elaboration corruption.** Nodes rebuilt by the monadic algebra translation or
   copattern elaboration carrying annotations that no longer match their structure.

## Why Staged Mutation Makes This Valuable Here

Three properties of the current checker make internal inconsistency a realistic failure
mode rather than a theoretical one:

- **Holes are solved after the fact.** Types and kinds live as `Fillable` cells;
  inference decisions are recorded as `FillId` solutions and only substituted during the
  finish phase (`resolve_holes_and_collect`), after which every kind and type is
  normalized in place (`normalize_and_validate_k` in
  [`lang/statics/src/check/mod.rs`](../../lang/statics/src/check/mod.rs)).
  A solved fill or rewrite that misses a site leaves a stale neighbor.
- **Elaborations rebuild terms.** `elaborate::monadic` consumes checked-term handles and
  reconstructs terms through `MonConstruct`; copattern elaboration generates matches and
  abstractions. These are Zydeco's analogue of Core-to-Core passes.
- **Annotations live in several side tables.** Node-keyed tables
  (`annotations_value`, `annotations_compu`, `annotations_vpat`, `annotations_tpat`,
  `annotations_abst`, `annotations_var`), surface-term-keyed facts (`term_facts`),
  and co-located kind indexes on type nodes must all agree.
  Rechecking and recursion retries replace provenance representatives
  (see `docs/proposals/arena-gc.md` for the retained/transient split),
  and nothing today re-establishes that the tables stayed in sync.

## The Shape of the Typed Artifact

The lint operates on the arena *after* the checker has finished and stripped its transient
state (`strip_checker_state` drops the per-node typing environments), so it may rely only on:

- the node stores `kinds_pre`, `kpats`, `tpats`, `types_pre`, `vpats`, `values`, `compus`;
- the node-keyed annotation tables listed above, plus the co-located kind of each type node;
- the definition tables (`datas`, `codatas`, `seals`, `type_definitions`, `value_aliases`,
  `inlinables`, `generated_defs`, `annotations_var`);
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

The well-formedness pass iterates the whole arena, orphaned nodes included, like the coverage validator
iterates every computation.
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

The well-formedness pass needs no type-level equality reasoning — only the leaf-resolving kind
equivalence above — and runs in one linear pass over the arena.

## Structural Re-Derivation from the Roots

The re-derivation pass reconstructs annotations the way the checker built them, but over the typed
representation rather than the surface, and synthesizing only — bidirectional checking is
irrelevant once every binder is explicit.
The traversal starts from the annotation roots: the root `TermAnnId`, every recorded
definition body (`value_aliases`, `inlinables`, `type_definitions`), every data and codata
arm, and every sealed type.
Orphaned nodes are simply never visited, which is the correct treatment of retries.

### Kinding rules

Write `Ψ ⊢ A : K` for "type node `A` has co-located kind `K`, derivable from its
children", where `Ψ` supplies kinds for type variables (`annotations_var`) and abstract
witnesses (`annotations_abst`).
All comparisons are between *normalized* forms (`normalized_at`, `normalized_kind_at`),
because definitional equality, not syntactic equality, is what the checker guarantees.

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
Data(d), CoData(c) : the parameter arrow kind derived from the definition's telescope
```

The binder of `Abs`, `Forall`, `Exists`, and `ValPi`'s type case is a `TypeBinder` that
introduces both a pattern and an abstract witness; the witness receives the pattern's
kind, and the body is kinded under both.

### Typing rules

Write `Γ ⊢v V ⇓ A` and `Γ ⊢c C ⇓ B` for synthesis of the recorded annotation from the
node's children.
The core call-by-push-value rules, as verified against the checker:

```text
Γ ⊢c C ⇓ B                    Γ ⊢v V ⇓ A
─────────────────────         ─────────────────────
Γ ⊢v {C} ⇓ Thk B              Γ ⊢c ret V ⇓ Ret A

Γ ⊢v V ⇓ Thk B                Γ ⊢c C ⇓ A -> B     Γ ⊢v V ⇓ A
──────────────────────        ─────────────────────────────────
Γ ⊢c ! V ⇓ B                  Γ ⊢c C V ⇓ B

Γ ⊢c C ⇓ Ret A    Γ, p : A ⊢c C' ⇓ B        Γ ⊢v V ⇓ A    Γ, p : A ⊢c C' ⇓ B
────────────────────────────────────        ─────────────────────────────────
Γ ⊢c do p <- C; C' ⇓ B                       Γ ⊢c let p = V in C' ⇓ B
```

The remaining cases follow the same pattern: `TAbs`/`TApp` introduce and instantiate
`Forall`; `VAbs` introduces `Arrow` (or `PackPi` for package-dependent abstractions);
`Fix` types its body at the arrow being defined; `Match` arms share one branch type;
`CoMatch` inhabits the codata type named by its hint, each arm agreeing with its
destructor's declared result; `Dtor` selects that declared result; `Ctor` inhabits the
data type owning the constructor, payload checked against the arm type; `VCons` builds
`Prod` of the component types; `SCons` opens an `Exists` telescope with its static prefix.
Value-level `ValAbs`/`ValApp` mirror the computation-level rules through `ValPi`.

The re-derivation pass checks, for every visited node, that the synthesized annotation equals the recorded
one up to normalized equality, and that every `Var`/`Abst` reference is bound by an
enclosing binder or is a global (a global definition, a sealed type, or an existential
witness opened by an enclosing package elimination).
Unbound and escaped references are reported even when the referenced table row exists,
which is what distinguishes this from the well-formedness pass's existence check.

## Failure Semantics and Gating

A lint failure is a compiler bug, never a user diagnostic.
The pass returns typed `LintError` values carrying the offending identifiers and both the
recorded and re-derived forms; the gated entry point reports them as an internal compiler
error and aborts, the way `debug_assert` failures abort.

The gate is a typed option, not a global flag: `CommandCompiler` grows a builder flag
exposed as `--lint-types` on the CLI, and the lint runs in `analyze` after a `Checked`
outcome, over the arena that `checked_program` re-materializes from the memoized check.
Because the lint is a pure function of the finished arena and runs outside the salsa
query graph, enabling it never invalidates memoized work, and editor sessions are
unaffected unless they opt in.

## Testing the Verifier

A lint that has never been shown to catch a seeded defect has no credibility, so mutation
tests are part of the deliverable, paired with clean-program counterparts as the repo's
testing principles require:

1. Check a representative program, clone the finished arena, corrupt exactly one fact
   (swap an annotation, detach a node, leave a fill unresolved, point a variable at a
   binder that does not enclose it), and assert the lint reports the matching error
   variant.
2. Run the same corpus with the lint enabled and assert silence, which guards against
   false positives — the property that makes the lint trustworthy enough to run in CI.

The corpus should exercise the elaboration-heavy paths (monadic blocks, generalized
comatches, packages) because those rebuild the most structure after checking.

## Phasing and Remaining Uncertainty

The well-formedness pass has landed in `lang/statics/src/validate/lint.rs`, gated behind
`CommandCompiler::with_lint_types` and the `--lint-types` CLI flag, with mutation tests in
`lang/tests/tests/tyck_lint.rs`.
Its first run over the whole `lib/` corpus produced zero findings after three invariant
corrections, recorded in the well-formedness section above and in
[`../logs/2026-09-02-tyck-lint.md`](../logs/2026-09-02-tyck-lint.md):
kind pairs must be compared through leaf-resolving normalized equivalence; witness kinds
are not arena-wide table entries; and hole term nodes are legitimate as foreign-import
placeholders.
The re-derivation pass follows once the well-formedness pass has run clean over the
test corpus for a while.
Four questions remain open and are deliberately deferred to the re-derivation pass's
implementation,
where the first traversal attempt will answer them:

- **Skolem scope discipline.** Existential elimination introduces witnesses that are
  recorded globally (`existential_skolems`) rather than through a structural binder.
  Whether the final artifact always connects such a witness to an enclosing package
  elimination — making escape structurally checkable — or whether some legitimate terms
  keep skolem references the traversal cannot justify must be settled by experiment.
- **`Label` payloads.** The design restricts named values to value types; whether every
  `Type::Label` in a finished arena accordingly wraps a `VType`-kinded payload (and never
  a computation type) should be asserted, but the invariant needs confirmation across
  the standard library first.
- **`ManifestKind` and `SCons` rules.** The precise kinded/typed shapes of manifest kind
  components and package witness prefixes are stated above at the level of confidence the
  current reading supports and will be pinned down rule-by-rule while implementing.
- **Variable representation after normalization.** The corpus shows no `Type::Var` nodes
  surviving in finished arenas of definition-heavy programs: bound type variables are
  referenced through abstract witnesses, and definition-backed variables are substituted
  away. Whether any legitimate finished arena retains `Type::Var`, and under what
  circumstances, determines whether the re-derivation pass ever needs the `annotations_var` lookup for
  kind synthesis or can treat a surviving `Var` as an immediate finding.

Related documents: the coverage pass this proposal sits beside is described in
[`exhaustiveness.md`](exhaustiveness.md); the arena's retained/transient split in
[`arena-gc.md`](arena-gc.md); query-owned intrinsics in
[`query-owned-statics.md`](query-owned-statics.md); normalization in
[`normalization.md`](normalization.md).
Implementation worklogs should record mutation-test results and any rule corrections as
they surface.
