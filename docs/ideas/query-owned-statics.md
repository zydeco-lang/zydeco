# Query-Owned Statics

## Problem

The producer-side migration of the judgment layer is bounded by the arena-read wall: most
judgments read tables of the `StaticsArena` while the check is still writing them —
`annotations_var` (lub-merged on repeated binders, diverging from the environment), the
`Fillable` pre-node state, `env_type` records, the builtin roles, and the `IntrinsicStatics`
singletons. Salsa queries cannot read that mid-check state, and per-call snapshots would
destroy memoization. Evidence and failed workarounds are recorded in
[docs/logs/query-based-tyck.md](../logs/query-based-tyck.md).

## Target Architecture

The typed arena becomes **query-owned state in the salsa sense** while remaining the
materialized output that downstream backends consume:

- Every judgment is a pure salsa query keyed by interned scoped entities plus environment
  carriers (`ScopedData`, `EnvData`, interned `DefId`/`TermId`/`PatId`).
- Mid-check arena reads become query calls. A table read by the checker is replaced by the
  query that *computes* that table cell deterministically from the source program, so no
  mutable state is observed during checking.
- `check_source` becomes a **materializer**: it walks the scoped program in canonical order,
  calls the judgment queries, and inserts their fragments into the `StaticsArena`. Arena
  identifiers stay derived per allocation site, so query results reproduce them without a
  cursor. The arena tables that downstream reads (`*_normalized`, annotations, environments,
  intrinsics) are filled by the materializer from query results, preserving the external
  contract.

## Table Conversion Patterns

The checker's tables fall into four patterns, each with one conversion recipe:

1. **Singleton caches** (`IntrinsicStatics`, global definitions). The query decides the
   singleton (memoized per check), the materializer records it. The checker-side cache
   lookups disappear from judgment code; `Construct::build` call sites that read the cache
   keep reading the materialized table, and their miss paths become `unreachable!` once the
   materializer guarantees the cache is filled before any reader — the migration order must
   establish that invariant per table.
2. **Merge tables** (`annotations_var`). The cell is a deterministic fold over the
   definition's binder sites in canonical order: the query locates each binder (via the
   scoped arena's pattern storage), computes the binder's contribution, and lub-merges in
   order. The binder-side `insert_or_get` + `replace_existing` mutation disappears.
3. **Fill-state tables** (`kinds_pre`, `types_pre` as `Fillable`). The fill state becomes a
   query keyed by the allocation site: `fill_state(db, data, site) -> Fillable<..>`; hole
   resolution becomes a query over `FillId` (the read-side `fill_solution` already exists).
   Filling a hole no longer mutates a pre-node; it changes which query result the materializer
   inserts.
4. **Environment records** (`env_type`, `annotations_*`). These are derived columns of the
   judgment queries: the query that produces a node also produces its records from the
   `EnvData` carrier, exactly as the current materializer does for the intrinsic judgments.

## Cycle Strategy

Judgment queries form a DAG except where binders and bindees are mutually recursive. Two
mechanisms combine, mirroring the current checker:

- `RecGroup` keeps its pre-introduction structure: the recursive group's identities (abstract
  types, definitions) are introduced by a group-level query before the equation queries run.
- salsa cycle recovery (`#[salsa::recover]`) is the backstop for cycles that the
  pre-introduction does not break, mirroring how the wholesale checker re-checks with
  coarser facts.

## Migration Order

1. `IntrinsicStatics` (this is the first table; the `Internal` judgment queries already
   produce the nodes, so the remaining work is moving the cache ownership and establishing
   the fill-before-read invariant).
2. `annotations_var` and the `Var` judgment (the merge-fold pattern; unlocks the most common
   term).
3. The `Fillable` pre-node state and hole solving (the fill-state pattern).
4. Remaining term and pattern judgments table by table, each step deleting the corresponding
   checker-side mutation.
5. `env_type`/annotation records fall out of step 4 as derived columns.

Each step: workspace suite green, diagnostic outputs diffed against the previous commit, one
commit per table.

## Risks

- Diagnostic ordering and blame text are user-visible; the fold queries must visit binder
  sites in exactly the checker's canonical order.
- The singleton-cache invariant (fill before read) can only be guaranteed by construction in
  the materializer; violated ordering would panic with `unreachable!`, which is loud and
  testable.
- The materializer must not slow the wholesale check: queries add salsa bookkeeping per node;
  benchmark after the first two tables and again at the end.

## Related

- Worklog and wall evidence: [query-based type checking](../logs/query-based-tyck.md)
- `docs/ideas/normalization.md` for the normalization semantics behind the fill-state pattern.
