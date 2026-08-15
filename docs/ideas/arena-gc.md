# Deterministic Arena Reclamation

## Problem

One type-checked root materializes a `StaticsArena` whose size is bounded by the
*elaboration* of the program, not by its source. Checking the standard library alone
(`std/std.zy`, 65K scoped terms) produces 2.06M `types_pre` nodes plus per-node annotations,
environments, and a normalized copy — a 32x amplification over the scoped program, dominated
by tiny structural nodes (46% `App`, 25% `Arrow`, 13% `Label`, 11% `Prod`). Measured peaks for
one fresh check dropped from 7.5GB to 2.42GB after the phase merge, the normalized delta, and
table pre-reservation, but the arena still retains hundreds of megabytes per root, and a
long-lived session that analyzes many roots accumulates them without bound. That accumulation
was the original cause of the test-suite OOM (a shared session reached ~29GB before the
`SessionPool` workaround capped it), and it is the editor's fate as well: one salsa database
per workspace, one arena per analyzed root, nothing ever reclaimed.

salsa 0.26 offers no per-query eviction (`Database::evict_lru` resets revisions only; the
storage is freed when the whole database drops). Reclamation therefore has to be designed by
us, at the arena layer, with deterministic rules.

## Principle: Recomputation

The judgment-layer migration has already made the arena a *cache* (see
[query-owned-statics.md](query-owned-statics.md) and
[docs/logs/query-based-tyck.md](../logs/query-based-tyck.md)). Every node's identifier is
derived deterministically from its site — `KeySpaceId::derive(tag, entity_space, entity_raw,
occurrence)` — and every node's content is the value of a small memoized judgment query keyed
by that site. Re-executing a query reproduces the same identifiers without a shared cursor, so
a dropped arena node can be rebuilt exactly, provided the query's inputs (the scoped program
and the salsa database) survive. The salsa judgment memos are the durable, recomputable layer;
the arena is its materialization.

Recomputation is therefore the budget question: a table is worth retaining exactly to the
extent that (a) it cannot be re-derived locally, or (b) its absence would force re-running the
checker over a region rather than replaying a judgment.

## Criterion: Keyed Index vs Occurrence Payload

The checker and the linkers read the arena in two fundamentally different ways:

- **Random access by key.** `annotations_var[def]`, `solus[fill]`,
  `codatas[id].get(&dtor)`, `type_definitions[def]` — lookups indexed by a *declared* name, a
  hole, or an abstract identity. Nothing in the neighborhood of the read site determines these
  entries; they are the global context that the checker extends and the linkers follow.
- **Traversal by occurrence.** The typed tree (`values`, `compus`, `types_pre`) is reached by
  walking from roots — the root annotation, definition bodies, pattern binders — along the
  child references embedded in each node. Every node along such a walk is the judgment value
  at an occurrence site and can be replayed.

This splits the arena into two generations:

- **L (long-lived): keyed indexes**, bounded by the source's *declaration* count (thousands),
  kept across roots. They are the entry points from which everything else is reachable and
  the context against which later checks run.
- **S (short-lived): occurrence payload**, bounded by the *elaboration* size (millions), valid
  while one root's consumers traverse it, then discarded.

The user-facing anchors this criterion predicts are variable names (the `DefId` index into
types) and hole solutions (the `FillId` index into unification outcomes): both are keyed,
globally consulted context, and both are exactly what the editor shows to the user. The
proposal generalizes them to every declaration-shaped table.

## Table Classification

| Retain (L) | Rationale |
| --- | --- |
| `annotations_var` | the name table; editor facts and env lookups index by `DefId` |
| `solus` / `fills` / `fill_scopes` / `fill_hints` | hole solutions; editor displays them |
| `datas` / `codatas` | arm tables keyed by `CtorName` / `DtorName`; stackir reads them |
| `type_definitions` / `inlinables` | definition-body entry points for unfolding and inlining |
| `value_aliases` / `package_aliases` | alias entry points for package resolution |
| `seals` / `absts` / `abst_hints` | abstract identities and their representatives |
| `existential_skolems` | skolem markers opened with packages |
| `terms` / hints | the scoped-entity-to-typed-id site index that makes S replayable |
| `intrinsics` / `builtin_roles` | query-owned singletons and role attachments |

| Discard (S) | Rationale |
| --- | --- |
| `types_pre` / `kinds_pre` | occurrence payload; 2.06M nodes for std |
| `values` / `compus` / `vpats` / `tpats` / `kpats` | the typed tree; traversal target of the linkers |
| `annotations_type` / `annotations_compu` / `annotations_value` | per-node annotations ride along with their node |
| `kinds_normalized` / `types_normalized` | derived columns of the same nodes |

## Lifecycle

- S lives in **one per-root materialization slot**. A root is checked into the slot, consumed
  (linked to dynamics, lowered to stackir, or queried for editor facts), and the slot is then
  discarded. Reclamation is deterministic: no heuristics, no reference counting, one
  generation. In the session this replaces `ProgramAnalysis`'s ownership of the arena with a
  scoped borrow.
- L lives as long as the database and stays small. Its own reclamation — if ever needed —
  belongs to the memo layer, not to this design: dropping L means re-*checking* regions, not
  re-materializing nodes, so it is a separate, much more expensive policy (root-scoped LRU over
  the salsa database, in the spirit of the test-suite `SessionPool`, productionized).
- The salsa judgment memos are the recompute source and are **not** part of S. Their growth
  across many roots in one database is the remaining retention question; see open questions.

## Replay Protocol

Dropped S nodes are rebuilt in one of two ways, converging on the second:

1. **Scoped slot (step 1).** The arena stays a materialized whole while a root is being
   consumed; only the drop-after-consume behavior changes. No consumer is modified.
2. **Demand-driven consumers (step 2).** `dynamics` linking and `stackir` lowering become
   traversals that request each node from an arena accessor, which replays the node's judgment
   query and materializes on demand. Editor facts already work this way
   (`normalized_type_at` and friends), so only the two backends need the conversion. The site
   index (`terms`/hints, part of L) is what lets the accessor map a typed id back to its
   judgment site.

## Alternatives Considered

- **Whole-database swap** (the `SessionPool` pattern). Already proven for tests, but it drops
  L along with S and pays for the standard library's sub-analyses again; retain it as the
  backstop, not the policy.
- **Shared arena behind `Arc<Mutex<_>>`.** Eliminates phase clones but breaks salsa's
  value-semantics snapshots: two database clones would observe each other's mutations.
- **Value hash-consing of the arena.** Measured 69.8% of the std types are content-unique, so
  deduplication saves at most ~30% of one table — not worth an interning layer over the query
  architecture.
- **Shape-based heuristics** ("certain types are worth keeping"). Rejected as a tiering rule:
  a type's value comes from being reachable from an L root, not from its shape. Canonicality
  is useful only as the hash-cons key for L payloads, should L itself need compression.
- **Construction/elimination rules as the criterion.** Correct in spirit but derivative:
  declaration forms are exactly the name-keyed entries; their *use sites* remain occurrence
  payload.

## Roadmap

1. Session-level drop-after-consume: `ProgramAnalysis` scopes the arena; tests assert that the
   S tables are empty once the root's consumer finishes. No consumer changes; suite green.
2. Demand-driven linking and lowering: replay-backed accessors for `values`/`compus`/`types`,
   then delete the wholesale materialization path for the backends.
3. Memo-layer policy: root-scoped LRU for the salsa database (after verifying what salsa 0.26
   supports for dropping individual inputs).

## Open Questions

- Can salsa 0.26 remove one root's inputs and memos without dropping the whole database? This
  decides whether the memo LRU lives inside one database or across generations of databases.
- Do `inlinables` / `value_aliases` / `package_aliases` have consumers outside the checker?
  If not, they only serve L-to-S replay and can merge into the site index.
- Which editor facts need S after the root is consumed? Hover reads annotations on demand, but
  the full-facts batch (semantic tokens, coverage) may require the traversal to complete before
  the slot drops.
