# Arena Reclamation Worklog

Companion to [docs/ideas/arena-gc.md](../ideas/arena-gc.md), which holds the design and
the classification tables. This log records attempts, observations, measurements, and
decisions while implementing the plan.

## Background Measurements (before this log)

- One fresh std check (`std/std.zy`, 65K scoped terms) materializes 2.06M `types_pre`
  nodes: 46% `App`, 25% `Arrow`, 13% `Label`, 11% `Prod`; 69.8% of node contents are
  unique, so value hash-consing is not worth an interning layer.
- Peak RSS for that check: 7.5GB initially; 2.42GB after the phase merge (`5b6f79cd`),
  table pre-reservation (`c485a16b`), and the normalized delta (`2680cb7d`).
- A long-lived session retains every root's arena: the test suite's shared session
  reached ~29GB before `SessionPool` capped it; the editor session has the same shape.
- salsa 0.26 stores everything per database (`Arc<Zalsa>`, freed on drop) and its
  `evict_lru` only resets revisions — no per-query eviction exists.

## 2026-08-15 — plan and step 1: L/S split with a recompute interface

### Plan

Implement the roadmap from the design note in two moves:

1. `check_source` keeps returning the full arena for now, but the session retains only
   the L tier (keyed indexes) and hands the S tier (occurrence payload) to consumers
   through a scoped, transient materialization interface. This is the drop-after-consume
   behavior without changing any consumer's traversal logic.
2. Follow-up (later log entries): strip S from the memoized check output itself, make
   the linkers demand-driven over judgment replay, and add the memo-layer LRU.

Assumptions per the current direction: nothing needs to survive the type-checking phase
except the L tier; coverage checking stays inside the check; semantic tokens are out of
scope for now.

### Observations

- `check_source`'s memoized `TyckOutput` owns the whole arena, so the salsa database
  retains S for as long as the root's inputs live, no matter what the session keeps.
  Step 2's strip is therefore what actually frees memory; step 1 only bounds the
  session's own copies.
- The judgment queries are the recompute source: every S node's id derives from its
  site via `KeySpaceId::derive(tag, entity_space, entity_raw, occurrence)` and its
  content is the judgment query's value, so a transient materializer can replay them.
- Consumers (dynamics link, stackir lower) traverse the typed tree by ids; they read
  the arena through `Index` and `get`, so routing them through a materializer view
  should be mechanical once the view exists.

### Decisions

- Materialization runs outside the salsa memo: the materialized arena is owned by the
  caller and dropped when consumption finishes, so its retention is exactly the
  consumer's lifetime.
- The checker and the materializer share one walk: the checker already walks the scoped
  program in canonical order, so the materializer reuses the same driver rather than a
  second traversal.
