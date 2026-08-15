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

## 2026-08-15 — round 1: salsa LRU on the check memo, then the L/S split

### Findings

- salsa 0.26 supports `lru = <usize>` per tracked function (via the `Lru` eviction
  policy); eviction fires in `reset_for_new_revision`, reachable without an input write
  through `Database::trigger_lru_eviction`. Creating new salsa inputs does *not* bump the
  revision, so multi-root sessions must trigger eviction explicitly.
- `lru = 1` on `check_source` plus a per-analyze trigger dropped the session suite peak
  from 18.0GB to 9.93GB; raising the pool cap to 64 grew it to 16.3GB, i.e. per-root
  judgment memos cost ~115MB and still need the pool cap. The entry-counted LRU cannot
  express root-scoped eviction for the fine-grained judgment queries (one root has
  millions of entries per query), so the pool generation remains their policy.
- A plain re-derivation of the pipeline does *not* hit the check memo: `ScopedData::new`
  creates a fresh tracked identity every call. Sharing the memo requires a tracked
  `resolved_data(db, root)` query so every consumer of one root holds the same
  `ScopedData` and therefore the same `check_source` memo entry.

### Changes

- `check_source` memoizes with `lru = 1`; the test pool triggers eviction per analysis.
- `StaticsArena::strip_occurrence_payload` clears the occurrence payload
  (`types_pre`/`kinds_pre`/`values`/`compus`/patterns/annotations/normalized) and keeps
  the keyed indexes.
- `analyze_source` strips before constructing `ProgramAnalysis`; the analysis now
  retains only the L tier. `CompilerSession::materialize_arena` / `checked_program` /
  `executable_program` re-materialize on demand from the memoized check; the three
  S-reading fact queries (`normalized_type_at`, `coverage_facts`,
  `term_annotation_at`) re-derive through `rechecked`.
- Consumers migrated: the CLI, the REPL engine, cajun, and the integration tests now
  obtain the typed arena from the session. cajun's `ProjectState` materializes once at
  load and holds the arena for the project's lifetime — the project is a live consumer
  of its root, which the design permits.

### Measurements

- Session suite (2 threads): 149 passed; peak RSS 9.93GB after the LRU step, 6.32GB
  after the L/S split; 72s wall. Full workspace suite: 739 passed, 0 failed.

## 2026-08-15 — round 2: shared-arena outcome, per-fact reads without cloning

### Findings

- `check_source`'s `returns(clone)` semantics handed every reader a deep copy of the
  arena, so the three S-reading fact queries paid a transient ~500MB clone per new key.
- Wrapping the outcome's arena in an `Arc` moves that cost: the memo and every reader
  share one arena; read-only consumers (the fact queries) read through the `Arc` in
  O(1), and only consumers that mutate during lowering (`checked_program`,
  `executable_program`, cajun's project) clone the arena out explicitly.
- `analyze_source` still clones before stripping: mutating the shared arena would
  corrupt the memo, and the stripped L tier is what the analysis owns.

### Changes

- `CheckedSource`/`RejectedSource` carry `Arc<StaticsArena>`;
  `SourceCheckOutcome::statics_arc` is the cheap shared read, `into_statics` clones for
  owned consumers.
- The fact queries read through `statics_arc` — hover no longer materializes the arena.

### Measurements

- Session suite (2 threads): 150 passed; peak RSS 6.33GB; 74s wall. Full workspace
  suite: 740 passed, 0 failed.

## 2026-08-15 — round 3: per-term facts become keyed indexes

### Findings

- Two of the three S-reading fact queries are per-*term*, not per-*node*: their results
  are bounded by the source size (65K entries), so they belong to L as recorded judgment
  results rather than as replay logic. The third (`normalized_type_at`) is per-node, but
  its callers only ever pass top annotation types, which are per-term.
- The site of any typed id is already recoverable from the L-tier `terms` bipartite
  for term tops; inner nodes of an annotation are not, so they need either a site index
  or an explicit contract.
- The normalization environment is what replay lacks: the checker snapshots it per term
  (`self.info`), so recording per-term env snapshots in L makes future re-normalization
  exact rather than approximate.

### Changes

- `StaticsArena` gains three L-tier tables: `term_anns` (final `TermAnnId` per scoped
  term), `term_envs` (interned env snapshot per scoped term), and `coverage_errors`
  (the finish-phase coverage failures).
- `term_annotation_at` and `coverage_facts` now read these tables from the stripped
  analysis; they no longer touch the occurrence payload or the check memo.

### Measurements

- Session suite (2 threads): 150 passed; peak RSS 6.38GB; 77s wall. Full workspace
  suite: 740 passed, 0 failed. L-tier growth per root is in the low megabytes.

## 2026-08-15 — round 4: normalized types as per-term keyed indexes

### Findings

- `normalized_type_at`'s only callers pass top annotation types. A per-term normalized
  table therefore answers the actual demand exactly, and the per-node replay machinery
  (recursive `normalize_type` query + type-synthesis dispatch) would be speculative
  complexity with no consumer. This deviates from the original replay plan in favor of
  the L/S criterion the plan itself established: facts bounded by the source's term
  count are keyed indexes.
- An annotation type of a computation/value term is *not* the term's `forth` entry (that
  holds the `CompuId`/`ValueId`), so the term lookup needs a dedicated top-type index.
  Inner type nodes are deliberately left without an entry: a lookup for them answers
  nothing rather than guessing with the top type's form.

### Changes

- `StaticsArena` gains `term_norms` (normalized annotation type per scoped term) and
  `type_sites` (top annotation `TypeId` -> scoped term, recorded at the term dispatch).
- `normalize_and_validate_k` records `term_norms` after normalization; the checker
  records `type_sites` next to `term_anns`.
- `normalized_type_at` reads `type_sites` then `term_norms` from the stripped
  analysis: all three fact queries now answer entirely from L and survive arena-memo
  eviction, covered by `facts_survive_arena_memo_eviction`.

### Measurements

- Session suite (2 threads): 151 passed; peak RSS 6.54GB; 74s wall. Full workspace
  suite: 741 passed, 0 failed.

### Next

- Per-node replay (recursive `normalize_type` query over the judgment layer, keyed by
  `type_sites`-style site records and `term_envs`) remains available as the path for a
  future consumer that needs arbitrary inner nodes; the foundation tables are in place.
- Judgment-memo root-scoped policy beyond the pool generation remains open.
