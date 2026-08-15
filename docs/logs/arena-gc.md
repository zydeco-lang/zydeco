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

## 2026-08-15 — round 5: clone only L, share S through every consumer

### Findings

- `analyze_source` still performed `(*statics).clone()` before
  `strip_occurrence_payload`. The strip made retention small, but the clone first copied all
  2.51M type nodes and their parallel tables. This was the single-check peak after the earlier
  retention rounds.
- Dynamics linking, Stack IR lowering, CLI backend retention, the TUI, cajun, and the session
  test pipeline only read `StaticsArena`. Their owned arena fields expressed an ownership
  requirement that the algorithms did not have.
- `types_normalized` had become a 71-entry delta, but `StaticsArena::reserve` still gave it the
  same capacity estimate as the 2.51M-entry `types_pre` table.

### Changes

- `StaticsArena::clone_keyed_indexes` constructs the L tier directly from the shared checked
  arena. `analyze_source` no longer clones S just to clear it.
- Full materializations and checked/executable programs now carry `Arc<StaticsArena>` all the
  way through dynamics, lowering, the CLI, the TUI, cajun, and test helpers. The obsolete
  owned-clone path was removed.
- The sparse normalized delta is no longer pre-reserved as if it were dense.

### Measurements

- Full-std minimal check: 2,497,757,184 -> 1,907,752,960 bytes peak RSS (-24%).
- Full-std dry run: 2,589,868,032 -> 1,949,368,320 bytes peak RSS (-25%).
- The focused analysis-retention and post-eviction fact tests remained green.

## 2026-08-15 — round 6: shrink the dominant enum instead of boxing every node

### Findings

- `Type` and `Fillable<Type>` were both 120 bytes. The largest variant was `PackPi`: its
  `PackTelescope` was 80 bytes because `im::Vector<AbstId>` alone occupied 64 bytes, even
  though package telescopes are usually singletons.
- Replacing the telescope tail with immutable shared slice storage makes `PackTelescope` 32
  bytes and `PackPi` 64 bytes. That drops the whole `Type` enum to 72 bytes without adding an
  allocation to each of the 2.51M nodes; only the comparatively rare telescope tail owns an
  `Arc<[AbstId]>`.

### Changes

- `PackTelescope::rest` now uses `Arc<[AbstId]>`. Its public construction, iteration,
  containment, length, and mapping behavior is unchanged.

### Measurements

- Full-std minimal check: 1,907,752,960 -> 1,442,791,424 bytes peak RSS (-24%).
- Full-std dry run: 1,949,368,320 -> 1,495,891,968 bytes peak RSS (-23%).
- The PackPi and Builtin-role integration targets passed (12 tests).

### Rejected experiments

- A per-substitution recursive cache avoided only 104 of 2,508,856 nodes (0.004%). Shared
  subgraphs inside one substitution are not the source of the expansion.
- A cross-judgment cache keyed by `(body, witness, argument)`, invalidated on hole-solution
  changes, avoided only 2,800 nodes (0.11%). The expansion is overwhelmingly distinct
  instantiation work, so the cache and its inference-state complexity were removed.
- Raising the pre-reservation factor from 16x to 40x increased peak RSS by 43MB and slowed the
  check. The final hash-table capacity is not the remaining explanation.

## 2026-08-15 — round 7: page the derived type-ID key space

### Findings

- The 2,508,856 `TypeId`s occupy 2,533,180 raw slots across 29,279 derived key spaces: 99.0%
  density. A hash table therefore repeats a 16-byte `(key_space, raw)` ID millions of times to
  represent data that is already almost a dense array inside each checking site.
- This property is specific to types. The other owning typed arenas are sparse within their
  key spaces: kinds 3.9%, type patterns 1.8%, value patterns 0.9%, values 0.7%, data 8.2%; they
  remain hash-backed.
- `Option<KindId>` occupied 24 bytes because zero was a valid `KeySpaceId`. Key-space identity
  has no meaningful zero value, so representing it as `NonZeroU64` exposes a Rust layout niche
  while preserving the 64-bit process/query identity.

### Changes

- Added `ArenaPaged` and `ArenaPagedAssoc`: an outer `FxHashMap` stores one vector per key
  space, and raw IDs index slots directly inside the vector. Missing mixed-category slots stay
  explicit as `None`; type pages are dense enough that this costs only 1%.
- `types_pre`, `annotations_type`, and checker-transient `env_type` use paged storage. The
  remaining sparse tables keep `ArenaSparse` / `ArenaAssoc`.
- `KeySpaceId` now wraps `NonZeroU64`, reducing `Option<KindId>` from 24 to 16 bytes. Outer page
  capacity is reserved for half the scoped-term count, matching the measured 43% key-space
  ratio rather than the 37x inner-node amplification.

### Measurements

- Paged type tables: 1,442,791,424 -> 1,086,537,728 bytes peak RSS; wall time 3.78s -> 2.73s.
- Nonzero key spaces and page-capacity tuning: 1,086,537,728 -> 1,056,161,792 bytes peak RSS.
- Whole round so far: 2,497,757,184 -> 1,056,161,792 bytes (-58%); the node count remains
  2,508,856, confirming that this round changes representation rather than semantics.

### Next

- The remaining per-root floor is genuine eager instantiation. Memoization experiments show
  that a lazy explicit-substitution representation, not another cache, is the next mechanism
  capable of reducing the 37x node count.
- Test-process concurrency and duplicate backend checks remain independent peak multipliers;
  revisit them after the single-root representation is stable and fully tested.

## 2026-08-15 — round 8: share phase products instead of cloning query results

### Findings

- A retained-heap census after round 7 separated the remaining memory into 336MB of type-page
  vectors, 281MB of hash-table storage, and about 60MB of persistent `TyEnv` HAMT nodes. The
  exact allocation histories showed that the largest hash tables were whole syntax and span
  arenas cloned at Salsa query boundaries, rather than checker lookup tables.
- `check_source` must clone the resolved arena before elaboration because judgments still read
  the immutable `ScopedData` input. Its *result* does not need another clone: the check memo,
  analysis, executable materialization, and dynamics linker all treat the elaborated scoped
  arena as immutable.
- `resolved_data` similarly copied the merged `SpanArena` into `ScopedData`, kept another copy
  in its tuple result, and copied it again into downstream results. A span arena is immutable
  after resolution, so one shared allocation is the correct ownership model.
- `term_envs` had no reader. Round 4's per-term normalized facts answered every current query,
  so retaining environments as a foundation for hypothetical inner-node replay kept a large
  persistent graph for an API that did not exist.

### Changes

- `ScopedData` owns shared span and resolved arenas. `TyckOutput`, `ProgramAnalysis`, checked and
  executable programs, and dynamics linkers carry `Arc` handles to the same phase products.
  Backend lowering makes an owned scoped copy only at the point where it actually inserts
  generated lowering definitions.
- Removed `term_envs` and its per-term writes. The checker still interns the environments needed
  by live type nodes; it no longer retains an unused per-source-term snapshot.

### Measurements

- Removing the dead environment snapshots changed peak RSS from 1,056,161,792 to
  1,050,476,544 bytes. Sharing phase products brought three repeated runs to 984,006,656,
  984,481,792, and 985,907,200 bytes (median 984,481,792; -6.8% for the round).
- Focused statics suites passed (33 tests). The session library suite passed all 151 tests with
  two test threads in 40.59s.

### Next

- The immutable resolved input and elaborated scoped output are still two full arenas. The
  checker only adds generated definitions, so a small elaboration overlay may replace that
  remaining whole-arena clone.
- Type pages remain the largest retained category. Measure their logical length versus capacity
  before deciding between exact compaction and a combined page representation.

## 2026-08-15 — round 9: isolate typed elaboration's definition delta

### Findings

- A direct mutation search initially suggested that checking never changed `ScopedArena`. The
  remaining writes were hidden behind the generic `Alloc<_, DefId>` implementation: typed
  construction creates fresh binder names and inserted them into `scoped.defs`.
- Those synthesized names are the only scoped mutation in production type checking. Patterns,
  terms, provenance, use maps, contexts, cocontexts, and block DAGs are all immutable checker
  inputs. Cloning all of them to make a small definition-name delta was an ownership mismatch.
- Dynamic linking and backend lowering only consume definition names from the scoped result.
  Source diagnostics still need the complete resolved arena, but they need its unchanged source
  form and can share the query input directly.

### Changes

- Generated definition names now live in `StaticsArena::generated_defs`. Checker and static
  formatter name lookup jointly view source definitions and that typed-elaboration delta.
- `Tycker` borrows `ScopedArena` immutably, and `check_source` returns the same shared resolved
  arena it received instead of cloning it before checking.
- Dynamics clones and merges only the source and generated definition tables. Stack IR and
  backend lowering start from a names-only scoped arena, then add their own generated names;
  they no longer clone source terms or context analyses.

### Measurements

- Three full-std minimal checks used 941,785,088, 945,946,624, and 948,420,608 bytes peak RSS
  (median 945,946,624), down 3.9% from round 8's 984,481,792-byte median.
- The current-tree baseline for this work is now down from 2,497,757,184 to 945,946,624 bytes
  (-62.1%). Focused statics suites passed all 33 tests; the 151-test session suite passed with
  two threads in 40.15s, including interpretation and backend-lowering coverage.

### Next

- Re-census the retained heap now that syntax clones no longer obscure the type-page footprint.
  In particular, distinguish live slot bytes from geometric `Vec` capacity and allocator
  high-water behavior.
