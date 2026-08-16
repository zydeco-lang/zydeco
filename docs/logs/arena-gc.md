# Arena Reclamation Worklog

Companion to [docs/proposals/arena-gc.md](../proposals/arena-gc.md), which holds the design and
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

## 2026-08-15 — round 10: bound paged-arena growth slack

### Findings

- The two final type tables each contained 2,533,180 logical slots across 29,279 pages, but
  ordinary `Vec` doubling left capacity for 3,906,339 slots. The 1,373,159 unused slots were
  54.2% of the live shape, repeated once for types and once for annotations. A checker-transient
  environment table follows the same page shape while it is being built.
- The excess came from applying a growth rule designed for a single long-lived vector to tens
  of thousands of independently growing, append-dense pages. The pages still need amortized
  growth, but a smaller factor bounds their aggregate tail slack.
- Allocator high-water behavior hides much of the retained-byte reduction from process peak
  RSS: page reallocations temporarily keep both buffers live, and freed size classes remain in
  the allocator. A retained-heap census nevertheless fell from 762.5MB to 567.2MB across the
  arena-sharing and page-growth rounds, while the external RSS measurement continued to improve.

### Changes

- Paged owning and associative arenas now share a 1.5x exact-reservation policy. It preserves
  logarithmic amortized growth while avoiding `Vec`'s 2x capacity step for every derived-ID
  page.

### Measurements

- Final capacity per type table fell from 3,906,339 to 3,087,852 slots: overhead is 21.9% rather
  than 54.2%, saving capacity for 818,487 slots in each of the two retained tables.
- Three full-std minimal checks used 935,198,720, 936,329,216, and 935,968,768 bytes peak RSS
  (median 935,968,768), down 10.0MB from round 9's 945,946,624-byte median. Warm user time also
  fell from roughly 1.40s to 1.18s.
- A 1.25x policy retained only 14.0% slack but raised median peak RSS to 940,326,912 bytes because
  of additional reallocation churn. A 1.75x policy raised it to 950,845,440 bytes because of
  unused capacity. Both experiments were removed; 1.5x was the measured optimum.
- All 14 focused arena tests and all 33 statics tests passed. The 151-test session library suite
  passed with two threads in 37.66s.

### Next

- The post-round retained heap attributes another large block to `clone_keyed_indexes`: the S
  and L tiers simultaneously own deep copies of normalized terms, annotations, and source-site
  indexes. These facts are immutable after checking, so copy-on-write sharing should remove the
  duplication without weakening post-eviction analysis.

## 2026-08-15 — round 11: represent keyed facts as one shared generation

### Findings

- `clone_keyed_indexes` avoided copying the typed occurrence tree, but still deep-cloned every
  L-tier table while the check memo retained the originals. A retained-heap trace attributed
  26.1MB of small allocations directly to that clone, plus 11.3MB for the `term_norms` hash
  table, 7.3MB for `term_anns`, and nested allocations owned by their values and provenance
  vectors.
- The common rule is lifecycle rather than table shape: every field selected by
  `clone_keyed_indexes` is mutable while checking and immutable once the result is published.
  They form one source-bounded generation. Treating each table as an independent owned value
  obscured that invariant and made a deep copy appear necessary.

### Changes

- Added `StaticsIndexes`, which owns the complete L generation. `StaticsArena` holds it behind
  one `Arc`, exposes its fields through `Deref`, and uses `Arc::make_mut` through `DerefMut`
  during construction. The checker's unique handle therefore mutates in place; a hypothetical
  write after sharing preserves value semantics with copy-on-write.
- `clone_keyed_indexes` now shares that generation in O(1) and default-constructs only the empty
  occurrence payload. A focused test asserts that retained and materialized views share the
  same generation while the retained S tables remain empty.

### Measurements

- Three full-std minimal checks used 915,406,848, 914,587,648, and 913,965,056 bytes peak RSS
  (median 914,587,648), down 21.4MB from round 10's 935,968,768-byte median. Warm user time was
  1.21–1.24s.
- Sharing only the five largest fields first produced a 914,391,040-byte median. Sharing the
  rest as one generation was within measurement noise, but removes the whole class of
  ownership duplication and prevents smaller L tables from accumulating alongside a live memo.
- The current-tree baseline is now down from 2,497,757,184 to 914,587,648 bytes (-63.4%).
- All 34 statics tests passed. The 151-test session library suite passed with two threads in
  38.42s, including the analysis-retention and post-eviction rematerialization tests.

### Next

- With representational duplication removed, the dominant remaining allocation is the 2.51M
  genuinely distinct eager type-instantiation nodes. Investigate an explicit-substitution
  representation that can defer recursive rewriting; the two memoization experiments in round
  6 show that another result cache cannot collapse this work.

## 2026-08-15 — round 12: indirect only the rare wide type variants

### Findings

- `Type` and `Fillable<Type>` were 72 bytes because `PackPi`, `ValuePackPi`, and `Exists` each
  carried a 64-byte payload inline. The other 99.29% of full-std nodes paid that enum-wide size
  even though applications, arrows, labels, and products need far less payload.
- A full-std census found 17,663 `Exists`, 62 `ValuePackPi`, and one `PackPi` node among
  2,508,856 types (0.707% combined). Indirection therefore adds very few allocations while
  removing 16 bytes from every slot in the dominant arena.

### Changes

- The three rare 64-byte variants now own boxed payloads. Direct `From<Exists>`,
  `From<ValuePackPi>`, and `From<PackPi>` conversions keep construction typed and centralize the
  indirection; consumers explicitly unwrap only when they take ownership of a signature.
- A layout regression test caps both `Type` and `Fillable<Type>` at 56 bytes, protecting the
  arena-wide consequence rather than relying on the current field arrangement by accident.

### Measurements

- `Type` and `Fillable<Type>` both fell from 72 to 56 bytes. At the measured 3,087,852-slot
  final page capacity, that predicts 49,405,632 saved bytes in `types_pre` alone.
- Three full-std minimal checks used 865,107,968, 865,058,816, and 864,747,520 bytes peak RSS
  (median 865,058,816), down 49,528,832 bytes from round 11's median — within 0.25% of the slot
  calculation. Warm user time was 1.14–1.18s.
- The current-tree baseline is now down from 2,497,757,184 to 865,058,816 bytes (-65.4%).
- All 35 statics tests passed. The 151-test session library suite passed with two threads in
  36.66s; the release CLI/TUI build and a focused cajun check also passed.

### Next

- The close agreement between predicted slot bytes and measured RSS confirms that the
  remaining arena is live data rather than allocator noise. Continue with substitution-call
  attribution, then prototype a compact suspended substitution only at the high-amplification
  boundary rather than adding a lazy case to every type operation speculatively.

## 2026-08-15 — round 13: attribute eager substitution by caller

### Findings

- A temporary call-site profiler counted 2,503,991 ordinary `TypeId` allocations during the
  full-std check; the final 2,508,856 total includes another 4,865 query-produced insertions.
  This closes the accounting gap: the remaining node count is created during checking, not by
  post-check normalization, arena cloning, or retained analysis.
- `FieldProjectionResolver::product_components_k` attributed 798,317 allocations to 8,075 root
  substitutions, while `value_candidates_k` attributed 413,066 to 16,441 roots. Together these
  two structural field-search loops create 1,211,383 nodes, 48.4% of ordinary type allocation.
- Analytic term annotations contributed 361,349 nodes. Existential projection opening
  contributed 210,694 environment-substitution nodes plus 165,861 abstract-substitution nodes;
  the analogous tuple/package traversal contributed another 121,600 plus 116,581.
- The largest individual substitutions created roughly 4,000–5,000 nodes. In the existential
  loop, one iteration recursively substitutes the complete remaining package body, opens one
  binder, recursively substitutes that binder's witness through the same remainder, and then
  repeats on the tail. Field search similarly substitutes a whole product before recursing into
  its already-rebuilt components. A source-linear telescope therefore materializes a quadratic
  sequence of distinct intermediate trees.
- Simply skipping the repeated calls is unsound. Environment substitution is intentionally not
  idempotent: replacing a variable returns the mapped type without recursively applying the
  same environment to that replacement, and each opened existential extends the pending
  abstract assignment. The next traversal step can therefore expose real additional work.

### Decision

- The first lazy prototype should be a local substitution closure for structural package and
  field traversal, not a new suspended variant in the global `Type` enum. The closure will carry
  a root `TypeId`, its environment, and pending abstract assignments; revealing the outer shape
  should force only variables, applications, projections, and sealed heads. Children retain the
  closure, and only a selected field or final result is materialized.
- This boundary directly covers the measured quadratic callers while leaving unification,
  normalization, formatting, linking, and every ordinary type match unchanged. If the local
  closure proves the node reduction and its invariants, the same representation can later move
  into the arena as an explicit-substitution node.

### Next

- Specify the closure's shadowing, chained-variable, abstract-witness, and hole behavior with
  focused tests. Then migrate one field-projection traversal and re-run the allocation census
  before widening the mechanism to existential and PackPi traversal.

## 2026-08-15 — round 14: distribute field substitution without rebuilding the telescope

### Findings

- An exact-semantics closure deferred field-search substitutions successfully, but materializing
  every selected route entry independently lost the sharing that eager traversal had created.
  The full-std check grew from 2,508,856 to 2,582,584 type nodes (+73,728), and three runs used
  875,085,824, 875,905,024, and 875,036,672 bytes peak RSS (median 875,085,824). That experiment
  was removed.
- The repeated eager algorithm conflated two operations. An inspected node needs `unroll` before
  its environment substitution, while a child reached through a stable label or product only
  inherits the parent's recursive substitution. Repeating both operations at every transparent
  depth made substitution results depend on how many labels surrounded a field.
- A first single-distribution prototype assigned `unroll` to inherited children too. It reached
  only 455,391 nodes before failing the standard-library check: the projected `IoErrorKind`
  payload changed from its sealed `Abst` identifier to the underlying `Data` identifier. This
  established that preserving the selected payload's representation is part of field lookup's
  contract even when the two types can later be unrolled for comparison.
- The remaining boundary is explicit: recursively *searching through* a sealed payload must
  start a fresh `unroll`-then-substitute step, but merely selecting that payload must not. A
  materialized node therefore retains the environment as search context without carrying
  another pending rewrite.

### Changes

- `DeferredEnvType` now gives field lookup three typed states: pending
  `UnrollThenSubstitute`, pending inherited `Substitute`, or materialized. Stable labels and
  products distribute the inherited state through their children without allocating. Variables,
  applications, projections, fills, and sealed heads establish materialization boundaries.
- Candidate discovery retains closures for the route and materializes only the unique result.
  Missing and duplicate-field searches no longer retain eagerly rewritten copies of every
  explored branch.
- Cycle detection keys both the root identifier and pending operation, so revisiting one graph
  node under a genuinely different deferred state does not look like a structural cycle.
- A chained-environment regression (`x -> y`, `y -> Unit`) proves that selecting the same field
  through one or two transparent labels returns `y` in both cases. A seal regression proves that
  direct selection preserves the abstract identifier while nested search can still unroll the
  seal and find a field inside its definition.

### Measurements

- The full-std check now retains 1,586,411 type nodes, 922,445 fewer than round 13 (-36.8%).
- Three release checks used 615,890,944, 614,039,552, and 614,989,824 bytes peak RSS (median
  614,989,824), down 250,068,992 bytes (-28.9%) from round 12's 865,058,816-byte median. Warm wall
  time fell from roughly 1.14–1.18s to 0.87s.
- The current-tree baseline is now down from 2,497,757,184 to 614,989,824 bytes (-75.4%).
- All 37 statics tests and all 151 session tests passed, including standard-library composition,
  interpretation, and backend-lowering coverage.

### Next

- Re-run call-site attribution against the smaller arena. The existential opening and
  tuple/package loops previously contributed about 615,000 environment and abstract-substitution
  nodes; they have the same “inspect one constructor, rebuild the remainder” shape and are the
  next candidates for a local closure carrying ordered abstract assignments.
- Measure route materialization separately. Product annotations still require several related
  views of the selected route, so a small closure-local materialization cache may preserve more
  sharing without changing the representation boundary established here.

## 2026-08-15 — round 15: compose monotonic telescope environments

### Findings

- Re-running call-site attribution after round 14 found 361,349 nodes in analytic term
  annotations, 288,938 in the remaining field-closure materializations, 210,694 environment and
  165,861 abstract-substitution nodes in existential projection opening, and another 238,181
  combined nodes in tuple/package traversal. The temporary profiler was removed after the
  census.
- An exact lazy telescope closure retained every environment snapshot and abstract assignment,
  then replayed them when the final package body was forced. The standard library passed, but
  forcing still created 209,999 environment and 165,374 abstract-substitution nodes. Deferral
  moved the work without composing it.
- The typing environment has a stronger invariant than the abstract assignments: opening a
  telescope only extends it. Each later snapshot contains every earlier definition mapping and
  the accumulated skolem scope. Applying all snapshots makes the number of substitutions depend
  on telescope depth, just as transparent label depth did before round 14.
- Abstract assignments cannot be discarded by the same rule. Their order is meaningful because
  a later payload can mention an earlier witness. They remain explicit until a fused traversal
  can apply the ordered list without rebuilding the type once per assignment.

### Changes

- `DeferredTelescopeType` carries one final environment, one outer-unroll bit, and an ordered
  persistent vector of abstract assignments. Descending into `ManifestKind` or `Exists` shares
  that state; replacing the environment snapshot is O(1).
- Existential projection opening now reveals telescope constructors without materializing their
  remaining bodies. Manifest type definitions are forced at their use site, and the final value
  body is forced once after the telescope has been opened.
- Materialization applies the ordered abstract assignments and then the newest environment once.
  A focused chained-environment regression proves that an older `x -> y` snapshot followed by a
  final environment containing both `x -> y` and `y -> Unit` yields `y`, rather than resolving an
  extra link merely because another telescope entry was traversed.

### Measurements

- The full-std check retains 1,395,567 type nodes, 190,844 fewer than round 14 (-12.0%). The
  existential environment caller fell from 210,694 to 20,337 nodes; the still-ordered abstract
  assignments account for 165,374 nodes.
- Three release checks used 594,739,200, 594,853,888, and 594,608,128 bytes peak RSS (median
  594,739,200), down 20,250,624 bytes (-3.3%) from round 14. Warm wall time was 0.84–0.85s.
- The current-tree baseline is now down from 2,497,757,184 to 594,739,200 bytes (-76.2%).
- All 38 statics tests and all 151 session tests passed, including full standard-library,
  interpreter, and backend-lowering coverage.

### Next

- Fuse the ordered abstract assignments into one recursive traversal. Replacement nodes must
  receive only the assignments that occur after their own assignment; PackPi witness binders
  must continue to shadow matching assignments.
- Reuse the composed telescope closure in tuple/package traversal, whose 238,181 nodes have the
  same constructor-by-constructor shape.
- Analytic term annotations are now the largest single caller at 361,349 nodes. Addressing them
  likely requires carrying a deferred expected type through `Action<AnnId>`, so keep that broader
  transition behind the two local telescope optimizations.

## 2026-08-15 — round 16: fuse ordered abstract substitutions

### Findings

- Round 15 still allocated 165,374 nodes while replaying 528 abstract substitutions over
  existential telescope bodies. Applying each assignment separately rebuilt the same remainder
  once per open binder, even though the ordered composition can be performed in one traversal.
- The composition rule is directional: when an abstract witness is replaced, its payload must
  receive only the assignments after that witness's assignment. For example, applying
  `a -> b` followed by `b -> Unit` to `a` produces `Unit`.
- A backward-resolved simultaneous map looked attractive and passed the complete standard
  library, retaining 1,252,221 type nodes with a 569,933,824-byte median peak RSS. It was
  nevertheless unsound under lexical shadowing. Given `free -> bound; bound -> replacement`, a
  `PackPi` binding `bound` must insert the locally bound identifier for `free`; resolving the map
  globally rewrote that identifier before the binder could hide the second assignment. The map
  prototype was removed.
- The safe fused representation is therefore the ordered assignment sequence itself. Each
  abstract occurrence selects its first matching assignment and applies only the suffix to the
  payload. `PackPi` and `ValuePackPi` filter their bound witnesses from the sequence before it
  reaches either a body occurrence or a replacement introduced inside that body.

### Changes

- `TypeId::subst_absts` applies an ordered slice of abstract assignments in one structural
  traversal; the existing single-assignment operation delegates to this primitive.
- Package-arrow binders filter shadowed assignments while traversing their codomains. A focused
  regression covers the case that defeated global map precomposition, and the telescope test
  covers transitive ordered replacement.
- `DeferredTelescopeType` now collects its persistent assignment list once and forces the final
  body with the fused operation before applying its composed environment.

### Measurements

- The full-std check retains 1,251,874 type nodes, 143,693 fewer than round 15 (-10.3%). The
  shadowing-safe traversal costs only 347 nodes more than the rejected simultaneous map.
- Three release checks used 570,900,480, 570,474,496, and 570,294,272 bytes peak RSS (median
  570,474,496), down 24,264,704 bytes (-4.1%) from round 15. Warm wall time was 0.78–0.79s.
- The current-tree baseline is now down from 2,497,757,184 to 570,474,496 bytes (-77.2%).
- All 40 statics tests and all 151 session tests passed, including standard-library composition
  and the new ordered-substitution and binder-shadowing regressions.

### Next

- Reuse `DeferredTelescopeType` in tuple/package traversal. Its previous 238,181 attributed nodes
  came from the same repeated environment-plus-abstract opening shape.
- Re-run call-site attribution after that migration. Analytic term annotations (previously
  361,349 nodes) and remaining field materialization (288,938 nodes) are then the largest known
  boundaries, but their exact shares should be measured against the smaller arena.

## 2026-08-15 — round 17: share telescope state in package construction

### Findings

- Tuple syntax serves two roles during analysis: an ordinary product after the static prefix, and
  a package constructor or pattern while the expected type exposes `ManifestKind` and `Exists`
  entries. Both the term and pattern paths had their own eager telescope loop, separate from the
  existential-projection loop optimized in rounds 15 and 16.
- Each static tuple item substituted the environment through the complete remaining telescope,
  substituted its witness through that rewritten tail, and then repeated the process for the
  next item. This was the same quadratic construction rule under a third surface syntax.
- The term path has one fixed environment. The pattern path grows its environment with checked
  pattern bindings and fresh skolems, so it must replace the closure's environment with the
  newest snapshot before revealing each constructor and before forcing the final body.
- The previous round-15 attribution assigned 238,181 nodes directly to these loops. The final
  arena fell by 292,719 nodes after migration; the older caller-local number excluded downstream
  rebuilding induced by the eagerly materialized intermediate types and preceded round 16, so it
  was a lower bound rather than an additive prediction for the newer tree.

### Changes

- Package term construction now carries `DeferredTelescopeType` while checking static tuple
  items. Manifest definitions are forced only for their equality check, abstract payloads are
  appended to the ordered assignment sequence, and the package value body is materialized once.
- Package pattern checking uses the same representation while retaining its essential sequential
  state: checked pattern environments, opened skolems, and source-order static patterns. It forces
  the remaining package body under the final environment before checking the value-pattern tail.
- The eager `map_while` term loop became an explicit stateful traversal. The state transition is
  now shared with projection opening instead of being encoded as repeated recursive substitution.

### Measurements

- The full-std check retains 959,155 type nodes, 292,719 fewer than round 16 (-23.4%).
- Three clean release checks used 528,515,072, 527,646,720, and 527,761,408 bytes peak RSS (median
  527,761,408), down 42,713,088 bytes (-7.5%) from round 16. Warm wall time was 0.62–0.63s and warm
  user time was 0.56–0.57s.
- The current-tree baseline is now down from 2,497,757,184 to 527,761,408 bytes (-78.9%).
- All 40 statics tests and all 151 session tests passed. The release CLI build and full standard
  library check also passed after removing the temporary node counter.

### Next

- Re-run caller attribution against the 959,155-node arena. The prior 361,349-node analytic-term
  annotation and 288,938-node field-materialization counts can no longer be treated as current,
  because their inputs and downstream shapes changed with telescope composition.
- Inspect the other recursive package consumers (`PackageInstantiation`, `PackPiWitnessSkolems`,
  and `PackPiPatternAssignments`). They open a domain one entry at a time and still perform eager
  environment and abstract substitution, though their call frequencies may be much lower.

## 2026-08-15 — round 18: preserve prepared analytic annotations

### Findings

- A fresh root-operation profiler attributed 954,482 checker-created type nodes in the
  959,155-node arena. Eager preparation of analytic term annotations dominated at 360,268 nodes;
  field materialization followed at 262,556, product revelation at 74,343, type-application
  normalization at 56,080, and type-abstraction recovery at 44,883. The temporary profiler was
  removed after the census.
- Every analytic term entry substituted and normalized its expected type before inspecting the
  term syntax. Transparent nodes—including metadata, source/signature boundaries, annotation
  holes, residuals, blocks, and definition tails—then forwarded the resulting `TypeId` as a new
  action. The child could not distinguish that prepared type from a fresh expectation and applied
  the same environment again.
- This was more than duplicated work. Environment substitution deliberately replaces a variable
  without recursively rewriting the replacement, so applying it once per transparent syntax node
  made a chained mapping advance according to source-tree depth. The field-label regression in
  round 14 exposed the same general bug: substitution count must follow a semantic boundary, not
  the number of representation wrappers crossed.

### Changes

- Analytic `Action` now carries the environment under which its expected type was prepared.
  Term entry substitutes and normalizes only when the current environment differs.
- Direct forwarding preserves that provenance. When a binder or definition produces a genuinely
  different environment, structural `TyEnv` comparison detects the change and performs the
  required new preparation; persistent clones take the pointer-equality fast path.
- The old generic `Action::switch` constructor was removed. Callers now choose between a fresh
  analytic action and explicit forwarding, making the preparation boundary visible in code.

### Measurements

- The full-std check retains 791,634 type nodes, 167,521 fewer than round 17 (-17.5%).
- Three clean release checks used 438,779,904, 439,107,584, and 439,353,344 bytes peak RSS (median
  439,107,584), down 88,653,824 bytes (-16.8%) from round 17. Warm wall time was 0.53–0.55s and
  warm user time was 0.49–0.50s.
- The current-tree baseline is now down from 2,497,757,184 to 439,107,584 bytes (-82.4%).
- All 40 statics tests and all 151 session tests passed, along with the release CLI build and full
  standard-library check.

### Next

- Extend preparation provenance to expected-type components extracted by a typing rule. Those
  children are already rewritten under the current environment, but `Action::ana` currently marks
  them fresh and can repeat the traversal.
- Re-profile before changing the field closure. Its 262,556-node share is now relative to the
  pre-round-18 arena, and eliminating analytic duplication may change how often field routes are
  materialized.

## 2026-08-15 — round 19: distribute prepared product annotations

### Findings

- Round 18 preserved preparation through wrappers, but ordinary tuple checking immediately called
  `reveal_or_refine_product_k`, which unrolled and recursively substituted the already prepared
  expected product again. Every subsequent component called `view_product_k` on the tail and then
  entered its child with a fresh analytic action. A product aligned with tuple syntax therefore
  still advanced environment substitution once per item.
- The pre-round-18 profile assigned 74,343 nodes directly to product revelation and 8,405 to the
  analogous named-value reveal. Their downstream children were counted under the common analytic
  boundary, explaining why removing the full chain saves more than those two direct totals.
- Skipping substitution unconditionally would lose the round-14 representation boundary. A direct
  prepared `Prod` or `Label` needs no work, while unrolling a sealed or projected head can expose a
  definition that has never received the current environment.

### Changes

- `reveal_or_refine_prepared_product_k` and `view_prepared_product_k` inspect direct product
  structure without recursively rewriting it. If `unroll` returns a different root, they apply the
  environment to that newly exposed representation once before refining or destructuring it.
- Tuple terms use prepared product views for the outer product, each item, and the final tail.
  `Action::ana_prepared` transfers the current preparation provenance to those child checks.
- Package tuples use the same prepared views after their static telescope prefix has been forced.
  Named value terms similarly avoid a second substitution for a direct prepared label while still
  re-entering the environment when unrolling changes the root.
- A focused regression proves both sides with a chained environment: direct prepared product
  components remain at the first replacement, while a product newly exposed through a seal receives
  that replacement exactly once.

### Measurements

- The full-std check retains 693,735 type nodes, 97,899 fewer than round 18 (-12.4%).
- Three clean release checks used 423,444,480, 424,607,744, and 424,198,144 bytes peak RSS (median
  424,198,144), down 14,909,440 bytes (-3.4%) from round 18. Warm wall time was 0.50s and warm user
  time was 0.46–0.47s.
- The current-tree baseline is now down from 2,497,757,184 to 424,198,144 bytes (-83.0%).
- All 41 statics tests and all 151 session tests passed, along with the release CLI build and full
  standard-library check.

### Next

- Carry preparation provenance through other expected-type destructors, especially arrows,
  thunks/returns, and codata arms. Their child environments need an explicit rule: same-environment
  components are already prepared, while binder extensions should apply only the new type-level
  assignments rather than replaying the complete old environment.
- Re-profile the 693,735-node arena before widening `DeferredEnvType`; field materialization was the
  second-largest prior boundary, but tuple and label changes may have removed some of its consumers.

## 2026-08-15 — round 20: reuse materialized field routes

### Findings

- A fresh root-operation profile attributed 689,062 checker-created nodes in the 693,735-node
  arena. Field lookup remained the largest boundary: final route materialization allocated
  223,889 nodes and search re-entry allocated another 38,667, for 262,556 nodes in total (38.1%).
  Common analytic preparation followed at 176,240 nodes; type-application normalization used
  56,080; type-abstraction recovery used 44,883; prepared product revelation had fallen to 1,597.
- A deferred field route retained a closure for every label, product, product component, and final
  payload. Finalization then recursively substituted every closure independently. Substituting a
  parent already constructs its rewritten children, so materializing the child closures repeated
  the same recursive work and discarded the parent-produced identifiers.
- Route materialization can instead carry the selected child of the previous materialized node.
  A matching label or product reuses that identifier. If the route crosses a seal or another
  opacity boundary, the expected shape is absent and the stored closure supplies the required
  one-time environment re-entry.
- A separate lean finalizer for term projections was valid but saved only five additional nodes on
  the full standard library (588,244 to 588,239). Term projections do not need the sibling
  annotations retained for pattern reconstruction, but nearly every relevant route already starts
  at a product and obtains those siblings from its one necessary materialization. The extra API was
  removed rather than keeping a workload-insignificant specialization.

### Changes

- Deferred product route steps now retain only the product closure and selected position. Their
  component identifiers are read from the materialized product spine instead of recursively
  substituting a second vector of component closures.
- Finalization threads the selected child through label and product steps and uses it whenever its
  structure matches the next route node. The final projected type is that same child, avoiding one
  more independent materialization.
- The existing opacity-boundary and label-depth regressions exercise the fallback and transparent
  paths respectively. The temporary allocation counter and the rejected term-only finalizer were
  removed after measurement.

### Measurements

- The full-std check retains 588,244 type nodes, 105,491 fewer than round 19 (-15.2%).
- Three clean release checks used 409,075,712, 409,026,560, and 409,370,624 bytes peak RSS (median
  409,075,712), down 15,122,432 bytes (-3.6%) from round 19. Warm wall time was 0.45s and warm user
  time was 0.41s.
- The current-tree baseline is now down from 2,497,757,184 to 409,075,712 bytes (-83.6%).
- All 41 statics tests and all 151 session tests passed, along with the release CLI build and full
  standard-library check.

### Next

- Re-profile the 588,244-node arena. Common analytic preparation is likely the largest remaining
  boundary, but route reuse changes its downstream inputs enough that the round-19 counts are no
  longer additive.
- Carry preparation provenance through same-environment arrow, thunk/return, and codata
  destructors. Treat binder extensions separately: replaying the complete old environment would
  reproduce the original amplification under a different helper.
- Inspect `normalize_app` and type-abstraction recovery after analytic preparation. Their previous
  56,080- and 44,883-node shares were stable across several rounds and may become worthwhile once
  the expected-type paths are composed.

## 2026-08-15 — round 21: preserve preparation through lexical extensions

### Findings

- Caller-aware instrumentation attributed 177,549 nodes to common analytic preparation in the
  round-20 tree. Two `let`-tail forwarding sites alone used 74,693 and 56,619 nodes, totaling
  131,312. Both passed an outer expected result type into a tail whose environment had gained the
  freshly bound local definition.
- Equality was too strong for preparation provenance. A type formed outside an inner binder's
  lexical scope cannot contain that binder's globally unique `DefId`, so adding the binder does not
  create a substitution obligation for the outer type. Every old definition mapping and visible
  skolem remaining unchanged is the relevant condition; unrelated new entries are harmless.
- Replaying the complete old environment is observably wrong even when the new binding is
  unrelated. Given the ordered mappings `x -> y` and `y -> Unit`, one preparation of `x` yields
  `y`. A second traversal caused only by entering a larger scope advances it to `Unit`. The count
  of environment applications therefore follows lexical provenance, not equality of whole context
  snapshots.
- This rule does not cover replacement environments. Monadic translation can remove mappings, and
  any changed definition mapping or removed skolem invalidates the provenance and still triggers
  preparation.

### Changes

- `TyEnv::is_extension_of` checks that every definition mapping and skolem in a base environment
  remains present with the same identity. Persistent-map pointer equality handles unchanged
  components directly; a subset comparison handles ordinary lexical extension.
- Analytic term entry accepts a prepared environment when the current environment extends it. It
  retains the original provenance rather than pretending the new entries were applied.
- An environment regression covers additions, replacements, missing definitions, and missing
  skolems. A checker regression uses the `x -> y -> Unit` chain to prove that an unrelated inner
  binding cannot advance an already prepared annotation.
- Temporary caller and allocation instrumentation was removed after the census.

### Measurements

- The full-std check retains 456,804 type nodes, 131,440 fewer than round 20 (-22.3%). The exact
  reduction consists of the two `let` paths plus a 128-node recursive-group tail with the same
  monotone-extension shape.
- Three clean release checks used 367,968,256, 368,738,304, and 368,148,480 bytes peak RSS (median
  368,148,480), down 40,927,232 bytes (-10.0%) from round 20. Warm wall time was 0.40s and warm user
  time was 0.36s.
- The current-tree baseline is now down from 2,497,757,184 to 368,148,480 bytes (-85.3%).
- All 43 statics tests and all 151 session tests passed, along with the release CLI build and full
  standard-library check.

### Next

- The remaining common-preparation callers total about 46,000 nodes. Explicit annotations lead at
  19,123, value-function bodies at 8,838, package introductions at 5,250, and value-arrow bodies at
  3,900. Separate already-prepared components from genuinely fresh annotations at those sites.
- Type-application normalization and type-abstraction recovery were previously 56,080 and 44,883
  nodes. Re-profile them against the smaller arena before deciding whether they now dominate.
- The substitution implementation already normalizes its result, while common analytic entry calls
  `normalize_k` again. The second pass allocated only hundreds of nodes in this profile, but the API
  contract should be made explicit before removing that redundant-looking call.

## 2026-08-15 — round 22: preserve prepared expected-type components

### Findings

- Preparation provenance applies structurally to components of an analytic type. Once an expected
  nondependent arrow has been prepared, both its domain and codomain have received the same
  environment operation. Checking the function body under a value binder extends the lexical term
  environment, but value terms cannot occur in types, so the codomain remains prepared under the
  outer environment.
- Thunk and return syntax encode their payload types as application components. Their analytic
  paths unify an already prepared expectation with a fresh shape and then immediately extract the
  payload. That component is likewise current; sending it through a fresh analytic action repeated
  the complete recursive traversal.
- This reasoning does not automatically apply to components of a synthesized function type. Such
  an annotation may have been created at a definition site and can still owe its first substitution
  at the application site. Those callers remain fresh pending a separate provenance rule.

### Changes

- Value-arrow and computation-arrow bodies now receive `Action::ana_prepared` with the outer
  environment that prepared their expected arrow. The scope-extension rule from round 21 then
  carries that provenance through the checked binder.
- Thunk bodies, forced values, and returned values receive prepared actions for payloads extracted
  from their current analytic shapes.
- Type-polymorphic and package-dependent function bodies remain on the fresh path because their
  codomains can mention the newly opened type witnesses.

### Measurements

- The full-std check retains 449,694 type nodes, 7,110 fewer than round 21 (-1.6%).
- Three clean release checks used 367,607,808, 367,362,048, and 367,788,032 bytes peak RSS (median
  367,607,808), down 540,672 bytes (-0.1%) from round 21. This RSS change is near run-to-run noise,
  while the deterministic arena-node reduction confirms the eliminated work. Warm wall time was
  0.39–0.41s and warm user time was 0.36–0.37s.
- The current-tree baseline is now down from 2,497,757,184 to 367,607,808 bytes (-85.3%).
- All 43 statics tests and all 151 session tests passed, along with the release CLI build and full
  standard-library check.

### Next

- Give synthesized annotations explicit preparation provenance, or prepare an entire synthesized
  arrow before destructuring it. Do not mark its domain prepared merely because the arrow node was
  normalized: normalization does not apply lexical substitutions.
- Audit explicit ascriptions separately. Their annotation term is synthesized in the current
  environment, but `Data` and `CoData` substitution currently allocate new definitions even when no
  arm changes, so allocation alone cannot prove that a substitution was semantically necessary.
- Re-run root-operation attribution before addressing type-application normalization and
  type-abstraction recovery.

## 2026-08-15 — round 23: reuse unchanged nominal definitions

### Findings

- `subst_env` rebuilt every `Data` and `CoData` definition unconditionally. Even an empty
  environment allocated a fresh nominal identity after recursively visiting unchanged arms. This
  violates the substitution invariant used by every other constructor: an unchanged subtree keeps
  its original `TypeId`.
- This was a real semantic and allocation defect but not the explanation for the remaining large
  ascription cost. Restoring no-op reuse removes only 87 nodes from the full standard library.
- Operation-local memoization was also tested and removed. A map from input `TypeId` to substituted
  result passed all 43 then-current statics tests but left the arena exactly unchanged at 449,694
  nodes. The hot structures are effectively trees within one traversal; amplification occurs across
  separate substitution operations, so per-operation DAG caching only adds lookup overhead.

### Changes

- Data and codata substitution now compares each original arm type with its substituted result. It
  returns the original nominal type when every arm is identical and allocates a fresh definition
  only when at least one arm changes.
- A regression covers both constructors and both paths: an empty substitution preserves identity,
  while a mapping used by one arm creates a distinct nominal definition containing the replacement.
- The temporary node counter and the zero-benefit memoization prototype were removed.

### Measurements

- The full-std check retains 449,607 type nodes, 87 fewer than round 22. This deterministic change is
  below process-RSS measurement resolution, so the last clean median remains the useful memory
  reference at 367,607,808 bytes (-85.3% from baseline).
- All 44 statics tests and all 151 session tests passed, along with the release CLI build and full
  standard-library check.

### Next

- Eliminate the cross-operation roundtrip in synthesized type functions. Their body is checked with
  a fresh abstract witness, recursively rewritten back to the source `DefId` when constructing
  `Type::Abs`, and recursively rewritten to an argument at every application. Retaining the witness
  in the abstraction representation can remove the 44,883-node recovery pass entirely.
- Keep the type-abstraction binder explicit in typed syntax rather than adding a side table that
  must be propagated whenever an abstraction node is reconstructed. Substitution, alpha-equivalence,
  normalization, formatting, and monadic elaboration must all agree on the binder.

## 2026-08-15 — round 24: retain type-abstraction witnesses

### Findings

- A synthesized type function previously checked its body with a fresh abstract witness, recursively
  rewrote that witness back to the source `DefId` when constructing `Type::Abs`, and then recursively
  rewrote the definition to an argument whenever the function was applied. The first rewrite alone
  accounted for 44,883 nodes in the last root-operation profile.
- The source definition is presentation metadata, not the semantic binder of the checked body. The
  abstract witness already has the required globally unique identity and is also the identity that
  substitution support, skolem scope, and alpha-equivalence need to recognize.
- Retaining that witness removed 45,337 full-std nodes. The 454-node difference from the attributed
  recovery count comes from downstream reconstruction changes, while the near equality confirms
  that the measured recovery path was causal.
- Round 23's zero-result operation-local memoization supports the same boundary: duplication was
  created by two distinct semantic operations with an intervening representation change. Preserving
  the checked representation removes the operation; caching within either traversal cannot.

### Changes

- `Type::Abs` now contains a `TypeAbstraction` with an explicit `TypeBinder` and body. Synthesis and
  analytic checking preserve the fresh witness used to check that body instead of recovering a
  source definition.
- Type application substitutes the retained witness directly. Abstract substitution shadows it,
  support collection treats it as bound, and least-upper-bound comparison alpha-renames the two
  witnesses before comparing bodies.
- Generic construction, formatting, inference resolution, filled normalization, and monadic
  elaboration all use the same representation. There is no side table or legacy abstraction form.
- Regressions cover beta reduction, binder shadowing, and alpha-equivalence between independently
  allocated type abstractions.

### Measurements

- The full-std check retains 404,270 type nodes, 45,337 fewer than round 23 (-10.1%).
- Three clean release checks used 358,678,528, 359,202,816, and 359,448,576 bytes peak RSS (median
  359,202,816), down 8,404,992 bytes (-2.3%) from round 22's last measured median. Warm wall time was
  0.39–0.40s and warm user time was 0.35–0.36s.
- The current-tree baseline is now down from 2,497,757,184 to 359,202,816 bytes (-85.6%).
- All 46 statics tests and all 151 session tests passed, along with the release CLI build and full
  standard-library check.

### Next

- Re-profile the 404,270-node arena. Type-application normalization previously used 56,080 nodes;
  its input representation changed, so its current cost and callers need fresh attribution.
- Look for repeated applications of the same retained type-function body across separate semantic
  operations. Any useful reuse belongs at that cross-operation boundary rather than inside one
  substitution traversal.
- Revisit the remaining common analytic-preparation callers, especially explicit ascriptions and
  synthesized arrows, only after distinguishing types that owe their first environment application
  from components that are already current.

## 2026-08-15 — round 25: cache field materialization across lookups

### Findings

- A fresh root-operation census attributed 382,605 of the 404,270 full-std type nodes. Structural
  field search was again the largest boundary: initial closure materialization used 118,398 nodes
  across 5,754 roots, and re-entry after an opacity boundary used another 38,665 across 5,394 roots.
  Together they accounted for 157,063 nodes, 38.9% of the complete arena.
- Round 20 reused children produced by one route's parent materialization, but separate field
  lookups still forced identical closures. The semantic identity is the source `TypeId`, exact
  `TyEnv`, and pending operation (`Substitute` or `UnrollThenSubstitute`). Repeating that triple
  while type state is stable produces the same immutable result.
- This is the cross-operation reuse that round 23's traversal-local memo could not see. Caching at
  the field-closure boundary removed 65,592 nodes; the remaining profiled field materializations
  used 91,471 nodes and represent cache misses or work after a necessary invalidation.
- The cache cannot survive arbitrary checker mutation. Filling an inference variable can change
  normalization, adding a seal changes unrolling, and attaching a value builtin role changes the
  metadata transferred to substituted labels. Each mutation therefore clears the cache.

### Changes

- The live checker owns a typed field-materialization cache keyed by root, structural environment,
  and deferred operation. It is transient checker state and is dropped before a checked outcome is
  published.
- Both inherited substitution and opacity-boundary re-entry use the same cache path. This keeps the
  two semantic operations distinct while allowing either result to be reused by a later lookup.
- Seal registration is centralized so it cannot bypass invalidation. Successful inference fills
  and value-role attachments clear the same cache; clearing retains the hash-table capacity to
  avoid allocation churn while dropping every state-dependent entry.
- Regressions prove that a repeated field lookup allocates no new type nodes while state is stable,
  that an inference update forces rematerialization, and that adding a seal invalidates a cached
  unsealed result.

### Measurements

- The full-std check retains 338,678 type nodes, 65,592 fewer than round 24 (-16.2%).
- Three clean release checks used 349,028,352, 349,274,112, and 349,126,656 bytes peak RSS (median
  349,126,656), down 10,076,160 bytes (-2.8%) from round 24. Warm wall time was 0.37s and warm user
  time was 0.33–0.34s.
- The current-tree baseline is now down from 2,497,757,184 to 349,126,656 bytes (-86.0%).
- All 48 statics tests and all 151 session tests passed, along with the release CLI build and full
  standard-library check.

### Next

- Type-application normalization is now the largest single stable root operation at 56,080 nodes.
  Attribute those applications by function identity and argument reuse before choosing between a
  cross-operation beta cache and a representation that keeps applications suspended longer.
- Common analytic preparation still uses 38,736 nodes. Its remaining callers need provenance
  classification rather than a global cache because the same environment is deliberately
  non-idempotent across genuine semantic boundaries.
- The remaining field misses use 91,471 nodes. Inspect their key distribution and invalidation
  epochs before widening the cache; unique materializations need a representation change, not a
  larger memo table.

## 2026-08-15 — round 26: preserve synthesized ascription annotations

### Findings

- Exact-key profiling ruled out a general beta-result cache as the next change. The full standard
  library performs 7,871 type applications allocating 56,080 nodes, but has 7,097 distinct
  `(function, argument, result kind)` triples. Reusing every repeated triple could save only 2,465
  nodes, too little to justify another mutable-state cache.
- The expensive applications are predominantly unique materializations. In particular, the
  `Std Reader Writer OS` signature application allocates 3,917, 3,918, and 3,920 nodes at its three
  left-associated arguments. Each step rewrites almost the complete remaining standard-library
  signature; fusing the arguments requires a suspended application representation, not memoization.
- The same profile reconfirmed 38,736 nodes in common analytic preparation. Explicit ascriptions
  accounted for exactly 19,123: their annotation term is synthesized in the current environment,
  but the body was entered with a fresh analytic action and therefore applied that environment
  again.
- Synthesis is the provenance boundary. It resolves the annotation using the current environment,
  and reconciling it with an outer analytic expectation does not make it stale. The ascribed body
  should receive the synthesized result as already prepared.

### Changes

- Explicit ascription now enters its body with `Action::ana_prepared` and the current environment.
  Kinds are unaffected; type annotations bypass the redundant substitution while retaining the
  existing lexical-extension checks for nested scopes.
- A checker regression synthesizes a labeled annotation whose payload participates in a chained
  environment. It proves that the body receives the synthesized label unchanged instead of
  advancing its payload through a second environment application.
- The source-boundary test fixture now accepts a caller-built scoped arena so the regression
  exercises the real `Tm::Ann` rule rather than reconstructing its intended action manually.
- The beta and analytic-preparation profilers were removed after the census; no application cache
  was retained.

### Measurements

- The full-std check retains 319,555 type nodes, 19,123 fewer than round 25 (-5.6%).
- Three clean release checks used 347,488,256, 347,455,488, and 347,357,184 bytes peak RSS (median
  347,455,488), down 1,671,168 bytes (-0.5%) from round 25. Warm wall time was 0.36–0.37s and warm
  user time was 0.33–0.34s.
- The current-tree baseline is now down from 2,497,757,184 to 347,455,488 bytes (-86.1%).
- All 49 statics tests and all 151 session tests passed, along with the release CLI build and full
  standard-library check.

### Next

- Design a suspended type-application representation that can accumulate the `Std` signature's
  three abstract assignments and materialize its body once. Intermediate type terms and the filled
  normalizer both need a coherent account; merely postponing `Type::App` in the checker would cause
  the finish pass to rebuild the same partial results.
- Re-profile common preparation after removing ascriptions. Roughly 19,600 nodes remain, led by a
  3,913-node preparation of the standard-library implementation body and several 1,076-node
  builtin-package arguments. Determine which are first applications and which have synthesis
  provenance from another typing rule.
- Retain the negative beta-cache result as a design constraint: optimize unique large applications
  by composing substitutions, while leaving the thousands of one-node applications simple.

## 2026-08-15 — round 27: preserve prepared universal codomains

### Findings

- Re-profiling the remaining common analytic preparation classified its large callers by source
  rule. Six nested universal-abstraction bodies in the integer and float package builders allocated
  334, 333, 332, 294, 293, and 292 nodes, respectively: 1,878 nodes of direct replay.
- The whole expected universal type had already been substituted and normalized before its outer
  abstraction was inspected. Opening the type binder extends the lexical environment, but the
  expected body refers to the retained abstract witness rather than the source `DefId`. That
  extension therefore creates no new substitution obligation for the codomain.
- Both universal branches discarded this provenance by entering the body with `Action::ana`.
  Replaying the complete outer environment at every nested binder copied successively smaller
  suffixes of the same function type, the same linear-source/quadratic-materialization shape seen
  in package telescopes.
- The rest of the large common-preparation roots are genuine first specializations. They include
  eight 1,076-node Builtin-package arguments, two 515-node Data-package arguments, and the
  3,913-node standard-library implementation result. Marking those prepared would be unsound;
  removing them requires sharing equivalent imported classifiers or changing their representation.

### Changes

- Analytic value and computation `forall` abstractions now pass their extracted codomain with
  `Action::ana_prepared`, recording the environment in which the enclosing expected type was
  prepared. Lexical extensions continue to be accepted only when they preserve that environment.
- A checker regression constructs a prepared computation universal whose environment would advance
  its codomain if replayed. It proves that opening the binder retains the current codomain rather
  than applying the outer mapping a second time.
- The temporary caller/source profiler and retained-node counter were removed after measurement.

### Measurements

- The full-std check retains 317,104 type nodes, 2,451 fewer than round 26 (-0.8%). The additional
  573-node reduction beyond the directly attributed replay comes from downstream types that no
  longer reconstruct the duplicated codomains.
- Three clean release checks used 346,357,760, 350,240,768, and 347,389,952 bytes peak RSS (median
  347,389,952), effectively unchanged from round 26's 347,455,488-byte median. Warm wall time was
  0.38–0.40s and warm user time was 0.34–0.36s.
- The current-tree baseline is now down from 2,497,757,184 to 347,389,952 bytes (-86.1%).
- All 50 statics tests and all 151 session tests passed, along with the release CLI build and full
  standard-library check.

### Next

- Treat the remaining common analytic preparation as necessary until a representation or sharing
  boundary proves otherwise. The caller census no longer supports broadening prepared provenance.
- Design suspended type application around the unique `Std Reader Writer OS` chain. It must compose
  abstract assignments and materialize the body once while still giving intermediate type terms and
  the filled normalizer a coherent normal form.
- Check whether structurally identical imported package classifiers can share an immutable checked
  identity without violating the deliberate freshening of source import occurrences. This is a
  source/query ownership question, not a substitution-cache question.

## 2026-08-15 — round 28: saturate type-application spines

### Findings

- The `Std Reader Writer OS` chain was only the visible tip of the type-application cost. Eagerly
  substituting each argument built a complete partial result, then field lookup, analytic
  preparation, and later applications copied those partial trees again. Removing the intermediate
  trees therefore eliminated much more than the 56,080 nodes attributed directly to beta
  reduction in round 25.
- A type application whose result kind is still an arrow cannot yet be inspected as a value or
  computation classifier. Its existing `Type::App` representation is already a complete typed
  closure: it names the function, argument, and result kind without materializing the function
  body. No new syntax variant or mutable side table is required.
- The right normalization boundary is saturation. A final base-kinded application can collect its
  complete left-associated spine, walk a direct nest of type abstractions, bind every argument, and
  apply the ordered abstract assignments in one structural traversal. Neutral or non-direct heads
  fall back to the previous stepwise behavior.
- The finish pass must preserve this boundary. A temporary phase census found 53,096 type nodes
  after judgments, 53,932 after hole resolution, and 53,969 after filled normalization. The finish
  pass adds only 37 nodes, demonstrating that suspension does not defer the removed materialization
  debt to the end of checking.

### Changes

- Checked type application now retains arrow-kinded prefixes as `Type::App` and materializes the
  spine when its result kind ceases to be an arrow.
- `TypeApplicationSpine` records every argument, result kind, and reusable original application
  node. Direct abstraction nests compose their witness assignments through `subst_absts`; the
  general fallback retains the former one-step semantics and reuses unchanged application nodes.
- Ordinary explicit normalization still forces an application spine for structural consumers.
  Filled normalization preserves higher-kinded prefixes, while least-upper-bound comparison forces
  a suspended application only when the other representation is not an application. This retains
  beta-equivalence without eagerly expanding equal compact spines.
- Multi-argument normalization uses the same spine operation rather than reintroducing sequential
  beta reduction through its convenience API.
- Regressions prove that a checked function-kinded prefix remains an application through filled
  normalization, a three-binder saturated application allocates only the two nodes in its result
  product, and a partial application unifies with its explicit beta normal form.
- The temporary spine and phase counters were removed after measurement.

### Measurements

- The full-std check retains 53,969 type nodes, 263,135 fewer than round 27 (-83.0%).
- Three clean release checks used 290,373,632, 290,439,168, and 290,275,328 bytes peak RSS (median
  290,373,632), down 57,016,320 bytes (-16.4%) from round 27. Warm wall time was 0.30s and warm user
  time was 0.27s, down from 0.38–0.40s and 0.34–0.36s respectively.
- The current-tree baseline is now down from 2,497,757,184 to 290,373,632 bytes (-88.4%).
- All 53 statics tests and all 151 session tests passed, along with the release CLI build and full
  standard-library check.

### Next

- Re-profile the 53,969-node arena from scratch. Every earlier percentage and ordering was measured
  on a representation dominated by eager partial applications, so field lookup and common analytic
  preparation need a new census before further changes.
- Separate remaining typed-arena cost from process-wide retained memory. RSS now falls much less
  than node count, so immutable source arenas, salsa query state, and non-type static tables are
  likely the next important owners.
- Reconsider sharing structurally identical imported classifiers only if the new ownership profile
  still attributes meaningful memory to them; saturation may already have removed their amplified
  copies.

## 2026-08-15 — round 29: compact free-variable contexts

### Findings

- macOS allocator snapshots separated the remaining process memory by phase. Live heap grew from
  14.0MB after loading the source graph to 44.9MB after parsing, 61.2MB after desugaring, and
  146.0MB after name resolution, before the type checker began. The resolved source arena was
  therefore already larger than the complete post-saturation checking delta.
- Controlled field-by-field drops identified the resolved-arena owners including nested
  allocations. `coctxs_term_local` released 49.1MB and `coctxs_pat_local` released 22.2MB. By
  comparison, resolved terms released 15.1MB, the textual provenance map 19.6MB, spans 12.7MB,
  and the remaining fields substantially less.
- The two dominant tables contain free-variable sets for each resolved occurrence. The standard
  library has 64,954 term sets containing only 72,733 logical entries in total, and the largest set
  has 73 entries. Their 71.3MB footprint came from one persistent hash-trie value per occurrence,
  not from a large amount of semantic data.
- Persistent structure was the wrong tradeoff at this scale. Most sets are empty or tiny, while the
  sum of all logical entries is itself small enough to store directly. A compact sorted sequence
  makes both the representation and its cost match the actual invariant: a small unordered set of
  resolved identifiers.

### Changes

- `CoContext` now stores a private sorted, deduplicated `Vec` instead of an `im::HashSet`.
  Construction and union re-establish the invariant; subtraction uses binary search, and callers
  inspect the set through `iter` and `is_empty` rather than its representation.
- Regressions cover sorting, deduplication, union, and subtraction. Surface formatting, session
  assertions, and stack-IR closedness checks use the representation-independent API.
- The phase snapshots and destructive field-drop instrumentation were removed after the ownership
  census.

### Measurements

- After the representation change, the resolved live heap is 85.0MB instead of 146.0MB. Controlled
  drops measure 8.8MB for term free-variable contexts and 1.6MB for pattern free-variable contexts,
  a combined 61.0MB reduction from their former 71.3MB footprint.
- Three clean release checks used 227,360,768, 232,013,824, and 227,147,776 bytes peak RSS (median
  227,360,768), down 63,012,864 bytes (-21.7%) from round 28. Warm wall time was 0.27–0.29s and warm
  user time was 0.24–0.26s.
- The current-tree baseline is now down from 2,497,757,184 to 227,360,768 bytes (-90.9%).
- All 22 utility tests, 140 surface tests, and 151 session tests passed, including utility doctests,
  along with the release CLI build and full standard-library check.

### Next

- Remove or stop retaining the 64,954-entry `ctxs_term` table. Every stored lexical context is empty
  in the standard-library program, no production reader exists, and the table still owns 6.4MB.
- Account for the checker's remaining roughly 112MB live-heap increase from resolved input through
  normalization. Inventory all static arena tables before changing another substitution path.
- Revisit the 19.6MB textual provenance map after establishing which editor and diagnostic queries
  consume each direction. Its many one-element vectors may have a similar representation mismatch,
  but unlike `CoContext` it encodes a real one-to-many relation that needs a usage census first.

## 2026-08-15 — round 30: remove empty term contexts

### Findings

- `ScopedArena::ctxs_term` had no production reader anywhere in the workspace. Its only consumers
  were writes in the resolver and hand-built test fixtures.
- All 64,954 entries in the full standard-library table contained empty `Context` vectors. The
  post-resolution collector threaded one root `Context::new()` unchanged through every term and
  pattern; binder-local definitions are recorded separately in `ctxs_pat_local` and the threaded
  value never participated in their computation.
- The table therefore encoded neither lexical scope nor an approximation useful downstream. Its
  6.4MB measured footprint was pure hash-table overhead for a historical analysis path.

### Changes

- Removed `ctxs_term` from `ScopedArena`, the resolver collector, and checker fixtures.
- Simplified the post-order collector to a context-free traversal. The unused fallible `Collect`
  adapter and its pattern result were removed; `ctxs_pat_local`, `coctxs_pat_local`, and
  `coctxs_term_local` retain their existing computations.

### Measurements

- Three clean release checks used 224,591,872, 223,379,456, and 223,543,296 bytes peak RSS (median
  223,543,296), down 3,817,472 bytes (-1.7%) from round 29. Warm wall and user time remained 0.27s
  and 0.24s respectively.
- The current-tree baseline is now down from 2,497,757,184 to 223,543,296 bytes (-91.0%).
- All 140 surface tests, all 53 statics tests, and all 151 session tests passed, along with the
  release CLI build and full standard-library check.

### Next

- Inventory the full static arena at the normalization peak. The source-side census now explains
  its dominant outliers, while checking still raises live heap by roughly 112MB.
- Distinguish durable static syntax and indexes from checker-transient query keys and Salsa memo
  values. A table's retained size is only actionable once its downstream readers and phase lifetime
  are known.
- Audit the source textual provenance relation separately after the checker census; its 19.6MB
  remains the largest measured resolved-arena field but may be required by source diagnostics.

## 2026-08-15 — round 31: inline singleton relation edges

### Findings

- The static field-drop census found another common shape behind three large owners. Term
  provenance used 22.5MB for 63,575 edges from 63,574 source terms, and pattern provenance used
  6.5MB for 27,151 edges from 27,137 source patterns. Almost every key in both directions therefore
  had exactly one partner.
- `ArenaBipartite` represented both directions as `HashMap<Id, Vec<Id>>`, allocating two vector
  buffers for almost every edge. `ArenaForth` did the same on its multi-valued direction, including
  the resolved textual provenance previously measured at 19.6MB. The maps must support genuine
  one-to-many or many-to-many cases, but paying the multi-edge representation at every singleton
  was unnecessary.
- After compacting the relation storage, controlled drops measured term provenance at 14.7MB and
  pattern provenance at 3.6MB. Live heap before checking also fell from 77.8MB to 73.4MB, showing
  the same benefit in source textual and user relations. The normalization peak fell from 192.3MB
  to 177.3MB.

### Changes

- Added a private `OneOrMany<T>` sequence to the arena utility layer. Its first element is stored
  inline; only insertion of a second element transitions to a vector, with exactly two initial
  slots.
- `ArenaForth` uses the compact sequence for its forward side, `ArenaBack` for its reverse side, and
  `ArenaBipartite` for both sides. Their public lookup APIs still expose slices, preserving the
  relation semantics without exposing the representation.
- Consuming relation merges now traverse the compact sequence directly, so singleton merging does
  not allocate a temporary vector. Existing relation tests cover singleton-to-multiple transitions,
  reverse lookup, duplicate rejection, and idempotent bipartite insertion.
- The allocator snapshots and destructive static-field census were removed after measurement.

### Measurements

- Three clean release checks used 208,846,848, 207,437,824, and 207,486,976 bytes peak RSS (median
  207,486,976), down 16,056,320 bytes (-7.2%) from round 30. Warm wall time was 0.25–0.26s and warm
  user time was 0.23s.
- The current-tree baseline is now down from 2,497,757,184 to 207,486,976 bytes (-91.7%).
- All 22 utility tests and utility doctests, 140 surface tests, 53 statics tests, 13 stack-IR tests,
  and 151 session tests passed, along with the release CLI build and full standard-library check.

### Next

- The static census now attributes 11.2MB to `types_pre`, 5.6MB to type annotations, 9.8MB to
  per-term normalized types, 7.5MB to term annotations, and 11.2MB combined to pre-normalized and
  normalized kinds. Determine which pairs duplicate the same fact and which are independent
  downstream indexes.
- Roughly 24MB remains live after dropping the entire static arena above the resolved-input
  baseline. Attribute that remainder to Salsa judgment/query keys and results before changing
  durable syntax tables.
- Consider split singleton/multiple hash tables only if relation storage remains prominent after
  the higher-level lifetime audit. The inline representation captures most of the avoidable
  per-edge allocation without complicating lookup.

## 2026-08-15 — round 32: store normalized kinds as a delta

### Findings

- Filled normalization retained all 40,619 pre-normalized kind nodes and also cloned a `Kind` into
  `kinds_normalized` for every ID. The controlled static drop measured the normalized column at
  7.4MB, in addition to 3.8MB for `kinds_pre`.
- Types had already established the right invariant: unchanged nodes are their own normal form,
  while the normalized table stores only old-ID-to-new-form deltas for solved fills or rebuilt
  paths. Kind finalization still implemented the older eager-column design.
- Applying the delta rule left zero normalized-kind entries for the full standard library. Hole
  resolution has already made every retained kind its own normal form before filled normalization;
  all 40,619 entries were representational duplicates.
- The editor is the only downstream normalized-kind reader. It already fell back to `kinds_pre`
  when a normalized entry was absent, so centralizing that lookup preserves classification for both
  unchanged kinds and genuine deltas.

### Changes

- Kind finalization now inserts into `kinds_normalized` only when the normalized ID differs from the
  original ID, matching type finalization.
- `StaticsArena::normalized_kind_at` owns the delta-plus-fallback lookup, and Cajun semantic
  highlighting uses that API instead of inspecting both tables itself.
- A checker regression covers both cases: an unchanged `VType` stores no delta, while a fill solved
  to that kind remains queryable through a stored old-ID delta.
- The normalization design note now states the sparse-delta invariant rather than promising a
  duplicate normalized entry for every finalized ID.

### Measurements

- The full standard-library check retains zero normalized-kind entries instead of 40,619.
- Three clean release checks used 201,654,272, 200,687,616, and 200,802,304 bytes peak RSS (median
  200,802,304), down 6,684,672 bytes (-3.2%) from round 31. Warm wall time was 0.25–0.26s and warm
  user time was 0.22–0.23s.
- The current-tree baseline is now down from 2,497,757,184 to 200,802,304 bytes (-92.0%).
- All 54 statics tests, all 151 session tests, and the focused Cajun semantic/stdio tests passed,
  along with the release CLI build and full standard-library check.

### Next

- Examine `term_norms` and `term_anns` together. They cost 9.8MB and 7.5MB respectively and share
  the same 63,574 source-term key domain; a single typed record may avoid one complete hash index
  without weakening demand-driven editor facts.
- Determine whether `annotations_type` can share the type page layout or move its kind directly into
  the type slot. Its 53,969-entry parallel page table costs 5.6MB, but unlike normalized kinds every
  value is independently used during checking and lowering.
- Attribute Salsa's post-static 24MB remainder before optimizing query keys. The durable arena is
  now small enough that transient query state can become the next dominant owner.

## 2026-08-15 — round 33: combine per-term static facts

### Findings

- `term_anns` and `term_norms` used two hash tables over the same source-term identity domain.
  Every one of 63,574 checked source terms had a final annotation, and 58,664 also retained the
  normalized form of its annotation type.
- The two tables serve distinct queries but not distinct lifetimes. Both are built by the same
  check, shared through the same `StaticsIndexes` generation, and retained specifically so editor
  facts survive stripping the occurrence payload.
- Combining them saves one key, hash, control byte, and bucket allocation per source term. The
  normalized value remains optional because kind-sorted and unresolved terms deliberately have no
  normalized type fact.

### Changes

- Replaced `term_anns` and `term_norms` with one `ArenaAssoc<TermId, TermFacts>`. Each record carries
  the final `TermAnnId` and an optional normalized `Type`.
- `StaticsArena` now owns typed record/update accessors. Recording a new annotation clears any
  normalized value derived from the prior annotation; finish-phase normalization enriches the same
  record afterward.
- Session annotation and normalized-type queries use the accessors, preserving their independent
  demand-driven Salsa entry points while sharing retained storage.
- A regression verifies that re-annotating a term invalidates its old normalized fact.

### Measurements

- Three clean release checks used 197,951,488, 197,591,040, and 197,574,656 bytes peak RSS (median
  197,591,040), down 3,211,264 bytes (-1.6%) from round 32. Warm wall time was 0.26s and warm user
  time was 0.23s.
- The current-tree baseline is now down from 2,497,757,184 to 197,591,040 bytes (-92.1%).
- All 55 statics tests and all 151 session tests passed, along with the release CLI build and full
  standard-library check.

### Next

- Profile whether term facts need a normalized `Type` clone for all 58,664 terms or whether multiple
  terms predominantly point at the same normalized annotation `TypeId`. An ID-based retained view
  would require keeping or separately materializing the referenced type node, so count sharing
  before changing ownership.
- Audit `annotations_type` together with `types_pre`. Co-locating each kind ID with its type payload
  could remove a 5.6MB parallel page hierarchy, but it changes the central typed-node slot and must
  preserve fill handling and query-derived IDs.
- Measure Salsa query families by memo count and value shape to explain the remaining post-static
  24MB rather than treating all memoization as one owner.

## 2026-08-15 — round 34: deduplicate normalized annotation facts

### Findings

- The 58,664 normalized term facts refer to only 27,049 distinct top annotation `TypeId`s. A source
  term can share its classifier with wrappers and other occurrences, and the normalized `Type`
  depends on that classifier identity rather than on the term that happened to expose it.
- `type_sites` existed only to gate normalized-type queries: it mapped a top annotation ID back to a
  source term, which then selected the per-term normalized clone. Inner type IDs had no site and
  therefore answered no fact. A normalized map keyed directly by top annotation ID enforces the
  same boundary without the reverse hop.
- This identity is durable across occurrence-payload eviction. Storing only the `TypeId` would lose
  the payload when the full arena memo is evicted, but storing one cloned `Type` per distinct top ID
  preserves stale-analysis facts while removing duplicate clones.

### Changes

- Source-term facts now retain only the final `TermAnnId`. Normalized annotation forms live in
  `annotation_norms: ArenaAssoc<TypeId, Type>`.
- Finish-phase normalization inserts the first normalized form for each distinct annotation ID and
  skips later terms sharing that ID. The demand-driven normalized-type query reads this map
  directly; membership both proves that the ID is a top annotation and supplies its durable form.
- Removed `type_sites` and the per-term optional normalized field. A regression records two terms
  with the same annotation ID and verifies that they share one normalized entry.

### Measurements

- The retained normalized-fact key domain falls from 58,664 source terms to at most 27,049 distinct
  top annotation types (-53.9%), while preserving the same query behavior.
- Three clean release checks used 189,267,968, 188,104,704, and 187,990,016 bytes peak RSS (median
  188,104,704), down 9,486,336 bytes (-4.8%) from round 33. Warm wall time was 0.25s and warm user
  time was 0.22–0.23s.
- The current-tree baseline is now down from 2,497,757,184 to 188,104,704 bytes (-92.5%).
- All 55 statics tests and all 151 session tests passed, along with the release CLI build and full
  standard-library check.

### Next

- Re-run the static ownership census after the source and retained-index changes. Earlier absolute
  field sizes are now stale, and the next target should be selected from the new peak rather than by
  subtracting old measurements.
- Audit `annotations_type` and `types_pre` as one typed-node record. Their IDs and page topology are
  identical, making this a stronger co-location candidate than unrelated hash indexes.
- Attribute Salsa's surviving query state by family. After the durable index reductions, the
  roughly 24MB post-static remainder is a larger fraction of the check and may contain repeated
  environment or input handles.

## 2026-08-15 — round 35: materialize leaf patterns without producer memos

### Findings

- A malloc-stack census of the live heap after the standard-library check attributed 1.76MB to
  13,772 `pat_leaf_node_judgment` memo allocations. Salsa's automatically interned query tuples
  occupied another 1.28MB, while the explicit `InternedPatLeafNode` value table and two visible
  lookup tables occupied at least 1.29MB. Dependency-edge vectors and the leaf patterns' share of
  `InternedPat` added further overhead.
- The query did no semantic work that could be reused independently. Its source-pattern argument
  was used only to derive an identifier, its `ScopedData` argument was deliberately ignored, and
  its remaining inputs were the already-computed annotation and a hole-or-variable enum. The
  enclosing `check_source` query already memoizes the whole materialized result.
- Unannotated variable stand-ins and synthesized-hole errors had the same shape: the caller had
  already selected the syntax arm, and the producer merely repeated that selection around a
  deterministic identifier calculation. Under the wholesale `ScopedData` input, retaining these
  producer memos cannot reuse work after a source change.

### Changes

- Added a typed `Tycker::query_derived_id` boundary that preserves the query allocation tag and
  exact `(site, occurrence, slot)` identity without requiring a Salsa query to own the ID.
- Replaced the leaf-pattern producer family with two local materializers: `PatternLeaf` inserts the
  kind, type, or value pattern at the existing slot 2, and `PatternVariableStandIn` inserts the
  inference fill and type at slots 0 and 1. The annotation and environment records are unchanged.
- Removed `pat_leaf_node_judgment`, `pat_var_hole_judgment`, `pat_hole_syn_judgment`, their outcome
  types, and the high-cardinality `InternedPatLeafNode` key. A synthesized hole now emits the same
  `MissingAnnotation` error directly.

### Measurements

- Three clean release checks used 181,600,256, 181,878,784, and 182,353,920 bytes peak RSS (median
  181,878,784), down 6,225,920 bytes (-3.3%) from round 34. Warm wall time was 0.24s and warm user
  time was 0.21s, also slightly below the prior 0.22--0.23s user-time range.
- The current-tree baseline is now down from 2,497,757,184 to 181,878,784 bytes (-92.7%).
- All 55 statics tests and all 151 session tests passed, along with the release CLI build and full
  standard-library check.

### Next

- Treat producer queries as an optimization only where they expose independently reusable work.
  Continue removing the highest-cardinality wrappers whose source arm and computed inputs are
  already known to the materializer; heap evidence identifies `sigma_syn_judgment` as the next
  visible memo family, though it is much smaller than leaf patterns.
- Audit `types_pre` and `annotations_type` as a single paged type-node record. They have identical
  key domains and page topology, and co-location can remove a parallel outer map and slot vector
  without changing the type graph.
- Revisit source provenance only after the checker-local opportunities. Its large hash tables are
  durable editor/diagnostic data, while producer memos are duplicate lifetime by construction.

## 2026-08-15 — round 36: co-locate type nodes and compact kind annotations

### Findings

- `types_pre` and `annotations_type` had an exact one-to-one key invariant: all 53,969 type nodes
  in the standard-library check had one kind annotation, every insertion site wrote the pair, and
  no annotation existed independently of its type payload. Separate paged arenas therefore stored
  the same key-space map, page vector, occupancy bit, and allocation boundary twice.
- The annotations themselves were also highly repetitive. The 53,969 occurrences referred to only
  4,684 distinct `KindId`s; the intrinsic `VType` accounted for 19,144 occurrences and `CType` for
  another 4,036. A full `KindId` is 16 bytes, so retaining one at every type slot encoded identity
  information much more often than necessary.
- A combined slot containing `Fillable<Type>` and a 32-bit arena-local kind index is smaller than
  the former pair of optional type and kind slots. The dictionary is arena-local because these IDs
  are meaningful only within the same checked generation.

### Changes

- Replaced `ArenaPaged<StaticsScope, TypeId>` plus `ArenaPagedAssoc<TypeId, KindId>` with one typed
  `TypeArena`. Each page slot owns a `TypeNode { value, kind_index }`; one compact vector resolves
  kind indexes back to `KindId`.
- Type insertion now takes the payload and classifier together, making the one-to-one invariant an
  API property rather than a convention spread across callers. `StaticsArena::type_kind` and
  `type_kind_at` provide the read boundary used by checking, normalization, formatting, the editor,
  and downstream inspection.
- An `FxHashMap` interns kind IDs while the arena is being built. `strip_checker_state` drops this
  construction-only reverse index; the forward vector remains, and a later insertion can rebuild
  the reverse index lazily without duplicating a kind.
- A layout and behavior regression verifies compact slots, shared kind entries, lookup after
  stripping, and lazy reconstruction after stripping.

### Measurements

- Three clean release checks used 179,486,720, 177,995,776, and 178,094,080 bytes peak RSS (median
  178,094,080), down 3,784,704 bytes (-2.1%) from round 35. Warm wall time was 0.23s and warm user
  time was 0.21s.
- The current-tree baseline is now down from 2,497,757,184 to 178,094,080 bytes (-92.9%).
- All 56 statics tests and all 151 session tests passed. The focused Cajun semantic suite and its
  semantic-token stdio flow passed, along with the release CLI build and full standard-library
  check.

### Next

- Refresh the ownership census: the former 5.6MB `annotations_type` owner no longer exists, and the
  type payload, compact dictionary, and page topology now need to be measured as one unit.
- Inspect whether the remaining producer-query families justify independent memoization. The heap
  snapshot identified `sigma_syn_judgment` as the next visible family, but its roughly 3,084 memos
  are small enough that a broader rule may be more useful than another isolated rewrite.
- Reconsider the provenance relation representation now that term provenance is again one of the
  largest durable checker fields. A split singleton/multiple layout may avoid paying an enum-sized
  value in every hash bucket while preserving the rare one-to-many cases.

## 2026-08-15 — round 37: discard resolver-local pattern free variables

### Findings

- Pattern free-variable contexts are an intermediate result of the resolver's postorder fold. A
  parent term combines a pattern's annotations with its body and bound variables to compute the
  term free-variable context; no checker, editor, diagnostic, or downstream pass reads the pattern
  free-variable map afterward.
- The standard library creates 27,137 pattern free-variable entries. Of those, 18,342 are empty;
  the remaining entries contain 11,940 definition IDs in total, with a maximum context size of 14.
  Retaining the surrounding hash map and one `Vec` value per pattern therefore costs much more than
  the semantic payload.
- The other scoped summaries have distinct lifetimes. Pattern binding contexts are read by Cajun to
  identify parameter definitions, while all 64,954 term free-variable contexts participate in the
  checker's incremental global-term classification. They cannot be removed at the same boundary.

### Changes

- Kept `coctxs_pat_local` as private `Collector` scratch state so the resolver's postorder equations
  are unchanged, but stopped transferring it into the durable `ScopedArena`.
- Removed the dead field from downstream arena fixtures. No compatibility field or recomputation
  path remains because there is no post-resolution consumer.

### Measurements

- Three release checks used 177,537,024, 177,602,560, and 177,946,624 bytes peak RSS (median
  177,602,560), down 491,520 bytes (-0.3%) from round 36. The smaller RSS change than the dead
  table's logical footprint indicates that the allocator already reused part of its released pages
  before the later static-checking peak.
- The current-tree baseline is now down from 2,497,757,184 to 177,602,560 bytes (-92.9%). Warm wall
  time was 0.23--0.24s and warm user time was 0.21s.
- All 140 surface tests, all 56 statics tests, and all 151 session tests passed, along with the
  release CLI build and full standard-library check.

### Next

- Measure which pattern binding contexts contain information beyond the direct binder leaves. Cajun
  needs the union only to enumerate parameter definitions, so a direct binder-to-role index may
  replace 27,137 retained vectors without moving resolver scratch state into the editor.
- Separate term free-variable summaries needed during checking from durable scoped syntax. They are
  consulted while global definitions accumulate, but become dead once the checked arena has
  recorded `global_terms`; an explicit phase boundary could release their 64,954 vectors sooner.
- Refresh the live heap census before choosing between the remaining producer-query families and
  provenance relations.

## 2026-08-15 — round 38: derive editor parameters from pattern syntax

### Findings

- `ctxs_pat_local` had only one post-resolution reader. Cajun scanned abstraction, `pi`, `sigma`, and
  manifest-existential binder roots, then used each stored context solely to collect the variable
  leaves beneath that pattern.
- The scoped pattern syntax already preserves precisely that tree. Annotation, name, constructor,
  and projection nodes forward to one child; alias and product patterns combine their children; a
  variable leaf contributes its definition. Rewalking those few parameter roots reproduces the
  editor result without retaining analysis for every pattern.
- The standard library's 27,137 stored binding contexts contained only 7,231 definition IDs. 20,147
  entries were empty, 6,906 were singletons, and the largest context contained 14 definitions. As
  with the pattern free-variable table, the hash buckets and `Vec` headers outweighed the payload.

### Changes

- Cajun's `ParameterDefinitions` now traverses the scoped binder patterns and collects variable
  leaves directly. Its semantic classification remains a typed `HashSet<DefId>`; only the source of
  that set changed.
- Kept pattern binding contexts in the resolver's private `Collector`, where they are required to
  subtract locally bound variables from term free-variable contexts, but stopped transferring them
  into `ScopedArena`.
- Removed the obsolete durable field from static-checker fixtures.

### Measurements

- Three release checks used 175,685,632, 176,340,992, and 176,439,296 bytes peak RSS (median
  176,340,992), down 1,261,568 bytes (-0.7%) from round 37.
- The current-tree baseline is now down from 2,497,757,184 to 176,340,992 bytes (-92.9%). Warm wall
  time was 0.23--0.24s and warm user time was 0.21s.
- All 140 surface tests, all 56 statics tests, all 151 session tests, all 31 Cajun unit tests, and all
  9 Cajun stdio tests passed. The release CLI build and full standard-library check also passed.

### Next

- Move term free-variable contexts across the checking boundary rather than retaining them in the
  durable scoped syntax. Unlike both pattern maps, their values are needed throughout checking, so
  this requires assigning ownership to the checked-analysis construction rather than simply
  discarding a resolver field.
- Refresh the live heap census at the new 176MB peak. The source arena now retains no pattern
  context tables, so stacks previously attributed to `Context<DefId>` should identify only the
  transient collector and term summaries.
- Compare the next producer-query family against durable provenance by measured retained bytes; the
  earlier snapshot predates both type-node compaction and removal of the two pattern maps.

## 2026-08-15 — round 39: page term free-variable contexts

### Findings

- The remaining term free-variable relation has 64,954 entries but only two identifier key spaces.
  Paging through each term's raw ID requires 96,052 slots, for 67.6% occupancy. That density is high
  enough for direct indexing to beat a hash bucket containing both a 16-byte `TermId` and a 24-byte
  context vector header.
- The empty slots do not allocate context payload vectors. Their option discriminants live inline in
  the page, while the 72,733 stored definition IDs keep exactly the same sorted-vector
  representation as before.
- The relation remains logically temporary. Paging reduces its overlap cost immediately without
  constraining the planned ownership change that will eventually drop it after source judgments.

### Changes

- Replaced the `ArenaAssoc<TermId, CoContext>` in the resolver collector and `ScopedArena` with
  `ArenaPagedAssoc<TermId, CoContext>`.
- Kept all insertions and reads behind the existing typed arena API. No checker or session behavior
  changed, and the representation remains cloneable for revision-owned scoped data.

### Measurements

- Three release checks used 174,211,072, 173,981,696, and 177,537,024 bytes peak RSS (median
  174,211,072), down 2,129,920 bytes (-1.2%) from round 38. The third sample was an allocator-page
  outlier; the first two agree within 230KB.
- The current-tree baseline is now down from 2,497,757,184 to 174,211,072 bytes (-93.0%). Warm wall
  time was 0.24s and warm user time was 0.22s.
- All 140 surface tests, all 56 statics tests, and all 151 session tests passed, along with the
  release CLI build and full standard-library check.

### Next

- Give term contexts an explicit checker-local owner and drop them after `run_judgments_k`, before
  hole resolution, normalization, validation, and construction of the durable analysis.
- Preserve one postorder context computation per resolved snapshot without storing the result in
  Salsa's `ScopedData`; recomputing it inside the whole-source check is preferable if the table then
  follows the check memo's `lru = 1` lifetime instead of every resolved snapshot's lifetime.
- Re-run the heap census after that phase split, because a smaller retained source baseline may move
  the peak to an earlier point in term checking.

## 2026-08-15 — round 40: make source contexts checker-local

### Findings

- `ScopedData` owns an `Arc<ScopedArena>` as a tracked Salsa field, `TyckOutput` returns the same arc,
  and `ProgramAnalysis` retains it for editor and diagnostic queries. Keeping term contexts in that
  arena therefore gave a checker-only table the lifetime of every resolved snapshot and finished
  analysis.
- The three free-variable checks occur only while source judgments run: two classify newly bound
  definitions as global, and one marks the typed result of each source term as global. Hole solving,
  normalization, coverage validation, diagnostics, the editor, and every backend use the recorded
  static facts rather than the source contexts.
- Pattern binding and free-variable maps are needed only while the postorder equations construct
  term summaries. Once the term table exists, both pattern maps can be destroyed before the first
  typed node is allocated.

### Changes

- Removed term contexts from `ScopedArena`; name resolution now returns only durable scoped syntax,
  users, provenance, and block dependency data.
- Added the typed `TermContexts::collect` boundary in the surface arena. Its private
  `ContextCollector` borrows scoped syntax, computes both pattern maps as scratch state, and returns
  only the paged term free-variable relation.
- `Tycker` owns `TermContexts`, builds it once for the source root, and exposes one private typed
  lookup to the three global-classification sites. Both whole-source checking paths release the
  table immediately after `run_judgments_k`, including the rejected-judgment path, before hole
  collection and normalization.
- The global checks now iterate borrowed contexts instead of cloning each sorted vector merely to
  test membership.

### Measurements

- Three release checks used 172,228,608, 171,982,848, and 171,950,080 bytes peak RSS (median
  171,982,848), down 2,228,224 bytes (-1.3%) from round 39. The narrow 279KB range confirms that the
  paged table no longer overlaps the final static-arena peak.
- The current-tree baseline is now down from 2,497,757,184 to 171,982,848 bytes (-93.1%). Warm wall
  time was 0.23--0.24s and warm user time was 0.21s.
- All 140 surface tests, all 56 statics tests, all 151 session tests, all 31 Cajun unit tests, and all
  9 Cajun stdio tests passed. The release CLI build and full standard-library check also passed.

### Next

- Capture a new malloc-stack census at the 172MB peak. The durable scoped arena now contains no
  context maps, and old stack categories for resolver collection should disappear before the
  checker reaches its maximum.
- Attribute the remaining source-side peak separately from the post-normalization peak. If the
  checker now peaks after contexts are released, further work belongs in typed facts, provenance,
  or Salsa memos rather than in resolver summaries.
- Re-evaluate the next producer-query family and typed provenance relation from current measurements
  instead of relying on the round-35 heap snapshot.

## 2026-08-15 — round 41: page scoped terms with known extents

### Findings

- A fresh malloc-stack census attributed one 14,827,520-byte live allocation to the scoped term
  hash table. Other leading durable owners were textual spans at 15.9MB, static pattern provenance
  at 11.7MB, term facts at 7.3MB, and the two textual-to-bitter provenance directions at 7.3MB and
  6.3MB. This made scoped syntax a larger target than any remaining individual producer memo.
- The standard-library scoped arena contains 64,954 terms in two key spaces spanning 96,052 raw
  slots, for 67.6% occupancy. An `Option<Term<DefId>>` is 64 bytes, so the occupied ID extents need
  about 6.1MB of direct slots. Definitions occupy only 3.8% of their raw extent and patterns 28.3%;
  keeping those two categories sparse avoids paying 2.3MB and 4.6MB logical pages, respectively.
- A plain switch to `ArenaPaged` did not reduce the peak. Three checks centered on 172,130,304 bytes,
  effectively unchanged from round 40. The large page had length 95,955 but capacity 137,920 after
  geometric growth; macOS reported its live allocation as the same 14,827,520-byte size class as
  the old hash table. This falsified the initial estimate because logical occupancy alone omitted
  both vector growth slack and allocator size-class behavior.
- Every prospective scoped term ID is already present in the bitter arena before resolution starts.
  Reserving each key space to its exact greatest raw ID removes geometric slack before the first
  insertion. A late `shrink_to_fit` would not solve the test problem because the larger allocation
  would already have contributed to peak RSS.

### Changes

- Replaced only scoped term ownership with `ArenaPaged`; definitions and patterns remain
  `ArenaSparse` according to their measured densities.
- Added `ArenaPaged::reserve_ids`, which groups a known external ID domain by key space and reserves
  each page to its greatest raw slot without filling gaps. A regression verifies that inserting the
  reserved IDs does not grow either page.
- The resolver pre-reserves scoped term pages from the complete bitter term domain before recursive
  resolution. Editor and session iterators now use the value-form IDs yielded by paged iteration.

### Measurements

- Three release checks used 167,362,560, 167,985,152, and 167,854,080 bytes peak RSS (median
  167,854,080), down 4,128,768 bytes (-2.4%) from round 40. Warm wall time was 0.23s and warm user
  time was 0.20s.
- The current-tree baseline is now down from 2,497,757,184 to 167,854,080 bytes (-93.3%).
- All 23 utilities tests and their doctests, all 140 surface tests, all 56 statics tests, all 151
  session tests, all 31 Cajun unit tests, and all 9 Cajun stdio tests passed. The release CLI build
  and full standard-library check also passed.

### Next

- Measure `TermFacts` occupancy against the same source term extents. Its 7.3MB single hash
  allocation is the next clear typed table, but it should use exact page reservation from the start
  rather than repeat the geometric-growth false start.
- Audit the 15.9MB span owner as separate per-source maps plus the assembled program map. The
  assembled 12.7MB allocation may admit a typed split by definition, pattern, and term ID, while
  the source templates need to remain independently reusable.
- Measure both directions and multiplicities of static pattern provenance before changing it. Its
  13.3MB total footprint is large, but one-to-many edges and query-derived key spaces make density
  less predictable than the source term arena.

## 2026-08-15 — round 42: separate sparse term identity from dense facts

### Findings

- The final standard-library analysis retains facts for 63,574 of the 96,052 source term slots:
  66.2% occupancy across extents of 97 and 95,955. `Option<TermFacts>` is 40 bytes, while the
  existing hash table owned one 7,487,488-byte allocation in the round-41 heap census.
- Direct paged storage needs 3,842,080 logical bytes at exact capacity, but reserving that entire
  extent in `Tycker::new` raised median peak RSS to 169,033,728 bytes. The facts are produced
  progressively during judgments, so eager allocation overlapped source contexts and early checker
  state that had not overlapped the gradually growing hash table.
- Allowing the wide page to grow progressively instead produced a stable 166,969,344-byte median
  over five warm checks. Its 1.5x growth policy still placed 40-byte payloads in every capacity
  slot, including gaps and the unused tail.
- Source identity and fact payload have different density requirements. A one-based `NonZeroU32`
  index makes `Option<TermFactsIndex>` four bytes; storing those indexes in the sparse page and the
  40-byte facts in an insertion-dense vector needs 2,927,168 logical bytes at final lengths. It also
  lets the wide payload follow actual judgment progress rather than raw source extents.

### Changes

- Added `TermFactsArena`, which maps each source term through a compact paged index into a dense
  fact vector. Re-recording a term replaces its existing dense slot, preserving the previous
  last-annotation semantics without appending duplicates.
- Extended exact known-ID reservation to `ArenaPagedAssoc` through the shared `PageSlots` policy.
  Term facts pre-reserve only their four-byte index pages; the dense payload vector remains
  progressive.
- Normalization continues to iterate source term/fact pairs through the typed arena boundary. A
  layout and replacement regression verifies the four-byte optional index and stable dense slot.

### Measurements

- Five release checks used 166,428,672, 166,805,504, 166,772,736, 167,313,408, and 166,920,192
  bytes peak RSS (median 166,805,504), down 1,048,576 bytes (-0.6%) from round 41. Warm wall time was
  0.22--0.23s and warm user time was 0.19--0.20s.
- The current-tree baseline is now down from 2,497,757,184 to 166,805,504 bytes (-93.3%).
- All 24 utilities tests and their doctests, all 57 statics tests, all 151 session tests, all 31
  Cajun unit tests, and all 9 Cajun stdio tests passed. The release CLI build and full
  standard-library check also passed.

### Next

- Refresh the live heap census now that neither scoped terms nor term facts use a wide hash bucket.
  This should distinguish the assembled span map, source provenance, and static pattern provenance
  without the former 14.8MB and 7.5MB owners obscuring them.
- Split the assembled span map by typed source ID category only if measured raw extents support
  compact pages. Per-source template span maps have independent Salsa reuse and should not be
  conflated with the assembled program representation.
- Measure provenance multiplicities as well as key density. A compact index plus dense edge payload
  may generalize better than replacing either direction with a wide optional relation value.

## 2026-08-15 — round 43: compact resolved source spans

### Findings

- A fresh live-heap census after round 42 measured 110.0MB of malloc-owned storage. Surface hash
  allocations accounted for 40.2MB, and the largest concrete allocation among them was the
  12,730,368-byte assembled textual span map. This displaced scoped terms and term facts as the
  leading individual source-side target.
- The assembled standard-library program has 98,398 spans in one identifier key space. Definitions,
  patterns, copatterns, and terms contain 3,431, 8,680, 1,937, and 84,350 entries respectively; their
  sum exactly equals the greatest allocated raw-ID extent. The shared textual parser allocator makes
  this relation fully dense even though the entity categories are separate types.
- Each `Span` occupied 72 bytes: two byte offsets, two resolved `usize` line/column pairs behind a
  `OnceLock`, and an optional shared path behind a second `OnceLock`. Location attachment consumes a
  fresh span before the value enters an arena, so neither lock mediated concurrent or repeated
  initialization. Their state and machine-word coordinates were representation cost rather than a
  semantic requirement.
- Source line and column positions fit compactly in two `u32` values. Encoding the one-based line as
  `NonZeroU32` gives `Option<CompactCursor2>` a niche, so both resolved endpoints occupy 16 bytes and
  the entire span occupies 40 bytes. A source beyond the compact coordinate range retains its byte
  offsets and path and falls back to byte-offset display instead of truncating a position.

### Changes

- Replaced both per-span locks with immutable compact location data and the existing shared path.
  `under_loc_ctx` now attaches that data while it exclusively owns the span.
- Kept the public byte offsets, path lookup, Ariadne conversion, and file/line/column display
  behavior unchanged for representable source files.
- Added layout, overflow, and rendered-location regressions. The 64-bit layout test fixes the intended
  40-byte bound explicitly.

### Measurements

- Five release checks used 164,642,816, 164,233,216, 164,216,832, 164,413,440, and 164,134,912 bytes
  peak RSS (median 164,233,216), down 2,572,288 bytes (-1.5%) from round 42. Warm wall time was 0.21s
  and warm user time was 0.19s.
- The current-tree baseline is now down from 2,497,757,184 to 164,233,216 bytes (-93.4%).
- All 27 utility tests and their doctests, all 140 surface tests, all 57 statics tests, all 151 session
  tests, all CLI tests, all 31 Cajun unit tests, and all 9 Cajun stdio tests passed. The release CLI
  build and full standard-library check also passed.

### Next

- Replace the assembled span hash map with storage that expresses the observed shared-key-space
  density. A dense span vector plus one-byte entity-category tags should need about 4.0MB of payload
  at current sizes while preserving typed iteration.
- Keep per-source template span maps independently reusable by Salsa. Their separate identifier key
  spaces and lifetimes require measurement before applying the assembled representation to them.
- Revisit the 11.7MB static pattern-provenance relation after the span index. Its multiplicity, rather
  than merely its key density, should determine whether it uses pages or a compact edge index.

## 2026-08-15 — round 44: make parser span identity implicit

### Findings

- Every `SpanArena` is produced by exactly one `Parser`. That parser owns one sequential allocator
  shared by definitions, patterns, copatterns, and terms, and its only allocation method records one
  span before returning the new ID. Consequently, a span's raw ID is always its insertion index,
  there are no gaps, and every arena contains only one key space.
- The entity category remains semantically necessary: definition ID 7 and pattern ID 7 must not be
  interchangeable even if their raw parts are equal. The full 24-byte `EntityId` is not necessary in
  every slot, however. One arena-level key space plus a one-byte category per raw index reconstructs
  the same typed identity without a hash lookup.
- At the assembled standard-library size, 98,398 compact spans occupy 3,935,920 bytes and their tags
  occupy 98,398 bytes. The two vectors and one optional key space add only fixed-size headers. This
  represents the mandatory relation in about 4.0MB before allocator rounding, with deterministic
  allocation-order iteration.
- The invariant applies independently to parsed source templates and to the assembled program. A
  dense representation reduces both without combining their Salsa lifetimes or key spaces, which
  explains why the measured gain exceeded the assembled-map estimate from round 43.

### Changes

- Replaced `SpanArena`'s `ArenaAssoc<EntityId, Span>` with parallel dense span and category vectors
  plus one retained key space. Insertion now verifies the single-parser key space and exact next raw
  index; lookup verifies both the key space and category.
- Added a low-level `restore_id` operation for storage that retains proof of a previously issued key
  space and raw slot. Span iteration uses it to recreate typed IDs and never allocates new identity.
- Parser finalization shrinks both vectors after construction, so durable source and assembled arenas
  do not retain geometric growth slack.
- Added regressions for typed-ID round trips, replacement, one-byte category layout, wrong-category
  rejection, allocation gaps, and foreign parser key spaces.

### Measurements

- Five release checks used 155,090,944, 154,615,808, 154,206,208, 154,796,032, and 154,222,592 bytes
  peak RSS (median 154,615,808), down 9,617,408 bytes (-5.9%) from round 43. Warm wall time was
  0.20--0.21s and warm user time was 0.18--0.19s.
- The current-tree baseline is now down from 2,497,757,184 to 154,615,808 bytes (-93.8%).
- All 27 utility tests and their doctests, all 143 surface tests, all 57 statics tests, all 151 session
  tests, all CLI tests, all 31 Cajun unit tests, and all 9 Cajun stdio tests passed. Focused Clippy
  passed with the repository's existing unrelated lint allowances. The release CLI build and full
  standard-library check also passed.

### Next

- Refresh the live heap census. The former 12.7MB assembled span hash allocation and all template
  span hashes should be absent, making the static pattern-provenance relation the likely largest
  concrete owner.
- Measure pattern-provenance edge counts, source and target key-space extents, and direction-specific
  query use before selecting a dense representation. A one-to-many relation should encode edges
  densely while keeping whichever endpoint is actually sparse behind a compact index.
- Revisit other textual per-entity tables only when their occupancy and lifetime justify doing so.
  The shared allocator does not imply that optional intentions or trivia are dense.

## 2026-08-15 — round 45: retain one source provenance representative

### Findings

- A fresh malloc-stack census after round 44 measured 100.9MB of live malloc-owned storage. The
  assembled span payload had fallen to 3,948,544 bytes, confirming that the former wide span hashes
  were gone. The largest remaining concrete allocations included 10,633,216 bytes for one static
  pattern-provenance direction, 7,487,488 and 6,438,912 bytes for the two textual-to-bitter
  provenance directions, 6,144,000 bytes for scoped term slots, 5,324,800 bytes for one static
  term-provenance direction, and 3,751,936 bytes for pre-normalization kind storage. Malloc stack
  logging inflated total process footprint to 210.1MB, so allocation ownership rather than that
  instrumented footprint was used for attribution.
- Static pattern provenance contained 27,151 edges between 27,137 source and 14,632 typed IDs. Only
  14 source IDs mapped to multiple typed IDs, while 12,519 typed IDs had multiple source IDs; both
  directions had maximum multiplicity two. Static term provenance contained 63,575 edges between
  63,574 source and 29,270 typed IDs. Only one source mapped to two typed IDs, but 4,576 typed IDs
  had multiple sources and one transparent typed term represented 8,831 source wrappers.
- No repository consumer queried the source-to-typed direction. Every durable typed-to-source
  consumer immediately selected `.last()` for a diagnostic or source span. Retaining both relation
  directions, every distinct edge, and idempotence indexes therefore encoded information no caller
  could observe.
- Three progressively narrower relation designs separated the relevant costs. A full transient edge
  set made insertion constant-time but raised median peak RSS to 155,107,328 bytes because its peak
  overlapped checking. Scanning reverse groups removed that set and reached a 152,141,824-byte median
  over samples of 153,092,096, 152,027,136, 152,436,736, 152,141,824, and 152,109,056 bytes, but warm
  user time rose to 0.19--0.20s because the 8,831-source group was scanned repeatedly. Side indexes
  only for groups of at least 16 sources restored runtime but retained a 153,616,384-byte median over
  samples of 153,796,608, 153,616,384, 153,714,688, 153,518,080, and 153,600,000 bytes. The consumer
  contract made all three full-edge variants unnecessarily general.

### Changes

- Replaced both static bidirectional provenance relations with `SourceProvenance`, a typed-keyed map
  containing one source ID per typed node. Recording a later check replaces the representative;
  transparent wrappers naturally collapse to the last wrapper checked.
- Changed diagnostics and typed source-span lookup to request that representative directly. Removed
  the construction-only duplicate indexes and finish step because the stored representation is now
  exactly the lasting query result.
- Added a regression that fixes replacement and independent typed-node behavior without preserving
  unobservable edge order or multiplicity.

### Measurements

- Five release checks used 150,470,656, 150,847,488, 150,798,336, 150,306,816, and 150,470,656 bytes
  peak RSS (median 150,470,656), down 4,145,152 bytes (-2.7%) from round 44. Every warm run took 0.20s
  wall time and 0.18s user time.
- The current-tree baseline is now down from 2,497,757,184 to 150,470,656 bytes (-94.0%).
- All 27 utility tests and their doctests, all 143 surface tests, all 25 statics unit tests and 33
  statics integration tests, all 151 session tests, all CLI tests, all 31 Cajun unit tests, and all 9
  Cajun stdio tests passed. Focused Clippy passed with the repository's existing unrelated lint
  allowances. The release CLI build and standard-library check also passed.

### Next

- Re-measure the heap after removing static provenance multiplicity. The two textual-to-bitter
  provenance directions are now the clearest relation target; audit their direction-specific query
  use before changing their representation.
- Determine why 3,227,648 bytes attributed to `env_type` remained live at the measurement pause even
  though checker-state stripping resets that arena. Distinguish a mislabeled stack from ownership in
  an earlier Salsa generation before changing its representation.
- Revisit the remaining source hashes and pre-normalization kind storage only from the refreshed peak;
  their individual allocations are now close enough that lifetimes may matter more than raw size.

## 2026-08-15 — round 46: encode textual origins as dense slots

### Findings

- The round-45 heap census attributed 7,487,488 and 6,438,912-byte live allocations to the forward
  and reverse directions of the textual-to-derived relation. Repository-wide use showed no query of
  textual-to-derived edges. Desugaring, deep cloning, error spans, name resolution, editor spans, and
  navigation all queried only one textual origin for a derived entity.
- One `Desugarer` allocator issues every bitter definition, pattern, and term ID in a shared sequence,
  and each allocation records its origin immediately. Resolution preserves those IDs; the only new
  resolved terms come from one second sequential allocator and are also recorded immediately. Raw
  derived IDs are therefore dense within each of the two key spaces, independent of entity category.
- A complete assembled textual program likewise has one parser key space. The full textual `EntityId`
  occupied 24 bytes because it repeated that key space and its category at every relation edge. Once
  the key space is stored at arena level, a textual origin needs only a four-byte raw ID and one-byte
  category, with an eight-byte aligned slot. A separate one-byte tag validates the derived category
  without retaining its ID.
- `SourceUnitDesugarer::new` still accepted an existing bitter arena even though every caller supplied
  an empty one. That obsolete path could combine different complete textual programs and invalidate
  the one-source-key-space invariant. Removing it makes the storage contract match the actual compiler
  phase boundary.

### Changes

- Replaced the bidirectional `ArenaForth` with `TextualOrigins`: one arena-level textual key space and
  dense origin/tag vectors per derived key space. Lookup reconstructs a previously issued typed
  textual ID and rejects a category mismatch.
- Routed bitter and scoped span lookup, deep cloning, synthetic scoped-term allocation, and Cajun
  navigation directly through derived-to-source lookup. No forward relation or derived-ID hash table
  remains.
- Made source-unit desugaring always start with an empty bitter arena. Added regressions for compact
  slot layout, all derived categories, wrong-category queries, allocation gaps, and mixed textual
  key spaces.

### Measurements

- Five release checks used 133,939,200, 134,447,104, 134,168,576, 133,922,816, and 133,840,896 bytes
  peak RSS (median 133,939,200), down 16,531,456 bytes (-11.0%) from round 45. Warm wall time was
  0.19--0.20s and warm user time was 0.17--0.18s.
- The current-tree baseline is now down from 2,497,757,184 to 133,939,200 bytes (-94.6%).
- All 27 utility tests and their doctests, all 146 surface tests, all 25 statics unit tests and 33
  statics integration tests, all 151 session tests, all CLI tests, all 31 Cajun unit tests, and all 9
  Cajun stdio tests passed. Focused Clippy passed with the repository's existing unrelated lint
  allowances. The release CLI build and standard-library check also passed.

### Next

- Capture a fresh heap census. Both formerly leading textual-provenance allocations and the static
  provenance edge payloads should be absent; use the new peak to decide between scoped syntax,
  pre-normalization kinds, spans, and retained editor facts.
- Trace the 3,227,648-byte allocation previously attributed to `env_type` through checker-state
  stripping and Salsa publication. It should not survive in a finished `StaticsArena`, so ownership
  evidence matters more than changing its container.
- Measure whether the remaining peak occurs before or after normalized editor facts are published.
  At 134MB, lifetime overlap may now dominate individual container overhead.

## 2026-08-15 — round 47: index sparse syntax into dense payloads

### Findings

- A fresh malloc-stack census after round 46 found 112,117 live allocations owning 71.2MB, down
  from 100.9MB after round 44. The former textual and static provenance payloads were absent. The
  largest actionable surface allocations were the exact 6,144,000-byte scoped term page and a
  3,719,168-byte pattern hash table; the assembled span payload remained 3,948,544 bytes and
  pre-normalization kinds occupied 3,751,936 bytes. Stack logging raised physical footprint to
  189.2MB, so these owner sizes, rather than the instrumented process total, guided the change.
- The census also attributed a 3,227,648-byte block to `env_type`, despite checker-state stripping
  replacing that arena with an empty value. The existing session regression confirms that a
  finished checked arena has zero type environments. As a separate ownership audit, temporarily
  removing `Clone` from `StaticsArena` still compiled statics, session, CLI, and Cajun, ruling out a
  production deep clone that preserves pre-strip state. The contradictory stack attribution is
  therefore allocator-history or block-reuse noise, not evidence for retaining `env_type`; no
  representation change was made from it.
- The standard-library scoped arena still has 64,954 terms across 96,052 raw slots, or 67.6%
  occupancy. Its 64-byte optional payload slots explain the exact 6.1MB page. Patterns occupy only
  28.3% of the same interleaved raw-ID domain, so direct optional payload pages would waste more
  memory than their hash table. These are two instances of one storage rule: shared allocation
  makes identity sparse within each syntax category, while the category's actual payload sequence
  is dense.
- A compact sparse index and dense payload vector express that rule without either compromise. A
  one-based `NonZeroU32` makes each optional index four bytes; gaps cost only that index, and full
  terms or patterns exist exactly once. Bitter and scoped arenas overlap during name resolution,
  so applying the representation on both sides removes more peak memory than the final scoped
  layout alone predicts.
- Exact reservation still matters even for the compact design. Initially relying on iterator size
  hints produced a 123,928,576-byte median because paged filtering cannot promise a nonzero lower
  bound. Counting IDs while deriving page extents and reserving the dense vector from that count
  reduced the final median by another 1,556,480 bytes without a temporary ID collection.

### Changes

- Added `ArenaIndexed`, a typed owning arena whose paged ID index points into an insertion-dense
  payload vector. It supports lookup, mutation, replacement, cloning, iteration with reconstructed
  typed IDs, and exact bulk reservation while preserving duplicate-ID checks.
- Changed bitter and scoped pattern and term storage to `ArenaIndexed`. The resolver reserves both
  scoped payload vectors and their compact index pages from the complete bitter ID domains before
  recursive resolution begins.
- Added regressions for multiple key spaces, large sparse gaps, four-byte optional indexes, exact
  reservation through a non-exact-size iterator, out-of-order insertion, and duplicate rejection.
  Updated Cajun for the value-form IDs produced by indexed iteration.

### Measurements

- Five release checks used 122,388,480, 122,814,464, 122,306,560, 122,372,096, and 122,355,712 bytes
  peak RSS (median 122,372,096), down 11,567,104 bytes (-8.6%) from round 46. Warm wall time was
  0.19s and warm user time was 0.17s.
- The current-tree baseline is now down from 2,497,757,184 to 122,372,096 bytes (-95.1%).
- All 29 utility tests and their doctests, all 146 surface tests, all 25 statics unit tests and 33
  statics integration tests, all 151 session tests, all 9 CLI integration tests, all 31 Cajun unit
  tests, and all 9 Cajun stdio tests passed. Focused Clippy completed with existing unrelated lint
  warnings. The release CLI build and standard-library check also passed.

### Next

- Capture the next live heap census. Pre-normalization kinds, assembled spans, annotation facts,
  and the remaining source maps are now similarly sized, so phase overlap should select the next
  target rather than container size alone.
- Audit textual syntax payload arenas against their shared parser allocator. The same sparse-ID,
  dense-payload rule may apply, but per-source Salsa templates and the assembled program have
  distinct lifetimes that must remain independently reusable.
- Revisit low-occupancy definition arenas only after measuring their absolute live allocation.
  Compact indexes are useful when payload hashes are material; density alone is not a reason to
  generalize every small table.

## 2026-08-15 — round 48: bound resolver-generated payloads and locate the peak

### Findings

- The post-round-47 heap made the scoped term payload the largest concrete allocation at
  8,306,688 bytes. Its allocation history showed both the resolver's initial exact reservation and
  a later `grow_one` from `ArenaIndexed::insert_new`. The initial bitter domain contains 64,896
  terms, while the resolved arena contains 64,954: only 58 context-elaboration terms exceeded the
  reserved capacity, but `Vec` doubled the 64-byte payload vector to 129,792 slots.
- Resolver-generated terms have a static upper bound. Context elaboration emits one term per SCC,
  every SCC contains at least one mobile parameter or definition, and all such source nodes already
  exist in the bitter arena. Counting those two variants before resolution may reserve a few slots
  for bindings combined into recursive SCCs, but it cannot underestimate the generated payloads.
- Removing that final growth did not materially change process peak RSS. Phase-boundary runs located
  the cumulative high-water mark at 62,603,264 bytes after resolution, 68,648,960 after checker
  construction, 117,850,112 after judgments, 119,406,592 after hole resolution, 122,224,640 after
  normalization, and 122,322,944 after checker-state stripping. The dominant remaining growth
  occurs during judgments, with normalization adding the final roughly 2.8MB. Freed resolver pages
  are reused by checking, so reducing a retained allocation need not lower the one-shot peak.
- A typed kind census sharpened the next target: the final arena contains 40,619 pre-normalization
  kinds, of which 28,605 are `VType`, 8,758 are `CType`, 2,156 are arrows, and 1,100 are labels.
  `Fillable<Kind>` is 40 bytes even though 92.0% of entries are one of two payload-free variants.
  Unlike the generated-term slack, this arena remains live through the measured high-water mark.

### Changes

- Extended `ArenaIndexed` bulk reservation with an explicit count of payloads whose IDs will be
  issued by the consuming pass. Sparse page extents remain limited to IDs that already exist.
- The resolver now reserves its bitter term domain plus the count of mobile source bindings. A
  regression inserts one generated ID from a new key space after a non-exact-size external
  iterator and verifies that the payload vector does not grow.

### Measurements

- Five release checks used 122,634,240, 122,159,104, 122,306,560, 122,388,480, and 122,273,792 bytes
  peak RSS (median 122,306,560), effectively unchanged from round 47. Warm wall time remained 0.19s
  and warm user time remained 0.17s.
- The current-tree baseline remains down from 2,497,757,184 to about 122.3MB (-95.1%). This round
  improves the retained scoped-arena bound and, more importantly, falsifies resolution as the
  remaining process peak.

### Next

- Store the two payload-free kind variants inline while keeping uncommon filled kinds behind dense
  indirection. This preserves borrowed arena access but avoids paying the 40-byte worst-case enum
  layout in 92% of kind hash buckets.
- Measure judgments and normalization separately after compacting kinds. Because `kinds_pre` stays
  live across both phases, a real layout reduction should affect the final high-water mark rather
  than merely changing allocator reuse.
- Continue treating `env_type` attribution as unproven until a phase-local ownership snapshot
  contradicts checker-state stripping; phase RSS alone does not establish which freed block an
  allocation history names.

## 2026-08-15 — round 49: inline common kind values

### Findings

- The phase audit from round 48 showed that pre-normalization kinds remain live through the process
  high-water mark, so their concrete layout matters even after resolver allocations have been
  released or reused. The final arena contains 40,619 entries, and 37,363 of them (92.0%) are the
  payload-free `VType` or `CType` variants.
- The previous sparse arena stored a 40-byte `Fillable<Kind>` directly in every hash bucket. This
  charged the worst-case arrow, label, or unresolved-fill layout to the two common values even
  though they need only a discriminant. The live-heap census attributed 3,751,936 bytes to that
  table before this change.
- A sparse identity map need not store its worst-case discriminated payload in every bucket. An
  eight-byte node can encode common values directly and point uncommon values into a dense side
  table. Static values reconstruct the same borrowed references for `VType` and `CType`, so the
  representation change does not force boxing into the public `Kind` type or alter callers.

### Changes

- Added `KindArena`, whose sparse table stores compact common-kind nodes and whose dense payload
  vector owns unresolved fills, arrows, and labels. Insertion, lookup, indexing, and iteration
  preserve the previous borrowed `Fillable<Kind>` interface.
- Changed `StaticsArena::kinds_pre` to use the compact arena. A regression checks the node layout,
  verifies that common variants consume no dense payload slots, and round-trips all supported
  storage forms through indexing and iteration.

### Measurements

- Five release checks used 120,274,944, 119,717,888, 119,668,736, 119,980,032, and 120,209,408 bytes
  peak RSS (median 119,980,032), down 2,326,528 bytes (-1.9%) from round 48. Warm wall time was
  0.18--0.19s and warm user time was 0.16--0.17s.
- The original current-tree baseline is now down from 2,497,757,184 to 119,980,032 bytes (-95.2%).
- All 26 statics unit tests and 33 statics integration tests, all 151 session tests, all 9 CLI
  integration tests, all 31 Cajun unit tests, and all 9 Cajun stdio tests passed. Focused Clippy
  completed with existing unrelated lint warnings. The release CLI build and standard-library
  check also passed.

### Next

- Capture one final live-owner census after removing the old 3.75MB kind table. Use phase lifetime
  and retained owner size together to decide whether another structural change is justified at the
  roughly 120MB peak.
- Prefer judgment structures over resolver storage when choosing any further target: judgments add
  about 49MB to the resident high-water mark, while all resolution completes at about 63MB and its
  released pages are already reused by checking.
- Treat normalization as a bounded secondary target. It adds roughly 2.8MB after judgments, hole
  resolution, and their retained editor facts are already present.

## 2026-08-15 — round 50: close the ownership audit on the test workload

### Findings

- The final malloc-stack census found 112,309 live allocations owning 66.0MB. Stack logging raised
  physical footprint to 176.1MB, so the normal five-run median from round 49 remains the process
  measurement. The census is useful for ownership: the former 3,751,936-byte kind table is gone,
  replaced by a 1,654,784-byte compact-node table and a 245,760-byte uncommon-payload table. Their
  combined 1,900,544 bytes are 49.3% smaller than the previous owner.
- The remaining large allocations are all bounded by source or final typed facts: the scoped term
  payload is 4,161,536 bytes; assembled spans are 3,948,544; term provenance is 2,703,360; term
  facts are 2,637,824; normalized annotations are 2,408,448; Salsa owns 1,392,640; pattern
  provenance is 1,359,872; the scoped pattern payload is 1,310,720; value patterns are 1,081,344;
  and compact textual origins are 1,064,960 bytes. None grows with repeated traversal of a type
  suffix.
- One 3,227,648-byte paged-table allocation is still symbolized as `env_type`. Finished arenas have
  zero type environments, `strip_checker_state` replaces the table with a fresh default value, and
  the existing regression observes the empty result. The matching live owner is the type-page
  directory, allocated through a layout-equivalent generic reservation path; code folding or stack
  attribution to that monomorphization is the most likely explanation. This is an inference from
  ownership and layout, not evidence that the environment table survives stripping.
- The phase measurements and final owner census now agree. Resolution ends around 62.6MB, checker
  construction around 68.6MB, judgments raise the high-water mark to 117.9MB, and normalization
  adds about 2.8MB. What remains at the roughly 120MB normal peak is simultaneous source-linear
  compiler state, rather than another hidden eager-instantiation sequence or retained phase clone.

### Test-workload validation

- The complete focused session library suite passed all 151 tests with two threads in 7.54s and
  used 455,426,048 bytes peak RSS. The last comparable suite measurement before the substitution
  and representation work was about 6.54GB and 74s, so test-suite peak memory is down roughly 93%
  and runtime roughly 90%.
- A full standard-library release check remains at the round-49 median of 119,980,032 bytes, down
  95.2% from the 2,497,757,184-byte baseline. Its typed arena contains 53,969 type nodes rather
  than the original 2,508,856, even though the resolved input still has about 65,000 source terms.

### Conclusion

- The root cause was eager recursive substitution at semantic wrappers, field searches, package
  telescopes, and partial type applications. Each step rebuilt the complete remaining type, so a
  source-linear sequence produced a quadratic sequence of distinct intermediate trees. Caching
  could not help because the intermediate substitutions were genuinely distinct; the fix was to
  preserve prepared representations, carry typed deferred substitutions across structural
  traversal, compose ordered assignments, and materialize once at a semantic boundary.
- Arena layout, full-phase cloning, transient environment retention, Salsa root retention, and
  test-session concurrency multiplied the cost of those generated nodes. Sharing immutable phase
  products, stripping checker-only state, bounding memo retention, and matching storage to measured
  key density removed those multipliers after the semantic node explosion was fixed.
- Further reductions are ordinary source-linear memory tuning. They may still be useful, but the
  pathological test-memory problem and its multiplicative mechanism are closed by the evidence
  above.

## 2026-08-15 — round 51: pack retained annotation and provenance dispatchers

### Findings

- The final owner census left term facts and static source provenance among the largest retained
  judgment structures. A typed census found 63,574 final term facts: 4,910 kind annotations,
  54,400 type annotations, 2,696 value annotations, and 1,568 computation annotations. No holes
  survived. The checker replaced an existing fact 487 times, but none changed annotation category.
- `TermAnnId` occupied 40 bytes. Its largest variants contain two ordinary 16-byte arena IDs; the
  remaining eight bytes came from storing the enum discriminant after the payload. Both IDs already
  contain four bytes of alignment padding after their raw indexes, so a storage-only record can
  place the category there and preserve both complete key spaces without interning or assuming that
  the two IDs share an allocation site. The census confirmed that 38,491 of 58,664 paired facts use
  different key spaces, ruling out a same-key-space shortcut.
- `PatId` and `TermId` dispatcher enums likewise occupied 24 bytes even though one concrete ID is 16
  bytes. Source provenance needs the dispatcher only as a hash key and never reconstructs it during
  iteration. Splitting the key into key space, raw index, and a one-byte category stores the category
  in existing padding and reduces the key to 16 bytes. This applies to 29,270 term representatives
  and 14,632 pattern representatives in the standard-library check.
- The shared rule is narrower than globally changing public syntax: when a dispatcher is an internal
  storage key or payload and callers still need the public enum, preserve the public representation
  at the boundary and pack its tag into the arena ID's alignment padding internally.

### Changes

- Term facts now own a 32-byte `CompactTermAnnId` and reconstruct the exact public `TermAnnId` on
  lookup. All five variants round-trip, including pairs whose IDs belong to independent key spaces;
  replacement semantics and the four-byte sparse source index are unchanged.
- Static pattern and term provenance now hash a 16-byte compact typed-entity identity rather than a
  24-byte dispatcher enum. Pattern and term APIs remain separately typed, and category tags still
  prevent cross-sort collisions.
- Added layout regressions for both compact records, independent-key-space annotation round trips,
  and the existing latest-source replacement behavior.

### Measurements

- Five release checks used 118,734,848, 119,357,440, 119,422,976, 119,308,288, and 119,275,520 bytes
  peak RSS (median 119,308,288), down 671,744 bytes (-0.6%) from round 49. The retained-record byte
  reduction is larger than the RSS movement because freed size classes continue to back later
  checking allocations.
- The current-tree baseline is now down from 2,497,757,184 to 119,308,288 bytes (-95.2%).
- All 27 statics unit tests and 33 statics integration tests, all 151 session tests, all 9 CLI
  integration tests, all 31 Cajun unit tests, and all 9 Cajun stdio tests passed. Focused Clippy
  completed with existing unrelated lint warnings. The release build and full standard-library
  check also passed.

### Next

- Pack source line and column pairs inside `Span`. The current two-u32 cursor record is already
  compact individually, but two such records make every span pay 16 bytes; a nonzero packed u32 per
  endpoint can preserve practical ranges and the existing byte-offset fallback in eight bytes.
- Use the measured normalized-annotation variant distribution before changing that 2.4MB owner.
  Applications and arrows account for 16,013 of 27,049 entries, while labels and products add
  another 6,915; any useful split representation must preserve cheap reconstruction without adding
  one heap allocation per common node.
- Revisit provenance only after another heap census. The compact hash keys remove dispatcher
  overhead, but replacing hashes with paged storage would be counterproductive because typed
  representatives inhabit many small derived key spaces.

## 2026-08-15 — round 52: pack source positions into one word

### Findings

- Assembled spans were the largest remaining concrete source owner at 3,948,544 bytes. Each `Span`
  occupied 40 bytes: sixteen bytes of authoritative byte offsets, sixteen bytes for two optional
  line-and-column cursors, and eight bytes for the optional source path.
- A cursor used eight bytes even though its two coordinates have very different practical ranges.
  Across every checked-in Zydeco source, the largest file has 1,254 lines and the longest line is
  158 bytes. Eighteen line bits cover 262,143 one-based lines, while fourteen column bits cover
  16,384 byte columns; those limits leave substantial headroom without widening the record.
- Line and column are derived presentation data. Byte offsets remain the authoritative source
  positions, and `Span` already falls back to displaying byte offsets when a cursor cannot be
  represented. The compact representation can therefore reject exceptional positions without
  truncation or changing source identity.
- The reusable rule is to keep the authoritative coordinate wide and make derived presentation
  coordinates opportunistically compact. This preserves correctness for unusually large inputs
  while charging ordinary inputs only for the representation they use.

### Changes

- Replaced the two-field cursor with one `NonZeroU32`: eighteen upper bits hold the one-based line
  and fourteen lower bits hold the byte column. The nonzero invariant preserves the `Option` niche,
  reducing each optional two-endpoint cursor pair from sixteen to eight bytes and `Span` from 40 to
  32 bytes on 64-bit targets.
- Added exact upper-bound round trips, line and column overflow rejection, and layout regressions for
  both the packed cursor and `Span`. The existing location, UTF-16, and byte-offset fallback tests
  continue to exercise the public behavior.

### Measurements

- Five release checks used 116,654,080, 118,472,704, 118,358,016, 118,439,936, and 118,439,936 bytes
  peak RSS (median 118,439,936), down 868,352 bytes (-0.7%) from round 51. A separate warm sample used
  118,226,944 bytes and took 0.18s wall time, 0.16s user time, and 0.01s system time.
- The current-tree baseline is now down from 2,497,757,184 to 118,439,936 bytes (-95.3%).
- All 29 utility unit tests and four utility documentation tests, all 146 surface tests, all 151
  session tests, all 9 CLI integration tests, all 31 Cajun unit tests, and all 9 Cajun stdio tests
  passed. Focused Clippy completed with existing unrelated lint warnings. The release build and full
  standard-library check also passed.

### Next

- Capture a fresh live-owner census after the packed dispatcher and span changes. Their record-size
  reductions should change the ranking among source provenance, spans, term facts, and normalized
  annotations even when allocator reuse hides part of the savings at process-RSS granularity.
- Use the census together with the normalized-annotation variant counts to decide whether a split
  representation is worthwhile. Avoid replacing one 56-byte inline value with one allocation per
  common application or arrow; pointer traffic could cost more than the retained bytes it removes.
- Continue distinguishing bounded source-linear tuning from the closed quadratic substitution
  mechanism. A next change should name a concrete retained owner and preserve cheap reconstruction
  at its public boundary.

## 2026-08-15 — round 53: batch and sort normalized annotation facts

### Findings

- A fresh post-round-52 malloc-stack census found 112,310 live allocations owning 63.7MB. Stack
  logging raised physical footprint to 172.6MB, so its purpose was ownership rather than process
  measurement. The largest identifiable owners were 4,064KiB of scoped terms, the previously
  attributed 3,152KiB page table, 3,088KiB of assembled spans, 2,352KiB of normalized annotations,
  2,128KiB and 1,072KiB of term and pattern provenance, and 2,064KiB of term facts.
- The census confirms the preceding layout changes at allocation granularity. Assembled spans fell
  from 3,856KiB to 3,088KiB; the two compact provenance tables now total 3,200KiB; and compact term
  facts fell from 2,576KiB to 2,064KiB. The 2,352KiB normalized-annotation hash table was therefore
  the next unchanged owner with a phase boundary that permits a different representation.
- Normalized annotation facts are produced only after all type normalization completes, and they
  are immutable afterward. The old loop nevertheless accumulated them one at a time in a hash
  table while retaining a separate vector of all 63,574 source term IDs. There are only 27,049
  distinct top annotations among the 58,664 facts whose classifier is a type.
- Retained lookup is an editor query, not a checker inner loop. Two parallel sorted slices provide
  borrowed lookup with about fifteen comparisons at this workload. They occupy exactly 432,784
  bytes of IDs plus 1,514,744 bytes of types, or 1,947,528 bytes total. That is 460,920 bytes (19.1%)
  less than the former 2,408,448-byte hash allocation, without boxing any type variant.
- The larger process win comes from batching. Collecting type IDs directly from term facts removes
  the temporary vector of 63,574 dispatcher IDs and never grows a hash table beside it. The shared
  rule is that an immutable derived index should be built in its final read representation when its
  complete input is already available at one phase boundary.

### Changes

- Added `NormalizedAnnotations`, which stores sorted `TypeId` and `Type` boxed slices in parallel
  and uses binary search for borrowed lookup. Construction asserts equal lengths and strict key
  ordering.
- Replaced per-term hash insertion with one batch pass: collect type-bearing annotations, sort and
  deduplicate their IDs, clone each already-normalized type once, and publish the parallel slices.
  Kind and hole facts remain excluded.
- Expanded the arena regression to cover out-of-order IDs, duplicate annotations, multiple type
  forms, and exclusion of a kind annotation. Public session and editor query behavior is unchanged.

### Measurements

- Five release checks used 115,949,568, 116,146,176, 116,228,096, 116,293,632, and 116,310,016 bytes
  peak RSS (median 116,228,096), down 2,211,840 bytes (-1.9%) from round 52. Warm wall time remained
  0.18--0.19s and warm user time remained 0.16--0.17s.
- The current-tree baseline is now down from 2,497,757,184 to 116,228,096 bytes (-95.3%).
- All 27 statics unit tests and 33 statics integration tests, all 151 session tests, all 9 CLI
  integration tests, all 31 Cajun unit tests, and all 9 Cajun stdio tests passed. Focused Clippy
  completed with existing unrelated warnings. The release build and full standard-library check
  also passed.

### Next

- Measure the 4,064KiB scoped term payload by variant and field layout before changing it. It is the
  largest confirmed source owner, but its public syntax is used throughout name resolution and
  checking, so a storage-only representation should be preferred over pervasive boxing.
- Evaluate `IndexMap`-style dense entries for mutable provenance. Its 16-byte typed key and 16-byte
  source value make open-addressed bucket slack expensive; a dense entry vector plus compact hash
  index may reduce both provenance owners while preserving lookup during checking.
- Re-measure the phase high-water mark before adding an end-of-phase compaction pass. Converting a
  live hash table into sorted slices briefly owns both allocations and can erase the retained saving
  if that conversion itself becomes the process peak.

## 2026-08-15 — round 54: shard dense source provenance by typed category

### Findings

- After round 51 packed the typed dispatcher, term and pattern provenance still occupied 2,128KiB
  and 1,072KiB open-addressed allocations. Each bucket stored a 16-byte typed identity and a
  16-byte source identity, so growing either table rehashed and copied every complete 32-byte entry.
- An unsharded `IndexMap` experiment lowered the five-run process median to 114,409,472 bytes because
  rehashing moved compact indexes rather than full entries. Its first live-heap census exposed a
  countervailing cost: dense entry buffers and hash indexes totaled about 4,288KiB, more than the
  3,200KiB retained by the original tables. Right-sizing at the judgment boundary reduced that to
  about 3,728KiB but could not cross the hash table's capacity steps.
- The provenance totals sat just above two such steps. Patterns split into 94 kind, 3,535 type, and
  11,003 value representatives. Terms split into 148 kind, 25,312 type, 2,306 value, and 1,504
  computation representatives. The combined pattern count of 14,632 exceeded a 14,336-entry usable
  capacity, while the combined term count of 29,270 exceeded a 28,672-entry capacity.
- Typed category is already part of identity and equality, so it is a valid hash partition rather
  than a workload heuristic. Sharding by category keeps every observed group below its next
  capacity step and prevents unrelated sorts from forcing each other's entry buffers to double.
- The final malloc-stack census attributes 1,679KiB to six visible right-sized dense entry buffers
  and 584KiB to seven compact index tables, about 2,263KiB total. That is 937KiB (29.3%) smaller than
  the two packed open-addressed tables, while the full live heap fell to 62.5MB. Stack logging raised
  physical footprint to 170.0MB, so normal runs remain the process measurement.

### Changes

- `SourceProvenance` now stores category-sharded `IndexMap`s with the existing Fx hasher. Its public
  pattern and term APIs still record the latest source representative and reconstruct exactly the
  same source IDs on lookup.
- Added an explicit end-of-judgments boundary that drops source contexts and calls `shrink_to_fit`
  on the nonempty provenance shards before hole resolution and normalization. Both the direct and
  query-driven checking paths use that boundary, including rejected judgments.
- Extended the provenance regression through right-sizing, while retaining checks for replacement,
  cross-category identity, and lookup after repeated updates. `indexmap` was already a workspace
  dependency of the statics crate, so the change adds no dependency.

### Measurements

- Five release checks used 113,672,192, 113,606,656, 113,917,952, 113,967,104, and 113,836,032 bytes
  peak RSS (median 113,836,032), down 2,392,064 bytes (-2.1%) from round 53. All warm samples took
  0.18s wall time and 0.16s user time; the first resource-accounting launch paid unrelated startup
  overhead but used the same memory range.
- The current-tree baseline is now down from 2,497,757,184 to 113,836,032 bytes (-95.4%).
- All 27 statics unit tests and 33 statics integration tests, all 151 session tests, all 9 CLI
  integration tests, all 31 Cajun unit tests, and all 9 Cajun stdio tests passed. Focused Clippy
  completed with existing unrelated warnings. The release build and full standard-library check
  also passed.

### Next

- Census the 4,064KiB scoped term payload by variant. A storage-only dense payload arena may be able
  to inline common small variants without changing the public `Term` enum, as the kind arena does.
- Inspect the 2,747KiB collection of per-source textual term tables and the 1,608/1,394KiB line-
  intention tables together. They share source-template lifetime; removing data duplicated by spans
  or tokens may be more valuable than shrinking another final static fact.
- Revisit the 3,152KiB page allocation only with stronger ownership evidence. Its old `env_type`
  symbol conflicts with the empty finished environment table, so layout matching remains more
  credible than treating the symbol name alone as a retained checker environment.

## 2026-08-15 — round 55: indirect rare surface term payloads

### Findings

- The final live-owner census identified the 4,064KiB scoped term allocation as the largest
  confirmed source-level owner. `bitter::Term<DefId>` occupied 64 bytes even though its most common
  variants carry only one or two IDs: the checked standard library contains 13,405 variables,
  10,073 internal nodes, 9,815 source boundaries, 8,670 applications, and 8,186 products among
  64,954 scoped terms.
- Six variants set the enum's 64-byte layout: `MetaT<TermId>` itself occupied 64 bytes, while
  manifest existentials, sequential binds, lets, mobile binds, and monadic blocks carried 48-byte
  payloads plus the discriminant. Only 3,488 standard-library terms (5.4%) use any of these variants,
  and two of the six are absent from the final scoped program.
- Indirecting those payloads makes the whole enum 48 bytes, a 25% slot reduction. The generic enum
  backs both desugared `Term<VarName>` and resolved `Term<DefId>` arenas, so the reduction applies to
  multiple phase products that coexist at the process high-water mark. The added boxes are paid
  only by rare variants.
- A final malloc-stack census found 115,831 live allocations owning 62.3MB; stack logging raised
  physical footprint to 168.1MB. The boxes add about 3,500 allocations and make the retained-live
  delta look modest, while normal process samples still fall by 3.5MB. The difference is expected:
  the final census sees one retained generation, whereas peak RSS sees several transient and
  retained desugared and resolved products using the smaller slot.
- The reusable rule is to evaluate enum indirection as `slot saving × all resident nodes × all live
  products`, then compare it with `box cost × rare nodes`. Looking only at one final arena misses the
  primary benefit when a shared syntax enum spans several pipeline stages.

### Changes

- Boxed the `Meta`, `ManifestExists`, `Do`, `Let`, `MobileBind`, and `MoBlock` payloads in
  `bitter::Term`. Manual `From` implementations preserve direct construction from the existing
  typed payload structs, while all pattern matches now cross the ownership boundary explicitly.
- Added a layout regression requiring `Term<DefId>` to remain at most 48 bytes. This guards against
  a future rare variant silently widening every surface term slot again.

### Measurements

- Five release checks used 110,329,856, 109,936,640, 109,985,792, 110,657,536, and 110,575,616 bytes
  peak RSS (median 110,329,856), down 3,506,176 bytes (-3.1%) from round 54. Warm samples took
  0.18--0.19s wall time and 0.17s user time; the first resource-accounting launch paid unrelated
  startup overhead.
- The current-tree baseline is now down from 2,497,757,184 to 110,329,856 bytes (-95.6%).
- All 147 surface tests, all 27 statics unit tests and 33 statics integration tests, all 151 session
  tests, all 9 CLI integration tests, all 31 Cajun unit tests, and all 9 Cajun stdio tests passed.
  Focused Clippy completed with existing unrelated warnings. The release build and full standard-
  library check also passed.

### Next

- Re-rank the remaining final owners. The largest visible dense source allocation is now about
  3,632KiB, followed by the unresolved 3,152KiB page attribution, 3,088KiB of assembled spans, and
  2,064KiB of term facts.
- Measure all pre-normalized static type variants before attempting another enum split. `Type`
  remains 56 bytes because four 48-byte payload families are inline; their frequency in retained
  normalized annotations alone is insufficient evidence for boxing across the checker arena.
- Inspect the 2,747KiB collection of per-source textual term tables together with the 1,608KiB and
  1,394KiB line-intention tables. They share parsed-template lifetime and may duplicate information
  already recoverable from tokens or spans.

## 2026-08-15 — round 56: remove generated-ID tail padding

### Findings

- The post-substitution checker now materializes only 53,969 pre-normalized type nodes for the
  standard library. The four 48-byte inline payload families account for 2,962 nodes (5.5%): 461
  type abstractions, 42 value foralls, 2,295 computation foralls, and 164 manifest kind entries.
  Boxing them would save eight bytes in each type slot but add about 3,000 allocations for a total
  type-arena opportunity below one megabyte.
- Each generated arena ID exposed a more general source of waste. Its identity consists of an
  eight-byte key space and a four-byte local slot, but the key space's eight-byte alignment gave
  every standalone ID four bytes of tail padding. The 16-byte physical representation propagated
  into hash keys, syntax payloads, binders, and typed dispatchers across every pipeline phase.
- Splitting the key-space word into high and low `u32`s preserves all 64 identity bits while lowering
  its storage alignment to four. A generated ID then occupies exactly twelve bytes. Recombination
  is a pair of shifts and preserves numeric ordering across the low-word boundary.
- The atomic change removes padding again at composite boundaries: `TypeBinder` falls from 32 to 24
  bytes, `ManifestKind` from 48 to 36, `Type` and `Fillable<Type>` from 56 to 48, and the full pattern
  and term dispatchers from 24 to 16. The common type slot therefore shrinks without any boxes.
- `TermAnnId` falls from 40 to 28 bytes. Its older 32-byte custom wrapper became larger than the
  ordinary typed enum, so retaining that local optimization would have defeated part of the base-
  representation win.
- `Option<Id>` remains 16 bytes because Rust cannot see the composite nonzero key-space invariant;
  this change does not regress its old niche-backed size. The reusable rule is to remove alignment
  waste from the atomic identity before introducing compact wrappers downstream, then re-audit every
  wrapper whose premise depended on the old layout.

### Changes

- Added `CompactKeySpaceId`, storing the exact high and low words of a `KeySpaceId`, and changed
  `new_key_type!` to place that representation beside the raw slot. The public `ArenaId` contract,
  allocator behavior, equality, ordering, hashing, debug forms, and reconstruction APIs are
  unchanged.
- Added upper-range round trips, ordering across a 32-bit word boundary, and exact size/alignment
  regressions for compact key spaces, generated IDs, and optional IDs.
- Removed `CompactTermAnnId` and store the now-smaller native `TermAnnId` directly in term facts.
  Updated the static type and dispatcher layout regressions to record the new representation floor.

### Measurements

- Five release checks used 102,203,392, 104,022,016, 103,809,024, 103,972,864, and 104,153,088 bytes
  peak RSS (median 103,972,864), down 6,356,992 bytes (-5.8%) from round 55. Warm runs took
  0.18--0.19s wall time and 0.16--0.17s user time; the first launch again paid startup latency.
- The current-tree baseline is now down from 2,497,757,184 to 103,972,864 bytes (-95.8%).
- All 30 utility unit tests and four utility documentation tests, all 147 surface tests, all 26
  statics unit tests and 33 statics integration tests, all 151 session tests, all 9 CLI integration
  tests, all 31 Cajun unit tests, and all 9 Cajun stdio tests passed. Focused Clippy completed with
  existing unrelated warnings. The release build and full standard-library check also passed.

### Next

- Remove the now-redundant category byte from each stored provenance key. Provenance is already
  sharded by typed category, so its `CompactTypedEntityId` needs the category only while selecting a
  shard; the 43,902 retained entries can use the native twelve-byte identity inside that shard.
- Capture a fresh live-owner census after the pervasive ID change. It should re-rank sparse maps and
  multi-ID syntax buffers more strongly than the earlier final-owner list predicts.
- Continue the parsed-template audit across textual term tables and line-intention tables. Their
  shared source lifetime remains the largest untested representation opportunity.

## 2026-08-15 — round 57: erase provenance categories inside category shards

### Findings

- Pattern and term provenance retain 43,902 representatives: 14,632 patterns and 29,270 terms.
  Round 54 already routed them into seven maps by typed category, but every key still repeated that
  category beside a key space and raw slot.
- After round 56, the old categorized key remained 16 bytes while each source ID became twelve.
  The key's eight-byte alignment made a `(key, source)` dense entry occupy 32 bytes. Within a shard,
  category is an invariant of the container rather than information belonging to each entry.
- A category-independent arena identity is twelve bytes. Pairing it with the twelve-byte source ID
  makes an entry 24 bytes, a 25% slot reduction with no semantic conversion or allocation. At the
  exact representative count, this removes 351,216 bytes before accounting for `IndexMap` capacity,
  transient growth, and simultaneously live phase products.
- The reusable rule is to use a discriminant once at a partition boundary and erase it inside the
  partition. The enclosing shard becomes the type proof; repeating the discriminant per element is
  both data duplication and, when it raises alignment, padding duplication.

### Changes

- Added `ArenaIdIdentity`, a category-independent wrapper around the padding-free key-space and raw
  slot. Its API explicitly requires an enclosing typed context, and its regression round-trips a
  generated ID while holding the twelve-byte layout.
- `SourceProvenance` now uses `ArenaIdIdentity` as each shard's stored key. `TypedEntityKey` exists
  only ephemerally to route a pattern or term dispatcher to its shard; it is never retained in the
  dense entry buffer.
- Extended the provenance layout regression to distinguish the 16-byte routing key from the
  twelve-byte stored identity while preserving replacement and post-compaction lookups.

### Measurements

- Five release checks used 102,678,528, 102,481,920, 102,465,536, 102,072,320, and 102,219,776 bytes
  peak RSS (median 102,465,536), down 1,507,328 bytes (-1.4%) from round 56. Warm runs took
  0.18--0.19s wall time and 0.17s user time.
- The current-tree baseline is now down from 2,497,757,184 to 102,465,536 bytes (-95.9%).
- All 30 utility unit tests and four utility documentation tests, all 26 statics unit tests and 33
  statics integration tests, and all 151 session tests passed. Focused Clippy completed with existing
  unrelated warnings. The release build and full standard-library check also passed.

### Next

- Capture a fresh malloc-stack census. The pervasive ID reduction and the 32-to-24-byte provenance
  entry transition should materially reorder sparse maps, dense syntax vectors, and source-template
  owners relative to the round-55 census.
- Audit parsed-template ownership as one unit: textual term tables, token/span provenance, and the
  two line-intention maps all share source lifetime and may retain overlapping location data.
- Inspect optional generated IDs only where a census identifies a concrete owner. Their physical
  size remains 16 bytes because Rust cannot infer the composite zero-key-space niche, but a custom
  optional representation is worthwhile only for a large dense field.

## 2026-08-15 — round 58: let transient type environments grow from evidence

### Findings

- A fresh malloc-stack census after round 57 found 115,630 live allocations owning 55.3MB. Stack
  logging raised physical footprint to 160MB, so the uninstrumented release measurements remain the
  process baseline. The largest source-linear owners were assembled spans at 3,162,112 bytes, the
  scoped term payload at 3,129,344, term facts at 1,851,392, compact pre-normalization kinds and one
  Salsa edge buffer at 1,392,640 each, scoped patterns and normalized annotation payloads at
  1,310,720 each, value patterns at 1,081,344, and textual origins at 1,064,960.
- One 2,179,072-byte page-directory allocation was again symbolized through `env_type` reservation.
  The final ownership label remains ambiguous because checker stripping empties that table before
  publication, but peak lifetime is a separate question: `Tycker::new` reserves the directory before
  judgments and holds it until checking completes. A transient table can therefore raise the process
  high-water mark without surviving in the finished arena.
- The reservation estimated type-environment key spaces as half of the 64,954 scoped terms, or
  32,477 pages. A temporary pre-strip probe measured the actual table at 53,969 entries spread over
  26,689 pages. With the estimate, `HashMap::reserve` selected capacity 57,344; growing from actual
  inserts selected capacity 28,672. The estimate was only 21.7% above the final page count, but it
  crossed a discrete growth boundary and doubled the directory.
- This sharpens the reservation rule: reserve from an exact or conservative lower-bound domain when
  storage is retained and dense, but do not transfer a proxy estimate into a transient sparse hash
  table. Hash-table capacity classes amplify small estimate errors, while organic growth already
  tracks the observed key domain.

### Changes

- Removed speculative outer-page reservation from `env_type`. Type environments retain the same
  paged representation and insertion semantics and now allocate only for key spaces checking
  actually visits.
- Kept the source-derived reservation for the retained pre-normalization type arena and exact ID
  reservation for term facts. Updated the reservation contract to distinguish those lasting stores
  from checker-transient environments.

### Measurements

- Five release checks used 99,614,720, 99,663,872, 100,073,472, 99,713,024, and 99,532,800 bytes peak
  RSS (median 99,663,872), down 2,801,664 bytes (-2.7%) from round 57. Every sample took 0.18s wall
  time and 0.16--0.17s user time.
- The current-tree baseline is now down from 2,497,757,184 to 99,663,872 bytes (-96.0%).
- All 26 statics unit tests and 33 statics integration tests and all 151 session tests passed.
  Focused Clippy completed with existing unrelated warnings. Formatting, the release CLI build, and
  the full standard-library check also passed.

### Next

- Re-rank the fresh owners after excluding the environment directory. The largest remaining mutable
  typed store is the 1,851,392-byte term-facts payload; audit whether its final dispatcher can be
  split by annotation category without duplicating source identity.
- Audit the parsed source template as one lifetime group. Assembled spans, textual terms, tokens, and
  line-intention maps may repeat source location structure even though each individual owner is now
  bounded.
- Treat the 3.13MB scoped term payload as a representation floor unless a variant census identifies
  enough rare wide payloads to move its 48-byte enum into a lower capacity class.

## 2026-08-15 — round 59: audit classifier duplication in term facts

### Findings

- The next retained typed owner is the 1,851,392-byte `Vec<TermFacts>`. Its 63,574 final records are
  4,910 kind terms, 54,400 type terms, 2,696 value terms, and 1,568 computation terms; no hole facts
  survive the successful standard-library check. Each record currently stores a 28-byte
  `TermAnnId`.
- A temporary pre-strip census found 8,556 distinct kind classifiers among the type facts and 2,130
  distinct type classifiers among the value and computation facts. Interning classifiers would
  remove repetition, but the canonical typed arenas provide a stronger possible representation.
- Every final value and computation fact's classifier exactly matched the annotation already stored
  under its `ValueId` or `CompuId`. Type facts differ: 9,309 of 54,400 reported kind classifiers did
  not match the kind co-located with their `TypeId`. Those differences are observable and rule out
  erasing all secondary IDs from term facts.
- A lossless compact design can store one twelve-byte category-independent typed identity plus a
  four-byte word containing the term category and an optional one-based override index. Canonical
  classifiers need no side payload; only the 9,309 exceptional type classifiers need a dense
  `KindId` override. On this workload, the logical payload would fall from 1,780,072 bytes to
  1,128,892 bytes before vector capacity, a 651,180-byte (36.6%) reduction.
- The reusable rule is to distinguish duplicated facts from exceptions to a canonical fact. Erasing
  the duplicated common case is sound only when the exceptional path remains explicit and typed;
  the 9,309 mismatches show why a census of equality, not just identifier cardinality, is required.

### Handoff

- The probe was removed after collecting the counts; no diagnostic environment variable or output
  remains in production code.
- If this owner is pursued, encode the category and override index in a typed metadata wrapper and
  reconstruct public `TermAnnId`s at the `StaticsArena` boundary. Regressions should cover all five
  categories, noncanonical type kinds, missing canonical tables in rejected checks, and replacement
  without unbounded orphaned override entries.
- The other measured follow-up remains the parsed-source lifetime group: spans, textual syntax,
  tokens, and line-intention maps should be audited together for repeated location structure.
