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
