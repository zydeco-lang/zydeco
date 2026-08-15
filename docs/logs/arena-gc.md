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
