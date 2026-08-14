# Query-Based Type Checking Worklog

## 2026-08-14 — plan and P1: salsa query spine, behavior-identical

### Plan

Type checking (`lang/statics/src/check`) is one monolithic forward pass: a single
`Tycker` driver mutates a `StaticsArena`, an error list, and a task stack, and walks the
whole scoped program on every analysis. The session layer is already salsa-based, but its
type-checking query has whole-file granularity, so one edited definition re-checks
everything and consumers cannot ask for one fact without paying for the whole pass.

The plan is to decompose type checking into demand-driven, memoized queries keyed by
scoped identifiers. Query inventory derived from the existing entry points, in migration
order:

```text
entry       check_source -> TyckOutput
judgment    kind_of / type_of / value_of / compu_of : su::TermId
            pat / copat : su::PatId / CoPatId x TyEnv
            lub, elaborate_monadic, data_of, codata_of
normalize   normalize_kind / normalize_type / unroll / subst / whnf
holes       fill_solution : FillId -> Option<AnnId>
validate    coverage : DataId -> Vec<CoverageError>
diagnostics reports(source) -> TyckReports
            blame_stack(term) -> Vec<TyckTask>
```

Decisions taken up front:

1. The external representation stays (`StaticsArena` + `TermAnnId`); the query layer is
   an internal organization, and downstream backends are untouched.
2. Judgment granularity is per node, keyed by scoped IDs, which are `Copy + Eq + Hash`.
3. Errors become query values (`Result<T, Vec<TyckErrorEntry>>`-ish) instead of a global
   writer list, retiring `_k` per judgment as it migrates.
4. Blame stacks become derived from salsa's active-query stack rather than snapshotted.
5. Cycles use the existing `RecGroup` pre-introduction strategy plus salsa cycle recovery.
6. Expectations stay out of query keys; queries synthesize, and holes carry deferred
   expectations.

Migration phases: P0 inventory and freeze, P1 salsa spine (this entry), P2 normalization
queries, P3 deterministic fresh IDs + `fill_solution`, P4 judgment layer by syntax
category, P5 recursion/validation/derived diagnostics, P6 session and editor wiring,
P7 benchmarks. Risks: diagnostic-text stability (P5 must diff against current output),
salsa per-query overhead (P7 guards), and the `no_eq` + `unsafe(non_update_types)`
stopgap on query results until they gain structural equality.

### Implementation

Survey facts that shaped the design:

- salsa 0.26 input-struct fields must be `Eq + Hash` and offer no `#[no_eq]`; the arenas
  (`SpanArena`, `ScopedArena`, `PrimDefs`) are not `Eq`, so they cannot be salsa inputs.
- `Tycker` writes the scoped arena during elaboration through the generic `Alloc`
  machinery (`AsMut::<ScopedArena>::as_mut(arena).insert_def(..)`), not through field
  syntax, so an earlier read-only conclusion was wrong; the query clones its input arena
  and returns the post-check arena.

Attempts and failures:

- First attempt: a separate `TyckSession` database holding the input as a plain field.
  Rejected in practice: salsa panics with "Cannot change database mid-query" because the
  session's `analyze_source` query cannot attach a second database on the same thread.
  Lesson: type-checking queries must join the session's salsa graph.

Landed design:

- `lang/statics/src/query.rs` declares a `TyckDb` salsa supertrait, a `ScopedData<'db>`
  tracked struct carrying the three arenas plus the root, and the tracked
  `check_source(db, data)` query. The session's `SourceQueryDb` extends `TyckDb`, so
  everything runs in one database.
- salsa 0.26 tracked-struct details discovered along the way: field attributes are
  separate (`#[tracked] #[no_eq] #[returns(ref)]`), not nested; `#[no_eq]` substitutes
  `always_update`, which is how non-`Eq` fields avoid the `Update`/`PartialEq`
  requirement; field types must be `Clone` (storage cloning), so `SingCell` and
  `PrimDefs` gained `Clone`; `ariadne::Report` is not `Clone`, so `TyckReports` became
  `Arc<Vec<Report>>`.
- `analyze_source` now builds `ScopedData` and consumes `check_source`; the direct
  `Tycker` call is gone from the session path. `check_source` still runs the wholesale
  `Tycker` internally, as planned for P1.

Verification: full workspace tests green; behavior unchanged, including cajun analysis
tests and the session reuse test (`Arc::ptr_eq` across repeated analyses).

### Unresolved

- The judgment layer still executes wholesale inside `check_source`; the per-node producer migration (P4) remains.
- Fresh `StaticsScope` IDs still come from a sequential `IdAllocator`; making them deterministic per input is
  prerequisite work for the judgment layer (P3/P4).
- Whether per-query salsa overhead regresses whole-check time is unmeasured until P7.

## 2026-08-14 — P2–P6: demand-driven fact layer, measured

- Landed the demand-driven fact queries in `lang/session/src/source/query.rs`, each memoized per
  `(root input, interned key)` and reusing the memoized `analyze_source`:
  `normalized_type`, `coverage`, `fill_solution`, `annotation_of_def`, `type_definition_of_def`,
  `annotation_of_term`. Interned key wrappers live in `lang/statics/src/query.rs`
  (`InternedType`/`InternedKind`/`InternedDef`/`InternedTerm`/`InternedFill`) because salsa query
  arguments must be salsa IDs; plain `Eq + Hash` node IDs do not qualify.
- `annotation_of_term` reconstructs `TermAnnId` from the `terms` provenance map and the per-sort
  annotation tables. Direction lesson: `ArenaBipartite::back` maps typed nodes to source terms, so the
  scoped-to-typed direction uses `forth`.
- Measured reuse: first `analyze` of a small root takes ~2.8 ms; the repeated, unchanged analyze takes
  ~52 µs (about 55×), confirming the memoization behavior already asserted by `Arc::ptr_eq` in the
  session reuse test.
- Attempted to make cajun's hover consume the fact queries by storing a session snapshot in
  `ProjectState`. Reverted after a diagnosed hang: the LSP `refresh` path re-analyzes and replaces the
  cached project on every request, and running salsa queries on the multi-threaded tokio runtime while
  holding a `std::sync::Mutex` over the snapshot deadlocks
  (`stdio_server_synchronizes_documents_and_answers_navigation_requests` hung with every thread parked;
  a probe run showed the second hover's `refresh` as the last event). Lessons recorded:
  - per-request consumers must not hold salsa sessions across the refresh/replacement cycle;
  - instrumenting the LSP binary with `eprintln!` probes plus `sample` on the hung process is an
    effective diagnosis path for protocol-level hangs.
- The cajun consumer wiring is deferred until the refresh design stops re-analyzing on every request;
  the fact queries remain the public session API for that future consumer.

## 2026-08-14 — P4 producer: derived identifier scheme (decision A)

- The user chose plan A: migrate the judgment layer into per-node producer queries. Design analysis
  before implementation:
  - Packing `(entity, occurrence, slot)` into the 32-bit raw index is too tight for realistic entity
    counts and cannot express occurrence cleanly; salsa-interned typed IDs are blocked because
    `ArenaId` requires the private `ArenaIdToken` and every arena and downstream backend expects
    `new_key_type` IDs.
  - Chosen scheme: one derived **key space per allocation site**. `KeySpaceId::derive(tag, entity,
    occurrence)` mixes the site identity into a 64-bit key space, and `derived_id(key_space, slot)`
    builds the identifier with the local allocation slot as the raw index. Re-executing a query
    reproduces its identifiers without a shared cursor; distinct sites can never collide because
    their key spaces differ. Both constructors live in `zydeco-utils` (`lang/utils/src/arena.rs`)
    with tests.
  - Phase boundary: dense arena categories (`AbstId`, `FillId`, `DataId`, `CoDataId`) issue their own
    `la-arena` raw indices, so they stay on the per-check allocator until `ArenaDense` grows a
    derived-insert path. The first migration slices therefore cover the sparse categories
    (`KindId`, `KPatId`, `TPatId`, `TypeId`, `VPatId`, `ValueId`, `CompuId`).

## 2026-08-14 — P4 producer: derived allocation sites, wired into the checker

- Swapped the `Tycker`'s sequential `IdAllocator` for a `DerivedAllocator` that pushes
  `(entity_space, entity_raw, occurrence)` sites around every scoped term and pattern check and
  issues fresh sparse identifiers from the top site with a site-local slot. The wholesale check
  is still one query, so this is a behavior-neutral refactor with new identifier values; the full
  workspace suite passes unchanged, including diagnostics that embed `concise()` suffixes.
- Two collision bugs found and fixed during the swap, each worth remembering:
  - The scoped identifiers of different categories (patterns vs terms) come from different
    allocators, so their raw indices overlap across categories. Deriving from the raw index alone
    collides (`entity=4 occurrence=0 slot=0` issued twice — one pattern, one term). The site
    identity must be the full `(key_space, raw)` pair of the source entity, not the raw index.
  - The root site `(0, 0)` collided with the first real entity's first check; the root now uses a
    sentinel identity.
  - Occurrence counts checks via a per-check counter table (`Tycker::check_counts`) rather than
    provenance records, because hole-producing checks record no provenance and would otherwise
    reuse a site on re-check.
- Diagnosis notes: an env-gated `HashSet` probe in `DerivedAllocator::fresh` that panics at the
  second issue of an identifier was far more effective than backtraces at pinning down the
  colliding pair; `RUST_BACKTRACE=1` showed the allocation path but not the colliding identity.

## Next step

- Decompose the judgment layer into producer queries keyed by interned scoped entities:
  `env_of_term` (the `TyEnvT` threading as a query DAG) feeding `tyck_term`, `tyck_pat`, and the
  sort judgments, with `TyckErrorEntry` lists returned as values. The derived allocator makes the
  identifiers reproducible per site, so judgment queries can construct their output nodes without
  a shared cursor. `AbstId`/`FillId` allocation still binds the first slices to constructs that do
  not seal types or introduce holes.

## 2026-08-14 — first producer query: intrinsic kind judgments

- The first salsa-produced judgment is live: `internal_kind_judgment(db, data, term)` produces the
  `VType`/`CType` kind node for `Internal` terms, and the checker's `InternalTerm::tyck_k` branch
  materializes the returned node instead of allocating it in context. `Tycker` now carries
  `db: &dyn TyckDb` and `data: ScopedData`, threading the salsa graph into the whole pass.
- Two mechanisms this step required, both worth remembering:
  - Query-produced identifiers use a separate derivation tag (`QUERY_DERIVATION_TAG`) so they can
    never collide with checker-allocated identifiers at the same site; the two slot counters are
    independent, so sharing one tag family would double-issue slot zero.
  - Salsa forbids creating tracked structs outside an active query ("cannot create a tracked
    struct disambiguator outside of a tracked function"), so programs assembled outside the source
    pipeline (tests, tools) cross into the graph through a `PendingParts` slot on the database:
    the caller fills `pending_parts`, then `intern_pending(db)` runs as a query, takes the slot,
    and builds the `ScopedData` tracked struct inside the query. `CompilerSession::check_resolved`
    wraps this for session consumers; the `std::sync::Mutex` slot must be `Arc`-wrapped because
    `Mutex` is not `Clone` and salsa databases are.
- The session's `ScopedProgram::check` test helper now goes through `CompilerSession::check_resolved`,
  so the session tests exercise the query pipeline end to end instead of constructing a `Tycker`
  directly.
- Workspace suite passes unchanged (61 targets green).

## 2026-08-14 — intrinsic type judgments through the query

- Extended the intrinsic judgment to the type-producing variants: `Unit`, `Thk`, `Ret`, and the
  primitive types now come from `internal_judgment(db, data, term, env)`, which returns a small
  node DAG (`InternalJudgment::Type { kinds, ty, ann }`). The checker materializes the fragment —
  kinds, type, `annotations_type`, and `env_type` — exactly as in-context `Alloc::alloc` recorded
  them.
- Two facts that shaped this slice:
  - `Construct::build` caches the intrinsic type singletons in `IntrinsicStatics` (one `Thk` type
    node per check, reused by every later site). The checker keeps that cache; the query only runs
    on a cache miss and produces the fresh nodes. The materializer must therefore stay the sole
    writer of the intrinsics cache.
  - `env_type` records feed normalization's free-variable and skolem-scope resolution, so the
    query takes the caller's environment as an `EnvData` tracked struct and the materializer
    records `env.clone()` alongside each type.
- `OS` stays checker-side for now: its resolution reads builtin roles from the arena, which is
  still being built during the check and cannot be read by a query.
- Workspace suite passes unchanged (61 targets green).

## 2026-08-14 — error judgments as query values, and the arena-read wall

- The `Monad`/`Algebra` arms now ride the query result as `InternalJudgment::Error(TyckError)`;
  the checker routes the returned error into the `err_k` writer. This is the first instance of the
  planned `err_k`-retirement pattern: decisions move into queries, the writer becomes a sink.
- Attempted to migrate the `Var` judgment next and hit a wall worth recording precisely: most
  remaining judgments read arena state that the check itself is still building —
  `annotations_var` for `Var`, `kinds_pre`/`types_pre` for unification, `env_type` for
  normalization, builtin roles for `OS`. A salsa query cannot read that mid-check state, and
  snapshotting it per call would defeat memoization. The queryable subset is therefore exactly the
  judgments that depend only on `(data, term, env, switch)`: intrinsic leaves today, plus any
  future judgment whose inputs are threaded through `EnvData`-style carriers.
- Implication for the remaining migration: the producer layer cannot go all the way without a
  deeper redesign in which the typed arena becomes query-owned state (interning typed nodes as
  salsa values and assembling the arena at the materialization boundary). That redesign touches
  the `new_key_type` foundation and every downstream backend, so it stays parked behind an
  explicit decision; the worklog keeps the wall documented as the boundary of the current
  architecture.

## 2026-08-14 — P6: cajun consumes the fact queries; refresh gets a fast path

- Fixed the root cause that had blocked the cajun wiring: `refresh_with_progress` re-analyzed the
  whole project on every LSP request. It now takes a fast path — an unchanged open document
  reuses its cached `ProjectState` — so repeated hovers, symbols, and navigation requests are
  pure cache reads. The stdio suite dropped from a hanging run to ~3.4 s for all eight tests.
- Hover is now wired through the session's memoized fact queries: `ProjectState` records its
  analyzed root, and `hover` answers the annotation and type-definition lookups via
  `CompilerSession::annotation_of_def` / `type_definition_of_def` against the live session. The
  previous attempt stored a session snapshot inside `ProjectState` behind a `std::sync::Mutex` and
  hung the LSP; this design keeps no stored session and locks only the tokio session and projects
  guards, in a consistent order, around memoized reads.
- The analysis test helpers (`ProjectState::load`) now return the session alongside the project so
  hover tests exercise the query path.
- Workspace suite passes unchanged (61 targets green), clippy clean.

## 2026-08-14 — probing the wall: `Var` stays arena-bound

- Tested whether the `Var` judgment could read its annotation from the environment instead of the
  arena. It cannot: the binder check (`Pat::Var`) writes `annotations_var` through
  `insert_or_get` followed by `Lub::lub_k` and `replace_existing` when a definition binds multiple
  times, so the arena annotation can diverge from the environment's `AnnId`. The wall is therefore
  not an artifact of the current query keys but of genuinely mutable mid-check arena state.
- The arena-read wall now bounds the producer migration definitively: the queryable layer is
  complete for this architecture (intrinsic judgments plus the demand-driven fact queries), and
  the remaining judgment migration requires the redesign decision (typed arena as query-owned
  state, touching `new_key_type` and the downstream backends).

## 2026-08-14 — query-owned statics: first table, the intrinsic singletons

- The user approved the query-owned-statics redesign; the design now lives in
  `docs/ideas/query-owned-statics.md` (four table-conversion patterns, fill-before-read
  invariant, materialization boundary, migration order).
- First conversion landed: `IntrinsicStatics` is query-owned. `intrinsic_singleton(db, data, key)`
  produces each singleton at a synthetic site discriminated by the key, and
  `check_source_outcome` materializes all of them (`vtype`, `ctype`, `thk`, `ret`, `unit`, all 13
  primitives) before any judgment runs. `InternalTerm` went back to plain cache reads through
  `Construct::build`; only the `Monad`/`Algebra` rejection still rides a query result. The
  checker-side singleton cache mutation is gone from the judgment path.
- This also corrected a semantic drift from the per-site judgment migration: `VType`/`CType`
  kinds and the intrinsic types are now single nodes per check again (the original
  `Construct::build` semantics), instead of one node per `Internal` term site.
- `env_type` records for the intrinsic types are materialized with the default environment: the
  nodes are closed, so normalization's free-variable and skolem lookups never observe them.
- Workspace suite passes unchanged (61 targets green), clippy clean.

## 2026-08-14 — phase granularity: the four-query checking chain

- Split the wholesale check into a four-query chain, each phase reading the previous phase's
  stable arena through a tracked struct:
  `tyck_judgments` -> `Judgments` -> `resolve_holes_phase` -> `Resolved` -> `finish_checked`
  (normalize + coverage). `check_source` is now the composition; the session API is unchanged.
- The finish phase itself split in the checker: `resolve_holes_and_collect` (hole solving +
  solution observation) and `normalize_and_validate_k` (normalization loops + coverage) are
  separate steps, with `finish_check_k` kept as their composition for direct callers.
- Two carry-over requirements discovered and solved during the split: the derived allocator's
  root-site slot counter must ride the phase state (`root_slot` on `Judgments`/`Resolved`), or
  the next phase's normalization allocations would re-issue the previous phase's identifiers;
  and the resumed `Tycker` must restore `errors`, `observations`, and the arena so reports and
  the writer monad behave exactly as the combined pass.
- This gives normalization its own query phase over a stable arena (the P2 goal at phase
  granularity) and establishes the keying pattern the table-level fill/normalize queries will
  use.
- Workspace suite passes unchanged (61 targets green), clippy clean.

## 2026-08-14 — the dense boundary falls: derived fill identifiers

- `fills` moved from `ArenaDense` to `ArenaSparse` with site-derived identifiers, so `FillId`
  allocation no longer depends on the la-arena cursor. Every sparse category now derives its
  identifiers from `(site, slot)`, and the design doc's dense-category boundary shrinks to
  `AbstId`/`DataId`/`CoDataId` (which remain dense for now).
- This was the last allocation class blocking fill-state queries: hole-filling sites are now
  reproducible per site, so `Fillable` states and the `annotations_var` merge fold can be
  computed as pure queries.
- Mechanical fallout fixed along the way: `InferenceRegion` and the monadic elaboration
  allocated fills through the dense `alloc`; both now use the derived allocator plus
  `insert_new`. Test fixtures switched to the `Alloc` machinery with explicit
  `InferenceSite` construction.
- Workspace suite passes unchanged (61 targets green), clippy clean.

## 2026-08-14 — the dense boundary is gone entirely

- `absts`, `datas`, and `codatas` joined `fills` on the sparse, site-derived scheme, so every
  typed identifier category (`KindId` through `CoDataId`) now derives from `(site, slot)`.
  Nothing in the checker observes la-arena cursors anymore; the entire `StaticsArena` is
  reproducible by pure computation from the source program, which was the last prerequisite
  for rewriting the judgment recursion as queries.
- Key-check before the conversion: no pass iterates these arenas (only keyed lookups), so the
  HashMap iteration order cannot leak into diagnostics or resolution order.
- Workspace suite passes unchanged (61 targets green), clippy clean.

## 2026-08-14 — first term judgment on the query graph: literal synthesis

- The `Tm::Lit` Syn path is the first whole-term judgment produced by a salsa query:
  `literal_syn_judgment(db, data, term) -> Option<LiteralSynOutcome>`. It computes the
  range-checked literal and its primitive singleton type without reading the arena: the type
  comes from the already query-owned `intrinsic_singleton`, and the value identifier is derived
  at the term's site under `QUERY_DERIVATION_TAG`.
- The checker branch now splits by mode. The Syn arm materializes the query outcome into
  `values` / `annotations_value` / `env_value` exactly as `Alloc::alloc` did (the environment
  record is the caller's cloned environment); the Ana arm keeps the arena-touching logic —
  `primitive_type` probing and `lub` — with its Syn sub-arms now `unreachable!()`.
- A semantic simplification falls out for free: literal types are now one shared intrinsic
  node per check instead of one fresh `PrimitiveTy` node per literal site. The nodes are
  structurally identical and closed, so the default environment record on the shared node is
  unobservable.
- Repeat checks of the same literal re-materialize the same value identifier: the Syn
  judgment runs once per term (occurrence is always zero), so the inserts are idempotent
  where the old per-occurrence allocation minted a fresh node each time.
- Two pre-existing clippy lints fixed along the way (`new_without_default` on
  `DerivedAllocator`, `clone_on_copy` in the hole-resolution query).
- Workspace suite passes unchanged (61 targets green), clippy clean.

## 2026-08-14 — hole and trivial judgments join the query graph

- `Tm::Hole` Syn and `Tm::Triv` Syn are query-produced now: `term_hole_syn_judgment`
  derives the stand-in fill at the term's site, and `triv_syn_judgment` shares the
  query-owned unit singleton as its type instead of building a fresh `UnitTy` node per site.
- Both materializers reproduce the old allocation exactly: the hole materializer records
  the term's `InferenceSite` in `fills`, and the triv materializer records
  `values` / `annotations_value` / `env_value` with the caller's cloned environment.
- The checker keeps the Ana arms (they read the arena through `lub` and the kind probe),
  so the Ana side stays put until the fill-state pattern lands; each branch's Syn arm is
  now the query materializer.
- All producer queries derive at their own term's site, so the query family's key spaces
  stay disjoint per term without any inter-producer coordination.
- Workspace suite passes unchanged (61 targets green), clippy clean.

## 2026-08-14 — the variable judgment takes the fold state as a query input

- `Tm::Var` is now judged by `var_syn_judgment(db, data, env, term, annotation)`: the
  `AnnId::Set` arm is a pure environment lookup returning the kind, and the `AnnId::Type` arm
  derives the `Value::Var` node at the term's site under `QUERY_DERIVATION_TAG`. The checker
  materializes `values` / `annotations_value` / `env_value` with the caller's cloned
  environment, and the `AnnId::Kind` arm keeps its checker-side `recursively_get_type` walk
  (it reads `types_pre` through the recursive-alias chain, so it stays until the fill-state
  pattern lands).
- The merge-fold cell enters the query as an input (`InternedAnn`): `annotations_var` itself
  is still checker-owned, because its binder-side contributions come from the pattern DAG,
  whose Ana annotations are judgment results. Once the binder constructs migrate, the fold
  flips from input to query-produced without changing the judgment's shape.
- Both the Syn and Ana paths route through the query for the two pure arms: the Ana path's
  `lub` result is just a different annotation input, so the arm split is by annotation shape
  rather than by mode.
- Workspace suite passes unchanged (61 targets green), clippy clean.

## 2026-08-14 — pattern leaves join the query graph

- `Pat::Hole` Syn became an error-only query (`pat_hole_syn_judgment` returns the
  `MissingAnnotation` rejection, matching the `internal_judgment` shape), and `Pat::Triv` Syn
  is now produced by `pat_triv_syn_judgment` on the shared unit singleton, deriving the
  value-pattern node at the pattern's site.
- The pattern materializer records `vpats` / `annotations_vpat` / `env_vpat` exactly as the
  pattern allocator did; the Ana arms keep their `lub` path, so the triv pattern's mode split
  mirrors the triv term's.
- The term and pattern Syn leaves are now exhausted: every remaining construct (Named, Label,
  Ann, Cons, Abs, App, the pack-pi family, and the pattern counterparts) is composite — its
  outer allocation consumes inner judgment results — and the variable's kind arm plus all Ana
  paths wait on the fill-state pattern. The next slices are therefore DAG-shaped rather than
  leaf-shaped: either the fill-state conversion (making `lub` query-able, which unblocks the
  fold's merge step and every Ana arm) or the first composite construct keyed on its inner
  results.
- Workspace suite passes unchanged (61 targets green), clippy clean.

## 2026-08-14 — first composite judgments keyed on inner results

- `Tm::Named` and `Tm::Label` are the first composite constructs on the query graph: the
  checker still drives the recursion (inner checks stay checker-side until their own slices
  land), but the outer judgment is now `named_syn_judgment` / `label_syn_judgment` keyed on
  the interned inner `TermAnnId` (`InternedTermAnn`). The lub-free arms and the outer
  rejections moved into the queries: named allocates the label kind and the named type node
  (slots 0 and 1 at the term's site), label allocates its kind node, and both return the
  `MissingAnnotation` / `SortMismatch` rejections as values — the first `err_k`-to-return
  conversions for judgments whose errors live at the outer level.
- The lub-dependent arms stay checker-side: named values (constructor-ish named terms) and
  label types both read the arena through `lub`, so they remain until the fill-state pattern
  converts `types_pre` reads into `fill_state` queries.
- `Pat::Named` followed the same recipe (`pat_named_syn_judgment`): the type-pattern arm
  allocates the label kind and the named `TPat` node, and the kind arm's expressivity
  rejection became a query return value; the value-pattern arm keeps its `lub` path.
- Workspace suite passes unchanged (61 targets green), clippy clean.

## 2026-08-14 — consumed judgments allocate through a query

- `Tm::Cons` Syn is now `cons_syn_judgment` keyed on the interned item and tail outcomes
  (`InternedConsItems`, `InternedTermAnn`): the right-nested product chain annotates against
  the shared vtype singleton and the consumed value node derives at the term's site, so the
  whole allocation block left the checker. The per-item sort rejections stay at their
  checker-side abort points — they happen mid-fold over item outcomes, and moving them would
  change when later items get checked (observable in arena contents) — so the query only ever
  receives value outcomes.
- This establishes the two-tier composite pattern for the remaining constructs: outer
  allocations and derived ids move into queries keyed on inner results, while errors that
  abort mid-fold keep their exact checker-side timing until the fold itself migrates.
- Workspace suite passes unchanged (61 targets green), clippy clean.

## 2026-08-14 — consumed patterns allocate through a query

- `Pat::Cons` Syn followed the consumed term: `pat_cons_syn_judgment` keyed on the interned
  item and tail pattern outcomes (`InternedPatItems`, `InternedPatAnn`) builds the product
  chain and the consumed value-pattern node at the pattern's site. The product nodes record
  the thread-accumulated pattern environment, while the pattern node records the outer
  environment — the materializer clones each exactly as the allocator did.
- Every remaining Syn arm now reads the arena through `lub`, `type_filled_k`, or
  `try_destruct_def` (Abs, App, Fix, Pi, Sigma, ManifestExists, Thunk, Force, Ret, Do, Let,
  the pack-pi family, the data declarations, and the pattern counterparts). The next slice is
  therefore the fill-state conversion: `fill_state(db, data, site)` over `types_pre`/
  `kinds_pre`, which turns those reads into query calls and unblocks both the Ana paths and
  the `annotations_var` merge step.
- Workspace suite passes unchanged (61 targets green), clippy clean.

## 2026-08-14 — the monadic core allocates through queries

- `thunk_judgment` / `ret_judgment` / `force_judgment` moved the CBPV monadic tails onto the
  query graph. Thunk and return build their type nodes as applications of the query-owned
  thunk/return singletons to the checked body type (annotated by the shared vtype singleton),
  so both modes of each construct share one query keyed on the checked body; force allocates
  its computation node keyed on the body and the destructured force type.
- The checker keeps the inference machinery (thunk/return holes, the lub, and the
  `type_filled_k` destructure) until the fill-state conversion turns those reads into
  `fill_state` query calls; the queries receive their results as inputs.
- Workspace suite passes unchanged (61 targets green), clippy clean.

## 2026-08-14 — applications and fixpoints allocate through queries

- `app_judgment` covers all four application tails (value and computation applications, term
  and type arguments) from one interned input (`InternedAppInput` carrying the kind
  discriminator, the recorded annotation, and the reported type). The polymorphic
  computation application's quirk — the node records the lub'd type while the judgment
  reports the substituted body type — is preserved explicitly via the two fields.
- `fix_judgment` allocates the fix computation node keyed on the checked binder, body, and
  result type. The checker keeps the arrow/forall destructuring and the argument checks,
  which read the arena until the fill-state conversion lands.
- Workspace suite passes unchanged (61 targets green), clippy clean.

## 2026-08-14 — the first fill-state content rides a query

- `hole_ana_judgment` is the seed of the fill-state pattern: the analyzed hole's stand-in fill
  and its `Fillable::Fill` type pre-node derive at the term's site, so a query now produces
  fill-state content (`HoleAnaKind::Type`), while the resolution side effects (`fill_k`'s
  solution write, `fill_hints`, and the `fill_scopes` bookkeeping) stay checker-side. The
  value and computation arms derive the hole value/computation nodes the same way.
- `Pat::Ctor` and `Pat::Alias` Syn became error-only queries returning `MissingAnnotation`,
  completing the pattern-level error arms alongside `Pat::Hole`.
- Workspace suite passes unchanged (61 targets green), clippy clean.

## 2026-08-14 — occurrence-aware derivation: fixpoint re-checks caught a collision

- The cajun stdio suite caught a real derived-id collision: `duplicate key in sparse
  arena` on a pi judgment in `lib/std/std.zy`. The backtrace showed `FixPoint`/`RecGroup`
  re-checking the same binding bodies, which increments the checker's per-entity occurrence —
  and the producer queries had been deriving their identifiers with occurrence hard-coded to
  zero. Two checks of the same entity therefore minted the same `KindId`.
- Fix: `DerivedAllocator::current_site` exposes the innermost `(space, raw, occurrence)`
  triple, `Tycker::site_occurrence` reads it, and every producer query now takes
  `occurrence: u32` as its last key ingredient, deriving under
  `QUERY_DERIVATION_TAG(space, raw, occurrence)` exactly as the checker allocator does under
  its own tag. Re-checked entities get distinct identifiers; the terms back-map keeps
  pointing at the last check's nodes, matching the original fresh-per-occurrence behavior.
- This corrects the earlier claim that entities are checked once: recursion-group fixpoint
  retries re-check bindings, so occurrence is live state, not vestigial bookkeeping.
- `sigma_syn_judgment` also landed in this batch: the existential and product tails derive at
  the term's site (annotated by the shared vtype singleton), and the kind arm's expressivity
  rejection became a return value.
- Workspace suite passes unchanged (61 targets green, including the cajun stdio test three
  times in a row), clippy clean.
