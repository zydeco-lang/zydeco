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

- Fresh `StaticsScope` IDs still come from a sequential `IdAllocator`; making them
  deterministic per input is prerequisite work for the judgment layer (P3/P4).
- Whether per-query salsa overhead regresses whole-check time is unmeasured until P7.
