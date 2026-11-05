# 02 — Statics (checker, inference, elaboration, queries, arenas)

> Scratch study note for re-onboarding. Not canonical: contracts below point to their authoritative references.

## 1. Responsibility

`lang/statics` owns the C5–C6 phases: it consumes the name-resolved scoped program and publishes a typed arena with a
checked or rejected outcome. Within that boundary it checks bidirectional kind, type, value, computation, and pattern
judgments; solves inference constraints; resolves holes and normalizes the typed graph; validates coverage inside the
check and offers execution-readiness validation to later consumers; and statically eliminates the residual program. It
also owns the Salsa query surface through which the session requests a check and the typed-arena representation that
surface returns ([C3 query ownership](../../../../docs/references/compiler.md#query-and-checker-ownership),
[C2 identities and arenas](../../../../docs/references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance)).

The crate is organized around the durable representation, not the checking driver:
`syntax` and `arena` define the typed products, `check` elaborates source into them,
`normalize` and `elaborate` transform them, and `query` exposes the incremental boundary.
Read the crate map in `lang/statics/src/lib.rs:10-66` and the module guide in `lang/statics/src/check/README.md`.

This card covers the checker phase, the C5–C6 contracts, and C2 identities as the checker uses them.
SPS IRs belong to the [stack IR card](03-stackir.md); interpreter behavior to the [execution card](05-execution.md);
source loading, packages, and session tooling to the [tooling card](06-tooling.md).
The front end ends at scoped syntax ([front-end card](01-frontend.md)).

## 2. Position in the Pipeline

Upstream, the session assembles the merged `ScopedArena`, its `SpanArena`, and the resolved root
and hands them to statics as `ScopedData` (`lang/statics/src/query/input.rs:35`).
C3's implementation table and C4 own how those products arise; statics never parses or resolves names.

Ordinary source checking is one coarse query.
`check_source` (`lang/statics/src/query/source.rs:100`) runs the judgments, finalization,
coverage, and static elaboration inside one Salsa memo and publishes a `SourceCheckOutcome`:
a checked root with an `Arc<StaticsArena>`, or a rejected arena with diagnostics.
`check_library`, `check_unit`, and `check_completion` (`:106`, `:122`,
`:138`) run the same request path for library, unit, and completion boundaries.
C3 owns the query/checker boundary and memo lifetime, C5 the checking contract, and C6 the elaboration contract;
this phase deliberately does not split the steps into separately cached queries (see §9).

Downstream, the session keeps keyed analysis facts and materializes the full arena on demand:
the session database implements `TyckDb` (`lang/session/src/source/query.rs:282`), and `materialize_arena`,
`checked_program`, and `executable_program` recover shared arenas from the coarse memo (`:501`, `:512`).
The interpreter linker consumes the residual root
([C7](../../../../docs/references/compiler.md#c7-linking-and-the-reference-interpreter)); SPS lowering
consumes the same arena ([C8](../../../../docs/references/compiler.md#c8-sps-lowering-normalization-and-demand));
hovers and diagnostics render typed types through `lang/statics/src/fmt.rs`
([C15 elaborated type rendering](../../../../docs/references/compiler.md#elaborated-type-rendering)).

## 3. Entry Points and Key Types

| What | Where | Why it matters |
| --- | --- | --- |
| `Tycker` | `lang/statics/src/check/mod.rs:70` | One check's mutable state: derived allocator, scoped input, statics arena, checked-term repository, task stack, diagnostics. |
| Checker lifecycle | `lang/statics/src/check/driver.rs:13`, `:59`, `:153`, `:220` | `new` → `run_judgments` → `finish_check` (holes, normalization) → `strip_checker_state`. |
| `check_source` | `lang/statics/src/query/source.rs:100` | Coarse whole-source memo (`lru = 1`) that sequences every phase and publishes the outcome. |
| Query inputs | `lang/statics/src/query/input.rs:10`, `:35` | `TyckDb` trait and `ScopedData` tracked struct: the only statics input crossing from the session. |
| Judgment protocol | `lang/statics/src/check/judgment.rs:5`, `:86`; `lang/statics/src/check/term/mod.rs:36` | `Tyck` and `Action` define synthesis vs analysis; `judge` runs one producer query at the current site. |
| Pattern dispatcher | `lang/statics/src/check/pattern/mod.rs:291` | Pattern dispatch and opening scopes parallel the term dispatcher. |
| Producer queries | `lang/statics/src/query/computation.rs:18`; `lang/statics/src/query/intrinsic.rs:37` | `no_eq` tracked functions return typed fragments and replay-stable identities keyed on a site. |
| Typed syntax | `lang/statics/src/syntax.rs:23`, `:31`, `:40`, `:47`, `:53`, `:88`, `:95`; enums at `:217`, `:542`, `:608`, `:644`, `:695` | Seven sorts plus dispatcher enums; rules narrow union-typed results through `as_*`/`try_as_*`. |
| `StaticsArena` | `lang/statics/src/arena.rs:647`; retained indexes at `:564` | Published nodes plus keyed facts; checker-only environment tables sit at `:678-694`. |
| Identity derivation | `lang/statics/src/alloc/identity.rs:31`, `:80`; `lang/statics/src/check/driver.rs:97` | Session-owned `DerivedIdRegistry` and per-check cursors; the root of replay-stable query identities. |
| Normalization | `lang/statics/src/normalize/filled.rs:12`; `lang/statics/src/normalize/resolution.rs:8`; `lang/statics/src/normalize/inference.rs:45` | Pass-wide caches for hole resolution and filled normalization; local product refinement. |
| Static elimination | `lang/statics/src/elaborate/static_values/mod.rs:63`, `:224`; `lang/statics/src/arena.rs:719` | Records source root and optional residual; execution selects the residual computation or value. |
| Post-check passes | `lang/statics/src/validate/coverage.rs:120`; `lang/statics/src/validate/executable.rs:25` | Coverage matrix and reachable-hole readiness, both outside `check`. |
| Builtin plan | `lang/statics/src/builtin.rs:367`, `:712` | Validates and plans the host Builtin package from checked static identities for interpreters and backends. |

## 4. Contracts and Invariants

- **Producer judgments are identity producers.** A syntax-family rule computes its checked inputs,
  then asks a Salsa producer for the allocation tail at the current occurrence;
  the checker materializes the returned fragment into its arena.
  The thunk rule shows the pattern: `lang/statics/src/check/term/computation.rs:27` calls `judge`,
  and `lang/statics/src/query/computation.rs:18-54` derives the two node identities and returns three arena payloads.
  Site entry and occurrence tracking live in `lang/statics/src/check/driver.rs:97-116`.
  Canonical boundary: [C3](../../../../docs/references/compiler.md#query-and-checker-ownership); identity rules:
  [C2](../../../../docs/references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance).

- **One memo orders the phases and publishes once.** `check_source` (above) runs judgments, finishes them,
  resolves holes, normalizes and validates, elaborates the static root, strips checker state,
  and only then builds the outcome (`lang/statics/src/query/source.rs:152-190`).
  Rejected runs still publish the arena for diagnostics and tooling but expose no checked root (`:177-188`).
  Canonical account:
  [C3 analysis facts and materialization](../../../../docs/references/compiler.md#analysis-facts-and-materialization).

- **A successful check leaves typed-arena invariants.** Every kind and type cell has left its fill form;
  every allocated value, computation, pattern, and type carries an annotation of the right kind;
  no residual hole survives except a foreign import's placeholder.
  Compilation does not enforce these; tests do (`lang/tests/src/invariants.rs`,
  named in [C6 typed-arena invariants](../../../../docs/references/compiler.md#typed-arena-invariants)).
  The checker's own finalization code that establishes them is `lang/statics/src/check/driver.rs:166-204`.

- **Inference closes at a source boundary before import reconciliation.** `InferenceRegion` records the fills inherited
  at entry; closing rejects newly introduced pattern fills
  that remain incomplete (`lang/statics/src/check/source.rs:100`, `:203`, `:208`).
  Failed speculative fills restore solutions and scopes, and lexical
  or abstract substitution follows solved holes before rewriting.
  Canonical rules: [C5 inference and reuse](../../../../docs/references/compiler.md#inference-and-reuse)
  and [L4](../../../../docs/references/language.md#4-classification-and-inference).

- **Checked synthesis is reused only under a context extension.** `CheckedTermRepository` keeps one
  canonical synthesis result per resolved term; a later request must extend the original typing context,
  and repeated or nested successes must agree on the arena root.
  A nested failure supersedes a retained root without rechecking (`lang/statics/src/check/source.rs:118`, `:160-190`).
  Canonical account: [C5 inference and reuse](../../../../docs/references/compiler.md#inference-and-reuse).

- **Finalization shares caches after inference freezes.** `HoleResolver`
  and `FilledNormalizer` each hold one pass-wide memo used
  across every arena root (`lang/statics/src/normalize/resolution.rs:8`,
  `lang/statics/src/normalize/filled.rs:12`); unchanged nodes keep their existing IDs
  and only changed paths enter `kinds_normalized`/`types_normalized` (`:73-78`, `:161`).
  Canonical account: [C5 finalization](../../../../docs/references/compiler.md#finalization).

- **Static elaboration separates the source root from an optional residual.** `StaticElaboration` records
  the original root plus a residual when one exists, so an unapplied static library value can be checked
  without a runtime representation (`lang/statics/src/elaborate/static_values/mod.rs:63`, `:224`).
  Linking and SPS lowering enter through `execution_value`/`execution_compu` (`lang/statics/src/arena.rs:719`, `:732`).
  Canonical rules: [C6 static elimination](../../../../docs/references/compiler.md#static-elimination)
  and [L10](../../../../docs/references/language.md#10-static-elimination).

## 5. Data Flow

The input is the scoped program.
`Tycker::new` reserves source-shaped storage, materializes the check-wide intrinsic singletons as a query,
and collects the scoped context table (`lang/statics/src/check/driver.rs:13-42`).
`run_judgments` then checks the scoped root: the term and pattern dispatchers push a task,
enter an allocation site, prepare the expected classifier, recurse, and record term facts.
Rules call producer queries for their allocation tails and write the returned nodes
into the arena; inference fills accumulate in the arena's fill tables,
and the checked-term repository shares canonical synthesis results.

When judgment checking stops, `finish_judgments` releases source contexts
and shrinks provenance (`lang/statics/src/check/driver.rs:87-90`).
`resolve_holes_and_collect` replaces solved type nodes, then `normalize_and_validate` normalizes every kind
and type root, validates foreign imports, retains one normalized top annotation
per distinct annotation, and runs coverage (`:166-204`).
`elaborate_static_root` stores the `StaticElaboration` (`:158-162`),
`strip_checker_state` drops the six per-sort environment tables and their interner (`:220-229`),
and the outcome publishes `Arc<StaticsArena>`.

Keep two states apart. **Checker state** is the mutable `Tycker`: the derived allocator, inference fills and solutions,
per-node typing environments, task and meta stacks, the checked-term repository, and diagnostics.
It is consumed by checking and discarded at publication.
**Published typed result** is the immutable arena: node arenas, annotations, normalized deltas,
provenance, static elaboration, and the retained keyed indexes that the session snapshots for editor facts.
Downstream phases see only the second.

```text
ScopedData(prelude-resolved ScopedArena, spans, root)
        │  check_source  (one Salsa memo, lru = 1)
        ▼
   Tycker ── term/pattern dispatchers ── producer queries (no_eq, derived IDs)
        │      inference fills, env tables, checked-term repository
        ▼
   finish: resolve holes → normalize → coverage → static elimination
        │      strip_checker_state
        ▼
   Arc<StaticsArena> + root ──► CheckedSource | RejectedSource
        │
        ├─► session: ProgramAnalysis keyed facts; on-demand materialization
        ├─► dynamics link (C7) and SPS lowering (C8): residual root
        └─► hovers/diagnostics: normalized annotations and typed formatter
```

## 6. Verification Commands

From the repository root at `e2dd8979`; use `cargo run --quiet --` because the installed `zydeco` may lag.
`target/debug/zydeco` is equivalent once built.
Scratch inputs live under `target/study-statics/` (ignored).

| Purpose | Command | Expected observable outcome |
| --- | --- | --- |
| Accept a whole program | `cargo run --quiet --bin zydeco -- check lib/tests/core/fact.zy` | Exit 0, no output (front end through statics and finalization accept). |
| Reject a sort mismatch | `cargo run --quiet --bin zydeco -- check target/study-statics/reject.zy` | Exit 1, `[tyck.type-mismatch]` reporting expected `Int`, found `String`, located at `reject.zy:2:15`. The scratch file binds `let x : Int = "not an int"` under a builtin import. |
| Identity-registry unit tests | `cargo test -p zydeco-statics --lib identity` | `6 passed; 0 failed; 51 filtered out` (≈20 s including the first test build). |
| Reuse and recovery unit tests | `cargo test -p zydeco-statics --lib check::source` | `7 passed; 0 failed; 50 filtered out`. |
| Producer-query inventory | `grep -rn 'unsafe(non_salsa_values)' lang/statics/src --include='*.rs' \| wc -l`; `wc -l lang/statics/src/query/*.rs \| tail -1` | `45` flagged tracked functions; `2307` lines under `query/`. Four are the coarse `lru = 1` source/completion queries; 38 are syntax-family rule producers, two are intrinsic producers, and one is the recursive-group producer. |
| Unreachable-glue inventory | `grep -rn 'unreachable!' lang/statics/src --include='*.rs' \| wc -l` | `152` occurrences (the size analysis measured 203 at its revision). `grep -rn 'query-produced' lang/statics/src \| wc -l` gives `42`. |
| N-ary refinement probes | `cargo run --quiet --bin zydeco -- check target/study-statics/nary-3.zy`; the same with `nary-2.zy` | Three-component destructuring exits 1 with `[tyck.type-expected]` (`found _ * _`) and a secondary `[tyck.missing-solution]`; the two-component counterpart exits 0 silently. Both files are the probes from the [n-ary todo](../../../../docs/todos/compiler-boundaries.md#n-ary-product-inference-refinement). |

## 7. Reading Order

1. `lang/statics/src/check/README.md`: the module map and the checking contracts in one page.
2. `lang/statics/src/lib.rs:10-66`: crate modules and the re-export surface.
3. `lang/statics/src/syntax.rs`: identifiers and dispatchers at `:11-98`, then the enum clusters at `:217`,
   `:542`, `:608`, `:644`, `:695`.
4. `lang/statics/src/arena.rs:536-861`: `StaticsArena`, `StaticsIndexes`, the six environment tables,
   and the execution-root selectors.
5. `lang/statics/src/check/mod.rs:70` and `lang/statics/src/check/driver.rs:13-229`:
   checker state and the full lifecycle.
6. `lang/statics/src/query/source.rs:100-190` with `lang/statics/src/query/input.rs`:
   the coarse query and its Salsa inputs.
7. `lang/statics/src/check/judgment.rs:5-100`, then one rule on both sides:
   `lang/statics/src/check/term/computation.rs:6-39` against `lang/statics/src/query/computation.rs:18-54`.
8. `lang/statics/src/fold.rs:34`, then `lang/statics/src/normalize/{resolution,filled,inference}.rs`: rebuilding,
   shared caches, and the refinement drift.
9. `lang/statics/src/elaborate/static_values/mod.rs:63-231`, `lang/statics/src/traverse.rs:162`,
   and `lang/statics/src/validate/`: the C6 tail.
10. References: [C2](../../../../docs/references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance),
    [C3](../../../../docs/references/compiler.md#c3-source-loading-sessions-queries-and-memory-retention),
    [C5](../../../../docs/references/compiler.md#c5-typed-representation-judgments-and-inference),
    [C6](../../../../docs/references/compiler.md#c6-typed-elaboration-residualization-and-validation),
    then the two evaluations' statics findings.

## 8. Checkpoint Questions

1. Which query owns one complete source check, and why does that bound any reuse claim?
   → `lang/statics/src/query/source.rs:100`;
   [C3 analysis facts](../../../../docs/references/compiler.md#analysis-facts-and-materialization).
2. Why do producer queries derive identifiers from an entity, occurrence, and slot instead of allocating directly?
   → [C3 query ownership](../../../../docs/references/compiler.md#query-and-checker-ownership);
   `lang/statics/src/query/computation.rs:44-46`.
3. Where does an occurrence come from, and why must re-checking the same entity derive distinct IDs?
   → `lang/statics/src/check/driver.rs:97-109`;
   [C2](../../../../docs/references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance).
4. Trace `{ ret 1 }`: which checker function, which producer query, which arena tables?
   → `lang/statics/src/check/term/computation.rs:6-39`; `lang/statics/src/query/computation.rs:18-54`.
5. What runs between `finish_judgments` and `strip_checker_state`, and in what order?
   → `lang/statics/src/check/driver.rs:153-229`;
   [C6](../../../../docs/references/compiler.md#c6-typed-elaboration-residualization-and-validation).
6. What happens to a rejected check's arena, and who may still read it?
   → `lang/statics/src/query/source.rs:177-188`;
   [C5 checker recovery](../../../../docs/references/compiler.md#checker-recovery).
7. Which tables disappear before publication, and why is dropping them safe?
   → `lang/statics/src/check/driver.rs:220-229`; `lang/statics/src/arena.rs:678-694`;
   [C3 analysis facts](../../../../docs/references/compiler.md#analysis-facts-and-materialization).
8. What are the typed-arena invariants, and why does compilation not enforce them?
   → [C6 typed-arena invariants](../../../../docs/references/compiler.md#typed-arena-invariants);
   `lang/tests/src/invariants.rs`.
9. How do the interpreter and SPS lowering select a residual root? → `lang/statics/src/arena.rs:719-740`;
   [C6 static elimination](../../../../docs/references/compiler.md#static-elimination).
10. Why does `(head, _, _)` reject while `(head, _)` passes, and what tracks the gap?
    → `lang/statics/src/normalize/inference.rs:45-52`;
    [compiler-boundaries n-ary](../../../../docs/todos/compiler-boundaries.md#n-ary-product-inference-refinement);
    formal note at `lang/statics/type-system.typ:1306-1310`.

## 9. Redundancy and Cleanup Signals

Statuses use this card's template: `open` = removable without an owner design decision;
`decision needed` = owner approval or a design choice gates it; `resolved` = removed or settled, with the evidence.
Appendix IDs refer to the [candidate inventory](../appendix-cleanup-candidates.md);
the [cleanup lens](../04-cleanup-lens.md#verification-protocol-before-proposing-a-removal)
defines the verification protocol.
Both evaluations call their findings proposals, not decisions.

| # | Signal | Evidence | Status | Assessment |
| --- | --- | --- | --- | --- |
| 1 | Per-judgment `no_eq` producer queries duplicate every typing rule; the coarse check already memoizes the whole result. | `lang/statics/src/query/` 2,307 lines with 45 `no_eq` tracked functions (38 syntax-family rule producers); thunk rule glue `lang/statics/src/check/term/computation.rs:6-39`; [audit §per-judgment](../../../../docs/evaluations/2026-10-21-cleanup-audit/README.md#per-judgment-checker-queries); [size §judgment queries](../../../../docs/evaluations/2026-10-25-size-analysis/README.md#judgment-queries-duplicate-every-checker-rule) | `decision needed` | Appendix B1 `open — reference gate`. Direct allocation is the audit's first suggested removal and the size analysis's ≈5k–6k structural simplification; it changes C3/C5 query-ownership text, so it needs approval, and the ≈30% overhead figure is a dated measurement to re-run. |
| 2 | Derived-identifier registry exists only to make producer-query identities replay-stable. | `lang/statics/src/alloc/identity.rs:31`, `:80`; `lang/statics/src/query/intrinsic.rs:40-46`; [C2 registry](../../../../docs/references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance) | `decision needed` | Audited as retained "derived identifiers, which keep Salsa replay stable" ([audit §retained](../../../../docs/evaluations/2026-10-21-cleanup-audit/README.md#retained-after-review)); the size analysis removes it together with signal 1 in favor of a sequential issuer. Conditional on that decision, not independently removable. |
| 3 | Sort dispatchers and union-typed rule results make rules narrow with `as_*`/`try_as_*` conversions. | `lang/statics/src/syntax.rs:23`, `:31`, `:40`, `:47`, `:53`, `:88`, `:95`; [size §sort dispatchers](../../../../docs/evaluations/2026-10-25-size-analysis/README.md#sort-dispatchers) | `decision needed` | The size analysis's fix is a core representation whose sorts are visible in the syntax so each rule returns one sort; that depends on the IR-boundary decision and touches C5's representation table. |
| 4 | Closed types: per-node typing environments and the `subst_env` calls they require. | Six tables stripped at `lang/statics/src/check/driver.rs:220-229`; declarations at `lang/statics/src/arena.rs:678-694`; 44 `subst_env` mentions in `lang/statics/src`; [size §structural simplifications](../../../../docs/evaluations/2026-10-25-size-analysis/README.md#structural-simplifications) | `decision needed` | Appendix C lists this as `open (evaluation)` gated on the IR/elaboration decision (≈0.8k–1.2k estimate). Removing the tables changes the C2/C5 representation contract, so it is not a code-only cleanup. |
| 5 | `unreachable!` glue around producer results and sort narrowing. | 152 occurrences in `lang/statics/src`; 42 sites whose message says the result is query-produced (e.g. `lang/statics/src/check/judgment.rs:98`, `lang/statics/src/check/term/atomic.rs:91`); [size §sort dispatchers](../../../../docs/evaluations/2026-10-25-size-analysis/README.md#sort-dispatchers) | `open` | Not separately actionable: roughly the producer-unwrapping share disappears with signal 1, the narrowing share with signal 3. Count is already down from the size analysis's 203 at `6aae43d0`. |
| 6 | Typed-arena lint and its re-derivation of checker kinds and type equalities. | Removed in `5e3332d4` (`lang/statics/src/validate/lint.rs` 1,048 lines, `rederive.rs` 1,542 lines deleted); test-only replacement `lang/tests/src/invariants.rs`; [size §types erased](../../../../docs/evaluations/2026-10-25-size-analysis/README.md#types-erased-and-then-re-derived); no `--lint-types` in current `zydeco --help` | `resolved` | Appendix E item. C6 now states tests check the invariants and compilation does not; the backend-flavored half of the erased-type finding belongs to the stack IR card. |
| 7 | Parallel witness and projection walks: several candidate searches and repeated telescope traversal. | Four candidate families in `lang/statics/src/check/projection/field.rs:6`, `:69`, `:130`, `:361`; telescope walkers in `lang/statics/src/check/functions/witness.rs:5-60` and `lang/statics/src/check/projection/field.rs:124-130`; [audit §parallel](../../../../docs/evaluations/2026-10-21-cleanup-audit/README.md#parallel-implementations) | `open` | Appendix B3/D6 merge direction (≈400 lines plus witness machinery). Behavior-preserving if C5's field-search and witness contracts stay fixed; verify each candidate function's clients before merging. |
| 8 | `PackPi` and `ValPi` bind witnesses through separate machinery; formation walks nested products while application walks a leading prefix. | `lang/statics/src/check/functions/application.rs:280-292`; `lang/statics/src/check/functions/witness.rs:5-60`; [nested-witness todo](../../../../docs/todos/compiler-boundaries.md#nested-witness-diagnostic) | `decision needed` | Generalized computation instantiation is explicitly a separate extension in the todo; improving the rejection diagnostic is the low-risk half but still changes behavior/tests. |
| 9 | N-ary products are preserved by L5, but expected-shape refinement constructs exactly two components. | `lang/statics/src/normalize/inference.rs:45-52`; `lang/statics/type-system.typ:1306-1310` (REFINE-PROD); reproduced by the probes in §6 | `decision needed` | Appendix B6 `open — reference gate`. Choosing arity-directed refinement changes accepted programs; it does not change the language rule, but the formal note's REFINE-PROD and the tracked todo must be updated together. |
| 10 | The driver's non-query source lifecycle may be unused after the coarse query took over orchestration. | `lang/statics/src/check/driver.rs:76`, `:124`, `:131`; the query path re-implements the sequence at `lang/statics/src/query/source.rs:152-190`; no in-tree caller found by grep | `open` | Unlisted in both evaluations; flagged here from code. It is public crate API; removing it changes only the statics API and its module guide, not C2–C6 contracts, but confirm external consumers before proposing it. |

## 10. Uncertainties and Drift

- **Dated figures.** The audit and size analysis measured `7e5ee4c1` and `6aae43d0`;
  the numbers above are re-measured at `e2dd8979` where a command is shown.
  The evaluations' ≈2.3k–2.8k and ≈5k–6k deltas are estimates and are not additive;
  do not quote them as current removal sizes.
- **`unreachable!` count drift.** 203 at `6aae43d0` against 152 now.
  No evaluation update paragraph records the drop; intermediate commits (query renames,
  error unification) likely account for it.
  I did not bisect, so treat the difference as refactor churn rather than a completed cleanup.
- **Status vocabulary mismatch.** This card's template labels reference-gated items `decision needed`;
  the manual's [appendix](../appendix-cleanup-candidates.md) uses `open — reference gate` for the same condition.
  Rows 1 and 9 map to appendix `open — reference gate` (B1, B6); row 2 maps to the appendix C allocate-directly row;
  row 4 maps to the appendix C closed-types row (`open (evaluation)`, gated on the IR/elaboration decision);
  rows 7 and 8 map to B3/D6 (row 8 also the nested-witness todo); row 6 maps to appendix E; rows 3, 5,
  and 10 have no separate appendix row.
- **Not re-run.** I did not run the full `lang/tests` or session suites, nor the evaluation scripts on a clean archive;
  the crate table and surface counts in the manual come from the lead's runs.
  The three-component probe was re-run here and still rejects; the two-component counterpart still passes silently.
- **Possible unlisted redundancy, unproven.** Signal 10 (driver source lifecycle)
  and any duplicate orchestration between `check/driver.rs` and `query/source.rs` were found by reading, not
  by the evaluations; a client search across tracked Rust found none, but generated or external users were not excluded.
