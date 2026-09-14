# Compiler memory retention

A checked root can elaborate into many more nodes than its source contains.
Keeping every complete typed arena in a long-lived session therefore makes memory depend
on the history of roots, even when an editor only needs a small set of current facts.
The [compiler reference](../references/compiler.md#analysis-facts-and-materialization) owns the implemented split
between retained keyed facts, shared full materializations, and query memo lifetimes.
This record concerns the remaining choices about what to retain and when to recompute it.

## Retention policy extensions

The current [retained-fact contract](../references/compiler.md#analysis-facts-and-materialization)
separates full-arena lifetime from fine-grained memos.
Extend reclamation to interactive sessions only with an account of active projects, overlays, and outstanding snapshots.
A replacement must preserve keyed facts after arena eviction and pair materialization with the correct revision;
a smaller arena cache alone cannot bound database storage.

## Next measurement pass

The 2026-09-14 code review at `3b4dd665` leaves the following concrete targets.
They are observed mechanisms whose cost needs measurement, not measured regressions or a selected replacement design.

| Target | Current mechanism | Comparison to make |
| --- | --- | --- |
| Judgment producers | [Small producer queries](../../lang/statics/src/query/computation.rs), such as `force_judgment`, intern already checked inputs and return a derived node | Record query executions, memo reuse, and bookkeeping time during cold checks, unchanged queries, and edits before changing query granularity. |
| Local inference | [InferenceRegion](../../lang/statics/src/check/source.rs) copies all existing fill IDs on entry and scans the fill table on close | Count visited fills as the number of local binders grows; compare region-owned fill tracking while preserving nested-region constraints and rejection diagnostics. |
| Retained analysis | [Materialization](../../lang/session/src/source/query.rs) recovers a full arena from the session's current inputs | Measure repeated root switching and edits, retained memos, and live snapshots separately from full-arena residency. |
| Editor lookups | [Cajun](../../editor/cajun/src/analysis.rs) retains a full arena, but symbol hover still obtains some facts through the session | Measure request cost and verify revision pairing before simplifying the path; term hover already uses the retained arena directly. |

The current reference requires an analysis and its materializing session to have matching revisions.
The API represents retained facts with `StaticsArena::clone_keyed_indexes()`
and does not encode that pairing in the analysis argument's type.
This is an API-hardening candidate, not a reproduced editor failure.
Compare explicit fact views and revision-bound materialization only after retaining tests for unchanged snapshots,
edited roots, rejected revisions, and arena eviction.

Use identical inputs and toolchains for each alternative, and record elapsed time,
peak RSS, retained counts, and correctness outcomes.
Pair these measurements with the [traversal evaluation](traversals.md#performance-validation) so a local reduction
in visits does not hide larger temporary summaries or query retention.

## Historical measurements and alternatives

Earlier investigations recorded the following measurements.
Their original notes do not identify every compared revision, build profile, and host;
they explain the decisions investigated and are not current performance guarantees.
Repeat the workloads under [C16's measurement contract](../references/compiler.md#following-a-change)
before using them to select a new policy.

| Observation | Recorded result | Design implication |
| --- | --- | --- |
| Full standard-library elaboration | About 65K scoped terms and 2.06M `types_pre` nodes; 46% `App`, 25% `Arrow`, 13% `Label`, 11% `Prod` | Optimize elaboration retention as well as source storage. |
| Shared test-session growth | About 29 GB before the session-pool cap | A live database needs an explicit lifetime policy. |
| Session-suite peak after the generation split and LRU work | 18.0 GB to 6.32 GB | Full-arena retention was a major contributor; this does not establish bounded fine-grained memos. |
| One standard-library check across successive changes | Peak RSS 7.5 GB, then 2.42 GB, then about 914 MB; warm checks about 1.2 s | Shared phase products and compact storage warranted investigation. |
| Type-content census | 69.8% content-unique | Hash-consing could save at most about 30% of that table before index overhead. |
| Producer-query migration, measured 2026-08-14 | Reported about 30% end-to-end checking overhead against the earlier binary | Re-measure bookkeeping costs before moving more solver work into queries. |

Per-node normalized-type replay was explored, but the identified tooling consumers requested top annotation types.
Keyed `type_sites` and `term_norms` therefore served those consumers without replaying arbitrary inner nodes.
An inner-node consumer would change that tradeoff.
Hash-consing remains an alternative if a new census shows enough structural duplication
to outweigh the additional index; the old census alone does not decide future representations.

## Open questions

- Can a database release one root's inputs and fine-grained memos while keeping shared providers and live snapshots?
  Compare root-aware reclamation with bounded generations using repeated edits and many distinct roots.
- Can retained `TermFacts` classifiers share their annotations?
  An earlier census found identical value/computation classifiers, but 9,309 of 54,400 type facts had a different kind.
  A compact encoding needs an explicit override unless a new equality census establishes a stronger invariant.
- Can parsed-source lifetimes share repeated location structure across spans,
  textual syntax, tokens, and intention maps?
  Audit them as one lifetime group so a smaller local table does not retain a larger owner.
- Would consumer-driven normalization of closed types save more than the shared eager finalizer?
  A replacement must preserve finalized identity lookups and diagnostics and account for the stateful solver boundary;
  moving normalization behind a query is insufficient by itself.
