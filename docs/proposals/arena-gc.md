# Compiler memory retention

A checked root can elaborate into many more nodes than its source contains.
Keeping every complete typed arena in a long-lived session therefore makes memory depend
on the history of roots, even when an editor only needs a small set of current facts.
The [compiler reference](../references/compiler.md#analysis-facts-and-materialization) owns the implemented split
between retained keyed facts, shared full materializations, and query memo lifetimes.
This record concerns the remaining choices about what to retain and when to recompute it.

## Retention criteria

Retain a fact when consumers need random access by semantic identity and recovering it would require a regional recheck.
Retain occurrence payload only while a consumer needs a full traversal, unless measurements justify caching it longer.
Deterministic allocation permits the same inputs to reproduce identities;
it does not make every intermediate solver state a pure function of an allocation site.
The [query/checker boundary](../references/compiler.md#query-and-checker-ownership) explains that distinction.

The current coarse check and fine-grained judgment memos have different costs and lifetimes.
A one-entry arena LRU can release a large materialization while leaving the database's interned inputs
and judgment memos.
Database generations give the test pool a deterministic reclamation boundary;
a general interactive-session policy must also account for active projects,
overlays, and outstanding analysis snapshots.
Any replacement should preserve keyed facts after arena eviction and pair materialization
with the correct source revision.

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
