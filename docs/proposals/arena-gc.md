# Deterministic Arena Reclamation

## Problem

One type-checked root materializes a `StaticsArena` whose size is bounded by the
*elaboration* of the program, not by its source. Checking the standard library alone
(`std/std.zy`, 65K scoped terms) materializes 2.06M `types_pre` nodes plus per-node annotations,
environments, and a normalized copy — a 32x amplification over the scoped program, dominated
by tiny structural nodes (46% `App`, 25% `Arrow`, 13% `Label`, 11% `Prod`).
A long-lived session that analyzes many roots accumulates these arenas without bound,
which was the original cause of the test-suite OOM (a shared session reached ~29GB before
the `SessionPool` workaround capped it) and would be the editor's fate as well.

salsa 0.26 offers no per-query eviction (`Database::evict_lru` resets revisions only; the
storage is freed when the whole database drops). Reclamation therefore lives at the arena
layer, with deterministic rules.

## Principle: Recomputation

The judgment layer makes the arena a *cache* (see
[query-owned statics](query-owned-statics.md)). Every node's identifier is derived
deterministically from its site, and every node's content is the value of a small memoized
judgment query keyed by that site. Re-executing a query reproduces the same identifiers, so
a dropped arena node can be rebuilt exactly, provided the query's inputs (the scoped program
and the salsa database) survive. The salsa judgment memos are the durable, recomputable layer;
the arena is its materialization.

Recomputation is therefore the budget question: a table is worth retaining exactly to the
extent that (a) it cannot be re-derived locally, or (b) its absence would force re-running the
checker over a region rather than replaying a judgment.

## Criterion: Keyed Index vs Occurrence Payload

The checker and the linkers read the arena in two fundamentally different ways:

- **Random access by key.** `annotations_var[def]`, `solus[fill]`,
  `codatas[id].get(&dtor)`, `type_definitions[def]` — lookups indexed by a *declared* name, a
  hole, or an abstract identity. Nothing in the neighborhood of the read site determines these
  entries; they are the global context that the checker extends and the linkers follow.
- **Traversal by occurrence.** The typed tree (`values`, `compus`, `types_pre`) is reached by
  walking from roots along the child references embedded in each node. Every node along such a
  walk is the judgment value at an occurrence site and can be replayed.

This splits the arena into two generations:

- **L (long-lived): keyed indexes**, bounded by the source's *declaration* count (thousands),
  kept across roots. They are the entry points from which everything else is reachable and
  the context against which later checks run.
- **S (short-lived): occurrence payload**, bounded by the *elaboration* size (millions), valid
  while one root's consumers traverse it, then discarded.

## Table Classification

| Retain (L) | Rationale |
| --- | --- |
| `annotations_var` | the name table; editor facts and env lookups index by `DefId` |
| `solus` / `fills` / `fill_scopes` / `fill_hints` | hole solutions; editor displays them |
| `datas` / `codatas` | arm tables keyed by `CtorName` / `DtorName`; stackir reads them |
| `type_definitions` / `inlinables` | definition-body entry points for unfolding and inlining |
| `value_aliases` / `package_aliases` | alias entry points for package resolution |
| `seals` / `absts` / `abst_hints` | abstract identities and their representatives |
| `existential_skolems` | skolem markers opened with packages |
| `terms` / hints | the scoped-entity-to-typed-id site index that makes S replayable |
| `intrinsics` / `builtin_roles` | query-owned singletons and role attachments |
| `term_norms` / `type_sites` | normalized annotation type per term, keyed by term site |

| Discard (S) | Rationale |
| --- | --- |
| `types_pre` / `kinds_pre` | occurrence payload; 2.06M nodes for std |
| `values` / `compus` / `vpats` / `tpats` / `kpats` | the typed tree; traversal target of the linkers |
| `annotations_type` / `annotations_compu` / `annotations_value` | per-node annotations ride along with their node |
| `kinds_normalized` / `types_normalized` | derived columns of the same nodes |

## As Built

The generation split is the policy in force; replay replaced it where measurement
showed keyed indexes answer the actual demand.

- `analyze_source` runs the check through the memo, then strips the occurrence payload
  (`StaticsArena::strip_occurrence_payload`) before constructing the `ProgramAnalysis`.
  An analysis therefore retains only the L tier. `CompilerSession::materialize_arena`,
  `checked_program`, and `executable_program` re-materialize the full arena from the
  memoized check on demand. Consumers (CLI, REPL engine, cajun, integration tests) obtain
  the typed arena through the session; cajun's `ProjectState` is a live root consumer and
  holds its materialization for the project's lifetime.
- `check_source` returns its arena behind an `Arc`. Read-only fact queries share it in O(1);
  only consumers that mutate during lowering clone the arena out explicitly.
- The three S-reading fact queries (`normalized_type_at`, `coverage_facts`,
  `term_annotation_at`) answer entirely from L: `normalized_type_at` reads the keyed
  `type_sites` and `term_norms` tables, so every editor fact survives arena-memo eviction.
  Per-node normalized-type replay (a recursive `normalize_type` query over the judgment
  layer) was measured to have no consumer — the only callers ask for top annotation types —
  so the keyed index is the delivered form and replay remains available for a future
  consumer that needs arbitrary inner nodes.
- `check_source` memoizes with salsa's `lru = 1` and the test pool triggers eviction per
  analysis. The entry-counted LRU cannot express root-scoped eviction for the millions of
  fine-grained judgment entries, so the pool generation remains their policy.
- Retention was additionally reduced by shrinking the dominant node enum, paging the derived
  type-ID key space with bounded growth slack, sharing phase products instead of cloning
  query results, isolating typed elaboration's definition delta, and packing source
  positions into one word.

### Measurements

- Session suite peak RSS fell from 18.0GB (pool cap) to 6.32GB through the LRU step and the
  generation split; a long-running session no longer grows per root.
- One full-std check retains ~914MB peak RSS (down from 2.42GB after the earlier phase merge
  and delta work, and from 7.5GB before them); warm re-checks run in ~1.2s.
- Value hash-consing was measured and rejected: 69.8% of std's types are content-unique, so
  deduplication saves at most ~30% of one table — not worth an interning layer.

## Open Questions

- Can salsa 0.26 remove one root's inputs and memos without dropping the whole database? This
  decides whether the memo policy lives inside one database or across generations of databases.
- The retained `Vec<TermFacts>` (63,574 records) could compact its classifiers: every value and
  computation fact's classifier matches its stored annotation, while 9,309 of 54,400 type facts
  report a kind that differs from the co-located one, so a compact encoding needs an explicit
  override path rather than plain erasure. A census of equality, not just identifier
  cardinality, is the prerequisite.
- The parsed-source lifetime group (spans, textual syntax, tokens, line-intention maps) should
  be audited together for repeated location structure.
