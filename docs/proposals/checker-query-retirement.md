# Checker query retirement and direct allocation

This proposal weighs one recorded cleanup finding: should the per-judgment Salsa producer queries
in `lang/statics/src/query/` be removed in favor of direct allocation in the checker?
This memo is the durable home of that finding, which two dated studies recorded,
the 2026-10-21 cleanup audit and the 2026-10-25 size analysis; it cites them by name only.
Their line estimates are reviewer estimates from their own revisions, they overlap,
and they are not current measurements.
The canonical contracts stay where they are:
[C2](../references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance) owns identities and arenas,
[C3](../references/compiler.md#query-and-checker-ownership) owns the query and checker boundary,
and [C5](../references/compiler.md#inference-and-reuse) owns checker-local reuse.
The query layer still implements that boundary at this revision, so this memo frames alternatives
and a staged recommendation rather than an adopted design.

## Motivation and current mechanism

A source check owns two nested publication layers.
The session resolves one source root into a scoped program and hands it
to the coarse [`check_source`](../../lang/statics/src/query/source.rs) query, which runs the mutable checker,
finalization, coverage, and static elaboration in one query and publishes an `Arc<StaticsArena>`.
The [analysis-facts and materialization contract](../references/compiler.md#analysis-facts-and-materialization) owns
that coarse layer, its `lru = 1` memo, and the retained keyed facts.
Underneath it, every allocation-producing syntax judgment is a *producer query*:
a tracked function that receives interned inputs, derives node identities,
and returns an immutable outcome that the checker writes into the arena.
The [query-and-checker-ownership contract](../references/compiler.md#query-and-checker-ownership) owns that split.

The layer's intended benefit is incrementality. The session's Salsa database owns query caching
and revision ownership ([C1](../references/compiler.md#c1-architecture-and-a-programs-path-through-the-compiler)),
so a tracked function could, in principle, answer an unchanged judgment from its memo instead of re-running the rule.
The [derived-identifier registry](../../lang/statics/src/alloc/identity.rs) makes the identities returned
by such a replay stable,
and [C2](../references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance) requires replay
to recover the identifiers held by retained semantic facts after arena-memo eviction.
The registry's own documentation states that purpose: it is "shared by a session's checker runs and producer queries"
and is kept alive "across query eviction and replay" ([identity.rs:26-33](../../lang/statics/src/alloc/identity.rs)).

The layer has two dates.
Routing checking through a Salsa `check_source` query arrived on 2026-08-14 in commit `b23f7253`,
whose module comment already said finer-grained judgment queries would replace the wholesale check "piece by piece";
the query directory itself was split out on 2026-09-07 in commit `e927f70d`,
which added eleven files and 2,390 lines and has been touched by fifteen commits in total.
The query modules now total 2,307 lines and export 45 tracked functions marked `no_eq, unsafe(non_salsa_values)`:
38 syntax-family rule producers, two intrinsic producers, one recursive-group producer,
and four coarse `lru = 1` source/completion queries.
Appendix [measured inventory](#appendix-measured-inventory) lists the commands and the per-module split.

A 2026-08-15 change, commit `d21ff226`, already retired the high-frequency leaf-pattern producer family
with local materializers, while preserving the producer allocation slots: `PatternLeaf`
and `PatternVariableStandIn` ([pattern/mod.rs:38-112](../../lang/statics/src/check/pattern/mod.rs)) now insert directly
and use `Tycker::query_derived_id` ([driver.rs:110-115](../../lang/statics/src/check/driver.rs)).
That precedent is the shape this proposal would follow for the remaining producers.

## Problem

The layer pays production and maintenance costs without delivering the reuse that motivates it.
Four properties combine to prevent memo reuse:

- every producer reads `ScopedData`, the tracked struct holding the scoped arena
  of one source snapshot ([input.rs:30-47](../../lang/statics/src/query/input.rs));
  an edit re-resolves the root into new arenas, so the tracked struct and every key derived from it change;
- both arena fields (`spans` and `scoped`) are `#[no_eq]`, so Salsa treats a recreated snapshot as changed
  and invalidates the readers even when the arena content is unchanged;
- producer outputs are marked `no_eq`, so a newly computed value is always treated as changed
  and cannot backdate dependents; `unsafe(non_salsa_values)` is the matching escape hatch
  for returned values that do not implement `SalsaValue` (Salsa 0.28.2, [Cargo.toml](../../Cargo.toml));
- the coarse `check_source` query keeps one arena memo (`lru = 1`
  [source.rs:100-103](../../lang/statics/src/query/source.rs)), and the test session explicitly triggers
  LRU eviction between analyses ([C3 analysis-facts](../references/compiler.md#analysis-facts-and-materialization)).

Within one check the ordinary path also consults each syntax-family producer once:
the term and pattern dispatchers enter a fresh allocation occurrence
per checked entity ([term/mod.rs:40-52](../../lang/statics/src/check/term/mod.rs),
[pattern/mod.rs:295-305](../../lang/statics/src/check/pattern/mod.rs)),
so a repeated entity receives a new site rather than a memo hit.
The one producer with genuine repeated calls is the intrinsic singleton query,
because every intrinsic use derives from the same synthetic site; the checker already materializes
that singleton set once in `Tycker::new` ([driver.rs:13-19](../../lang/statics/src/check/driver.rs),
[check/intrinsics.rs](../../lang/statics/src/check/intrinsics.rs)).
The main other reuse inside a check belongs
to `CheckedTermRepository` ([C5 inference and reuse](../references/compiler.md#inference-and-reuse)),
which is checker-local and independent of the producer memos.

So each edit rechecks the whole root, and the producer memos are neither reused
across edits nor needed for intra-check reuse.
What remains is the duplication cost described below.
The retention idea records about 30% end-to-end checking overhead from the producer-query migration,
as a 2026-08-14 measurement against the earlier binary, and explicitly asks
for re-measurement ([compiler memory retention](../ideas/README.md#compiler-memory-retention)).
That date is also the date of the initial `check_source` migration commit, before the finer producers had accumulated,
so the figure is a dated and unverified measurement of the query layer as a whole,
not an attribution to the per-judgment producers that B would remove.
It predates the leaf-pattern retirement of 2026-08-15, and it is neither a current number nor an expected saving.

## Per-judgment checker queries

The producer layer has four parts.

| Part | Count | Location and role |
| --- | ---: | --- |
| Syntax-family rule producers | 38 | One per surface rule family across `atomic`, `pattern`, `structure`, `computation`, `function`, `package`, and `data`; return node identities and outcomes |
| Intrinsic producers | 2 | `intrinsic_singleton` and `internal_judgment`; synthetic check-wide site independent of the first source occurrence naming an intrinsic |
| Recursive-group producer | 1 | `rec_group_abst_judgment_at`; group-level identities introduced before the equation checks |
| Coarse source/completion queries | 4 | `check_source`, `check_library`, `check_unit`, `check_completion`; one arena memo each (`lru = 1`) |

The 38 rule producers share one ownership protocol: each returns node identities and arena payloads
that the checker writes into the arena, and each answers `None` for an entity of another syntactic shape.
Most rules call `Tycker::judge` ([judgment.rs:86-100](../../lang/statics/src/check/judgment.rs)),
which interns the checked subject, forwards the checker's current site occurrence, calls the tracked function,
and unwraps the result with `unreachable!`; the `judge` doc comment records the shape contract.
Sixteen further call sites call a producer function without the helper, either inside a closure passed to `judge`
or directly, and repeat the same unwrap or `let Some(...) else { unreachable!(...) }` guard.

The intrinsic producers differ in kind.
`intrinsic_singleton` ([intrinsic.rs:31-95](../../lang/statics/src/query/intrinsic.rs)) derives every kind
and type node of `VType`, `CType`, `Thk`, `Ret`, `Unit`, and the primitive types
from the session registry's `Intrinsic` site, keyed by check root and intrinsic key.
`Tycker::new` calls it for every intrinsic key, writes the results into the arena,
and keeps a construction-only `IntrinsicStatics` table;
`internal_judgment` ([intrinsic.rs:97-112](../../lang/statics/src/query/intrinsic.rs)) exists only
to carry an `Internal` rejection as query data.
Neither producer performs allocational work that the checker cannot do directly,
because both derive identities from a site the checker already has.

The recursive-group producer ([source.rs:18-41](../../lang/statics/src/query/source.rs)) pre-introduces the three
identities of each recursive binding before the equation checks run, mirroring the checker's fixpoint prelude.
It is a producer because it must run before the judgments that consume those identities,
not because its result can be reused.

The four coarse queries orchestrate rather than allocate.
`SourceCheckRequest::run` ([source.rs:151-190](../../lang/statics/src/query/source.rs))
intentionally runs the whole pipeline in one query; its inline comment records
that splitting the phases into separate Salsa queries required deep-copying the full statics arena
across every boundary, which dominated check memory.
The coarse layer is therefore not part of the retirement question,
and [C3](../references/compiler.md#analysis-facts-and-materialization) continues to own it.

## Judgment queries duplicate every checker rule

The thunk rule shows the duplication in full.
The checker rule `check_thunk` ([computation.rs:6-39](../../lang/statics/src/check/term/computation.rs)) dispatches
on the source shape, obtains the body classifier from the checker's thunk hole,
checks the body, and converts the checked body into a `TermAnnId`.
It then calls `thunk_judgment` through `judge` (`:27`) and writes the returned fragment into five tables: `types_pre`
and `env_type` for the thunk type node, and `values`, `annotations_value`, and `env_value` for the thunk value node.
The producer ([query/computation.rs:18-54](../../lang/statics/src/query/computation.rs)) repeats the same rule
from scratch: it re-matches `su::Term::Thunk`, re-matches the body `TermAnnId`,
asks the intrinsic singleton query for the `Thk` and `VType` nodes a second time,
derives two identifiers from `query_space(term, occurrence)` plus fixed slots, and returns a five-field outcome.
Direct allocation is the same two derived identifiers plus the same five table insertions;
the checker already knows the shape, the intrinsic nodes, the annotation, and the occurrence.

That pattern repeats across the layer.
The query modules define 33 outcome structs and enums to carry the fragments back,
and 42 `unreachable!` messages in `lang/statics/src` state that a result is query-produced.
Several producers are pure single-node allocations:
`fix_judgment` ([computation.rs:347-360](../../lang/statics/src/query/computation.rs)) derives one `CompuId`
and wraps inputs the checker has already computed; the three-arm `sigma_syn_judgment`
and `manifest_exists_syn_judgment` ([package.rs](../../lang/statics/src/query/package.rs)) select
on checker-computed arm descriptions and derive one `TypeId` per arm.
Rejections also travel as data (`SigmaSynOutcome::Error(TyckError::Expressivity(...))`, `internal_judgment`),
so the checker converts errors back into reported diagnostics after crossing a query boundary instead
of reporting at the failure site, which the [diagnostic contract](../references/compiler.md#diagnostic-collection)
otherwise prescribes.

Some duplication is not mechanical. The producer must reproduce the checker's allocation order exactly,
because the derived identities are published in the arena and retained
as editor facts ([C2](../references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance)).
It must also reproduce the checker's handling of transparent wrappers, checking occurrences,
and auxiliary entities that allocate at an enclosing site (`app_judgment_at`, `InternedSite`).
The thunk example is simple; the package, function, and data families are not,
and those are exactly where the two implementations can drift apart.

The leaf-pattern retirement shows that the migration is tractable: it moved a whole producer family
into two checker-local materializers while keeping the producer allocation slots,
so the published identifiers did not change; only the owner of the allocation did.
The identity registry tests and the session replay test now guard that property.

## Sort dispatchers

The statics identifiers come in seven sorts, and seven dispatcher enums reunite them: `PatId`, `TermId`, `AnnId`,
`PatAnnId`, `TermAnnId`, `StaticPatId`, and `StaticTermId` ([syntax.rs:23-97](../../lang/statics/src/syntax.rs)),
with `ValBinder`, `ValArgument`, and `ValPiBinder` playing the same role for value functions.
Every rule narrows the union-typed result it receives with `as_*`/`try_as_*` conversions;
the dispatchers add eleven infallible accessors in `syntax.rs` and a fallible conversion family
in [check/annotation.rs](../../lang/statics/src/check/annotation.rs).

The producer layer mirrors this structure rather than causing it.
A producer returns a union-typed outcome because one surface form can produce several sorts:
`fn`, application, `let`, `pi`, and `match` each have several results,
and `check_let` ([computation.rs:168-207](../../lang/statics/src/check/term/computation.rs)) handles the product
of its bound term's sort and its tail's sort.
Removing producers leaves the dispatchers, their accessors, and every narrowing site in place.
The retired size analysis treated the union-typed checking result as a separate design question,
a core whose sorts are visible in the syntax; that question is outside this decision,
and neither option below reduces it.

## Options

The two retired studies proposed two depths of removal.
They are distinct choices, and the line estimates below are their reviewer estimates,
which overlap and are not current measurements.

| Option | Change | Keeps | Retired-study estimate | Principal risk |
| --- | --- | --- | ---: | --- |
| A. Status quo | No code change; keep all producers | Replay identities, coarse memo, facts, recovery paths; leaf-style piecemeal bypasses remain available | — | Duplication and the dated overhead stand; no path to reuse because keys are revision-scoped and outputs are `no_eq` |
| B. Direct allocation | Remove the 41 per-judgment producers (38 syntax-family, 2 intrinsic, 1 recursive-group); allocate directly in the checker using the same derived IDs; keep the four coarse queries, the registry, keyed facts, materialization, and session fact queries | Editor facts, completion typing, rejected-run facts, replay stability | ≈2.3k–2.8k lines (2026-10-21 cleanup audit) | Identity drift across eviction or replay; missed rejection/diagnostic paths; completion and intrinsic singleton identities |
| C. B plus registry removal | B, then replace the derived-identifier registry and most generic arena machinery with sequential issuers; Salsa would remain, if at all, at file granularity for Cajun | Same as B only if retained facts stop needing replay-stable IDs | ≈5k–6k lines (2026-10-25 size analysis), overlapping B's estimate | Contradicts C2's replay requirement; editor facts survive eviction by keyed indexes, and completion evidence and retained annotations reference arena IDs |

Option A is the safe baseline, but it is not a route to incremental reuse on its own.
Making the producers reusable would need stable keys across edits and comparable outputs; neither exists,
and providing them is a larger redesign of scoped identities than removing the layer.
The leaf-pattern retirement is the honest model for A's remaining upside:
high-cardinality wrappers can still be bypassed one family at a time without touching the references.

Option B is the recommended direction.
It removes the duplicated rule tails while leaving every consumer contract in place: the four coarse queries,
`ProgramAnalysis` keyed facts, `materialize_arena`/`checked_program`/`executable_program`
([session query.rs:501-554](../../lang/session/src/source/query.rs)),
the session analysis-fact queries ([same file](../../lang/session/src/source/query.rs)),
and Cajun hover and completion ([analysis.rs:303-320](../../editor/cajun/src/analysis.rs),
[completion.rs:33-53](../../editor/cajun/src/completion.rs)).
Because the checker derives the same `(site, occurrence, slot)` identifiers the producers derived,
the migration does not change published identities, which is what keeps replay and retained facts stable.
The leaf-family precedent and the existing `query_derived_id` boundary make this a mechanical change
per rule family rather than a new identity design.

Option C answers a different question, namely whether the registry and the generic arena utilities are needed at all.
Dropping the registry would remove the `KeySpaceRegistry` interning path and most
of [`utils/arena.rs`](../../lang/utils/src/arena.rs) (1,792 lines at this revision),
but it would also drop the mechanism that lets a retained fact recover its node identity after an arena memo is evicted.
A sequential issuer can reproduce identities only if every replay reconstructs the same allocation order;
partial materialization, rejected runs, completion checks, and auxiliary sites all perturb that order,
and no design in this repository currently establishes the invariant.
Option C therefore needs its own proposal, its own reference changes, and its own measurement;
it should not ride along with B.

## Recommendation

Adopt a staged sequence, with each stage gated by its own evidence:

1. **Re-measure the dated overhead before changing anything.** Run two comparisons with the same workload and toolchain:
   the revision before the 2026-08-14 `check_source` migration against the current tree,
   which re-measures the original claim, and the current tree against a direct-allocation pilot,
   which isolates the per-judgment producer cost that B would remove.
   Record the revision, workload, build profile, host, measured quantity, and default
   or experimental representation as [C16](../references/compiler.md#following-a-change) requires.
   The record should show how much of the original overhead survives the leaf-pattern retirement
   and whether a measurable fraction remains.
2. **If the measurement justifies it, implement option B in one change behind no flag.**
   The proposal should not be approved as an intention to migrate later;
   [AGENTS.md](../../AGENTS.md) requires replacing a design and its callers in the same change.
   Removing the per-judgment layer is a bounded refactor only when it is done whole:
   a rule migrated halfway leaves two allocation paths and no clear ownership.
3. **Decide option C separately, after B lands and is measured again.** Registry removal changes the retained-fact
   contract in [C2](../references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance)
   and the session's replay tests, so it needs its own evidence and approval.

The recommendation is conditional in the right place: stage 1 can conclude
that B's performance case is weak, in which case the duplication case still stands but the priority drops
and the memo should record the measurement and defer B rather than forcing it.
It cannot conclude that C is ready, because C's blocker is a design question (deterministic replay
without key spaces), not a measurement.

## Consequential reference changes

Adopting B changes the implemented boundary, so the references that own it must be amended with user approval
under the [documentation rules](../../AGENTS.md#documentation-structure-and-design-records);
proposals are not adopted design.
No change below should be made before that approval.

| Reference passage | Required change |
| --- | --- |
| [C3 query and checker ownership](../references/compiler.md#query-and-checker-ownership) | Rewrite the opening producer-query paragraph to describe direct allocation at the checker's allocation site; keep the paragraph on the checker-owned algorithmic core; replace the intrinsic-singleton paragraph with the checker's materialized table; drop the sentence that the current boundary gives producers replay-stable identities, or restate it as the checker's derived allocation |
| [C3 analysis facts and materialization](../references/compiler.md#analysis-facts-and-materialization) | Keep the coarse query, `lru = 1`, and keyed-fact paragraphs; remove the "fine-grained judgment memos have a different lifetime" sentence once no fine-grained judgment memos exist; re-read the materialization paragraph for wording that assumes producers |
| [C2 compiler data, identities, arenas, and source provenance](../references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance) | Update the derived-allocation table row and the producer-query sentences; the registry, its site types, eviction/replay rule, and `DerivationFamily` discussion stay only if the producer family survives as a distinct allocation family or is replaced by an equivalent checker-side derivation |
| [C5 inference and reuse](../references/compiler.md#inference-and-reuse) | No rule change under B: the checker-local `CheckedTermRepository` and its reuse conditions stay. Re-read the section when editing C3's pointer to it, and keep the publication boundary it defines |
| [C6 typed-arena invariants](../references/compiler.md#typed-arena-invariants) | No rule change under B; the invariants are publication properties, not producer properties. The migration must keep them, including annotations on allocations that inference retries abandoned |
| Module guides: [statics crate](../../lang/statics/src/lib.rs), [checker guide](../../lang/statics/src/check/README.md), [query module](../../lang/statics/src/query/mod.rs) | Remove or rewrite the producer-query descriptions; the query module keeps only database inputs and source orchestration under B |

Option C would additionally rewrite C2's replay requirement, the registry paragraphs, and most of the identity section.
Those changes are deliberately not listed above, because C is not part of this decision.

## Migration and validation

Under B the migration is one change with the following shape.

- **Delete the duplicate rule tails.** Remove the 38 syntax-family producers and their outcome types,
  the two intrinsic producers after replacing their one-time materialization,
  and the recursive-group producer after moving its identity pre-introduction into the checker's fixpoint path.
- **Allocate at the same site with the same slots.** Each former producer call becomes
  `tycker.query_derived_id::<Id>(slot)` plus the arena insertions, using the site occurrence the checker already tracks.
  The leaf-pattern materializers are the template.
  Auxiliary producers that allocate at an enclosing site keep the enclosing site's derivation.
- **Preserve rejection and observation paths.** Rejections that travelled as outcomes become `tycker.err(...)`
  at the failure site; retain one diagnostic per failure site and the same diagnostic codes.
  `TyckObservation`, `SourceCheckOutcome::Checked`/`Rejected`, and the retained keyed facts must publish exactly
  as before ([check/source.rs](../../lang/statics/src/check/source.rs)).
- **Keep the coarse queries, the registry, the keyed facts, and the materialization API.** No session,
  Cajun, CLI, or test interface changes in B.
- **Update the module guides** listed in the previous section, and remove the query module's doc claim
  that it exports syntax-family judgment producers.

Validation uses focused targets first, then the resumable suite, then the full suite:

```sh
cargo test -p zydeco-statics
cargo test -p zydeco-session --lib source::query::tests
cargo test -p zydeco-session --lib completion
cargo test -p cajun
cargo test-resume
cargo test-all --release
```

The session filters cover the retained-fact and eviction tests,
including `facts_survive_arena_memo_eviction`, `derived_ids_replay_after_eviction_through_a_session_clone`,
`analyses_retain_only_the_keyed_indexes_and_rematerialize_on_demand`,
and `independent_sessions_separate_identical_source_ids_and_release_their_registries`
([session query.rs](../../lang/session/src/source/query.rs)).
The completion filter covers the 32 completion tests in `lang/session/src/source/query/completion/tests.rs`;
the Cajun target covers hover, completion, and the editor fact paths.
Pair accepted and rejected fixtures where a rejection path moves, as [AGENTS.md](../../AGENTS.md) requires
for validation changes.

Re-measure before and after with the same command and record both runs.
The repository's check benchmark is [`scripts/bench-check.sh`](../../scripts/bench-check.sh),
which rebuilds the release CLI and reports the best wall time of N runs for `lib/std/std.zy`, the OOPSLA core,
and two representative tests:

```sh
scripts/bench-check.sh 5
```

Compare the current tree with a worktree at the commit before the change,
and record peak RSS separately from wall time, since reservation, allocation counts,
and elapsed time answer different questions ([C16](../references/compiler.md#following-a-change)).
The 2026-08-14 figure was recorded against the earlier binary; a reproduction therefore needs two builds
of the same workload, not an absolute number.

## Open questions and risks

- **Replay stability of retained IDs.** B keeps the registry and the `(site, occurrence, slot)` derivation,
  so it should not move published identities.
  The risk is a rule whose checker-side allocation order differs from the producer's order,
  for example when the producer allocated before an early error check.
  The replay test is the guard; any identity change must be explained before it is accepted.
- **Intrinsic singleton identity.** The intrinsic producers derive from a synthetic site keyed
  by check root and key, not from a source term.
  Direct materialization must use the same derivation and the same slots, or retained facts and editor annotations
  that mention intrinsic nodes will disagree with a re-materialized arena.
  The `IntrinsicStatics` construction-only table is already the natural home.
- **Completion typing.** Completion runs a disposable check through `check_completion`
  and compares annotations with the current source.
  Direct allocation must keep the completion arena's identities consistent with the strict check's identities
  for the same source root, so completion evidence and hover agree.
- **Rejected-run diagnostics.** Rejected checks retain static facts for diagnostics and tooling; a rejection
  that used to be a producer outcome must be reported exactly once at its source site, with the same code and span.
  Pair each moved rejection with a valid counterpart.
- **Salsa database lifetime.** Removing per-judgment memos reduces database bookkeeping,
  but the coarse arena memo and its `lru = 1` policy remain.
  The change should not alter eviction behavior, and the session pool's generation policy should be re-read
  after the migration because its rationale currently mentions the fine-grained memos.
- **Whether C is feasible without losing deterministic IDs.** A sequential issuer reproduces IDs only
  when replay reconstructs the same allocation order.
  Partial materialization, rejected runs, completion, and auxiliary sites all diverge from a single order,
  so C needs an explicit argument or a different retained-fact representation.
  This memo does not find one.
- **Measurement coverage.** `scripts/bench-check.sh` times four command-line checks;
  it does not exercise editor sessions, repeated root switching, or database bookkeeping.
  A pilot should also record retained arena sizes and the session eviction path before claiming the overhead is gone.
- **Per-rule drift during review.** The producers currently encode interpretation choices (transparent-wrapper handling,
  occurrence reuse, auxiliary sites) that are easy to lose when the tail moves.
  Review each migrated family against its producer's history, not only against its current output.

## Appendix: measured inventory

All figures below were measured on 2026-11-05 at revision `c6c90588` from the repository root.
The two retired studies measured earlier revisions; their figures differ and are cited only as estimates.

### Commands and results

| Command | Result |
| --- | --- |
| `wc -l lang/statics/src/query/*.rs` | 2,307 lines across 11 files |
| `grep -rn 'no_eq, unsafe(non_salsa_values)' lang/statics/src --include='*.rs' \| wc -l` | 45 tracked functions |
| `grep -rn 'no_eq, unsafe(non_salsa_values)' lang/ --include='*.rs' \| wc -l` | 57 tracked functions workspace-wide (45 statics, 11 session source queries, 1 completion query) |
| `grep -rn 'no_eq' lang/statics/src --include='*.rs' \| wc -l` | 48 mentions: 45 function attributes plus three `ScopedData`/`EnvData` fields |
| `grep -rn 'unreachable!' lang/statics/src --include='*.rs' \| wc -l` | 152 |
| `grep -rn 'query-produced' lang/statics/src --include='*.rs' \| wc -l` | 42 |
| `grep -rn 'pub struct .*Outcome\|pub enum .*Outcome' lang/statics/src/query \| wc -l` | 33 outcome types |
| `grep -rn 'tycker.judge(' lang/statics/src/check \| wc -l` | 44 judge call sites |
| `grep -rn 'crate::query::[a-z_]*_judgment(' lang/statics/src/check \| wc -l` | 16 further direct producer calls |
| `grep -c 'lru = 1' lang/statics/src/query/source.rs` | 5 matches: four attributes and one comment |
| `wc -l lang/statics/src/syntax.rs` | 709 lines; the dispatcher enums are at `:23-97` |
| `wc -l lang/statics/src/check/judgment.rs lang/statics/src/alloc/identity.rs` | 101 and 254 lines; `judge` at `judgment.rs:86-100`, registry at `identity.rs:26-52` |
| `wc -l lang/utils/src/arena.rs` | 1,792 lines (the retired size analysis measured 1,914) |
| `git log --oneline -- lang/statics/src/query \| wc -l` | 15 commits |
| `git show --stat --format='%h %ad' --date=short b23f7253 -- lang/statics/src/query.rs` | Initial Salsa `check_source` migration, 2026-08-14: 59 insertions in one `query.rs` |
| `git show --stat --format='%h %ad' --date=short e927f70d -- lang/statics/src/query` | Query directory added, 2026-09-07: 11 files, 2,390 insertions |
| `wc -l lang/statics/src/check/term/computation.rs lang/statics/src/query/computation.rs` | 374 and 360 lines; the thunk pair is `:6-39` and `:18-54` |
| `grep -c '#\[test\]' lang/session/src/source/query/completion/tests.rs` | 32 completion tests |

### Per-module producer split

| Module | Lines | Tracked producers | Producers |
| --- | ---: | ---: | --- |
| `query/computation.rs` | 360 | 8 | thunk, ret, force, do, let, app, app-at, fix |
| `query/pattern.rs` | 303 | 9 | trivial, named (syn/ana), cons, ctor (syn/ana), alias (syn/ana), project |
| `query/function.rs` | 286 | 3 | pi, abstraction, pack-pi introduction |
| `query/data.rs` | 241 | 6 | data, codata, match, ctor, comatch, dtor |
| `query/package.rs` | 222 | 3 | sigma, manifest-exists, pack |
| `query/structure.rs` | 201 | 4 | named, label, cons, projection |
| `query/atomic.rs` | 211 | 5 | literal, term hole, trivial, variable, hole analysis |
| `query/source.rs` | 191 | 5 | recursive group plus the four coarse queries |
| `query/intrinsic.rs` | 112 | 2 | intrinsic singleton, internal term |
| `query/input.rs`, `query/mod.rs` | 180 | 0 | shared keys and module boundary |
| **Total** | **2,307** | **45** | |

### Retired-study figures and provenance

- The retired 2026-10-21 cleanup audit measured `lang/statics/src/query/` at about 1,800 lines,
  counted 66 `unreachable!` glue sites for the producers, and estimated ≈2.3k–2.8k removable lines
  for "direct allocation with the existing derived identifiers".
- The retired 2026-10-25 size analysis counted 57 `no_eq` tracked functions workspace-wide,
  203 `unreachable!` sites in `lang/statics`, and eleven commits touching the query directory;
  it estimated ≈5k–6k lines for removing the producers plus the derived-identifier registry and most
  of `utils/arena.rs`.
- The two estimates overlap, were made at earlier revisions, and were not measured deletions.
  They are recorded here as provenance for the decision context, not as expected savings.
- The dated overhead claim is from the [compiler memory retention idea](../ideas/README.md#compiler-memory-retention):
  about 30% end-to-end checking overhead from the producer-query migration,
  measured 2026-08-14 against the earlier binary.
  It is unverified at this revision and predates the 2026-08-15 leaf-pattern retirement.
