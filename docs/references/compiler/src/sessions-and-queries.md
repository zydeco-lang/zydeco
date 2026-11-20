# C3. Source Loading, Sessions, Queries, and Memory Retention

## Implementation Representations

The language reference defines the [abstraction levels](../../language/src/sources.md#abstraction-levels)
and their responsibilities.
Their current implementation representations are:

| Abstraction | Current representation |
| --- | --- |
| Project | `Project`, `PackageContext`, and project registrations |
| Package | `Package` / `PackageId`, `PackageInstance`, and `ResolvedPackage` |
| Compilation unit | `ExecutableProgram`, `LibraryProgram`, and `UnitProgram` |
| Semantic unit | Merged `SourceId`, `ProgramAnalysis` / `CheckedProgram` |

`CompilerSession` manages source revisions, snapshots, and cached queries for these abstractions.
Package instance and analysis identity follow the
[package namespace rules](../../language/src/sources/packages.md#copy-resolve-merge-analyze).

## Source Loading and Package Selection

The [loader](../../../../lang/session/src/source/loader.rs) obtains source text
through session inputs, including editor overlays.
`CompilerSession::project` expands explicit discovery into source inputs;
`Project::with_registration` places another project's root at a package path.
`CompilerSession::load_package` exposes the resolved graph, and `analyze_package` gives its selected root semantics.
The CLI, REPL, and editor share these interfaces.
Operation policy and configuration syntax remain [open ideas](../../../ideas/README.md#project-context-inspection).

### Package Resolution Implementation

[Language-level package resolution](../../language/src/sources/packages.md#copy-resolve-merge-analyze) governs sharing
and identity.
Its [acceptance tests](../../../../lang/session/src/source/package/tests.rs) cover paths, lexical scope,
opaque names, root substitution, equal and distinct copies, conflicting definitions, and retained provenance.

`SourceTemplate` stores a parsed file and its source-analyzer results.
`PackageScope` follows the textual arena's shared child relation with an explicit work stack;
each child receives its lexical package context.
This traversal records definition and import contexts.
Source loading creates candidate `SourceFile` nodes for import routes and resolves their dependencies.
Available project declarations supply names; the selected term establishes code reachability.
Competing claims to a reached name are resolved before that binding is accepted.

The merge pass visits providers before consumers and interns structural syntax shapes.
Shapes retain syntax variants, scalar data, attached text, relevant meta annotations,
companion signatures, the default-prelude dependency, and merged import targets.
Package annotations contribute their typed roles and normalized relationship targets;
names are retained with the namespace bindings and provenance.
`SourceGraph.sources` stores the resulting nodes; `SourceGraph.instances` retains candidate contexts,
original templates, and import edges.
`ResolvedPackage` records names, roles, relationship targets, and declaration origins for these nodes.
Namespace equality and structural equality are compared directly through domain types.

[Source assembly](../../../../lang/session/src/source/program.rs) allocates semantic input identities
from the merged graph.
Its source map preserves original file spans for diagnostics and editor queries.

The [default prelude loader](../../../../lang/session/src/source/prelude.rs) records a separate
`SourceFile.prelude` edge for the [language-level default](../../language/src/sources/core-prelude.md),
rather than inventing an authored import site.
Provider ordering, structural merging, cycle checks, and revisioned inputs include that edge.
Leading opt-out inspection follows only the selected root's annotation and grouping spine;
it does not add another whole-source traversal.
The existing source-DAG copier prefixes a closed projection group, or preserves an inherited file opt-out
when copying an independently selected declaration.
This local construction uses the copier's existing schedule and shared textual child relation for provenance.
It does not require a separate reconstruction pass.
The bundled closed bootstrap uses the same source identities as checkout files;
existing files and overlays remain ordinary tracked inputs.

Source graph and analysis queries include the project and selected entry in their keys.
Every read template remains a tracked source input, including declarations used to establish availability.
A retained analysis preserves its request, graph, and provenance for later materialization.
`CompilerSession::complete_in` accepts the same project and entry package context for recovered completion queries.

Loading collects independent read, parse, directive, and cycle failures in `SourceLoadErrors`.
It publishes a complete source graph after the required inputs have been accepted.
The [loader regressions](../../../../lang/session/src/source/tests.rs) cover independent imports and signatures,
multiple cycles, and correction through overlays.

The CLI's [execution runner](../../../../cli/src/execution.rs) consumes checked `ExecutableProgram` snapshots,
sharing their immutable arenas across targets and lowering once for compiled backends.
It prepares temporary artifacts before execution and owns their cleanup.
[L15](../../language/src/execution-profiles.md#selecting-an-execution-backend) owns backend execution policy.

`CompilerSession` is the Salsa database and revision owner.
[ScopedData](../../../../lang/statics/src/query/input.rs) connects the resolved root,
scoped arena and spans to `TyckDb`.
The coarse [check_source query](../../../../lang/statics/src/query/source.rs) runs the mutable checker, finalization,
coverage, and static elaboration together, then publishes an `Arc<StaticsArena>` and a checked or rejected outcome.
Splitting those phases into separately copied arenas would multiply the dominant materialization cost.

## Shared Source Analysis

A parsed file answers several questions before assembly: which sources it imports,
where documentation attaches, which Builtin roles and intrinsic or literal splices need validation,
and which packages and discovery rules it declares.
The [source scan](../../../../lang/surface/src/textual/source/scan.rs) computes requested reachability once,
sweeps allocated terms once, and then visits file text blocks when requested.
Each term or existential parameter annotation event borrows one decoded semantic meta tree,
retaining textual identities for exact argument spans.

`SourceAnalyzer` separates these questions into independent consumers with typed results.
Pairs of analyzers compose statically: their `SourceInterest` values are unioned before scanning,
and both consumers observe the same immutable events.
Import-only queries omit reachability and unrelated decoding; documentation-only queries do not validate imports.
The full `SourceInventory` composes documentation, unattached-text warnings, imports,
Builtin roles, intrinsics, literals, packages, and discovery.
Query entry points use the same analyzers.

Domains preserve the distinction between the returned tree and allocations retained by parsing.
Documentation, packages, and discovery use reachable terms.
Import, intrinsic, and literal validation use every allocated term;
Builtin validation additionally visits existential parameter annotations.
Unattached-text warnings use relevant attachments throughout the arena and file trivia.
Package selection remains downstream of this complete-file inventory.

`SourceAnalysis` retains valid facts and diagnostics from independent sites.
Malformed arguments block only checks that require their decoded values; other sites still run. Package-name resolution
and conflicts follow the [package resolution reference](../../language/src/sources/packages.md#package-hierarchy).
The inventory is keyed by file-local textual identities.
Assembly and desugaring consume syntax at later identity and span boundaries;
reusing decoded inventory facts there would require an explicit remapping
under the [provenance contract](compiler-data.md).
The loader concatenates typed diagnostic collections and publishes a `SourceTemplate` only after every analyzer accepts.
Diagnostic order is a presentation choice, not an API guarantee; strict query facades return the full collection.
Individual malformed directives may stop at their first local error until a further recovery point is justified.

## Query and Checker Ownership

Allocation-producing syntax judgments are producer queries: they return node identities
and immutable outcomes from explicit inputs.
The checker inserts those outcomes into its materialized arena.
The [intrinsic singleton query](../../../../lang/statics/src/query/intrinsic.rs) derives identities
from a synthetic check-wide site, independent of the first source occurrence naming an intrinsic.
`Tycker::new` materializes those singletons and retains their complete `IntrinsicStatics` identities
for construction during checking.
Finished arenas retain the nodes while dropping this construction-only table.
Unification, fill resolution, substitutions, opening of a packed value, copattern elaboration,
and recursive-group processing retain a checker-owned algorithmic core.
Their intermediate results depend on mutable inference state; replaying a site alone cannot reconstruct that state.

A cached node must therefore be distinguished from a pure source-to-judgment function.
Per-call arena snapshots would make query keys expensive and defeat reuse;
a tracked mirror does not make an arbitrary table cell reconstructible from its site.
A worklist redesign of the solver would be a separate algorithmic change.
The current boundary gives producers replay-stable identities through the [session identity registry](compiler-data.md)
while letting inference retain its sequential state.
C5 defines the publication boundary after that state stops changing.

## Analysis Facts and Materialization

`analyze_source` retains `StaticsArena::clone_keyed_indexes()` in `ProgramAnalysis`.
That analysis is a source and fact snapshot, not a complete typed tree.

| Retained keyed information | Transient occurrence payload |
| --- | --- |
| Definition annotations, bodies, alias entry points, and intrinsic roles | Typed values, computations, and patterns |
| Fill sites, solutions, scopes, and abstract identities | Per-occurrence kinds and types |
| Data/codata definitions, term facts, normalized top annotations, and provenance indexes | Derived node columns needed during full-tree traversal |

Retained facts support random access by semantic identity where recovery would require a regional recheck.
Occurrence payload is materialized for full traversal; retaining it longer requires a measured consumer benefit.
Keyed `type_sites` and `term_norms` serve current top-annotation queries without replaying arbitrary inner nodes.

Fact queries read those retained indexes. Full-tree consumers call `materialize_arena`, `checked_program`,
or `executable_program`, which recover the arena through the coarse check.
Materialization uses the session's current inputs, so callers must pair an analysis
with the matching session revision or snapshot.
Live consumers share the complete arena through `Arc`; lowerers add phase-local metadata instead of cloning it.
Checker-only state, including per-node typing environments, is stripped before full-arena publication.
Cajun may keep a project's materialization alive while that project is in use.

`check_source` has a one-entry LRU configuration.
The test session pool explicitly triggers eviction between analyses and periodically replaces its database generation.
Fine-grained judgment memos have a different lifetime from the full arena;
dropping an analysis alone does not promise reclamation of all database storage.
[Retention questions](../../../ideas/README.md#compiler-memory-retention) concern memo lifetime,
compact facts, and parsed-source storage.
[Session tests](../../../../lang/session/src/source/tests.rs) and query tests check facts after eviction,
re-materialization, source hygiene, and failed revisions.
Compiler cache reclamation is independent of program-value collection in C12.
