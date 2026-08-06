# Query Type-System Worklog

Status: active; the shared session boundary, CLI migration, and editor integration are complete,
2026-08-05. Finer-grained static queries remain future work.

## Objective

Move Zydeco's static analysis into a demand-driven, memoized compiler database without changing the language's
current import, inference, or nominal-identity semantics. The migration should give the CLI and Cajun one durable
source database, make repeated analysis reuse unchanged work, and eventually make a definition or recursive binding
component the unit of type-checking invalidation.

The query system is also the ownership boundary needed to separate durable semantic identities from temporary
inference identities. A query may use an imperative local solver, but its reusable result must be immutable and must
not expose unresolved metavariables, local skolems, mutable compiler inputs, or output produced as a side effect.

## Starting Position

The original source driver already recorded two useful dependency structures. `SourceGraph` represents source files
and their import edges, while each scoped `BindingContext` retains the condensation DAG of one `begin` block. These
graphs answer different questions and will remain separate: source queries discover and instantiate files, while
static queries order definitions and recursive components inside one term.

The current checker is one mutable transaction. `Tycker` owns its allocator, typed arenas, unification solutions,
skolem constraints, diagnostic task stack, and error accumulator. It also borrows `ScopedArena` mutably because
internal elaborations allocate synthetic definitions into the scoped input. `finish_check_k` resolves every hole and
eagerly normalizes every allocated kind and type before returning one whole-program `StaticsArena`.

This stateful core does not need an immediate functional rewrite. The first query can execute the existing checker
as a local transaction. Finer queries require a later freeze boundary that converts the transaction into immutable
typed fragments and exports only stable semantic interfaces.

## Semantic Constraints

### Imported files are templates, not shared typed modules

Every import occurrence currently receives a fresh syntax clone. Name resolution resets lexical scope at the source
boundary, expected classifiers continue through that boundary, and nominal identities inside two imports of the same
file remain distinct. A cache keyed only by canonical file path may therefore share parsing and source metadata, but
it must not share resolved or typed occurrence identities.

The first query slice preserves this model exactly. File parsing is keyed by canonical path and source text;
whole-program assembly still creates fresh import instances before name resolution and checking. A later
`SourceInstanceId` will identify each import occurrence if instantiation itself becomes a query.

### Bidirectional checking is contextual

The same surface term may be checked more than once. Its result can depend on synthesis or analysis mode, the
expected classifier, lexical definitions, visible skolems, and the current inference transaction. An individual
expression is therefore not an appropriate first query key.

The intended static query boundary is an acyclic definition or a recursive binding SCC. Each such query will consume
the exported interfaces of its dependencies and will run one private inference transaction for its complete
component.

### Durable and local identities have different owners

The durable layer will eventually own canonical files, source instances, source definitions, and sealed nominal
types. Typed syntax nodes may remain local to an immutable checked fragment. Flexible metavariables and
instantiation skolems remain private to one `InferenceContext` and must not appear in an exported interface.

Existing process-unique arena key spaces continue to protect independently allocated fragments from collisions.
They are not, by themselves, stable semantic identities across query recomputation.

## Target Query Layers

The target frontend database grows in the following order:

1. Source-text inputs keyed by canonical file identity.
2. Parsed source templates and typed import sites, shared by every occurrence of one file.
3. Root source graphs and freshly assembled program terms.
4. Desugared and scoped source instances.
5. Checked binding components and their exported static interfaces.
6. On-demand normalization of closed, frozen kinds and types.
7. Aggregated diagnostics and editor-facing semantic indexes.

The first vertical slice deliberately combines layers 3 through 5 into one coarse root analysis query. This proves
input revision handling, dependency tracking, error recovery, and consumer integration before changing checker
semantics or typed representation.

## Query Result Contract

A successful component query will eventually return a `CheckedComponent` containing immutable typed syntax,
provenance, a semantic index, diagnostics, and a `ComponentInterface`. The interface must include every fact that a
dependent check can observe: inferred annotations, transparent type equations and value aliases, opaque nominal
identities, global or inlinable status, and Builtin roles.

The local inference transaction will close and freeze in this order:

1. Finish checking the complete component.
2. Follow all metavariable solutions and reject or recover unresolved exported holes.
3. Perform occurs and existential-escape checks.
4. Convert bound variables to an alpha-stable representation.
5. Replace source-defined nominal identities with durable database identities.
6. Intern or canonicalize exported closed kinds and types.
7. Sort diagnostics and semantic facts deterministically.
8. Publish an immutable fragment and interface.

Errors should remain structured data. Query execution must not print hole solutions or debug output directly;
those observations will become diagnostics or trace events rendered by the CLI and editor adapters.

## Staged Work

### 0. Establish the source query database

- [x] Select Salsa as the incremental query engine and register it as a workspace dependency.
- [x] Introduce a canonical source-text input and on-demand file cache.
- [x] Extract parsed source templates from source-graph occurrence state.
- [x] Memoize parsing independently for each canonical source file.
- [x] Add a coarse root analysis query around assembly, desugaring, resolution, and checking.
- [x] Preserve partial static facts when checking reports type errors.
- [x] Test reuse after no changes, unrelated-file edits, root edits, and imported-provider edits.

This stage now has no compatibility driver. `zydeco-session` owns source composition and frontend analysis;
an owned `ExecutableProgram` is the explicit handoff to consumers that perform mutable lowering.

### 1. Integrate a durable editor database

- [x] Give Cajun one long-lived database rather than rebuilding a project for each request.
- [x] Route open-document overrides through source-text input setters.
- [x] Allow overlay-only roots and imports before files exist on disk.
- [x] Restore a cached disk input when its editor override closes.
- [ ] Refresh cached disk inputs when watched files change outside the editor.
- [x] Keep a consistent query snapshot for each editor request.
- [ ] Add cancellation checkpoints around the coarse checker until finer queries provide natural checkpoints.
- [x] Preserve the existing progress protocol while phase queries are introduced.

### 2. Isolate the inference transaction

- [ ] Replace `Tycker`'s mutable scoped input with an immutable scoped view and a synthetic-definition output arena.
- [ ] Separate immutable checker dependencies from mutable inference state.
- [x] Replace checker printing with returned typed observations.
- [x] Publish an immutable whole-program analysis and an owned lowering handoff.
- [ ] Prevent `FillId` and local skolem identities from escaping the frozen result.
- [ ] Adapt statics formatting and downstream inspection through a semantic facade.

### 3. Introduce durable semantic identities

- [ ] Intern canonical file and source-instance identities in the database.
- [ ] Anchor source definitions to an instance and stable binder location.
- [ ] Split sealed nominal identities from bound variables and temporary skolems.
- [ ] Give typed fragments explicit ownership for their local term and pattern IDs.
- [ ] Make exported interface equality independent of local allocation order.
- [ ] Retain generative nominal identity for distinct import occurrences.

### 4. Query binding components

- [ ] Promote retained `BindingContext` SCCs to typed `ComponentId` values.
- [ ] Check every recursive SCC in one inference transaction.
- [ ] Export a typed interface separately from each component's checked body.
- [ ] Make dependents observe only the interface facts required by static inspection.
- [ ] Recheck downstream components only when an observable interface changes.
- [ ] Keep the residual block body as a dependent query after its binding components.

### 5. Make normalization and consumers demand-driven

- [ ] Normalize only closed frozen types and kinds.
- [ ] Replace direct `types_normalized` indexing with semantic-model accessors.
- [x] Route hover, semantic tokens, and definition lookup through query-owned analysis.
- [x] Give lowering a frozen checked-program view without mutable access to query inputs.
- [ ] Retire the eager whole-arena normalization pass after all consumers use the semantic facade.

## Validation Strategy

Correctness remains the first gate. During migration, the existing and query-backed paths should agree on accepted
programs, rejected programs, root classifications, formatted types, import freshness, and executable behavior.

Incremental tests will record query execution and cover the following edits:

- no input change, which should execute no compiler query again;
- an unrelated file change, which should not invalidate the root;
- whitespace and comment changes, which must update spans and tooling even when static structure is unchanged;
- a private body change whose exported interface remains equal;
- a transparent type equation or public classifier change;
- a provider edit observed through one or several import occurrences;
- an edit inside a recursive binding component; and
- a broken edit followed by recovery, with no stale typed facts crossing revisions.

Cold and warm timings, executed-query counts, and retained cache size will be measured separately. Parallel checking
will wait until query-local mutation and deterministic result ordering are established.

## Progress Log

### 2026-08-03

- Mapped the current source pipeline, mutable checker state, arena identities, import semantics, normalization tables,
  diagnostics, block SCC representation, lowering consumers, and Cajun's rebuild behavior.
- Chose a coarse root analysis query as the compatibility boundary and per-file parsing as the first reusable query.
- Chose Salsa for database revisions, memoization, on-demand inputs, cancellation support, and future interning.
- Registered Salsa at the workspace level and added a `CompilerSession` with canonical, on-demand source inputs.
- Split `SourceTemplate` from `SourceFile`. Parsed text, spans, syntax, documentation, and import sites are shared by
  canonical path, while graph assembly still freshens every import occurrence before name resolution.
- Added memoized parsing, root graph, and root analysis queries. The analysis query runs `Tycker` as a private local
  transaction and publishes an owned `ProgramAnalysis`.
- Retained the checked root on success and the checker's partial `StaticsArena` plus reports on rejection.
- Used Salsa's non-`Update` result escape hatch only for owned, `'static` compiler results. These queries use `no_eq`,
  so every actual recomputation invalidates dependents until frozen structural result equality is available.
- Gave Cajun one long-lived database. Open and changed documents update source inputs, closing a document restores its
  disk contents, and a Tokio mutex keeps each editor analysis on one coherent database revision.
- Moved Cajun's `ProjectState` onto the query-owned analysis result and preserved semantic highlighting after type
  errors. Existing progress event shapes remain intact; execution-aware phase progress awaits finer phase queries.
- Added invalidation tests for identical inputs, unrelated edits, provider edits, root edits, shared parse-template
  reuse, and rejected-source recovery.
- Validated the initial slice against all 123 source-pipeline tests and all 21 Cajun unit and stdio tests.

### 2026-08-05

- Removed `zydeco-driver` rather than retaining a compatibility facade. The replacement crate is named
  `zydeco-session` because its responsibility is a revisioned analysis lifetime shared by tools, not the complete
  compilation pipeline.
- Reduced the shared production boundary to source inputs, parsed templates, import graphs, source assembly,
  frontend queries, immutable semantic results, and typed observations. Backend policy and subprocess logic no
  longer appear in that dependency graph.
- Added disk-backed and overlay-backed source states. Overlay-only roots and imports can be analyzed before their
  files exist, while closing an overlay returns the input to its current disk state.
- Replaced type-checker `print!` and `println!` effects with `TyckObservation` values for inferred holes and explicit
  debug metadata. The CLI renders those values; Cajun can ignore or reinterpret them without redirecting process I/O.
- Published `ProgramAnalysis` as the immutable query result and `ExecutableProgram` as an owned clone for consumers
  whose lowering passes allocate synthetic definitions. This makes the ownership break between a local `Tycker`
  transaction and a frozen result explicit without pretending that `FillId` is a durable cross-revision identity.
- Moved the Stack IR optimization schedule into `zydeco-stackir` and assembly lowering and stack analysis into
  `zydeco-assembly`. These crates now own their semantic pass order without driver logging or dump configuration.
- Rebuilt the CLI as an adapter over `CompilerSession`. It owns typed target selection, source-aware diagnostic
  rendering, native artifact paths, runtime-file copying, external tool invocation, and process exit policy.
- Removed inert verbosity, build-dry-run, link-existing, stage-dump, runtime-trace, and logger configuration paths.
  The dormant REPL source and its commented integration points remain in place unchanged in purpose.
- Migrated Cajun to request-consistent session snapshots and immutable `ProgramAnalysis` values. Hover, definition,
  references, document symbols, semantic tokens, and partial facts after type errors use the same query-owned arenas.
- Kept the former end-to-end assertions as test-only fixtures instead of exporting production compatibility wrappers.
  The production driver-plus-CLI layer fell from 2,859 Rust lines to 2,469 lines, including the newly
  typed CLI diagnostics and native adapters, before further fine-query work.
- Validated the complete workspace test suite, including all 124 session tests, both CLI integration tests, all 21
  Cajun unit and stdio tests, and the native amd64 link-and-run matrix after removing runtime logging. The workspace
  also passes `cargo check --workspace --all-targets` and the repository's `cargo clippy-workspace` workflow.

The remaining disk-input item is intentionally still open. Closing an editor override reloads the file today, but
Cajun does not yet watch unopened imported files for changes made by another process. The next editor step should
connect file-watch events to `refresh_disk` without polling every cached input or invalidating unrelated roots.
