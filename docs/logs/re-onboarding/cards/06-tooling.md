# 06 — Tooling (session, CLI, TUI, Cajun, test harness, xtask)
> Scratch study note for re-onboarding. Not canonical: contracts below point to their authoritative references.

## 1. Responsibility

Tooling turns "a human or a process names a source file" into "a phase crate receives resolved,
checked input", and it owns every surface that reports, executes, and tests the result.
It does not restate language rules: textual parsing, desugaring, and name resolution belong to the
front-end card; typing belongs to statics; IRs, validators, and code generation belong to the
backend cards. A tooling change that needs new language meaning stops at the references.
The cleanup audit's yardstick applies here: beyond the CLI's `check`, `run`, and `build` paths,
none of this layer serves the three artifact demonstrations, yet each piece is built to production
generality with recovery, snapshots, and its own diagnostics path
([size analysis §Tooling at product standard](../../../../docs/evaluations/2026-10-25-size-analysis/README.md#tooling-at-product-standard)).

Current tracked Rust lines in this layer, measured over the tracked `.rs` files under each path with
`git ls-files` and `wc -l` at HEAD `e2dd8979` (the dirty tree is noted in §10):

| Path | Lines | Role |
| --- | ---: | --- |
| `lang/session/src` | 8,419 | revisioned sources, packages, queries, completion |
| `cli/src` | 4,191 | command surface, adaptation, lowering, artifacts, execution |
| `cli/tests` | 5,498 | CLI process and fixture regressions |
| `editor/cajun/src` | 6,507 | LSP server and editor-facing analysis |
| `editor/cajun/tests` | 1,539 | LSP stdio conformance |
| `tui/src` | 1,551 | Ratatui REPL |
| `xtask/src` | 2,361 | resumable Cargo test runner |
| `lang/tests/src` + `lang/tests/tests` | 1,505 + 6,128 | shared source harness and Rust integration targets |

The 2026-10-25 study measured 15,433 production and 14,935 test lines for session, CLI, Cajun,
TUI, and xtask at `6aae43d0`
([§Tooling at product standard](../../../../docs/evaluations/2026-10-25-size-analysis/README.md#tooling-at-product-standard));
the cleanup audit's area table put the same five directories at 27,530 Rust lines, 22% of the total
([§Where the weight is](../../../../docs/evaluations/2026-10-21-cleanup-audit/README.md#where-the-weight-is)).
`lang/tests` is test-only by construction; the audit put the workspace at 74 integration-test
binaries and `lang/tests` alone at about 1.6 GB per debug build
([cleanup audit §Tooling, tests, and documentation](../../../../docs/evaluations/2026-10-21-cleanup-audit/README.md#tooling-tests-and-documentation)).

## 2. Position in the Pipeline

The split is contractual, and the references assign it explicitly.

- **The session owns revisioned source state.** `CompilerSession` manages source revisions,
  snapshots, and cached queries; it expands a project's discovery rules, records editor overlays
  as `SourceInput` fields, resolves packages, and publishes analysis snapshots
  ([C3](../../../../docs/references/compiler.md#c3-source-loading-sessions-queries-and-memory-retention),
  `docs/references/compiler.md:589-600`). It never chooses a backend, writes an artifact, or parses
  command-line text.
- **The CLI owns command policy and the boundary outward.** It selects `package.zy` / `workspace.zy`
  roots, parses options into phase-owned plan types, renders diagnostics, lowers an already checked
  program, writes or links artifacts, and executes backends
  ([cli/README.md](../../../../cli/README.md#run-and-test-programs)). The execution runner consumes
  checked `ExecutableProgram` snapshots, shares immutable arenas across targets, lowers once for
  compiled backends, and owns temporary artifacts (`docs/references/compiler.md:651-654`); backend
  execution policy
  itself is [L15](../../../../docs/references/language.md#selecting-an-execution-backend).
- **The TUI, Cajun, and the test harness are consumers.** The TUI is the interactive front end for
  the same session ([C15 §Interactive Engine](../../../../docs/references/compiler.md#interactive-engine));
  Cajun adapts session facts to LSP and discards superseded results
  ([C15](../../../../docs/references/compiler.md#c15-diagnostics-formatting-documentation-and-interactive-tooling),
  [editor/README.md](../../../../editor/README.md)). `lang/tests` drives CLI types, not a second
  compiler. Everything lands in
  [C15](../../../../docs/references/compiler.md#c15-diagnostics-formatting-documentation-and-interactive-tooling)
  and [C16](../../../../docs/references/compiler.md#c16-validation-debugging-and-extending-the-implementation);
  the [rule-ownership map](../../../../docs/references/README.md#rule-ownership) says where a change belongs.

The session crate has no hidden phase. `lang/session/src/lib.rs:1-10` re-exports `source`, and
`lang/session/src/source/mod.rs:1-11` registers exactly eleven modules: `cache`, `err`, `graph`,
`loader`, `package`, `program`, `prelude`, `query`, `pipeline`, `report`, `warning`.
The directories `lang/session/src/{amd64,check,llvm,zasm,zir}` and `cli/src/{amd64,documentation}`
are **empty and untracked**: `git ls-files` lists no file in them and no module declaration names
them. They are local checkout leftovers, not architecture; treat them as noise (see §10).

## 3. Entry Points and Key Types

| What | Where | Why it matters |
| --- | --- | --- |
| `CompilerSession` | `lang/session/src/source/query.rs:258` | Salsa database, revision owner, and the only public door to analyses |
| `SourceInput` | `lang/session/src/source/query.rs:240` | The revision unit: path, disk text, editor overlay |
| `ProgramAnalysis` | `lang/session/src/source/query.rs:55` | Immutable facts, diagnostics outcome, and observations for one root |
| `SourceTemplate` / `SourceFile` / `SourceGraph` | `lang/session/src/source/graph.rs:30`, `:119`, `:157` | Shared parsed files, per-route nodes, merged DAG with package data |
| `SourceGraphLoader` / `SourceProvider` | `lang/session/src/source/loader.rs:31`, `:23` | Turns a project root into a graph; overlays enter through the provider |
| `Project`, `PackageContext`, `PackageNamespace` | `lang/session/src/source/package/context.rs:157`, `:116`, `:50` | Explicit roots, lexical package context, opaque-name namespaces |
| `PackageScope`, `SourceGraph::merge`, `Shapes` | `lang/session/src/source/package/resolution.rs:89`, `:166`; `lang/session/src/source/package/shape.rs:42` | Lexical scans, structural copy identity, interned shapes |
| `CompletionAnalysis` | `lang/session/src/source/query/completion.rs:27` | Recovered-cursor analysis with disposable arenas |
| `TyckReport` / `SourceCaches` | `lang/session/src/source/report.rs:13`, `lang/session/src/source/cache.rs:14` | Shared ariadne report shape and source caches for every frontend |
| `DiagnosticRenderer` | `cli/src/diagnostics.rs:10` | CLI rendering of session and lowering failures |
| `CommandCompiler` / `BackendProgram` | `cli/src/compile.rs:24`, `:279` | One-shot adapter over the session; frozen lowered program |
| `ExecutionRunner` / `PreparedExecution` | `cli/src/execution.rs:17`, `:181` | Validate, prepare, then run one checked program on selected backends |
| `BuildOptions` / `Executable` | `cli/src/native.rs:11`, `:278` | Build directories, target platform, native linking and exit codes |
| `Commands`, `SourceSelection`, `TargetArchitecture` | `cli/src/cli.rs:158`, `:253`, `:107` | The entire user-facing CLI surface as clap types |
| `Repl` / `ReplEngine` | `tui/src/app.rs:37`; `tui/src/engine.rs:33` | Terminal UI loop and the session wrapper that installs numbered inputs |
| `Cajun` | `editor/cajun/src/lib.rs:150` | LSP server; capabilities at `editor/cajun/src/lib.rs:398-438` fix position encoding and features |
| Harness registration macros | `lang/tests/src/lib.rs:811-917` | `check_source!`, `runtime_source!`, `e2e_sources!`, `e2e_io_source!` |
| `run` / `Store` | `xtask/src/runner.rs:52`; `xtask/src/state.rs:201` | `cargo test-resume` orchestration and its on-disk journal |

## 4. Contracts and Invariants

- **Inputs are revisioned, and an overlay is not a disk write.** `set_overlay` stores text beside
  `disk_text`; `refresh_disk` replaces only the disk input while an overlay stays effective;
  `clear_overlay` drops the overlay and re-reads disk (`lang/session/src/source/query.rs:430-476`).
  Every analysis is paired
  with the session state it was computed from, so a consumer that mixes revisions is wrong by
  construction ([C3](../../../../docs/references/compiler.md#c3-source-loading-sessions-queries-and-memory-retention)).
- **An analysis is a fact snapshot, not a typed tree.** `ProgramAnalysis` retains keyed indexes;
  full occurrence payload is recovered by re-running the coarse check
  (`materialize_arena`, `checked_program`, `executable_program`;
  `lang/session/src/source/query.rs:497-552`), whose memo
  keeps only the latest root (`lru = 1`). Calling these on a stale analysis re-checks it
  ([C3 §Analysis Facts and Materialization](../../../../docs/references/compiler.md#analysis-facts-and-materialization)).
- **Package identity is structural.** The merge pass interns each file's shape — syntax variants,
  scalar data, attached text, relevant meta annotations, signatures, prelude edge, and resolved
  import targets — and reuses an existing merged source when the key matches
  (`lang/session/src/source/package/resolution.rs:196-242`,
  `lang/session/src/source/package/shape.rs:48-109`). Copy identity therefore follows
  [copy/resolve/merge/analyze](../../../../docs/references/language.md#copy-resolve-merge-analyze),
  not file paths.
- **Diagnostics are complete collections, but their order is not an API.** Parsing retains every
  issue; the session concatenates typed collections per phase; CLI and TUI enumerate them
  independently (`lang/session/src/source/query.rs:199-238`, `cli/src/diagnostics.rs:13-50`). Tests
  compare contents, multiplicity, and locations
  ([C15 §Diagnostic Collection](../../../../docs/references/compiler.md#diagnostic-collection)).
- **Execution shares one checked snapshot and one lowering per command.** `ExecutionRunner::prepare`
  validates first, lowers only if a compiled target was requested, and reuses that lowering for all
  selected backends; preparation never executes (`cli/src/execution.rs:110-178`). Backend policy stays in
  [L15](../../../../docs/references/language.md#selecting-an-execution-backend).
- **The harness is declaration-driven.** A `.zy` case states stage, prelude, and expectation in
  leading comments; `case_fixtures()` discovers one trial per file without Rust registration
  (`lang/tests/src/lib.rs:790-808`, `lang/tests/tests/cases.rs:12-16`). Whole-program execution
  moves to the directive runner only when a fixture declares `-- stage: run`
  (`lang/tests/tests/directives.rs:16-40`); `.zy.in` templates substitute named parameters
  verbatim and cannot loop
  ([C16 §Source Fixtures and Runtime Oracles](../../../../docs/references/compiler.md#source-fixtures-and-runtime-oracles)).
- **Formatting policy lives in the source, not in each frontend.** `zydeco fmt` parses, prints, and
  compares (`cli/src/format.rs:60-74`); Cajun calls the same textual formatter
  (`editor/cajun/src/format.rs:33-49`). The CLI and Cajun must not introduce independent rules
  ([C15 §Formatting and Typed Rendering](../../../../docs/references/compiler.md#formatting-and-typed-rendering)).

## 5. Data Flow

The path from named source to executed artifact crosses four stages, each with one interface:

1. **Project roots.** The CLI checks for `package.zy` and `workspace.zy` in the working directory,
   expands them through `CompilerSession::project`, and stores the resulting `Project` in the
   `CommandCompiler` (`cli/src/main.rs:37-51`;
   `lang/session/src/source/query.rs:327-341`). Discovery globs expand here
   (`lang/session/src/source/package/discovery.rs:16-123`); project registrations are consumed in the
   loader even though only session tests construct them (`lang/session/src/source/loader.rs:265-284`).
2. **Source graph.** Loading goes through `QuerySourceProvider`, which reads disk text or overlay
   from `SourceInput` and parses each file once (`lang/session/src/source/query.rs:298-316`,
   `:679-694`). `SourceGraphLoader::load_root` then builds the DAG, attaching ordinary imports, the
   default-prelude edge, and a `.zyi` companion signature edge
   (`lang/session/src/source/loader.rs:159-208`, `:483-519`). The merge pass rewrites routes into
   merged sources and package records (`lang/session/src/source/package/resolution.rs:166-317`).
3. **Program pipeline.** `SourceGraph::parse` copies templates into one textual program
   (`lang/session/src/source/program.rs:21-32`), `desugar` and `resolve` produce bitter and scoped
   programs (`lang/session/src/source/pipeline.rs:35-61`), and `resolved_data` memoizes the result
   before the coarse `check_source` query publishes statics
   (`lang/session/src/source/query.rs:734-770`; `analyze_source` at `:772-806`).
4. **Consumers.** CLI commands materialize facts or the executable program
   (`lang/session/src/source/query.rs:501-580`), lower through `CommandCompiler`
   (`cli/src/compile.rs:265-275`, `:340-396`), and execute through `ExecutionRunner`
   (`cli/src/execution.rs:112-178`). The TUI wraps each submission in a generated root, analyzes the
   direct import first, then a `ret` wrapper, and folds observations into the rendered result
   (`tui/src/engine.rs:48-125`, `:335-355`). Cajun loads a project state, materializes its own root
   arena for its lifetime, and refreshes only after a revision check passes
   (`editor/cajun/src/analysis.rs:184-207`; `editor/cajun/src/lib.rs:183-196`). The Rust harness uses
   `CommandCompiler` + `ExecutionRunner` for every backend, so tests and CLI agree
   (`lang/tests/src/lib.rs:70-77`, `:159-173`).

Revisions enter only at `SourceInput` and `SessionState::advance_revision` (Cajun,
`editor/cajun/src/lib.rs:101-127`); overlays enter only through `set_overlay`; packages enter only
through `Project` roots, discovery rules, and registrations. No consumer inspects files directly
except the formatter and artifact builders, which are filesystem adapters outside the session.

## 6. Verification Commands

Run from the repository root; the `zydeco` binary on `PATH` is stale, so always go through Cargo.

| Purpose | Command | Expected observable outcome |
| --- | --- | --- |
| Package/discovery surface | `cargo run --quiet --bin zydeco -- show` | 212 lines: 9 `library` + 30 `test` declarations and 30 `of` relationship lines |
| Check + interpreter test path | `cargo run --quiet --bin zydeco -- test lib/tests/std/bool.zy` | `PASS [interpreter] …/lib/tests/std/bool.zy`, `1 passed; 0 failed.`, exit 0 |
| Lowering path | `cargo run --quiet --bin zydeco -- build lib/tests/core/fact.zy -t zir \| wc -l` | 231 lines of SPSLow; 161 are `[parameter:` declarations |
| CLI command surface | `cargo run --quiet --bin zydeco -- --help` | Lists `show`, `passes`, `fmt`, `run`, `check`, `test`, `repl`, `build` |
| REPL entry | `cargo run --quiet --bin zydeco -- repl --help` | `Usage: zydeco repl`, only `-h, --help` |
| TUI binary has no argument parser | `cargo run -p zydeco-tui -- --help` | Ignores `--help` and initializes a full-screen terminal; in a non-TTY sandbox it panicked with `failed to initialize terminal: PermissionDenied` |
| Resumable runner state | `cargo run --quiet -p xtask -- test-resume --status` | Prints saved-state path and counts; observed `2306 passed, 0 failed, 0 ignored, 0 pending across 96 targets`, saved 6 days earlier |
| Formatter check | `cargo run --quiet --bin zydeco -- fmt --check lib/tests/core/fact.zy` | No output, exit 0 (file already canonical) |

The TUI probe only shows that the binary ignores flags and needs a terminal; interactive behavior
was not exercised. The `test-resume` probe reads an older saved state, not a fresh run.

## 7. Reading Order

1. [C3](../../../../docs/references/compiler.md#c3-source-loading-sessions-queries-and-memory-retention):
   session responsibilities, loading, package selection, facts, retention.
2. `lang/session/src/lib.rs` → `source/mod.rs` → the public half of
   `lang/session/src/source/query.rs` (`:240-673`): types, snapshots, overlays, materialization.
3. `loader.rs` → `graph.rs` → `package/{context,discovery,resolution,shape}.rs`: how routes, shapes,
   instances, and packages become one graph.
4. `program.rs` → `pipeline.rs`: the textual program DAG and the desugar/resolve contracts.
5. `cli/src/cli.rs` → `main.rs` → `compile.rs` → `execution.rs` → `diagnostics.rs`: command policy,
   adaptation, lowering, execution, rendering.
6. [`cli/README.md`](../../../../cli/README.md) and
   [L15](../../../../docs/references/language.md#selecting-an-execution-backend): user-facing
   behavior and backend policy.
7. `editor/cajun/src/lib.rs` (capabilities, revision gate) → `analysis.rs` (project state) →
   `completion.rs` / `document_links.rs` (semantic projections).
8. `tui/src/engine.rs` → `submission.rs` → `app.rs`: submission wrappers, commands, terminal loop.
9. `lang/tests/src/lib.rs` macros → [`lang/tests/cases/README.md`](../../../../lang/tests/cases/README.md)
   → `lang/tests/tests/{cases,directives}.rs` → the `.zy.in` template module.
10. `xtask/src/{cli,runner,state}.rs` and
    [CONTRIBUTING §Iterate with Resumable Tests](../../../../CONTRIBUTING.md#iterate-with-resumable-tests)
    last: it exercises the whole workspace, not the compiler.

## 8. Checkpoint Questions

1. Which object owns a source revision, and what does `snapshot` promise? → §4 bullet 1;
   [C3](../../../../docs/references/compiler.md#c3-source-loading-sessions-queries-and-memory-retention).
2. Why can a `ProgramAnalysis` not answer an arbitrary typed query directly? → §4 bullet 2;
   [C3 §Analysis Facts and Materialization](../../../../docs/references/compiler.md#analysis-facts-and-materialization).
3. What makes two package copies merge or stay distinct? → §4 bullet 3;
   [copy/resolve/merge/analyze](../../../../docs/references/language.md#copy-resolve-merge-analyze).
4. Where does a `.zy` file enter the pipeline, and how many times is it parsed? → §5 steps 1–2;
   `lang/session/src/source/loader.rs:428-526` caches `SourceTemplate` per canonical path.
5. Which runner executes `-- stage: run` fixtures, and what does the other one reject? →
   `lang/tests/tests/directives.rs:94-166`; `lang/tests/tests/cases.rs:20-39`.
6. What does `cargo test-resume` save, and when is a pass reused? → §6 row 7;
   [CONTRIBUTING §Iterate with Resumable Tests](../../../../CONTRIBUTING.md#iterate-with-resumable-tests).
7. Which layer rejects an unsupported `--target-arch`: clap, session, or build? → §9 row 8 and
   `cli/src/main.rs:370-372`.
8. How do the CLI and Cajun share formatter behavior without sharing code paths? → §4 bullet 7;
   `lang/surface/src/textual/pretty.rs` is the one implementation.
9. Are `lang/session/src/amd64` and friends compiler phases? → no; empty untracked directories,
   §2 last paragraph.
10. What must be reworked if package relationships are deleted? → §9 row 1:
    `zydeco show`, Cajun document links, and the structural merge key.

## 9. Redundancy and Cleanup Signals

| # | Signal | Evidence | Status | Assessment |
| --- | --- | --- | --- | --- |
| 1 | Package hierarchy: opaque roots, unnamed packages, structural copies, `discover` globs, `test(of(...))` relationships | [Audit §Package hierarchy](../../../../docs/evaluations/2026-10-21-cleanup-audit/README.md#package-hierarchy) estimates ≈1.5k Rust + ≈2k test + 212 reference lines; session package modules measure 1,065 lines, with 570 more in dedicated test files; clients are `show` (`cli/src/main.rs:218-220`), Cajun document links (`editor/cajun/src/document_links.rs:76-114`), and the merge key (`lang/session/src/source/package/resolution.rs:209-212`) | decision needed | Removal changes approved reference text ([language.md §Source Packages](../../../../docs/references/language.md#source-packages), [§Package Hierarchy](../../../../docs/references/language.md#package-hierarchy)) and copy identity, so it needs owner approval and same-change tool rework |
| 2 | Root substitution / project registrations | `with_registration` is constructed only in session tests (`lang/session/src/source/package/tests.rs:102`, `:342`); production code only consumes registrations (`lang/session/src/source/loader.rs:265-284`, `lang/session/src/source/query.rs:376-382`); the call is documented in [compiler.md:597](../../../../docs/references/compiler.md#source-loading-and-package-selection) | decision needed | Falls with row 1; deleting the constructor also edits the C3 loading paragraph |
| 3 | Glob discovery walker | 127-line walker in `lang/session/src/source/package/discovery.rs`, tested by 181 lines; serves two `workspace.zy` files (root and `cli/tests/fixtures/packages/workspace.zy`) per the [audit](../../../../docs/evaluations/2026-10-21-cleanup-audit/README.md#package-hierarchy) | decision needed | Same package decision; would otherwise leave the two workspace files unselectable |
| 4 | Test-harness multiplicity, 13 registration mechanisms | [Size analysis §Tooling at product standard](../../../../docs/evaluations/2026-10-25-size-analysis/README.md#tooling-at-product-standard) counts 72 targets and thirteen mechanisms; verified here: four public macros plus two internal (`lang/tests/src/lib.rs:811-917`), `.zy` case discovery (`lang/tests/src/lib.rs:790-808`, `lang/tests/tests/cases.rs:12-16`), directive runner (`lang/tests/tests/directives.rs:16-40`), `.zy.in` templates (`lang/tests/src/template.rs:10`), 46 Rust integration files, 13 CLI test files; `cargo metadata` reports 74 test targets in the dirty tree | open | The recorded direction is "one test harness per crate" ([§Structural simplifications](../../../../docs/evaluations/2026-10-25-size-analysis/README.md#structural-simplifications)); no reference text blocks it |
| 5 | Resumable test runner plus tracked completions/tree-sitter parser | Audit update records that the repository **declined** these removals ([cleanup audit, intro update paragraph, lines 26–28](../../../../docs/evaluations/2026-10-21-cleanup-audit/README.md)); retained evidence: `xtask/src` 2,361 lines, `cli/completions` 1,235 tracked lines generated by `cli/build.rs:19-24`, `editor/tree-sitter-zydeco/src/parser.c` 19,916 tracked lines, documented at [CONTRIBUTING §Iterate with Resumable Tests](../../../../CONTRIBUTING.md#iterate-with-resumable-tests) | resolved | Deliberately retained; `.cargo/config.toml:17-18` makes `cargo test-resume` a first-class alias |
| 6 | `env!("CARGO_MANIFEST_DIR")` paths in installed binaries | [Audit §Defects](../../../../docs/evaluations/2026-10-21-cleanup-audit/README.md#defects-found-during-the-audit); production uses are `lang/session/src/source/prelude.rs:11-22` and `tui/src/engine.rs:44`; the bundled fallback (`lang/session/src/source/prelude.rs:58-70`) covers five bootstrap files but the test asserts `builtin.zy` is not bundled (`:84`), which the TUI imports | open | Unrecorded drift: `grep` finds no entry in `docs/todos`, while the audit says it "belongs there before repair" |
| 7 | Duplicate runtime digest and platform computation on every `-t exe` build | [Audit §Defects](../../../../docs/evaluations/2026-10-21-cleanup-audit/README.md#defects-found-during-the-audit) cites `cli/src/main.rs:496-513`; HEAD still calls `LibraryDigest::runtime` and `LibraryPlatform::for_target` twice (`cli/src/main.rs:496-514`), before any check that either import list is nonempty | open | Unrecorded drift; a local deduplication that changes no reference text |
| 8 | `--target-arch` accepts `Aarch64` but supports only AMD64 | [Audit §Test-only generality](../../../../docs/evaluations/2026-10-21-cleanup-audit/README.md#test-only-generality) and [post-audit verification](../../../../docs/evaluations/2026-10-21-cleanup-audit/README.md#post-audit-verification); values at `cli/src/cli.rs:107-130`, early rejection at `cli/src/main.rs:370-372` (moved there by HEAD `e2dd8979`); documented override in [README.md:108-111](../../../../README.md) and [cli/README.md:83-85](../../../../cli/README.md#native-executables) | open | HEAD already moved rejection before compiling; the option remains documented as the only way for an ARM host to request AMD64, so narrowing the value set still needs a CLI-docs decision |
| 9 | Cajun live editor configuration for two hover settings | [Audit §Test-only generality](../../../../docs/evaluations/2026-10-21-cleanup-audit/README.md#test-only-generality) estimates ≈270 lines; measured `editor/cajun/src/configuration.rs` is 268 lines (production 1-186, tests 187-268); documented at [editor/README.md:59-101](../../../../editor/README.md#runtime-configuration) and referenced from [C15](../../../../docs/references/compiler.md#cursor-based-selection) | decision needed | Removing it edits the editor guide and C15's "configured endpoint policy" account, so it needs approval |
| 10 | CLI and TUI repeat diagnostic dispatch, and the TUI hand-writes an ANSI stripper | [Audit §Parallel implementations](../../../../docs/evaluations/2026-10-21-cleanup-audit/README.md#parallel-implementations); `cli/src/diagnostics.rs:13-50` vs `tui/src/diagnostics.rs:6-38`; escape state machine at `tui/src/diagnostics.rs:40-76`; both already share `SourceCaches`/`TyckReport` | open | Merge behind one session-side renderer; no approved rule changes |
| 11 | `docs/evaluations` tracked data volume | Size analysis recorded 90,248 tracked lines at `6aae43d0` ([§Repository weight outside Rust](../../../../docs/evaluations/2026-10-25-size-analysis/README.md#repository-weight-outside-rust)); measured now: 31,647 lines, because `9bebc585` and `118b8ab1` (2026-10-28) retired four study directories; the remaining 29,695 lines are the CBPV runtime evidence that [`docs/evaluations/README.md`](../../../../docs/evaluations/README.md) retains | resolved | The record's figure no longer describes HEAD; further reduction is a release-asset decision, not code cleanup |
| 12 | Tooling scope sits outside the artifact boundary | [Size analysis §Design decisions for the artifact](../../../../docs/evaluations/2026-10-25-size-analysis/README.md#design-decisions-for-the-artifact) moves session overlays, Cajun, TUI, packages/discovery, and xtask to an extras workspace | decision needed | Largest structural cleanup, but it changes the artifact claim and approved reference text; the review says this decision comes first |

## 10. Uncertainties and Drift

- All evaluation figures describe their measured revisions (`7e5ee4c1` for the audit, `6aae43d0`
  for the size analysis), not HEAD `e2dd8979`. Re-measured here: `docs/evaluations` 31,647 tracked
  lines (not 90,248), session package modules 1,065 lines, `editor/cajun/src/configuration.rs` 268 lines,
  74 Cargo test targets in the dirty tree.
- The 13 registration mechanisms were not re-counted; §9 row 4 enumerates the forms verified in this
  checkout and attributes the count to the evaluation. The audit's "74 integration-test binaries"
  and the size analysis's "72" also disagree; the current 74 includes the untracked
  `lang/tests/tests/short_circuit.rs`, so the tracked number is 73.
- The audit's "session's whole-program test pipeline" removal could not be tied to a named API: the
  plausible commits are `29a7f107` (removes 154 lines of session source tests) and `ef7d1e6c`
  (171 lines), and the current `CompilerSession` public surface
  (`lang/session/src/source/query.rs:319-672`) contains no
  test-execution entry point; execution lives in `cli/src/compile.rs:198-263`.
- The audit says Cajun completions surface package relationships; in this checkout only document
  links consume `relations` (`editor/cajun/src/document_links.rs:76-114`), while completion exposes
  package roles and options from the metadata catalog
  (`editor/cajun/src/completion.rs:153-181`). Possible record drift.
- `--target-arch` status is a judgement call: the audit calls it test-only generality, but both
  README and `cli/README.md` require it for ARM hosts, so this card marks it open rather than
  removable.
- Collected as observed behavior: `cargo run -p zydeco-tui -- --help` ignores the flag and needs a
  terminal. There is no clap parser in `tui/src/main.rs`; the CLI help advertises `repl` only.
- The empty directories `lang/session/src/{amd64,check,llvm,zasm,zir}` and
  `cli/src/{amd64,documentation}` are untracked, so no commit or reference explains them; they were
  read (empty) and are excluded from this card's architecture claims.
- Local `.claude/worktrees/` copies and `editor/zed/target` artifacts are untracked build/checkout
  noise; they were excluded from measurements but distort naive repository-wide greps.
- No full workspace suite, doctest suite, or interactive REPL/TUI session was run. The
  `test-resume --status` output is a six-day-old saved state (96 targets, 2306 passes), not a
  current verification.
