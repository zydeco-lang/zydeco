# Artifact boundary and mainline admission

Status: decision memo for the owner. It changes no code; it asks for one boundary decision and one admission rule,
and it sequences every later cleanup against them.

## Motivation and context

The owner's goal is a cleanup campaign: review past features, then remove redundant code and features.
Zydeco grew as a research prototype, so it accumulated orthogonal product features at production generality:
incrementality, editor recovery, formatting fidelity, foreign interfaces, manual memory, and optional translations.
Each arrived with its own support modules, tests, and documentation, and none displaced an earlier mechanism.
"Redundant" is therefore ambiguous until the repository names what the implementation is for; without that boundary,
removal arguments reduce to taste, and a plausible-looking deletion can weaken a paper claim.

Two papers define what the implementation is for.
The OOPSLA relative-monad paper claims that relative monads model stack-manipulating computation,
that monadic blocks reinterpret CBPV by any relative monad, and that those blocks derive monad transformers;
its artifact additionally relies on term-oriented composition.
The stack-passing-style (SPS) paper contributes two calculi, SPS and `SPS_l`, and an implementation claim:
lowering, β-simplification, closure conversion with existential packages, and lowering to executable code.
The [language reference](../references/language.md)
and the [compiler reference](../references/compiler.md) already own those semantics and the phases that realize them;
`lang/stackir` implements both calculi.

Two dated studies measured the repository against those claims.
The **retired 2026-10-21 cleanup audit** catalogued features whose only client is their own tests
and proposed a recurring pattern.
The **retired 2026-10-25 size analysis** explained where lines go when no feature removal reaches them
and listed the artifact decisions that follow.
Both are proposals, not decisions; this memo distills their durable content, supersedes their reports,
and cites only canonical references, code, and commits.

The campaign therefore starts with one question: which crates, directories, and behaviors form the artifact core,
where does the rest live, and what rule must a future feature pass to stay on the main line?
This memo proposes an answer for the owner to approve or redirect.
It does not approve any individual removal; each later deletion or simplification still needs its own change,
and any change to approved reference text needs the owner's permission
under [AGENTS.md](../../AGENTS.md#documentation-structure-and-design-records).

## Yardstick

A part of the implementation serves the artifact when it contributes to one of the two paper demonstrations.
This is the criterion used throughout this memo and by every later proposal.

- **The OOPSLA artifact.** Relative monads model stack-manipulating computation
  ([L11](../references/language.md#11-relative-monads)); monadic blocks reinterpret CBPV code with any relative monad,
  including the transformer derivation; and term-oriented composition gives each term static semantics
  in a context ([L1](../references/language.md#term-oriented-composition)).
  The artifact's executable evidence is the interpreter plus the examples
  under [`lib/tests/oopsla`](../../lib/tests/oopsla/README.md), which need only `zydeco check` and the interpreter.
- **The SPS paper.** SPS makes the ambient stack explicit and second-class;
  `SPS_l` is its first-order closure-converted form.
  The implementation claim covers lowering checked CBPV to SPS
  and normalizing it ([C8](../references/compiler.md#c8-sps-lowering-normalization-and-demand)),
  closure conversion with existential closure
  and continuation packages ([C9](../references/compiler.md#c9-closure-conversion-and-first-order-spslow)),
  and lowering through ZASM and the native or Wasm backend
  to executable code ([C10–C13](../references/compiler.md#c10-zasm-stack-analysis-and-local-representation-choices)).
  The compiled-path examples under `lib/tests/core` and the directive runner are its evidence.
- **Everything else** is judged by whether a maintained client needs it, or lives outside the artifact.
  Tooling, formatting fidelity, incrementality, packages, FFI, manual memory,
  and numerics are not demonstrations; packages and a restricted monadic translation are still needed
  as means (see [Design decisions](#design-decisions-for-the-artifact)).

Two consequences follow, and both constrain the cleanup. First, not every unused-looking mechanism is scaffolding:
the SPSLow validators are the pipeline's only check that its `SPS_l` output is well typed,
so deleting them selects a different checking strategy rather than deleting housekeeping.
Second, removals are not equal: deleting a test-only feature saves its code,
while replacing the per-judgment query layer with direct allocation saves several times
as much and changes no source behavior.
The yardstick ranks candidates; it does not schedule them.

## Recurring pattern

The cleanup audit observed a pattern in nearly every removable feature: the feature's only clients were its tests,
or a small surface that the same change could rework.
It proposed the following admission rule for the main line:

> A language form, standard-library module, compiler pass, or CLI option stays on the main line only
> when something other than its own tests uses it: the standard library, the OOPSLA artifact, the SPS
> paper's compiled pipeline and its evidence, a demo, or a maintained tool path.

Work without such a client lives on an experiment branch or in a proposal until a client appears.
A removal that displaces a tool or library client must rework that client in the same change,
and a compatibility layer needs explicit authorization plus a stated removal condition
([AGENTS.md working principles](../../AGENTS.md#working-principles)).

**The decision requested here is to adopt this rule as the mainline admission convention.** It is a design decision
because it changes what future features may enter the tree and what evidence their proposals must supply;
it does not remove anything by itself.
Its practical form is a checklist question for every proposal and card review:
name the client first, then count the code.
Fixtures are clients only for the claim they evidence: OOPSLA examples admit interpreter features,
SPS fixtures admit lowering and backend features, and no unrelated feature is admitted merely
because a test can call it.
A demo is a client when it is maintained and reachable from the CLI or the test suite.

## Where the weight is

This is the measured baseline for the campaign. The table counts Rust lines at `e2dd8979` (2026-10-30),
the revision the retired studies' findings were re-checked against and the snapshot the study manual documents.
At the time of writing, HEAD `c6c90588` adds only the study manual, proposals,
and short-circuit test sources relative to that revision, so non-test production lines are unchanged.

| Crate | Production | Test | | Crate | Production | Test |
| --- | ---: | ---: | --- | --- | ---: | ---: |
| `lang/utils` | 2,687 | 1,031 | | `lang/session` | 4,498 | 3,921 |
| `lang/derive` | 297 | 171 | | `lang/tests` | 0 | 7,633 |
| `lang/machine` | 1,019 | 280 | | `cli` | 3,691 | 6,283 |
| `lang/syntax` | 2,930 | 626 | | `tui` | 1,140 | 411 |
| `lang/surface` | 15,122 | 8,376 | | `editor/cajun` | 4,124 | 3,922 |
| `lang/statics` | 26,327 | 3,320 | | `xtask` | 1,898 | 463 |
| `lang/dynamics` | 2,778 | 603 | | `runtime` | 1,737 | 447 |
| `lang/stackir` | 12,786 | 6,304 | | `lang/wasm-sps` | 1,221 | 115 |
| `lang/assembly` | 2,789 | 1,555 | | `lang/wasm-common` | 874 | 0 |
| `lang/amd64` | 2,173 | 0 | | **Total** | **88,091** | **45,461** |

Production code lines that exclude blanks and line comments total **76,775**;
`lang/statics` accounts for 23,006 of them.
The largest single crates are `lang/statics` (26,327), `lang/surface` (15,122), and `lang/stackir` (12,786);
the tooling crates hold 15,351 production and 15,000 test lines, and the test-only `lang/tests` crate holds 7,633.
Outside Rust the tree holds 16,326 tracked Markdown lines, 31,647 tracked lines under `docs/evaluations/`
before the two retired studies leave the tree, and 2,013 lines of removed-syntax notes under `docs/legacy/`.
`DESIGN.md` is 995 lines today;
the [reference-drift todo](../todos/reference-drift.md#duplicate-accounts-and-scratch-records) records
that much of it repeats the references.

**Method.** These figures were re-measured from a clean `git archive e2dd8979` tree.
The crate table uses the retired size analysis's line accounting: a file counts as test code
when a path component is `tests` or `examples`, when its name ends in `tests.rs`,
when a `#[cfg(test)] mod name;` declaration names it or an ancestor module, and otherwise the lines
from the first inline `#[cfg(test)]` attribute to the end; production code lines exclude blanks and line comments.
The retired script prints this table and then stops in its support-module section
because `lang/surface/src/textual/ugly.rs` no longer exists after `0f0f311f`;
only the table and production code totals are reproduced here.
Spot checks with `wc -l` and `find` reproduce the named figures: `lang/statics/src/query` is 2,307 lines,
45 producer functions in `lang/statics/src` carry `no_eq, unsafe(non_salsa_values)`,
`lang/surface/src/textual/pretty.rs` is 5,489 lines, `lib/std/memory` is 2,435 lines,
and `git ls-files '*.md'` gives 19,524 lines at HEAD, which is the 16,326 baseline plus the study manual's 2,845 lines
and 353 lines of new proposals.
The 16,326 figure includes 18 lines of proposal edits that were uncommitted when the manual measured it;
the clean `e2dd8979` archive has 16,308.

Treat every number as a dated snapshot.
The retired study's own limits apply, and this memo cannot replace a re-measurement before an individual removal.

## Proposed core workspace

The core workspace contains exactly what the three demonstrations need,
and it must build, test, and release without the extras.
The starting point is the retired size analysis's core list, with the support crates,
`lang/wasm-common` as the Wasm runtime contract, and a reduced `lang/session` made explicit.

| Component | Core disposition | Reference |
| --- | --- | --- |
| `lang/utils`, `lang/derive`, `lang/syntax` | Keep as support: spans, arenas, pass framework, macros, shared syntax vocabulary | [C1–C2](../references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance) |
| `lang/surface` | Reduced: textual parser, metadata catalog, desugaring, resolution; no comment/layout retention, formatting directives, or corpus fidelity | [C4](../references/compiler.md#c4-parsing-desugaring-and-name-resolution) |
| `lang/statics` | Reduced: checking, inference, elaboration, static elimination, and the validation the interpreter and lowering require; the query layer is subject to its own decision | [C5–C6](../references/compiler.md#c5-typed-representation-judgments-and-inference) |
| `lang/dynamics` | Keep: the reference interpreter is the oracle for every backend | [C7](../references/compiler.md#c7-linking-and-the-reference-interpreter) |
| `lang/stackir` | Keep: SPS and SPSLow lowering, normalization, closure conversion, and the checks the paper claims need | [C8–C9](../references/compiler.md#c8-sps-lowering-normalization-and-demand) |
| `lang/assembly`, `lang/amd64`, `lang/machine`, `runtime/` | Keep: ZASM, native preparation, emission, and the shared runtime model | [C10–C12](../references/compiler.md#c10-zasm-stack-analysis-and-local-representation-choices) |
| `lang/wasm-sps`, `lang/wasm-common` | Keep: the structured Wasm backend from SPSLow and its runtime contract | [C13](../references/compiler.md#c13-webassembly-backend-and-embedding) |
| `lang/session` | Reduced: source loading, package resolution, coarse analysis, and materialization for the CLI and tests; overlays and editor recovery are extras | [C3](../references/compiler.md#c3-source-loading-sessions-queries-and-memory-retention) |
| `cli` | Reduced: `check`, `run`, `build`, `test`, and SPS pass inspection for interpreter, AMD64, and `wasm-sps`; formatting, REPL, package listing, C-library and native-unit artifact commands are extras | [C1](../references/compiler.md#c1-architecture-and-a-programs-path-through-the-compiler), [L15](../references/language.md#selecting-an-execution-backend) |
| `lang/tests` | Keep one consolidated harness for the core fixtures | [C16](../references/compiler.md#source-fixtures-and-runtime-oracles) |
| `lib/std` | Reduced: the modules the core programs and OOPSLA examples reach, including the monad interfaces and the buffer/bytes storage `std.zy` reaches | [L12](../references/language.md#12-sources-imports-and-entry) |
| `lib/tests/oopsla`, `lib/tests/core`, `lang/tests/cases` | Keep as paper evidence: OOPSLA examples and the SPS compiled-path fixtures | [C16](../references/compiler.md#source-fixtures-and-runtime-oracles) |

Three scopes are **retained decisions, not extras**: packages stay because the standard library uses them
and static elimination of a dependently typed module language into plain CBPV is the clearest CBPV-as-IR demonstration;
monadic translation stays restricted to what the standard library uses; numerics are reduced to `Int`,
`Float64`, `Char`, `String`, `Bool`, and `Unit` rather than kept broad.
Those are D6–D8 below. The [retained list](#retained-after-review) — interpreter,
value functions and static elimination, recursive field search, `begin ... end` with `that`, `pack`, `|>`,
field puns, `@(m)`, SPS normalization, closure conversion, ZASM lowering, frame planning,
the copying collector, and `@[typeof]` — is core content and is not re-litigated by the boundary decision.

The core keeps its own dependency direction: support crates, then front end, statics,
dynamics, and the two backends; the reduced CLI and test harness depend on them.
No core crate may depend on an extra.

## Proposed extras and their destination

Extras are the mechanisms whose clients are a tool surface, an experiment, or a paper claim that the core does not make.
Some move; some are deleted. The distinction matters for every later patch.

| Extra | What moves | Why it is not core | Destination |
| --- | --- | --- | --- |
| Editor and session incrementality | Overlays, revision gating, completion/recovery queries, editor-facing fact queries in `lang/session` and `cli` | The core CLI loads and checks files; no core consumer edits a live buffer | Extras workspace; Cajun depends on a reduced core session |
| Cajun and editor integrations | `editor/cajun`, `editor/vscode`, `editor/zed`, `editor/tree-sitter-zydeco` | LSP, hover, rename, semantic tokens, and grammar maintenance are maintained tool paths but not artifact claims | Extras workspace; grammars may stay with their clients or move to a separate repository |
| TUI REPL | `tui`, `zydeco repl` | Interactive front end for the same session; the paper evidence uses checked programs, not the terminal UI | Extras workspace |
| Formatter fidelity | Comment and layout retention, `@[format(...)]` directives, `zydeco fmt`, formatter corpus tests, most of `lang/surface/src/textual/pretty.rs` | Canonical source printing is tooling; the artifact needs diagnostics and IR rendering, not author-layout preservation | Extras workspace; a minimal canonical printer may stay if core tests need it |
| FFI, C libraries, native units | C exports and imports, `--link-library`, object/staticlib/sharedlib targets, native-unit manifests (`cli/src/{library,unit,native}.rs`, libffi path) | The systems-programming thread lies outside both yardsticks and costs ≈1.9k production plus ≈1.2k test lines | Extras workspace behind a cargo feature; optional experiment branch |
| Manual memory and scalar kernels | `lib/std/memory` beyond the buffer/bytes reach of std, raw kernels and memory/scalar IR forms, `Addr` views | Same thread; std reaches only 592 of 2,475 memory lines at the measured revision, and the IR forms add to both SPS and SPSLow | Extras or experiment branch; keep a raw `Addr` adapter for bytes and I/O |
| Monadic extensions beyond std | Witness-dependent arrows, polymorphic value functions, applicative nominal families, and value roots in the translator | Only test clients; the shared nominal machinery stays with the computation translation std uses | Reject the payloads with the existing `tyck.expressivity` error rather than move code |
| Effect-library breadth | Reader, Writer, Option, StateExn and other effect modules beyond a minimal demonstration | No non-test module outside `lib/std/effect` imports the library; OOPSLA imports only `monad.zy` | Extras or examples; keep the monad interface, transformer derivation, State, Exception, and NonDet |
| Repository task runner | `xtask` and the resumable runner | Repository housekeeping, not artifact behavior; `cargo test-all` remains the contract | Extras workspace or keep in the repository as tooling; it is not part of the artifact build |
| Evaluation data and retired studies | Dated studies under `docs/evaluations/`, including the two retired 2026-10 studies | Measurement evidence is not a research demonstration and dominates non-Rust weight | Release asset or separate data repository; the two retired 2026-10 studies leave the tree with their retirement |
| Historical notes | `docs/legacy` | Removed syntax; not a rule home | Delete or archive outside the tree |

**Options for the destination.** Three options were considered; the recommendation is staged.

| Option | What it means | Tradeoffs | Recommendation |
| --- | --- | --- | --- |
| Status quo | Keep one workspace and one artifact; apply only the admission rule | No boundary to enforce; artifact reviews still meet tooling, FFI, and memory breadth; CI remains monolithic | Fallback if the owner prefers minimal churn; does not satisfy the artifact goal |
| Staged split in this repository | Move extras under an `extras/` root with their own workspace, keep one git history and release tag; gate CI jobs; core builds and tests alone | Reversible, one lockfile policy and shared target directory during the transition, small coordination cost; extras depend on core by path | **Recommended first step** |
| Separate repository now | Move extras to another repository and publish core as the artifact | Cleanest artifact, but cross-repo changes, version pairing, and duplicated CI are expensive before dependency directions settle | Later, after the staged split proves the dependency direction |

The staged split is recommended because it makes the boundary enforceable without waiting for a repository migration.
Concretely: create an `extras/` workspace whose members are the extras crates and directories;
remove them from the root workspace members in `Cargo.toml`; keep extras depending on core by path
while the coupling is still settling; split CI so a core job runs the core suite and an extras job runs the rest;
and keep one release tag for both until the extras move out.
A later change can then extract `extras/` into its own repository without touching core code.

The split must not confuse "moves" with "deletes".
Editor tooling, the REPL, the formatter, FFI, memory breadth, and evaluation data move
and their clients keep working from extras.
Manual-memory and scalar forms that only tests use, the monadic extension payloads,
and `docs/legacy` are proposed for deletion from the main line.
Packages, the restricted monadic translation, and reduced numerics stay in core; those are not extras.

## Design decisions for the artifact

The retired size analysis grouped the boundary-dependent choices into eight decisions.
This memo requests D1 and records the others as proposed resolutions for later approval;
each changes approved reference text or the artifact claim.

| # | Decision | Question | Proposed resolution |
| --- | --- | --- | --- |
| D1 | Artifact boundary | Which crates and directories are core, where do extras live, and what admission rule applies? | Options in [Proposed extras](#proposed-extras-and-their-destination); recommended staged split plus the [recurring pattern](#recurring-pattern) rule. This memo requests the owner's decision |
| D2 | IR boundary | Make the checked residual an explicit core CBPV IR, or interpret the statics arena directly? | Decide after D1. An explicit IR is the output of static elimination and monadic translation and the input of the interpreter and SPS lowering; it would turn CBPV-as-IR into a data type with its own checker and give typed SPS IRs closed types. Interpreting the statics residual directly is the cheaper alternative and keeps the arena as the IR |
| D3 | Typed SPS IRs | Carry the calculi's types through high SPS and SPSLow, turning the validators into IR type checkers, or keep the partial protocol check? | Prefer carrying the types once D2 settles; it makes the paper's judgments checkable and removes protocol extraction. Current partial validation is the fallback |
| D4 | Backend shape | Keep ZASM, or emit AMD64 from SPSLow with frame planning? | Keep ZASM conservatively: the paper's implementation claim includes it today, and the fixed-slot evidence is not sufficient to justify replacing frame planning |
| D5 | Surface area | Adopt the admission rule, unify synonym spellings, and decide `.zyi`, `@[doc]`, and text blocks | Adopt the rule; unify the synonym spellings, and remove `.zyi`, `@[doc]`, and `--\|` blocks after reworking the OOPSLA examples that carry them, with the language-reference changes approved together |
| D6 | Packages | Keep the package system and merge its parallel witness, telescope, and projection machinery? | Keep; merge the parallel machinery. Static elimination of the dependent module language is the clearest CBPV-as-IR demonstration and std needs it |
| D7 | Monadic translation | Keep the translation as the CBPV-to-CBPV demonstration and restrict it? | Keep as the translation demonstration, restricted to std's payload set; reject the test-only payloads with the existing expressivity error |
| D8 | Numerics | Keep every numeric width and scalar representation, or reduce? | Reduce to `Int`, `Float64`, `Char`, `String`, `Bool`, and `Unit`; this collapses the primitive and scalar tables, the ten numeric builtin packages, and the numeric copies in every host |

**Remaining independent decisions.** These are not part of the boundary,
but they gate the largest behavior-preserving savings and should be scheduled after D1.

| Area | Proposal | Reference |
| --- | --- | --- |
| Checker query retirement | Remove the per-judgment Salsa producers and allocate directly in the checker; deciding the derived-identifier registry and a sequential issuer is a separate follow-on after measurement. Current: `lang/statics/src/query` 2,307 lines, 45 `no_eq` producers, 152 `unreachable!` sites in `lang/statics/src` | [Checker query retirement](checker-query-retirement.md), [C3 query ownership](../references/compiler.md#query-and-checker-ownership) |
| Reconstruction frames vs recursion | Replace explicit reconstruction frames with direct recursion that grows the stack, protecting every recursive consumer; current: 126 `Work` variants, no `stacker::maybe_grow` in `lang/`, `RUST_MIN_STACK` 16 MiB | [C8 resumable execution](../references/compiler.md#resumable-folder-execution), [traversals](traversals.md#further-resumable-folder-migrations) |
| One host ABI table | Generate role signatures and each consumer's stubs from one table; give arithmetic an intrinsic body first so hosts stop sharing its copy. Current consumer modules total ≈4.3k lines | [C14](../references/compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls) |
| Test-harness consolidation | One harness per crate, with file-based cases only under the directive-driven fixture tree; current: 74 integration-test targets and thirteen registration mechanisms | [C16 fixtures](../references/compiler.md#source-fixtures-and-runtime-oracles), [CONTRIBUTING run tests](../../CONTRIBUTING.md#run-tests) |

Adopting or changing any of these requires owner approval
under [AGENTS.md](../../AGENTS.md#documentation-structure-and-design-records),
because each edits approved reference text.
The line estimates in the evidence sections overlap and are not additive; they are proposal material, not a budget.

## Suggested order

The boundary decision comes first because it determines what every later removal must preserve.
The sequence below follows the retired studies' agreement and keeps behavior-preserving work ahead of design changes.

1. **Decide D1 and the admission rule.** Approve or redirect the core and extras split
   and adopt the mainline convention.
   This memo is that decision; no code changes in the same step.
2. **Apply deletions without design change.** Remove raw memory kernels, dead high-SPS/SPSLow site maps,
   and leftover scaffolding; pair each removal with the reference amendments that own the affected account.
   This is the cheapest class and warms up the review path.
3. **Allocate directly in the checker.** Remove the unconsumed producer queries
   per the [checker proposal](checker-query-retirement.md); decide the derived-identifier registry
   and a sequential issuer separately, after measurement.
   Source behavior stays the same, but the query-ownership reference text changes.
4. **Replace explicit frames** with recursion plus stack growth (re-estimate first),
   then build **one host ABI table**, then **consolidate tooling and tests** once the boundary says
   which tooling is extra and which harness the core keeps.
5. **Decide and implement the IR-boundary-dependent work last.** D2 and D3, then D4 backend shape,
   D5 surface area, D6 package machinery merge, D7 monadic restriction, and D8 numerics.

Two cautions govern the order. First, the approximate line savings overlap; they are review estimates,
not measured deletions, and they must not be summed into a projected end state.
Second, a step that touches `lang/stackir`, ZASM,
or the native backend must keep the SPS paper's implementation claim true: lowering to SPS, β-simplification,
closure conversion with existential packages, and lowering to executable code.
A cleanup that makes the artifact smaller by weakening that claim is out of scope.

## Consequences for references and tests

The boundary changes the scope statements and the test matrix, not the language semantics.
Every affected rule keeps one canonical home; extras material moves or is marked as optional rather than being restated.

| Reference area | Consequence | Required action |
| --- | --- | --- |
| L11 relative monads | Translation stays, restricted scope; test-only payloads become rejections | Edit coverage and closed-boundary text with the D7 change ([L11](../references/language.md#11-relative-monads)) |
| L12 sources, imports, and entry | Packages stay in core; editor overlays and discovery tooling become extras | Keep package semantics canonical; mark editor incrementality as extras ([L12](../references/language.md#12-sources-imports-and-entry)) |
| L13 primitive values and capabilities | Numerics reduce; manual memory moves; bytes and I/O remain | Edit the primitive and manual-memory scopes with D8 and the memory decision ([L13](../references/language.md#13-primitive-values-and-capabilities)) |
| L14 foreign interfaces | FFI and native units move behind an extras feature | Mark the profile as optional and keep the contract canonical ([L14](../references/language.md#14-foreign-interfaces)) |
| L15 execution profiles | Core keeps interpreter, AMD64, and `wasm-sps`; tooling profiles leave | Update the profile table and backend selection policy ([L15](../references/language.md#selecting-an-execution-backend)) |
| C3 source loading and sessions | Reduced core session; overlays, completion, and editor queries move | Split or mark the editor-facing subsections ([C3](../references/compiler.md#c3-source-loading-sessions-queries-and-memory-retention)) |
| C10–C14 backend chapters | Core keeps the compiled path; ZASM, native, Wasm, FFI, and host text adjust with D3, D4, and the extras | Keep the paper's claim true; update current-implementation accounts only ([C10–C14](../references/compiler.md#c10-zasm-stack-analysis-and-local-representation-choices)) |
| C15 tooling | Cajun, TUI, formatter fidelity, and REPL move to extras | Mark them as extras and move presentation detail where it belongs ([C15](../references/compiler.md#c15-diagnostics-formatting-documentation-and-interactive-tooling)) |
| C16 validation | One harness per crate; core evidence stays | Record the core/extras test split and fixture ownership ([C16](../references/compiler.md#c16-validation-debugging-and-extending-the-implementation)) |
| [DESIGN.md](../../DESIGN.md#repository-layout) and the documentation index | Repository layout and component map change; the duplicate account shrinks | Update the layout table and remove restated rules per the [reference-drift todo](../todos/reference-drift.md#duplicate-accounts-and-scratch-records) |

The test and release consequences are equally concrete.
The interpreter stays the oracle ([C7](../references/compiler.md#c7-linking-and-the-reference-interpreter));
the OOPSLA artifact keeps its `zydeco check`/`zydeco run` commands and docker workflow;
the SPS compiled path keeps its `zir`, `zasm`, `asm`, `exe`, and `wasm-sps` evidence and its validators;
and the CLI entry points named in the core disposition stay working.
Core CI must run without extras: format, lint, the core test suite, doctests,
the OOPSLA examples, and the interpreter/native/Wasm agreement checks.
The tree-sitter and completions checks move with their clients or stay only while those clients are still maintained.
Release tagging, the [`Dockerfile`](../../Dockerfile), and the release notes describe the core artifact;
extras releases, if any, are separate.
Moving a crate must change the workspace members in [`Cargo.toml`](../../Cargo.toml), path dependencies,
CI job paths, and links in the same staged change; no moved crate may be left behind in the core workspace.

## Validation

The boundary decision is validated by demonstrating that the core can stand alone, not by a line count.
A candidate staged split is acceptable when all of the following hold.

1. The core workspace builds and tests alone: its members exclude every extras crate,
   and no core `Cargo.toml` depends on an extras path.
2. The artifact commands still work from the repository root:
   `cargo run --quiet --bin zydeco -- check lib/std/builtin.zy`,
   `cargo run --quiet --bin zydeco -- run lib/tests/oopsla/polynomial.zydeco`,
   `cargo run --quiet --bin zydeco -- check lib/tests/oopsla/exnt.zydeco`, and the SPS targets
   on `lib/tests/core` (`build -t zir`, `-t zasm`, `-t asm`, `-t exe`, `-t wasm-sps`).
3. The interpreter remains the oracle: the end-to-end fixtures agree across interpreter,
   AMD64, and `wasm-sps` wherever the backend supports them, and the compiling-path checks still pass.
4. Source behavior is unchanged: the core case fixtures and diagnostic expectations produce the same outcomes,
   and no source-language rule moved.
5. References and links are consistent: every changed scope has its owning reference updated,
   the [rule-ownership map](../references/README.md#rule-ownership) and `DESIGN.md` layout table reflect the split,
   the memo's anchors resolve from `docs/proposals/`, and no link points at a retired study.
6. The change is staged and reviewable: a mechanical move commit, a CI and manifest adjustment,
   separate behavior-preserving deletions, and no mixed boundary-plus-design rewrite.
   The split changes no source behavior, so it can land before the design decisions.
7. Formatting remains stable: `cargo fmt -- --check`, `cargo clippy-all -- -D warnings`,
   and the Markdown reflow check pass on the changed documents.

The campaign's own validation ladder stays as the repository defines it:
focused crate tests and fixture checks while iterating, `cargo test-resume` for the full suite,
and `cargo test-all --release` plus `cargo test-doc-all --release` only
for a finished change ([CONTRIBUTING run tests](../../CONTRIBUTING.md#run-tests)).
Native tests need AMD64 support
and Wasm tests need Node.js 24 ([CONTRIBUTING build the CLI](../../CONTRIBUTING.md#build-the-cli)).

## Open questions

1. Does the reduced session remain one crate, or does the core keep a loading-and-analysis crate
   while overlays, completion, and editor queries move to extras?
   The boundary table proposes the second; the split point needs a dependency check.
2. What is the minimal canonical source printer the core needs for diagnostics and tests,
   as distinct from the fidelity formatter?
   If the core needs none, the whole formatter becomes extras; if it needs some, the boundary must name
   which printing contracts stay ([C15](../references/compiler.md#formatting-and-typed-rendering)).
3. Which fixture sets count as maintained clients?
   The proposal keeps OOPSLA examples and SPS compiled-path fixtures; do demos
   and whole-program standard-library tests also admit features, or do they follow the core's existing evidence?
4. Does the staged split reside under one workspace with feature-gated members,
   or two workspaces with separate lockfiles and target directories?
   The former is simpler for reviewers; the latter is closer to the final repository shape.
5. How is test coverage preserved across the split without duplicating `lang/tests` and its roughly 1.6 GB debug build?
   Shared templates and fixtures may need a neutral location.
6. Which numeric widths must the host ABI and std keep for the paper evidence after D8?
   If any fixture relies on a reduced-away width, the evidence set changes with the decision.
7. Where does evaluation data live after the retired studies are removed: release assets or a separate data repository,
   and what retention policy applies to future dated studies?
8. Is keeping ZASM (D4) required for the implementation claim as the owner reads it,
   or may direct AMD64 emission from SPSLow replace it once the fixed-slot evidence is stronger?

## Evidence from the retired studies

These paragraphs distill the **retired 2026-10-21 cleanup audit** (measured at `7e5ee4c1`)
and the **retired 2026-10-25 size analysis** (measured at `6aae43d0`).
Both are proposals, not decisions; this memo supersedes their reports, so nothing below links to them.
Figures were re-checked at `e2dd8979` where noted and are dated snapshots;
`≈` values are review estimates that overlap and are not additive.
Each subsection states the current status of its finding.

### Systems-programming thread

Manual memory, compiled C libraries with native units,
and fixed-width numerics form one research thread outside both yardsticks; the audit proposed moving it
to an experiment branch and keeping only a raw `Addr` adapter for bytes and I/O.
At the measured revision 1,883 of the 2,475 `lib/std/memory` lines were imported only by tests,
documentation, and evaluations, while `std.zy` reached 592 lines through `memory/buffer.zy`
and `text/bytes.zy`; `lib/std/memory` is 2,435 lines today.
The thread also reaches the calculi's implementation: `AddrOffset` and `MemoryStep` belong to the syntax that high SPS
and SPSLow share, and neither SPS nor `SPS_l` has a counterpart, so moving the thread also removes forms from both IRs.
The current FFI and artifact clients cost about 2,200 lines
in `cli/src/{library,unit,native}.rs` plus the 1,264-line native stub, and twelve numeric builtin packages remain
under `lib/std/builtin/numeric/`, and the retired audit found ten of them identical up to the type name.

Status: decision needed — the destination is extras or an experiment branch;
C imports could move behind a cargo feature.

### Package hierarchy

Source packages add opaque roots, unnamed packages, route-dependent copies merged by structure,
`discover` globs, and `test(of(...))` relationships.
Relationships have no language operation as a client: `zydeco show` prints them, Cajun's document links surface them,
and they enter the structural merge key, so removing them also reworks those tools and changes copy identity.
Root substitution is constructed only in session tests,
and the discovery walker is 127 lines serving two `workspace.zy` files;
the session package modules measure 1,065 lines plus 570 in dedicated tests.
The decision is therefore not whether to keep packages — [D6](#design-decisions-for-the-artifact) keeps them —
but how much editor and discovery tooling follows them into extras and which parallel witness, telescope,
and projection machinery is merged first.

Status: decision needed — keep package semantics in core, merge the parallel machinery,
and move editor-facing discovery tooling with its clients.

### Monadic-translation extensions

The translator handles more payloads than the standard library uses: witness-dependent arrows,
polymorphic value functions, applicative nominal families, and value roots have only test clients,
together about 1.5k lines and 45 fixtures at the measured revision.
Those payloads share nominal-translation machinery with the computation translation std uses,
so the saving overlaps code that stays.
Rejecting the test-only payloads with the existing `tyck.expressivity` error preserves
the [closed translation boundary](../references/language.md#closed-translation-boundary)
and keeps external values as explicit parameters.
Current evidence remains the standard-library effect modules and the OOPSLA artifact, which imports only `monad.zy`.

Status: decision needed — D7 proposes keeping the translation restricted to std's payload set.

### Surface forms

The retired audit counted forms whose only clients are compiler cases or tests, with comments removed:
`define` 0 uses, `comatch p => M end` 0, `sigma` 4 (all compiler cases), `<|` 7 uses in 6 files,
`~>` views 16 uses in 6 files, `param ... that` 45 uses in 30 files, `.zyi` companions 0 files,
`@[doc]` 0 uses, `@(literal)` 1 use, and `--|` text blocks 53 uses.
Re-running the retired count on a clean `e2dd8979` archive gives the same counts (629 sources:
601 `.zy`, 0 `.zyi`, 28 `.zydeco`).
Some spellings are also synonyms: `forall` and `pi` desugar to one quantifier, `param p in` equals `fn p =>`,
and `def` on a value behaves as `let`; unifying them is a mechanical migration including the OOPSLA examples.
Removing `~>` views costs about 130 match arms and references in nine crates,
and removing `<|` costs a precedence level and a formatter case.

Status: decision needed — D5 adopts the admission rule and decides which forms are removed or unified,
with the language-reference edits approved together.

### Effect library breadth

No non-test module outside `lib/std/effect` imports the effect library, and the OOPSLA artifact imports only `monad.zy`.
A minimal demonstration keeps the monad interface, transformer derivation, and the State,
Exception, and NonDet effects, the last being the only effect whose carrier needs computation-type algebras;
Reader, Writer, Option, and StateExn could become examples.
Two fixes need no design change: `lib/std/effect/ret.zy:4` takes the Builtin package without using it,
which forces effect modules to take the package and their clients to apply it,
and `triv : Thk Top` is defined in seven effect modules (re-verified: seven).
The library is 1,027 lines today; seven OOPSLA example files import `effect/monad`
and `lib/tests/monadic` holds the other visible clients.
About 880 lines of library contracts in the language reference belong in a library reference.

Status: decision needed — keep a minimal demonstration and move or drop the breadth.

### Backend translation validators

`SpsLowProgram::try_new` runs the partial source-protocol and word-entry-contract validators on every compilation;
no backend reads their results, and they re-derive types after erasure to validate the compiler's own passes.
At the measured revision the validators were about 2,050 lines with 1,830 lines of dedicated tests.
Re-verified today: `lang/stackir/src/low/check.rs` is 763 lines, `lang/stackir/src/high/check.rs` is 562,
and protocol extraction is 1,214 lines in `lang/stackir/src/protocol.rs`, `protocol/source.rs`,
and `protocol/agreement.rs` (1,587 with the test modules beside them).
Only the `stack_protocols` integration test and the ZIR printer read the extracted tables;
high lowering also takes its canonical destructor tags from the protocol graph.

Status: decision needed — D3 chooses between keeping the partial check, carrying the calculi's types through both IRs,
or deleting the check.

### Test-only generality

Several production interfaces carry parameters that only tests vary.
Raw memory kernels, a fused IR that applies only to a 64-bit load, at most 32 operations,
and a store of the same type cost about 650–900 lines; the live editor configuration
in Cajun serves two hover settings and is 268 lines today (production 1–186);
and `--target-arch` accepts `Aarch64` but rejects it at build entry, leaving one supported value.
HEAD `e2dd8979` moved the architecture rejection before compilation and documented the AMD64 requirement,
but the option remains documented in the README and CLI guide as the only way for an ARM host to request AMD64.
The kernels are delete-only; the editor configuration and the architecture flag need boundary and CLI-docs decisions.

Status: open — the kernels can be removed with reference-text amendments; the other two wait on their client decisions.

### Parallel implementations

Each of these mechanisms exists more than once.
The walk over existential telescopes is written at least seven times, and field projection searches
through two near-identical candidate functions (≈400 lines); `PackPi` and `ValPi` bind witnesses
through separate machinery (≈500 lines), whose formation/application asymmetry is the tracked
[nested-witness discrepancy](../todos/compiler-boundaries.md#nested-witness-diagnostic);
builtins are implemented in the interpreter, the native stub, and the Node host beside a separate role table;
the source scanner and the desugarer both validate builtin and intrinsic annotations (≈420 lines);
the CLI and TUI repeat diagnostic dispatch, and the TUI strips ANSI escapes with a hand-written state machine;
and the arena traits carry about 54 source-level marker and accessor implementations (≈600 lines).
Re-verified builtin consumer sizes are `lang/statics/src/builtin.rs` 818,
the `lang/dynamics` implementations 774 plus 214, `lang/stackir/src/builtin.rs` 259,
`lang/wasm-common/src/host.rs` 176, `cli/wasm/wasm-host.mjs` 829, and `runtime/stub.rs` 1,264 lines, about 4.3k in all.
A grep counts 43 `ArenaSchema` and 38 `Allocates` implementations today.

Status: open — merge candidates with no reference change except the nested-witness todo.

### Tooling, tests, and documentation

The workspace builds 74 integration-test binaries today, `lang/tests` alone needs about 1.6 GB per debug build
at the measured revision, and source tests can be written six ways; counting every registration mechanism,
from directive fixtures and Rust macros over library sources to CLI process tests, gives thirteen.
The tooling crates held 15,433 production and 14,935 test lines at the measured revision
and hold 15,351 and 15,000 today.
The formatter had about 2,900 implementation and 2,850 test lines at the measured revision;
`lang/surface/src/textual/pretty.rs` alone is 5,489 lines now.
On documentation, the retired audit estimated that about 860 of DESIGN's 1,014 lines restated the references
and had drifted; DESIGN is 995 lines today and the reference-drift todo is still open,
while `docs/legacy` holds 2,013 lines that use removed syntax.

Status: decision needed — D1 moves the tooling surfaces to extras and keeps one harness;
the documentation duplication is an open repair tracked by the todo.

### Retained after review

The reviews judged these parts essential or proportionate: the reference interpreter, the oracle for every backend;
value functions, static elimination, and value-function witness routes that std's module factories use;
recursive field search and `;` pattern groups, which every std import uses, although their internals duplicate;
`begin ... end` blocks with `that`, the only way to write recursive sealed types; `pack`,
`|>`, field puns, and `@(m)`; high-SPS normalization, closure conversion, ZASM lowering,
and frame planning for AMD64; the copying collector and its block-start index; `@[typeof]`,
which supports the prelude bootstrap; and derived identifiers, which keep Salsa replay stable.
The SPS half of the yardstick adds high SPS and SPSLow with the lowering,
closure conversion, and package forms that the paper claims.
These stay in the core; the list is not re-litigated by this memo.

Status: resolved — reviewed and retained; do not remove without new evidence.

### Representation multiplication

The pipeline passes through nine syntax families — textual, bitter, scoped, statics, dynamics, high SPS,
SPSLow, ZASM, and AMD64 — and most carry their own identifier, arena, printer, span, and traversal modules.
At the measured revision the support matrix counted 11 `syntax.rs` files with 3,084 lines,
9 `arena.rs` files with 4,480, 11 printer files with 9,473, 5 `span.rs` files with 625,
5 `err.rs` files with 870, 4 `traverse.rs` files with 1,053, 3 `alloc.rs` files with 647,
and 4 `variables.rs`/`check.rs` files with 1,135, plus 79 `ArenaSchema` and `Allocates` implementations;
the review estimated that a new phase costs 1.5k–3k support lines before any semantics.
Two families duplicate their neighbors closely: the desugared bitter term enum has 42 variants
against the surface's 38, and scoped syntax reuses the bitter terms.
Since then, commits `80f63893`, `e9e3a970`, and `5bd19491` shared the stack IR syntax,
arena, traversal, site lookups, and printing across phases (e.g.
`lang/stackir/src/syntax.rs` 351, `arena.rs` 242, `traverse.rs` 306, `fmt.rs` 294 lines),
but the high and low phase files and their printers remain.

Status: decision needed — D2 and D3 determine whether the erased type families shrink further;
the shared module commits are a partial resolution.

### Types erased and then re-derived

High SPS and SPSLow erase the source types, after which protocol extraction,
the SPSLow protocol and entry-contract checks, and the two structural checks rebuild partial protocols
to validate the compiler's own passes.
With the typed-arena lint, the second type systems totaled 4,809 production lines at the measured revision; the lint
and its re-derivation, about 2.5k of them, were removed in `5e3332d4` and replaced by a test-only invariant check.
What remains is the extraction (1,214 lines) and the two validators (1,325 lines),
whose only test reader is `lang/tests/tests/stack_protocols.rs`, plus protocol-derived destructor tags in high lowering.
Carrying the calculi's types through both IRs would make the checks an IR type checker and remove the extraction;
Zydeco's polymorphism, packed values, and recursive codata would need types beyond the paper's.

Status: decision needed — D3.

### Explicit reconstruction frames

High lowering, normalization, and closure conversion are written as explicit reconstruction frames
for the resumable folder driver so that deeply nested residual programs cannot exhaust the native stack.
The three folders declare 126 `Work` variants today — 41 in `lang/stackir/src/high/lower/fold.rs:71`,
41 in `lang/stackir/src/high/normalize/fold.rs:48`, and 44 in `lang/stackir/src/low/convert/fold.rs:15` —
against 97 at the measured revision, and the 2026-09-14 migration added 1,606 production and 2,176 test lines net.
The workspace raises `RUST_MIN_STACK` to 16 MiB in `.cargo/config.toml:5` for residual depth; the printers, the protocol
and contract checks, and protocol extraction still recurse, and no `stacker::maybe_grow` call exists in `lang/`.
Plain recursion that calls `maybe_grow` in each recursive function,
as rustc's stack growth does, would protect every consumer at a fraction of the support code;
the retired estimate was ≈1.5k production plus ≈1.2k test lines.

Status: open — a code-only alternative, but the reference account needs a note before the migration
and the estimate should be re-measured before planning.

### Formatter fidelity

The formatter preserves comments, blank lines, and author layout intentions, and its production code
with the source-fidelity support it needs was 4,587 lines at the measured revision.
The parser serves it on every successful parse: the grammar calls `capture_source_information`
at each public entry point, recording arm prefixes, existential prefixes,
metadata prefixes, line extents, and comment anchors.
A canonical printer without retention would be a fraction of the size and the parser would keep only spans;
`lang/surface/src/textual/pretty.rs` alone is 5,489 lines today.
The CLI and Cajun share that implementation, so the boundary question is whether canonical source output is core tooling
or an extra; current formatting behavior is owned by [C15](../references/compiler.md#formatting-and-typed-rendering).

Status: decision needed — the boundary decides whether fidelity joins the formatter in extras;
a minimal canonical output, if the core needs one, stays.

### Builtins written once per consumer

The host boundary is written once per consumer: the signature plan in `lang/statics/src/builtin.rs`,
the interpreter's dispatch and operation bodies in `lang/dynamics`, host modules, the stack IR table,
the native stub in `runtime/stub.rs`, and the Wasm host split between `lang/wasm-common`
and `cli/wasm/wasm-host.mjs`, with a separate role catalog in `lang/syntax/src/lib.rs`.
The measured total was 4,503 lines, and the re-verified module sizes sum to ≈4.3k today.
Arithmetic reaches the hosts because it is the one primitive family without an intrinsic body:
`ExternalFunction::make_function` gives comparisons, address offsets, and memory steps intrinsic bodies,
while arithmetic becomes a primitive only during normalization, so the native stub and the Node host must implement it.
One host ABI table that generates the role signatures and each consumer's stubs would replace most of this;
giving arithmetic an intrinsic body first removes the hosts' copies.

Status: decision needed — the remaining independent decision above; it depends on C14 and the retained instruction
that intrinsics own their bodies.

### Tooling at product standard

The session, CLI, Cajun, TUI, and xtask hold 15,351 production and 15,000 test lines today,
and they provide overlays, package discovery and relationships, completion, hover,
rename, a REPL, C library and native-unit linking, and a resumable test runner.
Beyond the CLI's `check`, `run`, and `build` paths, none of this serves the three demonstrations,
and each piece is built to full generality with recovery, configuration snapshots, and its own diagnostics path.
The workspace defines 74 integration-test targets and the thirteen registration mechanisms above.
The audit's judgment is that this layer is proportionate only if a maintained tool path needs it,
which is exactly what the extras boundary decides.

Status: decision needed — D1; the recommendation is extras, with the reduced CLI keeping the artifact commands.

### Accretion

The heavy subsystems arrived in quick succession, each with its own support matrix and tests,
and none displaced an earlier mechanism.
At the measured revision the heavy paths, their first-commit dates, and the commits touching them were:

| Subsystem | First commit | Commits since |
| --- | --- | ---: |
| `lang/session/src/source/query.rs` | 2026-08-05 | 62 |
| `lang/surface/src/textual/pretty.rs` | 2026-08-08 | 70 |
| typed-arena lint | 2026-09-02 | 7 |
| `lang/statics/src/query` | 2026-09-07 | 11 |
| `lib/std/memory` | 2026-09-08 | 22 |
| `lang/machine/src/frames.rs` | 2026-09-08 | 4 |
| `lang/stackir/src/protocol.rs` | 2026-09-10 | 6 |
| `lang/session/src/source/package` | 2026-09-12 | 12 |
| `lang/stackir/src/passes.rs` | 2026-09-12 | 3 |
| `cli/src/library.rs` | 2026-09-13 | 7 |
| `lang/utils/src/fold.rs` | 2026-09-14 | 2 |
| `xtask` | 2026-10-20 | 8 |

Tracked Rust grew from 32.8k lines on 2026-07-01 to 80.8k on 2026-09-01 and 138.4k
on 2026-09-15; the measured revision had 140.4k, and the 69 restructuring commits
since 2026-07-15 rearranged 36.2k added against 34.5k deleted for a net gain of 1.7k.
The review's probable root cause was a research core accumulating orthogonal product features
at production generality without a client beyond its tests.

Status: open — the admission rule is the countermeasure; no mechanism has been displaced yet.

### Deletions without design change

This class keeps the language and pipeline and removes code that no client reaches.
At the measured revision the raw memory kernels remained at about 650–900 production lines plus their tests,
and removing them changes compiler-reference text but no source behavior; dead high-SPS
and SPSLow site maps (`pats` and `terms`) remained whose only readers copy them into the next phase's arena.
The current shared stack IR arena still stores those site maps: `lang/stackir/src/arena.rs:43-46` declares them
with phase aliases in the high and low arena modules, and the readers only propagate them.
The retired review judged this class the cheapest warm-up and put it immediately after the boundary decision.

Status: open — available now; pair each deletion with the reference amendments that describe the phase's data.

### Structural simplifications

The size analysis listed ten directions that keep the language and pipeline and change only how it is written:
allocate directly in the checker and remove the per-judgment producers and derived-ID registry (≈5k–6k); closed types,
removing per-node environment tables (≈0.8k–1.2k); interpret the statics residual and delete the dynamics syntax,
linker, and printer (≈0.85k); two front-end representations instead of three (≈2k); decode typed annotations once
during desugaring (≈0.9k–1.2k); plain arenas without scope schemas and access traits; replace explicit frames
with recursion and stack growth (≈1.5k plus ≈1.2k test); one host ABI table (≈2k); and one test harness per crate.
The dependent decisions and current status of each are in [Design decisions](#design-decisions-for-the-artifact)
and its remaining-independent table.
The estimates overlap and are not additive, and several change approved reference text.

Status: decision needed — the class is scheduled after the boundary; the checker item has its own parallel proposal.

### Repository weight outside Rust

The retired size analysis counted 90,248 tracked lines under `docs/evaluations`, most of them JSON
and CSV measurement data, and proposed keeping the data in a release asset rather than the artifact tree.
The figure is now 31,647 tracked lines because commits `9bebc585` and `118b8ab1`
on 2026-10-28 retired four study directories, and the retirement removes the two studies this memo distills.
The Markdown documentation measured 16,326 lines at the baseline, of which the study manual
that this memo serves is temporary and may be deleted; the clean `e2dd8979` tree has 16,308 lines,
and the tree held 19,524 lines after the manual and the committed proposals,
before this memo and its sibling were added.
`DESIGN.md` repeats reference accounts,
tracked by the [reference-drift todo](../todos/reference-drift.md#duplicate-accounts-and-scratch-records),
and `docs/legacy` holds 2,013 lines of removed-syntax notes.

Status: open — evaluation data is partly resolved and the two retired studies leave with their retirement;
the legacy notes and the DESIGN duplication remain.

### Defects found during the audit

The audit found three defects. Every `build -t exe` computes the runtime digest
and target platform twice to validate C-library and native-unit imports,
even when the program has none; the current `cli/src/main.rs:496-514` still calls `LibraryDigest::runtime`
and `LibraryPlatform::for_target` twice before any import list is checked.
`env!("CARGO_MANIFEST_DIR")` paths in the TUI engine and the session's prelude lookup tie an installed binary
to the checkout it was built from; the production uses are `tui/src/engine.rs:44`
and `lang/session/src/source/prelude.rs:13`, and the bundled prelude fallback covers five bootstrap files
but not `builtin.zy`, which the TUI imports.
The tracked n-ary product inference discrepancy comes from a two-component refinement in the statics normalizer
and is recorded in the [compiler-boundaries todo](../todos/compiler-boundaries.md#n-ary-product-inference-refinement).
Both CLI defects now have a bounded repair record in the [tooling-boundaries todo](../todos/tooling-boundaries.md),
which separates the installed-binary question into an installed-data-directory or embedded-standard-library repair;
the product-inference defect remains tracked in the compiler-boundaries todo.

Status: open — all three defects remain in the code; the two CLI defects now have a todo entry,
so repair can proceed without a separate recording decision.

### Post-audit verification

A verification pass after the audit re-derived the reproducible figures from a clean extraction
and checked review-derived claims against the code without a full rebuild.
It corrected several statements: `--target-os` has two implemented values,
so only `--target-arch` remains a candidate interface with one supported value; production defaults
to the `Local` representation policy while `build --representation` or the tests select the others; ten, not twelve,
numeric builtin packages are identical up to the type name; the `<|` and `~>` rows mixed use and file counts;
the `library(zydeco)` sentence distinguishes checked-in sources from test-generated ones;
the `builtin |> ...` count is standard-library-wide; the formatter and Cajun estimates are scope-dependent
(about 2,900 and 270 lines); the arena count is 54 source-level implementations;
validator savings distinguish deleting the protocol extraction from confining validation to tests;
and the package and monadic findings note their tool consumers, merge-key effect, and shared nominal machinery.
Two review counts did not reproduce exactly: about 1,250 dedicated memory compiler lines
and 36 memory-IR files remain estimates, bracketed at roughly 1.0k–1.3k lines
and 27–41 files depending on module-set definition.
These corrections are current; the findings they qualify remain proposals.

Status: resolved — verification corrections recorded; individual findings are handled in their own sections.

### Limits

The retired audit's limits bound its evidence: `≈` deltas are review estimates
that overlap and are not measured deletions; usage counts match regular expressions
against comment-stripped sources, so they count syntax rather than semantic dependence;
figures outside the reproduction came from seven review notes that were not retained,
so they rest on the reviews and the verification pass; and the reviews applied only the OOPSLA half
of the yardstick, with the SPS half applied afterward from the paper and the code.
The retired size analysis adds classification limits: test-code classification depends
on path components, file names, and `#[cfg(test)]` placement, so inline helpers
and trailing production items can be misclassified; thread attribution is by path prefix
and counts whole files once, and the session query module serves both editor tooling and the CLI; and figures
that neither the script nor the listed commands reproduce come from review passes and were checked selectively.
For this memo, the retired measurement script prints the crate table on a clean `e2dd8979` archive
and then stops in its support-module section because `lang/surface/src/textual/ugly.rs` was removed;
the Markdown figure includes 18 lines of proposal edits that were uncommitted when the manual measured it,
and the two retired studies are dated snapshots that must be re-measured before an individual removal is planned.

Status: resolved — method constraints recorded; treat every figure here as dated and re-measure before citing it
against a later revision.
