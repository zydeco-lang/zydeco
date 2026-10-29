# Size analysis, 2026-10-25

On 2026-10-25 a read-only review asked why the implementation is as large as it is,
and which cleanup and design directions would turn the repository into an artifact for CBPV compilation,
CBPV as a compiler IR, and the stack-passing-style calculi.
The target was `6aae43d0` on the `prelude` branch with a clean working tree.
The review read every phase's syntax definitions and the largest modules of each crate,
and measured the tree with the [reproduction script](#reproduction).
Review passes over the code and history supplied the figures that neither the script nor the listed commands reproduce;
the [limits](#limits) list them.

The record extends the [cleanup audit](../2026-10-21-cleanup-audit/README.md),
which catalogued features whose only clients are their tests.
It asks the complementary question: where do the lines go that no feature removal reaches?
The answer is largely structural.
About a third of the production Rust belongs to threads outside the paper pipeline.
The other two thirds is the pipeline itself; most of its size follows from how the pipeline is built,
and a smaller part from the breadth of the source language that the yardstick retains.

The findings are proposals, not decisions.
Removing or changing an approved feature changes the references and requires approval
under the [documentation rules](../../../AGENTS.md#documentation-structure-and-design-records).
This record describes the measured revision and is not a backlog.

## Yardstick

The [audit's yardstick](../2026-10-21-cleanup-audit/README.md#yardstick) applies unchanged:
the OOPSLA artifact's relative-monad claims and the stack-passing-style paper's two calculi,
SPS realized by high SPS and `SPS_l` realized by SPSLow.
This review adds the artifact framing that motivated it.
The artifact should demonstrate three things:

- **CBPV compilation**: source CBPV checked, statically eliminated, and compiled to executable code.
- **CBPV as a compiler IR**: the checked residual program that the interpreter
  and the SPS lowering both consume, together with the transformations expressed on it,
  such as [static elimination](../../references/compiler.md#static-elimination)
  and the [monadic translation](../../references/compiler.md#monadic-and-copattern-elaboration).
- **SPS and `SPS_l`**: lowering, β-simplification, closure conversion with existential packages,
  and lowering to executable code, as the audit's SPS half states.

Everything else in the repository is judged by whether it serves one of those three demonstrations.

## Where the weight is

Line counts follow `wc -l`. A file counts as test code when a path component is `tests` or `examples`,
when its name ends in `tests.rs`, or when a `#[cfg(test)] mod name;` declaration names it or an ancestor module;
in any other file, the lines from the first `#[cfg(test)]` attribute on an inline module to the end count as test code.

| Crate | Production | Test |
| --- | ---: | ---: |
| `lang/statics` | 31,208 | 4,363 |
| `lang/surface` | 15,794 | 8,242 |
| `lang/stackir` | 10,420 | 5,403 |
| `lang/session` | 4,512 | 3,925 |
| `editor/cajun` | 4,126 | 3,909 |
| `cli` | 3,760 | 6,227 |
| `lang/assembly` | 3,179 | 1,666 |
| `lang/utils` | 3,095 | 1,057 |
| `lang/syntax` | 2,937 | 626 |
| `lang/dynamics` | 2,850 | 523 |
| `lang/amd64` | 2,180 | 0 |
| `xtask` | 1,898 | 463 |
| `runtime` | 1,740 | 447 |
| `lang/machine` | 1,289 | 449 |
| `lang/wasm-sps` | 1,144 | 75 |
| `tui` | 1,137 | 411 |
| `lang/wasm-am` | 937 | 0 |
| `lang/wasm-common` | 897 | 0 |
| `lang/derive` | 297 | 171 |
| `lang/tests` | 0 | 7,734 |
| Total | 93,400 | 45,691 |

Attributing production lines to research threads by path prefix gives the following split.
The prefixes are listed in the script; the attribution is approximate because some modules serve more than one thread,
and the session's query module also hosts the non-incremental entry points that the CLI uses.

| Thread | Production lines | Share |
| --- | ---: | ---: |
| Incremental session and editor tooling | 9,079 | 9.7% |
| Post-hoc validators | 4,809 | 5.1% |
| Formatter and source fidelity | 4,587 | 4.9% |
| Packages, discovery, and the metadata catalog | 4,084 | 4.4% |
| Monadic elaboration | 3,282 | 3.5% |
| C FFI, compiled libraries, and native units | 2,261 | 2.4% |
| Repository tooling | 1,898 | 2.0% |
| Manual memory and scalar kernels | 1,672 | 1.8% |
| Abstract-machine Wasm backend and ZASM interpreter | 1,292 | 1.4% |
| Pass framework and plans | 746 | 0.8% |
| Remaining pipeline and CLI | 59,690 | 63.9% |

The audit's removal candidates lie in the first ten rows, about 33.7k lines.
The last row is the front end, checker, static elimination, interpreter, high SPS, SPSLow, ZASM,
native preparation, the structured Wasm backend, and the CLI driver.

The last row also carries the language breadth that the yardstick retains.
A review pass attributed the checker's production code to features, counting dedicated files and the branches
of shared rules in code lines, which exclude blanks and line comments; the checker has 27.6k such lines.
It found about 2.8k lines for packed values, existential telescopes, and `PackPi`; 1.8k for builtins, intrinsics,
and foreign calls; 1.7k for value functions, static elimination, and views; 1.4k for named components and field search;
and 0.4k for copatterns, with about 1.4k more inside shared rules such as `lub`, folding, and printing.
The same pass estimated a checker for CBPV with data and codata, thunks, `Ret`,
polymorphism, and simple existentials at 2.5k–3.5k such lines.
The structural causes below explain most of the rest of that row.

## Structural causes

### Representation multiplication

The pipeline passes through nine syntax families: textual, bitter, scoped, statics,
dynamics, high SPS, SPSLow, ZASM, and AMD64.
Most families define their own identifiers and carry their own storage, printing,
provenance, error, or traversal modules; scoped syntax reuses the bitter identifiers,
and the interpreter's syntax uses reference-counted pointers instead.
The support matrix at the measured revision counts whole files, in-file tests included:

| Module kind | Files | Lines | Families served |
| --- | ---: | ---: | --- |
| `syntax.rs` | 11 | 3,084 | all nine, plus two shared modules |
| `arena.rs` | 9 | 4,480 | all except dynamics and AMD64, plus the generic utilities |
| printers (`fmt.rs`, `pretty.rs`, `ugly.rs`, `debug.rs`) | 11 | 9,473 | all except AMD64; textual twice, bitter and scoped through the shared `debug.rs` |
| `span.rs` | 5 | 625 | textual, bitter, scoped, plus two shared modules |
| `err.rs` | 5 | 870 | textual, bitter, scoped, session, utilities |
| `traverse.rs` | 4 | 1,053 | scoped, statics, high SPS, SPSLow |
| `alloc.rs` | 3 | 647 | bitter, scoped, statics |
| `variables.rs`, `check.rs` | 4 | 1,135 | high SPS and SPSLow |

The generic arena layer adds 79 `ArenaSchema` and `Allocates` implementations across 11 files.
The review's estimate is that a new phase costs 1.5k–3k lines of such support before any semantics,
which is consistent with the sizes of the smaller families.

Two families duplicate their neighbors more closely than the others.
The desugared representation is larger than the surface it desugars: the textual term enum has 38 variants
and the bitter term enum has 42, because desugaring introduces administrative forms for mobile bindings,
residuals, blocks, recursive groups, monadic blocks, and retained comatch clauses while keeping most binder sugar.
Scoped syntax then reuses the bitter terms with resolved references and adds context and block machinery.

| Syntax file | Variant counts |
| --- | --- |
| `lang/surface/src/textual/syntax.rs` | Pattern 11, Term 38 |
| `lang/surface/src/bitter/syntax.rs` | Pattern 11, Term 42 |
| `lang/statics/src/syntax.rs` | Kind 4, Type 22, ValuePattern 10, Value 15, Computation 13 |
| `lang/dynamics/src/syntax.rs` | ValuePattern 8, Value 12, Computation 13 |
| `lang/stackir/src/high/syntax.rs` | Value 9, Stack 4, Computation 12 |
| `lang/stackir/src/low/syntax.rs` | Value 10, Stack 4, Computation 13 |
| `lang/assembly/src/syntax.rs` | Terminator 6, Instruction 12 |

The interpreter's syntax mirrors the statics residual with reference-counted pointers
in place of arena IDs, and [`link.rs`](../../../lang/dynamics/src/link.rs), 367 lines,
copies one into the other before [`eval.rs`](../../../lang/dynamics/src/eval.rs) runs.
The evaluator binds environments to patterns and bodies; it could do so over statics computation IDs directly,
which would remove the dynamics syntax, linker, and printer.

High SPS and SPSLow realize the paper's two calculi and should remain distinct types,
but they share only their node structs.
Each declares its own identifiers, arena, traversal, free-variable analysis, printer, and structural checks.
Excluding blanks, comments, and lines of punctuation alone, 474 of the 992 SPSLow lines
in those six modules repeat a high SPS line verbatim, and the two traversals avoid recursion with different techniques:
a hand-written work list for high SPS and the resumable folder driver for SPSLow.

### Judgment queries duplicate every checker rule

Each CBPV typing rule exists twice.
The thunk rule in [`check/term/computation.rs`](../../../lang/statics/src/check/term/computation.rs) is about 50 lines;
roughly 20 of them intern the surface term, call the Salsa producer `thunk_judgment`,
unwrap its result with `unreachable!`, and write the outcome into five arena tables.
The producer in [`query/computation.rs`](../../../lang/statics/src/query/computation.rs), about 40 lines
with its outcome struct, re-matches the surface term, looks up the intrinsic singletons, derives two identifiers
from the term, its checking occurrence, and a slot, and returns the nodes for the checker to store.
Direct allocation expresses the same rule as two arena insertions.

The producers are marked `no_eq` and `unsafe(non_salsa_values)`, 57 of them in `lang`,
so their memos cannot survive an edit, and the root `check_source` query keeps a single memo (`lru = 1`),
so every edit rechecks the whole root.
The [retention idea](../../ideas/README.md#compiler-memory-retention) records about 30% end-to-end checking overhead
from the migration, and the layer provides no reuse to offset it.
The layer also motivates the derived-identifier registry and the statics identity module beside the allocator.
Replay stability and independent issuers need key-spaced identifiers,
twelve bytes each, and the sparse, paged, and indexed storage
that recovers their memory cost makes [`utils/arena.rs`](../../../lang/utils/src/arena.rs) 1,914 lines.
The [compiler reference](../../references/compiler.md#query-and-checker-ownership) owns the boundary;
the audit's [per-judgment finding](../2026-10-21-cleanup-audit/README.md#per-judgment-checker-queries)
estimated the removal at 2.3k–2.8k lines.
The layer was added on 2026-09-07 and has 11 commits, so it is shallow-rooted.

### Constructor, destructor, and allocation interfaces

The typed syntax has four construction and inspection interfaces for the same node shapes:
`Construct` in [`construct.rs`](../../../lang/statics/src/construct.rs), 1,178 lines;
`MonConstruct` in [`elaborate/monadic/construct.rs`](../../../lang/statics/src/elaborate/monadic/construct.rs),
1,193 lines, which re-implements the same impl family through a macro while threading a monadic environment
and a fallible result; `Alloc` in [`alloc.rs`](../../../lang/statics/src/alloc.rs),
528 lines; and the inspection helpers in [`destruct.rs`](../../../lang/statics/src/destruct.rs), 457 lines.
One builder parameterized by its context would serve all three constructors.
Seventeen of the twenty helper methods that `construct.rs` adds to `Tycker` have no callers.

### Error plumbing

Checker failures use two result types.
`ResultKont` and plain `Result` coexist, with 241 `ResultKont` mentions in `lang/statics`,
and 31 function names exist in both a `_k` form and a plain form.
The error entry's `blame` field forces 404 `Location::caller()` arguments across the checker, 257 of them on lines
of their own, and the audit's [dead-code finding](../2026-10-21-cleanup-audit/README.md#dead-code) records
that only an unused renderer reads it.
The diagnostic message arms exist twice, once for that renderer and once for the published diagnostics,
and `TyckDiagnosticCode` repeats the 48 variants of `TyckError`.

### Sort dispatchers

The statics identifiers come in seven sorts, which is the right CBPV structure: kinds,
kind patterns, type patterns, types, value patterns, values, and computations.
Seven dispatcher enums then reunite them, `PatId`, `TermId`, `AnnId`, `PatAnnId`, `TermAnnId`, `StaticPatId`,
and `StaticTermId`, with `ValBinder`, `ValArgument`, and `ValPiBinder` doing the same for value functions,
and every rule narrows the union-typed result it receives with `as_*` and `try_as_*` conversions.
`lang/statics` has 203 `unreachable!` sites.
Classified by the surrounding code, about 70 of the roughly 190 production sites unwrap query-producer results,
about 40 narrow sorts, and most of the rest assert shapes that the package, field-route,
monadic, or constructor code established earlier.
The sort separation is not the cost; the union-typed checking result is, and it follows from the surface.
One surface form such as `fn`, application, `let`, `pi`, or `match` can produce several sorts, so `check_let_k`
in [`check/term/computation.rs`](../../../lang/statics/src/check/term/computation.rs) handles the product
of its bound term's sort and its tail's sort.
A core whose sorts are visible in the syntax would let each rule return one sort.

### Types erased and then re-derived

High SPS and SPSLow erase the source types,
after which [protocol extraction](../../references/compiler.md#partial-source-protocols),
the SPSLow protocol and [entry contract](../../references/compiler.md#word-entry-contracts) checks,
and the two structural checks rebuild partial protocols to validate the compiler's own passes.
With the [typed-arena lint](../../references/compiler.md#typed-arena-lint),
which re-derives the checker's kinds and type equalities, the second type systems total 4,809 production lines,
of which the lint and its re-derivation are about 2.5k.
The audit's [validator finding](../2026-10-21-cleanup-audit/README.md#backend-translation-validators) frames
the SPSLow checks as a choice about how to check `SPS_l`; this record adds that carrying the calculi's types
through both IRs would make the checks an IR type checker and remove the extraction.
Only one test, [`stack_protocols.rs`](../../../lang/tests/tests/stack_protocols.rs),
reads the extracted protocol tables; no backend does.
High lowering takes canonical destructor tags from the protocol graph, which any canonical numbering could supply.

### Explicit reconstruction frames

High lowering, normalization, and closure conversion are written as explicit reconstruction frames
for the [resumable folder driver](../../references/compiler.md#resumable-folder-execution),
so that deeply nested residual programs cannot exhaust the native stack.
The three folders declare 97 `Work` variants.
On 2026-09-14, nine commits moved those passes and CPS assembly lowering onto the frames
and the shared driver, adding 1,606 production and 2,176 test lines net.
The guarantee covers the folders and the two structural checks, which run on the traversals.
The printers, the SPSLow protocol and contract checks, and protocol extraction still recurse, and in a debug build
of the measured revision, `zydeco build lib/tests/core/fact.zy -t zir --dump-passes` overflows the main thread's stack
while printing the unnormalized high SPS.
The workspace already raises `RUST_MIN_STACK` to 16 MiB in [`.cargo/config.toml`](../../../.cargo/config.toml)
for residual depth, so tests run on large threads while the CLI's main thread keeps the platform default;
the suite passes where the dump overflows.
Plain recursion that calls `stacker::maybe_grow` in each recursive function, as rustc's `ensure_sufficient_stack` does,
would protect every consumer with a fraction of the code.

### The Builtin interface in every residual

[`fact.zy`](../../../lib/tests/core/fact.zy) is a 23-line program.
At the measured revision, `zydeco build lib/tests/core/fact.zy -t zir` prints 231 lines of SPSLow,
161 of which declare the type parameters of the Builtin package that every executable root receives;
with `--sps-passes none`, the output is 6,372 lines.
The program builds the standard library over the runtime Builtin record, so its residual carries
that record's projections into SPS, and the normalizer's field demands remove most of them.
This volume is what the explicit frames guard against, and it is what a before-and-after figure
of a small program would show.

### Formatter fidelity

The formatter preserves comments, blank lines, and author layout intentions.
Its production code with the source-fidelity support it needs is 4,587 lines,
and [`pretty.rs`](../../../lang/surface/src/textual/pretty.rs) alone is 5,273 lines, 2,536 of them production.
The parser serves it on every successful parse: the grammar calls `capture_source_information`
at each public entry point, recording arm prefixes, existential prefixes,
metadata prefixes, line extents, and comment anchors.
A canonical printer without retention would be a fraction of this size, and the parser would keep only spans.

### Builtins written once per consumer

The host boundary is written once per consumer: the signature plan in `lang/statics/src/builtin.rs`,
the interpreter's dispatch, operation bodies in `lang/dynamics/src/impls.rs`, and host modules, the stack IR table,
the native stub in `runtime/stub.rs`, and the Wasm host split between `lang/wasm-common` and `cli/wasm/wasm-host.mjs`,
4,503 lines in all, beside the role catalog in `lang/syntax/src/lib.rs`.
The interpreter, the native stub, and the Node host each implement the operations; the other modules describe them.
Arithmetic reaches the hosts because it is the one primitive family without an intrinsic body:
[`ExternalFunction::make_function`](../../../lang/stackir/src/builtin.rs) gives comparisons, address offsets,
and memory steps intrinsic bodies, while arithmetic becomes a primitive only during normalization,
so with `--sps-passes none` the native stub and the Node host must implement it.
The audit's [systems-programming finding](../2026-10-21-cleanup-audit/README.md#systems-programming-thread) adds
that ten numeric builtin packages in the standard library are identical up to the type name.
One host ABI table that generates the role signatures and each consumer's stubs would replace most of this.

### Tooling at product standard

The session, CLI, Cajun, TUI, and xtask hold 15,433 production lines and 14,935 test lines.
They provide overlays, package discovery and relationships, completion, hover, rename,
a REPL, C library and native-unit linking, and a resumable test runner.
The workspace defines 72 integration-test targets.
The audit counted six ways to write a source test; counting every registration mechanism,
from directive fixtures and Rust macros over library sources to CLI process tests, gives thirteen.
Beyond the CLI's `check`, `run`, and `build` paths, none of this serves the three demonstrations,
and each piece is built to full generality with recovery, configuration snapshots, and its own diagnostics path.

### Accretion

The heavy subsystems arrived in quick succession, each with its own support matrix and tests,
and none displaced an earlier mechanism.
The table lists the first commit that added each path and the number of commits touching it since.

| Path | Added | Commits |
| --- | --- | ---: |
| `lang/session/src/source/query.rs` | 2026-08-05 | 62 |
| `lang/surface/src/textual/pretty.rs` | 2026-08-08 | 70 |
| `lang/wasm-am` | 2026-08-30 | 18 |
| `lang/statics/src/validate/lint.rs` | 2026-09-02 | 7 |
| `lang/statics/src/query` | 2026-09-07 | 11 |
| `lib/std/memory` | 2026-09-08 | 22 |
| `lang/machine/src/frames.rs` | 2026-09-08 | 4 |
| `lang/stackir/src/protocol.rs` | 2026-09-10 | 6 |
| `lang/session/src/source/package` | 2026-09-12 | 12 |
| `lang/stackir/src/passes.rs` | 2026-09-12 | 3 |
| `cli/src/library.rs` | 2026-09-13 | 7 |
| `lang/utils/src/fold.rs` | 2026-09-14 | 2 |
| `xtask` | 2026-10-20 | 8 |

The history shows the same pattern in aggregate.
Tracked Rust grew from 32.8k lines on 2026-07-01 to 46.5k on 2026-08-01, 80.8k on 2026-09-01,
and 138.4k on 2026-09-15; the measured revision has 140.4k.
The 541 commits since 2026-07-15 that touch Rust added 209.2k lines and deleted 101.6k.
The 69 restructuring (`sisy`) commits among them added 36.2k and deleted 34.5k, a net gain of 1.7k,
so restructuring rearranged mechanisms rather than retiring them.

This is the probable root cause: a research core accumulated orthogonal product features, incrementality,
recovery, fidelity, foreign interfaces, and manual memory, each implemented at production generality
without a client beyond its tests.

## Directions

Line deltas marked ≈ are review estimates inferred from file sizes; they overlap and are not measured deletions.

### Deletions without design change

These are the audit's first two steps, restated with the sizes at this revision:
the [dead code](../2026-10-21-cleanup-audit/README.md#dead-code), the ZASM interpreter
and `wasm-am`, the pass framework beyond one normalization switch and one dump option,
the typed-arena lint with a small test helper kept, the recursive folder driver, the representation-policy trait,
the compact and moving frame variants, the raw memory kernels, and the test-only parser entry points and ugly printer.
Together they are ≈8k–10k production lines plus their tests, and they change compiler reference text
but no source behavior.

This review adds dead code that the audit did not list: the 17 uncalled `Tycker` helpers in `construct.rs`;
`Instruction::Clear`, which emitters match but nothing constructs; the unused `GenBindNull` grammar rule;
uncalled arena methods such as `map_id`, `insert_or_same`, `rebind`, and `filter_map_value`;
the `pats` and `terms` site maps of high SPS and SPSLow, whose only readers copy them
into the next phase's arena; and the AMD64 emitter's three front-end arena references,
which nothing reads and which are the only reason `zydeco-amd64` depends on `zydeco-surface` and `zydeco-statics`.

### Structural simplifications

These keep the language and the pipeline and change only how the pipeline is written.

- **Allocate directly in the checker.** Remove the per-judgment Salsa producers and the derived-identifier registry;
  the checker uses a sequential issuer, and Salsa remains, if at all, at file granularity for Cajun.
  This removes the query modules, most of `utils/arena.rs`, and about 20 lines from each of roughly 60 rules (≈5k–6k).
- **One builder.** A single build trait parameterized by its context replaces `Construct`, `MonConstruct`,
  and the `Alloc` impl families (≈1.5k).
- **One error discipline.** Plain results carrying a diagnostic with the current task's span, without `blame`,
  the second renderer, or the `_k` duplicates.
- **Closed types.** Substitute aliases during elaboration and represent bound type variables
  without per-node environments, removing the six per-sort environment tables that `strip_checker_state` discards
  after checking and most of the 47 `subst_env` calls (≈0.8k–1.2k).
- **Interpret the statics residual.** Delete the dynamics syntax, linker, and printer
  and keep the evaluator over computation IDs and environments (≈0.85k).
  This competes with the explicit core IR under the [design decisions](#design-decisions-for-the-artifact).
- **Two front-end representations instead of three.** Desugaring eliminates sugar instead
  of introducing mobile and residual forms, and name resolution runs in the same pass, so one arena, span,
  error, and freshening set disappears (≈2k).
- **Typed annotations.** Decode each meta annotation once during desugaring into a typed node.
  Today `intrinsic` alone has six types across the metadata catalog, the source directives, and their analyzer,
  and `prelude` metadata is decoded at four sites: desugaring, block collection, resolution,
  and the session (≈0.9k–1.2k).
- **Plain arenas.** A typed arena without scope schemas or access traits removes the 79 impls and most
  of the generic utilities.
- **Recursion with stack growth.** Replace the explicit frames with direct recursion
  that calls `stacker::maybe_grow` in each recursive function, which also protects the printers,
  the protocol and contract checks, and protocol extraction (≈1.5k, plus ≈1.2k test lines).
- **Shared SPS infrastructure.** Declare the forms that high SPS and SPSLow share once,
  with an extension variant for each calculus, so traversal, free variables, printing,
  and the structural checks are written once and closure conversion keeps only its translation cases (≈0.6k–0.8k).
- **One host ABI table** generating role signatures and stubs for the interpreter, native runtime, and Wasm host (≈2k).
  Giving arithmetic an intrinsic body first, as comparisons already have, removes the hosts' copies of it.
- **One test harness per crate**, with file-based cases only under the directive-driven fixtures in `lang/tests`.

### Design decisions for the artifact

Each item changes approved reference text and needs a decision.

- **Artifact boundary.** A core workspace holds `lang/syntax`, a reduced `lang/surface` and `lang/statics`,
  `lang/dynamics`, `lang/stackir`, `lang/assembly`, `lang/amd64`, `lang/machine`,
  `runtime`, `lang/wasm-sps`, a reduced `cli`, and `lang/tests`.
  Session overlays, Cajun, the TUI, the formatter, packages and discovery, foreign interfaces and native units,
  manual memory and scalars, the monadic extensions, xtask, and the editor grammars move
  to an extras workspace or a separate repository, and the core must build and test alone.
  This decision comes first because it settles what the other items must preserve.
- **IR boundary.** The checked residual is a view over the checker's arena.
  SPS lowering rejects leftover static forms as `ResidualStaticValue`, unwraps `Named` and `SCons`,
  and reads constructor indices, product layouts, and protocols from side tables,
  while the interpreter copies the same residual into its own tree.
  An explicit core CBPV IR, close to the interpreter's existing syntax, would be the output of static elimination
  and the monadic translation and the input of both the interpreter and SPS lowering.
  It turns the CBPV-as-IR demonstration into a data type with its own checker,
  rather than a convention over the checker's arena, and its closed types are what typed SPS IRs need.
  Interpreting the statics residual directly is the cheaper alternative and keeps the arena as the IR.
- **Typed SPS IRs.** Carry the calculi's types through high SPS and SPSLow
  so the validators become IR type checkers mirroring the paper's judgments, and delete the protocol extraction.
  This is a stronger artifact claim with fewer lines; debug-only validation is the fallback.
  The audit noted that Zydeco's polymorphism, packed values, and recursive codata need types beyond the paper's,
  so this item follows the IR boundary decision.
- **Backend shape.** Keep `SPS_l` to the structured Wasm backend directly and `SPS_l`
  through ZASM to AMD64, dropping `wasm-am` and the ZASM interpreter.
  Whether ZASM deserves its own representation, or AMD64 should be emitted from `SPS_l` with frame planning,
  is a real question; the paper's implementation claim includes ZASM today, so keeping it is the conservative choice.
  Retained native frames are an optimization: `wasm-am` keeps every ZASM variable in a fixed slot
  and passes the same end-to-end suite, so fixed slots on AMD64 would remove most frame planning (≈1.1k).
- **Surface area.** Adopt the audit's [convention](../2026-10-21-cleanup-audit/README.md#recurring-pattern),
  unify the synonym spellings, and drop `.zyi`, `@[doc]`, and text blocks.
  The grammar's 56 productions and 85 tokens each feed the parser, the formatter, the desugarer, and the checker,
  so each removed form shrinks four modules.
- **Packages and the dependent layer.** Keep them: the standard library needs them, and static elimination
  of a dependently typed module language into plain CBPV is the clearest demonstration of CBPV as an IR.
  Merge the parallel witness machinery, the repeated telescope walks, and the two projection candidate functions
  into one implementation; the layer is about 6k lines today across projection,
  functions, static values, normalization, and blocks.
  The [Builtin measurement](#the-builtin-interface-in-every-residual) shows what elimination leaves for a small program,
  which a paper figure would need to account for.
- **Monadic translation.** Keep it as the CBPV-to-CBPV transformation demonstration,
  restricted to what the standard library uses, rejecting other payloads with the existing expressivity error
  as the audit's [monadic finding](../2026-10-21-cleanup-audit/README.md#monadic-translation-extensions) proposes.
- **Numerics.** `Int`, `Float64`, `Char`, `String`, `Bool`, and `Unit` only, which collapses the primitive
  and scalar tables, the ten numeric packages, and the numeric parts of every host implementation.
  A 64-bit float does not fit a tagged word, so keeping `Float64` keeps one boxed scalar type;
  boxing it without the scalar-region optimization keeps the rest of that machinery out.

### Suggested order

1. Decide the artifact boundary.
2. Apply the deletions without design change.
3. Replace the explicit frames with recursion and stack growth, which also repairs `--dump-passes`.
4. Allocate directly in the checker, then unify errors and builders.
5. Decide the IR boundary, then either interpret the statics residual or introduce the core IR,
   and merge the front-end representations.
6. Replace the generic arenas and the host implementations with their single forms.
7. Decide the typed-IR and backend-shape questions.

If every direction lands, the review's judgment is that production Rust would fall
from 93k to the 40k–50k range, with tests shrinking in proportion.
That is an estimate, not a measurement.

## Repository weight outside Rust

- `docs/evaluations` holds 90,248 tracked lines, most of them JSON and CSV measurement data;
  the data belongs in a release asset rather than the artifact tree.
- `editor/tree-sitter-zydeco` holds 28,079 tracked lines, mostly generated parser tables.
- The Markdown documentation is 17,849 lines,
  and the [reference-drift todo](../../todos/reference-drift.md) already records
  that `DESIGN.md` restates the references.

## Reproduction

Extract the measured revision and run the script on it from a checkout that contains this record:

```sh
mkdir -p /tmp/zydeco-6aae43d0
git archive 6aae43d0 | tar -x -C /tmp/zydeco-6aae43d0
python3 -I docs/evaluations/2026-10-25-size-analysis/measure.py /tmp/zydeco-6aae43d0
```

The script printed:

```text
## Rust lines by crate
crate              production    test  test%
lang/utils               3095    1057    25%
lang/derive               297     171    37%
lang/machine             1289     449    26%
lang/syntax              2937     626    18%
lang/surface            15794    8242    34%
lang/statics            31208    4363    12%
lang/dynamics            2850     523    16%
lang/stackir            10420    5403    34%
lang/assembly            3179    1666    34%
lang/amd64               2180       0     0%
lang/wasm-common          897       0     0%
lang/wasm-am              937       0     0%
lang/wasm-sps            1144      75     6%
lang/session             4512    3925    47%
lang/tests                  0    7734   100%
cli                      3760    6227    62%
tui                      1137     411    27%
editor/cajun             4126    3909    49%
xtask                    1898     463    20%
runtime                  1740     447    20%
TOTAL                   93400   45691    33%

## Production code lines
code lines exclude blanks and line comments
lang/statics: 27594
total: 81974

## Support modules per syntax family
syntax (11): amd64/syntax.rs:415, assembly/syntax.rs:203, dynamics/syntax.rs:198, stackir/high/syntax.rs:130, stackir/low/syntax.rs:149, stackir/syntax.rs:141, statics/syntax.rs:709, surface/bitter/syntax.rs:327, surface/scoped/syntax.rs:106, surface/syntax.rs:37, surface/textual/syntax.rs:669
arena (9): assembly/arena.rs:202, stackir/arena.rs:27, stackir/high/arena.rs:234, stackir/low/arena.rs:191, statics/arena.rs:1051, surface/bitter/arena.rs:190, surface/scoped/arena.rs:302, surface/textual/arena.rs:369, utils/arena.rs:1914
alloc (3): statics/alloc.rs:528, surface/bitter/alloc.rs:66, surface/scoped/alloc.rs:53
span (5): surface/bitter/span.rs:32, surface/scoped/span.rs:41, surface/textual/span.rs:104, syntax/span.rs:78, utils/span.rs:370
err (5): session/source/err.rs:386, surface/bitter/err.rs:125, surface/scoped/err.rs:98, surface/textual/err.rs:189, utils/err.rs:72
fmt (8): assembly/fmt.rs:418, dynamics/fmt.rs:275, stackir/high/fmt.rs:424, stackir/low/fmt.rs:370, statics/fmt.rs:1221, surface/bitter/fmt.rs:62, surface/scoped/fmt.rs:67, syntax/fmt.rs:70
traverse (4): stackir/high/traverse.rs:231, stackir/low/traverse.rs:261, statics/traverse.rs:270, surface/scoped/traverse.rs:291
variables (2): stackir/high/variables.rs:205, stackir/low/variables.rs:217
check (2): stackir/high/check.rs:269, stackir/low/check.rs:444
printers: 11 files, 9473 lines
ArenaSchema/Allocates impls: 79 in 11 files

## Checker and query glue
unreachable! in lang/statics: 203
Location::caller() in lang: 404
ResultKont mentions in lang/statics: 241
salsa tracked queries marked no_eq in lang: 57
lines that are only Location::caller() in lang/statics/src: 257
function names with both a plain and a _k form in lang/statics/src: 31

## Explicit reconstruction frames
lang/stackir/src/high/lower/fold.rs: Work=40
lang/stackir/src/high/normalize/fold.rs: Work=37
lang/stackir/src/low/convert/fold.rs: Work=20
total Work variants: 97

## High SPS and SPSLow paired support modules
code lines exclude blanks, comments, and lines of punctuation alone
arena: high 142, low 127, low lines repeated verbatim in high 63
syntax: high 91, low 104, low lines repeated verbatim in high 63
traverse: high 160, low 175, low lines repeated verbatim in high 71
variables: high 143, low 150, low lines repeated verbatim in high 87
fmt: high 321, low 276, low lines repeated verbatim in high 174
check: high 86, low 160, low lines repeated verbatim in high 16
total: high 943, low 992, repeated 474

## Syntax enum variant counts
lang/surface/src/textual/syntax.rs: Pattern=11 Term=38
lang/surface/src/bitter/syntax.rs: Pattern=11 Term=42
lang/statics/src/syntax.rs: Kind=4 Type=22 ValuePattern=10 Value=15 Computation=13
lang/dynamics/src/syntax.rs: ValuePattern=8 Value=12 Computation=13
lang/stackir/src/high/syntax.rs: Value=9 Stack=4 Computation=12
lang/stackir/src/low/syntax.rs: Value=10 Stack=4 Computation=13
lang/assembly/src/syntax.rs: Terminator=6 Instruction=12

## Builtin implementations
lang/statics/src/builtin.rs: 818
lang/dynamics/src/builtin.rs: 214
lang/dynamics/src/impls.rs: 774
lang/dynamics/src/host.rs: 175
lang/stackir/src/builtin.rs: 246
runtime/stub.rs: 1267
lang/wasm-common/src/host.rs: 178
cli/wasm/wasm-host.mjs: 831
total: 4503

## Production lines by thread (by path prefix)
incremental session and editor tooling: 9079 (9.7%)
formatter and source fidelity: 4587 (4.9%)
packages, discovery, and the metadata catalog: 4084 (4.4%)
C FFI, compiled libraries, and native units: 2261 (2.4%)
manual memory and scalar kernels: 1672 (1.8%)
post-hoc validators: 4809 (5.1%)
monadic elaboration: 3282 (3.5%)
abstract-machine Wasm backend and ZASM interpreter: 1292 (1.4%)
pass framework and plans: 746 (0.8%)
repository tooling: 1898 (2.0%)
remaining pipeline and CLI: 59690 (63.9%)
total production: 93400
```

The accretion table needs the repository history rather than an extraction.
For each path, it records the date of the first commit that added it and the number of commits touching it:

```sh
git log --diff-filter=A --format=%ad --date=short -- PATH | tail -1
git log --format=%h -- PATH | wc -l
```

The growth and churn figures after the accretion table come from the history up to the measured revision:

```sh
for d in 2026-07-01 2026-08-01 2026-09-01 2026-09-15; do
  rev=$(git rev-list -1 --before=$d 6aae43d0)
  git grep -c '' $rev -- '*.rs' | awk -F: '{s += $NF} END {print s}'
done
git log --since=2026-07-15 --format=%h 6aae43d0 -- '*.rs' | wc -l
git log --since=2026-07-15 --format= --numstat 6aae43d0 -- '*.rs' | awk '$1 != "-" {a += $1; d += $2} END {print a, d}'
```

The restructuring figures repeat the last two commands with `--grep='^sisy:'`.
The frame migration sums `git show --format= --numstat` over `185332c2`, `ba2b8d57`,
`fb3b4ee7`, `194b944a`, `b8a9de75`, `9c1d3000`, `f6ea5a88`, `d97bbad0`, and `370195d8`,
counting files under `tests/` or ending in `tests.rs` as test code.

The Builtin and `--dump-passes` observations use a debug build of the measured revision:

```sh
cargo build -p zydeco-cli --bin zydeco
target/debug/zydeco build lib/tests/core/fact.zy -t zir | grep -c '^\[parameter:'
target/debug/zydeco build lib/tests/core/fact.zy -t zir --sps-passes none | wc -l
target/debug/zydeco build lib/tests/core/fact.zy -t zir --dump-passes
```

The integration-test target count comes from `cargo metadata --no-deps --format-version 1`.
The tracked line counts outside Rust come from `git ls-files -z DIR | xargs -0 cat | wc -l`
for `docs/evaluations` and `editor/tree-sitter-zydeco`, and from `git ls-files '*.md' | xargs cat | wc -l`
for the Markdown total. The rule line ranges come from reading the two computation modules at the measured revision.

## Limits

- Line deltas marked ≈ are review estimates inferred from file sizes; they overlap and are not measured deletions,
  and the 40k–50k projection compounds them.
- The thread attribution is by path prefix and counts whole files.
  Modules that serve several threads are counted once, and the session's query module is counted
  as incremental tooling although the CLI's non-incremental entry points live there too.
- The test split counts `#[cfg(test)]` items other than modules, such as helpers and imports, as production code,
  and counts production items that follow an inline test module as test code.
- Figures that neither the script nor the listed commands reproduce come from the review passes
  and were checked selectively: the checker's feature attribution, the classification of `unreachable!` sites,
  the thirteen ways to register a test, and the ≈ deltas.
- The `--dump-passes` overflow was observed in a debug build; a release build uses smaller frames
  and may not overflow on the same input.
- Variant counts recognize only the enum names the script lists and only variants written one per line
  at four-space indentation, which matches the syntax modules at this revision.
- The support-module table counts files by name, so support code under other names is not included.
- The judgments of what is core follow the yardstick above; an agenda centered on editor tooling,
  systems programming, or formatting fidelity would retain different parts.
  A narrower agenda, with a minimal CBPV source language, one integer type, one native backend,
  and no monadic translation, was estimated by the review passes at about 11k–14k code lines,
  against 82.0k production code lines at the measured revision.
