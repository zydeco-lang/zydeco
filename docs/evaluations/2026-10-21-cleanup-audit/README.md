# Cleanup audit, 2026-10-21

On 2026-10-21 a read-only audit asked three questions of the repository:
which features are unnecessary to Zydeco's core ideas, which mechanisms are redundant,
and which features are built monolithically where smaller existing features would compose.
The target was `7e5ee4c1` on the `prelude` branch.
Seven reviews covered the surface front end, the checker, the backends and runtime,
shared infrastructure, user-facing tooling, the standard library, and the documentation.
Direct measurements covered feature usage and code size, and spot checks confirmed the reviews' central claims.

The findings are proposals, not decisions.
Removing or changing an approved feature changes the references and requires approval
under the [documentation rules](../../../AGENTS.md#documentation-structure-and-design-records).
This record describes the measured revision and is not a backlog: settled designs belong
in the [references](../../references/README.md), and observed drift belongs in the [todos](../../todos/README.md).

An uncommitted implicit-prelude migration touching about 290 files was in progress in the working tree during the audit.
All measurements use a clean extraction of `7e5ee4c1`, so observations about prelude boilerplate may be superseded
by that change.

A post-audit verification pass re-derived the reproducible figures and corrected several statements;
the changes are recorded under [Post-audit verification](#post-audit-verification).
A later revision extended the [yardstick](#yardstick) to the two calculi of the stack-passing-style paper,
and [SPS yardstick revision](#sps-yardstick-revision) lists the findings it re-scoped.

## Yardstick

The core ideas come from two papers. The first, the OOPSLA paper on relative monads,
contributes the claims of its [artifact](../../../lib/tests/oopsla/README.md),
together with [term-oriented composition](../../references/language.md#term-oriented-composition):

- relative monads model stack-manipulating computation;
- monadic blocks reinterpret CBPV code with any relative monad;
- monadic blocks derive monad transformers.

The second, the stack-passing-style (SPS) paper, contributes its two main calculi,
which [`lang/stackir`](../../../lang/stackir/src/README.md) implements:

- **SPS** makes CBPV's stack an explicit, second-class sort.
  Each program consumes one ambient stack `•` exactly once on every path, and no value captures it.
  Continuations, argument pushes, and tag pushes build stacks, closures receive one at each call,
  and value and stack let-bindings are the join points.
  High SPS, `BranchJoinProgram` in [C8](../../references/compiler.md#c8-sps-lowering-normalization-and-demand),
  realizes SPS in the paper's branch-join fragment `SPS_br`, whose stack lets occur exactly
  around value-coproduct matches; the implementation treats scalar comparisons as a second branching form.
- **`SPS_l`**, which the code calls SPSLow, is the first-order target of closure conversion: blocks with self labels,
  jumps, value-existential closure packages, and stack-existential continuation packages.
  `SpsLowProgram` in [C9](../../references/compiler.md#c9-closure-conversion-and-first-order-spslow) realizes it.

Both IRs erase the calculi's types
and carry [partial protocol descriptors](../../references/compiler.md#partial-source-protocols) instead,
and both add primitives, external calls, and memory steps that the calculi lack.
The paper's implementation claim covers the compiled path: lowering checked CBPV to SPS, local β-simplification
whose reductions orient the SPS βη equations and whose sharing policy orients the join-point equations,
closure conversion with the existential-package representation, and lowering through ZASM to executable code.
The paper's duplicating and full schemes, which the implementation does not realize,
and its second-class procedures, an extension of SPS, are outside the yardstick.

The OOPSLA artifact needs only `zydeco check` and the interpreter ([oopsla.rs](../../../lang/tests/tests/oopsla.rs)),
whereas the SPS claims need the compiled pipeline through an executable backend.
The `e2e_sources!` and `e2e_io_source!` macros
in [`lang/tests/src/lib.rs`](../../../lang/tests/src/lib.rs) run the interpreter, AMD64,
and both WebAssembly backends, and `runtime_source!` runs the interpreter and both WebAssembly backends without AMD64.

## Where the weight is

| Area | Rust code lines | Share |
| --- | ---: | ---: |
| Front end and checker: surface, syntax, statics, dynamics | 58,668 | 48% |
| Compiled backends: stackir, assembly, amd64, machine, runtime, wasm-* | 26,769 | 22% |
| Tooling: session, cli, cajun, tui, xtask | 27,530 | 22% |
| Infrastructure and tests: utils, derive, lang/tests | 10,459 | 8% |
| Total | 123,426 | |

The standard library has 6,909 lines. Manual memory in `lib/std/memory` holds 2,475 of them,
while the relative-monad effect library, the subject of the OOPSLA paper, holds 1,045.
The [language reference](../../references/language.md) gives 640 lines to §1–§10, the calculus itself,
and 1,865 lines to §11–§14.

Under the SPS half of the yardstick, most of the compiled-backend row lies on the paper's executable path.
`lang/stackir`, `lang/assembly`, `lang/amd64`, `lang/machine`, and `runtime` hold 24,089
of its 26,769 lines, and the three WebAssembly crates hold 2,680.
`lang/stackir` alone has 14,547 lines, tests included: 6,413 in `src/high`, 5,222 in `src/low`,
which includes the SPSLow validators, and 1,703 in the protocol extraction.

## Recurring pattern

Most removable weight belongs to features whose only clients are their tests or a small surface
that the change also reworks: the manual-memory library beyond the 592 lines std reaches,
C exports and native units, package relationships, monadic-translation extensions,
view patterns, `@[doc]`, the typed-arena lint, high-SPS pass plans, the recursive folder driver,
the generic representation-policy trait, and compact frame environments.
The SPSLow validators left this list with the SPS half of the yardstick, because they check part
of `SPS_l`'s typing ([backend translation validators](#backend-translation-validators)).

A convention follows from these cases: a language form, standard-library module, compiler pass,
or CLI option stays on the main line only when something other than its own tests uses it, such as std,
the OOPSLA artifact, the SPS paper's compiled pipeline and its evidence, a demo, or a maintained tool path.
Other work lives on an experiment branch or in a proposal until such a client exists,
and a removal that displaces a tool or standard-library client must rework that client in the same change.
Adopting the convention is itself a design decision.

## Design findings

Each finding in this section changes approved reference text.
Line deltas marked ≈ are the reviews' estimates; they overlap and are not measured deletions.
The findings are scoped by the yardstick above: they do not measure what a removed capability might be worth outside
that agenda.

### Systems-programming thread

Manual memory, compiled C libraries with native units,
and fixed-width numerics form one research thread outside the yardstick.

- 1,883 of the 2,475 lines in `lib/std/memory` are imported only by tests, documentation, and evaluations.
  Std itself reaches 592 lines of it, through `std.zy`, which imports `memory/buffer.zy`, and through `text/bytes.zy`.
- The compiler carries about 1,250 lines of dedicated memory modules, and memory IR nodes appear
  in 36 non-test compiler files.
- All 19 standard-library uses of the static integer intrinsics `int_add`, `int_sub`,
  `int_and`, and `int_compare` are in `memory/`, as are 135 of the 170 value-function definitions in std.
- C exports and native units cost ≈1.9k Rust lines plus ≈1.2k test lines.
  One example, `lib/ffi/arithmetic.zy`, exercises C exports and is only type-checked;
  no checked-in source declares `library(zydeco)`, and the role appears only in sources that tests generate.
- 134 of the 182 builtin-role annotations are numeric;
  ten of the twelve `lib/std/builtin/numeric/*.zy` files are identical up to the type name,
  while `int64` and `uint64` add conversion roles.
  The OOPSLA, demo, monadic, and effect sources use only `Int`, `String`, `Char`, `Bool`, and `Unit`.

Moving the thread to an experiment branch would retain only a raw `Addr` adapter for bytes and I/O;
making `Bytes` a host primitive is the alternative.
C imports could sit behind a cargo feature, which would also remove libffi
and the C toolchain from the default interpreter build.
The [manual memory](../../references/language.md#manual-memory)
and [foreign interface](../../references/language.md#14-foreign-interfaces) sections would move with the code.
The thread also reaches the calculi's implementation: `AddrOffset` and `MemoryStep` belong
to the syntax that high SPS and SPSLow share, and SPS and `SPS_l` have no counterpart,
so moving the thread out would remove forms that both IRs add to the paper's calculi.

### Package hierarchy

[Source packages](../../references/language.md#source-packages) add opaque roots, unnamed packages,
route-dependent copies merged by structure, `discover` globs, and `test(of(...))` relationships.
Relationships have no language operation as a client: `zydeco show` prints them,
Cajun's completions and document links surface them, and they enter the structural merge key,
so removing them also reworks those tools and changes copy identity.
Root substitution is constructed only in session tests, and glob discovery,
about 400 lines, serves two `workspace.zy` files.
The machinery costs ≈1.5k Rust lines plus ≈2k test lines and 212 reference lines.
Quoted imports, name-to-file lookup, and the test role cover the current clients.

### Monadic-translation extensions

Std and the OOPSLA artifact use computation translation, type roots, and package patterns.
Translation of witness-dependent arrows,
[polymorphic value functions](../../references/language.md#polymorphic-value-functions),
[applicative nominal families](../../references/language.md#applicative-nominal-translation),
and value roots has only test clients: ≈1.5k lines and about 45 fixtures.
Those payloads share nominal-translation machinery with the computation translation std uses,
so the estimate overlaps code that stays.
Rejecting those payloads with `tyck.expressivity`, as the translator already does
for another unsupported form, keeps external values as explicit parameters,
which the [closed translation boundary](../../references/language.md#closed-translation-boundary) already requires.

### Surface forms

The [reproduction script](#reproduction) counted these uses with comments removed.

| Form | Uses | Note |
| --- | --- | --- |
| `define` | 0 | Diagnostics list `define` rather than `def` among expected tokens |
| `comatch p => M end` | 0 | Undocumented alias of `fn` |
| `sigma` | 4, all compiler cases | Equals `exists` for type binders and `*` for value binders |
| `<\|` | 7 uses in 6 files: one std module, 2 lib test files, 3 compiler-case files | Costs a precedence level and a formatter case |
| `f ~> p` views | 16 uses in 6 files: 2 lib tests, 4 compiler cases | About 130 match arms and references in 9 crates; `match f v` expresses them |
| `param ... that` | 46 uses in 31 files, of which 16 in std | Hoists an abstraction to the nearest block |
| `.zyi` companions | 0 files | Removing them also removes the reason for a separate `.zydeco` extension |
| `@[doc]`, `@(literal)` | 0 and 1 | The only consumers of `--\|` text blocks |
| `--\|` text blocks | 53 | Most of the 48 OOPSLA blocks are section headers; `zydeco check` warns about each |

Some heavily used spellings are also synonyms: `forall` and `pi` desugar to one quantifier,
`param p in` equals `fn p =>`, and `def` on a value behaves as `let`.
Unifying them requires a mechanical migration, including the OOPSLA examples.

### Backend scope

Two WebAssembly backends have been maintained side by side
since the [backend proposal](../../proposals/wasm-backends.md) of 2026-08-30,
and 15 of the 24 commits to their emitters changed both.
The backend review favors `wasm-sps`, which maps SPSLow's explicit stacks directly, whereas `wasm-am` embeds ZASM
in a dispatch loop; the proposal's 2026-08-30 prototype measurement counted 13,935 WebAssembly functions
for `fact.zy` from `wasm-am` against 525 from `wasm-sps`, and a focused test still asserts the ordering.
Removing `wasm-am` would save ≈1.1k lines, one crate, and a quarter of end-to-end runs.
The SPS half of the yardstick strengthens that preference: `wasm-sps` executes `SPS_l` blocks directly
as WebAssembly functions, and the paper's end-to-end evidence runs through ZASM and the native backend,
so `wasm-am` serves neither half.

### Effect library breadth

No non-test module outside `lib/std/effect` imports the effect library, and the OOPSLA artifact imports only `monad.zy`.
A minimal demonstration keeps the monad interface, the transformer derivation, State, Exception,
and NonDet, the last being the only effect whose carrier needs computation-type algebras;
Reader, Writer, Option, and StateExn could become examples.
Two fixes need no design change: `effect/ret.zy` takes Builtin without using it, which forces the effect modules
to take the package and makes their clients apply it (the direct `builtin |> import("ret.zy")` form appears
in six effect modules; `lib/std` contains 44 `builtin |> ...` applications in total,
most in the memory and package builders), and `triv : Thk Top` is defined in seven effect modules.
About 880 lines of library contracts in the language reference belong in a library reference.

## Implementation findings

These findings change code and the compiler reference's account of it, but no source-language behavior.

### Backend translation validators

`SpsLowProgram::try_new` in [`lang/stackir/src/low/check.rs`](../../../lang/stackir/src/low/check.rs)
runs the [partial source protocol](../../references/compiler.md#partial-source-protocols)
and [word entry contract](../../references/compiler.md#word-entry-contracts) validators on every compilation.
The validators are about 2,050 lines including the protocol-descriptor extraction they read,
with 1,830 lines of dedicated tests, and no backend reads their results.
They re-derive types after erasure to validate the compiler's own passes.

Under the SPS half of the yardstick, that re-derivation is the implementation's only check, partial as it is,
that its `SPS_l` output is well typed, which the paper's type-preserving compilation
and closure conversion lemmas guarantee for the paper's own translations.
The descriptors are partial forms of the SPS types that the paper's type translation assigns: `Thk(S)` stands
for `Closure B`, `A :: S` for `A ⊘ B`, `cont(A)` for `Kont A`, and a codata graph with tagged observations for `⊕`.
Comparing a block's entry protocol with the stack a jump supplies approximates the stack premise of the jump rule.
The entry contract's provenance rule, under which code from one opening consumes only that opening's environment
or residual stack, enforces what the freshness of a package's hidden type enforces in `SPS_l`.

The finding is therefore a choice about how to check `SPS_l`, rather than removable scaffolding:

- keep the partial check;
- carry the calculi's types through both IRs and check the paper's judgments,
  which would need types beyond the paper's for Zydeco's polymorphism, recursive codata, and packed values;
- delete the check and leave the pipeline's `SPS_l` output unchecked.

Deleting the validators together with the extraction saves ≈3.5k–4.2k lines,
but high lowering also takes its canonical destructor tags from the extracted protocol graph,
so it would need another source of tag numbering.
Confining validation to test builds removes it from the production path but keeps the extraction,
so it saves only the production portion.

### Per-judgment checker queries

The producer queries in `lang/statics/src/query/`, about 1,800 lines, read `#[no_eq]` inputs,
so their memos cannot survive an edit, while `check_source` already memoizes the whole check.
The checker carries 66 `unreachable!` glue sites for them,
and the [retention idea](../../ideas/README.md#compiler-memory-retention) records
about 30% end-to-end checking overhead from the migration.
Direct allocation with the existing derived identifiers would save ≈2.3k–2.8k lines;
[query ownership](../../references/compiler.md#query-and-checker-ownership) would need amending.
The proposal records the overhead as a 2026-08-14 measurement and asks for re-measurement
before extending the migration.

### Typed-arena lint

The [typed-arena lint](../../references/compiler.md#typed-arena-lint) and its re-derivation,
`validate/lint.rs` and `validate/rederive.rs` with about 2,600 lines, run only under `--lint-types` and in tests.
The statics review found no commit in which the lint caught a checker defect,
and it duplicates kind agreement and structural type equality.
A test helper of about 200 lines that checks fill closure, annotation sorts,
and hole policy would keep the useful part and save ≈2k lines.

### Pass framework for one pass

`HighSpsPass` in [`lang/stackir/src/passes.rs`](../../../lang/stackir/src/passes.rs) has a single variant, `Normalize`,
yet [compiler pass composition](../../references/compiler.md#compiler-pass-composition) provides runtime sequences,
observers, a plan grammar, a `zydeco passes` command, and four `--*-passes` options.
The `when` and `by_ref` combinators have no users, and only a test uses `repeat`.
A normalization switch and one dump option would save ≈1.0k–1.3k lines.

The SPS half of the yardstick gives that switch and dump option a client.
The paper's evidence shows a program's SPS before and after simplification and its lowered `SPS_l`
under a recorded configuration; `--sps-passes none`, `--dump-passes`, and `build --target zir` provide that today,
while the plan grammar, runtime sequences, and combinators still serve no client.
The catalog would gain clients if the reductions the paper names separately became separately selectable passes:
closure β as inlining, continuation β as call elimination, continuation η as tail-call optimization,
and the sharing policy, which `Normalize` currently performs together.

### Test-only generality

Several production interfaces carry parameters that only tests vary:

- the folder `Driver` parameter, whose `Recursive` implementation appears only in test modules (≈400 lines);
- the representation-policy trait, whose production path fixes one policy and whose variations come only
  from tests (≈350);
- compact and moving frame environments, whose feature and code paths are exercised only
  by tests (the compact environment is selected through a test that rewrites `runtime/Cargo.toml`) (≈650);
- raw memory kernels, a fused IR that applies only to a 64-bit load, at most 32 operations,
  and a store of the same type (≈650–900);
- the live editor configuration in Cajun, which serves two hover settings (≈270);
- `--target-arch`, which accepts `Aarch64` but rejects it downstream, leaving one supported value.

### Parallel implementations

Each of these mechanisms exists more than once:

- `MonConstruct` copies `Construct`, beside a third constructor interface, `Alloc` (≈700);
- the walk over existential telescopes is written at least seven times, and field projection searches
  through two near-identical candidate functions (≈400);
- `PackPi` and `ValPi` bind witnesses through separate machinery;
  formation walks nested products while application walks only the leading prefix, which
  is the [nested witness diagnostic](../../todos/compiler-boundaries.md#nested-witness-diagnostic) discrepancy (≈500);
- high SPS, SPSLow, and scoped syntax each define a depth-first visitor framework (≈300);
  a shared framework would still keep high SPS and SPSLow as distinct IRs,
  since they realize the SPS paper's two calculi;
- builtins are implemented in the interpreter, the native stub, and the Node host, with a separate role table;
- the source scanner and the desugarer both validate builtin and intrinsic annotations (≈420);
- the CLI and TUI repeat diagnostic dispatch, and the TUI strips ANSI escapes with a hand-written state machine;
- the arena traits carry 54 source-level marker and accessor implementations,
  and `ArenaAccess` is mostly imported only to bring methods into scope (≈600).

### Dead code

These items have no working production use:

- `error_entry_output` and its second copy of the 48 diagnostic message arms in `check/error.rs`;
  the `blame` field that only it reads forces 403 `Location::caller()` arguments (≈400);
- the ZASM interpreter behind `build -t zasm -x`, which reaches `todo!()` at the `Terminator::Extern` case recorded
  in the compiler todos at the time and at the `AllocContext` instruction that lowering emits at every jump (≈500);
- `CommandCompiler::interpret` and `BackendProgram::lower`, and the `ProgKont::Dry` result,
  which the only live interpreter call never requests;
- `StrictParser::term` and `pattern` with their recovering counterparts,
  whose test-only grammar entry points enlarge the LALRPOP tables, and the test-only printer `textual/ugly.rs`
  with 659 lines;
- `obliviate`, `keep_only`, and 78 commented-out lines in `lang/utils/src/graph.rs`;
- `AssemblyArena.deps`, read only by a test; retaining it forces every inserted program to be cloned.

## Defects found during the audit

- Parser diagnostics list `define` instead of `def` among expected tokens.
- Every `build -t exe` computes the runtime digest and target platform twice to validate C-library
  and native-unit imports, even when the program has none ([`cli/src/main.rs`](../../../cli/src/main.rs),
  lines 496–513).
- `env!("CARGO_MANIFEST_DIR")` paths in the TUI engine, and in the in-progress prelude code,
  tie an installed binary to the checkout it was built from.
- The tracked [n-ary product inference](../../todos/compiler-boundaries.md#n-ary-product-inference-refinement)
  discrepancy comes from a two-component refinement
  in [`normalize/inference.rs`](../../../lang/statics/src/normalize/inference.rs),
  which follows the unadopted right-spine binary-products design rather than n-ary products.

The product-inference finding is already tracked in the compiler todos, as is the ZASM panic above;
the parser-diagnostic, duplicate-digest, and manifest-path items are unrecorded drift and belong there before repair.

## Tooling, tests, and documentation

- **Tests.** The workspace builds 74 integration-test binaries,
  and `lang/tests` alone needs about 1.6 GB per debug build.
  [`lang/session/src/source/tests.rs`](../../../lang/session/src/source/tests.rs) carries its own backend pipeline
  for about 40 whole-program tests that belong in `lang/tests`, and source tests can be written six ways.
- **Test runner.** `xtask test-resume`, about 2,361 lines at the measured revision (committed around 2026-10-20),
  reimplements a test runner that CI does not use; `flake.nix` already provisions nextest.
- **Editors.** Three grammar artifacts support the compiler and two editor clients: the LALRPOP parser,
  tree-sitter (Zed) with about 25,000 committed generated lines, and TextMate (VS Code).
  `cli/build.rs` also writes 1,250 lines of tracked shell completions into the source tree.
- **Formatter.** The formatter has about 2,900 implementation and 2,850 test lines,
  a quarter of the surface implementation, and every `@[format]` option except `verbatim` is unused.
- **Documentation.** The review estimated that about 860 of the 1,014 lines
  in [DESIGN.md](../../../DESIGN.md) restate the references and have drifted from them.
  `docs/legacy`, with 2,013 lines, and the earlier tutorial use removed syntax;
  the OOPSLA README cites files that no longer exist; `docs/ideas` holds 22 near-empty files;
  and `cli/tests/passes.rs` runs sources stored under `docs/evaluations`.
  No reference or guide states which SPS calculus each stack IR realizes:
  the [stack IR guide](../../../lang/stackir/src/README.md) points to a paper correspondence
  in DESIGN.md's implementation architecture section, which the 2026-09-04 rewrite `7d2e434b` removed,
  so only code comments name `SPS_l` and the paper's stack-join placement.

## Retained after review

The reviews judged these parts essential or proportionate:

- the reference interpreter, the oracle for every backend;
- value functions, static elimination, and value-function witness routes, which std's module factories use;
- recursive field search and `;` pattern groups, which every std import uses, although their internals duplicate;
- `begin ... end` blocks with `that`, the only way to write recursive sealed types;
- `pack`, `|>`, field puns, and `@(m)`;
- high-SPS normalization, closure conversion, ZASM lowering, and frame planning for AMD64;
- the copying collector and its block-start index;
- `@[typeof]`, which is small and supports the prelude bootstrap;
- derived identifiers, which keep Salsa replay stable.

The SPS half of the yardstick adds the two IRs that realize the paper's calculi:
high SPS with its lowering from checked CBPV and its branch-join verifier, and SPSLow with the blocks,
jumps, and packages that closure conversion builds.

## Suggested order

1. Remove the dead code, which changes neither behavior nor reference text.
2. Remove test-only scaffolding and unconsumed machinery: the pass framework beyond a normalization switch
   and a dump option, the lint, folder drivers, representation policies, frame experiments, producer queries,
   and the duplicated test and tooling paths (the session source-test pipeline, `xtask test-resume`,
   editor-grammar and completion generation, and unused formatter options).
   Source behavior stays the same, but the compiler reference sections that describe them change,
   which requires approval.
3. Decide the design findings: the systems-programming thread, one WebAssembly backend,
   a minimal package system, the monadic extensions, the rare surface forms, the effect-library breadth,
   and how SPSLow output is checked against `SPS_l`.
4. Merge the parallel implementations, and fold documentation into the references that own each rule.

A step that touches `lang/stackir`, ZASM, or the native backend must keep the SPS paper's implementation claim true:
lowering to SPS, β-simplification, closure conversion with existential packages, and lowering to executable code.

## Reproduction

Extract the measured revision and run the usage script on it from a checkout that contains this record:

```sh
mkdir -p /tmp/zydeco-7e5ee4c1
git archive 7e5ee4c1 | tar -x -C /tmp/zydeco-7e5ee4c1
python3 -I docs/evaluations/2026-10-21-cleanup-audit/usage.py /tmp/zydeco-7e5ee4c1
```

The script printed:

```text
sources: 627 (.zy 599, .zyi 0, .zydeco 28)
`define` keyword: none
`sigma` binder: compiler cases 4 uses in 4 files
`<|` backward application: std 1 uses in 1 files, lib tests 2 uses in 2 files, compiler cases 4 uses in 3 files
`~>` view pattern: lib tests 11 uses in 2 files, compiler cases 5 uses in 4 files
`comatch p => M end` alias: none
`param ... that` placement: std 16 uses in 11 files, lib tests 17 uses in 11 files, compiler cases 7 uses in 4 files, docs 6 uses in 5 files
`@[doc]` annotation: none
`@(literal)` splice: lib tests 1 uses in 1 files
static integer intrinsics: std 19 uses in 8 files, compiler cases 3 uses in 1 files
`let val` / `param val`: std 170 uses in 37 files, OOPSLA 5 uses in 5 files, lib tests 20 uses in 11 files, compiler cases 74 uses in 61 files, docs 5 uses in 2 files, other 3 uses in 2 files
`--|` text blocks: OOPSLA 48 uses in 10 files, lib tests 5 uses in 3 files
```

The area table counts the `code` lines that tokei 15.0.0 reports for Rust in the listed directories of the extraction,
for example `tokei lang/surface lang/syntax lang/statics lang/dynamics`.
The per-crate and `lang/stackir` figures under [Where the weight is](#where-the-weight-is) count the same way,
one path at a time, for example `tokei -t Rust lang/stackir/src/high`;
the protocol figure combines `lang/stackir/src/protocol.rs` and `lang/stackir/src/protocol/`.
The reference figures count lines from each numbered `##` heading of `docs/references/language.md` to the next.

## Limits

- Line deltas marked ≈ are review estimates; they overlap and are not measured deletions.
- Usage counts match regular expressions against comment-stripped sources.
  They count syntax rather than semantic dependence, so a single use may stand for a whole library.
- Figures that the reproduction does not cover come from the reviews and were checked selectively.
  Spot checks confirmed, among others, the ZASM `todo!()` path, the always-on SPSLow validators,
  the single `HighSpsPass` variant, the `#[no_eq]` checker inputs, the 403 caller locations,
  the two-component product refinement, the duplicated runtime digest, the seven `triv` definitions,
  and the unused Builtin parameter of `ret.zy`.
- The record retains the usage script and its printed output; the seven review notes
  and their raw counts were not retained, so figures outside the reproduction rest on the reviews and
  on the verification pass below.
- Judgments of what is core follow the yardstick above; an agenda centered on systems programming
  in CBPV would retain different parts.
- The seven reviews applied only the OOPSLA half of the yardstick.
  The SPS half was applied afterward from the paper and the code, without repeating the reviews,
  and reflects the paper as of 2026-10-21.

## Post-audit verification

A verification pass after the audit re-derived the reproducible figures from a clean extraction of `7e5ee4c1`
and checked the review-derived ones against the code without a full workspace rebuild.
It confirmed the area table, the surface-usage output, the standard-library and reference line counts,
the memory reachability split, the wasm commit split, the defect list, and the spot checks above.
It corrected or qualified these statements in place:

- `--target-os` has two fully implemented values, Linux and macOS, so only `--target-arch` remains a candidate interface
  with one supported value.
- The representation-policy description inverted its implementations: production fixes the `Local` policy,
  and the `RepresentationStrategy` enum varies only in tests.
- Ten, not twelve, numeric builtin packages are identical up to the type name;
  `int64` and `uint64` add conversion roles, and the role denominator now names the annotations it counts.
- The `<|` and `~>` rows of the surface-forms table mixed use and file counts; both rows now state both.
- The `library(zydeco)` sentence now distinguishes checked-in sources from sources that tests generate.
- The wasm function counts are the proposal's historical 2026-08-30 measurement, now labeled as such.
- The `builtin |> ...` total of 44 is standard-library-wide, not the count forced by `ret.zy`;
  the text now separates them.
- The formatter and Cajun estimates were scope-dependent; measured values are about 2,900 and 270 lines.
  The arena count is 54 source-level implementations, `textual/ugly.rs` is 659 lines,
  the commented-out graph block is 78 lines, and the ZASM citation now names the todo's first reachable case.
- Validator savings now distinguish deleting the protocol extraction from confining validation to tests.
- The package-relationship and monadic-extension findings now note the tool consumers, merge-key effect,
  and shared nominal machinery that bound the removal estimates.
- The end-to-end-macro, test-runner timing, editor-grammar, and annotation-validation phrasings were corrected.
- The suggested order now covers the tooling and test-harness work and the effect-library decision,
  and the Limits section records the retained-evidence gap.

Two review counts did not reproduce exactly: about 1,250 dedicated memory compiler lines and 36 memory-IR files.
Re-derivation brackets them at roughly 1.0k–1.3k lines and 27–41 files depending on module-set definition,
so they remain review estimates.

After this record was written, the implicit-prelude migration landed as `1cb3ab80` (319 files),
including the `env!` prelude lookup noted as in progress.
The measurements and the reproduction still describe `7e5ee4c1`; run the script on the clean extraction,
because a working tree adds build artifacts and later sources to the scan.

## Dispositions

The repository kept or changed these tooling findings after the verification pass:

- **Test runner.** `cargo test-resume` is retained.
  It is documented in AGENTS.md and CONTRIBUTING.md, and it provides resume semantics that nextest does not;
  the finding that CI does not use it stands, but removal was declined.
- **Generated artifacts and editors.** The tracked shell completions and the tree-sitter parser are retained.
  The CI workflow now verifies that `cli/completions` matches the CLI definition,
  and the existing tree-sitter job already verifies its generated parser.
  Dropping or merging an editor client was not attempted.
- **Formatter options.** `width`, `indent`, `layout`, and `parentheses` are retained and documented
  in the compiler reference as reserved options for downstream code; removing them would change approved surface syntax.
- **Session test pipeline.** Whole-program cases moved to the shared test suites:
  directive-driven fixtures for the builtin and monadic suites, the existing demo and standard-library suites,
  and direct checks where a program has no execution contract.
  Source-graph, loader, overlay, span, and arena assertions remain in the session crate,
  so the pipeline now serves only crate-internal cases.
- **Docs-dependent test.** `cli/tests/passes.rs` now reads fixtures under `cli/tests/fixtures/` instead
  of sources under `docs/evaluations/`, keeping the evaluation record's evidence out of the test path.

## SPS yardstick revision

The audit measured the repository against the OOPSLA claims alone, but the next paper concerns stack-passing style,
and `lang/stackir` implements its two main calculi.
This revision compared the paper's SPS and `SPS_l` and its implementation claims with the code at `7e5ee4c1`.
The compiled-path crates and the compiler reference's C1, C8, and C9 are unchanged from there through `7fe0caed`.
The revision re-scoped these statements in place:

- The [yardstick](#yardstick) gained an SPS half: SPS realized by high SPS in the branch-join fragment,
  `SPS_l` realized by SPSLow, and the compiled path that the paper's implementation claim covers.
- The [backend translation validators](#backend-translation-validators) became a choice
  about checking `SPS_l` instead of removable scaffolding, and the deletion estimate now notes
  that high lowering takes its destructor tags from the protocol extraction.
- The [pass-framework finding](#pass-framework-for-one-pass) names the paper's before-and-after evidence
  as a client of the normalization switch and dump option, and states when the plan catalog would gain clients.
- The [systems-programming](#systems-programming-thread)
  and [backend-scope](#backend-scope) findings note how each bears on the two IRs.
- [Where the weight is](#where-the-weight-is) adds measured figures for the paper's executable path
  and for `lang/stackir`.
- The recurring pattern, the convention, the parallel visitors, the retained list, the suggested order,
  and the limits reflect both halves of the yardstick.
- The documentation findings record that no reference or guide states which calculus each IR realizes.

The other findings concern the front end, the checker, tooling, the standard library,
and backend mechanisms below `SPS_l` that the paper's claims do not name, such as representation policies
and frame environments; the revision found no client for them in the paper.

The compiler reference then gained the correspondence in [C8](../../references/compiler.md#correspondence-with-sps)
and [C9](../../references/compiler.md#correspondence-with-sps_l), and the stack IR guide now points there,
which resolves the documentation finding.
The compiler then implemented the paper's duplicating and full schemes beside branch-join,
selected with `--sps-scheme`, so the scheme exclusion in the [yardstick](#yardstick) describes `7e5ee4c1` only.
It then implemented the paper's second-class procedures in their non-recursive form:
the default SPS plan localizes closures that are only called, and closure conversion lowers procedures to labels,
so the procedure exclusion in the yardstick likewise describes `7e5ee4c1` only.

