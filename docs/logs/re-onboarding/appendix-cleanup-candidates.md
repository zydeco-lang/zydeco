# Appendix — Cleanup Candidate Inventory

> Scratch study note for re-onboarding. Rows are candidates distilled from the retired 2026-10-21
> cleanup audit and 2026-10-25 size analysis; their durable homes are the
> [artifact boundary proposal](../../proposals/artifact-boundary.md), the
> [checker query proposal](../../proposals/checker-query-retirement.md), and the
> [todo records](../../todos/README.md). They are **proposals, not decisions**, and line estimates
> overlap. Status was re-checked against `e2dd8979` where marked.

## Status Legend

| Status | Meaning |
| --- | --- |
| `resolved` | Removed or settled after the retired study; the evidence cites the commit or the study's update paragraph |
| `open — code only` | Removal should not change source behavior or approved design; verify callers first |
| `open — reference gate` | Removal changes approved reference text, so it needs owner approval under [AGENTS.md](../../../AGENTS.md#documentation-structure-and-design-records) |
| `decision needed` | A design choice with options; do not start with code |
| `retained` | Reviewed and deliberately kept; do not re-litigate without new evidence |
| `open (retired study)` | Finding stands in the retired study; not re-verified for this manual |

Line estimates are the evaluations' `≈` figures unless marked measured.
They are not additive. Signals found only while writing the module cards are in each card's §9;
this appendix records the evaluation findings plus their current status, and §G records drift discovered by this pass.

## A. Source, Library, and Artifact Findings

| # | Finding | Main evidence | Scale | Status at `e2dd8979` |
| --- | --- | --- | --- | --- |
| A1 | Systems-programming thread: manual memory, C exports/native units, fixed-width numerics lie outside both yardsticks | [audit §systems-programming](../../proposals/artifact-boundary.md#systems-programming-thread) | 1,883 of 2,475 `lib/std/memory` lines only tests/docs/evals; ≈1.9k + ≈1.2k test for C exports/native units; 10 numeric builtin packages identical up to type name | `decision needed`; `lib/std/memory` now 2,435 lines, `std.zy` reaches `memory/buffer.zy`; memory/scalar IR forms are shared by high SPS and SPSLow, so moving the thread also removes forms from both IRs |
| A2 | Package hierarchy: opaque roots, unnamed packages, route-dependent copies, `discover` globs, `test(of(...))` | [audit §package-hierarchy](../../proposals/artifact-boundary.md#package-hierarchy) | ≈1.5k Rust + ≈2k test + 212 reference lines; measured now: 1,065 session package production + 570 test lines, `with_registration` built only in tests | `decision needed`; clients are `zydeco show`, Cajun document links, and the structural merge key, so removal reworks tools and copy identity |
| A3 | Monadic-translation extensions: witness-dependent arrows, polymorphic value functions, applicative nominal families, value roots have only test clients | [audit §monadic-translation](../../proposals/artifact-boundary.md#monadic-translation-extensions) | ≈1.5k lines plus ≈45 fixtures; overlaps shared nominal machinery | `decision needed`; rejecting those payloads with `tyck.expressivity` preserves the closed translation boundary |
| A4 | Effect library breadth: no non-test module outside `lib/std/effect` imports it; one effect needs computation-type algebras | [audit §effect-library](../../proposals/artifact-boundary.md#effect-library-breadth) | ≈880 lines of library contracts in the language reference; two no-design-change fixes (`ret.zy` unused Builtin, seven `triv` definitions) | `decision needed`; current `lib/tests/monadic` and OOPSLA sources are the only visible clients |
| A5 | SPSLow validators: keep, type the IRs, or delete the check | [audit §validators](../../proposals/artifact-boundary.md#backend-translation-validators) | deleting validators + extraction ≈3.5k–4.2k; extraction alone feeds destructor tags | `decision needed`; re-verified present: `low/check.rs` 763, `high/check.rs` 562, protocol extraction 1,214 lines; validators + extraction are 2,304 non-test lines now (audit ≈2,050); the validators, the ZIR printer, and the `stack_protocols.rs` integration test read them, no backend does |
| A6 | `DESIGN.md` restates the references and has drifted | [audit §tooling-tests-docs](../../proposals/artifact-boundary.md#tooling-tests-and-documentation), [reference drift](../../todos/reference-drift.md) | ≈860 of 1,014 DESIGN lines at the measured revision | `open — reference gate`; the todo is still open |
| A7 | `docs/legacy` uses removed syntax | [audit §tooling-tests-docs](../../proposals/artifact-boundary.md#tooling-tests-and-documentation) | 2,013 lines in 3 files | `open — code only` (delete or archive outside the tree); re-verified 2,013 lines |
| A8 | Evaluation data in the artifact tree | [size §repository-weight](../../proposals/artifact-boundary.md#repository-weight-outside-rust) | 90,248 lines at `6aae43d0` | partially `resolved`: 31,647 tracked lines now |
| A9 | Retained after review (do not remove without new evidence): interpreter oracle, value functions/static elimination, recursive field search and `;` groups, `begin...end` with `that`, `pack`, `\|>`, field puns, `@(m)`, normalization, closure conversion, ZASM lowering, frame planning, copying collector, `@[typeof]`, derived identifiers | [audit §retained](../../proposals/artifact-boundary.md#retained-after-review) | — | `retained`; the SPS half adds high SPS and SPSLow |

## B. Implementation Findings

| # | Finding | Main evidence | Scale | Status at `e2dd8979` |
| --- | --- | --- | --- | --- |
| B1 | Per-judgment checker producer queries duplicate every typing rule; `no_eq` memos cannot survive an edit; `check_source` already memoizes the whole check | [audit §per-judgment](../../proposals/checker-query-retirement.md#per-judgment-checker-queries), [size §judgment-queries](../../proposals/checker-query-retirement.md#judgment-queries-duplicate-every-checker-rule) | ≈2.3k–2.8k lines; ≈30% end-to-end checking overhead recorded in the retention idea | `open — reference gate`; re-verified: `lang/statics/src/query/` 2,307 lines; 45 tracked producers in `lang/statics/src` marked `no_eq, unsafe(non_salsa_values)` (57 workspace-wide); 152 `unreachable!` in `lang/statics/src`. The size analysis also recommends removing the derived-identifier registry in favor of a sequential issuer (direct-allocation estimate ≈5k–6k) |
| B2 | Test-only generality: raw memory kernels (fused 64-bit load, ≤32 operations, same-type store), Cajun live-configuration parameter, `--target-arch` with one supported value | [audit §test-only](../../proposals/artifact-boundary.md#test-only-generality) | ≈650–900; ≈270 (Cajun); one option | `open — code only` for the kernels; `open — reference gate` for the flag. Re-verified: kernels live under `lang/stackir/src/low/scalar/`; `--target-arch aarch64` is accepted by the parser and rejected before compilation |
| B3 | Parallel implementations: existential-telescope walk ≥7 times; field projection through two near-identical candidate functions; `PackPi`/`ValPi` witness machinery; builtins implemented in interpreter, native stub, Node host plus a role table; annotation validation in scanner and desugarer; CLI/TUI diagnostic dispatch; 54 arena marker/accessor impls | [audit §parallel](../../proposals/artifact-boundary.md#parallel-implementations), [size §builtins](../../proposals/artifact-boundary.md#builtins-written-once-per-consumer) | ≈400; ≈500; 4,503 builtin lines (re-verified: `runtime/stub.rs` 1,264, `lang/statics/src/builtin.rs` 818, `lang/dynamics/{impls,builtin}.rs` 774+214, `lang/wasm-common/src/host.rs` 176, `cli/wasm/wasm-host.mjs` 829); ≈420; ≈600 | `open (retired study)`; the nested-witness discrepancy is tracked in [compiler boundaries](../../todos/compiler-boundaries.md#nested-witness-diagnostic) |
| B4 | Duplicate runtime digest/target computation on every `build -t exe` | [audit §defects](../../proposals/artifact-boundary.md#defects-found-during-the-audit) | two `.validate_*` calls around the `Exe` branch | `open — code only`; re-verified in `cli/src/main.rs` around the `BuildTarget::Exe` branch |
| B5 | `env!("CARGO_MANIFEST_DIR")` ties installed binaries to the build checkout | [audit §defects](../../proposals/artifact-boundary.md#defects-found-during-the-audit) | TUI engine and session prelude lookup | `open — code only`; re-verified: `tui/src/engine.rs:44`, `:410`, `:583`; `lang/session/src/source/prelude.rs:13` (production `DefaultPrelude::root`) |
| B6 | N-ary product inference refines unknown expected products to two components | [todos §n-ary](../../todos/compiler-boundaries.md#n-ary-product-inference-refinement) | one refinement helper | `open — reference gate`; tracked defect with accepted/rejected probes |
| B7 | Formatter fidelity: comment/layout retention drives parser capture and a large printer | [size §formatter](../../proposals/artifact-boundary.md#formatter-fidelity) | ≈2,900 implementation + ≈2,850 test lines (audit); `pretty.rs` 5,489 lines now | `decision needed`; canonical formatting without retention would be a fraction of the size |
| B8 | Test-harness multiplicity: six ways to write a source test, thirteen including every registration mechanism; 74 integration-test targets | [audit §tooling-tests-docs](../../proposals/artifact-boundary.md#tooling-tests-and-documentation), [size §tooling](../../proposals/artifact-boundary.md#tooling-at-product-standard) | tooling 15,433 production + 14,935 test lines at `6aae43d0` | `open (retired study)`; one harness per crate is the size-analysis direction |
| B9 | Formatter semantic-identity defect: `@[partial]` binders merge into the annotated header, so the printed form is accepted while the source rejects | Reproduced in [Card 01](cards/01-frontend.md) §10; tracked in [compiler boundaries](../../todos/compiler-boundaries.md#partial-annotation-scope-during-formatting) | one binder-header case plus a missing round-trip regression | `open — code only` (fix + regression); reference already owns the semantic-identity law in C15 |

## C. Structural Simplifications (Keep the Language and Pipeline)

| Direction | Estimate | Status | Depends on |
| --- | --- | --- | --- |
| Allocate directly in the checker; remove per-judgment queries and the derived-ID registry | ≈5k–6k | `decision needed` (approved query ownership changes) | B1 |
| Closed types: substitute aliases during elaboration; drop per-node environment tables | ≈0.8k–1.2k | `open (retired study)` | IR/elaboration decisions |
| Interpret the statics residual directly; delete dynamics syntax, linker, and printer | ≈0.85k | `decision needed` | IR boundary decision (D2) |
| Two front-end representations instead of three; resolve names in the same pass as desugaring | ≈2k | `decision needed` | C4 reference contract |
| Typed annotations: decode each meta annotation once during desugaring | ≈0.9k–1.2k | `open (retired study)` | metadata catalog |
| Plain arenas without scope schemas and access traits | not estimated | `decision needed` | C2 arena contracts |
| Plain recursion with `stacker::maybe_grow` instead of explicit frames | ≈1.5k + ≈1.2k test | `open — code only` (reference note needed) | none; re-verified: frames still present, `Work` variants grew 97 → 126, and no `maybe_grow` in `lang/`; re-estimate before planning |
| One host ABI table generating role signatures and stubs for interpreter, native runtime, and Wasm host | ≈2k | `decision needed` | C14 contract; give arithmetic an intrinsic body first |
| One test harness per crate | not estimated | `open (retired study)` | artifact boundary (D1) |

## D. Design Decisions for the Artifact

| # | Decision | Main question | Source |
| --- | --- | --- | --- |
| D1 | Artifact boundary | Which crates are the core workspace, and which move to extras or another repository? This comes first because it settles what every later removal must preserve. | [size §design-decisions](../../proposals/artifact-boundary.md#design-decisions-for-the-artifact) |
| D2 | IR boundary | Explicit core CBPV IR as static-elimination output, or interpret the statics arena directly? | same |
| D3 | Typed SPS IRs | Carry the calculi's types through high SPS and SPSLow, turning validators into IR type checkers? | same |
| D4 | Backend shape | Keep ZASM, or emit AMD64 from SPSLow with frame planning? Fixed slots would remove most frame planning. | same |
| D5 | Surface area | Adopt the audit convention; unify synonym spellings; decide `.zyi`, `@[doc]`, and text blocks. | same |
| D6 | Packages | Keep them (std needs them; dependent elimination is the clearest CBPV-as-IR demonstration); merge the parallel witness/telescope machinery. | same |
| D7 | Monadic translation | Keep as the CBPV-to-CBPV demonstration, restricted to what std uses. | same |
| D8 | Numerics | Keep `Int`, `Float64`, `Char`, `String`, `Bool`, `Unit` only; collapses primitive/scalar tables, ten numeric packages, and host copies. | same |

## E. Verified Deltas Since the Evaluations

| Item | Evaluation | `e2dd8979` |
| --- | --- | --- |
| Abstract-machine Wasm backend | listed as removable | removed in `5e4f4e46` |
| ZASM interpreter | listed as removable | removed in `36a60594` |
| Session whole-program test pipeline | listed as removable | removed in `29a7f107`, `ef7d1e6c` |
| Typed-arena lint and re-derivation | ≈2.5k lines | replaced by a test-only invariant check; `Location::caller()` in `lang/` is 0 |
| `docs/evaluations` tracked lines | 90,248 | 31,647 |
| Production Rust (same line accounting) | 93,400 | 88,091 |
| `lang/statics` production / `lang/stackir` production | 31,208 / 10,420 | 26,327 / 12,786 |
| Integration-test targets | 72 (retired size analysis), 74 (retired audit) | 74 |
| `.zyi` files, `@[doc]` uses | 0, 0 | 0, 0 |
| Viable native target architectures | one (AMD64) | one (AMD64); `aarch64` parses then rejects |
| Representation policies (`boxed/direct/local/shared`) | retired audit row dropped after verification; kept | kept and selectable through `build --representation`; default `Local` (native card re-check) |

## F. Surface-Form Counts at `e2dd8979`

From the retired study's surface-form census on a clean `git archive` of `HEAD` (629 sources: 601 `.zy`,
0 `.zyi`, 28 `.zydeco`):

| Form | Uses | Gate |
| --- | --- | --- |
| `define` alias | 0 | `open — reference gate` (delete alias) |
| `comatch` alias | 0 | `open — reference gate` (delete alias) |
| `sigma` binder | 4, all compiler cases | `decision needed` (synonym for `exists`/`*`) |
| `<\|` backward application | 7 in 6 files (std 1, lib tests 2, cases 4) | `decision needed` |
| `~>` view patterns | 16 in 6 files (lib tests 11, cases 5) | `decision needed`; ≈130 match arms/refs in 9 crates |
| `param ... that` placement | 45 in 30 files (std 15, lib tests 17, cases 7, docs 6) | `decision needed` |
| `.zyi` companions | 0 files | `decision needed` (also retires the separate `.zydeco` extension) |
| `@[doc]` annotation | 0 | `decision needed`; only consumer of `--|` blocks |
| `@(literal)` splice | 1 | `decision needed` |
| `--\|` text blocks | 53 (OOPSLA 48, lib tests 5) | tied to `@[doc]` |
| `let val` / `param val` | 273 in 117 files | `retained` (value functions are used by std) |
| static integer intrinsics | 22 (std 19, cases 3) | tied to A1 (memory) |

## G. Drift Found During This Pass (Now Recorded in `docs/todos/`)

These surfaced while writing the cards and are now recorded in [docs/todos](../../todos/README.md):
G1 and G2 in the new [tooling boundaries](../../todos/tooling-boundaries.md), G3,
G5, G6, G7, G9, and G10 in [reference drift](../../todos/reference-drift.md),
and G4 and G8 in [compiler boundaries](../../todos/compiler-boundaries.md).
The table keeps the study cross-reference; the todo entries own the repairs.

| # | Drift | Evidence | Owner that should record it |
| --- | --- | --- | --- |
| G1 | Duplicate runtime digest and platform computation on every `build -t exe` | `cli/src/main.rs:499-514` (both `LibraryDigest::runtime` and `LibraryPlatform::for_target` called twice); the [retired audit's defects section](../../proposals/artifact-boundary.md#defects-found-during-the-audit) says it belongs in todos | `docs/todos/tooling-boundaries.md` |
| G2 | `env!("CARGO_MANIFEST_DIR")` ties installed binaries to the build checkout | Production use at `tui/src/engine.rs:44` and `lang/session/src/source/prelude.rs:13`; `tui/src/engine.rs:410,583` are test fixtures | `docs/todos/tooling-boundaries.md` |
| G3 | Two reference sentences still say "all four backends" after the abstract-machine Wasm backend was removed | C14 line ~3076 and [L14 foreign decoding](../../references/language.md#foreign-decoding) line ~2407; current registrations are interpreter, AMD64, `wasm-sps` (`lang/tests/tests/builtin.rs:77`, `lang/tests/tests/ffi_examples.rs:165`) | `docs/todos/reference-drift.md` |
| G4 | The checker driver's non-query source lifecycle may be unused after the coarse query took over | `lang/statics/src/check/driver.rs:76,124,131` re-implemented by `lang/statics/src/query/source.rs:152-190`; no in-tree caller found | `docs/todos/compiler-boundaries.md` |
| G5 | `cli/wasm/wasm-numeric.mjs` comment claims the decimal spelling is shared with interpreter/native builtins; it is a cross-language copy | Comment at `wasm-numeric.mjs:1`; imported only by the Node host and `wasm_float_text` | `docs/todos/reference-drift.md` |
| G6 | The dynamics crate guide omits `host`, `foreign`, `memory`, and `representation` modules added later | `lang/dynamics/src/README.md` vs `lang/dynamics/src/lib.rs:8-28` | `docs/todos/reference-drift.md` |
| G7 | A load-bearing `\x01` prefix on exported host symbols suppresses the Mach-O underscore, but no reference explains it | `runtime/stub.rs` and `lang/machine/build.rs` exports; `nm` shows bare `zydeco_frame_step` | `docs/todos/reference-drift.md` |
| G8 | Emitter hits `todo!()` for more than six host arguments; the role table caps at five, so it is unreachable but unguarded | `lang/amd64/src/emit.rs:936`; `lang/syntax/src/lib.rs:738` | `docs/todos/compiler-boundaries.md` |
| G9 | C10's `--representation` sentence names ZASM, assemblies/executables, and Wasm but the CLI also applies the strategy to library/unit lowering | `cli/src/compile.rs:142,163`; C10 policy-selection section | `docs/todos/reference-drift.md` |
| G10 | The audit says Cajun completions surface package relationships; in this checkout only document links consume `relations`, while completion reads roles/options from the metadata catalog | `editor/cajun/src/document_links.rs:76-114` vs `completion.rs:160-180`; [audit §package hierarchy](../../proposals/artifact-boundary.md#package-hierarchy) | `docs/todos/reference-drift.md` |

## Reproduction

All current-status figures above can be re-derived with read-only commands.
The retired studies' own figures are dated; their reports and reproduction scripts left the tree
on 2026-11-05, and the measurement method plus the current baseline are recorded
in [Where the weight is](../../proposals/artifact-boundary.md#where-the-weight-is).

```sh
# Clean revision, no build artifacts
mkdir -p target/study/archive && git archive HEAD | tar -x -C target/study/archive

# Re-derive one candidate directly; record the command, revision, and scope
git grep -n '<public name>' HEAD -- '*.rs'

# Spot checks used for statuses
ls lang/statics/src/query | wc -l
grep -rn 'no_eq' lang | wc -l
grep -rn 'unreachable!' lang/statics/src | wc -l
wc -l lang/stackir/src/low/check.rs lang/stackir/src/high/check.rs
grep -rn 'maybe_grow' lang | wc -l
grep -rn 'CARGO_MANIFEST_DIR' tui lang/session
wc -l docs/legacy/*/* 2>/dev/null | tail -1
git ls-files '*.zyi' | wc -l
```

The evaluation scripts themselves are the authority on their methods and limits;
read [the audit's limits](../../proposals/artifact-boundary.md#limits)
and [the size analysis's limits](../../proposals/artifact-boundary.md#limits) before citing numbers to the owner.
