# 04 — Cleanup Lens

> Scratch study note for re-onboarding. The two evaluations cited here explicitly call their
> findings **proposals, not decisions**. Removing or changing approved design needs the owner's
> approval under [AGENTS.md](../../../AGENTS.md#documentation-structure-and-design-records).
> This chapter organizes the cleanup question; it does not authorize removals.

## Why Cleanup Needs a Yardstick

A research prototype accumulates features at different times for different reasons. Without a
criterion, "redundant" collapses into "I would have written it differently", and removal becomes
taste. The repository already supplies a criterion. The
[cleanup audit](../../evaluations/2026-10-21-cleanup-audit/README.md#yardstick) derives it from the
two papers that the source and compiler references implement:

- The OOPSLA artifact: relative monads model stack-manipulating computation; monadic blocks
  reinterpret CBPV code with any relative monad; monadic blocks derive transformers; plus
  term-oriented composition.
- The SPS paper: the SPS and `SPS_l` calculi, their lowering, β-simplification, closure conversion
  with existential packages, and lowering to executable code. `lang/stackir` implements both.

Everything else — tooling, fidelity, incrementality, packages, FFI, manual memory, numerics — is
judged by whether it serves one of those demonstrations, or by whether a maintained client needs
it. The [size analysis](../../evaluations/2026-10-25-size-analysis/README.md#yardstick) applies the
same yardstick to the lines that no feature removal reaches.

The owner's stated goal — "review past features and remove redundant code and features" — is
exactly this question. Two consequences follow:

1. **Not all unused-looking code is removable.** The SPSLow validators look like scaffolding but
   are the pipeline's only check that its `SPS_l` output is well typed; deleting them is a design
   choice about how to check the IR, not housekeeping. See
   [backend translation validators](../../evaluations/2026-10-21-cleanup-audit/README.md#backend-translation-validators).
2. **Not all removals are equal.** Deleting a test-only feature saves its code; replacing an
   incremental-query layer with direct allocation saves more and changes no source behavior.
   The [candidate inventory](appendix-cleanup-candidates.md) separates the classes.

## The Convention That Falls Out

The audit states a convention and notes that adopting it is itself a design decision
([recurring pattern](../../evaluations/2026-10-21-cleanup-audit/README.md#recurring-pattern)):

> A language form, standard-library module, compiler pass, or CLI option stays on the main line
> only when something other than its own tests uses it — std, the OOPSLA artifact, the SPS paper's
> compiled pipeline and its evidence, a demo, or a maintained tool path.

Work without such a client lives on an experiment branch or in a proposal until one appears.
A removal that displaces a tool or library client must rework that client in the same change.
This is the operational rule to apply when a candidate looks tempting: **name the client first**,
then count lines.

## Current Snapshot, Not the Evaluations' Snapshot

The evaluations measured `7e5ee4c1` (2026-10-21) and `6aae43d0` (2026-10-25). Several findings have
since been resolved, so re-verify before spending time. At `e2dd8979` this manual re-checked the
following.

Resolved since the evaluations:

- The abstract-machine Wasm backend was removed in `5e4f4e46` ("remove the abstract-machine Wasm backend").
- The ZASM interpreter was removed in `36a60594` ("remove the ZASM interpreter").
- The session's whole-program test pipeline was removed in `29a7f107` and `ef7d1e6c`.
- The typed-arena lint and its re-derivation were replaced by a test-only invariant check; current
  `Location::caller()` count in `lang/` is 0 (the size analysis measured 404).
- `docs/evaluations/` tracked lines fell from 90,248 at `6aae43d0` to 31,647 now; the size analysis
  finding about measurement data in the artifact tree is partly resolved.
- Production Rust fell from 93,400 lines at `6aae43d0` to 88,091 now (same script, current
  `git archive`; see [01 — Project Map](01-map.md#where-the-weight-is)).

Still present and worth re-verifying candidate by candidate:

- The per-judgment checker query layer: `lang/statics/src/query/` is 2,307 lines; 45 tracked
  producer functions carry `no_eq, unsafe(non_salsa_values)` in `lang/statics/src` (57 workspace-wide);
  `unreachable!` count in `lang/statics/src` is 152 (203 at the evaluation).
- Explicit reconstruction frames (126 `Work` variants now, 97 at the evaluation): `lang/stackir/src/high/lower/fold.rs`,
  `high/normalize/fold.rs`, and `low/convert/fold.rs` still define the `Work` discipline; no
  `stacker::maybe_grow` call exists in `lang/`.
- SPSLow validators: `lang/stackir/src/low/check.rs` 763 lines, `high/check.rs` 562 lines,
  protocol extraction 1,214 lines (`protocol.rs` + `source.rs` + `agreement.rs`; 1,587 with the dedicated test modules); the validators
  and the ZIR printer read the descriptors, and `lang/tests/tests/stack_protocols.rs` is the only
  integration test that reads the tables — no backend consumes them.
- Dead high-SPS/SPSLow site maps: `pats`/`vpats` storage now lives in the shared `AdminArena`
  (`lang/stackir/src/arena.rs:43-46`), with phase aliases in the high/low arena modules; the
  readers only propagate the maps to the next phase.
- Tooling defects: duplicate runtime-digest computation in `cli/src/main.rs` around the `Exe`
  branch; `env!("CARGO_MANIFEST_DIR")` in `tui/src/engine.rs` and
  `lang/session/src/source/prelude.rs`; `--target-arch` still accepts `aarch64` only to reject it
  downstream; `RUST_MIN_STACK = 16 MiB` in `.cargo/config.toml`.
- Surface-form candidates: `.zyi` files 0, `@[doc]` uses 0, `define`/`comatch` aliases 0,
  `@(literal)` 1 use, `--|` blocks 53, `~>` 16, `<|` 7 (current-tree counts from
  `docs/evaluations/2026-10-21-cleanup-audit/usage.py`; see [appendix](appendix-cleanup-candidates.md#f-surface-form-counts-at-e2dd8979)).

## Candidate Classes

The inventory lists individual items; this is the shape of the campaign.

**A. Deletions that change no design.** Dead code, test-only interfaces, and raw memory kernels.
The compiler reference text may still need amendment, but no source language behavior changes.
These are the cheapest wins and the right warm-up.

**B. Structural simplifications.** Keep the language and pipeline, change how the pipeline is
written. The size analysis lists ten, including direct allocation in the checker (≈5k–6k lines),
closed types (≈0.8k–1.2k), interpreting the statics residual instead of copying it into
`lang/dynamics` (≈0.85k), two front-end representations instead of three (≈2k), typed annotations
(≈0.9k–1.2k), plain arenas, recursion with `stacker::maybe_grow` (≈1.5k production plus ≈1.2k test),
one host ABI table (≈2k), and one test harness per crate. Estimates overlap; they are not additive.

**C. Design decisions.** The artifact boundary (which crates are core versus extras), the IR
boundary (a core CBPV residual IR versus interpreting the statics arena), typed SPS IRs, the
backend shape (ZASM versus direct AMD64), surface-area adoption, package scope, monadic translation
scope, and numeric scope. Each changes approved reference text and needs a decision before work
starts.

**D. Weight outside the pipeline.** Documentation duplication (`DESIGN.md` restating references;
legacy notes), evaluation data in the tree, test-harness multiplicity, and tooling built at product
generality for one user. Cheap to reason about, but removal reworks clients and references.

The [suggested order](#suggested-order-for-this-campaign) at the end of this chapter sequences them.

## Verification Protocol Before Proposing a Removal

Run this for every candidate. It is derived from the audit's reproduction limits
([limits](../../evaluations/2026-10-21-cleanup-audit/README.md#limits)) and this repository's
working principles.

1. **Work from a clean, isolated state.** The tree is dirty with in-progress proposals. Use a git
   worktree or branch so the candidate diff stays reviewable. Confirm `git status` before measuring.
2. **Find every client, not every occurrence.** Search tracked sources, std, `lib/`, docs, and
   generated artifacts for the candidate's public name and its behavior. Then classify: semantic
   use, incidental mention, or only its own tests. A syntactic count is not a dependency.
3. **Re-derive current figures.** Do not quote the evaluations' line counts as if current. Run the
   scripts on a clean archive of the current revision:
   ```sh
   mkdir -p target/study/archive && git archive HEAD | tar -x -C target/study/archive
   python3 -I docs/evaluations/2026-10-21-cleanup-audit/usage.py target/study/archive
   python3 -I docs/evaluations/2026-10-25-size-analysis/measure.py target/study/archive
   ```
   At `e2dd8979`, `measure.py` prints the crate table and then fails in `report_support`
   (`measure.py:229`, which still names the removed `lang/surface/src/textual/ugly.rs`); use only
   the crate table and record the failure rather than trusting stale support numbers.
4. **Classify the gate.** Does the removal change approved reference text (approval required)? Does
   it change source behavior (tests and docs required)? Does it touch the SPS compiled path or the
   OOPSLA artifact (the paper claims must stay true)? Is a tool or std client displaced (rework it
   in the same change)?
5. **Plan the whole change.** Code, callers, references, fixtures, and obsolete tests move together.
   [AGENTS.md](../../../AGENTS.md#working-principles) requires updating callers and removing the
   superseded path in the same change; compatibility layers need explicit authorization and a
   removal condition.
6. **Validate in a ladder.** Focused crate tests and `zydeco check` on fixtures; then
   `cargo test-resume`; full `cargo test-all --release` only for a finished change, since it is
   expensive and native end-to-end tests may fetch runtime dependencies. Keep the interpreter as
   the oracle: every backend change must preserve interpreter/native/Wasm agreement.
7. **Record the outcome where it belongs.** Settled rationale moves to the owning reference;
   observed drift to `docs/todos/`; unstarted design to `docs/proposals/`. Delete the scratch record
   when its durable content has moved.

## Traps

- **The evaluations' deltas overlap.** ≈ estimates are inferred from file sizes and are not
  measured deletions. Do not sum them; the 40k–50k projection in the size analysis compounds them.
- **Usage counts measure syntax, not semantics.** One occurrence may represent a whole library
  path; zero occurrences may mean the feature is reached through generated code.
- **"Decision needed" is not a blocker for study.** Understanding a candidate is useful even if
  the owner declines it. Label it correctly so the decline is recorded, not rediscovered.
- **The compiled path has a claim to keep true.** Any change under `lang/stackir`, ZASM, or the
  native backend must keep the SPS paper's implementation claim true (lowering, β-simplification,
  closure conversion, lowering to executable code). The
  [size analysis suggested order](../../evaluations/2026-10-25-size-analysis/README.md#suggested-order)
  puts the artifact-boundary decision first for this reason.
- **Validation is not free.** `cargo test-all` is CPU-intensive; native tests need AMD64 support;
  Wasm tests need Node.js 24. Use the [resumable runner](../../../CONTRIBUTING.md#iterate-with-resumable-tests)
  while iterating, and plan one full run per finished change.
- **Do not treat this manual as approval.** Everything under `docs/logs/` is scratch. The durable
  home of a decision is the reference section that owns it.

## Suggested Order for This Campaign

The audit and size-analysis orders agree on the first moves; interleaving them gives:

1. **Decide the artifact boundary.** Which crates/directories are core versus extras determines
   what every later removal must preserve. This is a design decision, not a code task.
2. **Apply deletions without design change.** Raw memory kernels, dead site maps, leftover
   scaffolding; pair with reference-text updates.
3. **Remove the unconsumed producer queries** and allocate directly in the checker. Behavior stays
   the same, but C3/C5 query-ownership text changes and needs approval.
4. **Replace explicit reconstruction frames** with recursion plus `stacker::maybe_grow`; this also
   protects the printers and protocol checks that still recurse today.
5. **Merge per-consumer implementations**: one host ABI table for interpreter, native stub, and
   Node host; give arithmetic an intrinsic body; unify the parallel witness/telescope machinery.
6. **Consolidate tooling and tests** only after the boundary decision says they are extras.
7. **Then the design decisions**: IR boundary, typed SPS IRs, backend shape, surface area,
   packages, monadic translation, numerics.

A useful first deliverable is not a removal but a **decision memo** for steps 1 and 3, since both
gate the largest savings and both need approval. The [question bank](05-question-bank.md) includes
questions that a good memo must answer.

## See Also

- [Candidate inventory](appendix-cleanup-candidates.md): per-finding table with status and evidence.
- [01 — Project Map](01-map.md): current crate sizes and where each subsystem lives.
- [Retained after review](../../evaluations/2026-10-21-cleanup-audit/README.md#retained-after-review):
  parts the audit judged essential; do not re-litigate them without new evidence.
