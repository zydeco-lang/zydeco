# Zydeco Re-Onboarding Manual

> **Status: temporary scratch record.** This directory lives under `docs/logs/` per
> [AGENTS.md](../../../AGENTS.md#documentation-structure-and-design-records) and is not canonical
> documentation. Its durable content should be folded into the owning references or deleted once it
> has served its purpose. It documents the snapshot `e2dd8979` (2026-10-30) and will drift.

## What This Is

A structured way to re-learn most of the Zydeco repository from scratch, aimed at the owner's stated goal:
*review past features, then remove redundant code and features.*

It exists because the repository's documentation is organized for lookup,
not for a first pass: the references own ~6.5k lines of canonical rules,
the compiler has 16 implementation chapters, and `lang/` holds 14 crates.
Reading linearly is slow and does not build the one mental model that matters:
**which representation exists at each pipeline stage, and what it costs.**

The manual has four layers:

1. **A map** of crates, phases, products, and references ([01](01-map.md)).
2. **A vertical slice** that runs one program through every backend
   and annotates the real output ([02](02-vertical-slice.md)).
3. **Six module cards**, one per subsystem, each with entry points, invariants, commands,
   checkpoint questions, and evidence-based cleanup signals ([cards/](cards/)).
4. **A cleanup workbench**: method ([04](04-cleanup-lens.md)),
   candidate inventory ([appendix](appendix-cleanup-candidates.md)), and self-test ([05](05-question-bank.md)).

## Suggested Use

**If you have two hours:** read [01](01-map.md), run [02](02-vertical-slice.md), skim the cards' tables.
You will know where everything lives and how a program becomes an executable.

**If you have a day:** add the cards in pipeline order (`01`→`06`), answer the checkpoint questions without notes,
and keep the glossary ([03](03-glossary.md)) open for lookups.

**If you are starting the cleanup campaign:** finish the day pass,
then read [04](04-cleanup-lens.md) and the [appendix](appendix-cleanup-candidates.md),
and work [05 — Question Bank](05-question-bank.md) section "Cleanup decisions".
The first concrete deliverable should be a decision memo for the artifact boundary and the checker-query removal,
not a patch.

The loop for every module is:
read the card → run its verification commands → answer its questions → name one cleanup signal
and find its clients → then decide whether the signal survives.

## Contents

| Chapter | Purpose | Lead |
| --- | --- | --- |
| [01 — Project Map](01-map.md) | Crates, products, pipeline, documentation map, current weight | Lead |
| [02 — Vertical Slice](02-vertical-slice.md) | One program through check, interpreter, ZIR, ZASM, AMD64, exe, Wasm, pass tracing | Lead |
| [03 — Glossary](03-glossary.md) | Working definitions for CBPV, compiler, and IR vocabulary | Lead |
| [04 — Cleanup Lens](04-cleanup-lens.md) | Yardstick, convention, candidate classes, verification protocol, suggested order | Lead |
| [05 — Question Bank](05-question-bank.md) | Self-test by layer plus decision questions | Lead |
| [Appendix — Candidate Inventory](appendix-cleanup-candidates.md) | Per-finding status, estimates, evidence, reproduction commands | Lead |
| [Card 01 — Front End](cards/01-frontend.md) | `lang/syntax`, `lang/surface`, parsing→resolution, formatter | frontend-card |
| [Card 02 — Statics](cards/02-statics.md) | Checker, inference, elaboration, queries, arenas | statics-card |
| [Card 03 — Stack IR](cards/03-stackir.md) | High SPS, normalization, closure conversion, SPSLow, protocols, passes | stackir-card |
| [Card 04 — Native Backend](cards/04-native-backend.md) | ZASM, stack/frame analysis, AMD64 emission, runtime packaging | native-card |
| [Card 05 — Execution Backends](cards/05-execution.md) | Reference interpreter, Wasm backend and host | execution-card |
| [Card 06 — Tooling](cards/06-tooling.md) | Session, CLI, TUI, Cajun, test harness, xtask | tooling-card |

Each card follows the same template: responsibility, pipeline position, entry points, invariants, data flow,
verification commands, reading order, checkpoint questions, cleanup signals, and uncertainties.

## Ground Rules

- **Use the checkout's CLI.** The installed `zydeco` on `PATH` is stale and rejects current `lib/std/builtin.zy`.
  Run `cargo build --bin zydeco` and use `target/debug/zydeco`, or `cargo run --quiet --bin zydeco --`.
- **Native targets need AMD64.** On this arm64 host pass `--target-arch x86-64` for `asm`,
  `exe`, and library targets; execution uses Rosetta.
  The interpreter and `wasm-sps` need no flag.
- **The WIP is committed.** The short-circuit tests and the three proposals that were uncommitted
  while this manual was written landed in `6b0aafcd`; read the proposals as work in progress design, not settled design.
  The tree is clean at the manual's publication.
- **The source studies are retired.** The 2026-10-21 cleanup audit
  and 2026-10-25 size analysis left the tree on 2026-11-05.
  Their remaining decisions live in [the artifact boundary proposal](../../proposals/artifact-boundary.md)
  and [the checker query proposal](../../proposals/checker-query-retirement.md), drift lives
  in [docs/todos](../../todos/README.md), and this manual's old citations now point at the distilled proposal sections.
  Treat all figures as dated snapshots.
- **Scratch, not approval.** Nothing here authorizes a removal or a design change.
  Approved design lives in [docs/references](../../references/README.md); open drift lives
  in [docs/todos](../../todos/README.md).
- **Commands are non-destructive.** Every chapter uses `check`, `run`, `build`, `passes`, `show`, and read-only scripts.
  Build output goes under `target/` (ignored).

## Maintenance

- When the snapshot moves, update the snapshot block in [01](01-map.md#snapshot), re-run the verification commands
  in [02](02-vertical-slice.md), and re-check the status column in the [appendix](appendix-cleanup-candidates.md).
- When a cleanup finding is resolved, mark it `resolved` with the commit in the appendix rather than deleting the row;
  the history is useful for the next pass.
- Before folding or deleting this directory, extract anything durable into the reference section
  that owns it (a decision rationale or a settled contract), and confirm the extraction with the owner.

## Sources

The structure follows the repository's own organization:
the [documentation index](../../README.md), the [compiler reference](../../references/compiler.md),
the [language reference](../../references/language.md), and the two retired 2026-10 studies,
distilled into [Artifact boundary and mainline admission](../../proposals/artifact-boundary.md)
and [Checker query retirement](../../proposals/checker-query-retirement.md).
Module cards cite the code they describe; the references remain the authority.
