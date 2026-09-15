# AGENTS

Guidance for automated assistants working in this repository.
`Prefer` and `consider` mark defaults that allow task-specific judgment; other directives are requirements
within their stated scope.

Zydeco is a call-by-push-value language prototype implemented as a Rust workspace under `lang/`.
Libraries and reusable examples live under `lib/`; regression and end-to-end projects live under `lib/tests/`.

## Language and Terminology

Use English for identifiers and repository prose, including comments and documentation.
Communicate in the user's preferred language, following the current conversation when none is stated.
Keep established technical terms in English when that preserves precision and searchability,
unless the user requests localization.
Use **runtime metadata** for information carried with a runtime representation, such as lengths or vtable pointers.
Call compiler `@[meta]` forms **meta annotations**, or **compile-time metadata**, to distinguish the two concepts.

## Project References

Read the relevant sections for the task:

- [README.md](README.md) for usage and quick start.
- [References](docs/references/README.md) for canonical language and compiler design.
- [DESIGN.md](DESIGN.md) for the project overview and repository layout.
- [CONTRIBUTING.md](CONTRIBUTING.md) for build, formatting, test, and contribution workflows.
- [OOPSLA artifact overview](lib/tests/oopsla/README.md) when working on those examples.

Keep documentation examples consistent with the repository's CLI flags and scripts.

## Working Principles

- When recurring cases support a shared rule, state it and suggest the abstraction or convention that follows.
  Distinguish evidence from inference, and favor connections that simplify future decisions.
  Do not invent abstractions merely to claim novelty.
- When replacing a design, update its callers and remove the superseded path in the same change.
  Retain compatibility layers only when the user explicitly requests a compatibility boundary;
  state its scope and intended removal condition.
- For new or substantially changed compiler passes,
  follow [traversal selection](docs/references/compiler.md#choosing-and-composing-traversals)
  and [resumable execution](docs/references/compiler.md#resumable-folder-execution).
  Reuse shared traversals, keep analyzers independent, and prefer `Explicit` folders for suitable reconstruction.
  Explain a different execution model when those interfaces do not fit.
- When changing validation or behavior with rejection cases, pair valid inputs with rejected counterparts.
  Assert the intended error and relevant failure invariants.
  Retain bug reproducers as regression tests.
- When adding example projects under `lib/`, consider wiring them into `lang/tests`.

## Rust Conventions

Register every Rust dependency in the root `Cargo.toml` under `[workspace.dependencies]`;
crates refer to it with `dependency = { workspace = true }`.

Represent semantic states, internal messages, and errors with domain types.
Parse external text into structured data early; use strings when the data itself is textual.
Introduce types as needed. Before using a string map, distinguish text storage or interning
from structured data better modeled with structs or traits.

Prefer iterator transformations such as `map`, fallible `collect`, `unzip`, and `fold` over mutable accumulators.
Keep mutation for essential sequential state or when a functional rewrite would materially harm clarity or performance.

Group functions on structs as methods or associated functions.
Use free functions only when required by the language, a macro, or an external interface.
Use `self` when consuming a value, `&self` or `&mut self` when borrowing it, and associated functions
when the struct serves as a namespace.

Prefer direct struct construction unless a constructor performs additional work or establishes a visibility boundary.
Do not add a lone `new` method that merely repackages a struct literal.
Name general constructors `new` and constructors indicating how a value is created `with_*`.

Give builders an associated entry point `fn new(required, ...)`.
Choose receivers according to whether `build` or `finish` moves owned fields:

- For consuming builders, use `fn build(self) -> T` and setters `fn with_*(mut self, ...) -> Self`.
- For builders that can borrow, prefer `fn build(&self) -> T` and setters `fn set_*(&mut self, ...) -> &mut Self`.

Use `with_*` and `set_*` consistently for optional configuration.
Prefer `derive` and `derive_more` when generated behavior exactly matches the intended semantics;
write manual implementations only when additional invariants require them.

## Validation and Formatting

Run focused tests for the affected crate or test target while iterating.
Run the full workspace suite (`cargo test-all`, including flag variants) only when the user explicitly requests it:
it is CPU-intensive, and native end-to-end tests may fetch runtime dependencies from crates.io.
`--all-targets` excludes doctests; when changing doctests, run them separately with `cargo test-doc-all`.

Format Rust with `cargo fmt --all`; lint with `cargo clippy-all`.
For a repository-wide Zydeco formatting pass, supply all tracked source files explicitly:

```sh
git ls-files -z '*.zy' '*.zyi' '*.zydeco' | xargs -0 cargo run --quiet --bin zydeco -- fmt
```

`cargo fmt` does not format Zydeco embedded in Rust string literals;
`zydeco fmt` only processes explicitly supplied files.
After a repository-wide Zydeco formatting pass, run the affected Rust tests and update embedded source fixtures
and expected output snapshots as needed.

## Documentation Structure and Design Records

[docs/README.md](docs/README.md) indexes the documentation:

- `docs/references/`: the sole authoritative home for all user-approved canonical design decisions,
  including language semantics, compiler architecture, and invariants.
- `docs/proposals/`: concrete design documents for ongoing features not yet implemented.
- `docs/ideas/`: higher-level academic discussions, research questions, and conceptual exploration.
- `docs/evaluations/`: dated experimental reports and their reproducible evidence, with one directory per study.
- `docs/todos/`: urgent codebase drift from the references.
  Link the governing rule, describe the discrepancy, and address it as soon as possible.

Adopting, changing, or promoting a canonical design requires user permission; existing authorization suffices.
Once a feature is implemented, move its approved, settled design into the references.
Keep only ongoing, unimplemented design work in proposals.

Maintain **one home per canonical rule**, in the reference section that owns its semantic boundary.
Outside references, prefer concise pointers over repeated design explanations.
Keep dependent examples and links consistent with the canonical rule.
When moving or merging content, remove superseded sections or files and update incoming links in the same change.

Scratch records in `docs/logs/` are temporary; fold durable content into its proper home, then delete the log.

## Documentation Style

Establish motivation and context before introducing machinery; explain which question each mechanism answers.
Introduce concepts before relying on them, and let information density rise gradually.
Connect paragraphs with reasoning that explains the next step, especially before increasing technical detail.
Use programming-languages terminology where precise, explaining specialized terms at first use
with source-level intuition or a concrete example.

Prefer positive explanations; use contrast when the distinction matters.
Let paragraph and sentence lengths follow the argument, avoiding formulaic rhythms and repeated contrastive phrasing.
Use parallel construction when it clarifies a comparison, enumeration, or invariant.
After revising, read the document from beginning to end to check its progression and concept order.

Wrap prose around 90–120 characters, breaking after sentence or clause punctuation, or before connectives.
Treat commas as ordinary clause boundaries. Do not enforce one sentence per line.
Keep complete sentences together when they fit; allow short final lines instead of splitting phrases to balance widths.
Preserve code blocks and other structural Markdown.
Reflow edited Markdown with `python3 docs/scripts/reflow-markdown.py path/to/document.md`; reflow Typst manually.

Capitalize titles in `docs/references/` with Chicago title case: capitalize most words,
including both parts of hyphenated compounds; keep short function words (a, an, the, and, or, of, in, to,
as, with) lowercase unless first; preserve code terms' exact spelling regardless of position (`SPSLow`,
`Ret`, `ZASM`, `AMD64`).

## Commit Messages

Use `prefix: lowercase description`, on one line with no trailing period.
Describe what changed. For `feat`, name the capability directly; use a leading `add` only when it improves clarity.

| Prefix | When to use |
|--------|-------------|
| `feat` | A new user-visible capability. |
| `incr` | Incremental progress: fixes, polish, tuning, or small feature additions. |
| `sisy` | Mechanical changes or internal restructuring with no behavior change. |
| `vibe` | Exploratory or prototype work that may be revised or replaced. |
| `repo` | Repository housekeeping, dependencies, migrations, or file reorganization. |
| `docs` | Documentation-only changes, including Rust docs and comments. |
| `test` | Test changes without production code changes. |
