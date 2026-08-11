# AGENTS

This file is guidance for automated assistants working in this repository.

## Language and Terminology

Use English for identifiers and repository prose, including code comments and documentation.
Communicate with users in their preferred language;
when no preference is stated, follow the language of the current conversation.
Keep established technical terms in English when that preserves precision and searchability,
unless the user asks for translated or localized terminology.

## Working Principles

### Extract reusable rules

Look for recurring shapes in syntax, architecture, failures, and design decisions while working.
When several observations support one explanation, state the shared rule
and suggest the abstraction or convention that follows from it.
Distinguish evidence from inference, and prefer connections that simplify future decisions.
A genuinely surprising observation is valuable when it compresses several facts into one useful principle;
do not manufacture novelty for its own sake.

### Preserve a reviewable design record

As substantial exploratory or design-heavy work converges,
propose formalizing it as a Markdown or Typst review document.
The proposal should identify the questions that the document would make easier to review, such as the problem,
constraints, alternatives, chosen invariants, representative examples, and remaining uncertainty.
Propose this artifact rather than creating it automatically, and skip the suggestion for small routine changes.

### Prefer direct transitions

Treat a clean transition to the intended design as the default.
When replacing a design, update its callers and remove the superseded path in the same change.
Do not retain deprecated APIs, adapters, dual representations, migration parsers,
or other compatibility layers unless the user explicitly requests a compatibility boundary.
When compatibility is required, make its scope and intended removal condition explicit.

## Adding a Dependency

All Rust dependencies are managed in the top-level `Cargo.toml` file, under `[workspace.dependencies]`.
All crates then use `dependency = { workspace = true }` to refer to
the workspace-registered dependencies in their own `Cargo.toml`.

## Project Snapshot

- Zydeco is a proof-of-concept programming language based on call-by-push-value.
- The implementation is a Rust workspace with multiple crates under `lang/`.
- The standard library and reusable examples live under `lib/`;
  regression and end-to-end projects live under `lib/tests/`.

## Key Docs

- `README.md`: top-level usage and quick start.
- `DESIGN.md`: language model, translation pipeline, and limitations.
- `CONTRIBUTE.md`: build/test workflows and contribution notes.
- `lib/tests/oopsla/README.md`: artifact overview and detailed examples.

## Common Workflows

Build the CLI:
```sh
cargo build --bin=zydeco --release
```

Format the codebase:
```sh
cargo fmt --all
```
Lint the codebase:
```sh
cargo clippy-workspace
```

Run a Zydeco program:
```sh
zydeco run path/to/main.zy
```

Run focused tests for the affected crate or test target while iterating.
Do not run `cargo test-workspace` as a routine verification step: it is CPU-intensive,
and its native end-to-end tests may also fetch runtime dependencies from crates.io.
Run the full workspace suite only when the user explicitly requests it:
```sh
cargo test-workspace
```

## Repository Layout

- `lang/`: parser, type checker, analysis session, interpreter, backends, and tests.
- `lib/`: standard library, example programs, and test projects under `lib/tests/`.
- `cli/`: command-line interface.
- `docs/`: literate Zydeco tutorial material (see `docs/spell`).
- `editor/`: editor integrations (TextMate grammar and VSCode extension).
- `web/`: web interface.

## Language Pipeline (High-Level)

The core phases are:

1. parsing (`lang/surface/src/textual`)
2. desugaring (`lang/surface/src/bitter`)
3. name resolution (`lang/surface/src/scoped`)
4. type checking (`lang/statics/src`)
5. linking (`lang/dynamics/src`)
6. evaluation (`lang/dynamics/src`)

Each phase generally includes `syntax`, `arena`, `err`, `fmt`, and `span` modules.

## Notes for Changes

- Prefer updating `DESIGN.md` or `CONTRIBUTE.md` when modifying architecture or workflows.
- Put exploratory design notes in `docs/ideas/`. Explain the motivating problem, relevant constraints,
  alternatives, and the principles by which a later decision should be judged.
- Put chronological implementation worklogs in `docs/logs/`. Record attempts, observations, failures,
  measurements, decisions, and unresolved questions so later work can build on the evidence.
- Cross-link a design note and its worklogs instead of duplicating their contents. Keep the stable design account
  in the idea or canonical architecture document, and keep the history of how it was reached in the worklog.
- Keep doc examples consistent with CLI flags and scripts in the repo.
- If you add new example projects under `lib/`, consider wiring them into `lang/tests`.

## Documentation Style

Wrap prose to approximately 90~120 characters per line.
Prefer one complete sentence within one line without breaking the natural flow of the paragraph.
If not, prefer breaks after punctuation (especially prefer period) or before connective words,
and relax the target for the final sentence of a paragraph rather than forcing an unnatural break.
Preserve the formatting of code blocks and other structurally significant Markdown.

Write for a reader encountering the design in sequence.
Establish the motivation and relevant context before introducing machinery,
and make clear which question each mechanism answers.
Reveal the argument gradually instead of presenting every consequence at once.
Use connective sentences to explain why one paragraph or section leads to the next,
especially before increasing the level of technical detail.
These connections should carry reasoning rather than serve as generic transitions.

Use programming-languages terminology when it adds precision,
but explain specialized terms at first use and relate them to source-level intuition or a concrete example.
Prefer positive accounts of what a construct provides.
Use definition by contrast or negation when the distinction itself matters,
but avoid making repeated "not X, but Y" formulations the main mode of explanation.

Let paragraph and sentence length follow the needs of the argument.
Avoid formulaic prose in which every paragraph has the same rhythm or structure.
Use parallel construction sparingly. Keep repeated grammatical patterns only when they clarify a comparison,
an enumeration, or a formal invariant rather than using them as a default rhetorical style.
After revising documentation, read it from beginning to end and check that the storyline unfolds naturally,
the information density rises gradually, and no paragraph depends on concepts introduced only later.

## Rust Code Style Guideline

Always prefer typed data structures over strings + parsers;
Never be afraid of defining too many types.
For examples,
- Include specific types of errors when creating an error type, not just strings.
- User input should be parsed to be structured data as soon as possible.
- Never use strings to represent states in the software's state machine.
- Never pass strings between internal components when the message could be typed.
- Whenever a hashmap of strings is created, think twice.
  Is it really relying on string deduplication?
  Or it's actually a "dynamic object", that might be concluded by a few traits?

Prefer a functional programming style whenever suitable.
Use iterator transformations instead of manual loops with mutable accumulators.
In particular, avoid mutable `Vec`s and repeated `push` calls when the same result
can be expressed clearly with operations such as `map`, fallible `collect`, `unzip`, or `fold`.
Keep mutation when it represents essential sequential state,
or when a functional rewrite would materially harm clarity or performance.

Prefer to use structs to pack a group of useful functions; prefer methods over functions.
Rust structs have better namespace-ish features than Rust modules.
Never write plain functions that are not wrapped in a struct with your best effort
unless there's no way around otherwise.
When wrapping the functions, abide by the following rules:
- Mention `self` in the signature if the methods are built around the struct type.
  - Take ownership (`self`) if being the elimination form of the struct type,
    namely consuming the struct.
  - Take reference (`&self` or `&mut self`) if the struct only needs to be borrowed.
- Use associated functions (similar to static methods) when the struct is purely a namespace;
  specifically, write `fn new` for "constructors" with no perspective,
  and `fn with_*` for "constructors" that hints how the struct is created.
- Avoid an `fn new` wrapper when it would be the struct's only method
  and merely repackages a struct literal.
- Prefer direct construction unless the constructor performs additional work
  or establishes an intentional visibility boundary for the fields.

For builder patterns, pick receivers based on whether the finalizer must move owned fields out.
If build/finish consumes,
- Use `fn build(self) -> T` for the builder.
- Make all setter methods take and return self `fn with_*(mut self, ...) -> Self` for easy chaining.
If build can borrow,
- Prefer setters `fn set_*(&mut self, ...) -> &mut Self`, and
- Prefer a finalizer `fn build(&self) -> T` so the builder can be reused.
Expose an associated entry point `fn new(required, ...)`,
and use `with_*/set_*` names consistently for optional configuration.

Prefer `derive` and `derive_more` over manual implementations
when their generated behavior exactly matches the intended semantics;
write a manual implementation only when it must enforce additional invariants.

## Commit Message Convention

Format: `prefix: lowercase description`

No capitalization after the colon. No trailing period. One line.
The description should say *what changed*, not *why* (the diff shows what; the description names it).

## Prefix Vocabulary

| Prefix | When to use |
|--------|-------------|
| `feat`  | A user-visible capability that did not exist before. |
| `incr`  | Incremental progress on an existing feature: bug fixes, polish, tuning, small additions. |
| `sisy`  | Mechanical changes: formatting, linting, renaming passes, internal restructuring with no behavior change. |
| `vibe`  | Exploratory, prototype-quality work. Expect rough edges; may be revised or replaced. |
| `repo`  | Repository housekeeping: migrations, dependency changes, formatter config, file reorganization, one-off maintenance. |
| `docs`  | Documentation-only changes (AGENTS.md, README, inline Rust docs/comments). |
| `test`  | Adding or updating tests without changing production code. |
