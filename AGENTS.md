# AGENTS

This file is guidance for automated assistants working in this repository.

## Adding a Dependency

All Rust dependencies are managed in the top-level `Cargo.toml` file, under `[workspace.dependencies]`.
All crates then use `dependency = { workspace = true }` to refer to the workspace-registered dependencies in their own `Cargo.toml`.

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
cargo clippy-all
```

Run a Zydeco program:
```sh
zydeco run path/to/main.zy
```

Run the test suite:
```sh
cargo test-all
```

## Repository Layout

- `lang/`: parser, type checker, analysis session, interpreter, backends, and tests.
- `lib/`: standard library, example programs, and test projects under
  `lib/tests/`.
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

Each phase generally includes `syntax`, `arena`, `err`, `fmt`, and `span`
modules.

## Notes for Changes

- Prefer updating `DESIGN.md` or `CONTRIBUTE.md` when modifying architecture or workflows.
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
