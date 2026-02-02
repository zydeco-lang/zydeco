# AGENTS

This file is guidance for automated assistants working in this repository.

## Adding a Dependency

All Rust dependencies are managed in the top-level `Cargo.toml` file, under `[workspace.dependencies]`. All crates then use `dependency = { workspace = true }` to refer to the workspace-registered dependencies in their own `Cargo.toml`.

## Project Snapshot

- Zydeco is a proof-of-concept programming language based on call-by-push-value.
- The implementation is a Rust workspace with multiple crates under `lang/`.
- The standard library and examples live under `lib/`, including `lib/oopsla`.

## Key Docs

- `README.md`: top-level usage and quick start.
- `DESIGN.md`: language model, translation pipeline, and limitations.
- `CONTRIBUTE.md`: build/test workflows and contribution notes.
- `lib/oopsla/README.md`: artifact overview and detailed examples.

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
zydeco run path/to/proj.toml --bin=foo
```

Run the test suite:
```sh
cargo test-all
```

## Repository Layout

- `lang/`: parser, type checker, interpreter, driver, and tests.
- `lib/`: standard library and example programs.
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

- Prefer updating `DESIGN.md` or `CONTRIBUTE.md` when modifying architecture or
  workflows.
- Keep doc examples consistent with CLI flags and scripts in the repo.
- If you add new example projects under `lib/`, consider wiring them into
  `lang/tests`.

## Rust Code Style Guideline

Always prefer typed data structures over strings + parsers, and
Never be afraid of defining too many types.
For examples,
- Include specific types of errors when creating an error type, not just strings.
- User input should be parsed to be structured data as soon as possible.
- Never use strings to represent states in the software's state machine.
- Never pass strings between internal components when the message could be typed.
- Whenever a hashmap of strings is created, think twice.
  Is it really relying on string deduplication?
  Or it's actually a "dynamic object", that might be concluded by a few traits?

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

For builder patterns, pick receivers based on whether the finalizer must move owned fields out.
If build/finish consumes,
- Use `fn build(self) -> T` for the builder.
- Make all setter methods take and return self `fn with_*(mut self, ...) -> Self` for easy chaining.
If build can borrow,
- Prefer setters `fn set_*(&mut self, ...) -> &mut Self`, and
- Prefer a finalizer `fn build(&self) -> T` so the builder can be reused.
Expose an associated entry point `fn new(required, ...)`,
and use `with_*/set_*` names consistently for optional configuration.
