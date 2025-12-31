# AGENTS

This file is guidance for automated assistants working in this repository.

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
cargo clippy --all --exclude zydeco-runtime
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
