# Contributing

Build the CLI, make a focused change, then test, format, and document it.
Run the commands below from the repository root.
For language and CLI usage, start with the [quick start](README.md#quick-start) and [CLI guide](cli/README.md).

## Build the CLI

Install the Rust toolchain required by [Cargo.toml](Cargo.toml), with `rustfmt` and `clippy`.
On Unix, building the interpreter also requires a C toolchain and `make` for bundled libffi.

```sh
cargo build --bin zydeco
cargo run --bin zydeco -- --help
```

Add `--release` to build `target/release/zydeco`, or install with `cargo install --path cli`.
The [CLI guide](cli/README.md#build-artifacts) lists native and WebAssembly tool requirements.
For editor work, follow the [Cajun guide](editor/README.md)
or [Tree-sitter development workflow](editor/tree-sitter-zydeco/README.md#development).

## Make a Change

Use the [repository map](DESIGN.md#repository-layout) to locate the implementation
and the [references](docs/references/README.md) to find its contract.
Follow [AGENTS.md](AGENTS.md) for code conventions and design approval. Compiler pass changes should follow
the shared [traversal](docs/references/compiler.md#choosing-and-composing-traversals)
and [resumable execution](docs/references/compiler.md#resumable-folder-execution) guidance.

Retain bug reproducers as regression tests.
When changing validation, pair accepted inputs with rejected counterparts and assert the intended error.
Update affected callers and documentation with the implementation.

## Run Tests

While iterating, select the affected crate, integration target, or fixture:

```sh
cargo test -p zydeco-surface --lib
cargo test -p zydeco-statics --test foreign
cargo test -p zydeco-tests --test cases -- literal-pattern/
```

Use [source cases](lang/tests/cases/README.md) for individual language behaviors.
Put whole-program regressions under `lib/tests/` and register them in `lang/tests/tests/`;
the [test harness reference](docs/references/compiler.md#source-fixtures-and-runtime-oracles) explains backend coverage,
I/O assertions, and source templates.
Native tests need the [AMD64 toolchain](cli/README.md#native-executables).
WebAssembly tests use Node.js 24; `NODE` selects its executable.

CI runs the full workspace suite and doctests separately:

```sh
cargo test-all --release
cargo test-doc-all --release
```

[.cargo/config.toml](.cargo/config.toml) defines the aliases;
the [CI workflow](.github/workflows/rust.yml) records tool setup and checks.
[AGENTS.md](AGENTS.md#validation-and-formatting) governs test selection for automated work.

### Iterate with Resumable Tests

`cargo test-all` always runs every test.
While fixing a failure in the full suite, use the resumable runner instead:

```sh
cargo test-resume
```

It builds the same targets, runs them one by one, and saves each outcome under `target/xtask/test-resume/`.
The next invocation reruns only tests that failed or never ran, so a fix continues from the previous failure.
Cargo options such as `--release` or `-p CRATE`, and test options after `--` such as `--test-threads=2`
or a name filter, are accepted.

- `--force` discards saved results and runs everything.
- `--status` prints the saved results; `--clear` deletes them for the invocation.
- `--dry-run` prints which targets would run without executing any test.

Harness output keeps its colors when the runner's stdout is a terminal.
Pass `--color=never` or `--color=always` after `--` to override that detection,
for example when redirecting a run into a file.

Concurrent invocations that share saved results wait for each other:
the second one prints a waiting notice and continues when the first finishes.
`--status` only reads a snapshot, so it stays available while a run is active,
and runs with different Cargo arguments or test selections use separate saved results and do not wait.

Saved passes are reused even when the build changed.
That keeps the fix-and-rerun loop short but can hide a regression in a test that already passed.
Validate the final state with `cargo test-all --release` (or `cargo test-resume --force`);
doctests remain covered by `cargo test-doc-all`.

## Format and Lint

```sh
cargo fmt --all
cargo clippy-all -- -D warnings
cargo run --bin zydeco -- fmt path/to/source.zy
```

Supply Zydeco files explicitly and format source embedded in Rust strings separately.
See the [formatter reference](docs/references/compiler.md#formatting-and-typed-rendering)
for source-level options and [AGENTS.md](AGENTS.md#validation-and-formatting) for repository-wide formatting.

## Work on Documentation

### Maintain Documentation

Use the [documentation index](docs/README.md) and [rule ownership map](docs/references/README.md#rule-ownership)
to choose the document to update.
Follow the [documentation conventions](AGENTS.md#documentation-structure-and-design-records)
when moving approved designs into the references or consolidating existing material.

Check complete source examples with `zydeco check`, verify links and anchors, and read the edited document in order.
Reflow Markdown with:

```sh
python3 docs/scripts/reflow-markdown.py path/to/document.md
python3 docs/scripts/reflow-markdown.py --check --changed
```

### Generate Documentation

Build Rust API documentation for the affected package, or regenerate the literate chapters:

```sh
cargo doc --package zydeco-statics --no-deps
make -C docs/spell build
```

Cargo writes to `target/doc/`; the chapter build uses `zsh` and `jq` and writes Markdown under `docs/spell/`.

## Submit the Change

Review the diff, summarize the change and validation performed,
and follow the [commit convention](AGENTS.md#commit-messages).
Enable optional formatting and check hooks with `pre-commit install`.

For releases, update the workspace version and lockfile, commit, and push a matching `v*` tag.
The [release workflow](.github/workflows/release.yml) publishes the release and generated notes.
