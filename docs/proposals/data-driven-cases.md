# Data-Driven Case Fixtures

Compiler regressions accumulated as Zydeco sources embedded in Rust string literals:
every case needed a hand-written `#[test]` function, per-file helper structs duplicated assertion logic,
embedded sources had no editor grammar or `zydeco fmt` coverage, and a source tweak recompiled the test binary.
The preceding consolidation moved the assertions onto structured diagnostic codes;
this proposal completes the step by making the sources themselves data.

The harness gives every `.zy` file under `lang/tests/cases/` one test,
so a new regression is a dropped-in fixture with no Rust change.
Registration is discovery, which also removes the silent-gap risk of hand-maintained lists (an orphan fixture
under `lib/tests` is untested until wired).

## Constraints

- Zydeco resolves imports only against the importing file's directory; inline cases therefore rely
  on a prelude wrapper rather than standalone imports.
- Expectations must stay structured: diagnostic codes and phases, not message prose,
  per the shared assertion API in `lang/tests/src/lib.rs`.
- The lexer treats `--|` as documentation text that warns when unattached, so directives use plain `--` lines.
- `zydeco fmt` re-renders comments canonically as `-- <text>`; directives must round-trip through formatting unchanged.
- The repository keeps dependencies deliberate; `libtest-mimic` enters as the standard per-trial runner
  because per-fixture filtering and reporting is the point of the design.

## Design

A fixture is a source fragment. The harness injects the same prelude `SourceCase` builds today —
the builtin library, plus the monadic basis when `-- prelude: monadic` is given —
so fixtures stay small and cannot drift from the inline harness.

Directives are the leading `--` comment lines; `-- stage:` selects `check` (default), `check-value`,
`run`, or `lower`; `-- expect:` selects `accepted` (default), `resolve-error`, or `reject(<code>)`.
Rejection codes use the stable `TyckDiagnosticCode::as_str` spellings;
parsing goes through a `FromStr` defined beside `as_str` with a round-trip test,
so fixture-facing spellings and compiler-facing spellings share one table.
Malformed, unknown, or inapplicable directives (such as `lower` with the monadic prelude) fail the trial —
a test whose stated expectation cannot be honored has no value.

Because directive lines are ordinary Zydeco comments, the whole file feeds the compiler unchanged,
and the files join the repository Zydeco corpus: the parser-agreement and formatter-law tests
in `zydeco-surface` cover every fixture, which also pins directive stability under `zydeco fmt`.

## Alternatives

Filename conventions (`foo.fail.zy`, or one directory per phase) would encode the stage in the path.
That splits one axis across the tree, forces renames when a case changes stage,
and still needs a directive for the expected code.
Topic directories remain — they give the tree its shape and trials their names —
while phase and expectation live in the file.

Golden `.stderr` snapshots, in the style of rustc's UI tests, would record rendered diagnostics.
They couple tests to prose that the code-based assertions deliberately avoid;
if regression-testing the rendered form of diagnostics becomes a goal, snapshots can layer onto the same fixtures.

`datatest-stable` wraps `libtest-mimic` with path-pattern conventions; using the runner directly keeps trial naming
and directive parsing explicit in roughly the same amount of code.

Converting the surface corpus itself to `libtest-mimic` was considered and deferred:
its recovery-law test is coupled to in-crate contract machinery, and the aggregated failures already name their files.
Both corpus tests now collect every violation before failing, which addresses the practical cost of aggregation.

## What Stays in Rust

Fixtures express one source, one stage, one expectation.
Multi-file cases (`check_with_import`), desugaring-phase variant assertions (the pack evidence checks),
stdout and I/O assertions, argument-passing end-to-end programs, the arena mutation tests,
and structural assertions over emitted assembly or foreign declarations remain Rust tests in `lang/tests/tests/`.

## Migration

The pilot moved the five small suites (literal patterns, pattern aliases, quantifiers,
value views, uniform terms) plus monadic-basis examples, 25 fixtures in total.
The larger inline suites migrate opportunistically; new rejection tests should be fixtures from the start.

## Uncertainty

Whether `run` fixtures should carry program arguments and expected exit codes,
and whether desugaring rejections deserve stable spellings (only two sites exist today),
are open until a second consumer appears.
