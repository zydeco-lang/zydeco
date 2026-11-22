# C16. Validation, Debugging, and Extending the Implementation

Choose the first boundary that can state the property being changed,
then assert the observable consequence at the necessary downstream boundary.
A correct interpreter result does not prove IR ownership or native root preservation;
an IR snapshot alone does not prove effect order.

| Change | Focused evidence |
| --- | --- |
| Grammar or formatting | Surface parser agreement, recovery contracts, semantic/comment preservation, idempotence |
| Typing, scope, or a source rejection | [Source fixtures](../../../../lang/tests/cases) or the relevant Rust integration target, with exact diagnostic codes |
| Typed-arena invariant | A seeded mutation and a clean counterpart in [arena invariant tests](../../../../lang/tests/tests/suite/arena_invariants.rs) |
| SPS reduction or representation | SPS and SPSLow invariant checks, blocked-rewrite cases, and runtime observations |
| Native layout or allocation | Preparation/model unit tests and targeted native-model or GC integration tests |
| Host or FFI operation | Signature rejection tests, adapter tests, declared I/O, and target-specific execution |
| Tooling | Current and failed revisions, exact cursor/site identity, and stale-result rejection |

## Source Fixtures and Runtime Oracles

The [case harness](../../../../lang/tests/tests/cases.rs) uses `libtest-mimic` to discover one trial per `.zy` file.
Files are source fragments wrapped with the same prelude as `SourceCase`.
Leading ordinary comments select stage, prelude, and expectation;
the [case guide](../../../../lang/tests/cases/README.md) is the directive reference.
Unknown, duplicated, or inapplicable directives fail a trial instead of weakening its expectation.
Diagnostic spellings are parsed beside their stable compiler codes and round-trip through that shared catalog.
Fixtures also enter the parser/formatter corpus,
so standalone trials need neither manual registration nor a Rust recompilation after source edits.

The [standard-library directive runner](../../../../lang/tests/tests/directives.rs) applies the same leading block
to whole-program fixtures.
A file enters that runner when it declares `stage: run`; `backends`, `args`, `input`,
`output`, and `setup` describe its execution.
Inline case fixtures reject those execution directives.

Topic paths name the regression; directives name its phase and expected outcome.
This avoids renaming tests when their stage changes and avoids coupling semantic assertions to rendered error prose.
Using the trial runner directly keeps naming and directive validation explicit.
Rendered-diagnostic snapshots can be added for a distinct presentation contract.
Table-driven `for` loops retain their sources and assertions together in Rust,
whether their input lists are handwritten or generated.
Multi-file imports, arena mutation, desugared-variant assertions, emitted-code structure,
process arguments, and I/O remain Rust tests or whole-program fixtures
because a single-fragment expectation cannot express those relationships.

Long shared program skeletons can obscure the varying inputs in a Rust table.
Keep those skeletons in [`.zy.in` templates](../../../../lang/tests/templates),
loaded with `include_str!` and rendered by [`SourceTemplate`](../../../../lang/tests/src/template.rs).
Rust still owns loops, case data, diagnostic codes, and assertions; concrete single cases remain `.zy` fixtures.
Templates provide named textual substitutions only, with no loops or conditionals.
`${name}` selects a parameter whose name is an ASCII identifier; ordinary Zydeco braces remain literal.
Each distinct parameter requires exactly one argument, even when it appears repeatedly.
Malformed placeholders and missing, unused, or duplicate arguments return structured errors before rendering.
Substitutions are copied verbatim without escaping or recursive expansion, so callers supply valid source text,
including any string-literal escaping their inputs require.

The memory and effects wrappers are the initial template users.
Their rendered programs go through the existing source harness, preserving each Rust test's pipeline checks.
[Template integration tests](../../../../lang/tests/tests/suite/templates.rs) also pair an accepted body
with an exact-code rejection, then check both expectations after formatting and verify formatter idempotence.
Raw `.zy.in` files are skeletons rather than complete Zydeco terms, so only their rendered sources enter these checks;
they are not discovered as standalone case fixtures or parser/formatter corpus files.

[The shared harness](../../../../lang/tests/src/lib.rs) supplies check, runtime, and end-to-end registrations.
Whole-program tests declare their backends, input, arguments, expected output, and exit behavior;
directive-driven fixtures declare the same execution metadata, with the runner requiring exit 0.
Undeclared stdin is EOF and output is captured.
Native FFI and toolchain-dependent cases can require explicit opt-in.
A regression must preserve both the accepted use and the rejected or non-optimizable counterpart relevant
to its invariant.

## Following a Change

For a source feature, follow textual syntax and metadata through scoped lowering/resolution, typing,
static elaboration, interpreter linking, SPS lowering, and every consuming backend.
For an optimization, identify its required input invariant and every representation or root map it changes.
For a primitive, update the role catalog, checked signature, materializers, interpreter,
native runtime/emitter, Wasm emitter/host, and conformance cases.
For foreign interfaces or package resolution,
follow the [shared compilation-unit design](native-emission.md#compilation-unit-preparation-and-artifacts).
Prefer existing domain types and shared declarations at each common boundary.

Diagnostic investigation starts from the saved source site and the earliest representation whose invariant fails.
The textual, scoped, statics, SPS, and assembly debug printers expose successive views;
use the relevant phase's verifier before interpreting a downstream crash as a source-language failure.
Keep commands in [CONTRIBUTING](../../../../CONTRIBUTING.md#run-tests).
Automated work reserves the full workspace suite for an explicit request.

Performance evidence records the revision, workload, build profiles, host/target,
measured quantity, and default or experimental representation.
Allocation counts, reserved capacity, peak RSS, and elapsed time answer different questions.
[Runtime evaluations](../../../evaluations/2026-09-08-cbpv-runtime/README.md) retain historical comparisons;
repeat them before selecting a new default or claiming a current improvement.
Reference examples and local links are checked separately from runtime tests,
and documentation drift belongs in [the todo records](../../../todos/README.md).
