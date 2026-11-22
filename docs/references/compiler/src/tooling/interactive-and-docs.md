# Completion, Documentation, and Interactive Engine

## Completion

[Completion queries](../../../../../lang/session/src/source/query/completion.rs) track the exact cursor hole
through recovery, resolution, and checking.
The session recovers only the current root; imports and companions remain strict.
Assembly remaps the exact cursor node into merged syntax, and desugaring provenance connects it to resolution.
A resolver `ScopeSnapshot` enumerates names through ordinary lookup, preserving shadowing and binder introduction depth.
General names require the original parser expectations to admit a term hole;
a recovered hole in a field-name position is insufficient.
Resolution can retain scope after recoverable unbound references;
a fatal phase failure cannot invent an unvisited cursor's environment.
Resolver scope supplies candidates; incoming analytic annotations supply expectations.
A synthesized placeholder is not an expectation. Multiple visits retain all analytic constraints.
Compatibility is `Equal`, `Unknown`, or `Mismatch`: only proven mismatches are omitted,
and candidates requiring inference remain unknown.
Probes reuse `Lub`, following solved fills but deferring unsolved ones without solution or scope writes.
`Equal` satisfies every analytic annotation without inference; one rigid rejection establishes `Mismatch`,
even when another part of the classifier is unresolved.
Other unavailable or inconclusive evidence remains `Unknown`.
Disposable compatibility checks must not mutate source facts or depend on enumeration order.
The session orders candidates by prefix, compatibility, scope proximity,
and deterministic label order; Cajun projects that order into protocol items.
Missing annotations remain optional, and classifier fit does not prove that every term-level constraint
or unrelated error is satisfied by insertion.
The completion query retains its latest result without installing repaired source or replacing strict analysis;
Cajun checks the revision of its disposable session snapshot.

Source-path completion recognizes the metadata catalog's `Source` argument kind.
It merges filesystem entries and overlays relative to the importing source,
excluding direct self-imports and symlink aliases.
Directories sort before conventional source files.
The edit replaces the current path component with source-language escapes,
preserving quotes, the written directory prefix, and following components.
Numbered imports, unrelated strings, comments, and unfinished escape sequences receive no path suggestions.
[Recovering parsing](../parsing-and-resolution.md#recovering-parsing) owns syntax recovery
and cursor identity; further candidate families and stale-state policy remain an
[open idea](../../../../ideas/README.md#structural-completion-and-last-successful-state).

## Source Documentation Annotations

[Source documentation](../../../language/src/meta-annotations.md#source-documentation) consists
of `@[doc]` and adjacent text attachment.
The [shared source scan](../sessions-and-queries.md#shared-source-analysis) retains these annotations
and reports detached text; formatting preserves their attachment.
Semantic documentation analysis, publication, editor presentation, and example verification are not implemented.

## Interactive Engine

The [TUI engine](../../../../../tui/src/engine.rs) stores complete submissions
as numbered source inputs in a session overlay.
Each recorded number identifies immutable source text; later inputs compose it explicitly with `@(import(N))`.
There is no hidden lexical environment.
Re-importing a computation can repeat effects: history stores source, not memoized runtime results.
Clearing the transcript leaves those importable sources available.
A rejected submission returns to the editor at the same number; retry replaces that unrecorded attempt.

For ordinary input, the engine installs the numbered submission directly as an overlay and checks it once
through `CompilerSession::check_repl_entry` against the default core prelude (and resolved imports),
without generating wrapper files or re-checking under a synthetic `ret` wrapper.
Types, kinds, and non-runtime values inspect directly via `ReplEntry` analysis facts;
runtime values and computations evaluate against the linked entry with captured output
and errors, preserving any user `@[debug(...)]` observations.
Explicit execution goes through the executable entry boundary.

Root command metadata is parsed into typed commands before ordinary submission handling.
Help and quit do not consume a source number; unsupported command arguments are rejected,
and unrecognized metadata remains ordinary language syntax.
The [CLI guide](../../../../../cli/README.md#interactive-repl) documents commands, editing keys, and retry interaction.
Deferred interaction work covers [history persistence and replay](../../../../ideas/README.md#repl-history-and-replay)
and [history pruning](../../../../ideas/README.md#repl-history-pruning).
