# C15. Diagnostics, Formatting, Documentation, and Interactive Tooling

Tooling uses the facts established by ordinary source phases and associates results with their source revision.
[Session queries](../../../../lang/session/src/source/query.rs) supply semantic identities;
[Cajun](../../../../editor/cajun/src/analysis.rs) adapts them to LSP and discards superseded results.
Rendered labels must not become keys for semantic lookup.

[Type diagnostics](../../../../lang/statics/src/check/error.rs) carry stable codes and structured relationships;
[report construction](../../../../lang/session/src/source/report.rs) resolves their source sites
and explanatory context.
CLI, TUI, and LSP render those reports for their surfaces.
Retained rejected facts can support useful tooling, while later errors caused solely
by an earlier failure should not manufacture independent evidence.
Definition, reference, rename, hover, and semantic-token operations use current provenance and lexical identity.
UTF-16 conversion happens when reading or writing client positions.

## Cursor-Based Selection

An editor cursor sits between characters, so a symbol command should accept both `|W` and `W|`.
Cajun's shared `symbol_at` lookup selects a resolved occurrence with `start <= offset && offset <= end`;
go-to-definition, find-references, prepare-rename, and rename all use this entry point.
The smallest matching source span wins.
Positions beyond either edge do not select that occurrence.

Cursor hit-testing is separate from source-range representation.
The [shared endpoint policy](../../../../editor/cajun/src/cursor.rs) changes which positions match,
while source spans, navigation targets, reference locations, and edit ranges remain half-open.
Hover uses the same occurrence lookup with its configured endpoint policy and falls back
to the smallest matching term; resolved symbols take precedence over enclosing terms.
The [editor guide](../../../../editor/README.md#runtime-configuration) documents the hover settings.

## Diagnostic Collection

Parsing retains all `ParseIssue` entries in `ParseFailure`.
`ParseError::diagnostics` pairs every issue with the exact rejected source snapshot; CLI reports,
editor locations, and plain-text formatting enumerate the complete collection.
Each issue retains its own token expectations and source range, including a zero-width EOF location.

Source analyzers, desugaring, and resolution retain vectors of typed domain errors.
Strict transformation failures carry a nonempty `Diagnostics<E>` collection together with their source context;
`Err` means no valid phase product is available, regardless of how many errors were established.
Analyses may expose partial facts separately, and completion has its explicit recovered-program contract.
Dependent phases still require valid input.
A failed binder cannot justify inventing a scope for its body.

Semantic folders and the checker record each diagnostic once and propagate `ReportedError` when their work fails.
Independent siblings continue where their input contracts remain satisfied; cached rejection does not replay errors.
The session and CLI, TUI, and LSP retain and render every reported error with its own source location.
Primary-location convenience APIs do not replace complete diagnostic enumeration.

Producers append diagnostics and composition concatenates collections.
Diagnostic ordering is not an API guarantee; successful fact ordering retains its own contract.
Tests compare contents, multiplicity, and locations rather than presentation order,
and pair rejected inputs with valid counterparts.
Equal spans alone do not identify duplicate errors.
Checker publication deduplicates complete `TyckDiagnostic` values. It suppresses unresolved-classifier consequences
when the fill's originating expression contains an already reported rejection site, including related sites
of an unconstrained-inference diagnostic; unresolved fills in independent expressions remain reportable.

Recovery boundaries are owned by their producing phases: [source loading](sessions-and-queries.md),
[shared source analysis](sessions-and-queries.md#shared-source-analysis),
[desugaring](parsing-and-resolution.md#desugaring-folders), [resolution](parsing-and-resolution.md#name-resolution),
[checking and finalization](checking-and-inference.md#checker-recovery),
[execution readiness](elaboration-and-validation.md#residual-runtime-traversal),
and [SPS verification](sps.md#sps-analysis-traversal).
These contracts state which work continues and which dependent product remains unavailable.
Further diagnostic producers and presentation customization remain
in the [traversal proposal](../../../proposals/traversals.md#diagnostic-collection-and-recovery).
