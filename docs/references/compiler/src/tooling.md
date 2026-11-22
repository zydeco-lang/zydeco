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
[`SpanIndex`](../../../../editor/cajun/src/span_index.rs) partitions definition, use,
and term spans by `FileId` over the session's shared `SourceMap` into per-file augmented interval trees once
per analysis revision, serving `symbol_at`, `term_at`, and `document_symbols`
in `O(log N_file)` time with deterministic `(length, order)` tie-breaking.
[`Cajun`](../../../../editor/cajun/src/lib.rs) caches canonicalized `Url -> PathBuf` mappings (`SourcePath::identity`)
per open document (refreshing on `did_save` and removing on `did_close`), and `ProjectState` precomputes `file_uris`
per `FileId`, eliminating repeated `realpath` syscalls and `Url::from_file_path` conversions from hot request paths.
Hover uses `symbol_at` with its configured endpoint policy and falls back to `term_at`; when a symbol or term hits,
`Cajun::hover` takes a disposable Salsa snapshot outside the session mutex and re-materializes `StaticsArena`
on demand inside `AnalysisTask::run`, so Salsa's `lru = 1` cache reclaims arenas when switching files.
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
[lowering and resolution](parsing-and-resolution.md#fused-lowering-and-resolution),
[name resolution](parsing-and-resolution.md#name-resolution),
[checking and finalization](checking-and-inference.md#checker-recovery),
[execution readiness](elaboration-and-validation.md#residual-runtime-traversal),
and [SPS verification](sps.md#sps-analysis-traversal).
These contracts state which work continues and which dependent product remains unavailable.
Further diagnostic producers and presentation customization remain
in the [traversal proposal](../../../proposals/traversals.md#diagnostic-collection-and-recovery).
