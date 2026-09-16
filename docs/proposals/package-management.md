# Source-package structure and project context

This proposal implements the [abstraction levels](../references/language.md#abstraction-levels) agreed
during the 2026-09-15 review: project, package, compilation unit, and semantic unit.
It contains pending carrier changes, frontend integration, and unresolved operation policies.
The references own the conceptual model and source rules.

[Source packages](../references/language.md#source-packages) own names,
exact term selection, discovery, roles, and relationships.
[Source loading and sessions](../references/compiler.md#c3-source-loading-sessions-queries-and-memory-retention)
own graph construction and analysis under explicit package bindings.
The [documentation proposal](documentation.md) develops semantic-unit queries and their documentation consumers.

## Motivation and starting point

The review found useful separation between project preparation, source selection, code dependencies, and test planning.
Imports already supply code edges; discovery is bounded by explicit roots; selected terms retain source provenance;
package bindings participate in analysis cache identity.
The direction is to consolidate these boundaries into reusable interfaces.

Under the [source-package contract](../references/language.md#source-packages), the selected provider is a term.
Project preparation needs to preserve its identity, classifier, and lexical boundary across frontends.
Independent emission also needs the declared external entry contract.

The implementation reset retained source selection, immutable bindings, compiler analysis,
documentation provenance, and external-contract checks.
Project preparation, operation orchestration, and documentation output are being rebuilt.
The retained regressions constrain the replacement; the relationship policies below remain open.

## Carrier implementation

The compiler reference's [carrier map](../references/compiler.md#abstraction-carriers) identifies the existing carriers.
The planned Rust interfaces are:

- [ ] Introduce `Project` for shared preparation and inspection, retaining immutable `PackageBindings`.
- [ ] Introduce `SemanticUnit` around the selected term, its explicit analysis context, and its semantic results.
  Reuse the facts and checked representations currently exposed by `ProgramAnalysis` and `CheckedProgram`.
  Preserve rejected analyses and useful diagnostics for editor consumers.
- [ ] Rebuild `CompilationUnit` from the selected package and its checked external contract,
  reusing `ExecutableProgram`, `LibraryProgram`, and `UnitProgram` validation.
  Keep target-specific preparation downstream so one semantic result can support several compilations.

Retain `Package` and `PackageId` with the identity refinements below.
Replace affected callers together; `CompilerSession` continues to manage the implementation's inputs and caches.
Documentation queries consume semantic results, with their remaining work tracked
in the [documentation proposal](documentation.md#targeted-lookup-and-independent-verification).

## Entry identity and terminology

The [source-package contract](../references/language.md#names-and-project-catalogs) separates qualified names
from term fields and filesystem layout.
The internal representation should make those distinctions visible to its callers.

The implementation represents a public name separately from an entry selector:
[`PackageId.name`](../../lang/session/src/source/package.rs) selects a nested declaration,
while `Package.name` records the declared name.
A named whole-file entry therefore has no nested selector.
The representation works, but two fields called `name` hide different responsibilities.

- [ ] Make whole-file and named-term selection explicit in the internal representation.
  A candidate is a selector enum with `FileRoot` and `NamedEntry(PackageName)` variants;
  choose the final names when updating all consumers.
  Preserve canonical identity across a root name and its file path.
- [ ] Explain naming a file and wrapping an import with a concrete comparison.
  A wrapper is its own entry and test subject
  under the [exact-selection contract](../references/language.md#concluding-files-and-exact-term-selection).
  Describe that distinction without suggesting that registration automatically aliases the imported implementation.
- [ ] Keep source-entry identity, compiler subject identity, and published documentation routes separate.
  A local file identity is sufficient for current compilation but does not define a future release URL.

No alias mechanism is required by this cleanup.
If a consumer later needs aliases that share test associations or other package behavior,
specify that relationship explicitly before changing wrapper identity.

## Shared project context

Compiler queries accept explicit package bindings.
The removed CLI orchestration prepared those bindings from conventional files in its working directory
and passed them to the REPL; Cajun's standalone analysis did not prepare the same context.
The replacement must share preparation across frontends,
addressing the [editor limitation](../../CONTRIBUTING.md#use-source-packages).

- [ ] Provide reusable project preparation for selected roots, discovery, package bindings, and snapshot ownership.
  Frontends should supply their chosen roots and consume the resulting package environment consistently.
- [ ] Make the active context inspectable: selected roots, entry declaration locations, discovery origins,
  and name bindings should be available to diagnostics and project inspection.
- [ ] Define refresh responsibilities for directory membership, disk changes, overlays, and root changes.
  Reuse the existing session revision and cache-key contracts.
- [ ] Exercise the same named import through CLI, REPL, editor, and documentation entry points with the same context.

The current [discovery boundary](../references/language.md#bounded-discovery) remains the baseline.
Shared preparation does not by itself choose ancestor searching, source-adjacent roots, recursive discovery,
or different precedence between `package.zy` and `workspace.zy`.

## Validation and operation planning

New callers need to know whether they have merely selected a source, checked its term,
validated its declared external contract, or prepared an operation.
`analyze_package` checks the term; existing executable and library checks establish external contracts.
Shared package operations must connect these results through the carriers above.

- [ ] Expose reusable package validation that combines source checking with the declared entry contract.
  Return a domain result identifying the established contract; retain source analysis for compiler and editor queries.
- [ ] Keep target-specific artifact preparation and execution downstream of package validation.
  Reuse the existing executable, C-export, and native-unit checks instead of duplicating them per command.
- [ ] Define a validation matrix separating malformed declarations, name conflicts,
  unresolved relationships, invalid entry contracts, and operation-specific failures.
  State which phase owns each diagnostic and which work must be withheld on failure.

Relationship policy needs an explicit decision before adding new kinds.
The removed test planner rejected unknown relationship kinds on its requested package
and resolved every available test's `of` subjects, including unrelated tests.
The replacement's failure scope remains an explicit policy decision.

The preferred direction is to validate relationship kinds and targets through an explicit boundary,
then let each operation follow the relationships it owns.
Two choices remain open:

- Should an unsupported kind prevent project validation, fail only an affected operation,
  or produce an inspection diagnostic until a strict validation step is requested?
  Preserve useful inspection and actionable errors for misspelled relationship names.
- Should an invalid test subject elsewhere in the project block a targeted test operation?
  Compare project-wide consistency with targeted-operation isolation and specify both requested and unrelated cases.

Resolve these choices together.
Preserve mistake detection for unsupported kinds and make each operation's failure scope explicit.

## Sequence and completion criteria

1. Refine entry identity and expose semantic-unit analysis, updating affected consumers together.
2. Share `Project` preparation and inspection across frontends.
3. Expose `CompilationUnit` validation and settle relationship-validation scope.
4. Apply these boundaries to the [documentation milestone](documentation.md#first-standard-library-milestone).

Extend the existing [package regressions](../../lang/session/src/source/package/tests.rs),
[discovery regressions](../../lang/session/src/source/package/discovery/tests.rs),
and [CLI coverage](../../cli/tests/package.rs):

- [ ] Preserve name/path identity, distinct wrapper identity, exact nested selection, and companion boundaries.
  Pair valid independent entries with entries that incorrectly capture surrounding bindings.
- [ ] Preserve bounded discovery and fresh overlays; pair included files with exclusions, missing matches,
  conflicting names, and unreadable or malformed inputs.
- [ ] Show that changing project bindings or revisions changes analysis where required and never reuses stale bindings.
- [ ] Reuse semantic results across supported compilation configurations, with invalid external contracts rejected
  before target-specific preparation.
- [ ] Pair valid role contracts with rejected executable and library boundaries across public callers.
- [ ] Test requested and unrelated relationship failures under the selected policy,
  with diagnostic locations and the invariant that failed preparation performs no execution or artifact replacement.

External dependency acquisition, version resolution, and source lockfiles remain separate work
under the [current package limits](../../DESIGN.md#current-limitations).
Their design should build on a clear local context and identity model when an actual consumer requires them.
