# Source-package structure and project context

The source-package system has a coherent core, but its identity, project context,
and operation boundaries need clearer ownership before more frontends depend on it.
This proposal records the directions from the 2026-09-15 review.
It contains implementation work and unresolved choices; the existing references remain the authority
for current behavior.

[Source packages](../references/language.md#source-packages) own names,
exact term selection, discovery, roles, and relationships.
[Source loading and sessions](../references/compiler.md#c3-source-loading-sessions-queries-and-memory-retention)
own graph construction and catalog-dependent analysis.
The [documentation proposal](documentation.md) is a consumer of these boundaries and owns its own subject
and publication model.

## Motivation and starting point

The review found useful separation between catalog preparation, source selection, code dependencies, and test planning.
Imports already supply code edges; discovery is bounded by explicit roots; selected terms retain source provenance;
catalog bindings participate in analysis cache identity.
The direction is to consolidate these boundaries into reusable interfaces.

Under the [source-package contract](../references/language.md#source-packages), the selected provider is a term.
Project preparation needs to preserve its identity, classifier, and lexical boundary across frontends.
Independent emission also needs the declared external entry contract.

The review ran the focused package and discovery regressions successfully: 41 tests passed.
Reproduce that coverage with:

```sh
cargo test -p zydeco-session --lib source::package::
```

Those tests establish the current behavior, including several deliberate choices whose costs are discussed below.
They do not establish frontend consistency or settle the proposed changes.

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

The session accepts explicit catalogs, while the [CLI](../../cli/src/main.rs) prepares them
from conventional files in its working directory.
The REPL receives those bindings; Cajun's standalone analysis does not yet prepare the same context.
The [editor limitation](../../CONTRIBUTING.md#use-source-packages) is a concrete reason
to share project preparation before extending other consumers.

- [ ] Provide a reusable preparation boundary for selected roots, discovery, catalog bindings, and snapshot ownership.
  Frontends should supply their chosen context and consume the resulting catalog consistently.
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
Today `analyze_package` checks the term, while CLI logic adds executable and compiled-library validation.

- [ ] Expose reusable package validation that combines source checking with the declared entry contract.
  Return a domain result identifying the established contract; retain source analysis for compiler and editor queries.
- [ ] Keep target-specific artifact preparation and execution downstream of package validation.
  Reuse the existing executable, C-export, and native-unit checks instead of duplicating them per command.
- [ ] Define a validation matrix separating malformed declarations, name conflicts,
  unresolved relationships, invalid entry contracts, and operation-specific failures.
  State which phase owns each diagnostic and which work must be withheld on failure.

Relationship policy needs an explicit decision before adding new kinds.
The current [`PackageTestPlan`](../../lang/session/src/source/package.rs) rejects unknown relationship kinds
on its requested package and resolves every catalog test's `of` subjects, including unrelated tests.
These are documented behaviors, not implementation drift.

The preferred direction is to validate relationship kinds and targets through an explicit boundary,
then let each operation follow the relationships it owns.
Two choices remain open:

- Should an unsupported kind prevent catalog validation, fail only an affected operation,
  or produce an inspection diagnostic until a strict validation step is requested?
  Preserve useful inspection and actionable errors for misspelled relationship names.
- Should an invalid test subject elsewhere in the catalog block a targeted test operation?
  Compare whole-catalog consistency with targeted-operation isolation and specify both requested and unrelated cases.

Resolve these choices together.
Silently ignoring unknown kinds would lose the mistake detection supplied by today's stricter planning;
copying the current test behavior to every new relationship would couple otherwise independent operations.

## Sequence and completion criteria

1. Clarify terminology and entry identity, updating consumers and removing superseded representations together.
2. Share project-context preparation and inspection across frontends.
3. Expose package validation and settle relationship-validation scope.
4. Apply these boundaries to the [documentation milestone](documentation.md#first-standard-library-milestone).

Extend the existing [package regressions](../../lang/session/src/source/package/tests.rs),
[discovery regressions](../../lang/session/src/source/package/discovery/tests.rs),
and [CLI coverage](../../cli/tests/package.rs):

- [ ] Preserve name/path identity, distinct wrapper identity, exact nested selection, and companion boundaries.
  Pair valid independent entries with entries that incorrectly capture surrounding bindings.
- [ ] Preserve bounded discovery and fresh overlays; pair included files with exclusions, missing matches,
  conflicting names, and unreadable or malformed inputs.
- [ ] Show that changing catalogs or revisions changes analysis where required and never reuses stale bindings.
- [ ] Pair valid role contracts with rejected executable and library boundaries across public callers.
- [ ] Test requested and unrelated relationship failures under the selected policy,
  with diagnostic locations and the invariant that failed preparation performs no execution or artifact replacement.

External dependency acquisition, version resolution, and source lockfiles remain separate work
under the [current package limits](../../DESIGN.md#current-limitations).
Their design should build on a clear local context and identity model when an actual consumer requires them.
