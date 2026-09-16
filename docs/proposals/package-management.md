# Source-Package Structure and Project Context

This plan implements the [abstraction levels](../references/language.md#abstraction-levels):
project, package, compilation unit, and semantic unit.
The [namespace and resolution proposal](package-resolution.md) develops the package hierarchy,
opaque roots, source instantiation, and merging before semantic analysis.
This plan covers their implementation, frontend integration, and operation policy.

[Source packages](../references/language.md#source-packages) describe the existing names,
exact term selection, discovery, roles, and relationships.
[Source loading and sessions](../references/compiler.md#c3-source-loading-sessions-queries-and-memory-retention)
own graph construction and analysis under explicit package bindings.
The [documentation proposal](documentation.md) develops semantic-unit queries and their documentation consumers.

## Motivation and Starting Point

Project preparation connects source selection, code dependencies, and operation planning.
It exposes each selected term's identity, classifier, lexical scope, and any declared external contract consistently
across frontends.

The retained implementation supplies source selection, immutable bindings, compiler analysis,
documentation provenance, external-contract checks, and regression coverage.
Build shared preparation and operation interfaces on these components using the new resolution model.

## Implementation Representations

The compiler reference's [implementation map](../references/compiler.md#implementation-representations)
identifies the existing representations.
The planned Rust interfaces are:

- [ ] Introduce `Project` for shared preparation and inspection.
  Prepare the resolution graph and expose its resulting bindings through immutable `PackageBindings`.
- [ ] Introduce `SemanticUnit` around a resolved, merged term, its analysis inputs, and its semantic results.
  Reuse the facts and checked representations currently exposed by `ProgramAnalysis` and `CheckedProgram`.
  Preserve rejected analyses and useful diagnostics for editor consumers.
- [ ] Rebuild `CompilationUnit` from the selected package and its checked external contract,
  reusing `ExecutableProgram`, `LibraryProgram`, and `UnitProgram` validation.
  Keep target-specific preparation downstream so one semantic result can support several compilations.

Refine `Package` and `PackageId` around candidate instances and merged package identity.
Replace affected callers together; `CompilerSession` continues to manage the implementation's inputs and caches.
Documentation queries consume semantic results, with their remaining work tracked
in the [documentation proposal](documentation.md#targeted-lookup-and-independent-verification).

## Entry Identity and Terminology

The [package hierarchy](package-resolution.md#package-hierarchy) supplies namespace paths,
while source selections identify the term to instantiate.
The internal representation should make both of these available to its callers,
together with the resulting merged package identity.

The current fields have two roles:
[`PackageId.name`](../../lang/session/src/source/package.rs) selects a nested declaration,
while `Package.name` records the declared name.
Whole-file selection addresses the file root.

- [ ] Make whole-file and named-term selection explicit in the internal representation.
  A candidate is a selector enum with `FileRoot` and `NamedEntry(PackageName)` variants;
  choose the final names when updating all consumers.
  Record the root, package context, and source origin of each selected instance.
  Name and file selections converge when their resolved contents agree
  under the [merging model](package-resolution.md#copy-resolve-merge-analyze).
- [ ] Explain naming a file and wrapping an import with a concrete comparison.
  An annotated import starts as its own candidate instance.
  Resolved contents, including package metadata, determine which candidates merge;
  retain both the registration and imported-source origins.
- [ ] Keep source-entry identity, compiler subject identity, and published documentation routes separate.
  A source path identifies an input; the resolved graph determines semantic identity,
  and output consumers choose routes from the retained bindings and provenance.

## Shared Project Context

Compiler queries accept explicit package bindings.
Shared preparation supplies the same resolution context to CLI, REPL, editor, and documentation consumers,
extending the [source-package workflow](../../CONTRIBUTING.md#use-source-packages).

- [ ] Provide reusable project preparation for selected source roots, discovery, the opaque namespace root,
  candidate instances, resolved imports, merging, package bindings, and snapshot ownership.
  Frontends should supply their chosen entry points and consume the resulting resolution graph consistently.
- [ ] Make the active context inspectable: selected roots, entry declaration locations,
  discovery origins, namespace substitutions, import routes, and resulting bindings should be available
  to diagnostics and project inspection.
- [ ] Define refresh responsibilities for directory membership, disk changes, overlays, and root changes.
  Reuse the existing session revision and cache-key contracts.
- [ ] Exercise the same named import through CLI, REPL, editor, and documentation entry points with the same context.

Use the current [discovery boundary](../references/language.md#bounded-discovery): the CLI selects `package.zy`
and `workspace.zy` in its working directory, and their explicit patterns determine the files to inspect.

## Validation and Operation Planning

Expose the results of source selection, term checking, contract validation, and operation preparation.
`analyze_package` checks the term; executable and library checks establish external contracts.
Relationship targets use the same package-path resolution as definitions and imports;
operation policy determines which resolved relationships an operation follows.

- [ ] Expose reusable package validation that combines source checking with the declared entry contract.
  Return a domain result identifying the established contract; retain source analysis for compiler and editor queries.
- [ ] Keep target-specific artifact preparation and execution downstream of package validation.
  Reuse the existing executable, C-export, and native-unit checks.
- [ ] Define a validation matrix separating malformed declarations, name conflicts,
  unresolved relationships, invalid entry contracts, and operation-specific failures.
  Assign each diagnostic to a phase and state the successful results required by the next phase.

Choose relationship-validation scope as part of operation planning:

- Which phase reports unsupported relationship kinds: project validation, an affected operation, or inspection?
- Which subject associations does a targeted test validate: all available tests' subjects or the selected suite's?

Specify both requested and unrelated cases, with diagnostics for misspelled kinds and unresolved targets.

## Sequence and Completion Criteria

1. Implement namespace paths, source instantiation, and merging in shared `Project` preparation.
2. Refine entry identity and expose semantic-unit analysis and inspection across frontends,
   updating affected consumers together.
3. Expose `CompilationUnit` validation and settle relationship-validation scope.
4. Apply these boundaries to the [documentation milestone](documentation.md#first-standard-library-milestone).

Extend the existing [package regressions](../../lang/session/src/source/package/tests.rs),
[discovery regressions](../../lang/session/src/source/package/discovery/tests.rs),
and [CLI coverage](../../cli/tests/package.rs):

- [ ] Exercise the [resolution cases](package-resolution.md#implementation-work),
  including converging name/file selections, wrapper origins, exact nested selection, and companion boundaries.
  Pair valid independent entries with diagnostics for captured enclosing bindings.
- [ ] Preserve bounded discovery and fresh overlays; pair included files with exclusions, missing matches,
  conflicting names, and unreadable or malformed inputs.
- [ ] Show that changed dependencies and source revisions produce the appropriate resolved nodes,
  while equivalent copies share semantic analysis.
- [ ] Reuse semantic results across supported compilation configurations, with invalid external contracts rejected
  before target-specific preparation.
- [ ] Pair valid role contracts with rejected executable and library boundaries across public callers.
- [ ] Test requested and unrelated relationship diagnostics under the selected policy.
  Begin execution and replace artifacts after all required preparation succeeds.

Future dependency acquisition, version resolution, and source lockfiles build on this local context and identity model.
The [package roadmap](../../DESIGN.md#current-limitations) records that scope.
