# Source-Package Structure and Project Context

This plan implements the [abstraction levels](../references/language.md#abstraction-levels):
project, package, compilation unit, and semantic unit.
Package paths, source selection, instance identity,
and graph construction follow the [namespace and resolution proposal](package-resolution.md).
This plan covers frontend integration, compilation-unit validation, and operation policy.

The [source-package reference](../references/language.md#source-packages) describes the existing implementation.
The [documentation proposal](documentation.md) develops consumers of the resulting semantic units.

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
  Follow the [resolution implementation plan](package-resolution.md#implementation-work).
- [ ] Introduce `SemanticUnit` around a resolved, merged term, its analysis inputs, and its semantic results.
  Reuse the facts and checked representations currently exposed by `ProgramAnalysis` and `CheckedProgram`.
  Preserve rejected analyses and useful diagnostics for editor consumers.
- [ ] Rebuild `CompilationUnit` from the selected package and its checked external contract,
  reusing `ExecutableProgram`, `LibraryProgram`, and `UnitProgram` validation.
  Keep target-specific preparation downstream so one semantic result can support several compilations.

Replace affected callers together; `CompilerSession` continues to manage the implementation's inputs and caches.
Documentation queries consume semantic results, with their remaining work tracked
in the [documentation proposal](documentation.md#targeted-lookup-and-independent-verification).

## Shared Project Context

CLI, REPL, editor, and documentation consumers share project preparation
through the [resolution model](package-resolution.md#copy-resolve-merge-analyze).

- [ ] Let frontends supply their chosen entry points and consume the resulting resolution graph consistently.
- [ ] Make the active context inspectable: selected roots, entry declaration locations,
  discovery origins, namespace substitutions, import routes, and resulting bindings should be available
  to diagnostics and project inspection.
- [ ] Define refresh responsibilities for directory membership, disk changes, overlays, and root changes.
  Follow the resolution graph's inputs when defining revision and cache identity.
- [ ] Exercise the same named import through CLI, REPL, editor, and documentation entry points with the same context.

## Validation and Operation Planning

Build contract validation and operation preparation on semantic-unit analysis.
Use [package resolution](package-resolution.md#paths-and-package-context) for relationship targets;
operation policy determines which resolved relationships an operation follows.

- [ ] Expose reusable package validation that combines source checking with the declared entry contract.
  Return a domain result identifying the established contract; retain source analysis for compiler and editor queries.
- [ ] Keep target-specific artifact preparation and execution downstream of package validation.
  Reuse the existing executable, C-export, and native-unit checks.
- [ ] Define a validation matrix for term checking, external contracts, and operation-specific failures.
  Assign each diagnostic to a phase and state the successful results required by the next phase.

Choose relationship-validation scope as part of operation planning:

- Which phase reports unsupported relationship kinds: project validation, an affected operation, or inspection?
- Which subject associations does a targeted test validate: all available tests' subjects or the selected suite's?

Specify both requested and unrelated cases, with diagnostics for misspelled kinds and unresolved targets.

## Sequence and Completion Criteria

1. Integrate the [resolution model](package-resolution.md) into shared `Project` preparation.
2. Expose semantic-unit analysis and inspection across frontends, updating affected consumers together.
3. Expose `CompilationUnit` validation and settle relationship-validation scope.
4. Apply these boundaries to the [documentation milestone](documentation.md#first-standard-library-milestone).

Extend the existing [package regressions](../../lang/session/src/source/package/tests.rs),
[discovery regressions](../../lang/session/src/source/package/discovery/tests.rs),
and [CLI coverage](../../cli/tests/package.rs):

- [ ] Exercise the [resolution cases](package-resolution.md#implementation-work) through public entry points,
  including refresh after source, directory, overlay, or root changes.
- [ ] Reuse semantic results across supported compilation configurations, with invalid external contracts rejected
  before target-specific preparation.
- [ ] Pair valid role contracts with rejected executable and library boundaries across public callers.
- [ ] Test requested and unrelated relationship diagnostics under the selected policy.
  Begin execution and replace artifacts after all required preparation succeeds.

Future dependency acquisition, version resolution, and source lockfiles build on this local context and identity model.
The [package roadmap](../../DESIGN.md#current-limitations) records that scope.
