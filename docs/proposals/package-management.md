# Source-Package Structure and Project Context

This plan implements the [abstraction levels](../references/language.md#abstraction-levels):
project, package, compilation unit, and semantic unit.
Package paths, source selection, instance identity,
and graph construction follow the [package namespace rules](../references/language.md#package-hierarchy).
This plan covers frontend integration, compilation-unit validation, and operation policy.

The [source-package reference](../references/language.md#source-packages) describes the existing implementation.
The [documentation design questions](documentation.md) concern possible future consumers of these semantic units.

## Motivation and Starting Point

Project preparation connects source selection, code dependencies, and operation planning.
It exposes each selected term's identity, classifier, lexical scope, and any declared external contract consistently
across frontends.

The implementation supplies project preparation, source instantiation and merging,
compiler analysis, external-contract checks and regression coverage.
Build operation interfaces on these components.

## Implementation Representations

The compiler reference's [implementation map](../references/compiler.md#implementation-representations)
identifies the existing representations.
The planned Rust interfaces are:

- [ ] Introduce `SemanticUnit` around a resolved, merged term, its analysis inputs, and its semantic results.
  Reuse the facts and checked representations currently exposed by `ProgramAnalysis` and `CheckedProgram`.
  Preserve rejected analyses and useful diagnostics for editor consumers.
- [ ] Rebuild `CompilationUnit` from the selected package and its checked external contract,
  reusing `ExecutableProgram`, `LibraryProgram`, and `UnitProgram` validation.
  Keep target-specific preparation downstream so one semantic result can support several compilations.

Replace affected callers together; `CompilerSession` continues to manage the implementation's inputs and caches.
Future documentation consumers remain subject to the [documentation design questions](documentation.md).

## Shared Project Context

CLI, REPL, and editor consumers share project preparation
through the [resolution model](../references/language.md#copy-resolve-merge-analyze).

- [ ] Expose project registrations and root selection through frontend configuration.
- [ ] Make the active context inspectable: selected roots, entry declaration locations,
  discovery origins, namespace substitutions, import routes, and resulting bindings should be available
  to diagnostics and project inspection.
- [ ] Define refresh responsibilities for directory membership, disk changes, overlays, and root changes.
  Follow the resolution graph's inputs when defining revision and cache identity.

## Validation and Operation Planning

Build contract validation and operation preparation on semantic-unit analysis.
Use [package resolution](../references/language.md#paths-and-package-context) for relationship targets;
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

1. Expose `CompilationUnit` validation and settle relationship-validation scope.
2. Validate the shared context through CLI and editor operations before adding future consumers.

Extend the existing [package regressions](../../lang/session/src/source/package/tests.rs),
[discovery regressions](../../lang/session/src/source/package/discovery/tests.rs),
and [CLI coverage](../../cli/tests/package.rs):

- [ ] Exercise the [resolution cases](../references/compiler.md#package-resolution-implementation)
  through public entry points, including refresh after source, directory, overlay, or root changes.
- [ ] Reuse semantic results across supported compilation configurations, with invalid external contracts rejected
  before target-specific preparation.
- [ ] Pair valid role contracts with rejected executable and library boundaries across public callers.
- [ ] Test requested and unrelated relationship diagnostics under the selected policy.
  Begin execution and replace artifacts after all required preparation succeeds.

Future dependency acquisition, version resolution, and source lockfiles build on this local context and identity model.
The [package roadmap](../../DESIGN.md#current-limitations) records that scope.

## Project Registration Configuration

`Project::with_registration` implements opaque-root substitution in the shared Rust interface.
Define its user-facing configuration syntax and entry-point conventions.
Document how tools refresh discovered inputs and retain selected import instances across project edits.
