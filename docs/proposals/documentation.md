# Documentation Publication and Extensions

This proposal develops package-aware documentation: presentation, lookup, verification, and reference output.
The [abstraction levels](../references/language.md#abstraction-levels) place semantic documentation with semantic units.
The [namespace and resolution proposal](package-resolution.md) supplies package paths, source instantiation, merging,
and shared semantic selectors; the [package-management plan](package-management.md) integrates them across frontends.

[C15](../references/compiler.md#documentation-subjects-and-provenance) describes documentation identity,
contract selection, provenance, and revision recovery.
[Example verification](../references/compiler.md#verifying-documentation-examples) describes the retained static checks.
The [language reference](../references/language.md#source-documentation) provides authoring syntax;
the [compiler reference](../references/compiler.md#documentation-workflow) covers editor use.
Repository writing and review follow [CONTRIBUTING](../../CONTRIBUTING.md#maintain-documentation).

## Motivation and Retained Foundation

A searchable standard-library reference provides the first milestone.
Build on the retained attachment, provenance, contract selection, instantiated types, and example-worker machinery.
Commands query those facts to identify subjects, retrieve explanations, and verify examples.
The [exposure record](../todos/compiler-boundaries.md#documentation-exposure-collisions) supplies the `std` reproducer
and repair criteria.

## Package-Aware Selection

Use the [shared selection model](package-resolution.md#shared-selection-and-documentation) for package paths,
semantic selectors, and the selected instance's resolution context and provenance.

- [ ] Route documentation commands through shared project preparation and semantic-unit queries,
  as developed in the [package proposal](package-management.md#shared-project-context).
- [ ] Let explicitly selected packages and guides determine the publication scope.

## Subjects, Selectors, and Published Anchors

The [shared semantic queries](package-resolution.md#shared-selection-and-documentation) supply subjects
and selector resolution.
Documentation presents the results and assigns stable output anchors.

- [ ] Render each interface occurrence, including anonymous branches, within its containing signature or section.
- [ ] Render named type members using compiler-recorded binder structure and projection evidence.
- [ ] Present selector diagnostics at their authored paths and derive stable URLs from resolved public routes.
- [ ] Preserve stable named routes through formatting and unrelated implementation edits.
  Decide any anonymous-anchor extension separately from compiler subject identity.

## Targeted Lookup and Independent Verification

Targeted lookup resolves a requested semantic subject directly.
Verification consumes source analysis, while reference generation assembles presentation and anchors.

- [ ] Use the same targeted subject query for source links and command lookup.
- [ ] Verify source links and opted-in examples from the selected analysis.
  Resolve guide member links through the selected public interface.
- [ ] Build output from validated links and established exposure routes.
  Offer example verification as an explicit operation, with diagnostics at the authored example or guide.

## First Standard-Library Milestone

Produce a searchable, compiler-grounded standard-library reference with authored explanations and checked examples.
Integrate package selection, subject queries, and prose for representative interfaces.

- [ ] Exercise `show`, `search`, `build`, and `check` on the full `std` entry and its data, memory, text,
  numeric, and system entry points.
- [ ] Document representative generic types, named fields, and a packed value with abstract witnesses;
  verify that public contracts preserve abstraction and that selected member queries reach their explanations.
- [ ] Retain the anonymous-branch collision reproducer alongside a named counterpart.
  Add missing and ambiguous selector cases, shadowed names, unrelated same-labeled members, and recursive interfaces.
- [ ] Pair valid examples and links with expected compiler rejections, wrong diagnostic codes or positions,
  missing imports, invalid links, worker failures, and timeouts.
  Replace output after a successful build.
- [ ] Check formatting-stable named anchors and consistent semantic targets across CLI, HTML, and editor consumers.

The following extensions build on this milestone.

## Richer Subjects and Interfaces

- Specify a typed schema for annotation options and attachment to parameters and arms.
  New attachment sites need exact provenance through desugaring and checking.
- Recover implementation prose beneath a docless explicit field contract through a bounded value-origin relation.
  Use projection provenance for the contract and a recorded implementation edge for binding and alias fallback.
- Add separate contract/implementation navigation, clickable type subterms, and expected-type discovery
  through semantic subjects.

## Publication and Client Extensions

- Extend publication to multiple packages through concrete page layouts, anchors, cross-package links,
  guide ownership, and validation cases.
- Define release-version URLs, authored stable anchors for anonymous sections,
  and an explicit internal publication mode.
  Preserve current named routes and distinguish local inspection from published exposure.
- Decide guide-discovery configuration and declared lexical source contexts for guide links.
- Support panel clients beyond VS Code and investigate signature help for Zydeco application forms.
  Negotiate client capabilities and retain source-revision checks for every action.
- For editable source generation, establish the required consumer
  and use the [rendering design questions](../ideas/typed-source-generation.md).
  Specify parsing and typing round trips for generated annotations.

## Composed and Executed Examples

Present composed setup as visible, copyable source and attribute setup and example diagnostics
to their authored locations.
Verification identity includes imported inputs; edited scratch examples receive their own verification identities.
Example workers use the [shared resolution context](package-resolution.md#shared-selection-and-documentation).

Reference generation uses static analysis.
Runtime examples use an explicit runner with declared capabilities, isolated fixtures,
cancellation, resource bounds, and dependency preparation.
Specify typed value comparisons or declared input/output expectations,
together with failure, timeout, and partial-effect reports.
Programmable widgets have their own execution and portability design.

## Validation Criteria

Pair every new origin relationship with shadowed names, unrelated same-labeled fields,
nested versus immediate RHS prose, distinct existential openings, docless contracts, and stale revisions.
Retain Unicode locations and stable named routes.
Compare semantic targets across renderers while allowing their presentation to differ.
For execution, pair successful examples with missing capabilities, cancellation, exhaustion, and unexpected effects.
These checks extend the existing [semantic](../../lang/session/src/source/documentation/semantic/tests.rs)
and [example](../../lang/session/src/source/documentation/examples/tests.rs) regressions.
