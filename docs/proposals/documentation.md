# Documentation Publication and Extensions

This proposal develops package-aware documentation: presentation, lookup, verification, and reference output.
The [abstraction levels](../references/language.md#abstraction-levels) place semantic documentation with semantic units.
The [namespace and resolution proposal](package-resolution.md) supplies package paths, source instantiation, merging,
and shared semantic selectors; the [package-management plan](package-management.md) integrates them across frontends.

[C15](../references/compiler.md#documentation-subjects-and-provenance) describes documentation identity,
contract selection, provenance, and revision recovery.
Its [publication and verification section](../references/compiler.md#documentation-publication-and-verification)
describes exposure routes and static examples.
The [language reference](../references/language.md#source-documentation) provides authoring syntax;
the [compiler reference](../references/compiler.md#documentation-workflow) covers editor use and commands.
Repository writing and review follow [CONTRIBUTING](../../CONTRIBUTING.md#maintain-documentation).

## Motivation and Retained Foundation

A searchable standard-library reference provides the first milestone.
Build on the retained attachment, provenance, contract selection, instantiated types, and example-worker machinery.
Commands query those facts to identify subjects, retrieve explanations, and verify examples.
The [exposure record](../todos/compiler-boundaries.md#documentation-exposure-collisions) supplies the `std` reproducer
and repair criteria.

## Package-Aware Selection

- [ ] Route documentation commands through shared package selection and semantic-unit analysis.
  Reuse source selection, resolved package bindings, merged identity, and exact term selection;
  consume the shared project context developed in the [package proposal](package-management.md#shared-project-context).
- [ ] Support one selected source package through either its name or its file path first.
  Preserve selected-term documentation boundaries, companion behavior, and the requesting instance's resolution context
  and provenance for example workers.
- [ ] Extend publication to multiple packages through concrete page layouts, anchors, cross-package links,
  guide ownership, and validation cases.

The intended command forms extend file-path selection with package names:

```text
zydeco doc show -p std
zydeco doc search -p std int
zydeco doc build -p std --output std.html
zydeco doc check -p std
```

Explicit selection should determine the publication scope.
The resolved graph makes packages available, and the documentation operation chooses which entries to present.
Shared analysis retains the source and import origins needed for that presentation.
Use the [shared selection model](package-resolution.md#shared-selection-and-documentation) for package paths
and semantic selectors, with explicit guide selection for the first milestone.

## Subjects, Selectors, and Published Anchors

Shared compiler queries identify semantic subjects and resolve selectors.
Documentation presents those subjects and assigns stable output anchors.

| Responsibility | Question |
| --- | --- |
| Subject identity | Which checked interface occurrence is described, including distinct anonymous branches? |
| Public selector | Does this authored field/result path identify one subject? |
| Presentation | How are signatures, named members, binders, and anonymous structure displayed? |
| Published anchor | Which addressable section receives a stable link? |

- [ ] Give each anonymous interface occurrence an identity and render it within its containing signature or section.
  Provide routes for uniquely selectable subjects, including result selectors.
- [ ] Expose named type members through compiler-recorded binder structure and projection evidence.
- [ ] Report missing and ambiguous selectors at their authored paths.
  Preserve each subject's identity and derive stable URLs from public routes.
- [ ] Preserve stable named routes through formatting and unrelated implementation edits.
  Decide any anonymous-anchor extension separately from compiler subject identity.

Specify the subject representation and selector syntax using products, existential interfaces,
named type components, generic results, and recursion.
Inspect their checked interfaces structurally.

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

- [ ] Exercise `show`, `search`, `build`, and `check` on the full `std` entry and its data,
  memory, text, numeric, and system entry points.
  Use file paths now and package selection when implemented.
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
Example workers capture the originating instance's root, package context, and resolved inputs.
Checking and scratch editing then reproduce the requesting run's package-path meaning.

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
