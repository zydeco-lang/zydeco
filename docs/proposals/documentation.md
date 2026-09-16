# Documentation publication and extensions

[C15](../references/compiler.md#documentation-subjects-and-provenance) owns documentation identity,
contract selection, member provenance, and revision recovery;
its [publication and verification contract](../references/compiler.md#documentation-publication-and-verification)
owns exposure routes and isolated static examples.
The [language reference](../references/language.md#source-documentation) provides authoring syntax;
the [compiler reference](../references/compiler.md#documentation-workflow) covers editor use and commands.
This proposal contains the command and output reconstruction, additional compiler relationships,
authoring mechanisms, and client work still needed.
The immediate direction is package-aware documentation with a public-interface model that handles the standard library.
The language reference's [abstraction levels](../references/language.md#abstraction-levels) place semantic documentation
with semantic units and package selection with projects.
The [package-management proposal](package-management.md) implements those carriers and develops operation policy;
this document develops documentation subjects, lookup, verification, and output.
Repository writing and review belong to [CONTRIBUTING](../../CONTRIBUTING.md#maintain-documentation),
and observed implementation failures
to the [documentation exposure todo](../todos/compiler-boundaries.md#documentation-exposure-collisions).

## Motivation and retained foundation

Before the implementation reset, the 2026-09-15 review found
that the bundled counter example supported all four documentation commands,
but the full `std` entry failed during public-interface expansion.
The [failure record](../todos/compiler-boundaries.md#documentation-exposure-collisions) owns the reproducer,
affected entries, and bounded repair criteria.
A usable standard-library reference is the next concrete consumer.

Retain the compiler's attachment, provenance, contract selection, instantiated types, and example-worker machinery.
Those relationships supply the semantic-unit evidence needed to identify fields
and establish which explanation follows an instantiated use.
The replacement commands query that evidence for lookup and verification, then assemble output when requested.
The required redesign concerns selection, public exposure, and operation dependencies.

## Package-aware selection

- [ ] Route documentation commands through shared package selection and semantic-unit analysis.
  Reuse `SourceReference`, package bindings, normalized entry identity, and exact term selection;
  consume the shared project context developed in the [package proposal](package-management.md#shared-project-context).
- [ ] Support one selected source package through either its name or its file path first.
  Preserve selected-term documentation boundaries, companion behavior, and captured bindings for example workers.
- [ ] Define multiple-package publication only when its page organization, anchors, cross-package links,
  and guide ownership have concrete consumers and validation cases.

The intended command forms extend file-path selection with package names:

```text
zydeco doc show -p std
zydeco doc search -p std int
zydeco doc build -p std --output std.html
zydeco doc check -p std
```

Explicit selection should determine the publication scope.
Project membership makes a package available; it does not require publishing that package.
Dependency analysis likewise does not require a separate API page for every imported entry.
Keep source-package names distinct from member selectors and preserve explicit guide selection for the first milestone.
A new package role or documentation relationship is not required for this integration.

## Subjects, selectors, and published anchors

The removed exposure traversal combined structural enumeration, public-route assignment, and duplicate rejection.
Unnamed components could share a parent path and independently append result steps, creating collisions such as `()/()`.
The shared rule motivating the change is that a semantic subject can exist without a unique public route.

Separate these responsibilities:

| Responsibility | Question |
| --- | --- |
| Subject identity | Which checked interface occurrence is described, including distinct anonymous branches? |
| Public selector | Does this authored field/result path identify one subject? |
| Presentation | How are signatures, named members, binders, and anonymous structure displayed? |
| Published anchor | Which addressable section receives a stable link? |

- [ ] Retain anonymous interface occurrences without forcing each one into the public-selector namespace.
  Initially render such structure within its containing signature or section;
  independently address uniquely selectable subjects.
  Preserve useful, unambiguous result selectors.
- [ ] Expose named type members through compiler-recorded binder structure and projection evidence.
  Printed names or local binder hints alone do not establish a public member route.
- [ ] Preserve distinct subjects and reject ambiguous selectors with a diagnostic about the requested path.
  Do not choose the first collision, merge unrelated subjects, or expose transient arena IDs as stable URLs.
- [ ] Preserve stable named routes through formatting and unrelated implementation edits.
  Decide any anonymous-anchor extension separately from compiler subject identity.

The exact internal subject representation and any additional selector syntax remain open.
The first implementation must account for products, existential interfaces, named type components,
generic results, and recursion without executing computations that construct packed values.

## Targeted lookup and independent verification

The removed implementation constructed the whole reference before every subcommand
and enumerated all exposure routes before selecting a member-link target.
An unrelated collision could therefore block a specific lookup or source-example check.

- [ ] Introduce a targeted subject query that resolves the requested selector
  without requiring all other routes to have independently publishable anchors.
  Reuse the same semantics for source links and command lookup.
- [ ] Verify source links and opted-in examples from the selected analysis without requiring HTML publication.
  Guide member links still require public-target resolution; their validation must remain explicit.
- [ ] Keep publication dependent on valid links and established exposure rules.
  Keep example verification an explicit operation, with failures attributed to the authored example or guide.

## First standard-library milestone

The acceptance target is a searchable, compiler-grounded standard-library reference with checked examples.
Complete the package boundaries above, repair exposure and targeted lookup,
then add representative authored prose to the standard library.
A successful build with no explanations is insufficient evidence of documentation usefulness.

- [ ] Exercise `show`, `search`, `build`, and `check` on the full `std` entry and its data,
  memory, text, numeric, and system entry points.
  Use file paths now and package selection when implemented.
- [ ] Document representative generic types, named fields, and a packed value with abstract witnesses;
  verify that public contracts preserve abstraction and that selected member queries reach their explanations.
- [ ] Retain the anonymous-branch collision reproducer alongside a named counterpart.
  Add missing and ambiguous selector cases, shadowed names, unrelated same-labeled members, and recursive interfaces.
- [ ] Pair valid examples and links with expected compiler rejections, wrong diagnostic codes or positions,
  missing imports, invalid links, worker failures, and timeouts.
  Failed builds must not replace existing output.
- [ ] Check formatting-stable named anchors and consistent semantic targets across CLI, HTML, and editor consumers.

The remaining sections retain later extensions.
Runtime examples, programmable widgets, and elaborate publication configuration follow this milestone.

## Richer subjects and interfaces

- Specify a typed schema for annotation options and attachment to parameters and arms.
  New attachment sites need exact provenance through desugaring and checking.
- Recover implementation prose beneath a docless explicit field contract only through a bounded value-origin relation.
  Current projection provenance identifies the contract; binding and alias fallback works
  where an implementation edge is already known.
  Similar labels or structural types cannot supply that missing relation.
- Add separate contract/implementation navigation, clickable type subterms,
  and expected-type discovery using semantic subjects rather than parsing display strings.

## Publication and client extensions

- Define release-version URLs, authored stable anchors for anonymous sections,
  and an explicit internal publication mode.
  Preserve current named routes and distinguish local inspection from published exposure.
- Decide guide-discovery configuration and declared lexical source contexts for guide links.
- Support panel clients beyond VS Code and investigate signature help for Zydeco application forms.
  Negotiate client capabilities and retain source-revision checks for every action.
- For editable source generation, establish the required consumer
  and use the [rendering design questions](../ideas/typed-source-generation.md).
  Typed rendering alone does not promise a reparseable annotation.

## Composed and executed examples

Composed setup must remain visible and copyable.
Errors in setup and errors in the example retain their distinct authored locations;
changing any imported input invalidates the relevant verification identity.
An edited scratch example must remain distinct from verification of the published source.

Runtime examples need declared capabilities, isolated fixtures, cancellation, bounded resources, and explicit execution.
A thunk classifier establishes neither purity nor termination.
Reference generation must not silently execute examples or acquire dependencies.
Decide typed value comparisons or declared input/output expectations without reparsing display strings.
Define failure, timeout, and partial-effect reporting before adding a run fence to the current static checker.
Author-programmable widgets would introduce additional execution and portability decisions
and are not implied by this runner.

## Validation criteria

Pair every new origin relationship with shadowed names, unrelated same-labeled fields,
nested versus immediate RHS prose, distinct existential openings, docless contracts, and stale revisions.
Retain Unicode locations and stable named routes.
Compare semantic targets across renderers while allowing their presentation to differ.
For execution, pair successful examples with missing capabilities, cancellation, exhaustion, and unexpected effects.
These checks extend the existing [semantic](../../lang/session/src/source/documentation/semantic/tests.rs)
and [example](../../lang/session/src/source/documentation/examples/tests.rs) regressions.
