# Documentation extensions

[C15](../references/compiler.md#documentation-subjects-and-provenance) owns implemented documentation identity,
contract selection, member provenance, and revision recovery;
its [publication and verification contract](../references/compiler.md#documentation-publication-and-verification)
owns exposure routes and isolated static examples.
The [language reference](../references/language.md#source-documentation) provides authoring syntax;
the [compiler reference](../references/compiler.md#documentation-workflow) covers editor use and commands.
This proposal contains the additional compiler relationships, authoring mechanisms, and client work still needed.
It concerns Zydeco's documentation feature.
Repository writing and review belong to [CONTRIBUTING](../../CONTRIBUTING.md#maintain-documentation),
and observed documentation disagreements to the [drift list](../todos/reference-drift.md).

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
