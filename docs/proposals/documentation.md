# Documentation Design

Documentation tooling is deferred while its design is reconsidered.
Only [`@[doc]` and adjacent text attachment](../references/language.md#source-documentation) remain implemented.
The previous semantic indexes, selectors, publication commands, example workers,
editor integrations, and their tests have been removed.
This document records open questions, not an adopted replacement design.

## Questions to Settle

- What is a documentation subject in a term-oriented language, including anonymous interface branches,
  type members, bindings, and arbitrary expressions?
- Which compiler-recorded relationships justify sharing prose across aliases,
  imported terms, public contracts, and instantiated uses?
  How should missing provenance be presented?
- How do subject identity, a public selector, and a published URL relate without requiring every subject
  to have a unique name?
- Which source and package context belongs to each link, example, and editor action?
  How should imports, source copies, overlays, and revisions affect that context?
- Which facts should lookup, verification, CLI output, and editor presentation share?
  How can verification remain independent of publication layout?
- What should a useful standard-library reference display, and how will readers navigate large signatures,
  anonymous branches, generic types, and authored explanations?

## Evidence for a Future Design

Before adopting an implementation, specify accepted and rejected behavior across its public consumers.
Use representative standard-library interfaces alongside small examples with named and anonymous members,
public contracts, imports, shadowing, and generic uses.
If example checking returns, require source comments and guides to use the same verification rules,
and test that CLI and editor requests preserve the same package context.
If publication returns, check both semantic targets and ordinary relative links at their authored origins.
Keep runtime examples, additional annotation options, and client-specific interaction open until their semantics
and required scope are established.

[Package resolution](../references/language.md#package-hierarchy) supplies the existing source-context model.
Repository documentation maintenance remains in [CONTRIBUTING](../../CONTRIBUTING.md#maintain-documentation).
