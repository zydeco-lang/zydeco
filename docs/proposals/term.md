# Binding placement and source identity

Zydeco lets programmers place a binding where it best explains a term while the resolver schedules dependencies.
That convenience needs a stable answer to two questions: which binder does an occurrence name,
and which definitions introduce a fresh identity?
The current rules belong to [L3](../references/language.md#3-bindings-and-scope),
[L4](../references/language.md#4-classification-and-inference),
and [L12](../references/language.md#12-sources-imports-and-entry);
[C4](../references/compiler.md#c4-parsing-desugaring-and-name-resolution) owns dependency scheduling.

## Placement and identity are independent

Binding form determines transparency or generativity; its connective determines placement and scope.
Keeping these choices independent avoids making a name's meaning depend
on whether its author writes a definition before or after its use.
Ordinary `let` remains sufficient for transparent package composition; `def` supplies nominal identity
where clients must not identify distinct declarations.

Resolution chooses lexical binder identities before dependency reordering.
Scheduling may then move elaboration, but cannot change capture, shadowing, or which nominal declaration a use denotes.
This is why textual substitution and sequential declaration environments are inadequate implementation models.
Patterns introduce their names under the shared [pattern rules](../references/language.md#7-patterns-and-coverage).

A source is one complete term rather than an implicit declaration namespace.
Repeated imports of one source share that source's declarations; distinct declaration occurrences retain their identity.
A source closes its own inference obligations before an importer uses its result.
This makes independent checking meaningful and avoids letting importer order choose the meaning of a library.

## Dependency-directed elaboration

The resolver's dependency graph permits recursive components, and the checker requires the relevant sealed types
and kinds to be available when checking them.
That is the implemented admissibility boundary.
A stronger guardedness or positivity discipline is still a design decision, tracked with its evidence
in [reference drift](../todos/reference-drift.md#language-accounts).

Before adding such a discipline, distinguish the intended recursive values and types from rejected cycles,
and decide whether the criterion protects normalization, representation, or logical consistency.
Accepted and rejected examples must establish that purpose; scheduling alone does not establish any of those properties.
