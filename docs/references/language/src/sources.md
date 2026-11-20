# Sources, Imports, and Entry

## Abstraction Levels

Zydeco's [term-oriented composition](conventions.md#term-oriented-composition) gives each term static semantics
in a context and composes terms through ordinary language constructs.
The following abstractions connect that semantic structure to source reuse and independent compilation.

| Abstraction | Responsibility |
| --- | --- |
| Project | Available packages, discovery roots, and package-name bindings |
| Package | A term selected for distribution and reuse, with identity, role, and relationships |
| Compilation unit — optional at package level | Establish a complete external contract for independent compilation |
| Semantic unit | Establish a term's meaning in context: resolved names, classifier, and provenance |

A semantic unit is a term considered with its analysis context.
Processing source into its semantic form includes lexing, parsing, name resolution, and type checking.
Semantic units compose through the language's binding, abstraction, application, and import rules;
independent source selection retains the [source-boundary requirements](#source-boundaries).
One semantic unit can participate in several compositions and supported compilations.
Its meaning depends on its source inputs and resolution context; target and optimization choices belong
to the consuming compilation.

A compilation unit supplies the external contract needed to emit a selected package independently.
The [source-package roles](sources/packages.md#source-packages) describe these optional contracts.

The compiler reference maps these abstractions
to their [implementation representations](../../compiler/src/sessions-and-queries.md#implementation-representations).
Package context, source selection,
and import identity follow the [package namespace rules](sources/packages.md#package-hierarchy).
Source text can retain [documentation annotations](meta-annotations.md#source-documentation).

## Source Boundaries

A source file contains one complete term.
After imports and any companion annotation are assembled, each source synthesizes its classifier
from its own lexical bindings, explicit imports, and [default vocabulary](sources/core-prelude.md).
Provider bindings are always explicit.
Provider inference closes before comparison with import-site expectations.
An independently selected term follows the same binding and inference boundary.

Imports, companion signatures, and the closed default prelude dependency form an acyclic graph.
Imported computations execute at every dynamic occurrence.
Package context and sharing follow the [resolution model](sources/packages.md#copy-resolve-merge-analyze).

A `foo.zy` implementation may have an independently checked `foo.zyi` type companion.
The pair behaves as an annotation of the implementation by that type.
Companions can import other sources and be imported themselves.
Source terms supply their imports and parameters explicitly;
[package roles](sources/packages.md#source-packages) describe optional compilation contracts.

`@[typeof] @(import("library.zy"))` extracts the provider's complete classifier.
For a builder, querying a particular result requires applying the builder in the operand.
A companion may query another acyclic source; querying its own implementation creates a rejected import cycle.
Classifier queries couple a signature to the inspected implementation; use an explicit public signature
when its contract should remain stable across implementation changes.

`check` accepts kinds, types, values, and computations.
`run` and executable builds require a computation accepting the host Builtin packed value
and ending in its `OS` protocol:

```zydeco
param (/stdio; /process) : @(import("../../lib/std/builtin.zy")) in
! stdio/write_line "hello, world!" { ! process/exit 0 }
```

The REPL stores complete terms as numbered inputs; `@(import(1))` imports input 1,
while `@(import("1"))` addresses a file.
Its root commands are `@[type] e`, `@[run] e`, `@(help)`, and `@(quit)`.
By default it inspects kinds/types and evaluates values or directly returning computations.
Explicit execution can supply Builtin.
REPL evaluation captures output and uses empty stdin and arguments.
