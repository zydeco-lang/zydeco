# Package Namespaces and Resolution

A package is a term selected for distribution and reuse, inspired
by Zydeco's [term-oriented design](../references/language.md#term-oriented-composition).
A package definition is a meta annotation on a term, so it establishes a package context
for the lexical extent of that term.
Package resolution instantiates source in that context, merges identical resolved copies,
and gives each resulting term semantics once.

This proposal develops the [abstraction levels](../references/language.md#abstraction-levels)
through the package hierarchy, opaque roots, and source instantiation.
The [package-management plan](package-management.md) covers frontend integration and operation policy,
and the [documentation proposal](documentation.md) develops consumers of the resulting semantic units.

## Package Hierarchy

The package hierarchy is a tree of naming contexts.
A point in the tree can serve as a namespace prefix, a package definition site, or both.
The resolution run supplies each source instance's entry package context;
enclosing package-definition annotations establish their annotated terms' package contexts.
For example:

```text
<root>
└── std
    ├── data
    │   └── smoke
    └── text
```

Defining `data` in the lexical scope of `std` associates its term with `/std/data`.
The prefix records the naming context at the definition site.

### Paths and Package Context

Package namespace paths are inspired by filesystem paths.
The package context is the namespace from which relative package paths are resolved.
An ordinary path starts from the package context, `.` denotes that context,
`..` denotes its parent, and a leading `/` starts from the root.
All package paths follow this same rule, including paths in definitions, imports, and relationships.

| Package Context | Package Path | Target |
| --- | --- | --- |
| `/std` | `data` | `/std/data` |
| `/std/data` | `.` | `/std/data` |
| `/std/data/smoke` | `..` | `/std/data` |
| `/app` | `../std/data` | `/std/data` |
| Any package context under the same root | `/std/data` | `/std/data` |

Relative and absolute spellings identify the same declaration.
Normalizing redundant steps such as `./data` and `data/../data` gives one canonical relative path
from a given package context to a target, and one canonical absolute path from the root.

Package paths use the lexical package context; quoted file paths use the referencing file's location.
Thus `@(import(/std/data))` uses a package path, while `@(import("data.zy"))` uses a file path.
Both select source for the same instantiation process.

### Lexical Scope of Package Context

A package-definition meta annotation resolves its name in the enclosing package context.
That name establishes the package context for its relationship arguments and annotated body.
Sibling terms retain the enclosing package context.

```zydeco
@[package(library, name(std))]
(
  @[package(library, name(data))] (),
  @[package(library, name(text))] ()
)
```

At a project's source root, `std` resolves to `/std`.
Both `data` and `text` are defined in the lexical scope of that annotation,
so their names resolve to `/std/data` and `/std/text`.
Their annotated bodies have package contexts `/std/data` and `/std/text`, respectively.

Relationship arguments use the same package-path rules in their lexical package context.
For example, a test named `/std/data/smoke` can use `of(..)` to identify `/std/data`.

An imported copy starts in the package context supplied by its route through the resolution run.
Package definitions inside that copy establish lexical scope for their annotated terms in the same way.
The importer's surrounding terms retain their enclosing package context.

### The Opaque Root

The root is an opaque name: `/std/data` denotes `<root>/std/data`.
The leading slash supplies that root.

Two components describe package-path resolution:

| Component | Meaning |
| --- | --- |
| Root `ρ` | The opaque namespace denoted by leading `/` |
| Package Context `ν` | The namespace used by relative paths, including `.` |

At a project's initial source root, `ν = ρ`.
Package definitions establish an inner `ν`; resolution supplies the context for each imported instance.

Giving the root a name makes registering one project inside another a namespace substitution.
Registering project B under A's `/vendor/b` substitutes:

```text
ρB ↦ ρA/vendor/b
```

B's authored `/std/data` then denotes `ρA/vendor/b/std/data`.
This substitution supplies the semantics of project registration and renaming.
The implementation plan will define its configuration syntax.

### Unnamed Packages Have Opaque Names

An unnamed package receives an opaque name for its declaration within an instantiation.
For example, `@[package(test)]` in package context `/std` establishes the package context `/std/α`,
where `α` is explanatory notation for that name.

Inside the package, `.` refers to `/std/α`.
Declaring `data` produces `/std/α/data`, addressed as `data` from that package context.

Every namespace point has an absolute semantic path.
Package paths obtain opaque components from their resolution context: absolute paths use the root,
and relative paths use the package context.
Parent steps follow the namespace tree from that context.

## Resolution Runs and Source Copies

A package context depends on how the package was reached in this resolution run.
Each import route produces a copy of the file or package on the resolution graph, with its entry package context.
Within the copy, package annotations determine lexical scope through the term structure.
The rule applies uniformly to source files, named and unnamed packages, libraries, tests, and programs.
Several importers can supply the same context, and different contexts can lead to identical resolved contents.

Each copy participates in a composition as a semantic unit. Package resolution supplies its package context;
the [source-boundary rules](../references/language.md#source-boundaries) govern ordinary lexical bindings and inference.

## Copy, Resolve, Merge, Analyze

Compiler sharing follows resolved file and package imports:

**Copy → resolve imports for this run → merge identical resolved packages → analyze once per merged node.**

1. **Instantiate.** Each route to a file or package produces a separate candidate instance.
   Its import route supplies the entry package context.
2. **Resolve.** Resolve each instance's package definitions and imports in their lexical package context.
3. **Merge.** Once dependencies are resolved and themselves merged, merge candidates with identical contents
   and dependency targets.
4. **Analyze.** Give each resulting node semantics once.

For example, two copies of a file containing `@(import(data))` can merge
if their respective `data` targets ultimately become the same resolved package and their remaining contents agree.
Different resolved dependencies produce distinct package nodes.

The comparison is structural equality of resolved inputs.
Source loading reads the syntax needed to identify definitions and imports.
The acyclic import graph orders merging from providers toward consumers: compare dependency targets
through their merged nodes, then compare the remaining contents.
Semantic-unit analysis processes the resulting graph.

Local semantic declaration identities are created after merging.
Merged copies share those identities; distinct resolved instances receive their own.
Namespace bindings record which merged packages their paths select.
Merged nodes retain import routes and source locations as provenance.

Resolved contents include the term's companion signature and relevant meta annotations.
Consumers use the retained roles, relationships, documentation, names, and origins.
Copies that merge can share a binding; conflicting resolved definitions produce a diagnostic with both origins.

An imported computation executes at each dynamic occurrence.
A semantic unit supports several compilations through the package's declared external contract.

## Shared Selection and Documentation

Package operations and documentation use the same package resolution and semantic-unit queries.
A package path selects a package in the resolved graph.
A semantic selector addresses a subject within its checked interface, such as a field or a function result.
The [documentation work](documentation.md#subjects-selectors-and-published-anchors) develops presentation
and stable links from the selected semantic subjects.

Documentation queries, links, and example workers use the analyzed instance's resolution context and provenance.
They reproduce its meaning and identify the declaration or imported copy being inspected.
Reference output chooses pages and anchors from the selected semantic subjects.

## Implementation Work

The [package-management plan](package-management.md) integrates this model with project preparation,
source selection, and compilation-unit validation.
Represent candidate instances, resolved dependencies, and merged package nodes explicitly.
Analysis and cache identity follow the merged graph and its source inputs.

The first implementation should exercise the following cases together:

| Case | Expected Result |
| --- | --- |
| Relative, absolute, self, and parent paths to the same point | One resolved target after normalization |
| A prefix used solely as a naming context | Named children are available through their package paths |
| Nested and sibling package definitions | Each name uses the enclosing package context; each body and relationship uses the declared package context |
| An unnamed definition with a named child | Relative paths reach the opaque package and its child |
| A project root substituted under another project's prefix | Authored absolute paths follow the substituted root |
| Two source copies with equal resolved contents and dependencies | One merged package and one semantic analysis |
| The same source resolving to different dependencies | Distinct resolved packages and analyses |
| Distinct resolved packages claiming the same path | A diagnostic retaining both declaration origins |
| Shared analysis with several import occurrences | Origin-aware queries and execution at each dynamic occurrence |

Expose the resolution graph so CLI, REPL, editor, and documentation consumers can select and explain the same instances.
