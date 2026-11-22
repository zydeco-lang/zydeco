# Source Packages and Resolution

## Source Packages

A package is a term selected for distribution and reuse.
It may be supplied as source or, when its external contract supports independent emission, as compiled artifacts.
This distribution role is independent of the term's language-level structure: a source package can supply a type term,
a [packed value](../polymorphism-and-packed-values.md#existentials-and-packed-values),
or a function that constructs one.
A package-definition meta annotation selects a term and records its role:

```zydeco
-- library.zy
@[package(library, name(example/math))]
(#answer = 42)
```

The first argument chooses a role:

| Role | Independently emitted boundary |
| --- | --- |
| `library` | Source interface only; its implementation is incorporated into consumers. |
| `library(c, export(...), ...)` | Explicit [C exports](../foreign-interfaces.md#compiled-libraries-and-c-exports). |
| `library(zydeco)` | A [native initializer](../foreign-interfaces.md#native-zydeco-units) returning the prepared library value. |
| `binary` | Executable Builtin/OS boundary. |
| `test` or `test(of(...))` | The same executable boundary, with test associations. |

A compilation unit is an independently selected source term with a complete external entry contract.
Binary and test roles already establish that contract; a compiled library declares its own.
The compiled-library contract exposes the term through the C or Zydeco ABI, within the supported entry profile.
There is no additional `unit` annotation or package argument.
Naming makes the term selectable by name; independent emission additionally requires the external contract.
The [compiler reference](../../../compiler/src/native-emission.md#compilation-unit-preparation-and-artifacts)
explains how the designs of compilation units, FFI, and package management evolve together.

A test is itself a package: `test(of(...))` records its subjects, and plain `test` describes an independent test.
Subsequent arguments supply an optional `name(...)` and typed relationships.
Source libraries may expose any source classifier.
Compilation-unit validation checks the declared compiled-library interface or the executable Builtin/OS contract.

### Relationships and Operations

Relationships associate packages with tests and other subjects:

| Relationship | Meaning |
| --- | --- |
| `test(target)` | A companion test associated with the declaring package |
| `of(subject, ...)` under `test(...)` | The packages tested by this test |

Relationship targets follow the shared [package-path rules](#paths-and-package-context).
Relationship-driven test selection remains an [open idea](../../../../ideas/README.md#project-context-inspection).
The [CLI execution workflow](../../../../../cli/README.md#run-and-test-programs) governs backend selection
and test execution, while [Execution Profiles](../execution-profiles.md) define backend runtime contracts.

## Package Hierarchy

A package definition is a meta annotation on a term, so it establishes a package context
for the lexical extent of that term.
Package resolution instantiates source in that context, merges identical resolved copies,
and gives each resulting term semantics once.

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
Its user-facing configuration syntax remains an [open idea](../../../../ideas/README.md#project-context-inspection).

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
the [source-boundary rules](../sources.md#source-boundaries) govern ordinary lexical bindings and inference.

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
Consumers use the retained roles, relationships, attached text, names, and origins.
Copies that merge can share a binding; conflicting resolved definitions produce a diagnostic with both origins.

An imported computation executes at each dynamic occurrence.
A semantic unit supports several compilations through the package's declared external contract.
