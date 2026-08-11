# Declaration Removal Worklog

Status: declaration removal complete; follow-up design work remains, 2026-07-31.

## Objective

Remove the declaration sort from Zydeco and represent every source file by one complete term.
Definitions, parameters, abstraction boundaries, library interfaces, and executable bodies should all use
the ordinary term language. Files compose by importing terms and applying or unpacking them,
rather than by merging declarations into a shared namespace.

This decision also removes authored project configuration files.
The command line selects one root source file, and meta notations inside that file describe its source dependencies.
In particular,

```zydeco
@[import("path/to/file.zy")] _
```

asks the compiler to replace the annotated hole with a fresh instance of the complete term contained in the
referenced file. Relative paths are resolved from the importing file; absolute paths are accepted directly.
Imports are compile-time elaboration directives and have no residual static or dynamic meaning.

The migration proceeded additively until the maintained corpus had moved to this model.
The declaration-based driver continued to run old projects during that transition,
then was removed without transferring its package-merging semantics to the source driver.

## Settled Design

### Files denote terms

A Zydeco file contains exactly one term.
The term may classify as a kind, type, value, or computation, and may itself be a function or package.
The file format does not distinguish libraries from executables.
`zydeco check` may accept any well-classified root term, while `zydeco run` imposes the executable contract
and asks the launcher to supply the Builtin package.

An outer `begin ... end` is useful when the file contributes mobile bindings, but it is not inserted implicitly.
A mobile `that` binding must therefore have an explicit enclosing block in the same file.

### Imports are term splices

The first import form is deliberately narrow:

```zydeco
@[import("relative/or/absolute/path.zy")] _
```

The `import` meta must have exactly one path argument and must annotate a hole term.
The directive consumes the hole before ordinary hole elaboration begins.
Consequently, an imported hole never becomes a type-checking `FillId`, an inferred term, or an undefined value.

If file `F` imports path `p`, import expansion resolves `p` to a file `G` and substitutes the complete term of
`G` at that occurrence. Imported files may recursively contain imports of their own.
The compiler retains the written path for diagnostics and uses a canonical path for dependency identity,
cycle detection, and source caching.

### File boundaries are hygienic

Importing a term does not import a namespace.
An internal source boundary remains around every imported term through name resolution, with the following effects:

- Free names in the imported file cannot capture bindings from the importer.
- Bindings introduced in the imported file cannot capture occurrences in the importer.
- A mobile `that` binding cannot move across the imported file's source boundary.
- Dependencies on an importing context must be represented by `param` and supplied by ordinary application.
- Expected classifiers may still flow into the imported term during bidirectional type checking.

This gives `import` the semantics of hygienic term composition.
A distinct construct such as `include` would be required if contextual syntax insertion is ever desired.

### Every import occurrence is fresh

Canonical paths identify source templates, not shared term instances.
The compiler may read and parse a file once, but every import occurrence freshens all term, pattern,
binder, and nominal identities in the substituted instance.

For example,

```zydeco
begin
  let first = @[import("library.zy")] _ that
  let second = @[import("library.zy")] _ that
  ...
end
```

contains two lexical instances of `library.zy`.
Nominal definitions in `first` and `second` are therefore distinct.
When sharing is intended, the program imports once and reuses the resulting binding:

```zydeco
begin
  let library = @[import("library.zy")] _ that
  ...
end
```

Parsing and source caching must preserve this distinction.
Cached syntax may be cloned, but resolved or typed identities must never be shared merely because two imports
resolve to the same path.

### Import and block dependencies remain distinct

The compiler maintains two dependency structures with different meanings.

The source import graph has canonical source files as nodes.
An edge records that one file contains an import of another file.
This graph determines source discovery, cycle rejection, invalidation, and provider-first compilation scheduling.

Each `begin ... end` separately retains its binding dependency graph.
That graph orders `param`, `let`, and `def` contributions within one term and classifies recursive type SCCs.
Import scheduling must not be folded into block scheduling, and block mobility must not cross a source boundary.

Multiple import sites may point to one source-graph node.
They remain distinct occurrence sites during program assembly so that fresh substitution is preserved.

### Programs assemble into one root term

For a root file such as

```zydeco
begin
  let std = @[import("../std/std.zy")] _ that
  param builtin that
  let api = std builtin that

  ...
end
```

the import graph establishes the dependency from the root to `std.zy`.
The block graph independently establishes that `api` depends on `std` and `builtin`.
After import expansion and block elaboration, the compiler holds one root term.
The static and dynamic pipelines consume that term directly.

The standard library is an ordinary imported term that accepts Builtin and returns its public package.
Builtin itself remains a distinguished host-provided package.
For execution, the launcher constructs the concrete Builtin package, applies it to the assembled binary,
and runs the resulting `OS` computation.

## Current Compiler Position

Uniform term composition already provides the central replacement for declarations within one term.
A `begin` block collects mobile `param`, `let`, and `def` bindings, resolves their whole-block scope,
retains their dependency DAG and SCC decomposition, and elaborates them to ordinary CBPV terms.
The implemented fragment supports type and value bindings with computation or value tails,
as well as type bindings with type tails. A pure value tail uses an administrative scoped binding
in typed syntax; dynamics interprets its lexical environment and Stack IR folds it into the existing value plan.
Type bindings erase after checking when the tail is a value.

The compiler pipeline now carries one explicit root term from parsing through name
resolution and type checking. Nested blocks retain their `BindingContext` only as
the dependency plan used to elaborate mobile binders into ordinary term structure.
Executable commands require the checked root to be a computation; libraries may
retain any classifier accepted by `check`.

| Layer | Current representation |
|---|---|
| Textual syntax | `SourceUnit` contains exactly one term; declaration IDs, arenas, and grammar have been removed |
| Meta syntax | Imports assemble sources, intrinsic splices insert core terms, and Builtin roles attach during checking |
| Bitter syntax | Desugaring produces only definitions, patterns, and terms |
| Scoped syntax | `ResolveSourceOut.root` identifies the root; every context binding originates at a term |
| Statics | `CheckedSource.root` is the classified term identity; no declaration or entry arena remains |
| Dynamics | `DynamicsArena` owns one linked computation root |
| Stack IR | `RootLowerer` lowers the typed computation directly; lexical `let` structure carries definitions |
| Driver | `check`, interpreter `run`, and native compilation accept one root source path |
| Editor and tests | Editor analysis and maintained fixtures parse and resolve one source term |

The package-stew driver and project-manifest discovery have been removed.
Import assembly is the only whole-program source composition path.

## Target Driver

The new driver begins with a root path rather than a project file:

```text
zydeco check path/to/root.zy
zydeco run path/to/main.zy
```

Its pipeline has two ordering phases.
Parsing is needed to discover imports, so the complete graph cannot be known before any parsing occurs.
After discovery finishes, source elaboration and assembly can proceed in provider-first topological order.

The intended pipeline is:

1. Load and parse the root file as one term.
2. Decode its meta notations and collect typed import sites.
3. Resolve every import path and recursively load newly discovered source files.
4. Construct the canonical-path source graph.
5. Reject import cycles and report the complete cycle through source spans.
6. Traverse the graph in provider-first topological order.
7. Instantiate each import site with a fresh copy of its provider term.
8. Preserve an internal source boundary around each copy during name resolution.
9. Assemble one root term with complete multi-file source provenance.
10. Type-check, lower, and evaluate or compile that root term.

The precise expansion layer may be textual or bitter syntax.
Whichever layer is chosen must support fresh identity allocation, retain spans in the originating file,
and preserve the internal source boundary until hygiene has been enforced.
Import expansion must finish before ordinary static holes are created.

The driver should use typed structures corresponding to the semantic stages, for example:

```text
SourceGraph
  root source
  parsed source units indexed by canonical path
  typed import sites
  source dependency DAG

ProgramAssembly
  fresh import instances
  combined source provenance
  one root term
```

These names are provisional, but the separation is not.
A source node represents cached file contents; an import site represents a fresh use of those contents;
and a program assembly represents the single term passed to the language pipeline.

Known meta notations should likewise be decoded into typed directives near parsing.
The existing generic surface notation may remain useful as concrete syntax,
but later passes should receive an `ImportDirective` with a validated path rather than inspecting
`Meta.stem == "import"` and interpreting nested strings repeatedly.

During migration, the declaration-based `BuildSystem` coexisted as a legacy adapter.
Stage 6 removed that adapter after every maintained source had moved to the root-term path.

## Staged Work

### 0. Define term source units and typed import metadata

- [x] Introduce a typed source-unit representation whose root is one textual term.
- [x] Preserve the legacy declaration `TopLevel` temporarily; remove it in stage 6.
- [x] Distinguish string literals, identifiers, and meta applications in the metadata syntax.
- [x] Decode `import` into a typed directive containing its source span and path.
- [x] Require `import` to have one string path argument and to annotate `_`.
- [x] Reject malformed import directives before ordinary desugaring or hole checking.
- [x] Add parser tests for relative paths, absolute paths, malformed arguments, and non-hole payloads.

This stage establishes the source language contract without yet reading another file.

### 1. Build the source import graph

- [x] Add a root-file loader independent of `LocalPackage`.
- [x] Resolve relative paths against the importing file's directory.
- [x] Preserve the written path while indexing sources by canonical path.
- [x] Recursively parse imports and record every import occurrence.
- [x] Cache loaded and parsed source templates by canonical path.
- [x] Build the source dependency graph after discovery.
- [x] Reject self-imports and longer cycles with a path-and-span diagnostic.
- [x] Retain provider-first topological order for later assembly.
- [x] Track source hashes for invalidation without assigning term identity from those hashes.

Tests should cover chains, diamonds, repeated edges, missing files, invalid source files, symlinks,
relative imports nested at several directory levels, and absolute imports.

### 2. Assemble hygienic fresh terms

- [x] Introduce an internal source-boundary representation for imported terms.
- [x] Clone and freshen the imported syntax at every import site.
- [x] Recursively substitute providers in dependency order.
- [x] Prevent free names in an imported term from resolving in its importer.
- [x] Prevent mobile bindings from crossing a source boundary.
- [x] Allow expected classifiers to pass through the boundary.
- [x] Preserve imported-file spans on every cloned term.
- [x] Consume import directives before desugaring and erase source boundaries during type checking.
- [x] Produce one assembled root term and source map.

Freshness tests must distinguish two imports of the same nominal definition.
A companion test should import once, bind once, and demonstrate explicit sharing.
Hygiene tests should reject accidental free-name capture and cross-file `that` mobility.

The implementation clones cached textual syntax because this is the earliest layer with typed import metadata
and complete source spans. Each occurrence receives fresh textual identities before desugaring.
The internal boundary remains through name resolution, where it resets lexical scope and stops block collection,
and becomes transparent when the term enters statics.

### 3. Establish the root-term compiler path

- [x] Route `zydeco check <file>` through source discovery and program assembly.
- [x] Feed the assembled root into desugaring, resolution, and type checking without a declaration package.
- [x] Represent the checked result as a typed root term rather than a declaration entry.
- [x] Lower a checked computation root directly through dynamics and Stack IR.
- [x] Preserve the legacy driver temporarily; remove it after source migration.
- [x] Add a declaration-free root-term fixture that imports no ordinary library.
- [x] Exercise the interpreter and Stack IR root pipelines without host operations.

This stage established the first complete vertical slice of the new driver.
It proved that a file could be parsed, assembled, checked, and lowered without `proj.toml`
while declaration data structures still existed in the temporary compatibility path.

The typed stage carriers `SourceBitter`, `SourceScoped`, and `SourceChecked` preserve the root identity
without manufacturing a declaration. A checked computation root can now link directly into dynamics
or lower directly into Stack IR. At this checkpoint, both backends still retained declaration entry adapters;
stage 6 later removed them.
The direct-root contracts are covered independently with constructed checked computations.
A source-level `ret ()` fixture now traverses parsing, assembly, desugaring, resolution, checking, direct linking,
evaluation, and direct Stack IR lowering without `proj.toml`, a declaration, an `extern`, or a hidden prelude.
The Stack IR middle-end and assembly and native-emission pipelines are shared with the compatibility driver,
so a root file now reaches ZIR, analyzed ZASM, and amd64 emission without constructing a project or declaration package.

### 4. Supply Builtin and construct the standard library

- [x] Decode explicit `@[intrinsic(role)] _` splices for the five intrinsic CBPV constructors.
- [x] Require an intrinsic role and a hole payload, and reject unknown or malformed splices during loading.
- [x] Treat intrinsic-looking identifiers as ordinary bindable names in the root-term path.
- [x] Decode `@[builtin(...)]` annotations into typed host-type and host-operation roles.
- [x] Reject malformed and unknown Builtin roles during source loading.
- [x] Preserve typed Builtin roles through desugaring and name resolution.
- [x] Restrict type roles to existential package entries and operation roles to named value classifiers.
- [x] Transfer type roles to fresh abstract identities during existential opening and `PackPi` translation.
- [x] Resolve literal and compiler-generated host-type syntax against the applicable lexical role.
- [x] Reject missing or ambiguous roles at use sites and conflicting roles on one static identity.
- [x] Reject duplicate type and operation roles within one package signature.
- [x] Define exact foundational classifiers using only host atoms and intrinsic CBPV constructors.
- [x] Reject a foundational operation role when its package entry has a different classifier.
- [x] Identify legacy roles mentioning `Bool`, `Option`, or `List` as requiring foundational replacements.
- [x] Define the canonical declaration-free Builtin package signature.
- [x] Complete it with foundational replacements for every remaining library-shaped `extern`.
- [x] Make `zydeco run <file>` check the root against the package-dependent executable contract.
- [x] Construct a concrete signature-directed Builtin package in the interpreter.
- [x] Construct the corresponding Builtin package and outer application in Stack IR.
- [x] Route root-file native compilation and launch through the direct Stack IR path.
- [x] Preserve lexical binding identities when normalization clones nested package projections.
- [x] Preserve the normalized variable-user index when cloning captured recursive thunks.
- [x] Emit signed division and remainder for the exact arithmetic fragment on amd64.
- [x] Apply Builtin to the assembled binary before interpreting its resulting `OS` computation.
- [x] Establish a declaration-free standard-library shell from Builtin to a manifest public package.
- [x] Define and consume a derived standard-library operation implemented in ordinary Zydeco.
- [x] Let an imported block construct and return a pure value package.
- [x] Define an abstract algebraic package with introduction values and a polymorphic eliminator.
- [x] Define `Monad` and `Algebra` in an ordinary manifest library package.
- [x] Resolve each `monadic` block against its lexical `Monad` and `Algebra` types.
- [x] Provide native runtime symbols for every exact host function retained in a linked executable.
- [x] Rewrite the standard library as one complete term accepting Builtin and returning a public package.
- [x] Import initial standard-library source components with `@[import(...)] _`.
- [x] Use explicit application and package patterns throughout the migrated library path.
- [x] Decide which standard-library types are manifest and which are existentially abstract.
- [x] Remove source-level `extern` uses from the migrated path.

Builtin and the standard library remain distinct.
Builtin describes host capabilities, while the standard library is ordinary Zydeco code that consumes them.

### 5. Migrate the source corpus

- [x] Add a small direct-Builtin root using the canonical signature.
- [x] Add a small client of the declaration-free standard-library package.
- [x] Port initial small programs that consume Builtin directly.
- [x] Port initial small clients of the standard-library package.
- [x] Exercise abstract and manifest types across imported package boundaries.
- [x] Port an example containing a recursive nominal type SCC.
- [x] Port every executable fixture in `lib/tests/compile` to an independent root term.
- [x] Port every non-monadic fixture in `lib/tests/pack` to an independent root term.
- [x] Port an initial non-monadic slice of `lib/tests/exec` to independent root terms.
- [x] Preserve every focused named-field `lib/tests/exec` regression as a root term.
- [x] Port representative medium recursive clients that consume the declaration-free standard library.
- [x] Port every `lib/tests/exec` binary independent of `Monad` and `Algebra`.
- [x] Port the monadic `PackPi` fixture with a lexical library basis.
- [x] Port maintained monadic-block examples without changing computation order.
- [x] Port initial package-dependent and existential examples.
- [x] Replace project-based test helpers with root-file helpers.
- [x] Migrate the maintained OOPSLA, tutorial, and playground corpora.
- [x] Migrate the editor corpus.
- [x] Remove every maintained `proj.toml` after its roots have independent term-based replacements.

Each migrated root should be runnable or checkable without an authored configuration file.
Unsupported examples should remain explicitly listed rather than silently retaining legacy assembly.

### 6. Remove declarations and the legacy driver

- [x] Replace root declaration collection with ordinary term and block resolution.
- [x] Remove `BindingId::Declaration` and `ContextBody.id`.
- [x] Remove textual and bitter `DeclId`, `Declaration`, `TopLevel`, modifiers, and declaration arenas.
- [x] Remove `ReplInput::Declaration`.
- [x] Remove `StaticsArena::decls`, declaration checking tasks, and declaration entry points.
- [x] Store executable roots as computations rather than declaration identifiers.
- [x] Remove dynamic declarations and the dynamic declaration SCC evaluator.
- [x] Simplify Stack IR lowering to consume the typed root directly.
- [x] Remove `main`, `extern`, `alias`, `pub`, and declaration-only grammar.
- [x] Remove declaration handling from spans, formatting, diagnostics, and editor services.
- [x] Remove `LocalPackage`, `PackageStew`, `Dependency`, `UseStd`, and marked package binaries.
- [x] Remove legacy project discovery.
- [x] Remove all maintained `proj.toml` files.

The block context DAG, SCC nodes, and binding records are not declarations and should remain.
They are the explicit compiler plan by which an ordinary term's context is assembled.

### Declaration-removal checkpoint

The representation cut is now complete across the compiler:

- textual and bitter arenas contain no declaration category;
- scoped roots are `TermId`s, and `BindingId` is the originating `TermId`;
- statics returns a `TermAnnId` for the root and stores no declaration or entry arena;
- dynamics evaluates one `RcCompu` root without a declaration SCC; and
- Stack IR lowers the checked computation directly, preserving term-level `let`
  bindings instead of reconstructing globals.

At this checkpoint, all 35 surface tests, 23 statics tests, two dynamics tests,
four Stack IR tests, and 114 source-driver tests pass. `cargo check --workspace`
and `cargo test --workspace` also succeed, including all doctests.
A source search under `lang`, `cli`, and `editor` finds no `DeclId`, `Declaration`,
declaration `TopLevel`, `ContextBody`, `BindingId::Declaration`, or
`BindingForm::External` representation. No authored `proj.toml` remains.

## Risks and Invariants

### Discovery precedes topological compilation

Import edges occur inside parsed metadata, so the compiler must parse files while discovering the graph.
It must not maintain a second ad hoc path scanner merely to claim that parsing is topological.
Provider-first ordering begins once discovery has produced the complete graph.

### Source caching must not imply identity sharing

The source graph deduplicates file loading by canonical path.
Program assembly does not deduplicate import occurrences.
Every occurrence freshens lexical and nominal identities, including repeated and diamond-shaped imports.

### Hygiene must survive whole-program assembly

Putting every imported expression into one root term must not erase file boundaries before name resolution.
Otherwise an accidental free name or mobile binding could recreate the global declaration soup
that this migration removes.

### Import cycles are not language recursion

A cyclic source graph has no finite substitution and must be rejected.
Recursive types remain block SCCs, and recursive computations continue to use `fix`.
Neither mechanism gives a meaning to cyclic file expansion.

### Metadata has phase-specific meaning

`import` is consumed during source elaboration.
Root directives such as executable or test expectations, if introduced, belong to the driver or checking boundary.
Checking annotations such as `debug` belong later.
Typed metadata should make these phases explicit instead of letting string comparisons accumulate across the compiler.

### Primitive bootstrapping remains critical

A temporary hidden prelude would reproduce the old namespace mechanism and should not become part of the new driver.
The five core constructors enter the source graph once, as manifest static fields at the head of
`lib/std/builtin.zy`.
That signature contains the only authored occurrences of `@[intrinsic(vtype)] _`,
`@[intrinsic(ctype)] _`, `@[intrinsic(thk)] _`, `@[intrinsic(ret)] _`, and `@[intrinsic(unit)] _`.
The role argument is mandatory because a roleless intrinsic hole cannot distinguish `VType` from `CType`;
using the surrounding binder name would make alpha-renaming change the program.
An intrinsic splice is consumed during desugaring and produces the corresponding canonical internal term.
The Builtin signature then gives these terms ordinary language-level names.
Every consumer obtains them by unpacking the package rather than by repeating compiler metadata.

An omitted classifier on a manifest field is inferred from its definition.
The general formation judgment accepts `exists (X as D). B` when `D : S`, where `S` may be `Set` or a kind,
and checks `B` under the transparent equation `X ≡ D : S`.
This admits `VType` and `CType` as manifest kind fields.
`Thk`, `Ret`, and `Unit` are manifest type fields under the same syntax.
All five are static and erased; only ordinary abstract type fields contribute witnesses to `PackPi`.

The surface pipeline separately decodes an annotation such as `@[builtin(int)]` into a closed `BuiltinRole`
and rejects malformed or unknown keys before assembly. Only host-provided abstract types (`Int`, `Char`,
`String`, and `OS`) and host operations are Builtin roles. `VType`, `CType`, `Thk`, `Ret`, and `Unit`
remain intrinsic language structure, while `Monad` and `Algebra` remain ordinary library definitions.

The typed role survives desugaring and name resolution. During checking, a host-type role must annotate
an existential package entry, and a host-operation role must annotate a named value classifier.
Opening an abstract package transfers its type roles to the fresh identities observed by the consumer.
The same transfer occurs when monadic translation freshens a `PackPi` witness.
Compiler-generated host-type syntax resolves a role only among the existential witnesses visible in its lexical
`SkolemScope`. A missing role and multiple visible witnesses for one role are both static errors.
Host-type roles accept only abstract existential entries: `Int`, `Char`, and `String` require `VType`,
while `OS` requires `CType`. Host-operation roles accept only named value classifiers.
After a package-dependent arrow has formed, the checker audits its complete package-local role assignment.
One signature may contain each host type or operation role at most once, while independent signatures may
reuse the same roles without sharing identities.

The checker owns canonical identities for `VType`, `CType`, `Thk`, `Ret`, and `Unit`.
The resolver preserves these compiler-generated terms and Builtin host-type references without manufacturing
namespace entries for intrinsic language structure.

The executable boundary completes the bootstrap.
Statics derives one backend-independent package plan from the checked domain.
The plan erases existential type components, retains typed operation roles and product layout,
and validates the executable `OS` result contract before a backend materializes any values.
The interpreter and Stack IR lowerer both consume this plan, construct their primitive operation values,
and apply the resulting package to the package-dependent root.
Execution requires exactly one `os` witness in the `PackPi` telescope and requires the codomain to be that
abstract witness. A declaration-free fixture with `int`, `os`, `add`, and `exit` roles executes through the
root-file command and returns the host exit status computed by the program.

The direct source path now reuses typed pipeline objects for inlining, optional CPS conversion, closure conversion,
normalization, assembly lowering and analysis, and native emission. CLI build dispatch accepts a single `.zy`
root, so `zir`, `zasm`, `asm`, and `exe` require no project discovery.
The source fixture reaches amd64 emission through this path. The experimental LLVM emitter still cannot represent
some assembly-local bindings; its shared adapter now reports that inherited limitation as a typed driver error
instead of panicking.

Exact classifier validation uses a typed ABI grammar rather than comparing formatted types.
Its value classifiers contain only the host atoms `Int`, `Char`, and `String`, plus thunks;
its computation classifiers contain `OS`, returners, value-to-computation arrows,
and computation-polymorphic binders with bound result classifiers.
The validator follows named and variable aliases, checks host atoms by their abstract witness roles,
and verifies every operation entry before any backend constructs the package.

Every Builtin operation now has a representation-independent classifier.
Integer and string comparisons select between two computations of an arbitrary result classifier.
String splitting selects either an empty computation or a continuation receiving the two resulting strings.
Line parsing selects between an `OS` failure continuation and an integer-success continuation.
Argument access is a computation-polymorphic lazy right fold:

```zydeco
forall (R : CType) .
  Thk R -> Thk (String -> Thk R -> R) -> R
```

These classifiers expose host observations without constructing a library-defined value.
The ordinary standard library turns them into its abstract `Bool`, `Option`, and `List` packages.
In particular, the argument fold rebuilds `List String` from the package's exported `nil` and `cons`,
and the thunked recursive result preserves the usual lazy right-fold behavior.
Package materialization resolves the typed roles to their foundational host symbols.

`lib/std/builtin.zy` now denotes the canonical signature as one complete type term.
It binds aliases for `Thk` and `Ret`, existentially introduces the four host types, and labels every
operation with its Builtin role. A declaration-free executable imports that file as its parameter classifier.
The launcher materializes the resulting package in both the interpreter and Stack IR, and the fixture reaches
amd64 emission without project configuration.
Direct tests exercise comparison, optional-pair, line-parsing, and argument-fold eliminators
without mentioning ordinary algebraic types.

The native runtime now exports every exact host symbol used by the canonical package.
Stack IR distinguishes an operator from a host function and records whether that function returns one machine word
or selects a Zydeco computation.
Returning calls resume the implicit CBPV continuation with their result.
Control calls first return a typed transfer descriptor, allowing the Rust stack frame to unwind before assembly
resumes the selected closure with zero, one, or two arguments.
This boundary supports comparison and split branches, string observation and conversion, I/O continuations,
random integers, and the lazy argument fold without jumping out of a live Rust frame.

`lib/std/std.zy` is an ordinary standard-library term.
It is a pure package-dependent function from Builtin to a public value package.
The public signature uses manifest existential fields for the four host types, so clients receive transparent
equations while the package remains self-describing. It also imports the Boolean, optional-value, and list
components exactly once and re-exports their abstract type witnesses and operations.
The operation product is retained as one tail value:
a parameter pattern opens `(Int, Char, String, OS, api)`, and forwarding reconstructs that compact package
instead of repeating every operation. The standard-library term projects the named operations from `api`
and returns them under its own manifest package signature. Its `increment` operation is implemented in ordinary
Zydeco by calling primitive addition, demonstrating that one package can mix forwarded host capabilities with
derived library definitions. It also wraps every foundational eliminator whose public meaning uses an abstract
algebraic type: comparisons produce `Bool`, splits and line parsing produce `Option`,
and argument folding produces `List`.

The `std/minimal.zy` client imports both terms transitively, applies the standard library, opens its result,
and invokes projected `increment`, `sub`, and `exit` operations. It checks and runs in the interpreter and reaches
amd64 emission without declarations or configuration. This path also exposed an older normalization defect:
substitution assignments are stored from inner to outer, but their deep clone had freshened them in that order.
The clone now traverses outer to inner, so a projected field captured by a thunk remains bound after alpha-renaming.
The current client annotates its Builtin parameter explicitly.
Inferring an entirely unannotated `param builtin` from a later `PackPi` application requires sort constraints
to flow backward into pattern elaboration; the checker currently chooses type- versus value-pattern syntax
before checking that body. Projection also operates on an opened product, so an existential public package
must presently be opened by a pattern before its fields can be selected.

The first legacy ports now cover a transparent type alias and local function, a polymorphic identity function,
product construction and elimination, pattern matching, copattern matching, continuation cloning,
captured mobile definitions, higher-order computation parameters, nested computation binds,
exact signed arithmetic, recursive summation and factorial, and a recursive nominal natural-number type.
Their new roots use block-scoped `let` and `def`, accept Builtin as a parameter, and either consume it directly
or apply the standard-library term. Every port interprets to exit status zero and reaches amd64 emission without
authored configuration. The arithmetic fixture checks negative quotient and remainder behavior and verifies that
native emission selects `idiv`; linked amd64 execution remains a target-host validation gate rather than a
portable test.

The original uniform-composition fixture is now itself one complete term.
It deliberately places a value parameter before the type parameter needed to classify it;
the block dependency graph orders the wrappers correctly, while the source remains free to present related
definitions together. A separate existential-product port checks nested package opening and product reassociation
without a declaration entry.

Every executable under `lib/tests/compile` is now an independent root term at its original path.
The ports cover finite continuation-based I/O, intentionally nonterminating computations, named and mixed products,
and the largest named-term fixture with local `data` and `codata` definitions.
Terminating roots are interpreted and lowered to amd64; intentionally divergent roots are checked and lowered
without being evaluated. The remaining `compile/prelude.zy` is not an executable:
its declaration-era responsibilities are now divided between explicit intrinsic splices and `lib/std/builtin.zy`.
The terminating integration slice now runs twenty-six migrated roots through both the interpreter and linked
amd64 executables, so `lib/tests/compile/proj.toml` has been removed.
Four additional roots exercise the exact native boundary: returning string operations, control operations that
resume closures with zero or two arguments, the lazy argument fold over two process arguments, and UTF-8 string
literals. Together, the thirty roots provide sixty interpreter and linked-amd64 integration cases.
The amd64 emitter stores literal bytes in read-only data and asks the runtime to construct the same opaque host
string representation used by Builtin operations; it does not assume a Rust `String` memory layout in assembly.

The three delimited-control fixtures now bind `Kont` as an ordinary type-level term and obtain `Monad` from the
lexical monadic-basis package.
Their specialized answer-type and answer-type-polymorphic `reset`/`shift` encodings still evaluate to 25,
while the generic `try`/`throw` encoding retains both handled and ordinary-return paths.
All three roots execute through the interpreter and linked amd64 backend, and
`lib/tests/delimcc/proj.toml` has been removed.

The stack-oriented fixtures now use root terms for direct, explicit-stack, and CPS tree inversion,
for annotating a tree with its traversal backtrace, and for recursive list merging.
Their former call-by-value machine is covered by the shared OOPSLA root, which preserves the same expression,
environment, value, stack, answer, and machine structure.
All four programs now run through both backends without project discovery, and
`lib/tests/stack/proj.toml` has been removed.

The AVL/hash example now follows the same source-root path while retaining the complete balanced-tree algorithm.
It opens the standard-library package, then places the AVL and hash type constructors and their operation graph in
an inner block whose `that` bindings may move only within the scope established by that package elimination.
Recursive term definitions still use explicit CBPV `fix`.
Because the standard library deliberately exports `Bool` and `Option` abstractly, the port branches and inspects
optional values through their public eliminators rather than recovering the providers' constructors.
The original insertion, rotation, deletion, replacement, search, and structural-equality scenario exits
successfully through both the interpreter and linked amd64 backend.
Its integration test no longer discovers a project, and `lib/avl/proj.toml` has been removed.

The manifest, named, interleaved, and uniform package fixtures likewise inhabit their original paths as root terms.
The uniform fixture exposed a lexical-shadowing defect in nested blocks:
the persistent-map union used to install block-wide binders could retain an enclosing definition when that map
was larger than the inner binder map. Block scope construction now applies explicit functional updates,
so inner mobile binders reliably shadow enclosing names regardless of map size.
This permits a nested recursive `def LocalList` to inhabit an outer transparent `let LocalList`
without manufacturing a self-edge on the outer binding.
The monadic package fixture imports the explicit lexical `Monad` and `Algebra` basis.

The package ports cover named manifest fields and an interleaved existential telescope.
The counter fixture exposes an `Int` representation transparently and consumes the package through a named pattern.
The interleaved fixture mixes manifest and abstract witnesses, forms a `PackPi` from the package pattern,
and returns values whose result type depends on the opened witnesses.
Both fixtures preserve their original static meaning while replacing declarations and project configuration
with one block term and an explicit Builtin parameter.

The first `lib/tests/exec` ports cover universal and existential quantification, optional codata destructors,
partial type annotations, the Church encoding of `Ret`, intrinsic `Unit`, signed arithmetic,
and equivalent even/odd programs expressed through recursive data, recursive codata, and computation-level `fix`.
Each source is an independent block term parameterized by the canonical Builtin package.
The `Unit` example interprets with its original output and lowers through StackIR.
Its string literal now has the same linked-amd64 representation exercised by the compile corpus.

Every focused named-field fixture under `lib/tests/exec` now has a declaration-free counterpart.
The small roots retain the separate regression intent for mixed and nested products, named patterns,
data payloads, function arguments and results, codata destructors, and field punning.
The existing comprehensive `compile/named.zy` and `compile/named-mixed.zy` roots serve the two overlapping fixtures;
all nine counterparts interpret successfully and lower through amd64.

The `choice` fixture is the first medium legacy client of the assembled standard-library term.
Its generic type constructors remain mobile block definitions, while values that depend on the package opened by
`do` use lexical `def ... in ...`; this directly exercises the distinction between `that` mobility and `in` scope.
The program retains recursive trees and paths, computation-indexed choices, captured continuations,
abstract Boolean comparison, and its original `0`, `1`, `o` output.
It interprets successfully and lowers through StackIR assembly.

The medium execution slice now also covers a call-by-name fixed-point combinator, lazy recursive lists,
variadic codata, an object encoded by recursive codata, and a client of the standard library's abstract `List`.
These ports exercise higher-order recursion, nested `fix`, exported introduction and elimination operations,
and continuation-based output without recovering declaration visibility.
Each program retains its original observable behavior in the interpreter and lowers through StackIR assembly.

The remaining non-monadic execution binaries add defunctionalized folds, a deterministic pushdown automaton,
a small CBPV self-interpreter, and a regular-expression engine.
The automaton translates its former transparent type alias to `let`; using `def` there correctly seals the arrow
and prevents its use as a function classifier.
The self-interpreter preserves mutually recursive `SynVal`/`SynComp` and `SemVal`/`SemComp` type components,
providing an end-to-end check that separate type definitions remain recursive SCC nodes in the block context.
The execution corpus that follows depends on lexical definitions of `Monad` or `Algebra`.

The executable integration harness now has a root-source test path that checks a `.zy` file, applies its Builtin
package, evaluates it with empty input, and requires its unique result to be exit status zero.
All forty-seven maintained executable tests now use this path directly.
The obsolete `lib/tests/exec/proj.toml` has been removed.
The five package integration tests likewise run their root counterparts directly,
and `lib/tests/pack/proj.toml` has been removed.
The three former `lib/tests/monadic` binaries now use the migrated exception-transformer roots and a minimal
Builtin executable root, so their project configuration has also been removed.
The unreferenced `compile-more` project configuration has been removed, while its comparison sources now use
root-term syntax. Its redundant exception source is represented by the maintained OOPSLA exception fixture.

The two maintained negative fixtures are now root terms as well.
One still reaches the checker without enough information to synthesize a constructor scrutinee, while the other
still fails resolution on its intended free variable.
Source-driver regressions assert those phase boundaries directly, and `lib/tests/fail/proj.toml` has been removed.

`lib/std/monad.zy` now defines `Monad` and `Algebra` as ordinary transparent types and returns them in a value
package whose existential witnesses have manifest equations.
A `monadic` node carries two hidden variable terms for these names; resolution therefore selects the definitions
visible at that exact lexical site and records their users and block dependencies like any authored reference.
The checker validates their higher kinds and stores the selected type constructors in the monadic translation
environment instead of consulting declaration-era primitive cells.
The imported package passes a minimal `Ret`-monad execution test, the interleaved monadic `PackPi` fixture,
the algebra-construction fixture, and a higher-kinded free-monad fixture through interpretation and amd64.
Monadic construction now also records codata layout hints for generated comatches and destructor heads,
matching the invariant already maintained for source-written terms.
A nested-block regression imports the basis twice and confirms that a monadic block selects the nearest lexical
`Monad` and `Algebra`, rather than falling back to a process-wide primitive registration.
The transformer, free-handler, and backtracking fixtures now also run as declaration-free roots.
Their explicit continuation applications preserve the same sequencing without dedicated surface syntax.
The string literals in two of these programs now have a complete route through linked amd64.
The call-by-value and CBPV interpreter fixtures now follow them.
Their recursive environment, value, computation, and syntax types become ordinary block-level type SCCs,
and their former transparent aliases become `let` bindings.
The 929-line `cbpv-monadic` parser and interpreter is now a root term as well.
It defines its Boolean, option, list, and product types locally, obtains every host operation through its explicit
Builtin parameter, and imports the lexical `Monad` and `Algebra` basis.
Its four parser and evaluator demonstrations retain their original output and reach analyzed StackIR assembly.
The old project configuration no longer lists this source as a declaration-era binary.

Migration of the OOPSLA artifact has begun with its polynomial and calling-convention examples.
Both are now independent root terms with explicit Builtin parameters and no dependency on the artifact's
declaration-based `core.zydeco`.
The polynomial reaches native emission, while the calling-convention example retains its ordinary,
optional-argument, and variadic codata interfaces and both implementations of `sum_and_mult`.
The artifact's call-by-value abstract machine now follows them as one mutually recursive block context.
Its syntax, environment, semantic values, machine, answers, and interpreter are all term-level definitions;
the original example still prints `true` and reaches analyzed StackIR assembly.
The continuation, state, I/O, free, and exception monads import the ordinary lexical monadic basis.
The exception-law counterexample retains its observable `2 != 1` result, and the relative-monad algebra fixture
checks arrow, product, coinductive, and defunctionalized exception algebras.
The ExnT and ExnKT artifact tests share one stronger root that executes both generated and manual transformer
implementations.
All nine OOPSLA integration tests now bypass `lib/tests/oopsla/proj.toml`.
The artifact runner and README now route every command to those maintained source terms and explain whole-program
imports in place of project configuration.
`lib/tests/oopsla/proj.toml` has therefore been removed; the adjacent declaration-era files remain only as the
paper artifact's historical presentation.

`lib/playground/main.zydeco` is the first existing user-facing program migrated in place.
It is now a package-dependent root term, runs directly from its source path, and exits with its original status 42.
Its `proj.toml` has been removed, and the contributor guide and artifact reusability example now teach the
configuration-free command and explicit Builtin parameter.

The three reusable programs under `lib/examples` are now root terms as well.
The abort example uses a local variadic codata interface, and the two input loops consume the foundational
`read_line_as_int_branch` operation instead of importing a declaration-defined `Option`.
The algebra server is shared with the corresponding OOPSLA integration test and retains the full collection of
relative-monad algebras before running its overflow-checking sum loop.
Their integration tests run the source files directly, and `lib/examples/proj.toml` has been removed.

The Spell tutorial has been migrated in place rather than shadowed by test-only roots.
Its opening chapter now teaches a file as one term, explicit intrinsic and Builtin dependencies,
block-local `data` and `codata`, and the mobility distinction between `that` and `in`.
The following chapters retain their CBPV progression through thunks and returns, strict and lazy data,
object-like codata, an encoded fixed point, mutual recursion, continuation-passing I/O, and higher-kinded optics.
The reference chapter replaces its former `extern` declarations with an explicit existential library parameter
and remains a check-only example until such a package is supplied.
The tutorial's Markdown converter is itself a Builtin-parameterized root term, and its script invokes that source
directly. Eight executable chapters now pass in both the interpreter and linked amd64 backend.
`docs/spell/proj.toml` has been removed.
No integration test now invokes the project-binary harness, so its `ProjectBinary` wrapper and project-specific
test macros have been removed; the remaining test helpers accept root source paths only.

The focused static regression suites now use the same source driver.
Their temporary roots import and unpack the Builtin package signature and, where required,
obtain the lexical `Monad` and `Algebra` basis from an ordinary library parameterized by that package.
The existential, named-term, uniform-composition, and `PackPi` suites therefore exercise 51 judgments without
constructing a package stew or loading the declaration-era standard-library project.
With `lib/std/proj.toml` removed, no authored `proj.toml` remains in the repository.

The separate tic-tac-toe document already identifies itself as a non-working sketch, has no integration coverage,
and carried a manifest whose standard-library path no longer existed.
That stale configuration has been removed; the declaration-era source remains historical material rather than a
maintained executable migration target.

The first literal-bearing monadic block exposed two assumptions left over from declaration-era primitives.
An abstract Builtin value type has the unique trivial structure required by signature translation, so structure
translation now synthesizes `Top` when an unregistered reference has value kind while continuing to require an
explicit structure for computation kinds and higher kinds.
Monadic value translation also preserves typed literals instead of treating them as unreachable.
The migrated `algtrans` fixture exercises return, thunk, thunk-argument, `forall VType`, and `forall CType`
translation against the lexical basis and reaches amd64.
Generated `ExnT` and Church-encoded `ExnKT` transformers now exercise higher-kinded results as well.
This required the monadic preliminary environment to retain every lexical type binding, including manifest
existential witnesses and transparent aliases, while continuing to admit only globally translatable term bindings.
Generated constructors and constructor patterns now receive data-layout hints alongside the codata hints already
recorded for generated comatches and destructor heads.
At this checkpoint, all 114 root-source driver tests pass through the applicable interpreter and backend paths.
The fixture still presents the generated transformer with its source monad and expanded transformer result
classifier at the translation site. Moving either through an additional global transparent alias can leave
different unfolded forms on the two sides of a manifest lexical basis.
Alias normalization across that boundary therefore remains an explicit monadic-migration risk.

The recursive natural-number addition port exposed a second normalization invariant.
Substitution-normal form indexes every variable occurrence so single-use assignments can be inlined.
Deep cloning freshened variable references without adding the cloned nodes to that index;
normalization could therefore discard a recursive closure binding while leaving cloned references to it.
User registration now belongs to the normalized arena and is applied consistently during elaboration,
deep cloning, and value replacement.
A focused StackIR test checks the index directly, while the migrated addition program verifies the complete
closure-conversion and repeated-normalization pipeline.

`lib/std/bool.zy`, `option.zy`, and `list.zy` establish the first reusable algebraic component boundaries.
Each file is a pure package-dependent function that produces an existential value package and keeps its
constructors lexical to the provider.
The Boolean package exports introduction values, ordinary operations, and a computation-polymorphic `branch`.
The other two packages abstract type constructors at kind `VType -> VType` and expose introduction functions
plus one-layer eliminators. `List` is recursive, so this also verifies that a recursive type constructor can
remain abstract across a source and package boundary.
Clients consume these types exclusively through the exported operations, and `std.zy` composes the three
packages by opening and repacking their witnesses rather than relying on shared declaration names.
Supporting these pure package results required value-producing blocks to retain local value bindings in typed syntax.
That `Value::Let` form is administrative: the interpreter evaluates it with a scoped environment,
and Stack IR appends its bindee and binder to the value plan before lowering the tail.
Each component checks as a value root. Declaration-free executables consume their re-exported interfaces
through the standard library, exit successfully in the interpreter, and reach amd64 emission.

As the canonical package grew, imported standard-library terms crossed the stack limit during recursive compiler
passes. The test profile now optimizes `zydeco-surface`, `zydeco-statics`, and `zydeco-assembly`;
the development profile explicitly optimizes the first two workspace crates for the CLI source path.
These targeted overrides reduce recursive frame size without increasing the global stack allowance.

The larger AVL root exposed a structural limit beyond those profile adjustments.
Assembly lowering represented each remaining instruction as a boxed Rust continuation and invoked the complete
chain before returning, while stack measurement and inlining recursively followed the resulting linear program.
Lowering now reserves an instruction identity and schedules its continuation on an explicit worklist.
Stack analysis materializes each linear program chain once, folds layouts forward, and performs inlining in reverse.
The transformed passes preserve the CPS dependencies without making the Rust call stack proportional to program
size. Stage dumps are also formatted only when trace logging is active, so a silent integration test does not build
and discard a potentially large diagnostic document.

### Pure parameterized packages

Pure type abstraction now forms a value-level universal, and a pure function whose boundary pattern opens a package
forms a value-level package-dependent arrow. Both forms preserve static witnesses through checking and application,
then erase them before dynamics and Stack IR. Ordinary value arrows still reject existential escape, so only the
explicit package telescope extends witness scope over the result.

Block elaboration already lowers `param` to abstraction, `let` and `def` to scoped bindings, and `begin ... end` to
its residual term. With the two pure classifiers available, those forms can now produce types and values directly as
well as computations. `lib/std/monad.zy`, the algebraic component modules, and the aggregate `lib/std/std.zy` use
this boundary: their Builtin parameters, transparent type definitions, and result packages need no outer thunk or
`ret`. Consumers apply each module as a value and open the result with `let`; exported operations remain
computation-typed.

Focused checks cover explicit and synthesized pure universals, package-dependent value application, nested
existential escape rejection, monadic translation, interpreter erasure, Stack IR lowering, and a parameterized block
that combines type definitions, value definitions, local bindings, and a pure package result.

Checker-only library fixtures now end in the value or manifest package they expose. Their integration harness
requires a value root and invokes only `zydeco check`; it neither supplies the executable contract nor evaluates the
term. Focused static snippets likewise expose the type, value, package, or suspended computation under examination.
Executable fixtures retain an exit only when its status represents an observed runtime assertion.

### Transparent and abstract interfaces remain explicit

Source import makes an implementation term available at compile time,
but does not itself decide its abstraction boundary.
Manifest package signatures expose transparent equations.
Ordinary existential packages and sealing hide representations.
The imported file's outer annotation and term structure determine which interface its clients observe.

### Executable meaning comes from the command boundary

A file does not become a binary because it contains a declaration named `main`.
`zydeco run` supplies the executable expectation and Builtin application.
`zydeco check` should continue to accept roots with other classifiers.
If root metadata later records test or execution intent, it should not change the underlying term.

### Dependency ordering does not order effects

The source graph schedules compilation, and block DAGs schedule types and values.
Neither graph sequences CBPV computations.
Effect order remains expressed by computation terms and relative-monad structure.

### Diagnostics require multi-file provenance

An import error should identify both the import site and the referenced path.
An error inside an imported term should point into that imported file.
Cycle diagnostics should show every participating import site.
Fresh cloning must therefore preserve spans while assigning new semantic identities.

### Absolute paths trade portability for directness

Absolute paths are accepted as requested, but they make a source tree location-dependent.
Relative paths should remain the portable default.
A future package resolver or lock file may provide reproducible external dependencies,
but it must elaborate to the same typed import graph rather than restore namespace merging.

## Validation Gates

The completed root-file driver satisfies the following source-migration gate:

1. Every loaded file parses as exactly one term.
2. Import paths resolve relative to their containing files.
3. Import cycles receive deterministic multi-file diagnostics.
4. Two imports of one source produce distinct nominal identities.
5. One imported binding reused twice preserves one identity.
6. Imported free names cannot capture caller bindings.
7. Mobile bindings cannot cross source boundaries.
8. Errors retain the correct originating file and span.
9. The assembled program reaches statics as one root term.
10. A declaration-free fixture checks without `proj.toml`.

The legacy driver and declaration sort were deleted after the following gate was satisfied:

- every maintained root uses the term parser;
- all source dependencies use import metadata;
- the standard library consumes Builtin and exports an explicit package;
- no source `extern` or declaration `main` remains;
- interpreter and native backends consume the same typed root representation;
- project-based test helpers have been replaced;
- editor paths have been replaced;
- no maintained `proj.toml` remains; and
- a repository search finds no required `Declaration`, `DeclId`, or declaration `TopLevel` representation.

## Open Work

- Determine how unknown and phase-inappropriate meta notations are diagnosed.
- Specify root-only metadata for tests and other driver actions without reintroducing project configuration.
- Infer the sort and classifier of an unannotated parameter from later applications.
- Decide whether named projection should open existential package prefixes automatically.
- Decide whether named `data` and `codata` retain dedicated term-level sugar.
- Define source hashing and incremental invalidation for the import graph.
- Decide whether absolute imports should produce a portability warning.
- Defer separate compilation artifacts, registries, and generated lock files until whole-program source imports work.
