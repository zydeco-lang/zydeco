# Zydeco Compiler Implementation Reference

This reference describes the compiler's current representations, phase contracts, and maintenance entry points.
It assumes basic programming-languages background.
The [language reference](language.md) owns source semantics; [CONTRIBUTING](../../CONTRIBUTING.md) owns setup
and general command-line workflows.
This reference owns [compiler pass composition](#compiler-pass-composition),
including pipeline selection and inspection.
Component guides map local modules, and linked design records retain alternatives and open decisions.

Each completed program has one selected root. Each phase establishes the representation its consumers need;
source identities and diagnostic provenance survive even when runtime structure is erased or rebuilt.
The distinction between source checking, executable selection, and backend preparation is essential
when locating failures.

1. [Architecture and a Program's Path](#c1-architecture-and-a-programs-path-through-the-compiler)
2. [Compiler Data and Identities](#c2-compiler-data-identities-arenas-and-source-provenance)
3. [Sources, Sessions, and Retention](#c3-source-loading-sessions-queries-and-memory-retention)
4. [Parsing and Resolution](#c4-parsing-desugaring-and-name-resolution)
5. [Checking and Inference](#c5-typed-representation-judgments-and-inference)
6. [Elaboration and Validation](#c6-typed-elaboration-residualization-and-validation)
7. [Reference Interpreter](#c7-linking-and-the-reference-interpreter)
8. [High SPS and Normalization](#c8-high-sps-lowering-normalization-and-demand)
9. [Closure Conversion](#c9-closure-conversion-and-first-order-spslow)
10. [ZASM and Local Representations](#c10-zasm-stack-analysis-and-local-representation-choices)
11. [Native Preparation and Emission](#c11-native-preparation-activation-frames-and-amd64-emission)
12. [Native Runtime and Collection](#c12-shared-native-model-allocation-and-collection)
13. [WebAssembly](#c13-webassembly-backends-and-embedding)
14. [Builtin and Foreign Contracts](#c14-builtin-contracts-primitive-operations-and-foreign-calls)
15. [Tooling](#c15-diagnostics-formatting-documentation-and-interactive-tooling)
16. [Validation and Extension](#c16-validation-debugging-and-extending-the-implementation)

## C1. Architecture and a Program's Path Through the Compiler

[CommandCompiler](../../cli/src/compile.rs) adapts the revisioned compiler session to CLI commands and tests.
[CompilerSession](../../lang/session/src/source/query.rs) owns source inputs and analysis;
its products distinguish retained analysis facts, a complete checked program, and an executable computation.
The interpreter and every compiled target start from the same checked source and static-elimination boundary.
Compiled targets use stack-passing style (SPS), where arguments, destructor observations,
and return continuations form explicit stacks.

```mermaid
flowchart TD
    Sources[Source graph and parsing] --> Frontend[Desugaring and resolution]
    Frontend --> Statics[Checking, finalization, coverage, static elaboration]
    Statics --> Queries[Static queries and editor analysis]
    Statics --> Entry[Executable selection and completeness]
    Entry --> Dynamics[Builtin linking and interpreter]
    Entry --> High[BranchJoinProgram: high SPS]
    High --> Normalize[Selected high-SPS passes: normalization by default]
    Normalize --> Low[SpsLowProgram: closure conversion]
    Low --> SPS[Structured WebAssembly]
    Low --> Portable[Portable ZASM and stack analysis]
    Portable --> AM[Abstract-machine WebAssembly]
    Portable --> ZInterp[ZASM interpreter]
    Low --> Native[Native ZASM and checked frame preparation]
    Native --> AMD64[AMD64 emission and runtime linking]
```

The main boundaries are concrete program types:

| Product | Establishing entry point | Consumer |
| --- | --- | --- |
| `TextualProgram`, `BitterProgram`, `ScopedProgram` | [Source assembly and surface pipeline](../../lang/session/src/source/pipeline.rs) | The next frontend phase |
| `ProgramAnalysis` | `CompilerSession::analyze` | Diagnostics and tooling facts |
| `CheckedProgram` | `CompilerSession::checked_program` | Full typed-tree inspection |
| `ExecutableProgram` | `CompilerSession::executable_program` | Builtin root linking or lowering |
| `DynamicsProgram` | [BuiltinRootLinker](../../lang/dynamics/src/link.rs) | Reference interpreter |
| `BranchJoinProgram` | [BuiltinRootLowerer](../../lang/stackir/src/high/lower.rs) | Selected high-SPS transformations, then closure conversion |
| `SpsLowProgram` | [SpsLowPipeline::run](../../lang/stackir/src/pipeline.rs) | Structured Wasm or assembly lowering |
| `AssemblyProgram` | [LoweringPipeline::run](../../lang/assembly/src/pipeline.rs) | ZASM interpreter or AM Wasm |
| `NativeProgram` | Native pipeline from `LoweringPipeline::with_native_frames` | AMD64 emitter |

Checked-root lowering, SPSLow conversion, and assembly lowering compose through typed compiler passes;
[compiler pass composition](#compiler-pass-composition) below owns their execution and error contract.

Consider this executable, which exits successfully:

```zydeco check
param (/Int; /process) : @(import("../../lib/std/builtin.zy")) in
let api = (#run = { fn (n : Int) => ret n }, #spare = 7) in
do code <- ! api/run 0;
! process/exit code
```

Source loading checks the Builtin signature independently.
Checking resolves `api/run` to a structural field route and relates `process` to the executable's type witnesses.
Static elaboration erases static evidence while retaining the explicit thunk and computation calls.
High lowering constructs the product and its consuming stack.
Normalization exposes the known field and thunk, binds `n` to `0`, forwards the return
into `code`, and removes unused fields.
Any remaining closure crosses C9 as a closure record with explicit captures.
The selected backend then realizes the remaining host exit; the interpreter executes the checked residual term directly.

A source error stops before that fork:

```zydeco reject=tyck.unconstrained-inference at=1:4
fn x => ret x
```

Its inference region cannot close. The session retains a rejected analysis with diagnostics and available facts;
`CommandCompiler::analyze` reports rejection, and executable selection cannot produce a backend input.
Load, parse, desugar, and resolve failures retain the source information available at their own boundary.

### Compiler Pass Composition

An optimization can expose work for a later pass, and applying a pass again may simplify its result.
Compiler developers need explicit control over order, omission, and repetition when comparing transformations.
The pass scheme gives Rust callers ordinary values and typed interfaces for that control;
CLI plans select built-in transformations within a phase whose required checks and lowering remain fixed.
The same interface composes validation and representation-changing lowerers,
so configurable optimization stages fit into the compiler's existing phase boundaries.

#### Typed Passes and Static Composition

The [`CompilerPass<Input>` trait](../../lang/utils/src/pass.rs) exposes each transformation's input,
associated `Output`, and domain `Error`.
A pass value holds configuration and typed dependencies;
`run(&mut self, input)` creates the temporary construction state for that invocation.
Inputs can own a program or borrow one, preserving the existing arena ownership boundaries.
Completed phase arenas need not expose mutable access.
Functions and closures returning `Result` also implement the interface, allowing local checks to participate
without additional named types.

`pipeline![first, second, ...]` constructs a nonempty sequence and connects each output to the next input.
Stage expressions are evaluated once, in declaration order, when the pipeline is constructed.
Execution follows that order and stops at the first error.
A sequence is itself a pass, so named pipelines and nested sequences compose through the same interface.
Rust checks adjacent input and output types; semantic ordering requirements remain the responsibility
of the selected stages and their validated program types.

Stages in a sequence share an error type. `map_err` translates domain errors at a composition boundary;
`with_error` gives an infallible pass the enclosing pipeline's error type.
`run_infallible` removes the unreachable error case when executing an entirely infallible sequence.
For a high-SPS program `high` and its naming arenas, a Rust caller can select repeated normalization:

```rust
use zydeco_stackir::{SpsLowPipeline, high::normalize::Normalizer};
use zydeco_utils::{pass::CompilerPass, pipeline};

let optimizations = pipeline![Normalizer, Normalizer];
let mut lowering = SpsLowPipeline { scoped: &scoped, statics: &statics }
    .with_optimizations(optimizations);
let sps_low = lowering.run_infallible(high);
```

#### Runtime Sequences and Execution Adapters

[`PassSequence<'p, Ir, E>`](../../lang/utils/src/pass/sequence.rs) stores a vector
of configured passes sharing one input/output contract and error type.
Static composition retains concrete pass types; a runtime sequence boxes passes at its storage boundary.
`with_pass` appends an occurrence and retains its position, including duplicates.
An empty sequence and `Identity` return their input.
`when(enabled)` conditionally runs a stage that preserves its IR type;
`repeat(times)` executes the whole enclosed stage or group exactly that many times, stopping at the first error.
A disabled stage or zero repetitions return the input without invoking the enclosed pass.
Configuration is constructed once, while each invocation receives the preceding invocation's output.

`by_ref` allows a sequence to borrow an existing configured pass,
and the sequence lifetime permits borrowed dependencies without imposing `Clone`, `Send`, or `'static` on every pass.
Failure does not roll back ownership of an input consumed by a pass.
This makes reuse explicit while leaving each pass responsible for its temporary construction state.

#### Observation and Failures

[`with_observer`](../../lang/utils/src/pass/observe.rs) wraps a pass with typed before/after hooks
for inspection, verification, or rendering.
`PassLocation` identifies its occurrence with a name and structural index path;
the wrapper counts invocations, including repetitions and reuse across outer executions.
Paths are stored with zero-based indices and displayed with one-based positions.
Observers borrow the program, and timing measures pass execution separately from observer callbacks.
A rejected before hook prevents the pass from running; a rejected after hook prevents downstream execution.
`PassFailure` retains the occurrence and distinguishes the original domain error
from a before/after observation failure.
Panics remain compiler bugs and are not converted into ordinary pass errors.

#### Built-In Plans and Phase Boundaries

[`SpsLowPipeline`](../../lang/stackir/src/pipeline.rs) declares high-SPS checks,
optional transformations, and closure conversion as a sequence.
Its `with_optimizations` method accepts any pass preserving `BranchJoinProgram`,
including `Identity`, a custom Rust pass, or a nested sequence.
The selected stage runs between the required high-SPS checks and before closure conversion.
The default runs the existing normalizer once, keeping its combined reductions and demand analysis;
an empty selection still runs the checks and closure conversion.
[High SPS](#c8-high-sps-lowering-normalization-and-demand) defines the representation and normalization rules.

[`HighSpsPlan`](../../lang/stackir/src/passes.rs) describes built-in selection.
`Default` selects one normalizer, `None` selects no optional transformations,
and `Custom` retains an ordered vector of typed `HighSpsPass` entries.
The textual forms are `default`, `none`, and comma-separated pass names; the initial catalog contains `normalize`.
An empty textual selection, unknown names or options, empty list entries,
and presets embedded inside lists are rejected before source loading or artifact creation.
Displaying a plan gives its canonical textual selection; `explain` expands its optional stages together
with their required checking and conversion boundaries.
Rust callers can provide arbitrary `CompilerPass` implementations without joining the built-in catalog.

The command compiler owns the plan and instantiates its passes after checked arenas are available.
An observed plan shares a borrowed observer among distinct occurrences;
each compilation creates fresh invocation counts and temporary pass state.
The high-SPS observer verifies lexical ownership, branch joins, and root closure using borrowed phase data.
The unobserved default retains static composition.
`BackendProgram` records the selection that produced its frozen SPSLow input,
and its assembly cache belongs to that product.
Selecting another high-SPS plan requires a new lowering result, so cached assembly cannot cross selections.
[Assembly representation policy](#policy-selection) remains independently selectable and invalidates
that product's assembly cache.

High SPS is the configurable optimization boundary.
[`LoweringPipeline`](../../lang/assembly/src/pipeline.rs) composes required assembly construction,
stack analysis, and publication from a borrowed SPSLow program.
Its `with_native_frames` option returns a pass that ends in checked `NativeProgram::prepare`,
retaining the distinct portable and native output types and the native frame-planning error.
A future assembly rewrite must execute before the affected analyses,
or explicitly reestablish them before publishing the completed program.
Convergence and shared analysis caching require additional change and invalidation contracts.
Stage scheduling and prerequisite order are explicit in the selected sequence;
source query caching and revision ownership remain
with the [session's Salsa database](#c3-source-loading-sessions-queries-and-memory-retention).

#### Selecting and Inspecting Passes

These commands run from the repository root; [CLI setup](../../CONTRIBUTING.md#build-the-cli) explains how
to build or install `zydeco`.
Discover the optional passes and explain their enclosing compiler phase without loading a source:

```sh
zydeco passes
zydeco passes --sps-passes default
zydeco passes --sps-passes normalize,normalize
```

Every `build` target accepts `--sps-passes`.
The source interpreter (`run`), `check`, and the REPL do not execute this backend phase and do not accept the option.
For example, disable optional high-SPS transformations or select repeated normalization:

```sh
zydeco build lib/tests/core/representation-policies.zy --target zir --sps-passes none
zydeco build lib/tests/core/representation-policies.zy --target wasm-sps --sps-passes normalize,normalize
```

Use `--trace-passes` to report occurrences and timings, `--verify-passes` to check high-SPS invariants
before and after each selected pass, and `--dump-passes` to render the corresponding intermediate programs.
Trace and dump output goes to stderr, leaving the selected target's stdout intact.
With `none`, there are no optional occurrences to inspect; the required phase checks still run.

```sh
zydeco build lib/tests/core/representation-policies.zy --target zir \
  --sps-passes normalize,normalize --trace-passes --verify-passes --dump-passes
```

#### Pipeline Examples and Validation

The runnable Rust example [`cli/examples/pipelines.rs`](../../cli/examples/pipelines.rs) demonstrates static
and dynamic composition, a custom pass with borrowed configuration, and a timing comparison on the same checked source:

```sh
cargo run --release -p zydeco-cli --example pipelines -- --iterations 30 \
  lib/tests/core/representation-policies.zy lib/tests/core/gc-stress.zy
```

The CSV reports average pipeline construction and normalization times separately,
excluding source checking and high-SPS construction.
It also records the resulting computation-node count.
Use repeated runs when comparing timings; these measurements do not establish generated-program performance.

Focused checks cover ordering, failure boundaries, CLI selection, and applicable backend execution:

```sh
cargo test -p zydeco-utils -p zydeco-stackir --lib
cargo test -p zydeco-cli --test passes
```

The ZASM interpreter has no external-call dispatch; its coverage checks lowering,
while native and both WebAssembly backends execute host effects.
Numeric imports retained by an empty selection use the same arithmetic contracts as normalized primitive instructions,
including trapping operations and wide scalar boxes, as described under [primitive calls](#primitive-calls).

## C2. Compiler Data, Identities, Arenas, and Source Provenance

An ID's Rust type identifies its node category; its opaque key space and raw index identify an allocation.
A source binder, a particular checking occurrence, and an emitted runtime value are different identities.
[The arena utilities](../../lang/utils/src/arena.rs) separate issuing those IDs from owning and accessing their storage.

| Mechanism | Invariant |
| --- | --- |
| `IdAllocator<Scope>` | One non-cloneable sequential issuer owns a fresh key space and cursor. |
| Derived allocation | Entity identity, checking occurrence, derivation family, and local slot reproduce an allocation site. |
| `Allocates<Id>` | A capability declares which categories an operation may issue. |
| `ArenaSchema<Id>` | An owning representation declares the item stored under an ID. |
| Dense storage | The backing arena supplies the raw index and rejects another arena's key space. |
| Sparse, paged, and indexed storage | Producer-issued IDs retain their identity when storage changes. |
| Associative side tables | Each write explicitly inserts, replaces, upserts, or ensures an association. |
| `FrozenArena<A>` | Consumers receive storage without the builder's indexed-mutation capability. |

Sequential issuers normally live on a parser, resolver, or lowerer rather than its published arena.
High SPS retains its definition issuer until consuming closure conversion moves it into the low administrative arena;
low syntax has a separate issuer and never reuses high syntax IDs.
Statics producer queries derive occurrence IDs, while the checker materializes their returned fragments.
Rechecking the same source entity in a distinct occurrence must not overwrite an earlier elaboration.

Provenance records the actual relation between representations.
Surface-to-typed mappings are many-to-many because transparent syntax and repeated checking can share
or duplicate results; one typed node may produce several IR nodes.
Parsed entities use the tagged `EntityId` category rather than casts between definition, pattern, and term IDs.
Generated definitions and phase-local provenance belong to the phase that creates them.

[Span](../../lang/utils/src/span.rs) stores two `BytePos(u32)` endpoints: eight bytes per span.
Compact byte positions avoid retaining a path, line, and column at every syntax node.
A `FileMap` owns a file's path, retained text, global base, and line-start index;
`SourceMap` orders files by their global base.
File-local parsing starts at byte zero.
Merging rebases spans into one address space beginning at one, reserving the zero span for a dummy location.
The [merged surface span arena](../../lang/surface/src/textual/span.rs) owns the source map;
a cached parse template retains its local file map until assembly.

Location rendering finds the file and line by binary search and derives character columns from retained text.
It performs this work on demand, including after rejection; a `ResolveFailure` retains the error and span arena.
Editor conversion to UTF-16 belongs at the protocol boundary and does not change compiler byte offsets.
Before comparing a global span with an editor's file-local cursor or range, localize it through the owning map.
File attribution alone cannot validate this conversion: source-location tests must also check hover
and token ranges in non-ASCII text and in files beyond the first merged source.

Allocation and provenance changes should exercise arena tests and source-location tests,
including repeated checking and the distinction between local and merged spans.

### Choosing and Composing Traversals

Repeated structural recursion belongs beside the representation that defines its children.
A visitor observes that structure and accumulates facts; a folder transforms children and rebuilds their parent.
Clients supply semantic rules, while the shared operation describes how to reach or reconstruct ordinary children.
Phase-specific builders retain authority over allocation and provenance.

The implemented interfaces serve different questions and therefore expose different views:

| Input and purpose | Shared operation | Identity and scheduling contract |
| --- | --- | --- |
| File directives and documentation | [Source scan and analyzers](#shared-source-analysis) | File-local textual identities; analyzer interests select allocated terms, reachable terms, and text blocks |
| Bitter or scoped syntax reconstruction | [Owned surface folder](#surface-structural-rebuilding) | Child IDs flow through client hooks; the folder supplies copying, reference, scope, and rejection policies |
| Desugaring and lexical resolution | [Desugaring folders](#desugaring-folders) and [resolution events](#name-resolution) | Semantic handlers own telescope and scope transitions; builders and event consumers own their respective results |
| Resolved syntax analysis | [Scoped traversal](#scoped-structural-traversal) | Borrowed nodes; choose unique identities or path occurrences; cycles reject the traversal |
| Raw inferred classifiers | [Classifier folders](#classifier-folders) | Owned child reconstruction; preserve unchanged IDs; clients select substitution environments and reductions |
| Executable typed terms | [Residual runtime traversal](#residual-runtime-traversal) | Borrowed runtime nodes from the residual root; visit each identity once and exclude static evidence |
| Typed terms to lexical high SPS | [Residual lowering folder](#residual-lowering-folder) | Explicit reconstruction frames; inherited continuation stacks; fresh output syntax for every input occurrence |
| Lexical high SPS analysis | [High SPS traversal and analyzers](#high-sps-analysis-traversal) | Observe every incoming edge for ownership, but expand children and compute exit summaries once |
| Lexical high SPS normalization | [Normalization reconstruction](#normalization-reconstruction) | Explicit frames retain forward producer scopes and resume rebuilding with backward consumer demands |
| Lexical high SPS to first-order SPSLow | [Closure conversion folders](#closure-conversion-folders) | Occurrence-local renaming and ordered captures; fresh syntax with preserved allocation and publication order |
| First-order SPSLow analysis | [SPSLow traversal and analyzers](#spslow-traversal-and-analyzers) | Observe ownership edges and compute variable summaries in one scan; metadata adds no executable occurrences |

Before reusing a traversal, establish its input view, child environments, occurrence policy, and output identity policy.
A raw classifier, its normalized view, and a residual executable are different inputs even
when they share arena storage.
A source-node count, an occurrence count, and an allocation count likewise answer different questions.
Memoization belongs to the client contract: a node ID suffices only when the result is independent
of inherited context and of any state that can change during the cache's lifetime.
The concrete sharing and cache rules live with each interface above.

Independent analyzers can receive the same stream of events when their input view,
boundaries, and schedule agree and neither analysis consumes the other's evolving state.
They retain separate facts and diagnostics; composition does not require coupling their domain logic.
A validator should record independent failures while the shared traversal continues
under its established recovery rules.
The [diagnostic contract](#diagnostic-collection) distinguishes recoverable facts from a valid phase product.

Successive transformations require their own scheduling argument before fusion.
For example, folding literal addition and then incrementing every literal transforms `Add(Lit(1), Lit(2))`
into `Lit(4)`.
Running both rewrites at each node in postorder instead produces `Lit(6)`
because the parent receives incremented children.
A common input/output IR does not establish equivalence.
[Explicit pass composition](#compiler-pass-composition) preserves those stage boundaries;
the [SPS demand schedule](#consumer-demands) similarly remains a semantic dependency of normalization.

Traversal tests compare separate and composed facts, diagnostics, sharing, and provenance.
Visit and allocation counts establish the work removed by a migration; compilation-time
or memory claims additionally require measurements that include callback work and retained summaries.
[Further extensions](../proposals/traversals.md) remain proposals until a concrete client justifies them.

### Resumable Folder Execution

A folder that rebuilds a parent after visiting its children must retain the unfinished parent.
The shared [folder execution interface](../../lang/utils/src/fold.rs) separates
that continuation from the choice of where to store it.
It supports mutable compiler state without retaining a mutable borrow of the folder across child execution.

`Folder` defines `Input`, `Output`, and an owned `Frame` for the unfinished parent.
`enter(input)` begins an operation; `resume(frame, child)` continues it with a completed child result.
Both return `Step::Call { input, frame }`, `Step::TailCall(input)`, or `Step::Return(output)`.
A leaf returns directly without constructing a continuation frame.
A tail call transfers to another operation without retaining the current one;
both drivers execute these transfers in a loop while preserving the caller's unfinished parent.
A parent may select its next child using an earlier child's result and the current compiler state.
Frames contain owned local state, arena IDs, or references to stable external inputs;
they cannot borrow the mutable folder itself.

`Explicit::run(&mut folder, root)` stores unfinished parents in a vector and executes through a loop.
`Recursive::run(&mut folder, root)` retains the same frames in Rust recursive calls.
Both implement `Driver`, so a caller can select `D: Driver` statically without changing the folder.
Production residual lowering, Builtin packed value materialization, high SPS normalization,
closure conversion, and CPS assembly lowering use `Explicit`; `Recursive` supports bounded comparisons
and consumes native stack proportional to pending calls.
The driver introduces no boxed callbacks or individual heap allocation for each continuation.
Frame payloads and outputs may allocate according to the folder's representation.

The folder owns child order, scope transitions, allocation, provenance, sharing, caching, and recovery.
An error-valued output returns to its parent through the same protocol as a successful result;
the parent decides whether independent siblings remain visitable.
Drivers neither short-circuit errors nor implicitly deduplicate shared inputs.
A folder may retain separate typed result stacks when its domain operations produce different categories of syntax.
Its `Output = ()` then signals completion, and each reconstruction frame consumes exactly its own completed children.
The driver still owns the suspended calls; these result stacks retain domain data rather than pending work.
An explicit driver bounds only its own call depth: recursive semantic helpers and destruction
of nested frame payloads retain their separate stack requirements.

Driver regressions compare dependent child selection, repeated occurrences, event order, and independent rejection.
A 100,000-level non-tail fixture executes on a 128 KiB stack through `Explicit`.
A separate 100,000-step tail-call fixture uses the same small stack through both drivers
and checks that the original parent resumes.
The [Builtin packed value folder](../../lang/stackir/src/high/lower/builtin.rs) is a production client:
its product frame accumulates fields in input order and transfers the completed vector into the output node.
Its tests compare both drivers' materialized syntax and arena counts, retain the empty-product rejection,
and construct and destroy 16,384 nested products on a 512 KiB stack through `Explicit`.
These checks establish behavior and depth robustness, without claiming a compilation-time speedup.

The [high SPS pattern folder](../../lang/stackir/src/high/normalize/pattern.rs) copies one owned pattern layer
and replaces each child slot as its result returns.
Constructor payloads, alias components, and product fields retain their structural order.
The completed layer moves into the target arena with its original definition IDs,
source site, product layout, and protocol evidence.
This avoids a separate result stack and transfers child vectors directly into rebuilt nodes.
Pattern depth uses the selected driver independently of the normalizer's consumer schedule.
Driver comparisons cover these invariants and empty field vectors with nonzero physical arity;
the normalization depth regression retains 8,192 nested alias patterns on a 512 KiB stack.

### Surface Structural Rebuilding

Bitter and scoped syntax share `Pattern` and `Term<Ref>`, with source names
or resolved definition IDs in reference positions.
Their [owned rebuilding operations](../../lang/surface/src/fold.rs) describe immediate children once
for that syntax family.
`Pattern::fold_with`, `Term::fold_with`, and the copattern operations consume a node
and rebuild its children through a `Folder`.
Literal values, field names, meta annotations, and other non-child payloads retain their contents.
Internal terms are leaves, and sealing retains its wrapper around the transformed payload.

The folder supplies `fold_def`, `fold_pat`, and `fold_term` for ID-bearing children;
those hooks own arena lookup, recursive descent, and identity policy.
`InputRef` and `OutputRef` distinguish source and resolved references, and `Error` makes rebuilding fallible.
`fold_var` returns an output term: successful resolution can produce `Var(definition)`,
while supported recovery can produce an internal hole after recording an error.
Changing a binder identity does not implicitly rename references; each folder supplies its reference behavior.
Freshening preserves source names and uses `Infallible`.
The structural operations themselves neither issue arena IDs nor memoize and do not establish lexical environments.
Fixed independent child hooks are evaluated before propagating rejection.
`fold_items` controls iteration over collections: recovery-aware folders visit every independent item,
record each diagnostic at its producer, and propagate only the already-reported failure token.
A semantic handler retains responsibility for children that require a preceding child's environment.

Rebuilding follows the existing bitter copying order: annotations process payload then classifier,
view patterns process function then pattern, and binder-bearing forms process binder before body or bindee.
Binding tails follow their bindees. Recursive definitions and arms retain their sequence,
and a copattern spine processes its head followed by its tail items.
A monadic block processes body, monad, then algebra.
These are structural copying rules; a scope-sensitive folder must choose its own child environments
or handle the enclosing form before delegating to `fold_with`.

The [bitter builder](../../lang/surface/src/bitter/alloc.rs) owns one sequential ID issuer and its `BitterArena`.
Every `Alloc` operation stores a node and its textual origin together; consuming `finish` publishes a `FrozenArena`.
Desugaring owns a builder alongside its textual inputs and source-term memo table.
[FreshenFolder](../../lang/surface/src/bitter/freshen.rs) uses the same builder to copy an existing bitter fragment:
every reachable occurrence receives fresh IDs, including definition IDs, while retaining the original textual origins.
Source names remain unchanged for subsequent lexical resolution.
Source and signature boundary payloads are copied per occurrence, even when the input shares a provider.
There is no copy memoization, and source identities in `partial_binders` remain valid through the retained origins.
Freshening uses recursive descent and requires acyclic input; it does not provide the scoped visitor's explicit-stack
or cycle-reporting guarantees.

The folder is used for binder copies in annotated abstractions, generated binding classifiers,
and recursive binding sugar.
Its regressions check distinct resolved binders, retained provenance, copattern order,
shared annotation copying, and sealed payloads.

### Scoped Structural Traversal

Resolved syntax can share source roots, so structural analysis operates on a graph of arena entities.
[Traversal](../../lang/surface/src/scoped/traverse.rs) owns the exhaustive child enumeration and depth-first schedule;
independent `Visitor` implementations receive borrowed `Node` values at entry and exit.
An explicit work stack avoids recursion through the Rust call stack.
The traversal reads one immutable `ScopedArena`; visitors own their analysis state and do not schedule its children.

The root is a scoped `EntityId`. Definition IDs are leaves, including those reached through pattern binders
and variable references; the traversal does not follow them to defining terms.
It follows source and signature boundaries, annotations, and the elaborated children of blocks,
without following side tables or establishing a lexical environment.
Mobile syntax is an invariant violation because resolution must have eliminated it.
Term and pattern IDs must name existing nodes in the supplied arena.

Child order is deterministic and preserves the structural analysis order:

| Form | Child order |
| --- | --- |
| Term or pattern annotation | Payload, then classifier. |
| View pattern | Function, then pattern. |
| Abstraction, fixpoint, or quantifier | Pattern, then body. |
| Let, do, manifest existential, or packed value layer | Definition or bindee, pattern, then body or tail. |
| Match | Scrutinee, then each arm's pattern and tail in arm order. |
| Copattern clause | Patterns in spine order, then the tail; clauses retain their order. |
| Recursive group | Each definition's pattern and bindee in group order, then the tail. |
| Monadic block | Monad, algebra, then body. |

Products, applications, and data/codata arms retain their stored order.
This is an analysis schedule; [resolution](#c4-parsing-desugaring-and-name-resolution) owns lexical scope transitions.

`Sharing::UniqueNodes`, the default, delivers one entry and exit per distinct entity.
Completed nodes are skipped before lookup or descent, including a provider reached by multiple import boundaries.
Structural scheduling therefore takes work proportional to reachable nodes and edges, excluding the analyses' own work.
`Sharing::Occurrences` revisits completed nodes along each edge for analyses whose observations depend on the path.
Both modes distinguish an active ancestor from a completed node and reject a structural cycle with `TraversalCycle`.
Recursive language bindings remain valid because definition references are leaves.

`Together { first, second }` delivers each entry and exit to both visitors in declaration order.
Visitors can own or borrow their state and must have the same `Break` type.
They share the traversal's ordering, boundaries, and sharing policy; independently pruning a subtree is unsupported.
For independent, completing analyses this produces the same results as separate runs under that policy.
It does not fuse successive rewriting passes or preserve the error priority of running two validators in sequence.

`Traversal::run` returns `Result<ControlFlow<V::Break>, TraversalCycle>`.
A visitor's `Break` stops immediately, before subsequent callbacks, and returns the supplied domain value.
Neither a break nor a cycle balances pending exits or rolls back visitor state.
Every run has fresh traversal bookkeeping, so a configuration can be reused after either outcome.
Callers must distinguish a completed analysis from its partial state after stopping.

[ContextCollector](../../lang/surface/src/scoped/context.rs) is the postorder free-variable analysis.
It temporarily records bound and free definitions for patterns, and free definitions for terms.
`finish` retains only the term summaries as `TermContexts`.
These summaries are independent of the incoming environment, so `TermContexts::collect` uses unique-node traversal.
Other analyses can share its traversal through `Together`.

The [traversal regressions](../../lang/surface/src/scoped/traverse/tests.rs) cover composed results,
callback order and early breaks, cycles, dependent binders, and deep syntax.
A repeated-import graph with 37 distinct term nodes has 16,381 occurrences;
the unique-node traversal delivers 37 entries and exits.
This checks traversal work, not end-to-end compilation speed.

## C3. Source Loading, Sessions, Queries, and Memory Retention

### Abstraction Carriers

The language reference defines the [abstraction levels](language.md#abstraction-levels) and their responsibilities.
Their current implementation carriers are:

| Abstraction | Current carrier |
| --- | --- |
| Project | `PackageBindings`; shared project preparation is being rebuilt |
| Package | `Package` / `PackageId` |
| Compilation unit | `ExecutableProgram`, `LibraryProgram`, and `UnitProgram` |
| Semantic unit | Partly represented by `ProgramAnalysis` / `CheckedProgram` |

`CompilerSession` manages source revisions, snapshots, and cached queries for these abstractions.
Reusing semantic analysis requires matching source inputs and resolution context.
The [package proposal](../proposals/package-management.md#carrier-implementation) records the pending carrier
consolidation; the [documentation proposal](../proposals/documentation.md) records the remaining query and output work.

### Source Loading and Package Selection

The [source graph](../../lang/session/src/source/graph.rs) identifies canonical paths,
numbered inputs, imports, and type companions.
The [loader](../../lang/session/src/source/loader.rs) obtains source text through session inputs,
including editor overlays; [assembly](../../lang/session/src/source/program.rs) combines providers
with explicit source and signature boundaries.
Loading visits every independently available import and companion signature before returning.
A template cache records accepted, missing optional, and rejected files by canonical path;
the first request reports a provider failure, and subsequent requests do not replay it.
A graph-node cache distinguishes active, completed, and rejected source roots.
Active roots permit recording cyclic edges, while rejected roots retain only temporary edges for diagnostics.
The loader visits those edges to collect DFS back-edge cycles, including independent cycles in rejected sources;
it does not enumerate every possible path around a cycle.
Read, parse, directive, and cycle failures leave through the nonempty `SourceLoadErrors` collection.
The graph and assembly boundary is strict: no partially loaded graph is published or cached as successful.
Session revisions invalidate rejected results when their source inputs change.
The [loader regressions](../../lang/session/src/source/tests.rs) cover shared rejected providers,
independent imports and signatures, multiple cycles, and correction through overlays.
The independence of provider inference and source scope is specified in [L12](language.md#12-sources-imports-and-entry).

[Package selection](../../lang/session/src/source/package.rs) separates authored references (`SourceReference::Package`
or `SourceReference::Path`) from resolved source entries (`PackageId`).
The surface decoder indexes explicit meta annotation names and retains their term identities, roles, and relationships.
Compiler queries consume immutable `PackageBindings` supplied by project preparation.
The [project-context rules](language.md#names-and-project-catalogs) specify root selection,
bounded discovery, and conflicting-name rejection;
the [shared preparation implementation](../proposals/package-management.md#shared-project-context) is being rebuilt.
Those bindings are part of every dependent query key and are retained with analyses
and documentation-worker requests; changing bindings changes the dependent analysis identity.
Source loading does not expand discovery.

Inspection shares each parsed file and extracts direct code edges
without materializing a compiler graph per declaration.
The loader keys roots by canonical path and textual term, following ordinary imports and companion signatures only.
Each graph node shares its containing template; only the selected term's code, documentation, and warnings participate.
Several roots can share one file, and merged source inputs and spans deduplicate that file.
Typed relationships remain separate from code dependencies;
[test planning](../proposals/package-management.md#validation-and-operation-planning) is being rebuilt
around the selected project's forward and reverse associations.
The [source-package section](language.md#source-packages) owns selection, discovery, and operation rules.
Extensions to package resolution follow the
[shared design of compilation units, FFI, and package management](#compilation-unit-preparation-and-artifacts).

The CLI's [execution runner](../../cli/src/execution.rs) consumes checked `ExecutableProgram` snapshots,
sharing their immutable arenas across targets and lowering once for compiled backends.
It prepares temporary artifacts before execution and owns their cleanup.
CLI `run`, package `test`, and the Rust source fixture harness use this runner;
[L15](language.md#selecting-an-execution-backend) owns selection and execution policy.

`CompilerSession` is the Salsa database and revision owner.
[ScopedData](../../lang/statics/src/query/input.rs) connects the resolved root, scoped arena and spans to `TyckDb`.
The coarse [check_source query](../../lang/statics/src/query/source.rs) runs the mutable checker, finalization,
coverage, and static elaboration together, then publishes an `Arc<StaticsArena>` and a checked or rejected outcome.
Splitting those phases into separately copied arenas would multiply the dominant materialization cost.

### Shared Source Analysis

A parsed file answers several questions before assembly: which sources it imports,
where documentation attaches, which Builtin roles and intrinsic or literal splices need validation,
and which packages and discovery rules it declares.
The [source scan](../../lang/surface/src/textual/source/scan.rs) computes requested reachability once,
sweeps allocated terms once, and then visits file text blocks when requested.
Each term or existential parameter annotation event borrows one decoded semantic meta tree,
retaining textual identities for exact argument spans.

`SourceAnalyzer` separates these questions into independent consumers with typed results.
Pairs of analyzers compose statically: their `SourceInterest` values are unioned before scanning,
and both consumers observe the same immutable events.
Import-only queries omit reachability and unrelated decoding; documentation-only queries do not validate imports.
The full `SourceInventory` composes documentation, unattached-text warnings, imports,
Builtin roles, intrinsics, literals, packages, and discovery.
Query entry points use the same analyzers.

Domains preserve the distinction between the returned tree and allocations retained by parsing.
Documentation, packages, and discovery use reachable terms.
Import, intrinsic, and literal validation use every allocated term;
Builtin validation additionally visits existential parameter annotations.
Unattached-text warnings use relevant attachments throughout the arena and file trivia.
Package selection remains downstream of this complete-file inventory.

`SourceAnalysis` retains valid facts and diagnostics from independent sites.
Malformed arguments block only checks that require their decoded values; other sites still run.
Package duplicate checks retain the first valid declaration in source order.
Discovery validates each site's arguments and checks duplicates over the complete annotation set;
placement is checked when there is one unambiguous declaration.
Successful query ordering remains source order, with packages ordered by name.
The inventory is keyed by file-local textual identities.
Assembly and desugaring consume syntax at later identity and span boundaries;
reusing decoded inventory facts there would require an explicit remapping
under the [provenance contract](#c2-compiler-data-identities-arenas-and-source-provenance).
The loader concatenates typed diagnostic collections and publishes a `SourceTemplate` only after every analyzer accepts.
Diagnostic order is a presentation choice, not an API guarantee; strict query facades return the full collection.
Individual malformed directives may stop at their first local error until a further recovery point is justified.

### Query and Checker Ownership

Allocation-producing syntax judgments are producer queries: they return node identities
and immutable outcomes from explicit inputs.
The checker inserts those outcomes into its materialized arena.
The [intrinsic singleton query](../../lang/statics/src/query/intrinsic.rs) derives identities
from a synthetic check-wide site, independent of the first source occurrence naming an intrinsic.
`Tycker::new` materializes those singletons and retains their complete `IntrinsicStatics` identities
for construction during checking.
Finished arenas retain the nodes while dropping this construction-only table.
Unification, fill resolution, substitutions, opening of a packed value, copattern elaboration,
and recursive-group processing retain a checker-owned algorithmic core.
Their intermediate results depend on mutable inference state; replaying a site alone cannot reconstruct that state.

A cached node must therefore be distinguished from a pure source-to-judgment function.
Per-call arena snapshots would make query keys expensive and defeat reuse;
a tracked mirror does not make an arbitrary table cell reconstructible from its site.
A worklist redesign of the solver would be a separate algorithmic change.
The current boundary gives producers deterministic identities while letting inference retain its sequential state.
C5 defines the publication boundary after that state stops changing.

### Analysis Facts and Materialization

`analyze_source` builds documentation while the complete arena is available,
then retains `StaticsArena::clone_keyed_indexes()` in `ProgramAnalysis`.
That analysis is a source and fact snapshot, not a complete typed tree.

| Retained keyed information | Transient occurrence payload |
| --- | --- |
| Definition annotations, bodies, alias entry points, and intrinsic roles | Typed values, computations, and patterns |
| Fill sites, solutions, scopes, and abstract identities | Per-occurrence kinds and types |
| Data/codata definitions, term facts, normalized top annotations, and provenance indexes | Derived node columns needed during full-tree traversal |

Retained facts support random access by semantic identity where recovery would require a regional recheck.
Occurrence payload is materialized for full traversal; retaining it longer requires a measured consumer benefit.
Keyed `type_sites` and `term_norms` serve current top-annotation queries without replaying arbitrary inner nodes.

Fact queries read those retained indexes. Full-tree consumers call `materialize_arena`, `checked_program`,
or `executable_program`, which recover the arena through the coarse check.
Materialization uses the session's current inputs, so callers must pair an analysis
with the matching session revision or snapshot.
Live consumers share the complete arena through `Arc`; lowerers add phase-local metadata instead of cloning it.
Checker-only state, including per-node typing environments, is stripped before full-arena publication.
Cajun may keep a project's materialization alive while that project is in use.

`check_source` has a one-entry LRU configuration.
The test session pool explicitly triggers eviction between analyses and periodically replaces its database generation.
Fine-grained judgment memos have a different lifetime from the full arena;
dropping an analysis alone does not promise reclamation of all database storage.
[Retention design questions](../proposals/arena-gc.md) concern memo lifetime, compact facts, and parsed-source storage.
[Session tests](../../lang/session/src/source/tests.rs) and query tests check facts after eviction,
re-materialization, source hygiene, and failed revisions.
Compiler cache reclamation is independent of program-value collection in C12.

## C4. Parsing, Desugaring, and Name Resolution

[Textual syntax](../../lang/surface/src/textual/README.md) owns Logos tokens, literal decoding,
LALRPOP parsing, metadata syntax, spans, trivia, and retained layout intentions.
Strict parsing supplies compilation input; recovering parsing supplies structured holes
and recovery evidence for tooling.
A recovered edit is not silently accepted as an executable program.
Formatting consumes the same textual model, so parser changes must preserve the information required by C15.

### Recovering Parsing

Completion is normally requested while a token or surrounding construct is incomplete.
A successful strict parse therefore cannot be a prerequisite.
Reusing an older successful parse alone is insufficient: the edit may introduce a new binder, change shadowing,
move the cursor into another scope, or establish a new expected type.

The grammar uses LALRPOP's special `!` symbol for recovery.
At a parser error, LALRPOP can inject that symbol, execute a recovery action,
discard input until the grammar can continue, and report both the parse error and discarded tokens.
Recovery points must be selected by the grammar author, and lexer failures are not recovered automatically.

That mechanism is suitable for retaining surrounding syntax, but it is not itself a completion model.
Zydeco needs two additional distinctions:

- A **completion hole** deliberately marks the cursor.
  It is not a user error and must remain identifiable through textual syntax, desugaring, resolution, and checking.
- A **recovery node** replaces malformed or missing syntax around the cursor.
  It carries a typed recovery identity and diagnostic range rather than becoming indistinguishable
  from an authored `_` hole.

The normal lexer never emits a completion token.
A tooling iterator wraps the ordinary token stream, removes the active token when appropriate,
and inserts one zero-width completion marker at the cursor.
The marker is deliberately not an authored `_` token and is not accepted as an ordinary grammar terminal.
A term or pattern recovery point turns it into a typed hole when that category is admissible;
otherwise the parser records fixed expectations such as `in`, `=>`, or `end` at the marker.
A marker-only issue is then removed from ordinary syntax diagnostics.
If the same recovery discards source tokens too, its diagnostic is retained at the first discarded source token.
The synthetic marker never appears among the discarded source tokens reported to callers.

LALRPOP's `expected` lists contain diagnostic terminal names.
They may be useful evidence while prototyping, but Cajun must not parse those strings.
The surface parser converts them into typed `TokenKind` values before exposing them.
These payload-free kinds are derived from the lexer's `Tok` declaration,
so an expected identifier needs no invented source text and no separately maintained terminal inventory.

`TokenMetadata` reads fixed spellings directly from Logos's `#[token(...)]` attributes.
A token with several aliases selects one registered spelling explicitly: both `def`
and `define` lex as `Define`, whose canonical spelling remains `define`.
Variable lexical categories have no fixed source spelling.
Their grammar names default to the variant name, with explicit overrides for existing labels such as `LowerId`.
Trivia and malformed lexical tokens are marked as excluded from parser expectations;
the synthetic `Completion` and `Invalid` terminals stay private to the parser.

The same generated metadata supplies fixed-token formatting and the parser's terminal-name conversion.
The LALRPOP external-token table remains explicit grammar integration:
conformance tests compare both terminal names and their mapped lexer variants.
This preserves the grammar as the syntax specification while avoiding an additional runtime catalog.
Unknown settings, ambiguous canonical spellings, and conflicting terminal names fail macro expansion.

#### Strict and Recovering Modes

The parser exposes two explicit outcomes over the same grammar:

- **Strict parsing** is used by compilation, formatting, and normal source loading.
  Any recovery issue makes the source invalid, preserving the current language acceptance boundary.
- **Recovering parsing** returns the partial textual arena, its root when one was recovered,
  and a collection of typed recovery issues.
  Editor queries may continue through recovered holes.

The generated LALRPOP parser sits behind this surface API so strict callers cannot accept a recovered source
by ignoring its issues.

#### Trust Boundary and Recovery Contracts

The implementation keeps `parser/grammar.lalrpop` as the single syntax specification
and its generated parser as the reference implementation.
Both modes run that parser; strict mode accepts only a returned root with no issues.
Recovery policy lives in the two `!` productions and LALRPOP's runtime.
There is no handwritten parser, synchronization algorithm, repair-search engine, or separate proof system to maintain.

The surrounding Rust code establishes smaller integration contracts.
A grammar semantic value carries either ordinary syntax or an opaque recovery handle.
The enclosing allocation rule records the exact `PatId` or `TermId` against that handle.
An authored hole at the same span, a previous parse's allocation, and a second zero-width hole
therefore cannot be mistaken for the same recovery event.
An abandoned semantic value can have no allocated hole; an allocated node can also outlive its parser stack entry.
Completion exposes an allocated hole only when it remains reachable from the returned root.
Ordinary recovery issues can retain links to abandoned allocations for diagnostics.

`RecoveringParser::new(source)` borrows a source snapshot,
and `RecoveringParser::at(source, offset)` additionally validates and binds a completion cursor.
Parsing then takes only `&mut Parser`, never a second source argument.
The cursor's bounds, UTF-8 boundary, and replacement range consequently belong to the actual input being parsed.
One lexical stream supplies comment and quoted-literal boundaries to the parser and Cajun,
including the EOF cursor of an unfinished token.
Byte ranges are retained as byte ranges; layout lookup uses character boundaries.

Lexical failures become a grammar-known `Invalid` terminal with a typed `LexicalError` payload.
Like `Completion`, it has no successful production and is excluded from public expectations.
This lets LALRPOP recover using its existing points instead of treating a lexer error
as an early return or an apparent EOF.
Diagnostics retain typed invalid tokens even when they are discarded alongside the completion marker.

Numeric conversions use LALRPOP's fallible actions.
Metadata integers remain signed 64-bit values, while ordinary integer literals remain arbitrary precision.
A failed conversion returns a source-located `LiteralError`, keeps any earlier recovery issues,
and returns no syntax root.
Fallible-action errors are fatal in LALRPOP; they do not run `!`.
Recoverable invalid metadata values remain unsupported; conversion failure returns diagnostics without a syntax root.

These contracts are regression-tested against the reference parser.
They do not claim formally verified parsing, minimal edits, or maximal context retention.
LALRPOP may pop consumed stack entries as well as discard unread tokens; its `dropped_tokens` list describes the latter,
not every source fragment replaced during recovery.

The grammar currently recovers at term and pattern atoms.
Malformed lists can retain later complete bindings through those points;
additional list or arm productions need a concrete lost-context regression.
A second handwritten repair parser would create a competing syntax contract.
Recovered nodes remain semantic holes with recorded origins,
while strict parsing rejects the recovery before elaboration.
The [recovery tests](../../lang/surface/src/textual/parser/tests/recovery.rs) pair retained contexts
with strict rejection, including authored holes, Unicode cursors, invalid literals, and abandoned recovery events.
Further grammar and candidate extensions remain in the [completion proposal](../proposals/completion.md).

### Desugaring Boundary

[Bitter desugaring](../../lang/surface/src/bitter/README.md) removes surface sugar while keeping unresolved names.
It expands binding headers and telescopes, makes nominal sealing and CBPV introductions explicit,
and retains special boundaries such as classifier queries and monadic payloads.
Source assembly has already resolved import and literal splices.
Primitive terms have compiler-owned identities, and every generated node records its textual origin.
A special metadata node must survive whenever ordinary forwarding would change its checking environment or expectation.

### Desugaring Folders

[`DesugarFolder`](../../lang/surface/src/bitter/desugar/mod.rs) owns textual lookup,
recursive lowering, the `BitterBuilder`, and a source-term memo table.
Successful and rejected terms enter that table through one path;
reusing a rejected source node does not repeat its diagnostic.
Independent child checks collect their errors before rejecting the enclosing construction.
A rejected source returns all collected errors with their source context and publishes no complete bitter arena.

The [telescope rules](../../lang/surface/src/bitter/desugar/telescopes.rs) lower parameter sequences,
flatten consecutive existential layers, and construct quantifier and packed value layers.
The [binding rules](../../lang/surface/src/bitter/desugar/bindings.rs) interpret binding flavors
and build terms together with classifiers over the same parameter fold.
Classifier state distinguishes absence, an annotation on the complete binding,
and an annotation extended across its parameters.
Only the last case freshens classifier binders.
Destructor parameters discard a propagated abstraction classifier, replace a binding classifier with a hole,
or reject a value abstraction, according to the enclosing rule.

Already-lowered construction receives the builder;
[CBPV introductions](../../lang/surface/src/bitter/desugar/cbpv.rs) share this allocation and provenance boundary.
Rules retain their semantic child schedules: a binding lowers its binder, bindee, classifier, and parameters;
a packed value lowers its body before processing parameters in reverse, with evidence before the parameter form.
Meta annotation inspection precedes payload lowering when its rule depends on authored syntax.

[`MetaRules`](../../lang/surface/src/bitter/desugar/meta.rs) inspects read-only textual syntax
and selects one typed action: preserve an annotation, construct an intrinsic, query a classifier,
build a monadic block, or record partial binders and preserve the annotation.
The catalog identifies the recognized kind once; arbitrary annotations retain their structure.
Intrinsic and FFI payload checks inspect authored syntax before any lowering can erase grouping.
Intrinsic construction skips its hole payload; classifier queries and monadic actions lower their payload once.
Partial actions carry the header's textual binder identities and record them before visiting the payload,
including when that payload has already been memoized.
Existential parameter annotations use their own allowed-role validation and retain their precise annotation sites.

### Name Resolution

[`ResolveFolder`](../../lang/surface/src/scoped/resolver.rs) replaces names with `DefId`s
while preserving source identities.
A scoped builder materializes resolved nodes and issues new identities only for context elaboration.
The folder owns the diagnostic collector, required dependency analyzer, and a statically composed observer product.
Its standard profile always includes reference indexing and documentation; `with_observer` adds passive consumers.
The [scope module](../../lang/surface/src/scoped/scope.rs) supplies the same shadowing rules for lookup and enumeration.

The inherited `ResolveEnv` is explicit.
Pattern resolution returns its identity and extended environment.
Classifiers precede their annotated binders; dependent pattern sequences
and copattern spines thread environments left to right.
Match arms receive independent environments. Monadic blocks resolve their basis before their body.
An exhaustive constructor classification delegates ordinary reconstruction to the shared structural folder;
constructors with binding, boundary, or scheduling effects have explicit semantic handlers.

Source and signature boundaries start with isolated environments.
A shared provider is resolved once; cached success and rejection do not replay its events or diagnostics.
Successful lookup emits a borrowed `ResolvedReference` containing the occurrence,
selected definition, optional owning `BindingSite`, and active enclosing bindings.
Failed lookup records its diagnostic and emits no successful-reference event.
It becomes a hole only inside the resolving computation so other names can be checked.

[`ResolutionObserver`](../../lang/surface/src/scoped/observers.rs) receives references
and scope events without access to mutation, pruning, or rejection.
Pairs forward events and pair their final outputs.
Reference indexing constructs the definition-to-use relation.
Documentation captures the scope before an annotated payload.
Hole events retain their exact textual origins; completion captures only the requested origin
and keeps its last actual visit.
Holes synthesized from failed name lookup do not emit further scope events.
The scope view is borrowed; snapshots are allocated only by consumers that retain them.
Context elaboration emits no second event stream for generated wrappers.

Blocks require a preparatory binder scan so forward references can resolve before their definitions are visited.
The [block collector](../../lang/surface/src/scoped/blocks.rs) respects nested block and provider boundaries,
projects candidate binders once, and installs an unambiguous block-wide environment.
[`DependencyAnalyzer`](../../lang/surface/src/scoped/dependencies.rs) receives the same reference events
but is a required collaborator: each block needs its result before it can be elaborated.
`begin_block` seeds every candidate, including candidates with no edges.
An event adds dependencies to every active binding whose owner matches the dependency's owner;
nested block graphs remain separate.
`finish_block` removes and returns the completed graph, while `abort_block` discards an incomplete graph
before recovery continues outside that block.
All graphs must be closed when resolution finishes.

Strongly connected components form a condensation DAG; source order breaks ties and orders recursive members.
All recursive parameter components are diagnosed before elaboration.
Parameters become abstractions, acyclic definitions become lets, and recursive definition components become `RecGroup`;
the checker subsequently verifies those groups' admissibility.
`Residual` indirections preserve ownership at the original mobile sites after binders move.
The [context collector](#scoped-structural-traversal) summarizes free definitions in the elaborated term for checking.

Strict publication rejects the entire scoped program when any diagnostic was recorded.
Independent children and block candidates continue after a failure where their inputs remain available.
Duplicate binders are all diagnosed, then resolution skips the ambiguous block scope instead of choosing a winner.
Completion can retain a request-local program with unbound-reference recovery;
other failures retain captured observations but prevent program publication.
No rejected strict result reaches type checking or execution as a normal program.
Parser and completion tests, observer visit counts, dependency lifecycle tests,
and the [uniform-term fixtures](../../lang/tests/cases/uniform-term) exercise this boundary.
The [binding rules](language.md#3-bindings-and-scope) explain placement and identity;
[deferred recursion work](../ideas/recursive-admissibility.md) retains stronger admissibility questions.

## C5. Typed Representation, Judgments, and Inference

The [checker guide](../../lang/statics/src/check/README.md) maps the implementation modules.
`Tycker` carries one source check's mutable state; `Tyck` and `PatternAction` select synthesis or analysis.
Term and pattern dispatchers own task stacks, allocation-site guards, prepared expectations, and source-fact recording.
Syntax-family rules should use those entry points so recursive judgments preserve the same administrative invariants.

[Typed syntax](../../lang/statics/src/syntax.rs) separates kinds, types, patterns, values, and computations.
`TermAnnId` pairs a sorted term with its classifier.
Kinds and types may initially contain fills; annotations, environments, nominal identities,
and source occurrences are recorded in the [statics arena](../../lang/statics/src/arena.rs).

| Mechanism | Typed representation and checking responsibility |
| --- | --- |
| Kinds and type functions | `VType`, `CType`, arrows, named kinds, type abstraction/application, and substitution |
| Nominal and abstract types | Seals, `AbstId`s, definition bodies, and witness scopes |
| Ordinary CBPV functions | `Arrow`, `Forall`, abstractions, and value/type application |
| Value functions | `ValPi`, its binder sort, and witness projections for structured arguments |
| Value calculations | `Value::Match` with value arms and `IntValueOp` with two checked integer operands |
| Packed values | `Exists`, manifest equations, `ManifestKind`, and static-prefix patterns |
| Dependent computation functions | `PackPi` with canonical witnesses and a dependent codomain |
| Products and named fields | Component vectors, labels, and resolved routes with physical product positions |

### Inference and Reuse

The [source inference rules](language.md#4-classification-and-inference) determine where flexible value types arise.
`InferenceRegion` in [source.rs](../../lang/statics/src/check/source.rs) records inherited fills at entry;
closing the region rejects newly introduced pattern fills whose solutions remain incomplete.
A source closes independently before `CheckedTerm::reconcile_k` compares an import-site expectation.

[Compatibility](../../lang/statics/src/check/lub.rs) combines constraints through `Lub`.
[Shape refinement](../../lang/statics/src/normalize/inference.rs) creates component fills
of the required CBPV sorts and retains their originating site.
Filling checks occurs and visible-skolem conditions.
Sharing a flexible type across occurrences intersects admissible scopes; subsequent solutions obey that intersection.
A failed speculative fill restores both solutions and scopes.
Diagnostics retain the inference site and the conflicting body or call-site constraints.
Lexical and abstract-witness substitution follow solved type holes before rewriting their solutions.
This lets an inferred factory result specialize independently at each application, including value-function fields;
the shared solution itself remains unchanged.

`CheckedTermRepository` retains a canonical synthesis outcome per resolved term for sources,
classifier queries, and monadic payloads.
Reuse requires a context extension preserving every original binding and visible witness;
successful repeated or nested requests must agree on the arena root.
This permits a recursive annotation to be revisited after recursive bindings are installed.
A failed synthesis is retained as a recorded rejection within that checker,
including an outer request that fails after a nested request has retained a root.
Later references propagate that failure without repeating synthesis.
Use-site reconciliation still runs separately; a caller's incompatible expectation does not reject the shared provider.
A `TypeOf` boundary synthesizes its operand once, extracts and scope-checks the existing classifier IDs,
and reconciles expectations afterward.
It does not forward its own expectation, annotation, or seal into the operand.
In particular, direct extraction from `ret v` relies on recording `Ret A : CType` correctly.

### Checker Recovery

Checking can continue after an error when the next judgment's inputs are already established.
The checker owns typing environments and inference constraints, so its recovery points follow judgment dependencies.
A structural visitor alone cannot supply the missing classifier of a failed function or the scope of a failed binder.

[ResultKontIterator::collect_k](../../lang/utils/src/err.rs) exhausts a sequence of independent `_k` requests.
Each failing request stores its diagnostic and returns `KontFailure`;
the collector returns all successful results in order only when every request succeeds.
Any failure rejects the combined result, so callers cannot accidentally publish an incomplete product or case tree.
The migrated judgment boundaries are:

| Boundary | Work that continues | Required evidence |
| --- | --- | --- |
| Product synthesis and checking | Every component, including the final component | A shared lexical environment; checking first establishes component classifiers and arity |
| Packed value payload checking | Every remaining runtime component | The dependent witness prefix has succeeded and instantiated the body classifier |
| Data and codata declarations | Constructor parameter types and destructor result types | The declaration's required kind |
| Match arms | Other patterns and their bodies, including result-type reconciliation | A checked scrutinee; each body requires its own successful pattern and scope closure |
| Comatch elaboration | Independent clause bodies, destructor groups, and argument-pattern checks | The expected computation classifier and the current clause prefix |
| Finalization | All kind and type roots | Completed source judgments; dependent validation still requires successful normalization |

Kind and result-type reconciliation retain the corresponding component or arm as their diagnostic site.
A failed match pattern skips that arm's body while other arms continue.
Dependent telescope witnesses, sequential local bindings, and malformed shared clause prefixes remain prerequisites:
recovery does not invent their environments or annotations.
The [diagnostic contract](#diagnostic-collection) owns collection, deduplication, and frontend reporting.

Imports provide a further independent boundary because each provider checks in its own empty source environment.
If root judgments reject, `IndependentSources` uses the [scoped visitor](#scoped-structural-traversal)
to collect reachable source and signature boundaries in postorder.
The checker then synthesizes boundaries that ordinary checking did not reach, visiting dependencies before importers.
This recovers a later imported source even when an earlier local binding prevented traversal of the importing body.
The existing source-boundary rules close inference and check signature requirements;
canonical synthesis outcomes prevent repeated provider work.
Successful programs incur no recovery scan.

These outcomes belong to one checker invocation; an edited source starts a new check against the current query inputs.
Rejected checks retain established static facts for diagnostics and tooling, but expose no checked root.
The [source diagnostics regressions](../../lang/tests/tests/diagnostics.rs)
and [session regressions](../../lang/session/src/source/tests.rs) cover independent errors, exact locations,
shared providers, unavailable binder scopes, rejected parent products, and correction through overlays.

### Witness Evidence and Field Lookup

A manifest entry checks its equation and substitutes it through the remaining telescope;
an abstract entry introduces an identity subject to scope checks.
Source formation and opening rules live in [L9](language.md#9-polymorphism-and-packed-values).
`functions` establishes canonical witness telescopes for dependent binders
and substitutes caller-visible evidence at application.
A runtime thunk's implementation can be unknown while its signature supplies the dependency.
Value-function witness projections separately record which structured argument components supply evidence.
`PackPiInstantiationState` consumes the physical leading existential prefix
and substitutes its abstract witnesses through the codomain.
`ValPi` has a separate projection-based argument representation;
its structured evidence handling does not extend `PackPi` to witnesses nested beneath arbitrary product fields.

[Projection checking](../../lang/statics/src/check/projection) resolves a unique route
through the receiver's allowed structure, including the identities shared by one selective opening of a packed value.
The resolved route records every product position and its product type.
Selected runtime fields elaborate to ordinary typed patterns; unselected static positions receive internal witnesses.
A whole-value alias carries the same opening's prefix for forwarding.
Witness inspection uses C6's static reducer and never obtains evidence by executing a computation.

### Classifier Folders

Substitution and finalization repeatedly rebuild the same classifier shapes.
The [classifier folder](../../lang/statics/src/fold.rs) owns that exhaustive structural reconstruction;
`LexicalSubstitution`, `AbstractSubstitution`, `HoleResolver`, and `FilledNormalizer` supply its local rules.
`TypeFolder::fold_children` consumes a raw `Type` and transforms its immediate children in field order.
It reads the inferred graph through `types_pre`; following solutions, unfolding definitions,
reducing applications or projections, and consulting normalized views remain explicit client operations.

A body callback receives `TypeScope`, distinguishing type abstractions, universal
and existential telescopes, value functions, and witness-dependent functions.
Domains precede their dependent bodies.
The callback chooses the inherited substitution: ordered abstract substitution removes formal witnesses
under type abstractions, `ValPi`, and `PackPi`, and applies only later assignments to a replacement.
Universal and existential bodies retain the substitution used when opening their telescopes.
Lexical substitution uses the supplied lexical environment.
Binder pattern IDs, witness identities, and witness projections are preserved by structural rebuilding.

`TypeRebuilder` returns the original ID when children and kind are unchanged.
Changed paths allocate through `Tycker` with the caller's environment and classifier;
rebuilt labels transfer Builtin roles and member provenance.
Nominal data and codata definitions receive fresh IDs only when their arms change.
Filled normalization keeps nominal definition IDs and finalizes their arm classifiers through the arena-wide loop.
This preserves the normalized lookup view attached to the existing arm IDs.

The folder introduces no implicit memo table. Substitution can depend on an environment or assignment suffix,
whereas finalization shares caches only after inference closes, as specified below.
Occurrence-sensitive support collection retains its own scope-aware traversal.
[Folder regressions](../../lang/statics/tests/folders.rs) check unchanged identity
and allocation counts on shared tails; [substitution](../../lang/statics/tests/substitution.rs),
[dependent arrows](../../lang/statics/tests/pack_pi.rs),
and [scope rejection](../../lang/statics/tests/existential_scope.rs) exercise the semantic client rules.

### Finalization

After inference closes, hole solutions are stable for the remainder of finalization.
[Resolution and normalization](../../lang/statics/src/normalize) share memo tables across every arena root,
rebuilding changed paths and reusing results for shared tails.
Original IDs remain valid lookup keys; `kinds_normalized` and `types_normalized` store changed finalized views,
while unchanged IDs expose their existing nodes.
Missing solutions and sort errors remain diagnostics.

The shared context bounds structural traversal by the graph's nodes and edges, plus reductions that create new nodes.
Starting a fresh memo at every root would repeatedly traverse shared signature tails.
Compiler-owned identity maps use the repository's fast internal hashing;
changing storage must preserve key-space identity.
Finalized annotations feed coverage, residualization, and editor facts.
[Inference](../../lang/tests/tests/inference.rs), [classifier-query](../../lang/tests/tests/typeof.rs),
and [statics tests](../../lang/statics/tests) cover closure, scope, rejection, and reuse.

## C6. Typed Elaboration, Residualization, and Validation

Elaboration turns checked source mechanisms into a form shared by execution backends.
It runs in the unpublished arena so the source root and source facts remain available
while generated nodes acquire fresh identities and provenance.
The coarse check orders judgments, hole resolution, type normalization, coverage,
and static-root elaboration before publishing its result.

### Monadic and Copattern Elaboration

[Monadic checking](../../lang/statics/src/check/monadic.rs) checks the supplied `Monad` and `Algebra` basis,
synthesizes the payload once under its monadic environment, and passes a `CheckedTerm`
to the [algebra translation](../../lang/statics/src/elaborate/monadic).
The translator constructs lifted types, structures, and terms, rebinding their static evidence into the output context.
It translates the retained checked payload rather than rechecking its source under unrelated expectations.
[L11](language.md#11-relative-monads) specifies the source translation and basis;
generated code must satisfy the same finalization and residual representation boundaries as ordinary terms.

[Copattern elaboration](../../lang/statics/src/check/copattern.rs) follows the expected residual classifier:
a codata step groups destructor clauses, an arrow step introduces a shared argument
and accumulates its patterns, and a universal step introduces a type binder.
On reaching bodies, pending argument patterns become a correlated match.
A witness-dependent boundary currently admits one clause whose witnesses can scope the dependent result.
The output has one typed arm per destructor and hints identifying generated argument matches and witness binders.
Repeated source destructors can therefore be exhaustive alternatives over arguments
without becoming duplicate typed arms.

### Coverage

[Coverage validation](../../lang/statics/src/validate/coverage.rs) consumes normalized typed syntax
and its data/codata hints.
It converts variables, holes, and admitted alias groups to wildcards, preserves structural heads and product arities,
erases type witnesses, and treats literal observations as opaque to structural coverage.
Opaque patterns contribute no wildcard row.
This implements the conservative source policy in [L7](language.md#7-patterns-and-coverage).

A matrix row is one alternative; its columns are simultaneous constraints.
For a selected head, specialization removes that head and inserts its payload columns.
Wildcard rows specialize to wildcard payloads.
The default matrix keeps wildcard rows and removes their first column.
Data heads have one payload, product heads have their component arity, unit has none,
and named wrappers and existential values each have one dynamic field.
Typed view observations remain opaque unless their nested pattern is already a wildcard.
The uncovered-row recursion has these base cases:

```text
no columns: one uncovered empty row iff the matrix has no rows
no rows and no known head space: an all-wildcard uncovered row
no head space: recurse into the default matrix
finite head space: recurse for every head, then rebuild its uncovered patterns
```

Keeping columns together preserves correlations: rows `(+True(_), _)`
and `(_, +False(_))` leave `(+False(_), +True(_))` uncovered.
Product matrices retain their typed component vectors and explicit nesting.
A known empty data space has no inhabitants.
Generated copattern argument matches use the same procedure; separate codata validation checks missing
and duplicate typed destructors.

Diagnostics retain at most eight uncovered witnesses and probe for a ninth to mark truncation.
That bounds reported evidence, not the coverage decision.
Normalization and local errors prevent malformed rows from reaching this pass.
[Coverage tests](../../lang/tests/tests/coverage.rs) pair accepted matrices with correlated gaps,
empty types, and nested observation failures.
[Remaining pattern decisions](../proposals/exhaustiveness.md) cover usefulness, literal matching extensions,
and refutable conjunctions.

### Static Elimination

[Static elaboration](../../lang/statics/src/elaborate/static_values) uses a lexical evaluator
whose values can contain static closures, packed value structure, and shared references to runtime data.
It reduces type/value applications, known constructors, projections, openings of packed values,
value matches, and integer value operations.
For example, applying `val x => (x, x)` to a runtime variable produces a shared residual binding and product;
the runtime variable need not become a compile-time constant.
Computation thunks remain suspended, and general computation execution supplies no static evidence.

The frontend materializes each integer value intrinsic as a curried value abstraction around a typed leaf operation.
Value-match checking infers the result sort from its arms, or from the expected classifier for an empty match;
value and computation matches share coverage validation.
Residualization evaluates the selected value arm in a cloned lexical environment and folds integer leaves to literals.
Application-site provenance keeps failures in generated primitive bodies attached to source calls.
The interpreter and compiled backends have no runtime case for these static operations.

`StaticShape` supports witness inspection during dependent checking, including packed value and product structure.
It exposes only caller-visible witnesses and returns opaque evidence when reduction cannot establish them.
`StaticElaboration` records the original source root and an optional residual root:
an unapplied static library export can be checked without having a runtime representation of its own.
Reification creates fresh typed residual nodes, preserving runtime sharing and effect order.
Representability checks cover surviving values, thunk bodies, and computation classifiers after instantiation.
Export selection defers thunk-body residualization until the selected value is reified.
This permits private static helpers and unselected holes
without relaxing representability inside surviving computations.

The source elimination requirements and reducer resource limits are specified
in [L10](language.md#10-static-elimination).
A residualization failure reports `tyck.static-elimination` at its source site;
exhausted witness inspection supplies no evidence.
Both linking and SPS lowering select the stored residual root.
[ExecutionReadiness](../../lang/statics/src/validate) rejects reachable executable holes before effects;
typed holes may still be inspected during ordinary checking.
[Static-elimination regressions](../../lang/tests/tests/static_elimination.rs) distinguish erased library structure,
runtime payloads, shared runtime data, and failed evidence recovery.

### Residual Runtime Traversal

Execution readiness asks which remaining values and computations can reach execution.
The [runtime graph view](../../lang/statics/src/traverse.rs) starts from the shared residual root selected
by `StaticsArena::execution_value` or `execution_compu`, then follows runtime children in the typed arena.
It excludes classifiers, static witnesses, and pattern annotations.
A checked foreign implementation is an opaque leaf whose authored hole payload is supplied externally.
Static source nodes remain available to tooling.

`RuntimeGraph::nodes` yields borrowed nodes and visits each runtime identity once per invocation.
The iterator owns its pending stack and visited set, making sharing and cycle termination explicit
without retaining a cache across arena changes.
Clients can combine independent observations over this same stream.
`ExecutionReadiness` collects every distinct reachable value or computation hole into `ExecutableHoles`;
execution, library export checking, and the REPL reject the complete collection before lowering or running.
[Runtime traversal tests](../../lang/statics/tests/runtime_traversal.rs) pair shared reachable holes
with the accepted residual root that erases them; foreign-interface tests cover supplied implementations.

### Typed-Arena Lint

[LintChecker](../../lang/statics/src/validate/lint.rs) is an optional verifier over a complete arena.
It independently inspects the artifact, but shared derivation rules establish consistency rather
than a separate soundness proof: a faulty rule can reproduce the same wrong result.
Its practical boundary is corruption introduced by mutation, normalization, retries,
or generated syntax, demonstrated with seeded defects.
It checks fill closure, annotation presence and sorts, agreement of surface-keyed and node-keyed views,
and existence of referenced nodes and definitions.
Kind comparisons resolve normalized structure; raw kind-ID equality is insufficient after reconciliation.
Paired value and computation annotations likewise compare normalized type structure,
including the payloads of copied data/codata arms; different arena IDs alone do not establish disagreement.
Bound type witnesses compare by alpha-equivalence (consistent renaming of a binder and its uses),
with matching kinds and named-pattern structure.
Free witnesses retain their distinct identities.
Structural-comparison memoization includes the binder correspondence, so agreement
under a universal cannot equate unrelated witnesses outside it.
The well-formedness sweep includes orphaned allocations left by retries.
Abstract-witness kinds can come from their annotation, a denoting type node, or an enclosing binder;
requiring an `annotations_abst` row for every witness would reject legitimate artifacts.

[Re-derivation](../../lang/statics/src/validate/rederive.rs) checks type-former kinds across the whole arena.
Supported term-constructor shapes and witness binding are checked from source, residual,
and definition roots, using the documented ambient-witness policy.
Roots include recorded aliases, inlinable and type definitions, data/codata arms, and seals.
Supported introduction shapes cover thunks, returns, named values, units, and literals;
operand-dependent checks exclude abstract identities and applications other than `Thk` and `Ret`.
Shared nodes can have different use-site instantiations,
so the verifier cannot soundly compare every recorded parent/child annotation
or reconstruct every lexical reference context from this artifact.
Arena-wide annotation integrity includes abandoned allocations,
while structural scope reconstruction applies only to reachable roots.
A shared node can carry a generic or labeled annotation at one recording site and an instantiated or plain annotation
at another; the artifact lacks the per-use evidence needed for unrestricted parent/child re-derivation.
Product checks and definition-reference scope across imports and aliases remain deferred;
reference existence is still checked.

A reachable abstract witness must be structurally bound or ambient.
The ambient set includes seals, existential skolems, definition-denoted identities,
and named witnesses: recursive groups allocate identities together,
and packed value elaboration can distribute bindings without one structural encloser.
This policy detects unbound anonymous witnesses with existing table entries,
but does not reconstruct every source non-escape proof.
The visited-node cache checks a shared node under the first encountered scope only.
Scope-sensitive traversal, finer export provenance, and use-site derivation evidence remain
in the [lint proposal](../proposals/tyck-lint.md).

`CommandCompiler::with_lint_types`, exposed as `--lint-types`, runs the verifier
after a successful check and outside query memoization.
Findings are typed internal errors and abort the gated command; they are not source diagnostics.
Its hole policy also detects non-foreign term placeholders, so this verifier has a stricter completion expectation
than ordinary hole inspection.
[Mutation tests](../../lang/tests/tests/tyck_lint.rs) must show that a seeded corrupt fact is detected,
paired with clean cases that prevent false positives.

## C7. Linking and the Reference Interpreter

[Linking](../../lang/dynamics/src/link.rs) selects residual syntax, erases types
and witnesses, and constructs `DynamicsProgram`.
Builtin root linking materializes the host-provided packed value from its validated typed signature;
foreign declarations remain checked call plans until their runtime loader is used.
Named routes become structural tuple access, and aliases preserve a shared bindee.

[Runtime syntax](../../lang/dynamics/src/syntax.rs) separates dynamic terms from semantic values,
environments, suspended computations, and continuation state.
The [stepper](../../lang/dynamics/src/eval.rs) evaluates values, forces thunks,
consumes arguments and destructor observations, matches data, and resumes return continuations.
A thunk captures its lexical environment; `fix` supplies explicit computation recursion.
No backend-specific demand or frame optimization changes this reference evaluation path.

The interpreter uses Rust representations for values and host resources rather than the native tagged heap.
Its allocation behavior therefore does not predict native or Wasm costs.
The runtime borrows its input, output, error, and argument interfaces from the caller;
CLI runs use process streams, while tests and the REPL supply captured streams.
`ProgKont` distinguishes continuation outcomes, exit status, dry execution, and runtime errors.
An executable failure is reported at this boundary rather than silently retried on another target.

The [dynamics guide](../../lang/dynamics/src/README.md) maps runtime modules.
[C14](#c14-builtin-contracts-primitive-operations-and-foreign-calls) owns shared host roles and call plans;
[L6](language.md#6-computations-and-control) owns source dynamics.
Source regressions and runtime parity tests use the interpreter as one observable execution surface.

## C8. High SPS Lowering, Normalization, and Demand

[High lowering](../../lang/stackir/src/high/lower.rs) is indexed by the stack consuming a residual computation.
It builds complete user and Builtin structures into `BranchJoinProgram`.
High SPS retains lexical values, closures, continuations, arguments, and explicit ambient stacks.
Stack lets guard value-coproduct matches so every branch shares one supplied continuation stack.
The [high verifier](../../lang/stackir/src/high/check.rs) checks closed roots, lexical ownership,
and this branch-join shape.

The [pass composition contract](#built-in-plans-and-phase-boundaries) owns the configurable stage between high-SPS
checks and closure conversion, including its default normalization and built-in plans.
The [selection and inspection guide](#selecting-and-inspecting-passes) gives the corresponding CLI commands.
These optimizations are optional consequences of known runtime structure;
they do not relax L10's source elimination boundary.
The normalizer preserves definition identities while allocating fresh syntax for the surviving lexical tree.

### Residual Lowering Folder

Static composition can produce thousands of nested bindings from a small authored program.
The [lowering folder](../../lang/stackir/src/high/lower/fold.rs) uses the shared
[resumable folder driver](#resumable-folder-execution) for patterns, values, computations,
and ordered match decisions together; thunk bodies and branch tails resume through the same driver.
`RootLowerer::run_with_driver::<D>` selects continuation storage for this folder
and Builtin packed value materialization.
Production lowering selects `Explicit`, so residual reconstruction depth does not consume the Rust call stack.
`Lowerer` retains allocation, provenance, product layout, protocol extraction, and diagnostic state.

Domain frames retain the next reconstruction operation, while typed result stacks hold completed patterns,
value-binding plans, and computations.
Product cursors and branch iterators request one child at a time.
This retains structural order without constructing a separate list of pending child calls.
Tail calls cover erased wrappers and intermediate transitions that need no additional unfinished parent.

Computation visits inherit the high SPS stack that consumes their result.
Reconstruction preserves the semantic child schedule: application arguments precede their function body;
a `do` continuation is lowered before its bindee; value-binding plans wrap their completed continuation
in evaluation order.
Closures and coproduct branches receive fresh ambient-stack nodes.
Definitions retain their identity, and generated patterns and terms retain their typed source sites
and applicable protocols.
Erased witnesses and type applications produce no runtime nodes.

Input sharing always expands into fresh output syntax for each occurrence.
Memoizing lowered nodes by typed ID would violate lexical ownership and ignore inherited continuation stacks.
Ordered pattern plans may share flat plan IDs, including fallback decisions,
but each execution of a plan constructs its own syntax.
Constructor fallthrough binds the remaining rows once as a closure and resumes it
from rejected payloads or unmatched tags.
Flat plan storage also avoids recursive cloning and destruction of long decision chains.

Residual static-value failures accumulate in occurrence order; any failure prevents publication of a program.
Successful reconstruction validates lexical ownership and branch joins before returning `BranchJoinProgram`.
[Depth regressions](../../lang/stackir/src/high/lower/tests.rs) construct and drop residual fixtures
on a 512 KiB worker stack, covering mixed computation and thunk nesting, value lets,
structural aliases, literal fallthrough, and Builtin products.
Driver comparisons cover mixed reconstruction and ordered literal fallthrough,
including generated syntax, raw allocation slots, arena counts, and source provenance.
Separate assertions cover fresh ownership and protocols; independent rejected values accumulate under both drivers.
This guarantee concerns residual reconstruction; classifier protocol extraction, normalization analyses,
and subsequent conversion have their own traversal contracts.

### High SPS Analysis Traversal

Lexical ownership, branch-join placement, and free variables all depend on the same high SPS structure.
The [shared traversal](../../lang/stackir/src/high/traverse.rs) owns its exhaustive child enumeration.
It exposes borrowed pattern, value, stack, and computation nodes; definitions remain leaves.
An entry event identifies the immediate incoming edge and whether the occurrence is first, shared, or cyclic.
A stack-let tail carries its owning join ID so placement checks do not need their own recursive descent.

Every incoming edge is observed, including repeated ownership.
Children and exit events are scheduled once per node; repeated edges do not expand the same subtree again,
and active back edges terminate traversal of that edge.
`BranchJoinValidator` reports repeated owners and misplaced joins without stopping independent checks.
`BranchJoinProgram` is constructed only when its complete `BranchJoinErrors` collection is empty.
This preserves lexical-tree ownership even though the structural driver can safely inspect a malformed graph.

`Variables` computes bound and free variable summaries at exit, reading completed child facts.
Pattern binders remove names only from their lexical bodies; recursive binders,
continuations, and match arms retain their existing scope rules.
A cyclic graph has no free-variable result. The public `Vars` and `FreeVars` operations use this analyzer,
and closure conversion computes one set of facts for its immutable input, reusing them for every capture list.
Those transient facts are released with conversion; keeping summaries trades temporary memory
for avoiding repeated walks through nested closure bodies.

`Together` delivers the same events to independent analyzers.
High SPS verification composes ownership and variable analysis in one traversal
and retains both invariant failures when the graph is acyclic.
The normalizer still owns forward producer propagation and backward consumer demands.
[Traversal regressions](../../lang/stackir/src/high/traverse/tests.rs) compare separate and composed results,
count entries and exits on shared syntax, and cover binders and cycles with independent ownership failures.

### Normalization Reconstruction

After residual lowering, the unoptimized lexical tree can still contain long binding and closure chains.
The [normalization folder](../../lang/stackir/src/high/normalize/fold.rs) reconstructs values, computations,
and stacks through the shared [resumable folder driver](#resumable-folder-execution).
`Normalizer::run_with_driver::<D>` selects continuation storage for this reconstruction, pattern copying,
and [pattern decisions](#pattern-decisions-and-validation); production normalization selects `Explicit`.
Frames retain unfinished consumers and their producer environments,
while typed result stacks retain completed syntax and its consumer demands.
Product and branch cursors request one child at a time; split-binding iterators resume their components
in the order required by demand propagation.
Tail calls transfer reductions to surviving bodies without retaining the eliminated wrapper.
Pattern-fact distribution uses an explicit stack; discardability and movable-stack checks iterate
through their relevant children.

The folder preserves the [consumer-demand schedule](#consumer-demands).
A binding installs producer facts before visiting its tail, then resumes with the tail's demands
to discard, split, or rebuild the producer.
Split components consume those demands from last to first, so reconstructed bindings execute in their original order.
Unknown branches retain separate scopes and contribute completed demands before their shared scrutinee is rebuilt.
Primitive continuations follow the same schedule, retaining potentially trapping operations even without a value demand.
Suspended bodies return capture demands without executing their code.

Delayed ambient stacks are stored in a flat table whose IDs refer to both the stack node and its original scope.
This preserves substitutions across nested closure and argument reductions while allowing long substitution chains
to be resolved and destroyed without recursive Rust calls.
The existing environment table and occurrence counts retain their distinct roles
in forwarding and exclusive body movement.
Reconstruction preserves definition identities, source sites, applicable protocol evidence,
and the lexical ownership and branch-join validation boundary.

The bounded normalization behavior fixtures run through both drivers and compare generated syntax,
raw allocation slots, arena counts, source provenance, and protocol evidence.
They cover sharing, branch demands, primitive folding, and preservation of potentially trapping operations.
Additional comparisons cover empty and wide product, match, and comatch child sequences.
[Depth tests](../../lang/stackir/src/high/normalize/depth_tests.rs) use a 512 KiB stack for discarded
and retained bindings, suspended bodies, ambient substitutions, argument stacks, alias parameters, and unknown branches.
The [CLI regression](../../cli/tests/source_build.rs) builds the factorial source through both Wasm backends
without a compiler worker or Cargo's `RUST_MIN_STACK` environment setting.
These tests cover reconstruction depth; recursively structured semantic facts, demands, and protocol evidence,
as well as later compiler passes, remain separate concerns.

### Local Reductions

The [normalizer](../../lang/stackir/src/high/normalize.rs) records lexical facts for aliases,
literals, products, constructors, and suspended code.
Its principal reductions are:

```text
force (closure • => M) S          ==> M[S/•]
let arg(p) :: • = arg(v) :: S in M ==> let p = v in M[S/•]
return v to (kont p => M)         ==> let p = v in M
kont x => return x to •          ==> •
closure • => force f •           ==> f
```

Forwarding uses a variable `f` or a plain continuation binder.
Known destructor tags select their comatch arm; known constructors select their match arm and bind the payload.
Matching product introductions and patterns split into bindings in evaluation order.
A selected value branch can drop its join guard; an unknown branch retains the shared stack.

Substitution preserves the producer's lexical environment and the captured ambient stack.
It stops at stack binders, including closures, recursion, argument consumers, comatch arms, and branch joins.
Moving a remaining stack requires its argument values to be discardable;
tags recurse through the stack, and continuation bodies stay suspended.
A trapping argument retains its evaluation boundary.
The popped head argument is bound before the consumer executes.

### Sharing and Discardability

A directly forced closure literal reduces. A variable-bound general closure can move into its force only
with one syntactic occurrence; counts conservatively include dead code.
Shared closures stay bound, and aliases do not establish exclusive ownership.
Recursion is never unfolded. Value aliases forward without copying compound values;
nontrivial literals substitute only through singly used bindings, while trivial values can forward freely.
A returned value used repeatedly keeps one binding.

Discardability determines whether an unused producer may disappear.
Variables, literals, trivial values, and suspended closures are discardable;
products and constructors inherit it from their contents.
Arithmetic is discardable only when total and its operands are discardable.
Integer division, remainder, and holes retain possible traps unless literal folding proves successful evaluation.
Executed external calls and `Fix` remain even if their result is unused.
A dead suspended thunk can disappear without inspecting its recursive body.
These conditions preserve the order and multiplicity of effects while local reductions expose further consumers.

### Consumer Demands

[Demand analysis](../../lang/stackir/src/high/demand.rs) is the backward component of the same traversal:

| Demand | Observation |
| --- | --- |
| `Absent` | No surviving consumer needs the value. |
| `Fields` | Consumers need specified physical product positions and nested demands. |
| `Used` | An unknown consumer requires the complete value. |

Join unions field positions recursively, and `Used` absorbs other demands.
An empty `Fields` map still observes product shape and differs from `Absent`.
At `let p = V in M`, producer facts first normalize `M`; its free-definition demands are translated
through `p` into a demand on `V`.
The rebuilt producer contributes its own free demands, and demands for bound definitions leave the map.
Independent branches join before the producer is rebuilt.
There is no global demand fixed point or recursive-call specialization.

A known call exposes argument and return bindings before demands are read,
allowing a consumer to prune a passed module value.
Unknown calls observe arguments whole.
Escaping thunk bodies contribute demands on their captures while remaining suspended.
A surviving constructor match observes its scrutinee whole; known selection removes demands from discarded arms.
Forcing a thunk observes that thunk, not a projection of its eventual result.

Logical suffix patterns translate into physical product positions by shifting their nested demands.
A rebuilt suffix spread retains the required product arity even when its individual fields are absent.
Alias members join demands on the same scrutinee; a whole-value use
through one alias defeats selective pruning by another.
Eliminating an unpack can also eliminate its shape demand.
An unobserved field becomes trivial only when its producer is discardable.

Source value and parameter classifiers also contribute the [partial protocol evidence](#partial-source-protocols)
retained across rebuilding.
Discarded producers lose their evidence; whole retained values and parameters carry it to closure conversion.
Codata tag producers and consumers use the canonical observation numbering defined at that same boundary.

### Primitive Calls

A known primitive thunk initially has the form `closure • => extern f •`.
Recognizing it through aliases, projections, or constructor payloads exposes the external call even
at several call sites: only the operation identity is copied, with each site's original stack.
Known integer or float arithmetic with two visible arguments becomes `Primitive { operation, operands }`:

```text
extern add (arg(x) :: arg(y) :: (kont z => M)) ==> let z = PrimitiveAdd(x, y) in M
```

The argument stack's construction order evaluates the second operand before the first.
The remaining stack must satisfy the movement conditions above.
The result stays shared, and exposing its continuation as a binding avoids allocating a continuation record.
Literal folding obeys [L13's numeric rules](language.md#13-primitive-values-and-capabilities).
A zero divisor remains a runtime operation at its original position, even if its result is unused.

When optional normalization is disabled, retained arithmetic calls execute through the ordinary host-call ABI.
The native runtime and WebAssembly host implement those calls with the same numeric rules
and spare-box contracts as their corresponding primitive instructions.

Escaping arithmetic retains its thunk interface, with an inline primitive in its body.
An unknown callee remains indirect.
Address calculation and scalar memory accesses have the intrinsic bodies described below;
other known Builtin operations remain external calls.
The typed primitive survives SPSLow;
[scalar region lowering](#scalar-value-boundaries) then makes representation conversions explicit for ZASM
and direct Wasm instruction selection.

### Address Calculations

Byte displacement is a pure calculation on an unmanaged address.
Builtin lowering materializes `memory_offset` as a closed function returning `AddrOffset { base, displacement }`,
even with optional normalization disabled.
Compiler provenance identifies this fixed builtin body, so normalization can expose it
through aliases and field projections at several call sites.
General source closures retain their existing sharing restrictions.
An escaping or unknown function keeps the ordinary thunk calling convention.

`AddrOffset` takes an `Addr` and a signed `Int` byte displacement and produces an `Addr`.
The low-SPS protocol checker distinguishes address evidence from numeric evidence and rejects known operand mismatches.
Partial protocol compatibility does not establish allocation validity or access permission.
The calculation wraps at the execution profile's address width, without dereferencing,
checking a range, or asserting an in-bounds pointer.
It preserves the base pointer's provenance in the interpreter.
Normalization removes a known zero displacement only when evaluating that displacement is total;
trapping operand evaluation remains at its original position.

Closure conversion preserves the operation.
Its ZASM instruction consumes the address and tagged displacement and produces one address word.
AMD64 decodes the signed `Int` and adds it directly to the address bits;
both Wasm backends use wrapping 64-bit arithmetic on their virtual addresses.
The operation itself allocates nothing and makes no host call.
Its operands evaluate displacement first, then base, matching argument-stack construction.
This does not remove ordinary call or continuation costs at unknown boundaries.

The [address-offset regressions](../../cli/tests/passes.rs) check zero-offset elimination, signed offsets,
wrapping without invalid dereferences, and execution with normalization enabled and disabled in native code
and both Wasm backends.

### Ordered Scalar Memory Accesses

Builtin lowering materializes integer and float `load_le`/`store_le` and unmanaged address-slot operations
as ordered `MemoryStep` computations, including when optional normalization is disabled.
A load binds one result before its successor; a store has only a successor.
The shared [access domain](../../lang/syntax/src/memory.rs) identifies the scalar role and load/store direction.
It determines the exact carrier width, numeric little-endian or native address interpretation, and access alignment one.
Stronger alignment, volatile accesses, and atomics require additional contracts.
The low-SPS checker validates known address, result, and stored-value protocols.

High normalization exposes the fixed builtin body through known aliases and field projections.
It preserves every access, including a load whose result is unused, and sequences the successor after it.
Each expanded load gets a fresh result binding. Reusing a builtin wrapper must preserve earlier observations
that remain live across later stores and loads of the same address.
Memory effects do not enter pure value commoning or arithmetic evaluation.
An unknown callback remains an ordinary thunk invocation after the access;
primitive recognition alone asserts no callback lifetime, uniqueness, or contification guarantee.

Closure conversion and ZASM preserve the ordered operation.
AMD64 emits one unaligned, exact-width load or store, with scalar decoding and encoding at the ordinary value boundary.
`Int` and `UInt` use eight-byte carriers but reject out-of-domain bits before publishing a loaded result.
Signed narrow loads extend their sign, and float loads/stores preserve IEEE bits, including NaN payloads.
Unmanaged address slots retain address semantics rather than becoming scalar integer conversions.
The caller remains responsible for allocation validity, initialization, and access permission.

Wide loads allocate an opaque scalar box when producing an ordinary value.
Their raw bits remain outside the GC root range across that allocation; existing live values remain roots.
Stores decode their ordinary input before writing.
[Raw memory kernels](#raw-memory-kernels) remove these conversions when a bounded local chain closes at a store.

Both Wasm backends call `raw_*` memory imports with virtual 64-bit addresses and raw bits:
loads return one `i64`, and stores return nothing.
No source callback or spare box crosses these imports.
Generated code performs the ordinary value conversions and carrier validation.
The Node host retains its virtual-memory lookup cost; these operations do not address managed Wasm linear memory.
Interpreter adapters implement the same carrier and effect contract with their own memory representation.

The [scalar-memory regressions](../../cli/tests/passes.rs) cover unaligned integer extremes,
little-endian bytes and adjacent sentinels, invalid carriers before callbacks and later effects,
float bit preservation through unknown callbacks, and live native roots during collection.
Both normalization modes and all three compiled targets are exercised.

### Pattern Decisions and Validation

A known scrutinee permits branch selection only after every earlier arm has been ruled out.
The [decision folder](../../lang/stackir/src/high/normalize/decision.rs) compares a pattern
with borrowed producer facts and returns true, false, or uncertainty.
Alias members and product fields are considered in source order.
The first false or uncertain child ends that decision; a later mismatch does not override earlier uncertainty.
Unknown product shape remains uncertain, while distinct known constructor tags decide false.

The folder uses the normalizer's selected execution driver.
Requests retain pattern IDs and borrowed fact views; product suffixes are slices of the existing physical fields.
They create no temporary product fact or reference-counted field copies.
Constructor payloads transfer directly to the next decision; alias and product cursors retain the next position.
Deciding a pattern creates no syntax or binder identities.

Bounded comparisons cover accepted, rejected, and uncertain decisions, suffix layouts,
empty and wide patterns, and early termination before later children are inspected.
An alias chain of 16,384 levels executes and drops on a 512 KiB stack.
A separate 8,192-level product comparison borrows facts whose fixture retains and releases each level explicitly.
That fixture establishes decision depth, not depth-independent destruction of production `KnownValue` trees.
Owned fact construction
and demand operations remain [separate work](../proposals/traversals.md#normalization-fact-ownership).

Integer literal match plans lower to the raw `BuiltinValueRole::Integer(t, Eq)` branch
with success and failure continuations.
There is no structural literal-pattern node in SPS.
Structural aliases survive high and low SPS; assembly saves and reloads their bindee,
while direct SPS Wasm uses a local.
Resolved field routes already consist of ordinary structural patterns and erased witness evidence.

Normalizer and pipeline tests pair reductions with shared closures, traps, unknown branches, and suspended recursion.
[Demand tests](../../lang/tests/tests/demand.rs), [pattern fixtures](../../lang/tests/cases/literal-pattern),
and [core fixtures](../../lang/tests/tests/core.rs) exercise field pruning and compiled decisions.
A new reduction needs both a reducible example and a case where sharing, stack movement, or effects prevent it.

## C9. Closure Conversion and First-Order SPSLow

[SpsLowConverter](../../lang/stackir/src/low/convert.rs) consumes lexical high SPS and creates fresh low syntax.
Free-variable analysis determines ordered captures; renamed capture bindings close each generated block.
A closure becomes a record with an explicit environment and code pointer.
Its entry unpacks the captured environment before consuming ordinary arguments.
A continuation record retains its code and residual stack, including the bindings needed when it resumes.
Force and return open the corresponding closure or continuation record and jump to its code.

[Low syntax](../../lang/stackir/src/low/syntax.rs) separates `Block`, `Jump`,
`ClosurePackage`, `ContinuationPackage`, `OpenClosure`, and `OpenContinuation`.
Products, primitives, patterns, and external-call forms share their structural vocabulary with high SPS.
Source captures remain value identities; code labels and lexical syntax nodes belong to the new low arena.
Administrative definition allocation continues from the consumed high program as described in C2.

`SpsLowProgram` has one root and first-order closed blocks.
Its [verifier](../../lang/stackir/src/low/check.rs) checks reference closure,
lexical node ownership, retained branch joins, and the word entry contracts below.
A single-occurrence syntax tree means runtime sharing is explicit in variables;
it is not a proof that a continuation is dynamically used once.
Native preparation must establish the stronger lifetime facts in C11.
Structured Wasm can consume this representation directly because block boundaries and residual stacks are explicit.

### Closure Conversion Folders

[Closure conversion](../../lang/stackir/src/low/convert/fold.rs) uses the shared
[resumable execution interface](#resumable-folder-execution) for mutually dependent value,
stack, and computation translation.
`SpsLowConverter::run_with_driver::<D>` selects execution statically; the compiler pass selects `Explicit`.
Requests carry source IDs and indices into the conversion's renaming environments.
Frames retain ordered captures, translated binders, and branch positions
while typed result vectors hold completed children until their parent can be allocated.
Capture bindings, recursive labels, block entries, and record metadata retain their established allocation order.
Each source occurrence creates fresh output syntax with its corresponding origins and protocols.

[Pattern translation](../../lang/stackir/src/low/convert/pattern.rs) is a separate folder using the same driver.
Each completed pattern returns both first-order syntax and its ordered source-to-fresh-binder assignments.
Constructor frames retain one payload; alias and product cursors accumulate children
and assignments in structural order.
Pattern syntax has no edge back to values, stacks, or computations, so this subwalk does not retain a recursive cycle
through the conversion folder.

Bounded regressions compare `Explicit` and `Recursive` syntax, binder order, allocation slots,
origins, layouts, entry protocols, and continuation capture metadata.
Pattern regressions also cover empty field vectors and rejected layouts that cannot publish their parent.
An 8,192-level alias fixture constructs, translates, and drops on a 512 KiB stack.
Direct conversion fixtures additionally reconstruct and drop 4,096 nested bindings
in a closure and a 16,384-frame argument stack on that stack size.
These fixtures isolate reconstruction from downstream validation.
Semantic protocol cloning and destruction, low verification, and later phases retain their own depth requirements;
explicit conversion does not establish an end-to-end depth guarantee.

### SPSLow Traversal and Analyzers

The [SPSLow traversal](../../lang/stackir/src/low/traverse.rs) describes executable children once.
Its resumable folder retains a borrowed node and the next child position.
Each child selection takes constant time, including wide products and branch vectors; frames do not copy those vectors.
`Traversal::run_with_driver::<D>` selects the shared execution driver, with `Explicit` as the default.

Every incoming edge produces an entry event marked first, shared, or cyclic.
Only the first occurrence expands children and receives an exit event.
Block entry patterns precede their body in stack-consumption order;
the tail of a stack let carries its immediate branch-join owner.
Continuation metadata and provenance refer to existing syntax and add no executable edges.
`Together` sends the same events to independent observers.

[Variable analysis](../../lang/stackir/src/low/variables.rs) computes bound patterns and free terms at exit.
Block labels and entry words bind only their body; record openings and local bindings likewise remove definitions
from their dependent body rather than from the record or bindee.
A cyclic traversal exposes no usable variable summaries.
The [structural validator](../../lang/stackir/src/low/check.rs) observes ownership
and branch-join edges alongside these summaries, then checks block captures,
continuation contexts, and root closure from the completed facts.
This avoids rescanning each block body and continuation context.
Malformed structure is rejected before dependent capture checks consume summaries.
The temporary summaries are dropped before entry-contract and protocol validation.
Validation still returns one error;
broader diagnostic recovery remains [unfinished work](../proposals/traversals.md#diagnostic-collection-and-recovery).

Driver comparisons cover event order, sharing, cycles, entry words, lexical binders,
empty and wide fields, and rejection without publishing an invalid program.
Direct fixtures analyze and drop 16,384 nested bindings on a 512 KiB stack,
including a block whose capture check reuses those summaries.
Entry-contract and protocol validation remain separate semantic traversals with their own depth requirements.
Summary storage is proportional to retained facts, which can exceed syntax size;
these tests establish traversal counts and depth behavior rather than memory or compilation-time gains.

### Word Entry Contracts

Closure conversion introduces words that have no corresponding source argument:
a closure's captured environment and a continuation's saved environment.
Making their roles explicit lets callers and code entries agree before assembly lowering chooses stack operations.
A returned source value also has an explicit entry role.
The remaining source arguments and effects still use the residual stack.

Each `Block` declares [EntryParameters](../../lang/stackir/src/low/entry.rs),
and each `Jump` supplies an `EntryArgument` before its residual stack:

| Entry kind | Parameters in consumption order | Word supplied by the jump | Residual stack at the jump |
| --- | --- | --- | --- |
| Closure | Environment | Environment from the closure record | Ordinary argument/effect stack |
| Continuation | Result, environment | Returned result | Saved environment followed by the caller's stack |

Every listed parameter occupies one ordinary target value word, including a product or buffer handle.
`EntryParameters::words` fixes their order for both ZASM lowering and direct SPS Wasm emission.
The block's patterns bind these parameters before its body executes;
ordinary `LetArg` nodes in the body consume subsequent user arguments.
`OpenContinuation` restores the residual stack, so returning supplies only the result word.
The explicit entry forms replace administrative `LetArg` prologues and `Arg` prefixes in SPSLow.
Their lowering preserves the existing physical word convention.

The [entry verifier](../../lang/stackir/src/low/contracts.rs) checks a code value's origin
and entry kind at each record construction and jump.
A block and its recursive self label carry the declared kind.
Known unit/product environments must agree with a direct block's outer environment arity;
unknown shapes remain subject to upstream source typing.
This is a partial shape check, not reconstruction of the erased source types or their field classifiers.

Opening a closure establishes a local association between its code and its whole environment.
An indirect jump or repackaging must preserve that association.
Opening a continuation similarly associates its code with the restored residual stack;
returning or repackaging must use that same residual stack.
Code from one opening cannot consume another opening's environment or continuation stack,
even if both records happen to have the same shape.
The verifier propagates evidence through whole-value aliases and projections of known complete products.
Destructuring an opaque environment does not establish permission to reconstruct an equivalent one.
Stack operations retain evidence when their known pushes and pops cancel; a remaining prefix or a pop
into the opaque restored stack loses the required agreement.

These checks assume source typing and closure conversion establish the shapes of dynamically obtained records.
The administrative checks are supplemented by the partial source protocol checks below.
Neither establishes continuation lifetime or reconstructs complete source typing.
Host and C external calls retain their own upstream signatures and transfer contracts.
Local representation policies must preserve the ordinary word transport at this boundary;
source storage alignment and padding alone cannot select a different call layout.

For native retained-frame lowering, `ContinuationEntry` additionally records the result pattern,
body, and ordered capture bindings.
The verifier compares that metadata with the explicit continuation entry and its record
before C11 replaces portable captures with retained slots.
There is no second executable entry prologue to infer or keep synchronized.

### Partial Source Protocols

The compiler retains a partial description of the source stack protocol so
that known call components can be checked after normalization.
This description follows [Ret and stack extent](language.md#ret-and-stack-extent):
an installed continuation hides its saved residual stack, and there is no end-of-frame constructor.
In particular, an argument prefix followed by `cont(A)` does not bound the stack extent or allocation lifetime.

[ValueProtocol and StackProtocol](../../lang/stackir/src/protocol.rs) are owned IR data extracted
from checked classifiers.
Values retain unit, primitive, product, and thunk structure when known.
Codata interfaces retain observation alternatives in an owned `ProtocolGraph` shared by the high and low arenas.
Disclosed seals and applied type functions can lead back to existing graph instances.
Unresolved first-order value and computation witnesses retain program-local parameter names and their kinds.
Universal binders retain their scope in the descriptor, while captured parameters can occur freely in local entries.
Unsupported higher-kinded applications, existential carriers, and other unsupported forms contribute unknowns.

| Stack descriptor | Interpretation |
| --- | --- |
| `?` | Unknown protocol; no statement about whether the stack is empty or how large it is |
| `aN` | A named computation parameter; repeated occurrences retain a relationship |
| `forall aN . S` | A source type binder around `S`, consuming no runtime stack component |
| `A :: S` | One argument classified by `A`, followed by protocol `S` |
| `cont(A)` | An installed continuation accepting `A`, with its saved stack hidden |
| `pN` | A program-local reference to a codata interface's observation alternatives |
| `.d#i :: S` | A producer has supplied observation `.d`, with runtime tag `i`, above remainder `S` |

Each codata instance receives a graph reference before its observations are translated.
Recursive occurrences with the same captured arguments reuse that reference,
including cycles through argument prefixes or thunk components.
This keeps a finite description even when executions accumulate an unbounded number of observations and arguments.
The graph records no physical stack extent.

The [source extractor](../../lang/stackir/src/protocol/source.rs) interprets disclosed type-function applications
against the frozen checked arena, without allocating substituted types or rerunning the checker.
A source expression carries lexical bindings for the witnesses that its supported structure uses freely.
Applying a checked type abstraction binds the argument to its witness;
a named binder projects its payload, while a plain binder keeps the whole argument.
Nested abstractions capture the outer arguments they use.
Known named projections, value products, thunk protocols, and computation arguments use these bindings as well.
For example, `Stream A R = codata .item : A -> Stream A R; .done : R end` retains an `Int` payload
and `cont(Int)` result when applied to `Int` and `Ret Int`.
Applying the same family to `Char` preserves a separate instance.

Instance keys contain the source codata identity and its captured source expressions and bindings.
They do not use partial protocol compatibility to equate arguments; distinct unresolved witnesses remain distinct keys.
Only an existing exact instance closes a recursive edge.
When a source codata is already being translated under different arguments,
a new application contributes unknown instead of starting another specialization.
For `Growing A` whose next observation requires `Growing (A * A)`,
the current observation retains its known payload and the changing tail remains opaque.
The same guard covers growth through returned thunks.
Repeated source nodes along unguarded reduction or value/argument paths also stop conservatively.
These guards impose no unfolding-depth limit, but can lose evidence for finite nested applications too.
General nonregular recursion remains outside this instantiation procedure.

Observation indices are the zero-based ranks of complete destructor names in lexicographic order within the interface.
High lowering obtains both pushed tags and case tags from the descriptor's canonical ordering.
Consequently, structurally equal codata interfaces agree on runtime indices even
when their source declarations list observations in different orders.
Both the name and index participate in low protocol checking.

High lowering records value and pattern protocols, recursive entry protocols, and the interface of each codata case.
Normalization copies facts for retained whole values and patterns into the fresh arena;
discarded values and pruned product fields do not retain stale whole-value facts.
Surviving cases keep their interface, and rebuilding shares the immutable graph while preserving its references.
Primitive results can recover their scalar protocol from the typed operation itself.
Closure conversion transfers these facts to the low arena and records an `EntryProtocol` for each block.
Closures retain their incoming source stack protocol, and continuation entries retain the accepted value protocol.
Recursive self labels inherit the same entry descriptor as their block.
These descriptions come from checked classifiers, not a count of leading `LetArg` nodes.

The [protocol verifier](../../lang/stackir/src/low/protocols.rs) checks known components before SPSLow is published.
It compares a closure's protocol with the supplied argument stack,
and a continuation's accepted value with the result delivered to it.
Entry kinds agree with administrative roles; known incoming parameter protocols agree
with their retained source pattern classifiers.
Bindings, aliases, product projections, and record openings propagate local evidence.
An opened thunk supplies its code's protocol; an opened continuation supplies the code's accepted value protocol
while its restored stack becomes opaque to this analysis.
The separate provenance check still associates that code with the exact restored stack.

For a supplied observation, the verifier selects that alternative from the expected codata descriptor
and checks its remainder.
A retained case checks its supplied stack against the declared interface, validates branch coverage and tag indices,
and checks each branch against the selected observation's protocol.
This branch evidence remains available even when the ambient stack became opaque at a continuation opening.
SPSLow publication validates graph and parameter references, including parameter kinds, before examining transfers.

The [agreement checker](../../lang/stackir/src/protocol/agreement.rs) compares the whole transfer
with a fresh constraint set.
Each side has its own parameter namespace, and each universal occurrence gets a fresh scope.
Within a scope, repeated occurrences of a parameter share constraints.
For example, `forall A . A -> Ret A` agrees with `Int :: cont(Int)` but rejects `Int :: cont(Char)`.
Two separately quantified thunk components can each instantiate the same source binder differently.
A computation parameter can relate a callback's required protocol to the stack supplied after it.
The SPS verifier skips leading universal binders when inspecting the next runtime stack component;
these binders never add a physical stack word.

Parameter constraints retain every known partial shape rather than selecting one representative.
For example, observations of `(?)`, `(Int)`, and `(Char)` for one value parameter must still reject:
the unknown field cannot discard the later concrete conflict.
Ordinary unknown occurrences remain independent gaps, including repeated visits to an unknown codata payload.
For a computation parameter, different supplied tags can select different alternatives of the same interface;
a known complete interface must admit each supplied observation with its proper index and remainder.

Comparison builds finite local graphs keyed by codata reference and relevant captured parameter bindings.
Already compared graph components close recursive comparisons, while every other observation remains checked.
Parameters bound outside a recursive interface stay related across its observations.
A universal binder introduced inside a codata interface is retained in the published descriptor,
but its parameter positions are treated as unknown during agreement.
Fresh instantiation at each observation needs a further comparison extension;
reusing one inferred argument for every visit would reject valid polymorphic observations.
Fixed components and recursive observation coverage remain checkable under this conservative treatment.
Changing captured bindings along a recursive comparison also leaves an opaque component.
There is no unfolding-depth limit or runtime iteration bound.

Unknown components are compatible with the existing word transport, and only known conflicts are rejected.
This relation is nontransitive; successful agreement cannot establish type equality or authorize a different ABI.
Abstract storage-carrier identity remains an upstream source typing property; agreement checks consistent known shapes,
without reconstructing the explicit type arguments erased before SPS or proving parametricity of a generic body.
Those remain upstream typing obligations. Layout/reference-map evidence is not reconstructed.
Host and C external transfers keep their existing signature checks.
No frame-size, frame-lifetime, or stack-scanning plan is derived from these descriptors.

`zydeco build --target zir` displays entries such as `closure[Int :: cont(Int)]`
and `continuation[Thk(Int :: cont(Int))]` alongside their administrative word parameters.
It also prints finite definitions such as:

```text
[protocol:p0] codata { .done#0: cont(Int); .item#1: Int :: p0; }
```

Parameter declarations print their kinds, for example `[parameter:a0] VType` and `[parameter:a1] CType`.
A polymorphic relay can retain an entry such
as `closure[forall a0 . forall a1 . Int :: a0 :: a0 :: Thk(a0 :: a0 :: a1) :: a1]`.
These names describe source relationships and convey no representation size or allocation policy.

[Protocol regressions](../../lang/tests/tests/stack_protocols.rs) check those surviving descriptions,
rejected argument/result and observation conflicts, graph integrity, and a source computation
that accumulates a runtime-dependent number of argument/tag frames before its installed continuation.
That program dynamically selects and returns its codata consumer, and also calls a returned arithmetic worker
through a recursive computation-polymorphic forwarder on all backends.
The [declaration-order regression](../../lib/tests/core/codata-order.zy) exercises equal structural interfaces
with reversed source ordering on all backends.
The [parameterized regression](../../lib/tests/core/parameterized-protocols.zy) instantiates one recursive family
with both `Int` and `Char`, including a dynamically selected and returned stream thunk.
Low mutation tests reject an incompatible payload after the first recursive observation.
The [growing-family regression](../../lib/tests/core/growing-protocols.zy) checks
that conservative extraction still permits successive observations with larger product types on all backends.
The [symbolic relay](../../lib/tests/core/symbolic-protocols.zy) calls one recursive worker
at different value and computation types on all backends.
Its low mutation test rejects conflicting arguments for one parameter.

## C10. ZASM, Stack Analysis, and Local Representation Choices

[Assembly lowering](../../lang/assembly/src/lower.rs) consumes SPSLow into a control-flow graph
with explicit operand and control stacks, environment variables, labels, and instructions.
[StackAnalyzer](../../lang/assembly/src/analyze.rs) assigns the stack/environment locations needed by that graph.
The portable result is an `AssemblyProgram` for the [ZASM interpreter](../../lang/assembly/src/interp.rs) or AM Wasm.
Native lowering selects a distinct frame-aware path before preparation.

### CPS Assembly Lowering

Assembly lowering uses continuation-passing style (CPS): a value or pattern receives a consumer describing what
to compile after its stack operations, under the resulting context.
Some handlers also need a child's entry ID before they can finish their own construction.
The [assembly folder](../../lang/assembly/src/lower/folder.rs) represents these two kinds of unfinished work separately.
A typed `Continuation` is a compilation consumer; a `Frame` resumes a caller after a child returns its entry ID.
This separation preserves instruction scheduling while allowing every syntax descent
to use the [shared folder driver](#resumable-folder-execution).

Consumers are defunctionalized: records for subsequent values, patterns, instructions, computation bodies,
terminators, sequences, and branch tables replace captured Rust functions.
Internal `ContId` links refer to a flat per-run vector, independent of the assembly allocator.
Applying a consumer takes its slot once and moves its payload into the next operation.
Links are affine, meaning they can be used at most once: a value hole emits `Abort`
and can abandon an entire consumer chain, including an unfinished field sequence.
Unused records drop with the run without recursively following their links.
`ContId` is neither `Copy` nor `Clone`, so applying a consumer also consumes its handle.
Consumption removes a vacant suffix from the vector, allowing subsequent consumers to reuse that storage.
Interior vacancies stay in place until consumers above them finish; outstanding IDs never shift.
Abandoned consumers remain occupied until the run ends, and the vector retains its allocated capacity.
This bounds storage across sequential completed steps without promising space proportional only
to live consumers for every schedule.
This assembly-local storage policy does not change the generic driver.

Instruction construction reserves a `ProgId` and queues a typed pending instruction.
Completion pops this queue in LIFO order, applies `ContextUpdate::{Keep, Bind, Clear}`,
and calls the instruction's consumer through `Step::Call` to obtain its successor ID.
It then publishes the instruction with the original context, before completing any new jobs queued by that consumer.
Obtaining an entry ID therefore does not imply that its body is published.
One driver run includes initial syntax lowering, root-entry marking, and draining the pending queue.
Tail transfers use `Step::TailCall`; blocks, native resume entries, branch arms,
and publication jobs retain explicit return frames only when they need a child's ID.
Consumers dispatch directly to the selected value, pattern, or computation rule.
Those syntax rules return their descendants as driver work; they do not invoke consumer application directly.
This acyclic call boundary avoids a redundant dispatch for the selected rule
while keeping syntax-depth-dependent execution on the driver.

Sequence cursors preserve the stack machine's ordering: value fields and primitive operands are pushed right to left,
patterns and alias members are consumed left to right, and a residual stack precedes its argument or tag.
Closure words come from the shared machine model, and block parameters follow `EntryParameters::words()`.
Branch entries and their symbols are reserved in source order, while deferred arm bodies may finish in reverse order.
Extern discovery therefore remains attached to visiting the computation that requests it.
An ordinary block reserves its recursive symbol before lowering entry parameters;
its return frame fills that symbol and records the label after receiving the entry ID.
Native resume lowering allocates capture bindings before its child call, then creates the resume symbol,
records frame-entry metadata, and emits `RetainFrame` followed by the code address.

`LoweringPipeline` selects `Explicit` by default.
`with_driver::<D>()` statically changes CPS execution for both portable lowering and `with_native_frames()`;
representation analysis, stack analysis, and checked native preparation keep their own phase boundaries.
The [lowering tests](../../lang/assembly/src/lower/tests.rs) compare both drivers across all representation policies,
recursive symbols, context resets, literals, primitives, external discovery, alias and branch order,
abandoned consumers, and rejected multiple irrefutable arms.
A saved trace from before defunctionalization additionally fixes allocation slots, publication order,
definition associations, contexts, successor links, and native entry metadata, erasing only per-run key spaces.
Pipeline comparisons include the finished native activation layouts, owners, slots, and liveness maps.

The [depth regression](../../lang/assembly/src/lower/tests/depth.rs) lowers and drops 16,384-level argument/tag stacks,
constructors, unboxed patterns, nested branch tables, and portable/native continuation entries on a 256 KiB stack.
It also abandons and destroys a deep consumer chain at a value hole.
The [storage regression](../../lang/assembly/src/lower/folder/tests.rs) checks
that 16,384 sequential completed consumers retain the same vector capacity as 32 steps,
while preserving the emitted terminator.
These direct fixtures isolate folder execution from semantic validation and stack analysis.
The guarantee covers lowering's control flow and flat continuation teardown; context cloning,
retained assembly contexts, and the separate validators and analyzers still determine other costs.
The [isolated lowering study](../evaluations/2026-09-14-assembly-lowering/README.md) records historical timing
and allocation measurements for the boxed implementation and both folder drivers.

### Product Layout and Local Unboxing

[ProductLayout](../../lang/assembly/src/syntax.rs) distinguishes logical arity from the physically stored fields.
Tuple tails and projections must respect that distinction; a suffix pointer refers into an existing payload.
Closure record layout is derived from the shared machine model rather
than a separately maintained field-order convention.
Changing packing must update patterns, closure opening, stack analysis,
and collector interior-pointer handling together.

[LocalUnboxing](../../lang/assembly/src/unbox.rs) runs over SPSLow
while producer/consumer relationships remain explicit.
It marks immediate product construction/elimination pairs, direct closure forcing, variable-bound products
whose uses are all suitable projections, and local closure bindings whose uses all open the closure.
Lowering then omits the corresponding pack/unpack pair or expands a variable into field slots.

The collector observes entry events from the [shared SPSLow traversal](#spslow-traversal-and-analyzers).
Its variable-use folder reuses that traversal's child cursor with a different semantic view:
closed blocks and binding patterns are boundaries, while immediate projection
and closure-opening children receive their respective use roles.
Other value uses are escapes, including captures carried by closure environments and continuation residuals.
This preserves the closed-block rule without duplicating structural child enumeration.
`LocalUnboxing::with_policy_and_driver::<D>` selects execution for both walks;
ordinary policy entry points select `Explicit`.
The use folder does not invoke the collector, so a candidate's subwalk introduces no recursive callback cycle.

Bounded tests compare both drivers across static and dynamic policies, accepted expansions,
escaping counterparts, and the order of calls to a stateful policy.
Small-stack fixtures collect through 16,384 bindings and classify a 16,384-level nested escaping value,
including destruction of the arena and results.
The latter fixture isolates classification from semantic validation.
Variable candidates still require their own use scans; driver selection does not claim to remove those scans.
Alias uses and escaping or unknown consumers retain the ordinary boxed representation.
Escape classification follows occurrences of the candidate variable through values and residual stacks;
an unrelated primitive, constructor, or closed block does not constitute an escape.
The explicit environment/result arguments of a jump are escaping uses
under the [word entry contract](#word-entry-contracts).
SPSLow's closed-block invariant places captures in explicit environments and continuation residuals.

### Policy Selection

[RepresentationPolicy](../../lang/assembly/src/representation.rs) separates a preference
from the analysis that justifies it.
The collector first establishes a compatible producer/consumer shape and the required local use evidence,
then asks the policy about an `UnboxingOpportunity`: its reason and field-word count.
Accepting every opportunity cannot waive an escape, width, or calling-contract restriction.
Rejecting a closure's expansion also prevents expansion of an environment transported inside that boxed closure.
Product fields retain their ordinary tagged-word representation.
[Scalar regions](#scalar-value-boundaries) separately justify local raw arithmetic without changing field layouts.

| Policy | Selected opportunities |
| --- | --- |
| `Boxed` | Keep residual product, closure, and scalar operation boxes after the selected high-SPS transformations. |
| `Direct` | Immediate product elimination and opening of a syntactic closure record. |
| `Local` (default) | `Direct`, plus a variable-bound product used only through compatible projections. |
| `Shared` (experimental) | `Local`, plus a variable-bound closure used only through closure openings. |

Each policy is a Rust type. `RepresentationStrategy` selects the same policies through a runtime enum.
`LoweringPipeline::with_representation` accepts a policy type or the enum and produces an immutable assembly program;
the policy is consumed during compilation and adds no runtime representation dispatch.
Custom Rust policies can restrict selection by reason or width without replacing the collector or the emitters.

`CommandCompiler::with_representation` and `BackendProgram::with_representation` provide per-compilation enum selection.
Changing a backend program's strategy invalidates its cached portable assembly.
Native frame preparation consumes the same policy before establishing its frame and root maps.
The CLI's `build --representation` applies to ZASM, AMD64 assembly/executables, and both Wasm backends.
SPS Wasm consumes its [scalar choice](#scalar-value-boundaries); product policies remain assembly-specific.
Zir rejects an explicit selection because it precedes physical representation lowering.
The old process-wide `ZYDECO_DISABLE_UNBOXING` switch has been removed; select `Boxed` explicitly instead.

The delivered analysis is local representation selection.
It does not implement the proposed three-way choice among unboxed fields, stack-allocated products,
and region-allocated products, or interprocedural escape propagation.
Those choices and their constraints remain in [escape and unboxing](../proposals/escape-unboxing.md).
Local analysis tests and core execution cases should verify both allocation removal and the uses that retain boxing.
The [policy experiment](../../cli/examples/representations.rs) compares residual allocation sites and field words,
including a Rust const-generic width limit; these are static code counts, not executed allocations or peak memory.
The [execution fixture](../../lib/tests/core/representation-policies.zy) exercises a locally opened recursive closure
with live captured values.
See [the contribution workflow](../../CONTRIBUTING.md#representation-experiments) for reproducible commands.

### Storage Evidence and Machine Calls

Source storage and compiler representation choices provide complementary evidence.
A logical `UInt8 * UInt32` can have an eight-byte natural encoding or a 64-byte aligned stored representation;
both still cross ordinary Zydeco calls through the established word convention.
Local unboxing removes cells where justified, without interpreting a source dictionary's padding or alignment.

| Description | Established evidence | Limit |
| --- | --- | --- |
| [Source `Representation A`](language.md#typed-pointers-and-slices) | One layout witness shared by typed pointers and CPS operations | No selected register class or argument width |
| [Source `Plan A`](language.md#static-layout-plans) | Validated byte placement and inspectable field offsets | No type identity for each placement, reference map, or calling convention |
| SPSLow `ProductLayout` | Logical arity and producer/consumer structure | No byte padding or scalar register classification |
| [Word entries](#word-entry-contracts) | Ordered administrative environment/result words and code/record provenance | No different component transport or complete source stack protocol |
| [Partial source protocols](#partial-source-protocols) | Known components, scoped parameters, and regular recursive codata | No nominal storage identity, explicit type-application evidence, or physical stack extent |
| [Native frame plans](#c11-native-preparation-activation-frames-and-amd64-emission) | Live tagged-word slots, entry roles, and suspension ownership | No layout mixing raw scalar bits with managed references |

[Stored source calls](language.md#stored-call-interfaces) share a carrier and its dictionary without changing transport.
Selecting another machine ABI requires the
[remaining call-boundary work](../proposals/escape-unboxing.md#remaining-machine-call-boundary)
to establish representation identity, agreement at both ends, target placement, and tracing together.

## C11. Native Preparation, Activation Frames, and AMD64 Emission

[NativeProgram::prepare](../../lang/assembly/src/frames.rs) validates the frame-aware ZASM result
before [AMD64 emission](../../lang/amd64/src/emit.rs).
The checked product contains activation ownership, entry contracts, initialized bindings,
continuation provenance, packed frame slots, and root/suspension maps.
Invalid preparation is a `FramePlanError`; emission cannot silently fall back to an unchecked environment layout.

`BackendProgram::emit_amd64` returns an `Amd64Artifact` containing assembly text
and the foreign imports and libraries collected from that same native program.
CLI and test builds link this artifact after one lowering pass.

| Entry role | Required transition |
| --- | --- |
| Local branch | Keep the current activation and its established bindings. |
| Closure entry | Establish an activation from captures and incoming arguments. |
| Return continuation | Restore the retained activation and initialize the result binding. |

`ContinuationEntry` provenance records the returned-value pattern, the body after the portable capture preamble,
and the source-to-capture binding relation.
Validation checks these against the actual record and preamble;
metadata does not introduce another executable occurrence.
Native lowering replaces capture construction and unpacking with `RetainFrame` and resumption aliases.
Ownership follows local and suspension/resumption edges; forward dataflow verifies initialized bindings independently
of ZASM context annotations.
Captured aliases resolve recursively to their original definitions.

Slot assignment combines ordinary liveness with preservation by pending continuations.
A later result may reuse a dead slot only when no pending continuation still needs its binding.
Nested continuations can retain different subsets of one activation;
root and interference information must include all pending uses.
A forward may-analysis unions pending captures at joins; a resumption consumes its own suspension
while preserving older ones.
A write interferes with every binding preserved across it, even when the written result is dead.
Deterministic greedy coloring assigns canonical definitions to slots; aliases inherit their source slot.
Packed size is a safe bound from the assigned slots, not an optimal-coloring or recursion theorem.
The same offsets are used for accesses, capture descriptors, and active root maps.

### Activation Lifetime

An activation is one dynamic invocation, potentially spanning local blocks and several resumptions;
a block label alone does not establish a new activation.
The environment stores the current bindings, while a capture environment stores the values needed
when suspended code begins or resumes.
Portable lowering copies continuation captures into a heap product and unpacks it on return.
Retained native frames preserve the same values in their existing slots, avoiding that capture copying.
Ordinary escaping closures still own heap capture environments.

With the control-stack top on the left, a portable call enters with `E_f :: argument :: L_k :: E_k :: S`,
where `E_k` is the continuation's capture product.
Native preparation replaces it with `E_f :: argument :: L_k :: token(F) :: S`;
the token retains the caller activation `F` and the entry descriptor identifies its resumption layout.
An independent allocation frontier tracks reserved environment storage as well as the active base.
Restoring only the base would not reclaim or preserve the correct extent.
The environment and control stacks remain separate, preserving the ordinary argument protocol.

The default retained environment stores frames in growable Rust-owned storage.
Closure entry may reserve and relocate that storage; generated code reloads its active base into `rbp`.
Saved references are logical offsets and tokens, never raw environment-slot pointers in managed values or closures.
Suspend and Resume preserve the model's nesting discipline.
Resuming consumes the most recent pending token, restores the owning frame and frontier,
and releases younger unretained storage.
A tail transfer reuses an unretained active frame or preserves it beneath its callee when a continuation still needs it.

The lifetime justification comes from the concrete lowered stack operations:
SPSLow values cannot contain a residual machine stack, checked continuation bodies cannot refer
to their own entry label, and native continuation opening consumes its token destructively.
Recursive closure invocation establishes another activation instead of reentering a suspended one.
Source `Ret` types and lexical node occurrence counts alone do not establish this property.
Escaping ordinary closures own heap captures with adequate lifetime.
A future machine-stack capture or multi-shot continuation operation must revisit preparation and storage together.

Arguments and closure captures must be staged before their source storage can be reused.
Every pending continuation preserves its own binding set;
resuming an inner continuation cannot destroy an older continuation's captures.
A return stages its result and target before restoring the owner and frontier.
Environment transitions do not collect managed values.

A jump is not itself evidence of a tail call: calls with explicit continuations also lower to jumps.
With a fixed pending continuation set, a tail chain's logical frame extent is bounded
by its retained storage plus the largest active frame, independently of chain length.
Cached capacity can retain earlier peaks.
Reclamation requires that active use has ended and no pending continuation can reach the frame.
No managed value, escaping closure, or foreign borrow may contain a raw pointer to an environment slot.
Knowing an entry layout establishes slot availability, not constant contents or a general static fact system.

At collection, root maps include active and all suspended live slots, together with control-stack and host roots.
Reserved or dead slots are not roots even when their words look pointer-shaped.
Changing frame storage must preserve this distinction:
retaining one small capture must not keep a dead large value alive.
The [environment capability](#environment-actions-and-roots) defines address stability and model transitions.

The emitter serializes model-defined actions and implements SysV register placement,
stack alignment, jumps, return prologues, and host bridges.
[Native packaging](../../cli/src/native.rs) bundles the runtime and shared model
and invokes the target toolchain using [CONTRIBUTING's build contract](../../CONTRIBUTING.md#compile-programs).
[Native-model tests](../../lang/tests/tests/native_model.rs) check preparation and transitions;
[native frame proposal](../proposals/native-frames.md) retains alternative storage and evaluation decisions.
Model regressions reject mismatched layouts, uninitialized reads, stale or out-of-order tokens,
and reservation overflow without corrupting callers.
Tail-chain tests check bounded usage, while GC tests keep a moved object alive solely through a suspension
and exclude dead slots in the same frame.

### Compilation-Unit Preparation and Artifacts

Independent emission fixes a contract between a producer and consumers that need not share its source.
The designs of compilation units, [FFI](language.md#14-foreign-interfaces),
and [package management](language.md#source-packages) are therefore coupled.
For compiled libraries, FFI determines which representations, control transfers, and ownership may cross the boundary;
unit selection checks that the chosen source implements a supported entry profile.
Package management names and resolves providers.
Selecting or distributing compiled providers also depends on their interfaces, target and profile compatibility,
and dependency closure.

Changes to any of these contracts must be reviewed against the other two,
with affected rules and interfaces updated in the same design change.
An FFI extension can change unit eligibility, import/export adaptation, runtime entry
and teardown, generated interfaces, and artifact compatibility.
Conversely, changes to unit boundaries or package resolution can require new foreign contracts.
For example, retained callbacks would require a unit lifetime and reentry protocol and compatibility information
for consumers of its distributed artifacts.

The checked [entry representations](../../lang/session/src/source/query.rs) describe a process program,
a collection of C exports, or a [native unit initializer](#native-unit-artifacts).
The [carrier implementation plan](../proposals/package-management.md#carrier-implementation) rebuilds their association
with a selected package as `CompilationUnit`.
The source roles and legality rules belong to [L14](language.md#compiled-libraries-and-c-exports).
[Library checking](../../lang/statics/src/check/library.rs) reuses the source judgment query, Builtin domain validator,
ordinary field resolver, directional foreign classifier, static elaborator, and readiness checker.
The checked product retains one residual root and scalar signature per export in a shared typed arena.
`RootLowerer::run_with_builtin` supplies the optional validated provider through the same high-SPS entry path used
for executables; subsequent SPS, representation, and native-frame passes are shared.

Each `CExportEntry` adds a SysV adapter around its prepared native root.
It saves the caller's callee-saved registers and raw arguments outside the traced stack range,
acquires the unit guard, creates the instance, and pushes a typed external return delimiter.
Arguments are converted in reverse stack order without integer allocation.
Scalar encoders truncate unspecified upper argument bits to the C carrier width and check the source payload range.
The residual root receives the ordinary argument stack and `Ret` completion, without an artificial `OS` exit.
On return, the adapter decodes the result before teardown and restores the C stack and preserved registers.

[LibraryBuilder](../../cli/src/library.rs) validates exports and dependencies before invoking native tools.
NASM emits one object per export and one shared guard object.
Private source labels remain local to each object.
A relocatable link combines them for `object`; `ar` creates `staticlib`.
`sharedlib` links them with matching runtime support built in release mode with PIC and aborting panics.
ELF external calls use PLT relocations; source code references are relative
and jump-table relocations follow the existing ELF/Mach-O emitter conventions.
An ELF version script or Mach-O export list hides every symbol except the declared C exports, including runtime support.
Shared runtime references bind within their own image.
Host resumption bridges belong to the model's runtime support once per image, rather than each compiled unit.

Raw objects and static archives ship a separate matching runtime archive.
A C link includes that support once after the unit code and its dependencies;
a Zydeco executable supplies it through its own runtime build.
Executable packaging first combines raw units with the program object,
ensuring their helper references are visible when the final linker scans the Rust runtime archives.
Model and runtime identities must agree across raw units sharing support.
A shared artifact contains private support and can be consumed across compiler versions compatible
with its public profile.
AMD64 target and manifest compatibility are checked before linking or interpreter execution.

The versioned manifest records package identity, target, entry profile, compiler/model/runtime fingerprints,
artifact and interface content hashes, selected export paths and C signatures, imports, and dependency manifests.
Logical library names replace package namespace separators with dots.
The generated header spells fixed-width C types and `void`;
the generated `.imports.zy` preserves the selected field structure using typed `ffi` declarations
and canonical type intrinsics.
It requires neither the producer's source nor its static-only interface.

`LinkedLibraries` resolves explicit manifests and verifies all transitive content, including build provenance.
Conflicting logical identities, colliding public symbols, incompatible signatures
or profiles, and stale artifacts fail preparation.
Signature deserialization reuses the constructor's arity bound.
Raw dependencies already included in a shared image remain verified provenance rather than duplicate link inputs;
shared dependencies reachable through them still contribute loader requirements.
Such embedded raw interfaces are private to the image; consumers needing them directly select their own manifest.
The Unix interpreter receives exact shared-library paths and still loads symbols lazily.
Native linking adds exact artifacts and loader paths; manifest-free C imports retain ordinary system-library resolution.

Publication uses a staging directory and an immutable content-addressed bundle.
After assembly, runtime compilation, linking, and interface generation succeed,
the builder publishes convenience artifact/header/binding symlinks and finally the public `*.library.json` symlink.
That manifest is the authoritative commit point and resolves to a complete immutable bundle;
dependencies record immutable manifest paths rather than mutable convenience names.
A failed compiler or native tool leaves an earlier publication usable.
Content hashes identify changes, not the authenticity of an untrusted producer.
Preserve bundle-relative dependency locations when distributing artifacts.

[Library regressions](../../cli/tests/library.rs) exercise independent selection, erased private helpers,
rejected signatures, C scalar transport, repeated calls, nested collection through raw and shared dependencies,
entry guards, source-free consumers, and failed-build preservation.
Manifest unit tests cover compatibility, corruption, hidden dependencies, and identity collisions without loading code.

#### Native Unit Artifacts

[Native Zydeco units](language.md#native-zydeco-units) use the same checking, static specialization,
readiness, SPS, and native-frame machinery as other boundaries.
`CheckedUnit` retains one `UnitInitializer`, its exported classifier, optional Builtin plan, and residual return root.
Preparing that return together preserves sharing between the exported values.
The closed structural classifier is retained before erasure
as [`UnitValueType` and `UnitStackType`](../../lang/syntax/src/unit.rs), independently of SPS partial protocols.
Unsupported source types reject through the [unit classifier](../../lang/statics/src/unit.rs).
Generated source reconstructs the complete supported classifier without producer-local arena identities.

A native initializer receives the caller's ordinary `Ret Exports` stack
and enters a fresh native activation within the active runtime.
It has no extra closure-environment argument.
`NativeEntry::Unit` publishes this root under its generated linker symbol; the root's stack parity is unknown,
so native helper calls use the existing dynamic alignment path where necessary.
An imported initializer lowers to an ordinary thunk whose body tail-jumps to that symbol
after consuming its own closure environment.
It forwards the continuation stack unchanged, including retained-frame tokens and residual data.
Calls through the returned exports use ordinary closure conversion and word entries.
The process supplies the runtime instance and its root-stack extent for all participating units.
Values retained across initialization or calls remain in traced source slots, stacks, or closure environments;
there is no untraced global export table or external handle registry.

[`UnitBuilder`](../../cli/src/unit.rs) emits one relocatable object with reachable private code,
a self-contained `.imports.zy`, and a versioned `*.unit.json` manifest.
The `word-initializer-v1` profile records the package, target, compiler executable hash,
runtime model identity, runtime source hash, initializer symbol, complete export type,
imported initializer signatures, artifact/interface hashes, and dependency manifests.
The symbol identity includes source dependencies and the prepared interface;
it is an implementation identity rather than a cross-version ABI promise.
No runtime archive is bundled with a native unit: the consuming executable supplies matching support once.

`--link-library` accepts both manifest kinds.
`LinkedUnits` validates native manifests and their transitive closure, including content hashes, profiles,
exact compatibility, symbol/type agreement, duplicate provider conflicts, and cycles.
Each unit may import only providers in its declared dependency closure.
Repeated references to one artifact share one linked object; this does not cache source initialization.
Raw native objects join the process object before the runtime archive is linked.
Validation fails before executing initialization or publishing a consumer executable.
Compiler-generated objects are trusted to implement their manifests; hashes do not verify machine code or authenticity.

Publication uses the shared staging and atomic symlink mechanism above.
The immutable native bundle contains its object, generated binding, and manifest;
the public `.unit.json` link is published last and selects the complete bundle.
Dependencies refer to immutable manifests with bundle-relative paths.
Library artifacts currently keep native-unit and C dependency boundaries separate.
The language reference owns initialization multiplicity, source-visible eligibility, and supported execution targets.

[Native unit regressions](../../cli/tests/unit.rs) remove producer source before dependent compilation,
pass captured boxed values through multiple objects, exercise both stack parities and collection
in callers/callees, and reject incompatible imports before publication.
[Manifest tests](../../cli/tests/unit_manifest.rs) cover malformed interfaces, corrupted contents,
compatibility mismatches, shared dependencies, and undeclared or conflicting providers without executing code.

## C12. Shared Native Model, Allocation, and Collection

[zydeco-machine](../../lang/machine/src/lib.rs) is a dependency-free `no_std` model shared
by the compiler and native stub.
It owns tagged words, closure records, host-transfer records, and frame actions.
Code generation derives layout from 64-bit carriers; the target runtime checks its `usize` representation against them.
The bundled model sources determine fingerprinted process and library entry symbols,
so mismatched compiler/runtime source bundles fail to link.
This is artifact pairing, not verification of handwritten instruction selection.

Odd words are immediate. Even words are pointer-shaped.
`Int`/`UInt`, narrower integers, `Float32`, characters, and tags fit in an immediate word.
`Int64`, `UInt64`, and `Float64` payloads use opaque scalar boxes.
Integer arithmetic wraps at the source payload width, including operations left
as builtin calls after optional normalization.
Products and closures occupy scanned blocks.
Source numeric domains and immediate ranges are specified in [L13](language.md#13-primitive-values-and-capabilities);
storage and C carriers are specified in [L14](language.md#storage-and-foreign-transport).
Aligned host-owned objects outside the managed spaces remain unchanged by tracing.

### Scalar Value Boundaries

A primitive's numeric domain, ordinary value representation, and storage/C carrier answer different questions.
[`IntegerType`](../../lang/syntax/src/lib.rs) keeps those facts separate as `bits`,
`representation`, and `storage_bits`.
An eight-byte carrier therefore does not imply either a full-width source domain or an unboxed value word.

Every `Int64`, `UInt64`, or `Float64` at a general value boundary is a pointer to a one-word opaque box,
even when its bits could fit in an immediate.
This includes arguments, returns, products, captures, existential/polymorphic values, and native unit interfaces.
The payload contains raw bits and the collector never traces it; only the enclosing pointer is a root.
These compiler-owned primitive representations require no source meta annotation.

[Scalar regions](../../lang/syntax/src/scalar.rs) make representation changes explicit
as `Decode`, `Encode`, and raw `Arithmetic` instructions.
Their verifier derives each definition's `Value(T)` or `Raw(T)` representation, checks width and signedness
through its scalar type, rejects unavailable operands, and checks the declared ordinary result type.
Regions contain no calls, branches, or scanned-field construction.
Only a verified, immutable `ScalarProgram` reaches instruction selection.
Source typing and SPSLow protocol validation supply the input classifiers;
region verification checks their representation-preserving use, rather than reconstructing source typing.

The shared [SPSLow analysis](../../lang/stackir/src/low/scalar.rs) groups `Int64`, `UInt64`,
and `Float64` primitive trees and adjacent single-use primitive bindings, up to 32 operations per region.
Leaves must be scalar literals or variables.
Calls, branches, captures, shared bindings, and intervening constructions preserve ordinary value boundaries.
Arithmetic retains its evaluation order, including division/remainder failures.
The rewrite cancels `Decode(Encode(raw))`, removes unused conversions, and verifies the result again.
`Boxed` retains individual primitive boundaries; `Direct`, `Local`, and `Shared` enable this elimination.
The [execution regression](../../cli/tests/passes.rs) compares both policies with the interpreter
and all compiled backends, and counts the removed scalar allocation sites.

Native preparation assigns checked, distinct homes for value words and raw bits.
The [AMD64 emitter](../../lang/amd64/src/emit/primitive.rs) initializes additional value homes
before allocation and places raw homes below an explicit collector root cursor.
Live box pointers can therefore move during collection while raw numeric bits remain unchanged.
The [Wasm emitter](../../lang/wasm-common/src/word.rs) uses distinct locals; its heaps currently do not collect.
Signed minimum divided by `-1` is handled before target overflow traps.
Host builtins still receive spare boxes before decoding arguments, so their conversion helpers do not collect.

Negative tests reject representation mismatches, raw region exits, and incorrect or aliased native homes.
Runtime regressions cover full-width C round trips, source-free unit calls across collection,
and numeric payloads equal to heap addresses.
These checks cover the implemented lowering; machine instruction selection remains trusted and tested.
[Further scalar unboxing](../proposals/escape-unboxing.md#further-scalar-unboxing) needs explicit evidence at joins,
source calls, captures, and mixed raw/reference layouts.

### Raw Memory Kernels

A [memory kernel](../../lang/syntax/src/scalar/kernel.rs) encloses raw scalar arithmetic between one load and one store.
The shared [low-SPS planner](../../lang/stackir/src/low/scalar/memory.rs) recognizes `Int64`, `UInt64`,
and `Float64` loads followed by a primitive tree or adjacent single-use scalar bindings
and a store of the same scalar type, with at most 32 arithmetic operations and 32 intervening bindings.
Every eliminated scalar binding must be consumed inside the kernel.
Calls, branches, shared or escaping scalar results, other effects, and incompatible scalar types end eligibility.
An independent destination variable or bounded total wrapping address calculation may move to the entry.
Intervening `AddrOffset` bindings may also move there when their bases and displacements are independent
of the loaded value and scalar intermediates; their original order and bindings remain available to later uses.
These address bindings count toward the same 32-binding limit and may be shared because they are retained.
A possibly trapping displacement calculation retains ordinary lowering in its original order.
The boxed policy disables kernel formation; other scalar-eliminating policies enable it.

The immutable `ScalarKernel` has an explicit memory boundary: input zero is the loaded scalar,
other inputs are decoded ordinary scalar variables, constants become raw literals, and the raw result feeds the store.
Its verifier reuses the scalar arithmetic representation checker, requires matching load/store types,
and rejects unavailable or mismatched operands.
Its instruction set contains no source calls, managed values, scanned fields, or representation-changing steps.
Ordinary `ScalarProgram` inputs and results continue to require the value ABI.
No raw entry convention is inferred for a source function or unknown callback.

ZASM keeps the complete kernel in one instruction, consuming two addresses and its ordinary scalar arguments.
Native lowering loads once, executes the checked arithmetic sequence, and stores once;
literal and intermediate scalars introduce no boxes, callback objects, or successful-path calls inside the kernel.
Raw definitions have distinct temporary stack homes, with no collection inside that area.
Fatal arithmetic helpers do not return; required failures precede the store and later arithmetic.
This removes allocation sites without claiming register allocation or minimal spill traffic.
Wasm uses raw locals and the existing virtual-memory imports for the load and store;
the host's address lookup cost remains.

The [regressions](../../cli/tests/passes.rs) pair optimized kernels with ordinary and unknown-callback paths,
check full-width wrapping and runtime operands across all backends, preserve error order,
and exercise the size limit, overlapping unaligned addresses, and retained pure address bindings.
The [typed field update](../../lib/tests/std/typed-memory-kernel.zy) composes record state transitions,
field projection, and a fixed view with these same scalar operations.
The [assembly interpreter check](../../lang/assembly/src/interp.rs) pairs a successful write
with arithmetic failure and verifies unchanged destination bytes on failure.
The [dated probe](../evaluations/2026-09-15-memory-kernels/README.md) records whole-program allocation sites separately
from the allocation-free kernel.
The [typed field/header comparison](../evaluations/2026-09-15-typed-memory/README.md) exercises this boundary
through std recipes and records residual allocation outside it.
Calls, joins, managed components, and broader contification remain
in [memory compilation](../proposals/memory-compilation.md#44-carry-raw-components-only-across-agreeing-entries).

### Runtime Instances

[RuntimeInstance](../../runtime/stub.rs) owns the managed heap, frame store, stack root bound,
host-transfer record, retained immutable allocations, I/O handles, invocation arguments, and host-owned strings.
A thread-local pointer dispatches helpers to the active instance; it owns no language state itself.
Entry saves its predecessor and installs a fresh instance, while normal completion restores
that predecessor and drops the completed instance before releasing its unit guard.
Nested calls into different units therefore preserve the suspended caller's heap, roots, transfer state, and resources.
The [source entry profile](language.md#compiled-libraries-and-c-exports) defines concurrency and fault behavior.

The process launcher in [runtime/main.rs](../../runtime/main.rs) uses the same instance API,
with process arguments and the established executable entry.
Reusable support disables the `process-entry` feature.
Semispace words and block indices are boxed allocations, avoiding multi-megabyte stack temporaries on each C entry.
Host strings retain stable boxed addresses in their instance, and handles/buffers use instance-owned arenas.
These resources are released at normal teardown; managed GC does not individually reclaim host-owned strings.

### Environment Actions and Roots

[frames::Environment](../../lang/machine/src/frames.rs) exposes Enter, Suspend, Resume, and Roots actions.
Enter reserves before changing state and returns the possibly relocated active base.
Suspend validates captures and creates a tagged token; Resume validates its owner and nesting, then restores the frame.
Roots returns mutable addresses for the active map joined with all pending suspension maps.
Only initialized live slots are roots; reserved capacity and dead tagged words are insufficient evidence of liveness.

The retained store grows geometrically and caches capacity at its high-water mark.
Only successful Enter may relocate retained frame words; Suspend keeps the active base stable.
Slot access uses raw pointers without constructing Rust references over generated-code storage.
Metadata vectors hold indices and checked tokens, so their growth cannot invalidate logical frame references.
Failed word reservation preserves existing words, bases, and tokens;
zeroed capacity does not count as an initialized binding.
Transitions may allocate Rust metadata but do not collect the managed heap;
metadata allocation still has the Rust allocator's ordinary failure behavior.
Root addresses expire at the next attempted environment transition, including a failed reservation.
They identify values and need not coincide with active-layout offsets in every environment implementation.
Managed collection may rewrite those values but cannot relocate their published addresses.
A model declaration generates action headers and serialization order;
emitted records and target carriers share checked alignment.
A return prologue removes `token(F)` while preserving `result :: S`, resumes the owner,
restores `rbp`, and then binds the result.
Returning host and C calls enter that same prologue.

The [native environment proposal](../proposals/native-frames.md) retains the compact-storage
and moving-root experiments; they do not extend the default environment contract described here.

### Managed Allocation

The [native stub](../../runtime/stub.rs) uses [CheneyHeap](../../runtime/gc.rs) with two fixed 1 MiB semispaces.
The live graph, including headers, must fit in one space.
Allocation receives a deferred root source: a successful fast allocation does not enumerate roots;
collection requests the control-stack range and live frame slots and updates them in place.
An oversized request can fail before enumeration.
Failure after collection still leaves the relocated live graph valid.

A block-start index finds the allocation owning a word-aligned interior payload pointer.
Copying installs forwarding information and preserves sharing, cycles, and the interior offset.
Cheney scanning traverses pointer-bearing payloads; opaque scalar bits are copied without tracing.
Unmanaged pointers remain stable.
Scanning order and forwarding must never reinterpret scalar bits as managed references.

Host helpers that can trigger collection must publish every live managed word and reload relocated roots afterward.
Raw borrowed C argument frames are discarded before any result-box allocation that can collect.
[GC regressions](../../lang/tests/tests/native_gc.rs) cover roots, interior pointers,
aliasing, cycles, capacity failure, and allocation after collection.
Frame storage reclamation and managed-value liveness are separate obligations.

## C13. WebAssembly Backends and Embedding

Both emitters produce core `wasm32` modules with `memory`, `entry`, and `_start` exports.
An embedding supplies the `zydeco` import namespace; the module is not a standalone WASI program.
[wasm-common](../../lang/wasm-common/src) owns shared role and word conventions.

The pipeline forks after shared closure conversion: SPSLow still retains lexical blocks,
while ZASM has decomposed them into machine program points.
Starting at high SPS would duplicate closure conversion; recovering structure
from ZASM would require reconstructing lexical regions and stack effects.
Direct SPS emission can use structured Wasm and locals within a block and remains independent
of lazy ZASM construction and native preparation.
AM emission reuses ZASM's local representation and stack work and closely matches its interpreter,
at the cost of one emitted function per program point.
Both use module-owned dispatch because recursive host calls would consume the engine's stack
on unbounded source transfers.

| Backend | Input and control representation | Storage consequences |
| --- | --- | --- |
| [wasm-sps](../../lang/wasm-sps/src/emit.rs) | One function for the root and each SPSLow block; structured code and locals inside blocks; a trampoline between blocks | Persistent `[head, tail]` stack frames, boxed products, and a bump heap |
| [wasm-am](../../lang/wasm-am/src/emit.rs) | Portable ZASM program points, a private program counter, instruction functions, and a dispatch loop | Reusable indexed environment, fixed 1 MiB operand/control stack, and a bump heap |

Neither path uses recursive host calls to realize unbounded Zydeco control transfers.
SPS closure code handles are tagged table indices; AM program counters are private untagged indices.
Both heaps grow without collection.
SPS partial products use a pointer into the product suffix; constructors use `[tag, payload]`.
Emission sorts arena-derived IDs before assigning functions, imports, locals,
and static-data offsets; unsupported forms report typed emission errors.
The SPS path retains lexical structure but currently uses a whole-program local plan and uniform product boxing.
The AM path reuses ZASM representation work but emits at machine-program-point granularity.

### Module and Host ABI

Source values cross as tagged `i64` words. Pointer-shaped values address module or host-owned representations;
module-created closures and stack records remain opaque to the host except through the shared protocol.

| Import shape | Contract |
| --- | --- |
| Returning operation | Zydeco arguments as `i64`, one `i64` result |
| Control operation | Arguments as `i64`; result is count, closure, and two argument slots, all `i64` |
| Potential full-width scalar result | A trailing `i32` spare-box address where the operation signature requires it |
| `string_literal` | UTF-8 byte offset and length as `i32`, opaque host string word as `i64` |

Control arity is at most two. Spare boxes belong to the module's allocation protocol;
the host must not invent closure layouts or return unregistered control code.
Native C imports are rejected by both emitters.

The [Node host](../../cli/wasm/wasm-host.mjs) supplies I/O, resources, random integers, and scalar adapters.
The CLI bundles it with its numeric and checked-memory helpers for source execution outside the checkout.
`run` inherits terminal streams; `test` captures output with explicit input through the shared execution runner.
Argument lookup uses the invocation's supplied sequence; lazy folds use module-created source closures,
so multi-argument traversal needs no host-created closure layout.
[Backend strategy questions](../proposals/wasm-backends.md) retain default-target criteria and historical comparisons.
Conformance tests should distinguish module emission, embedding failures, stack exhaustion, and source runtime failures.

## C14. Builtin Contracts, Primitive Operations, and Foreign Calls

The [syntax role catalog](../../lang/syntax/src/lib.rs) identifies intrinsics and operations with domain types.
[Static Builtin validation](../../lang/statics/src/builtin.rs) checks the authored
[Builtin signature](../../lib/std/builtin.zy) against those roles.
Interpreter linking and [SPS Builtin lowering](../../lang/stackir/src/builtin.rs) materialize
the validated structural plan.
Host calls retain `BuiltinValueRole` through dynamic and Stack IR syntax.
The role supplies arity and calling mode; emission derives the external symbol at the target ABI boundary.

Canonical representation types have shared intrinsic identities;
opening a provider's packed value introduces witnesses for its resource capabilities.
Named structural routes and static fields erase before backend layout.
[L13](language.md#13-primitive-values-and-capabilities) owns source observations,
and [module interfaces](language.md#module-interfaces-and-shared-openings) explain dependency choices.
Returning and continuation-selecting operations have distinct host call plans.
C8 owns arithmetic exposure and folding; C11–C13 own the resulting target words and calls.
The dynamic and ZASM interpreters share `PrimitiveOp::evaluate` with constant folding.

Strings are immutable UTF-8 text. `Bytes` is a source-defined abstraction
whose [owning design](language.md#immutable-owners-and-source-bytes) specifies its representation and operations.
The compiler and host recognize general memory capabilities, without a byte-sequence type or operation family.

[Manual memory](../../lang/machine/src/memory.rs) is shared by the interpreter and native host.
`Address` wraps a raw pointer; `MemoryLayout` validates allocation requests using the platform allocator layout.
Allocation is uninitialized, and explicit release receives the exact base and original layout.
Native `Addr` words contain pointer bits directly.
The tracing collector leaves unmanaged addresses outside its managed semispaces unchanged;
raw storage must not contain unrooted managed references.
There are no grant records, liveness tables, pointer-slot maps, or initialization bitmaps.
`RetainedMemory` owns explicitly transferred immutable allocations until runtime teardown.
It reserves retention bookkeeping before accepting ownership, so failure leaves ownership with the caller.

The [Node host](../../cli/wasm/wasm-memory.mjs) emulates unmanaged memory with virtual 64-bit addresses and byte arrays.
Its range lookup is an embedding implementation cost; it is not a source guarantee of recoverable raw-access errors.
The virtual address space is separate from managed Wasm memory and exports no native pointer.
[L13](language.md#manual-memory) owns caller obligations and the distinction between static evidence and runtime data.
The source [view](../../lib/std/memory/view.zy),
[field](../../lib/std/memory/field.zy), [record](../../lib/std/memory/record.zy),
and [array](../../lib/std/memory/array.zy) factories use these same primitives.
C6 eliminates fixed view functions and field recipes; runtime-selected views and layouts use ordinary source thunks.
`Fields` states and layout witnesses introduce no runtime tags or new builtin roles.
Array whole-value reads explicitly build managed logical contents; direct initialization and indexing do not.

Scalar `store_le` takes an address, value, and completion; `load_le` takes an address and result successor.
These roles use [ordered scalar memory accesses](#ordered-scalar-memory-accesses),
which own their instruction selection, representation adapters, and carrier validation.
They have no CPS host-call or hidden spare-box ABI.
The [source codecs](../../lib/std/memory/codecs.zy) write products directly into destination fields.
Padding contributes no load or store. Fixed realization requires statically evaluable size and alignment;
dynamic realization captures the required placement in its source operations.
Types and state witnesses erase, while explicit queries may materialize constants and ordinary closures may allocate.

String imports and primitive I/O reads return a retained address and byte count through a two-argument successor.
UTF-8 decoding validates the bytes in a caller-established readable range.
I/O writes receive a writer, raw address, count, error successor, and completion.
The source system library mediates these raw interfaces through its shared packed value for immutable bytes.
Byte codecs validate exact lengths before raw scalar reads.

Argument lookup returns one string or selects the missing branch; it retains no Zydeco continuation.
The native host caches argument strings outside the managed heap, with no managed references in the snapshot.
The [source argument library](../../lib/std/system/arguments.zy) supplies traversal and lazy tails.
These use the ordinary closure, activation, and collection protocols, including reuse and abandonment.
The former native host closure and fixed host-root table have been removed.
[Argument regressions](../../lib/tests/builtin/argument-contract.zy) exercise repeated forcing across collection,
live captured `Float64` values, discarded tails, and invalid indices on all four backends.
Source observations belong to [L13](language.md#13-primitive-values-and-capabilities).

Host resource tables allocate monotonically increasing handle IDs and validate reader and writer operations.
Closing removes the resource, so every alias subsequently observes `Closed`;
identifiers are not source-visible pointers.
Reserved standard-stream capabilities remain open.
The interpreter backs them with injected streams, while native execution uses the process streams.
Primitive success/error branches carry a result or an error kind and message;
[the library](../../lib/std/README.md#streams-and-files) constructs `Result`, `Option`, and typed paths.
Adapters distinguish EOF, empty data, invalid text, I/O errors, and closed resources.
[Capability design](../proposals/filesystem.md) owns stream extensions and resource-lifetime choices.

### Foreign Calls

[ForeignSignature](../../lang/statics/src/foreign.rs) is a checked call plan for a returning C thunk.
Arguments are supported integers or `Addr`. An address contributes one raw pointer.
Bindings supply any length or capacity as a separate integer.
At most six C arguments are accepted. Results are supported integers or `Unit` (C `void`).
Its constructor enforces the flattened bound, and the validated fields remain private.
Expansion yields ordered `ForeignArgument` entries identifying the source parameter and its integer,
or pointer component; both execution paths consume that plan.
Checking validates the declared shape, not the external symbol's actual ABI.
The trust and borrowing obligations belong to [L14](language.md#14-foreign-interfaces).

The Unix [interpreter adapter](../../lang/dynamics/src/foreign.rs) lazily loads libraries and symbols,
caches call interfaces by target and signature, and calls through libffi while borrowing scalar argument storage.
Missing libraries and symbols are runtime errors; generated native programs do not depend on libffi.
AMD64 marshals the retained source arguments into a temporary raw C frame,
loads the SysV argument registers, and discards that frame before encoding the result.
A valid result resumes the ordinary return continuation; an invalid `Int` or `UInt` result reports a range error.
Integer components retain their source type and signedness through the call plan.
`IntegerType::bits` describes arithmetic width; `storage_bits` describes the byte and C carrier width.
Native encoders truncate C return registers to the carrier width before checking the source range;
the [SysV ABI clarification](https://gitlab.com/x86-psABIs/x86-64-ABI/-/merge_requests/61)
leaves excess integer register bits unspecified.
`Int64` and `UInt64` results are boxed after the raw scratch frame has been discarded;
the raw result remains in an untraced callee-saved register during allocation.
Other integer results and unit fit immediate words.
For C exports, raw input registers are saved above the instance's traced stack bound.
Each incoming full-width integer receives a box, and the entry allocator roots previously encoded arguments.
Result decoding happens before the instance is destroyed, returning all payload bits in the C result register.
The libffi adapter uses exact scalar storage and return types for integers,
and its explicit void-return operation avoids reading nonexistent result storage.
Native address marshalling moves the pointer word directly, with no host conversion call.
The interpreter verifies that the source value is an `Address` and passes its raw pointer to libffi.
Neither path validates the allocation or establishes a borrow: memory validity
and foreign ownership are caller obligations.
Marshalling helpers do not collect. Explicit retention protects `Bytes` storage across the call;
manual allocations remain live until their owner releases them.
The `Ret` classifier alone establishes neither termination nor a cleanup scope.
Callbacks into the active instance remain unsupported. Explicit compiled-library manifests select exact artifacts
through the [unit linker](#compilation-unit-preparation-and-artifacts);
otherwise native linking uses the library's linker name and interpreter loading uses platform shared-library names.
Native foreign imports are unsupported in Wasm and the ZASM interpreter.

[Foreign signature tests](../../lang/statics/tests/foreign.rs)
and [FFI integration tests](../../lang/tests/tests/ffi.rs) pair valid shapes with unsupported arities,
argument sorts, results, loader failures, and borrowing cases.
Scalar exports use the [C entry adapter](#compilation-unit-preparation-and-artifacts) and the same scalar call plan.
Incoming pointers remain outside the scalar export profile; they need a separate entry adapter and binding contract.
[Callbacks and retained values](../proposals/c-ffi.md#closures-callbacks-and-reentry) still require ownership
and entry protocols beyond fresh scalar calls.

## C15. Diagnostics, Formatting, Documentation, and Interactive Tooling

Tooling uses the facts established by ordinary source phases and associates results with their source revision.
[Session queries](../../lang/session/src/source/query.rs) supply semantic identities;
[Cajun](../../editor/cajun/src/analysis.rs) adapts them to LSP and discards superseded results.
Rendered labels must not become keys for semantic lookup.

[Type diagnostics](../../lang/statics/src/check/error.rs) carry stable codes and structured relationships;
[report construction](../../lang/session/src/source/report.rs) resolves their source sites and explanatory context.
CLI, TUI, and LSP render those reports for their surfaces.
Retained rejected facts can support useful tooling, while later errors caused solely
by an earlier failure should not manufacture independent evidence.
Definition, reference, rename, hover, and semantic-token operations use current provenance and lexical identity.
UTF-16 conversion happens when reading or writing client positions.

### Diagnostic Collection

Parsing retains all `ParseIssue` entries in `ParseFailure`.
`ParseError::diagnostics` pairs every issue with the exact rejected source snapshot; CLI reports,
editor locations, and plain-text formatting enumerate the complete collection.
Each issue retains its own token expectations and source range, including a zero-width EOF location.

Source analyzers, desugaring, and resolution retain vectors of typed domain errors.
Strict transformation failures carry a nonempty `Diagnostics<E>` collection together with their source context;
`Err` means no valid phase product is available, regardless of how many errors were established.
Analyses may expose partial facts separately, and completion has its explicit recovered-program contract.
Dependent phases still require valid input.
A failed binder cannot justify inventing a scope for its body.

Semantic folders record each diagnostic once and propagate `ReportedError` when reconstruction fails.
Independent siblings continue where their input contracts remain satisfied; cached rejection does not replay errors.
The session and CLI, TUI, and LSP retain and render every reported error with its own source location.
Primary-location convenience APIs do not replace complete diagnostic enumeration.

Producers append diagnostics and composition concatenates collections.
Diagnostic ordering is not an API guarantee; successful fact ordering retains its own contract.
Tests compare contents, multiplicity, and locations rather than presentation order,
and pair rejected inputs with valid counterparts.
Equal spans alone do not identify duplicate errors.
Checker publication deduplicates complete `TyckDiagnostic` values. It suppresses unresolved-classifier consequences
when the fill's originating expression contains an already reported rejection site, including related sites
of an unconstrained-inference diagnostic; unresolved fills in independent expressions remain reportable.

Recovery boundaries are owned by their producing phases:
[source loading](#c3-source-loading-sessions-queries-and-memory-retention),
[shared source analysis](#shared-source-analysis), [desugaring](#desugaring-folders),
[resolution](#name-resolution), [checking and finalization](#checker-recovery),
[execution readiness](#residual-runtime-traversal), and [high SPS verification](#high-sps-analysis-traversal).
These contracts state which work continues and which dependent product remains unavailable.
Further diagnostic producers and presentation customization remain
in the [traversal proposal](../proposals/traversals.md#diagnostic-collection-and-recovery).

### Formatting and Typed Rendering

Formatting is a semantics-preserving normalizer over the parsed textual arena.
Canonical syntax gives equivalent spellings one form; retained intentions preserve author layout only
at declared grammatical boundaries.
A pretty-printer maps that model to a document, and a formatter parses, prints, and replaces source.
The CLI and Cajun share that implementation.
The [textual formatter](../../lang/surface/src/textual/pretty.rs) owns source layout;
its [options](../../lang/surface/src/textual/pretty/config.rs) default to width 100, indentation 2,
preserved layout intentions, and minimal parentheses.

#### Terminology

A **sequence binding** is one of `let`, `do`, `def`, or `param`: a binding whose computation continues into a tail.
Sequence bindings are block-like: their tail always breaks onto a new line at the binding's indentation,
and the binding itself starts on a line of its own.

A **tail marker** is one of `in`, `that`, or `;`: the token that closes a sequence binding and begins its tail.
The **placement markers** are `in` and `that` in particular.
The printer places them through the inline, attached, and aligned tiers, mirroring the definition separator.

A **stage separator** is `:` or `=`: `:` joins a head with its type, and `=` joins the type with its bindee.
A **scope marker** is `.` or `=>` and introduces a scope body.
An **arm block** is one of `match`, `comatch`, `data`, or `codata`: a construct whose arms each occupy their own line.
Arm blocks are block-like: they always expand, so they begin on a line of their own.
A **delimited region** is a `{ }` or `( )` group, or a `begin`...`end` block:
its contents nest inside its delimiters while the delimiters themselves hug the surrounding line.

#### Layout Laws

Every formatter guarantee and layout family below follows from four layout laws.
The printer may introduce or remove a break, or add or remove indentation, only where a layout law permits it;
everywhere else it keeps the canonical compact layout.

**A break belongs to a boundary.** Every line break is owned by exactly one grammatical boundary.
Punctuation placement and continuation indentation belong to the boundary,
never to either neighboring child in isolation.
A gap without a declared boundary is canonical spacing and never breaks.

**Indentation comes from the owning boundary.** A boundary contributes indentation only
through its declared continuation policy: a broken boundary hangs its continuation one level below the boundary's line,
or aligns it for aligned families; a joined boundary contributes nothing.
There is no other source of nesting, and no fixed offsets: the printer measures the boundary's indentation dynamically.

**Closers return to their boundary.** A delimiter closer returns to its opener's line.
A stage separator or tail marker returns to the binding's indentation through three tiers:
it stays with a single-line payload, follows the payload's final line when that line returned
to the boundary (the delimited closer), or takes a line of its own at the boundary.

**Blocks anchor; delimiters hug.** A construct whose interior aligns with its head — a sequence binding,
whose tail aligns with the binding, or an arm block, whose arms align with the keyword — must begin on a boundary line.
A delimited region instead hugs the line it lands on: the opener stays put,
the contents nest one level inside, and the closer returns to the opener's line.
A singleton group therefore keeps its delimiters whenever its contents span more than one line.

This section specifies textual formatting: boundaries consult retained intentions,
carry trivia, and honor `@[format(...)]` directives.
The [statics renderer](#elaborated-type-rendering) has its own grammar and layout implementation.
It uses breakable document gaps and precedence-aware grouping, without source intentions or trivia;
it does not implement every textual layout family.
The scoped formatter is a debug renderer, and the dynamics printers render linked IR;
neither defines canonical source layout.

#### Retained Source Information

A parsed source has three kinds of printable information:

| Kind | Examples | Preservation contract |
| --- | --- | --- |
| Canonical syntax | binders, applications, precedence, field payloads | Preserve meaning and choose one spelling. |
| Trivia | documentation, line, and block comments | Preserve content and effective attachment. |
| Intentions | a joined line, a break, one empty line, a multiline group | Use only at declared layout boundaries. |

Spans provide evidence for trivia and intentions; they are not a second printable syntax tree.
A leading comment extends the layout start of its anchor,
so the boundary compositor sees the separation before the comment.
The comment text itself remains stored once in `SurfaceTrivia`.

```text
source -> lexer and parser -> textual arenas and spans
       -> presentation capture -> trivia and intentions
       -> grammar-aware document construction
       -> width selection -> formatted source
```

Punning belongs to canonical syntax rather than intention.
If a named term or pattern contains the same-named variable, the printer always chooses its concise form.
An annotation with a hole payload prints in its parenthesized `@(meta)` form;
`@[intrinsic(int)] _` and `@(intrinsic(int))` therefore converge.
Line comments use `--` or `--|`; nested block comments retain their delimiters and relative indentation.
Raw whitespace is not retained except under an explicit `@[format(verbatim)]` directive.

#### Formatter Laws

The following laws elaborate the layout laws for concrete constructs.

##### Semantic Identity

Formatted output must parse and desugar to the same structure.
Parentheses are removed only when the exact parser position accepts the enclosed term or pattern.
Annotation payloads are checked separately because moving an annotation across a named
or projected pattern changes the tree even when the printed tokens look similar.

##### Content Retention

Every comment survives. Documentation starts on a fresh line at the indentation of its anchor,
and only an adjacent documentation block attaches to `@[doc]`.
An unattached block remains visible and produces a warning.

##### Canonical Convergence

Equivalent spellings converge on one form. Horizontal spacing and puns are canonical,
empty regions contain at most one empty line, and a complete source ends with one newline.
Formatting twice with the same options must have no further effect.

##### Layout as a Lower Bound

Vertical separation has the following order:

```text
joined < broken < one empty line
```

Under the `Preserve` policy an observed break is not collapsed and a larger empty region becomes one empty line.
Under `BlankLinesOnly` the same holds for blank lines, while every single break is left to the width decision.
Under `Ignore` no observed separation is retained.

A joined boundary can still break when its compact form does not fit.
A boundary that always breaks, such as the gap between match arms or between the stages of a `do` chain,
retains one empty line where the source had at least one.
Local groups remain compact when they fit, even inside an expanded parent.
Because a retained break persists, a line that the width forced to wrap in an earlier run stays wrapped
until the author rejoins it; the lower bound never re-joins automatically.

##### Boundary Composition

A syntax case combines child documents through a named boundary policy.
It must not inspect rendered text to discover whether a child fits or spans lines.
Punctuation placement and continuation indentation belong to the relationship between children rather than
to either child in isolation.

#### Boundary Algebra

`LayoutFragment` carries a document and the first and last syntax anchors represented by that document.
`LayoutBoundary` names the source gap to consult:

- `Between` lies between consecutive entities.
- `AfterStart` lies between an enclosing construct and its first child.
- `AfterArmPrefix` lies between an arm header and its payload.
- `AfterExistentialOpen` lies just inside a grammar-owned parameter delimiter, between the `(` and its binder.
- `BeforeExistentialParameter` lies before a grammar-owned parameter delimiter that is not part of its binder.
- `BeforeEnd` lies between the final child and its closing delimiter.

The printer then chooses how much of that source information applies.
A boundary can be canonical, preserve the full break intention, or preserve only an empty line
after moving a marker onto its own line.

`BoundaryLayout` supplies the compact gap, expanded gap, marker placement, and continuation nesting.
Its common forms are named by their effect: `aligned`, `hanging`, and `nested`.
`StagedBoundary` distinguishes an ordinary annotation from the `:` and `=` stages of a binding,
because their expanded forms carry different indentation.

Document alternatives use the `pretty` algebra directly.
A flexible boundary exposes its compact projection to an enclosing group
while retaining a complete expanded alternative.
A candidate that is valid only on one line contains a flat-mode guard.
The final renderer therefore performs the only width selection; the printer does not render temporary strings
or maintain a syntax-specific boundary mode.

##### Canonical Gaps

A source gap participates in intention preservation only when a syntax case declares a layout boundary for it.
Every other gap between entities is canonical spacing.
Postfix projections and destructors never break, so a source break before `/` or `.` joins the operator to its head.
`in` and `that` belong to the bindee's line; an empty line written
before the placement marker is re-anchored between the tail marker and the following tail.
Sequence tails always start a new line even when the source joined them.
Declaring a new boundary is the only way to make a gap intention-aware.

#### Canonical Layout Families

Most constructs use one of these families:

| Family | Compact form | Expanded form |
| --- | --- | --- |
| Delimited region | Contents stay between delimiters; a thunk is `{ body }`. | Contents nest once and the closer returns to the opener. |
| Juxtaposition or list | Items use their canonical separator on one line. | Continuations nest once while fitting subgroups remain intact. |
| Parameter telescope | A fitting telescope follows its head. | A joined first row stays beside the head; the remaining rows hang one level below. The head stands alone only when the source broke the first row away or the row does not fit, while width expansion gives each parameter a row. |
| Infix chain | Operators have one space on each side. | `*` and `->` lead continuation lines without recursive indentation. |
| Headed scope | A short head keeps `.`, `=>`, and its body together. | A multiline head ends with an aligned marker, then the body nests once. |
| Staged binding | Header, type, bindee, and placement remain together when they fit. | `:`, `=`, and then `in` or `that` close the stages at the binding indentation. |
| Sequence binding | A short stage may remain compact. | The tail marker (`in`, `that`, or `;`) always breaks, the tail returns to the binding indentation, and the binding starts on a line of its own. |
| Arm block | A short arm header and payload share a line. | Arms begin with aligned `\|`; a broken payload nests once, while comments before `|` remain at the arm boundary. Blank lines between arms, after the head, and before `end` survive as one empty line. |

Each grammatical group makes one width decision for the boundaries it owns.
If a delimited row overflows, the delimiters and item separators enter their expanded layout together;
boundaries inside each item remain independent.
A grammar-owned parameter delimiter carries no syntax entity of its own,
so a delimited group whose content anchor is its own entity spans no source gap after its opener:
the group reads its retained break from the recorded `AfterExistentialOpen` boundary instead of an anchor pair.
A comment written before the `(` anchors at the parameter boundary and stays outside the delimiters,
while a comment after the `(` belongs to the binder payload and keeps its line for that boundary.
Staged bindings follow one nesting discipline. A joined stage boundary never nests its continuation:
the continuation's own layout families measure from the binding's indentation,
so a delimited type hangs its contents one level below the binding and returns its closer to the binding.
Only a broken boundary hangs the continuation one level below.
The `=` stage then chooses between three tiers: the whole `type = bindee` stage on one line, the separator attached
to the type's final line when that line returns to the binding indentation (the delimited closer),
or the separator on its own line at the binding indentation.
The placement marker mirrors the same three tiers: it stays on a single-line bindee,
follows the bindee's final line when that line returns to the binding indentation (a delimited closer,
as in `end in` or `} that`), and otherwise breaks onto its own line at the binding indentation.
A broken bindee hangs one level below its separator,
and the printer captures the binding indentation dynamically instead of assuming fixed offsets.
Preserved source breaks partition fitting rows, but an overflowing row expands the complete outer layer rather
than whichever nested boundary happens to encounter the width limit first.

For layout purposes the scope markers `.` and `=>` and the tail markers `in` and `that` are scope-boundary markers.
This is a presentation role shared by several grammar categories.
A constituent is “short” exactly when its complete compact alternative fits in the remaining configured width;
there is no second length threshold.

Canonical textual printing folds adjacent scopes of the same form into one parameter telescope.
Under `Preserve`, a source line break before the nested introducer stops the fold;
under `BlankLinesOnly` only a blank line does.
This rule applies to `fn`, `pi`, `forall`, `sigma`, and `exists`.
Consecutive existential nodes also normalize to one telescope during desugaring,
so the compact and repeated spellings have the same elaboration.

Minimal parenthesis formatting retains grammar-required groups.
It also retains a singleton group whenever its contents span more than one line,
so the delimiters can hug the enclosing line while the contents nest inside.
Applications are the one self-grouping family: their own compact-or-hanging boundary subsumes a singleton wrapper.
`Parentheses::Preserve` is available when every parsed singleton group must remain.

#### Formatter Directives

Printer policy is expressed in the source as a `format` meta annotation:

```text
@[format(width(100), indent(4), layout(blank_lines))] expression
```

Each option is a nested call taking one argument, except `verbatim`, which takes no argument:

- `width(columns)` sets the target line width;
- `indent(columns)` sets the indentation width;
- `layout(preserve)`, `layout(blank_lines)`, or `layout(ignore)` selects how much recorded layout is retained;
- `parentheses(minimal)` or `parentheses(preserve)` selects singleton-group treatment;
- `verbatim` copies the annotated expression's original source text unchanged, including its internal line breaks,
  indentation, and comments.

A directive applies to the annotated expression and everything inside it.
Options without a directive keep their enclosing values, so nested annotations override enclosing options field
by field and the innermost directive wins.
A malformed `format` annotation is inert: the printer renders it as ordinary metadata and applies no options,
leaving the misspelled directive visible in the output.

Metadata calls are structured delimiter groups rather than opaque rendered strings.
A fitting call stays compact; an overflowing call expands its immediate argument list,
while nested calls make their own width decisions.
This choice is local to the annotation, so the length of its following payload cannot force short metadata to wrap.
Under `Preserve`, argument rows authored on separate lines remain separate,
and comments anchored to nested metadata arguments remain in the group.
A comment before an annotation's `@` remains outside its brackets; a comment
after the opening bracket remains inside the metadata wrapper.

Structural options (indentation, layout intentions, parenthesis treatment) shape the payload document directly.
A width change instead pre-renders the payload at its own width and embeds the result below the annotation,
because the document renderer applies one width to the whole document.
An embedded multiline payload keeps its relative indentation and its empty lines free of trailing whitespace.
A verbatim payload is emitted as source text rather than through the document algebra,
so it is the explicit way to opt a region out of canonical formatting.

#### Components and Policy

`PrettyFormatter` coordinates three reusable components over one arena.
`GrammarContext` classifies rendered terms and patterns against parser requirements.
`Punning` recognizes concise field payloads.
The boundary compositor combines anchored documents with retained layout.

Semantic preservation, comment retention, punning, and convergence are laws rather than options.
Printer policy controls the positive `IndentWidth`, target line width, how much recorded layout is retained,
treatment of transparent parentheses, and the explicit `verbatim` escape hatch.
The layout policy has three tiers: `Preserve` keeps every observed break and blank line,
`BlankLinesOnly` keeps blank lines while the width decides single breaks,
and `Ignore` leaves every optional break to the width decision.
Policy comes from `@[format(...)]` directives in the source rather than frontend settings,
so `zydeco fmt` and Cajun share one behavior and must not introduce independent formatting rules.
`zydeco fmt --check` remains the only frontend option: it reports files that would change without writing them.

#### Verification and Extension

The regression matrix covers each layout family in compact, source-broken, and width-broken forms.
Corpus checks reparse formatted source, compare desugared structure, preserve comments, and require idempotence.
The current source corpus and runner boundaries belong to [C16](#source-fixtures-and-runtime-oracles);
[CONTRIBUTING](../../CONTRIBUTING.md) owns formatting commands.

When syntax is added, first identify each child's parser requirement,
then choose its canonical spelling and an existing layout family for each boundary.
Add a new primitive only when those choices require a new invariant.
Comments use entity anchors, typed arm and delimiter boundaries, and exclusion ranges.

#### Elaborated Type Rendering

The [scoped formatter](../../lang/surface/src/scoped/fmt.rs) is for debug output.
The [statics formatter](../../lang/statics/src/fmt.rs) renders elaborated types for hovers,
diagnostics, and IR inspection.
It uses precedence-aware parentheses, declaration hints for abstract witnesses, and source-shaped manifest entries.
Synthesized projection types cannot generally be recovered by slicing source text.
It has no retained trivia; width chooses breaks at its declared document gaps.

The textual and statics renderers keep separate grammar contexts and construct their own documents.
Typed quantifier heads can remain unbreakable and exceed the width budget;
adjacent quantifiers are not generally folded.
Readable primitive names and witness hints need not form reparseable annotations.
Interactive type links require semantic anchors from the renderer, never reparsing its text.
Remaining questions concern [round-trip source generation](../ideas/typed-source-generation.md)
and [telescope layouts](../ideas/type-rendering-layout.md).

### Completion and Documentation

[Completion queries](../../lang/session/src/source/query/completion.rs) track the exact cursor hole
through recovery, resolution, and checking.
The session recovers only the current root; imports and companions remain strict.
Assembly remaps the exact cursor node into merged syntax, and desugaring provenance connects it to resolution.
A resolver `ScopeSnapshot` enumerates names through ordinary lookup, preserving shadowing and binder introduction depth.
General names require the original parser expectations to admit a term hole;
a recovered hole in a field-name position is insufficient.
Resolution can retain scope after recoverable unbound references;
a fatal phase failure cannot invent an unvisited cursor's environment.
Resolver scope supplies candidates; incoming analytic annotations supply expectations.
A synthesized placeholder is not an expectation. Multiple visits retain all analytic constraints.
Compatibility is `Equal`, `Unknown`, or `Mismatch`: only proven mismatches are omitted,
and candidates requiring inference remain unknown.
Probes reuse `Lub`, following solved fills but deferring unsolved ones without solution or scope writes.
`Equal` satisfies every analytic annotation without inference; one rigid rejection establishes `Mismatch`,
even when another part of the classifier is unresolved.
Other unavailable or inconclusive evidence remains `Unknown`.
Disposable compatibility checks must not mutate source facts or depend on enumeration order.
The session orders candidates by prefix, compatibility, scope proximity,
and deterministic label order; Cajun projects that order into protocol items.
Missing annotations remain optional, and classifier fit does not prove that every term-level constraint
or unrelated error is satisfied by insertion.
The completion query retains its latest result without installing repaired source or replacing strict analysis;
Cajun checks the revision of its disposable session snapshot.

Source-path completion recognizes the metadata catalog's `Source` argument kind.
It merges filesystem entries and overlays relative to the importing source,
excluding direct self-imports and symlink aliases.
Directories sort before conventional source files.
The edit replaces the current path component with source-language escapes,
preserving quotes, the written directory prefix, and following components.
Numbered imports, unrelated strings, comments, and unfinished escape sequences receive no path suggestions.
[Recovering parsing](#recovering-parsing) owns syntax recovery and cursor identity;
the [completion proposal](../proposals/completion.md) retains further candidate families and stale-state policy.

#### Documentation Workflow

[Source documentation](language.md#source-documentation) specifies `@[doc]` attachments and semantic links.
Cajun uses the first top-level prose paragraph as a hover summary and shows full prose in name completion.
Semantic links and invalid-link diagnostics use standard LSP; the persistent panel is a VS Code client feature.

##### Reading Documentation in the Editor

Place the cursor on a name or expression and run **Zydeco: Show Documentation**
from the VS Code command palette or editor context menu.
The panel displays complete Markdown and offers these actions:

| Action | Behavior |
| --- | --- |
| Follow and Pin | Follow the cursor, or retain the selected source occurrence. Edits before a pin move its position; overlapping edits invalidate it and require a new selection. |
| Type views | Switch between the type at the cursor and the documented declaration when both are available. |
| Source and Back | Open the explanation's source and navigate through related documentation. Entries in edited files are discarded rather than reusing old positions. |
| Check and Open scratch | Verify an opted-in example or open a complete editable copy in a temporary file. Ordinary hover, completion, and diagnostics work on the copy; Save As keeps it. |

The panel requires Cajun's version 1 documentation capability.
It refreshes after source changes and discards results from older revisions, including example checks.
During incomplete edits, available semantic facts still provide documentation;
where resolution fails, surviving comments can be read without an inferred type.
The panel links to the selected documentation origin; separate contract/implementation tabs, clickable subterms
of panel types, signature help, and expected-type search remain [proposed features](../proposals/documentation.md).

##### Building a Project Reference

Build the CLI with `cargo build --bin zydeco` or install it with `cargo install --path cli`.
The [counter example](../examples/documentation/counter.zy) has a companion interface and a guide.
From the repository root:

```sh
zydeco doc show docs/examples/documentation/counter.zy value
zydeco doc search docs/examples/documentation/counter.zy counter
zydeco doc build docs/examples/documentation/counter.zy \
  --guide docs/examples/documentation/guide.md --output /tmp/counter-docs.html
zydeco doc check docs/examples/documentation/counter.zy \
  --guide docs/examples/documentation/guide.md
```

Use `target/debug/zydeco` if you built without installing.
`doc show` defaults to the entry subject, `.`.
Field paths use `/`; `()` selects a function or computation's result interface,
so a selector such as `'()/value'` describes a field of a generic result.
These are documentation paths, not executable Zydeco expressions.
Search covers public names and prose; HTML search also includes the selected guides.
The publication and verification contracts below define what each command includes and checks.

#### Documentation Subjects and Provenance

[Documentation analysis](../../lang/session/src/source/documentation.rs) connects authored attachment,
typed subject, origin, contract, and use context.
An authored origin identifies the prose, source location, and lexical scope of its links.
A use context identifies the selected occurrence, exposed interface, current classifier, and established instantiation.
One explanation can appear with several instantiated signatures; same-spelled fields
in unrelated interfaces have no such relationship.
Two existential openings can share documentation while retaining distinct witnesses.
Documentation identity participates in neither type equality nor runtime representation.

Resolved variables, simple aliases, imports, and transparent wrappers follow recorded origin edges.
Field projections and projection patterns use the owning interface and resolved member provenance,
which survives substitution and opening of a packed value.
A field label or similar printed type is insufficient evidence.
Arbitrary computations constructing a packed value do not establish origin relationships for every value they use.
When those relationships are unavailable, views show the known type and directly attached prose.

Direct prose appears as local context before inherited content, without rewriting the provider's explanation.
An explicit annotation selects documented interface prose in preference to its implementation;
at a binding with an established implementation edge, a docless annotation can fall back along that edge.
A projected field with an explicit contract instead follows that contract's member provenance:
the implementation relationship needed to recover a docless field's body is not generally available.
Imported type terms and `.zyi` companions establish the same kind of public contract;
paired filenames alone cannot associate every nested field with an implementation definition.
Contract/implementation navigation
and broader field fallback remain [proposed extensions](../proposals/documentation.md).

[Semantic links](language.md#semantic-documentation-links) retain typed lexical
or member targets and exact authored ranges for diagnostics and navigation.
An unresolved lexical name does not become an ownerless field search.
Frontends render compiler-resolved targets without reconstructing identities from display text.
This shared semantic index supports generated references, search, hover, and the editor panel;
an editor-owned index or a Markdown-only extractor could not establish the same typed relationships.

Attachments can remain useful after an unrelated failure when the actual annotation and payload survive recovery.
Recovery cannot attach detached prose to a guessed subject, a same-spelled field, or a coincident old source range.
Transient subject IDs and editor actions are checked against their source revision.
[Semantic regressions](../../lang/session/src/source/documentation/semantic/tests.rs) pair preserved origins
with shadowing, unrelated fields, contract boundaries, recovery, and stale IDs.

#### Documentation Publication and Verification

The public graph follows the selected entry's exposed classifier, named fields,
and generic result interfaces without executing arbitrary runtime terms.
Recursive paths link back to established subjects.
Exposed signatures preserve abstraction; local inspection of a private binding does not publish it.
Published anchors start with `api`, use UTF-8 hexadecimal `-f-...` field segments
and `-result` result segments, and reject duplicate public paths.
They contain no arena IDs or source offsets.
Formatting-only changes therefore preserve named routes.
Stable anonymous anchors, internal publication, and release-version URLs remain future work.

Only explicitly supplied `--guide` pages join the reference's search and link index.
Repeat the flag to include more pages; guide filenames must have distinct stems.
Guides use `[value](zydeco:member:./value)` to refer to the selected public root.
They have no implicit lexical source scope.

The output is one self-contained HTML file with local search, source links, stable public anchors,
the compiler version, and SHA3-256 hashes of exact source and guide inputs.
Dependency documentation describes the sources actually analyzed and remains usable offline.
Raw author HTML is rendered as text and images are represented by their alt text.
An older published build remains a distinct snapshot.
`doc build` validates semantic links but does not check or execute examples, and the page claims no verification result.
Use `doc check` separately in CI.

##### Verifying Documentation Examples

A plain `zydeco` fence displays code.
Add `check` to require a complete source term that checks successfully.
For a guide in this reference directory:

````markdown
```zydeco check
let counter = @(import("../examples/documentation/counter.zy")) in counter/value
```
````

An expected-rejection example declares both a compiler diagnostic code and a position:

````markdown
```zydeco reject=tyck.missing-named-field at=1:15
(#value = 42)/missing
```
````

Positions are one-based line and UTF-16 column within the displayed example.
Every reported type diagnostic must match the expected code and contain that position.
A successful program, a different error, a missing import, a compiler crash,
or a timeout cannot satisfy an expected rejection.

[Example checking](../../lang/session/src/source/documentation/examples.rs) constructs isolated source requests
with paths relative to the owning document.
Verified fences must be top-level, unindented Markdown blocks containing complete source with explicit imports.
There is no implicit surrounding lexical context or hidden setup.
Imported inputs participate in verification identity.
**Open scratch** rewrites compiler-recognized file imports to absolute paths so the temporary copy keeps its context;
numbered REPL imports cannot be copied this way.
Scratch edits have their own source identity and do not verify the original published text.

`doc check` validates links and opted-in examples from the analyzed dependency graph and selected guides.
It fails for invalid options or failed checks and reports diagnostic locations in the authored comments or guide.
The panel checks a selected example with the editor's current overlays.
Both use the same isolated compiler worker with a 30-second timeout, 64 KiB example limit,
16 MiB request limit, and 1 MiB response limit.
These are time and data limits, not an operating-system memory sandbox.
The checker does not execute examples and rejects `run` fences.
[Example regressions](../../lang/session/src/source/documentation/examples/tests.rs) check isolated outcomes
and locations.
Runtime examples and composed setup remain in the [documentation proposal](../proposals/documentation.md).

### Interactive Engine

The [TUI engine](../../tui/src/engine.rs) stores complete submissions as numbered source inputs in a session overlay.
Each recorded number identifies immutable source text; later inputs compose it explicitly with `@(import(N))`.
There is no hidden lexical environment.
Re-importing a computation can repeat effects: history stores source, not memoized runtime results.
Clearing the transcript leaves those importable sources available.
A rejected submission returns to the editor at the same number; retry replaces that unrecorded attempt.

For ordinary input, a generated observation root declares its own Builtin parameter around the imported complete input.
The engine analyzes the direct import first, then tries a `ret` wrapper if the first analysis rejects,
allowing values and returning computations without requiring a user wrapper.
If both analyses reject, it reports the original direct diagnostic.
Static observations use analysis facts; runtime observations link the accepted root with captured output and errors.
Explicit execution goes through the executable entry boundary.

Root command metadata is parsed into typed commands before ordinary submission handling.
Help and quit do not consume a source number; unsupported command arguments are rejected,
and unrecognized metadata remains ordinary language syntax.
[CONTRIBUTING](../../CONTRIBUTING.md#use-the-interactive-repl) owns commands, keys, and retry interaction.
Deferred interaction work covers [history persistence and replay](../ideas/repl-history-replay.md)
and [history pruning](../ideas/repl-history-pruning.md).

## C16. Validation, Debugging, and Extending the Implementation

Choose the first boundary that can state the property being changed,
then assert the observable consequence at the necessary downstream boundary.
A correct interpreter result does not prove IR ownership or native root preservation;
an IR snapshot alone does not prove effect order.

| Change | Focused evidence |
| --- | --- |
| Grammar or formatting | Surface parser agreement, recovery contracts, semantic/comment preservation, idempotence |
| Typing, scope, or a source rejection | [Source fixtures](../../lang/tests/cases) or the relevant Rust integration target, with exact diagnostic codes |
| Typed-arena invariant | A seeded mutation and a clean counterpart in [type-lint tests](../../lang/tests/tests/tyck_lint.rs) |
| SPS reduction or representation | High/low invariant checks, blocked-rewrite cases, and runtime observations |
| Native layout or allocation | Preparation/model unit tests and targeted native-model or GC integration tests |
| Host or FFI operation | Signature rejection tests, adapter tests, declared I/O, and target-specific execution |
| Tooling | Current and failed revisions, exact cursor/site identity, and stale-result rejection |

### Source Fixtures and Runtime Oracles

The [case harness](../../lang/tests/tests/cases.rs) uses `libtest-mimic` to discover one trial per `.zy` file.
Files are source fragments wrapped with the same prelude as `SourceCase`.
Leading ordinary comments select stage, prelude, and expectation;
the [case guide](../../lang/tests/cases/README.md) is the directive reference.
Unknown, duplicated, or inapplicable directives fail a trial instead of weakening its expectation.
Diagnostic spellings are parsed beside their stable compiler codes and round-trip through that shared catalog.
Fixtures also enter the parser/formatter corpus,
so a source edit needs neither manual registration nor a Rust recompilation.

Topic paths name the regression; directives name its phase and expected outcome.
This avoids renaming tests when their stage changes and avoids coupling semantic assertions to rendered error prose.
Using the trial runner directly keeps naming and directive validation explicit.
Rendered-diagnostic snapshots can be added for a distinct presentation contract.
Multi-file imports, arena mutation, desugared-variant assertions, emitted-code structure,
process arguments, and I/O remain Rust tests or whole-program fixtures
because a single-fragment expectation cannot express those relationships.

[The shared harness](../../lang/tests/src/lib.rs) supplies check, runtime, and end-to-end registrations.
Whole-program tests declare their backends, input, arguments, expected output,
and exit behavior; undeclared stdin is EOF and output is captured.
Native FFI and toolchain-dependent cases can require explicit opt-in.
A regression must preserve both the accepted use and the rejected or non-optimizable counterpart relevant
to its invariant.

### Following a Change

For a source feature, follow textual syntax and metadata through bitter/scoped forms, typing,
static elaboration, interpreter linking, high lowering, and every consuming backend.
For an optimization, identify its required input invariant and every representation or root map it changes.
For a primitive, update the role catalog, checked signature, materializers, interpreter,
native runtime/emitter, Wasm emitters/host, and conformance cases.
For foreign interfaces or package resolution,
follow the [shared compilation-unit design](#compilation-unit-preparation-and-artifacts).
Prefer existing domain types and shared declarations at each common boundary.

Diagnostic investigation starts from the saved source site and the earliest representation whose invariant fails.
The textual, scoped, statics, SPS, and assembly debug printers expose successive views;
use the relevant phase's verifier before interpreting a downstream crash as a source-language failure.
Keep commands in [CONTRIBUTING](../../CONTRIBUTING.md#run-tests).
Automated work reserves the full workspace suite for an explicit request.

Performance evidence records the revision, workload, build profiles, host/target,
measured quantity, and default or experimental representation.
Allocation counts, reserved capacity, peak RSS, and elapsed time answer different questions.
[Runtime evaluations](../evaluations/2026-09-08-cbpv-runtime/README.md) retain historical comparisons;
repeat them before selecting a new default or claiming a current improvement.
Reference examples and local links are checked separately from runtime tests,
and documentation drift belongs in [the todo records](../todos/README.md).

## Navigation Indexes

| Source mechanism | Typed boundary | Execution route |
| --- | --- | --- |
| A named field | Resolved structural route and typed payload | Tuple access/patterns; names erase |
| An existential witness | Scoped abstract identity or manifest equation | Static evidence erases; payload remains |
| `val` or a view | `ValPi` and static application/pattern | C6 removes it before executable lowering |
| `@[typeof]` | Reused classifier identity | Operand has no runtime query node |
| A thunk | Checked computation capture | Interpreter closure or SPS closure conversion |
| `ret` and `do` | Computation and its consuming continuation | SPS return/binding, then target control representation |
| A literal pattern | Typed integer and opaque coverage observation | Equality branch during high lowering |
| A monadic block | Retained checked payload and translated algebra structure | Ordinary residualization and backend pipeline |
| A foreign thunk | Validated `ForeignSignature` | libffi or native returning-call bridge |

| Invariant | Establishing/checking boundary | Principal consumers |
| --- | --- | --- |
| Hygienic source and binder identities | C2–C4 | Checking and editor provenance |
| Closed inference and stable normalized types | C5 | C6, linking, and all lowering |
| Representable executable residual | C6 and executable selection | C7–C14 |
| Lexical single occurrence and branch joins | High/low validators, C8–C9 | Normalization, capture analysis, local unboxing |
| Native activation and initialization contracts | C11 preparation | Emitter, frame model, collector |
| Tagged words and shared record layouts | C12 model and C13 ABI | Host adapters, generated code, runtime |
| Complete mutable roots at collection | C11 maps and C12 publication | Native moving collector |
| Revision-correct semantic identity | C3 and C15 | Editor results and interactive actions |

The phase table in C1 is the entry-point index; chapter links lead to the owning modules and focused regressions.
