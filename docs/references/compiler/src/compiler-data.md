# C2. Compiler Data, Identities, Arenas, and Source Provenance

An ID's Rust type identifies its node category.
Surface and stack-IR identifiers carry an opaque key space and raw index, while static syntax identifiers (`AbstId`,
`FillId`, `KindId`, `TypeId`, `KPatId`, `TPatId`, `VPatId`, `ValueId`, `CompuId`,
`DataId`, and `CoDataId`) use four-byte dense [`DenseId`](../../../../lang/foundation/src/dense.rs) indices
into [`StaticsArena`](../../../../lang/statics/src/arena.rs),
and residual [`CoreArena`](../../../../lang/ir/src/syntax.rs) nodes use their own module-local
four-byte dense indices (`VarId`, `VPatId`, `ValueId`, `CompuId`, and `DataId`).
Identifier names (`VarName`, `CtorName`, `DtorName`, `FieldName`) are backed
by global [`Sym`](../../../../lang/foundation/src/symbol.rs) handles in `SymbolInterner`,
reducing clones and equality comparisons to integer copies.
[The arena utilities](../../../../lang/foundation/src/arena.rs) separate issuing keyed IDs from owning
and accessing their storage.

| Mechanism | Invariant |
| --- | --- |
| `IdAllocator<Scope>` | One non-cloneable sequential issuer owns a fresh key space and cursor. |
| `KeySpaceRegistry<Site>` | Complete site equality reuses an identity; distinct sites and independent registries receive fresh key spaces. |
| Dense statics and generated definitions | `StaticsArena` allocates four-byte `DenseId` indices in `DenseArena`, while `DefGenAllocator` derives reproducible `DefId`s from `KeySpaceRegistry<su::TermId>`. |
| `TypeSummary` | Each node in `TypeArena` stores a one-byte summary bitmask propagating reachable fills, variables, abstract types, projections, beta/seal heads, and primitives. |
| `Allocates<Id>` | A capability declares which categories an operation may issue. |
| `ArenaSchema<Id>` | An owning representation declares the item stored under an ID. |
| Dense storage | The backing arena supplies the raw index and rejects another arena's key space or index type. |
| Sparse, paged, and indexed storage | Producer-issued IDs retain their identity when storage changes. |
| Associative side tables | Each write explicitly inserts, replaces, upserts, or ensures an association. |
| `FrozenArena<A>` | Consumers receive storage without the builder's indexed-mutation capability. |

Sequential issuers normally live on a parser, resolver, or lowerer rather than its published arena.
SPS retains its definition issuer until consuming closure conversion moves it into the low administrative arena;
SPSLow syntax has a separate issuer and never reuses SPS syntax IDs.
Statics allocates dense nodes directly inside `StaticsArena` during a single checker run,
using `DefGenAllocator` to give generated `DefId`s replay-stable identities across session eviction.
Rechecking the same source entity in a distinct occurrence allocates fresh dense nodes
without overwriting earlier elaborations.

Session-owned `KeySpaceRegistry<su::TermId>` instances survive coarse query eviction
and remain available until the last session owner is dropped.
`KeySpaceRegistry<Site>` interns complete keys: hashing accelerates lookup, full key equality establishes identity,
and an atomic mutex guarantees disjointness across independent registries and sequential allocators.
Within a site, callers assign distinct local slots (`DefGenAllocator`) up to `u32::MAX`,
failing cleanly on exhaustion rather than wrapping.

Registry mappings survive query eviction and remain available until the last registry owner is dropped.
They retain site identities rather than arena payloads, with memory proportional to the number of registered sites.
Published arenas keep their numeric IDs without retaining the registry, and dropping a registry never recycles its IDs.
Replay within the same registry preserves identity, while numeric values depend on first-registration order;
they are not portable identities across independent sessions or processes.

Provenance records the actual relation between representations.
Surface-to-typed mappings are many-to-many because transparent syntax and repeated checking can share
or duplicate results; one typed node may produce several IR nodes.
Parsed entities use the tagged `EntityId` category rather than casts between definition, pattern, and term IDs.
Generated definitions and phase-local provenance belong to the phase that creates them.

[Span](../../../../lang/foundation/src/span.rs) stores two `BytePos(u32)` endpoints: eight bytes per span.
Compact byte positions avoid retaining a path, line, and column at every syntax node.
A `FileMap` owns a file's path, retained text, global base, and line-start index;
`SourceMap` orders files by their global base.
File-local parsing starts at byte zero.
Merging rebases spans into one address space beginning at one, reserving the zero span for a dummy location.
The [merged surface span arena](../../../../lang/surface/src/textual/span.rs) owns the source map;
a cached parse template retains its local file map until assembly.

Location rendering finds the file and line by binary search and derives character columns from retained text.
It performs this work on demand, including after rejection; a `ResolveFailure` retains the error and span arena.
Editor conversion to UTF-16 belongs at the protocol boundary and does not change compiler byte offsets.
Before comparing a global span with an editor's file-local cursor or range, localize it through the owning map.
File attribution alone cannot validate this conversion: source-location tests must also check hover
and token ranges in non-ASCII text and in files beyond the first merged source.

Allocation and provenance changes should exercise arena tests and source-location tests,
including repeated checking and the distinction between local and merged spans.

## Choosing and Composing Traversals

Repeated structural recursion belongs beside the representation that defines its children.
A visitor observes that structure and accumulates facts; a folder transforms children and rebuilds their parent.
Clients supply semantic rules, while the shared operation describes how to reach or reconstruct ordinary children.
Phase-specific builders retain authority over allocation and provenance.

The implemented interfaces serve different questions and therefore expose different views:

| Input and purpose | Shared operation | Identity and scheduling contract |
| --- | --- | --- |
| File directives and documentation | [Source scan and analyzers](sessions-and-queries.md#shared-source-analysis) | File-local textual identities; analyzer interests select allocated terms, reachable terms, and text blocks |
| Bitter or scoped syntax reconstruction | [Owned surface folder](#surface-structural-rebuilding) | Child IDs flow through client hooks; the folder supplies copying, reference, scope, and rejection policies |
| Desugaring and lexical resolution | [Desugaring folders](parsing-and-resolution.md#desugaring-folders) and [resolution events](parsing-and-resolution.md#name-resolution) | Semantic handlers own telescope and scope transitions; builders and event consumers own their respective results |
| Resolved syntax analysis | [Scoped traversal](#scoped-structural-traversal) | Borrowed nodes; choose unique identities or path occurrences; cycles reject the traversal |
| Raw inferred classifiers | [Classifier folders](checking-and-inference.md#classifier-folders) | Owned child reconstruction; preserve unchanged IDs; clients select substitution environments and reductions |
| Raw checked terms and static evidence | [`CheckedNode` children](elaboration-and-validation.md#nominal-algebra-construction) | Include classifier, pattern, and static-argument edges; clients select declaration expansion and use `TypeFolder` for classifiers |
| Executable residual terms | [Residual runtime traversal](elaboration-and-validation.md#residual-runtime-traversal) | Borrowed `&CoreArena` nodes from `CoreProgram::root()` and `retained_holes`; visit each identity once |
| Residual `CoreProgram` to lexical SPS | [Residual lowering folder](sps.md#residual-lowering-folder) | Explicit reconstruction frames; inherited continuation stacks; fresh stack-IR syntax for every input occurrence |
| Lexical SPS analysis | [SPS traversal and analyzers](sps.md#sps-analysis-traversal) | Observe every incoming edge for ownership, but expand children and compute exit summaries once |
| Lexical SPS normalization | [Normalization reconstruction](sps/normalization-and-demand.md#normalization-reconstruction) | Explicit frames retain forward producer scopes and resume rebuilding with backward consumer demands |
| Lexical SPS to first-order SPSLow | [Closure conversion folders](closure-conversion.md#closure-conversion-folders) | Occurrence-local renaming and ordered captures; fresh syntax with preserved allocation and publication order |
| First-order SPSLow analysis | [SPSLow traversal and analyzers](closure-conversion.md#spslow-traversal-and-analyzers) | Observe ownership edges and compute variable summaries in one scan; metadata adds no executable occurrences |

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
The [diagnostic contract](tooling.md#diagnostic-collection) distinguishes recoverable facts from a valid phase product.

Successive transformations require their own scheduling argument before fusion.
For example, folding literal addition and then incrementing every literal transforms `Add(Lit(1), Lit(2))`
into `Lit(4)`.
Running both rewrites at each node in postorder instead produces `Lit(6)`
because the parent receives incremented children.
A common input/output IR does not establish equivalence.
[Explicit pass composition](architecture.md#compiler-pass-composition) preserves those stage boundaries;
the [SPS demand schedule](sps/normalization-and-demand.md#consumer-demands) similarly remains a semantic dependency
of normalization.

Traversal tests compare separate and composed facts, diagnostics, sharing, and provenance.
Visit and allocation counts establish the work removed by a migration; compilation-time
or memory claims additionally require measurements that include callback work and retained summaries.
[Further extensions](../../../proposals/traversals.md) remain proposals until a concrete client justifies them.

## Resumable Folder Execution

A folder that rebuilds a parent after visiting its children must retain the unfinished parent.
Front-end passes, statics judgments, normalizers, substitutions, unifiers, elaborators,
formatters, and code emitters run as natural recursive Rust functions guarded
by [`zydeco_foundation::stack::maybe_grow`](../../../../lang/foundation/src/stack.rs),
which grows heap stack segments dynamically past the 64 KiB red zone
without requiring a workspace `RUST_MIN_STACK` override or custom large worker stacks.
For middle-end and backend transformations,
the shared [folder execution interface](../../../../lang/foundation/src/fold.rs) stores continuations
in an explicit vector, so input depth does not consume the native stack.
It supports mutable compiler state without retaining a mutable borrow of the folder across child execution.

`Folder` defines `Input`, `Output`, and an owned `Frame` for the unfinished parent.
`enter(input)` begins an operation; `resume(frame, child)` continues it with a completed child result.
Both return `Step::Call { input, frame }`, `Step::TailCall(input)`, or `Step::Return(output)`.
A leaf returns directly without constructing a continuation frame.
A tail call transfers to another operation without retaining the current one;
the driver executes these transfers in a loop while preserving the caller's unfinished parent.
A parent may select its next child using an earlier child's result and the current compiler state.
Frames contain owned local state, arena IDs, or references to stable external inputs;
they cannot borrow the mutable folder itself.

The driver, `Explicit::run(&mut folder, root)`, stores unfinished parents in a vector and executes through a loop.
Residual SPS lowering (`CoreProgram -> FragmentProgram`), Builtin stack-IR frame materialization,
SPS normalization, closure conversion, SPSLow traversal, and CPS assembly lowering all run through it.
The driver introduces no boxed callbacks or individual heap allocation for each continuation.
Frame payloads and outputs may allocate according to the folder's representation.

The folder owns child order, scope transitions, allocation, provenance, sharing, caching, and recovery.
An error-valued output returns to its parent through the same protocol as a successful result;
the parent decides whether independent siblings remain visitable.
The driver neither short-circuits errors nor implicitly deduplicates shared inputs.
A folder may retain separate typed result stacks when its domain operations produce different categories of syntax.
Its `Output = ()` then signals completion, and each reconstruction frame consumes exactly its own completed children.
The driver still owns the suspended calls; these result stacks retain domain data rather than pending work.
The driver bounds only its own call depth: recursive semantic helpers and destruction
of nested frame payloads retain their separate stack requirements.

Driver regressions check dependent child selection, repeated occurrences, event order, and independent rejection.
A 100,000-level non-tail fixture executes on a 128 KiB stack,
and a separate 100,000-step tail-call fixture uses the same small stack and checks that the original parent resumes.
The [Builtin packed value folder](../../../../lang/stackir/src/high/lower/builtin.rs) is a production client:
its product frame accumulates fields in input order and transfers the completed vector into the output node.
Its tests check product order and materialization, retain the empty-product rejection,
and construct and destroy 16,384 nested products on a 512 KiB stack.
These checks establish behavior and depth robustness, without claiming a compilation-time speedup.

The [SPS pattern folder](../../../../lang/stackir/src/high/normalize/pattern.rs) copies one owned pattern layer
and replaces each child slot as its result returns.
Constructor payloads, alias components, and product fields retain their structural order.
The completed layer moves into the target arena with its original definition IDs,
source site, product layout, and protocol evidence.
This avoids a separate result stack and transfers child vectors directly into rebuilt nodes.
Pattern depth uses the driver independently of the normalizer's consumer schedule.
Regressions cover these invariants and empty field vectors with nonzero physical arity;
the normalization depth regression retains 8,192 nested alias patterns on a 512 KiB stack.

## Surface Structural Rebuilding

Bitter and scoped syntax share `Pattern` and `Term<Ref>`, with source names
or resolved definition IDs in reference positions.
Their [owned rebuilding operations](../../../../lang/surface/src/fold.rs) describe immediate children once
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

The [bitter builder](../../../../lang/surface/src/bitter/alloc.rs) owns one sequential ID issuer and its `BitterArena`.
Every `Alloc` operation stores a node and its textual origin together; consuming `finish` publishes a `FrozenArena`.
Desugaring owns a builder alongside its textual inputs and source-term memo table.
[FreshenFolder](../../../../lang/surface/src/bitter/freshen.rs) uses the same builder
to copy an existing bitter fragment: every reachable occurrence receives fresh IDs,
including definition IDs, while retaining the original textual origins.
Source names remain unchanged for subsequent lexical resolution.
Source and signature boundary payloads are copied per occurrence, even when the input shares a provider.
There is no copy memoization, and source identities in `partial_binders` remain valid through the retained origins.
Freshening uses recursive descent and requires acyclic input; it does not provide the scoped visitor's explicit-stack
or cycle-reporting guarantees.

The folder is used for binder copies in annotated abstractions, generated binding classifiers,
and recursive binding sugar.
Its regressions check distinct resolved binders, retained provenance, copattern order,
shared annotation copying, and sealed payloads.

## Scoped Structural Traversal

Resolved syntax can share source roots, so structural analysis operates on a graph of arena entities.
[Traversal](../../../../lang/surface/src/scoped/traverse.rs) owns the exhaustive child enumeration
and depth-first schedule; independent `Visitor` implementations receive borrowed `Node` values at entry and exit.
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
This is an analysis schedule; [resolution](parsing-and-resolution.md) owns lexical scope transitions.

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

[ContextCollector](../../../../lang/surface/src/scoped/context.rs) is the postorder free-variable
and source-boundary analysis.
It temporarily records bound definitions on patterns,
and retains free definitions alongside `SourceBoundary` reachability flags for both terms
and patterns inside `TermContexts`.
These summaries are independent of the incoming environment, so `TermContexts::collect` uses unique-node traversal.
Other analyses can share its traversal through `Together`.

The [traversal regressions](../../../../lang/surface/src/scoped/traverse/tests.rs) cover composed results,
callback order and early breaks, cycles, dependent binders, and deep syntax.
A repeated-import graph with 37 distinct term nodes has 16,381 occurrences;
the unique-node traversal delivers 37 entries and exits.
This checks traversal work, not end-to-end compilation speed.
