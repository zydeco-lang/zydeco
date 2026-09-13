# Reusable folders and traversal composition

This proposal collects the remaining work on reusable compiler traversals.
The implemented scoped visitor and its composition contract live
in the [compiler reference](../references/compiler.md#scoped-structural-traversal).
That implementation is the starting point for the extensions below; this document proposes no change
to source semantics or to the existing ordering of compiler phases.

## Motivation and scope

Adding a syntax constructor currently requires updating many operations that each describe how
to recurse through the same representation.
Bitter cloning, desugaring, and resolution contain their own recursive dispatch; typed substitution,
hole resolution, and normalization repeat much of their rebuilding structure.
This spreads structural knowledge across passes and makes each pass responsible for both traversal and its own rules.

The intended separation is between a representation's structural operations
and the specific visitor or folder using them.
A visitor observes nodes and produces facts.
A folder transforms nodes, rebuilding parents from transformed children.
Independent analyses can compute their results during the same traversal,
while transformations keep the ordering required by their input and output contracts.

The [Rust compiler's foldable/folder interface](https://rustc-dev-guide.rust-lang.org/ty-fold.html)
illustrates the separation between structure and transformation.
Zydeco must additionally account for arena identities, shared source roots, and phase-local provenance.
The design should grow from concrete migrations rather than parameterizing every IR behind one universal node type.

## Representation-owned structure

Extend the surface traversal machinery with a borrowed arena adapter and a rebuilding interface.
Bitter and scoped syntax already share `Pattern` and `Term<Ref>`;
their [debug formatter](../../lang/surface/src/debug.rs) demonstrates adapting the phase-specific arena
and references without duplicating the syntax dispatch.
Reuse that syntax family when the bitter folder is introduced.
Textual syntax and typed syntax need their own structural implementations because their node categories differ.

Keep exhaustive structural matches beside the relevant IR.
A pass should supply local behavior, and only take control of child scheduling when its semantics require it.
Borrow nodes for inspection; use an owned reconstruction when a folder must change their children.
Allocation belongs to a phase builder, following the existing
[identity and provenance contract](../references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance).

The folder interface should distinguish three questions explicitly:

- Which source node is being read, and which representation is being constructed?
- Which inherited environment applies to each child, and which results return from that child?
- When is an existing identity reusable, and when must the builder issue a fresh identity?

This supports ordinary identity-preserving rewrites and intentional freshening
without making their identity policies implicit in a generic memo table.
Prefer an exhaustive hand-written implementation for the first folder.
Consider generating borrowed traversal and rebuilding from one structural declaration only
after two real clients show that their common structure is stable.

## Composition and scheduling

The existing scoped visitor composes independent callbacks
under its [shared traversal contract](../references/compiler.md#scoped-structural-traversal).
Extend this idea to analyses that return results from children: the combined result at a node is a pair,
and each analysis receives its own components of the child results.
Neither component should have to invoke the other or repeat the recursive descent.

Two whole-tree transformations require a stronger argument before fusion.
For example, let one transformation fold literal addition and another increment every literal.
Applied successively to `Add(Lit(1), Lit(2))`, they produce `Lit(4)`.
Applying both transformations at each node in postorder instead produces `Lit(6)`
because the parent sees the already-incremented children.
A same-IR input/output type alone does not justify fusion.
Keep such stages in [explicit pass composition](../references/compiler.md#compiler-pass-composition)
unless a local scheduling contract establishes equivalence.

Observation also needs an explicit choice of input: a folder may expose source nodes,
rebuilt nodes, or allocation events.
A count of source nodes and a count of generated nodes are different analyses.
Define their event boundaries before composing them.

Pruning is a later extension.
A combined visitor cannot silently accept one component's decision to skip a subtree that another component needs.
Initially, group analyses with the same traversal policy.
Introduce independent activity masks or separate traversal groups only when a concrete consumer needs them.
Likewise, preserve diagnostic precedence when combining validators:
interleaving two validators can change which error is reported first.
Accumulating each validator's outcome and selecting errors at the enclosing stage boundary may be necessary.

## Next migration: bitter cloning

Replace [DeepClone](../../lang/surface/src/bitter/clone.rs) with a specific `FreshenFolder` over bitter syntax.
Move the required allocation and origin operations out of their dependency on `Desugarer` and into the bitter builder.
The folder should express copying; the shared structural implementation should express where its children live.

Preserve the existing fresh-definition behavior used when a binder appears in both a term and its generated classifier.
Keep source provenance on all copies. Specify whether each copied source boundary retains a shared provider
or intentionally duplicates it before introducing memoization into cloning.
This must be tested separately from the per-node policy used by read-only analysis.

Migrate all cloning callers and remove the superseded trait in the same change.
Useful regressions include annotated abstractions, recursive binding sugar, copattern spines,
and patterns whose annotations contain source boundaries.

## Desugaring decomposition

Give a `DesugarFolder` a traversal/builder context and separate rule modules for:

| Rule family | Responsibility |
| --- | --- |
| Meta annotations | Interpret recognized annotations and choose whether to preserve, replace, or inspect a payload. |
| Telescopes | Lower parameter sequences, existential layers, and package evidence. |
| Bindings | Expand binding headers, currying, recursive forms, and nominal sealing. |
| CBPV introductions | Construct explicit thunk/return forms and their generated annotations. |

These modules are rule owners within one lowering operation; they need not each run a complete source traversal.
The driver should own ordinary descent, source-term memoization, and publication through the builder.
Keep dependent term and classifier construction together when currying requires them to agree.

Some rules need access to source syntax before their children are lowered.
The `partial` meta annotation records source binders before currying changes their nesting;
an intrinsic annotation validates that its original payload is a hole.
Consecutive existential forms and annotated abstraction bodies also require inspecting a larger source shape.
Provide specific pre-descent handlers for those cases instead of requiring every rule to be a postorder rewrite.

Validate accepted and rejected meta annotation payloads, parameter forms, and package evidence.
Compare generated binder identities, annotations, and error spans as well as whether checking succeeds.

## Resolution decomposition

Keep lexical environments and name rewriting in a `ResolveFolder` with an explicit strict or completion recovery policy.
After lookup, emit a typed resolved-reference event containing the occurrence,
selected definition, and the binding ownership needed for dependency tracking.
Expose scope events at the original authoring sites for completion and documentation captures.

Separate consumers can then construct the reference index, block dependency edges,
completion captures, and documentation scopes.
They consume the established lookup result instead of each resolving the spelling again.
Completion capture is an observation; replacing unbound references with holes is part of the resolver's recovery policy.

Retain the [binding and scheduling sequence](../references/compiler.md#c4-parsing-desugaring-and-name-resolution):
discover mobile candidates, install their binders, resolve references and collect dependencies,
then schedule strongly connected components and elaborate the block.
Make these components independently reviewable without promising one simple depth-first walk for the whole algorithm.
Free-variable summaries must continue to consume the elaborated block structure.

Pattern annotations, dependent pattern sequences, match arms, recursive groups,
and source boundaries require different scope transitions.
The scoped structural visitor's analysis order does not establish those transitions.
A resolver-specific driver must make them explicit, preserving binder identities across scheduling.
Regressions should pair forward references with unbound references, legal recursive definitions
with rejected recursive parameters, and independent source roots with attempted capture from an importer.
Retain the exact-scope and strict/recovering completion tests.

## Typed folders and graph views

Use typed substitution and hole resolution as the first rebuilding clients after the surface folder settles.
The unused `LocalFoldStatics` declaration can then be replaced by the implemented interface.
Retain unchanged node identities where the current operation does, including provenance
and Builtin-role transfer on rebuilt nodes.
Type checking remains a judgment-driven algorithm; only its structural operations are candidates for these folders.

Typed syntax has several relevant graph views: raw inferred nodes,
solved and normalized classifiers, and the residual runtime graph.
The [finalization contract](../references/compiler.md#finalization) establishes
when solutions are stable enough for shared memoization.
The [execution readiness check](../../lang/statics/src/validate/executable.rs) deliberately
follows residual runtime children and excludes eliminated static material.
A single undifferentiated children iterator would erase that distinction.

Context-sensitive analyses need more than a node-ID cache.
For example, [type support collection](../../lang/statics/src/normalize/scope.rs) intersects the admissible scope
of a shared inference hole across its occurrences.
A unique-node walk could miss the more restrictive occurrence.
Use occurrence traversal or a memo key that includes the relevant environment;
discard or invalidate results when the state they depend on changes.
Share caches across multiple roots only within one stable analysis invocation.

Validate substitution under binders, accepted and escaping witnesses, missing solutions,
shared classifier tails, and reuse of unchanged identities.
Pair a hole in reachable runtime syntax with a hole confined to eliminated static code.

## Further applications and review sequence

Source directive extraction is a promising analysis-composition client: the loader currently requests documentation,
imports, Builtin roles, intrinsics, literals, and package information through separate scans.
Preserve each query's reachability and recovery behavior before sharing an inventory,
and retain the loader's diagnostic precedence.
Avoid eagerly computing unrelated expensive queries merely because their traversal can be shared.

High SPS has structural variable collection and validation clients,
but its normalizer propagates producer facts forward and demands backward through the same reconstruction.
Preserve the [consumer-demand dependency](../references/compiler.md#consumer-demands) when extracting its rule modules.
Its [lexical ownership checks](../../lang/stackir/src/high/check.rs) also reject repeated syntax ownership;
deduplicating visits must never hide that rejection.

The remaining work fits one migration sequence:

1. Generalize the surface structural implementation as required by `FreshenFolder`, migrate bitter cloning,
   and remove its old recursive path.
2. Separate desugaring's rule families around that builder and traversal boundary.
3. Extract resolution events and their consumers while retaining block discovery and scheduling stages.
4. Introduce typed graph views and folders for substitution and finalization.
5. Apply the established interfaces to source inventories and SPS analyses where they simplify concrete callers.

For each migration, compare separate and composed results and retain visit-count regressions on shared graphs.
Measure allocations and representative compilation time before making broader performance claims.
Run focused crate and source-case tests; use the full workspace suite only when explicitly requested.
Move each implemented, approved contract into its owning reference section and remove its settled design text here.

The remaining choices are the precise cross-phase folder interface, whether structural code generation pays for itself,
which consumers need independent pruning, and which measured workloads justify shared analysis inventories.
Resolve each at its first concrete client rather than committing the entire compiler to those choices in advance.
