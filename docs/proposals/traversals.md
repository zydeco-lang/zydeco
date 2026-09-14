# Reusable folders and traversal composition

This proposal collects the remaining work on reusable compiler traversals.
The surface migrations are implemented; this document retains extensions that still need concrete clients.
The implemented [scoped visitor](../references/compiler.md#scoped-structural-traversal)
and [surface rebuilding and freshening](../references/compiler.md#surface-structural-rebuilding) contracts live
in the compiler reference.
Those implementations are the starting point for the extensions below.
The migrations preserve language constructs and compiler phase ordering.
The agreed diagnostic direction is to report multiple errors in every pass; collection order is sufficient initially,
with presentation customization left for later.

## Motivation and scope

Adding a syntax constructor currently requires updating many operations that each describe how
to recurse through the same representation.
The implemented classifier folders remove repeated rebuilding from substitution and finalization.
Remaining traversal clients should be migrated when their shared structure simplifies the code.

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

Extend the surface traversal machinery with a borrowed arena adapter as further clients need it.
Bitter and scoped syntax already share `Pattern` and `Term<Ref>`;
their [debug formatter](../../lang/surface/src/debug.rs) demonstrates adapting the phase-specific arena
and references without duplicating the syntax dispatch.
The [owned folder](../../lang/surface/src/fold.rs) now rebuilds that shared syntax family.
Textual syntax and typed syntax need their own structural implementations because their node categories differ.

Keep exhaustive structural matches beside the relevant IR.
A pass should supply local behavior, and only take control of child scheduling when its semantics require it.
Borrow nodes for inspection; use an owned reconstruction when a folder must change their children.
Allocation belongs to a phase builder, following the existing
[identity and provenance contract](../references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance).

Extensions of the folder interface should distinguish three questions explicitly:

- Which source node is being read, and which representation is being constructed?
- Which inherited environment applies to each child, and which results return from that child?
- When is an existing identity reusable, and when must the builder issue a fresh identity?

This supports ordinary identity-preserving rewrites and intentional freshening
without making their identity policies implicit in a generic memo table.
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
Validators accumulate diagnostics without preventing independent analyses from running.
Their composition combines facts and diagnostic collections;
the [recovery contract](#diagnostic-collection-and-recovery) determines which work can continue after rejection.

## Diagnostic collection and recovery

The implemented [diagnostic collection contract](../references/compiler.md#diagnostic-collection) covers parsing,
source analyzers, desugaring, and resolution, including complete frontend reporting and explicit recovery boundaries.
The agreed direction extends to every pass: report independent errors justified by available input,
and preserve their complete collection through the session and frontends.
This does not require executing a dependent pass on an invalid intermediate representation.

Source graph loading now collects independent imports, companion signatures, and dependency cycles under the
[provider recovery contract](../references/compiler.md#c3-source-loading-sessions-queries-and-memory-retention).
Inspect checker and later validation loops for early exits over independent work.
Reuse their domain diagnostic collections and suppress consequences of an unavailable prerequisite.

Recovery within one malformed directive remains local to its decoder.
A missing package name prevents duplicate-name checks for that site but does not prevent validating other sites.
Extending recovery through ambiguous scopes or into later intermediate representations requires a specific contract
for the missing information; it should follow a demonstrated tooling or diagnostic need.

## Migration order and acceptance criteria

The implemented surface boundaries have one canonical home each:
[structural rebuilding](../references/compiler.md#surface-structural-rebuilding),
[desugaring folders](../references/compiler.md#desugaring-folders),
[shared source analysis](../references/compiler.md#shared-source-analysis),
and [resolution events and dependencies](../references/compiler.md#name-resolution).
The file scan precedes assembly and therefore has different identities and spans from desugaring;
sharing decoded facts across that boundary would require an explicit remapping.

Next, extend independent checker and validator recovery and migrate typed structural operations.
For each migration, remove the superseded recursion and update all callers in the same change.
Compare separate and composed facts and diagnostics, including multiplicity and exact locations.
Verify accepted-program behavior, rejected inputs' failure status, and the absence of invalid normal products.
Count visits and allocations on shared graphs before making compilation-time claims.

## Remaining graph views

Typed substitution, hole resolution,
and filled normalization now use the [classifier folder](../references/compiler.md#classifier-folders).
The unused `LocalFoldStatics` interface is removed.
The raw inferred graph, normalized classifier views, and residual runtime graph remain distinct.
The [runtime graph iterator](../references/compiler.md#residual-runtime-traversal) now exposes the residual view
and supports complete execution-hole collection.

Context-sensitive analyses need more than a node-ID cache.
[Type support collection](../../lang/statics/src/normalize/scope.rs) intersects the admissible scope
of a shared inference hole across its occurrences.
A unique-node walk could miss the more restrictive occurrence.
Further migrations must use occurrence traversal or memo keys containing the relevant environment,
and invalidate results when the state they depend on changes.

## Remaining review sequence

High SPS variable collection
and ownership validation now use the [shared analysis traversal](../references/compiler.md#high-sps-analysis-traversal).
Verification composes those analyzers, and closure conversion reuses variable facts across its capture queries.
The normalizer's forward producer facts and backward demands retain their established semantic schedule.
Further reconstruction work should be driven by a concrete shared rule,
preserving the [consumer-demand dependency](../references/compiler.md#consumer-demands).

Continue the [diagnostic producer audit](#diagnostic-collection-and-recovery).
Structural code generation, independent pruning masks, and further graph views remain optional extensions
that require concrete clients.
Measure traversal work, allocations, and temporary storage before claiming compilation-time or memory improvements.
