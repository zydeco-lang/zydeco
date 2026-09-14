# Reusable folders and traversal composition

This proposal retains unfinished extensions to compiler traversal and diagnostic recovery.
The implemented surface, classifier, runtime, and SPS interfaces are indexed
by [choosing and composing traversals](../references/compiler.md#choosing-and-composing-traversals).
That reference owns their shared design rules and links to the detailed phase contracts.
The [diagnostic collection contract](../references/compiler.md#diagnostic-collection) records current recovery
and reporting behavior.

## Diagnostic collection and recovery

Two concrete followups remain, in recommended implementation order:

| Priority | Work | Estimated difficulty | Expected benefit |
| --- | --- | --- | --- |
| 1 | Collect [package catalog and discovery](../../lang/session/src/source/query.rs) failures across selected roots and discovered files | Medium | Report several broken packages in one run, reducing correction cycles |
| 2 | Collect independent [SPSLow validation](../../lang/stackir/src/low/check.rs) failures | Medium–high | Expose multiple structural or contract problems during compiler development |

Package catalog construction currently visits roots and discovered files through separate fallible requests.
Collect independent provider and discovery failures without publishing an incomplete catalog.
Rechecked at `3b4dd665` on 2026-09-14 with a newly built debug CLI:
`workspace.zy` containing `@[discover(include("a.zy", "b.zy"))] ()`,
with both discovered files containing only `(`, makes `zydeco show` report only `a.zy`.
After replacing `a.zy` with `()`, it reports `b.zy`; replacing both with `()` succeeds.
Both rejected runs leave stdout empty.
Retain this pair of independent failures and the successful catalog when extending recovery,
including the invariant that failed discovery publishes no partial result.

SPSLow validation currently returns the first structural, entry-contract, or protocol failure.
Establish recoverable structural regions before batching dependent contract checks;
malformed ownership and cyclic input cannot supply the evidence those checks expect.

More checker recovery should begin with a demonstrated diagnostic or tooling gap.
The implemented [checker recovery](../references/compiler.md#checker-recovery) covers independent components,
arms, closed imports, and finalization roots.
Recovery through failed sequential bindings, telescope witnesses,
or ambiguous clause prefixes would require an explicit representation of unavailable evidence
before their dependent bodies could be checked.
Do not turn those prerequisites into fabricated annotations merely to continue traversal.

Error presentation customization remains deferred.
A future renderer can group or sort the retained diagnostics without changing producer order
or the phase's acceptance boundary.

## Further resumable folder migrations

The implemented [resumable execution contract](../references/compiler.md#resumable-folder-execution) provides
statically selected native and explicit-stack drivers for residual lowering, Builtin package materialization,
high SPS reconstruction and pattern decisions, closure conversion, SPSLow structural analysis, and local unboxing.
Their equivalence and depth checks support using the interface for those boundaries.
The following sections record boundaries skipped after evaluation because their ownership
or consumer interfaces need a further design choice.

### Classifier environments and suspension

The [raw classifier folder](../../lang/statics/src/fold.rs) supplies immediate recursive callbacks.
For `ValPi`, it first rebuilds a parameter's domain, then lends the updated local binder
to `fold_body` through `TypeScope::ValueFunction`.
Package bodies similarly receive a reference to their local witness telescope.
[Abstract substitution](../../lang/statics/src/normalize/substitution.rs) filters its ordered assignments
into a local vector and lends that vector to a temporary child folder.
These references remain valid while the original Rust calls are active.
A resumable request must retain the necessary state after those calls have returned.

A generic driver alone cannot choose the ownership of child environments,
the representation of updated binders, or the lifetime of filtered assignment sequences.
Wrapping each existing callback in a fresh `Explicit::run` would retain the recursive callers around those runs.
The recursive call cycle must instead cross the shared suspension protocol.

Two concrete alternatives remain open:

| Alternative | Benefit | Cost and required decisions |
| --- | --- | --- |
| Own scope descriptors and assignment sequences in each child request | Direct correspondence with the current lexical calls; compiler borrows end before suspension | Decide when to move, copy, or share binder and assignment data; repeated filtering and copying need measurement |
| Store environments in a per-run table and pass environment IDs plus assignment suffix positions | Stable small requests and explicit environment lifetimes; can reuse common prefixes | Introduces storage and retirement rules; filtering must preserve ordered replacement semantics and binder shadowing |

Prefer evaluating owned requests first for clarity, then introduce table storage only
if concrete clients justify its additional machinery.
This preference does not adopt either representation.
The shared raw-type layer should describe structural child slots once, while semantic clients choose child environments
and retain their existing allocation, normalization, and cache policies.
`Tycker` remains shared mutable run state and is reborrowed between folder steps.

Before resuming this migration, compare a simple raw structural client with abstract substitution,
hole resolution, and filled normalization.
The validation set must include unchanged-ID reuse, changed labels and provenance,
shared solved tails, several missing solutions, updated parameter domains,
ordered assignment suffixes, and witness shadowing through each body boundary.
Include reduction calls reached after reconstruction:
an explicit structural subwalk alone does not bound recursive normalization or substitution helpers.
Replace the affected callback interfaces and their callers together once the ownership choice is settled.

### Surface rebuilding and recovery

The [freshening folder](../../lang/surface/src/bitter/freshen.rs) has a suitable ownership policy:
it copies each occurrence into `BitterBuilder`, retains textual origins, and stores no borrowed child environment.
Its recursive edge, however, passes through the shared [surface rebuilding interface](../../lang/surface/src/fold.rs).
Patterns can contain terms in annotations and views, so migrating only pattern recursion would leave that cycle intact.

`Pattern::fold_with` and `Term::fold_with` currently finish their children inside ordinary Rust callbacks.
The interface also changes reference representation from `InputRef` to `OutputRef`,
and its generic `fold_items` callback supplies the grouping boundary for recovery.
Freshening keeps source names and cannot fail; resolution converts names to definition IDs
and collects independent reported failures through `OrdinaryFolder::fold_items`.
An annotation's structural copying order is term then classifier, while the resolver deliberately handles
that constructor itself and resolves the classifier first.
A child layer must expose positions without imposing one semantic schedule on both clients.

This is a design boundary beyond changing the driver invocation.
Copying the syntax match into a dedicated freshening machine would duplicate the shared structural rule.
Starting a new explicit driver inside each existing callback would still leave recursive callbacks on the native stack.
The useful next step is a resumable description of one owned syntax layer, shared by copying and resolution.
It must support partial child results, reference conversion, and continuing independent children after rejection
without constructing a successful parent from failed prerequisites.

Two implementation approaches deserve comparison before replacing the interface:

| Approach | Benefit | Cost and design question |
| --- | --- | --- |
| Handwritten owned layers with typed child slots and reconstruction frames | Explicit constructor coverage and recovery boundaries; can be evaluated directly with both existing clients | Decide how a partially rebuilt layer represents input and output references, and how callers select independent groups and scope-dependent children |
| Generate layers and their child operations from a structural declaration | One child description can support synchronous and resumable execution without parallel handwritten matches | Adds generation machinery and annotations for child roles; semantic scheduling and recovery still belong to the client |

Prefer a handwritten layer evaluation first, using freshening and ordinary resolution together.
If accepted, replace their shared structural interface and callers in one change.
This preference does not yet choose the layer representation or adopt a generated syntax description.
A later complete resolver migration must also retain global environments beyond the current borrowed calls
and resume block dependency collection through its existing success and abort boundaries.
The [resolver](../../lang/surface/src/scoped/resolver.rs)
and [block handling](../../lang/surface/src/scoped/blocks.rs) show those current lifetimes;
the [name-resolution reference](../references/compiler.md#name-resolution) owns their semantic contract.

Acceptance should cover repeated source and signature boundaries, fresh binder identities,
origins, annotation and view children, and copattern spines.
Pair successful name resolution with several unresolved siblings; preserve diagnostic multiplicity,
reference and scope events, dependency cleanup, and the absence of a published strict-resolution product on failure.
A direct deep fixture must cross pattern-to-term edges and include destruction of retained layer state.

### SPSLow semantic evidence

The implemented [SPSLow scan](../references/compiler.md#spslow-traversal-and-analyzers) covers structural ownership
and variable summaries.
The [entry-contract validator](../../lang/stackir/src/low/contracts.rs)
and [protocol validator](../../lang/stackir/src/low/protocols.rs) require more than the same structural events.
They propagate entry-specific evidence through bindings, open packages, and branch-local contexts.

`ValueEvidence::Product` owns nested field evidence, while `StackEvidence` owns boxed argument and tag tails.
Branch contexts and alias binders clone those structures.
The protocol validator adds `ValueFact::fields`, constructs nested facts from source protocols,
and clones inline `ValueProtocol` and `StackProtocol` components.
An explicit syntax folder would leave recursive construction, cloning, and destruction of these facts in place.
The existing protocol graph names recursive definitions; it does not flatten every inline field or stack prefix.

This is a medium–high difficulty migration, deferred until evidence ownership is chosen.
Compare per-validation fact arenas with IDs and shared context prefixes against retaining owned trees
with iterative fact operations and destruction.
Arena IDs make suspension and teardown straightforward but introduce fact lifetimes and storage policy;
owned trees retain the current interfaces but require coordinated changes to every recursive operation.
Keep entry kind, opening identity, environment arity, alias propagation, and restored-stack checks unchanged.
Include both successful and rejected deep products, argument stacks, aliases, and branch contexts,
and drop retained facts and error payloads on the small test stack.

### Normalization fact ownership

Borrowed [pattern decisions](../references/compiler.md#pattern-decisions-and-validation) now use the common driver,
including suffix views that borrow existing physical fields.
The remaining owned [producer facts](../../lang/stackir/src/high/normalize.rs)
and [consumer demands](../../lang/stackir/src/high/demand.rs) are a medium–high difficulty case,
skipped because traversal and fact lifetime must be addressed together.

`known` recursively constructs product and constructor facts through `shared`,
and primitive fact evaluation calls back into `known` for its operands.
`KnownValue` shares children through `Rc`, so copying a child handle is shallow,
but releasing its last owner can recursively destroy the entire nested fact.
Moving syntax descent to frames would not address temporary facts, environment teardown,
or early-return paths that release such owners.

`Demand::Fields` owns nested demands in a `BTreeMap`.
Pattern translation, `join`, suffix extraction, cloning, equality, and destruction depend on that structure.
Even `Used.join(deep_fields)` can recurse while discarding the absorbed operand,
although the join needs no recursive semantic work.
An explicit join folder alone therefore cannot establish a useful depth guarantee.
Inline source protocol operations have the related evidence-ownership boundary described above.

Compare a per-normalization fact arena with IDs and suffix ranges against retaining owned trees
with iterative construction, combination, cloning, and destruction.
An arena would make sharing and teardown explicit, but requires a retention policy
and changes consumers that currently move or cheaply share values.
Owned trees preserve more interfaces, but every ownership exit needs an audit;
changing `Drop` also affects how Rust permits moving fields from consumed values.
Do not add an input-depth cutoff or claim that a reference-counted pointer alone solves destruction depth.

Preserve the reference's producer-before-consumer schedule, alias restrictions on moving closures,
physical suffix positions, empty product-shape demands, and `Used` absorption.
Acceptance should exercise deep facts retained by several environments, last-owner release, short-circuit decisions,
primitive operand recursion, and nested demands joined and discarded through branches.
Measure retained memory as well as traversal depth before selecting an arena policy;
the [compiler memory proposal](arena-gc.md) owns the broader retention questions.

### Other execution adapters

The scoped and high SPS analysis visitors already enumerate children directly into one traversal vector.
Adapting their callback-based child enumeration to one-child-at-a-time calls would need retained child sequences
or a shared child cursor; re-enumerating all children for each position would make wide nodes quadratic.
A migration needs a concrete maintenance benefit and must preserve entry/exit balancing, cycle handling,
early termination, and occurrence policies.

The residual runtime traversal is a lazy `Iterator`: a consumer can stop after any yielded node.
A whole-fold `Driver::run` cannot replace that suspension boundary without changing the consumer interface
or buffering the traversal.
Keep a lazy adapter requirement separate from reconstruction driver selection.

Desugaring retains its current execution mechanisms.
Do not expand the common folder protocol merely to accommodate all these mechanisms at once.
Each subsequent migration should demonstrate a simpler client under both drivers and retain its depth fixture.

### Assembly continuations and publication

[Assembly lowering](../../lang/assembly/src/lower.rs) is a medium–high difficulty case,
skipped after evaluating its syntax and instruction execution separately.
`Stack::Arg`, `Stack::Tag`, and continuation packages descend into the rest of the stack directly,
retaining boxed consumers for values and tags.
The existing `pending` loop therefore bounds deferred instruction work, not all syntax descent.
Product and alias handlers also build nested consumers, and branch handlers lower child programs
while constructing their jump tables.

An instruction's `Construct::build` reserves its `ProgId` immediately.
Its pending callback computes the next context, invokes the consumer to obtain the successor ID,
then publishes the instruction with its original context.
[`Kont` and `CxKont`](../../lang/assembly/src/arena.rs) encode those consumers and context updates as boxed functions.
Native continuation lowering additionally creates capture bindings, a resume entry,
a symbol, and frame metadata around that schedule.
Wrapping the pending loop in the common driver would retain the direct recursion and nested function ownership.

The promising next step is to replace these boxed consumers with typed continuation and context operations,
then drive syntax descent and instruction completion through the same suspension protocol.
Compare a continuation arena with IDs against owned reconstruction frames that reserve output IDs before descent.
The former can preserve shared continuation boundaries but needs storage and retirement rules;
the latter makes ownership local but must distinguish obtaining an ID from publishing its instruction.
Neither representation is selected here, and the common driver does not yet need an expanded protocol.

Acceptance must compare allocation slots, definition associations, contexts, successor links,
branch order, and native frame-entry metadata under both drivers.
Include deep argument/tag stacks, nested patterns, wide products, branch tables,
portable continuation packages, and native resume entries.
Check final assembly validation and emitted Wasm as well as lowering alone;
teardown must also avoid recursively dropping nested continuation payloads.

## Additional graph views and adapters

A borrowed adapter for both bitter and scoped syntax may become useful if another analysis needs it.
Their [debug formatter](../../lang/surface/src/debug.rs) already adapts the shared syntax family,
while the implemented owned folder serves reconstruction.
Wait for another concrete borrowed consumer before expanding the interface.
Generating borrowed traversal and owned rebuilding from one structural declaration is likewise optional;
first establish that two real clients share a stable child description.

Further typed migrations must identify the graph they observe.
[Type support collection](../../lang/statics/src/normalize/scope.rs) depends on occurrence-specific scope constraints;
a replacement needs a context-sensitive traversal or a justified memo key under the reference's cache rules.
The raw classifier folder and residual runtime iterator provide distinct starting points. Further SPS
rebuilding must preserve the established [consumer-demand schedule](../references/compiler.md#consumer-demands).

Residual lowering and high SPS normalization now use explicit reconstruction frames,
as recorded in [C8](../references/compiler.md#c8-high-sps-lowering-normalization-and-demand).
Further depth work should distinguish those completed migrations from recursive semantic helpers:
normalization's owned known-value construction, structured demand and protocol operations,
and low verification still have recursive paths.
Use direct phase fixtures to establish each remaining limit before choosing its work frames or fact representation;
include destruction of nested retained facts in that audit.
The compiler has no end-to-end arbitrary-depth guarantee.

## Pruning and synthesized results

The existing `Together` visitors compose independent observations under one traversal policy.
A future generic interface for synthesized child results could pair those results at each node,
letting each analyzer consume only its own child's component.
Adopt it only when it removes concrete repetition beyond the current postorder summaries.

Independent pruning needs a separate design: one analyzer cannot discard a subtree required by another.
Candidate approaches are per-analyzer activity masks or separate traversal groups.
Specify callback balancing, partial-result status, and diagnostic collection before choosing an approach.
Neither facility is needed by the current composed analyzers.

## Performance validation

End-to-end performance measurement is an optional followup of low–medium estimated difficulty.
Existing tests establish traversal and allocation counts, but do not establish compilation-time or peak-memory gains.
Compare the implementations before and after these migrations on representative programs,
including repeated imports and nested closures, with the same inputs and toolchain.
Measure cold and warm checks separately, repeat timings, and record peak memory alongside traversal
and allocation counts.
In particular, account for the temporary variable summaries retained during closure conversion.
The benefit is evidence of the overall effect and identification of any memory regressions before further optimization.

## Migration acceptance

For each extension, identify the client and its repeated structural rule,
then apply the [reference's traversal selection criteria](../references/compiler.md#choosing-and-composing-traversals).
Remove the superseded recursion and update callers in the same change.
Compare accepted behavior and rejected inputs, including error multiplicity and locations,
arena identities, provenance, and the absence of invalid normal products.
Measure traversal work, allocations, and temporary storage before claiming compilation-time or memory improvements.
