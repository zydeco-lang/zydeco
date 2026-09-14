# Reusable folders and traversal composition

This proposal collects the remaining work on reusable compiler traversals.
The core interfaces below are a design draft for discussion before implementation.
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
Desugaring and resolution contain their own recursive dispatch; typed substitution,
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

The source loader currently propagates the first failed directive query. Desugaring and strict resolution also return
individual failures, while the parser retains multiple issues and the type checker already exposes a diagnostic
collection. The intended direction is multiple-error reporting throughout the compiler. A pass should report the
independent errors it can establish from its available input, and the session and frontends must preserve that
collection instead of selecting one entry. Follow the
[diagnostic provenance contract](../references/compiler.md#c15-diagnostics-formatting-documentation-and-interactive-tooling)
when retaining rejected facts and suppressing consequences of an earlier failure.

### Collections and phase outcomes

Use a vector of typed domain diagnostics in each analyzer or semantic folder.
Pure analyses can return useful facts and diagnostics together:

```rust
struct Analysis<T, D> {
    facts: T,
    diagnostics: Vec<D>,
}
```

For example, an import analyzer retains every successfully decoded site and every independently rejected directive.
Its facts are partial when diagnostics reject some sites; they are not themselves a validated source template.
Keep existing domain error enums, source identities, and related locations rather than flattening failures to strings.
The session maps collections into its source-aware diagnostic types at the phase boundary.

Strict transformation entry points can keep `Result<Output, Failure>`.
Their failure type carries a nonempty diagnostic collection and the source context needed to render every entry.
This fits the existing `CompilerPass` interface: `Err` means that a valid phase product is unavailable,
not that only one error was found.
Retained tooling facts belong in a phase-specific rejected outcome,
following the checker's existing checked/rejected distinction.
Do not represent a partially built arena as a successful normal output merely because traversal continued.

Inside a semantic folder, record a diagnostic once and return a small `ReportedError` token
when the current node cannot produce its required result.
Propagating that token does not emit the diagnostic again.
Local `Result` remains useful for dependent operations; the driver decides where
to catch a rejected node and continue with independent work.
Memoized rejected source nodes must not replay their diagnostics on subsequent visits.

### Difficult tradeoff: where recovery is meaningful

Accumulating errors requires deciding what remains meaningful after a failure.
An invalid import has no effect on decoding a separate literal annotation.
An invalid binder, however, can prevent establishing the environment needed by its body.
The structural folder cannot invent that environment or an arbitrary replacement binder.

For ordinary children with independent inputs, evaluate every child before propagating a failed result.
The shared structural mapper can stage those child results and reconstruct the parent only when all are available.
A short-circuiting `collect::<Result<_, _>>()` must not prevent visiting the remaining independent children.
Sequential telescope and scope handlers retain explicit recovery boundaries for children that depend on earlier results.
Use existing, justified recovery forms where possible; otherwise reject the affected construct
and continue with its independent siblings or source units.
Do not promise an exhaustive list of errors whose interpretation depends on missing semantic information.

This distinction also applies between passes. Continue later analysis only when it has input satisfying its contract,
or through a specifically supported recovered outcome.
An invalid program must never reach execution or code generation as a successful compilation.
The first migrations can accumulate errors within a pass while keeping strict phase boundaries;
broader analysis of recovered intermediate representations needs an explicit recovery contract.
The same reporting direction applies to parsing, loading, checking, and later validation passes,
using their own unit and dependency boundaries rather than one universal recovery mechanism.

### Initial presentation order

Append diagnostics as each producer collects them and concatenate composed analyzers' collections
in their existing composition order.
Keep current ordering of successful facts where callers require it.
Do not add category priorities, a global source-position sort, or ordering configuration in these migrations.
Arena iteration may affect presentation order; that order is not an API guarantee.
Future frontend ordering can use the retained typed categories and source locations without changing traversal.

Tests should assert diagnostic contents, multiplicity, and locations independently of presentation order.
Distinct errors at the same span remain distinct; matching a span alone is not a deduplication rule.
Known dependency failures should suppress consequent diagnostics at their producer,
as the checker already does for missing solutions caused by a rejected expression.

## Core interfaces for the three surface migrations

The immediate work covers desugaring rule ownership, shared source-directive analysis, and resolution events.
These mechanisms operate at different boundaries:

| Boundary | Driver | Local operations or consumers | Published result |
| --- | --- | --- | --- |
| One parsed file | `SourceScan` | Typed source analyzers | Facts used to construct a `SourceTemplate` |
| Assembled textual syntax | `DesugarFolder` | Telescope, binding, CBPV, and meta annotation rules | Bitter syntax through `BitterBuilder` |
| Bitter syntax with lexical environments | `ResolveFolder` | Dependency analysis and resolution observers | Scoped syntax and resolution facts |

The source scan runs before assembly.
Its IDs and spans belong to one parsed file; desugaring consumes the assembled program's identities.
The two stages can share meta annotation decoders,
but transferring cached facts between them requires an explicit remapping.
These migrations do not introduce that additional cache.

### Structural folders and semantic scheduling

The current shared `Folder` is infallible and preserves the reference type.
Resolution needs fallible lookup, changes `VarName` to `DefId`, and can replace an unbound variable
with an internal hole to continue diagnostic collection or completion recovery.
Extend that existing interface only as those concrete requirements demand.
The following signatures sketch the intended boundary; they are not implemented APIs:

```rust
trait Folder {
    type InputRef;
    type OutputRef;
    type Error;

    fn fold_def(&mut self, id: DefId) -> Result<DefId, Self::Error>;
    fn fold_pat(&mut self, id: PatId) -> Result<PatId, Self::Error>;
    fn fold_term(&mut self, id: TermId) -> Result<TermId, Self::Error>;
    fn fold_var(&mut self, name: Self::InputRef)
        -> Result<Term<Self::OutputRef>, Self::Error>;
}
```

`Term<R>::fold_with` would return `Result<Term<F::OutputRef>, F::Error>` for a folder with `InputRef = R`.
The variable hook returns a term so the recovery policy can produce either `Var(definition)` or `Hole`.
An error recorded while producing a recovery hole still rejects the strict phase outcome.
`FreshenFolder` uses `VarName` on both sides and `Infallible` as its error;
its copying and provenance policies remain those in the reference.
Replace the existing structural implementation and migrate its callers together.
Do not maintain separate infallible and fallible constructor matches.

The structural mapper supplies ordinary child reconstruction.
A semantic folder supplies inherited information and receives synthesized results:
for resolution, the inherited information is the lexical environment,
and a pattern additionally returns the environment extended by its binders.
Use explicit `ResolveEnv` arguments and a short-lived adapter containing the current node
and environment when delegating ordinary reconstruction.
An adapter passes the same input environment to independent children;
it does not mutate one ambient scope that accidentally leaks between siblings.
Ordinary reconstruction follows the collection rule above: evaluate independent child hooks
before deciding whether the parent can be built.
Semantic folders use `ReportedError` for already-recorded source failures.

Resolution must classify constructors exhaustively into ordinary reconstruction or a semantic handler.
A new constructor must require reviewing that classification, even if the shared mapper already knows its children.
Annotations, binders, source boundaries, blocks, and monadic blocks have scheduling requirements beyond field order.
For example, resolution visits an annotation's classifier before its payload,
while copying visits payload before classifier.
Scope-changing handlers therefore choose child order and environments explicitly.

Textual lowering changes constructors and node categories, so `DesugarFolder` has its own typed entry points.
Do not parameterize every textual enum merely to force it through the same Rust trait.
Both semantic folders centralize recursion and publication; their rule modules
and analyzers do not implement another independent recursive walk.

The tradeoff is deliberate: one universal fold algebra would expose more opportunities for automatic composition,
but would also require encoding lexical effects, recovery, constructor changes, and scheduling barriers in its types.
Small structural interfaces plus explicit semantic folders make the current dependencies reviewable.
Generate structural declarations only after their shared requirements have been demonstrated by real clients.

## Desugaring decomposition

The implemented [desugaring folder and rule owners](../references/compiler.md#desugaring-folders) centralize
recursive lowering, source-term memoization, telescope rules, paired binding construction, and diagnostic collection.
The remaining extraction concerns meta annotation inspection and dispatch.

### Meta annotation actions

Give `MetaRules` one explicit dispatch over the recognized annotation kind.
Its inspection step validates the arguments and any required raw payload shape,
retaining both annotation and payload origins for diagnostics.
It returns a domain action such as:

```rust
enum MetaAction {
    Preserve(Meta),
    Intrinsic(IntrinsicRole),
    TypeOf,
    Monadic,
    Partial(Meta),
}
```

These actions describe the current lowering choices, not an extensible sequence of arbitrary plugins.
`Intrinsic` validates an original hole payload and constructs the intrinsic without lowering that payload.
`TypeOf` and `Monadic` lower their payload once and construct their specialized nodes.
`Partial` records the relevant source binders before lowering its payload, then retains the meta annotation.
Validated FFI and term-level Builtin annotations follow preservation when appropriate;
invalid placement remains an error at the same source site.
Existential parameter annotations keep their distinct allowed roles and diagnostic sites.

This source inspection belongs before descent.
For example, recording partial binders after currying would lose the original binding header boundary,
and validating an intrinsic after lowering could accept a transformed payload that was not an authored hole.
A source-term cache hit must not suppress a required action on a newly encountered enclosing annotation.
Avoid an inherited ambient meta annotation mode: the existing source-ID memoization would then need that mode
in its key or a proof that lowering is independent of it.

## Shared source-directive analysis

### One inventory scan with explicit reachability

The [current loader](../../lang/session/src/source/loader.rs) invokes documentation, unattached-text,
import, Builtin, intrinsic, literal, package, and discovery queries separately.
The [surface queries](../../lang/surface/src/textual/source.rs) have different domains:

| Analysis | Current input domain |
| --- | --- |
| Documentation, package annotations, discovery annotations | Nodes reachable from the returned source root |
| Imports, literal splices, intrinsic validation | All allocated terms in the file's textual arena |
| Builtin validation | All allocated term annotations and existential parameter annotations |
| Unattached-text warnings | All relevant annotation attachments and file trivia |

The distinction matters for recovery: allocations can remain after their parser stack entries are discarded.
A scan restricted to the returned tree would change some current validation behavior.
The initial implementation should compute reachability once through `TextArena::children`,
then scan the arena's terms once, emitting events with a reachable/unreachable classification.
Finish with the trivia scan needed for unattached-text warnings.
This is one shared reachability walk plus one shared arena sweep, rather than one root traversal per analysis.

Each analyzer selects its existing domain.
A future change to make every analysis use only reachable nodes would simplify the scan,
but needs an explicit decision about malformed or abandoned source material.
Do not silently introduce it during traversal extraction.
Package selection remains downstream: a file inventory does not erase the distinction between the complete file
and the selected nested package root.

### Event protocol and composition

Use a borrowed `SourceView` for syntax, spans, trivia, and root information, and typed events for:

- A term meta annotation, with its term, meta, payload, and reachability.
- An existential parameter annotation, with its owner, parameter position, binder, meta, and reachability.
- An attached text block or file text block, where warning analysis needs it.

Decode the semantic meta tree once for an annotation event requested by the active analyzers.
Retain its textual IDs alongside the decoded value so argument errors still identify exact source ranges.
An unknown annotation does not require running every known decoder.

```rust
trait SourceAnalyzer {
    type Output;
    fn interests(&self) -> SourceInterest;
    fn observe(&mut self, event: &SourceEvent<'_>, source: &SourceView<'_>);
    fn finish(self) -> Self::Output;
}
```

`Together<A, B>` forwards an immutable event to both analyzers and returns their paired outputs.
Use static composition for the file-loading profile and smaller profiles for import-only or documentation-only callers.
Implement each decoder and accumulator once; query-style entry points route through those profiles.
A request for documentation alone must not begin rejecting unrelated import annotations.
Each analyzer also declares a typed `SourceInterest` describing its event kinds
and whether it needs reachability or trivia.
Composition unions those interests before scanning.
This lets the scanner avoid unrelated decoding and lets an import-only profile omit reachability computation.

Analyzer output can be `Analysis<Vec<ImportSite>, ImportDirectiveError>`, a documentation vector,
or a diagnostic collection for validation that does not need to retain sites.
An analyzer continues after a rejected site, retaining valid facts and errors from its later events.
The dispatcher continues delivering each event to all interested analyzers.
This collects multiple failures within one category as well as across categories.
Query callers consume these complete results; remove adapters that return only the first directive failure.
Successful outputs retain their current ordering, including package results ordered by name.
The loading profile maps and concatenates the diagnostic vectors at its boundary.
It constructs a valid `SourceTemplate` only when no error rejects that template;
partial inventory facts may be retained separately for diagnostics or supported tooling queries.
Keep the temporary scan state local to parsing.

### Independent checks and validation prerequisites

Both annotations in the following source should contribute diagnostics:

```zydeco
(@[literal(extra)] _, @[import] _)
```

Report the literal's unsupported argument and the import's missing target.
Two malformed imports must likewise produce two diagnostics.
The [initial presentation policy](#initial-presentation-order) determines how the collected errors are displayed.

Keep the prerequisites of each validation rule explicit.
A malformed argument can prevent extracting the name needed for a package duplicate check.
Skip checks that need that unavailable name, while validating other package sites.
Retain successfully decoded names and source sites for aggregate checks at `finish`.
Discovery duplicates and placement checks inspect the complete annotation set;
argument validation on independent sites still runs when another site has failed.
Builtin term and existential-parameter annotations both contribute diagnostics.

Existing individual decoders can initially retain a local `Result` for a malformed directive.
The analyzer catches each rejected site and resumes with the next site.
Additional errors inside one directive can be collected as its decoder gains justified recovery points.
This avoids requiring a universal recovery grammar before reporting independent file errors.

## Resolution decomposition

### Folder, environment, and publication

`ResolveFolder` should own a scoped builder, the required dependency analyzer, a composed set of observers,
a diagnostic collector, and an explicit strict or completion publication policy.
Move name lookup and scope enumeration to a shared scope module; both references
and captured scopes continue to use the same shadowing rules.
The core entry points have these semantic results:

```rust
fn term(&mut self, id: TermId, env: ResolveEnv) -> Result<TermId, ReportedError>;
fn pattern(&mut self, id: PatId, env: ResolveEnv) -> Result<ResolvedPattern, ReportedError>;

struct ResolvedPattern {
    pattern: PatId,
    env: ResolveEnv,
}
```

Ordinary resolution preserves existing IDs. The builder materializes their resolved contents, retains origins,
and allocates new IDs only for the established context elaboration operations.
An accepted result publishes `ScopedArena` together with completed reference and documentation facts.
The existing free-variable analyzer still runs on the elaborated scoped structure.

Pattern annotations resolve their classifier before introducing the annotated binders.
Dependent pattern sequences and copattern spines thread the returned environment left to right;
match arms receive independent input environments.
A source or signature boundary starts with an isolated environment and resolves a shared provider only once.
The ordinary child adapter is used only for constructors whose children inherit the same environment and
whose current order agrees with structural reconstruction.

### Typed facts and passive observers

Emit a resolved-reference event after successful lookup, containing the occurrence ID,
selected definition, and the selected definition's optional `BindingSite`.
Also expose a borrowed view of all active enclosing binding sites.
The dependency analyzer needs every active binding with the same block owner as the selected dependency;
choosing only the innermost binding would lose edges in nested contexts.
An observer must never repeat name lookup to recover the selected definition.
Failed lookup records a diagnostic without emitting a successful-reference event or an invented dependency edge.

```rust
struct ResolvedReference<'event> {
    occurrence: TermId,
    definition: DefId,
    dependency: Option<BindingSite>,
    active_bindings: ActiveBindings<'event>,
}
```

`ActiveBindings` is a borrowed iterator view over the existing persistent sequence;
an event does not allocate a vector of enclosing bindings.

Emit scope events at the original sites already observed today: an authored documentation annotation
before its payload, and a source hole when that hole is visited.
Include the current ID, textual origin, site kind, and a borrowed scope view.
The completion observer matches the exact remapped textual target, not an equal span.
Recovered unbound variables become holes as a resolution policy decision;
those generated holes are not additional authored cursor events.

```rust
trait ResolutionObserver {
    type Output;
    fn reference(&mut self, event: &ResolvedReference<'_>);
    fn scope(&mut self, event: &ScopeEvent<'_>);
    fn finish(self) -> Self::Output;
}
```

The reference-index observer constructs the existing definition-to-use relation.
The documentation observer retains scopes only for documentation sites.
The completion observer materializes a snapshot only at its exact target.
Scope events borrow the environment; observers explicitly own the small snapshots they retain.
Do not construct and store a scope snapshot at every visited node or retain a complete event log.
Observers cannot mutate the builder, alter lookup, prune descent, or abort resolution.
Their event order follows the semantic folder's actual schedule, not a second structural traversal.
Composition forwards each event and pairs the final outputs, as with source analyzers.
The standard compilation profile includes reference indexing and documentation facts;
the completion profile additionally captures its requested site.
Publication requires the facts expected by `ScopedArena`; optional observation must not silently omit those fields.

Events describe source resolution, so a provider cache hit does not replay its references or scopes,
and context elaboration does not emit a second stream for generated wrappers.
The source and provider identities in the existing indexes remain the unit of indexing.
Preserve the current rule when several visited nodes share an origin: completion replaces its retained site
on each exact-origin match in the existing visit order.

### Difficult tradeoff: dependencies are required intermediate results

Reference indexing and tooling are passive observers.
Dependency analysis is separate logic, but its result is required before resolution can finish a block.
Give it the same reference events plus an explicit block lifecycle:

```text
begin_block(block, candidate_ids)
    resolve candidate annotations and bindees
    resolve residual body
finish_block(block) -> DepGraph<BindingId>
    schedule strongly connected components
    elaborate the block through the scoped builder
```

`begin_block` seeds every candidate, including those with no references.
The collector keeps independent active graphs keyed by block owner, so nested blocks do not steal outer edges.
`finish_block` removes and returns exactly that block's graph.
Scheduling receives it as a typed input; no observer downcast or globally shared side table is needed.
Successful publication requires all block graphs to have been consumed.
An explicit `abort_block` removes an incomplete graph when a failure skips the rest of that block.
This cleanup is required before recovery continues with a sibling; unfinished graphs cannot leak into later analysis.
Validate independent strongly connected components before deciding whether block elaboration can succeed,
so several illegal recursive parameter groups can contribute diagnostics.
A graph from incomplete traversal cannot be published as a complete dependency result.

Keep this analyzer as a required collaborator of `ResolveFolder`, alongside the optional observer product.
Dispatch reference events to it and the observers without exposing their mutable state to each other.
This gives independent ownership and testing while retaining the actual data dependency.
A uniform optional-observer interface would misleadingly allow a configuration without the facts needed for scheduling,
or would have to recover them through a hidden channel.

Block candidate discovery remains before binder installation and reference resolution.
Record candidate binders once where possible and reuse that record when establishing ownership,
but preserve the collector's source, nested-block, and mobile-binding boundaries.
A reference-event stream cannot discover a forward binder in time to resolve an earlier use.
This is why the design shares the reference-resolution traversal without claiming
that the complete block algorithm is one ordinary depth-first traversal.

### Recovery and partial results

Both strict and completion analysis collect independent resolution errors.
Reuse the existing unbound-reference recovery where it permits continuing lexical resolution:
record the error and use an internal hole at that occurrence.
Keep its recovery origin available so it cannot become a fresh authored cursor event
or independent evidence for a consequent diagnostic.
Strict compilation still rejects the result when any resolution error was recorded,
even if those internal replacements allowed traversal to finish.
Completion can retain a request-local recovered program under its existing tooling contract.
The completion observer only captures scope; it does not own error collection or decide whether analysis continues.

For failures without a justified replacement, skip the affected dependent region and resume with independent siblings.
Binder discovery can collect multiple duplicate declarations before rejecting an ambiguous block scope.
Initially, skip resolution that needs that scope rather than selecting an arbitrary winning definition.
An absent enclosing block or invalid context component likewise prevents publishing its affected construction.
Extending recovery through ambiguous scopes can be considered separately when it provides useful additional diagnostics.

On success, publish complete syntax and required facts.
On rejection, retain all collected diagnostics and supported observations in a phase-specific failed outcome;
strict resolution publishes no normal scoped program.
Completion retains captured sites even when a later, unrecoverable region prevents producing its recovered program.
An unvisited target still has no invented scope.
Observers see the actual visits made during recovery, potentially including siblings after a rejected branch.
There is no replay or rollback promise, and facts from rejected analysis are not presented as a complete index.
The same successful input should produce identical reference and dependency facts under strict and completion policies.

## Migration order and acceptance criteria

Implement these as separate reviewable changes after the design choices are settled:

1. Extract raw meta annotation inspection and typed actions, with one driver-owned memo publication path
   for successful and rejected terms.
2. Introduce `SourceScan`, migrate the full loading profile and smaller query callers, and delete repeated scans.
   Collect errors from every independent directive site and return the full collection.
3. Extract the resolution policy, typed events, and passive observers while preserving the existing semantic schedule.
   Use its diagnostic collector in strict analysis as well as completion, with explicit recovery boundaries.
4. Move dependency accumulation behind the explicit block lifecycle and connect ordinary resolution reconstruction
   to the extended shared folder.
   Remove the superseded recursion and accumulation paths as their callers migrate.
5. Audit the remaining pass and source-unit boundaries for early exits over independent work.
   Reuse parser issues and checker diagnostics, collect failures from independently available sources or imports,
   and extend checking and later validators at their own recovery boundaries.
   Dependent lowering stages continue to require a valid preceding product.

Each migration must carry its complete diagnostic collection through session queries and CLI, TUI, and LSP presentation.
Update the producer and its callers together; a frontend
that displays only the first entry would leave the work incomplete.
This reporting direction extends across all passes, while the first four changes complete the three surface tasks.

Validation must distinguish accepted-program equivalence from recovery behavior:

| Area | Positive and boundary cases |
| --- | --- |
| Desugaring | Every binding flavor; annotated and unannotated telescopes; accepted and rejected destructor parameters; fresh classifier binders; retained origins; multiple independent child errors; no invented binders after rejection |
| Meta annotations | Accepted and rejected raw payloads and arguments; partial source binders; existential role placement; exact error spans; shared rejected terms reported once |
| Source analyzers | Separate versus composed facts and diagnostic collections; reachable versus abandoned allocations; term versus parameter annotations; multiple errors within and across categories; duplicate package/discovery errors; selected nested package roots |
| Resolution observers | Shadowing, exact cursor identity, documentation timing, shared provider reuse, independent arms, multiple unbound references in strict and completion analysis, strict/recovering agreement on valid sources |
| Dependency lifecycle | Forward references, nested block ownership, candidates with no edges, legal recursive groups, multiple rejected components, graph cleanup before continuing after a rejected block |
| Reporting and phase boundaries | Every diagnostic reaches the frontend with its source location; rejected inputs keep a failure exit status; no normal product reaches a dependent phase after rejection; no errors invented solely from recovery placeholders |

Retain count-based tests showing that the file-loading profile performs one reachability computation and one term sweep,
and that composing resolution observers does not add another resolution walk.
These establish work performed; compilation-time benefits still require measurement.
Compare diagnostics as collections, retaining multiplicity and source locations without asserting presentation order.
Pair rejected multi-error fixtures with valid counterparts to verify
that accumulation does not alter successful compilation.

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

High SPS has structural variable collection and validation clients,
but its normalizer propagates producer facts forward and demands backward through the same reconstruction.
Preserve the [consumer-demand dependency](../references/compiler.md#consumer-demands) when extracting its rule modules.
Its [lexical ownership checks](../../lang/stackir/src/high/check.rs) also reject repeated syntax ownership;
deduplicating visits must never hide that rejection.

The remaining work fits one migration sequence:

1. Complete the [surface migrations and their acceptance criteria](#migration-order-and-acceptance-criteria).
2. Introduce typed graph views and folders for substitution and finalization.
3. Apply the established interfaces to SPS analyses where they simplify concrete callers.

For each migration, compare separate and composed results and retain visit-count regressions on shared graphs.
Measure allocations and representative compilation time before making broader performance claims.
Run focused crate and source-case tests; use the full workspace suite only when explicitly requested.
Move each implemented, approved contract into its owning reference section and remove its settled design text here.

The remaining choices are the precise cross-phase folder interface, whether structural code generation pays for itself,
which consumers need independent pruning, and which measured workloads justify shared analysis inventories.
Resolve each at its first concrete client rather than committing the entire compiler to those choices in advance.
