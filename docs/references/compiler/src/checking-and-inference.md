# C5. Typed Representation, Judgments, and Inference

The [checker guide](../../../../lang/statics/src/check/README.md) maps the implementation modules.
`Tycker` carries one source check's mutable state; `Tyck` and `PatternAction` select synthesis or analysis.
Term and pattern dispatchers own task stacks, allocation-site guards, prepared expectations, and source-fact recording.
Syntax-family rules should use those entry points so recursive judgments preserve the same administrative invariants.

[Typed syntax](../../../../lang/statics/src/syntax.rs) separates kinds, types, patterns, values, and computations.
`TermAnnId` pairs a sorted term with its classifier.
Kinds and types may initially contain fills; annotations, environments, nominal identities,
and source occurrences are recorded in the [statics arena](../../../../lang/statics/src/arena.rs).

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

`ValuePatternShape::is_static_opening` recognizes the checked form of a projection that binds only static components
and discards its runtime payload, excluding value binders, refutable patterns, and view applications.
The [static-only opening rule](../../language/src/bindings-and-scope.md) permits classifier continuations
with ordinary witness-escape validation.
Wholly manifest openings also return their checked value or computation continuation directly,
introducing no runtime let or payload evaluation.
An opening with fresh abstract witnesses retains its evidence for required static specialization.
The prelude regressions compare interpreter-input node counts with opt-out counterparts
and retain ordinary runtime-hole rejection, abstract-opening evidence, and the array-memory stack-overflow reproducer.

## Inference and Reuse

The [source inference rules](../../language/src/classification-and-inference.md) determine
where flexible value types arise.
`InferenceRegion` in [source.rs](../../../../lang/statics/src/check/source.rs) records inherited fills at entry;
closing the region rejects newly introduced pattern fills whose solutions remain incomplete.
A source closes independently before `CheckedTerm::reconcile` compares an import-site expectation.

[Compatibility](../../../../lang/statics/src/check/lub.rs) combines constraints through `Lub`.
[Shape refinement](../../../../lang/statics/src/normalize/inference.rs) creates component fills
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

## Checker Recovery

Checking can continue after an error when the next judgment's inputs are already established.
The checker owns typing environments and inference constraints, so its recovery points follow judgment dependencies.
A structural visitor alone cannot supply the missing classifier of a failed function or the scope of a failed binder.

The checker follows the [diagnostic contract](tooling.md#diagnostic-collection): each operation records its diagnostic
where the failure arises and returns `ReportedError`, which callers propagate without reporting again.
[CollectReported::collect_reported](../../../../lang/utils/src/err.rs) exhausts a sequence of independent requests
and returns all successful results in order only when every request succeeds.
Any failure rejects the combined result, so callers cannot accidentally publish an incomplete product or case tree.
A probe asks whether a judgment holds, as a classifier view or completion's annotation compatibility does.
Its failure answers the question instead of rejecting the program, so the probe keeps none
of the diagnostics of a failed attempt; `Tycker::speculate` runs such a probe.
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
The [diagnostic contract](tooling.md#diagnostic-collection) owns collection, deduplication, and frontend reporting.

Imported providers follow the
[source binding and inference boundaries](../../language/src/sources.md#source-boundaries).
If root judgments reject, `IndependentSources` uses the [scoped visitor](compiler-data.md#scoped-structural-traversal)
to collect reachable source and signature boundaries in postorder.
The checker then synthesizes boundaries that ordinary checking did not reach, visiting dependencies before importers.
This recovers a later imported source even when an earlier local binding prevented traversal of the importing body.
The existing source-boundary rules close inference and check signature requirements;
canonical synthesis outcomes prevent repeated provider work.
Successful programs incur no recovery scan.

These outcomes belong to one checker invocation; an edited source starts a new check against the current query inputs.
Rejected checks retain established static facts for diagnostics and tooling, but expose no checked root.
The [source diagnostics regressions](../../../../lang/tests/tests/diagnostics.rs)
and [session regressions](../../../../lang/session/src/source/tests.rs) cover independent errors, exact locations,
shared providers, unavailable binder scopes, rejected parent products, and correction through overlays.

## Witness Evidence and Field Lookup

A manifest entry checks its equation and substitutes it through the remaining telescope;
an abstract entry introduces an identity subject to scope checks.
Source formation and opening rules live in [L9](../../language/src/polymorphism-and-packed-values.md).
`functions` establishes canonical witness telescopes for dependent binders
and substitutes caller-visible evidence at application.
A runtime thunk's implementation can be unknown while its signature supplies the dependency.
Value-function witness projections separately record which structured argument components supply evidence.
Product routes select argument components; package leaves consume a static prefix.
An `Unpack` route records a physical prefix and continues into its payload, so witnesses separated
by ordinary fields or generated algebra structures keep their exact paths and order.
Routes with no remaining witness demand collapse to `Ignore`; a package opening
whose payload is ignored uses the same leaf form whether it came from source checking or translation.
`PackPiInstantiationState` consumes the physical leading existential prefix
and substitutes its abstract witnesses through the codomain.
`ValPi` has a separate projection-based argument representation;
its structured evidence handling does not extend `PackPi` to witnesses nested beneath arbitrary product fields.

[Projection checking](../../../../lang/statics/src/check/projection) resolves a unique route
through the receiver's allowed structure, including the identities shared by one selective opening of a packed value.
The resolved route records every product position and its product type.
Selected runtime fields elaborate to ordinary typed patterns; unselected static positions receive internal witnesses.
A whole-value alias carries the same opening's prefix for forwarding.
Witness inspection uses C6's static reducer and never obtains evidence by executing a computation.

## Classifier Folders

Substitution and finalization repeatedly rebuild the same classifier shapes.
The [classifier folder](../../../../lang/statics/src/fold.rs) owns that exhaustive structural reconstruction;
`LexicalSubstitution`, `AbstractSubstitution`, `HoleResolver`, and `FilledNormalizer` supply its local rules.
`TypeFolder::fold_children` consumes a raw `Type` and transforms its immediate children in field order.
It reads the inferred graph through `types_pre`; following solutions, unfolding definitions,
reducing applications or projections, and consulting normalized views remain explicit client operations.

A body callback receives `TypeScope`, distinguishing type abstractions, universal
and existential telescopes, value functions, and witness-dependent functions.
Domains precede their dependent bodies. The callback chooses the inherited substitution:
ordered abstract substitution removes formal witnesses under every binder, including universal
and existential quantifiers, and applies only later assignments to a replacement.
Telescope opening removes the selected binder before substituting its body.
A complete quantified type embedded in an argument retains its own binding scope,
even when a shared source type reuses the witness identity being instantiated outside it.
Lexical substitution uses the supplied lexical environment.
Binder pattern IDs, witness identities, and witness projections are preserved by structural rebuilding.

`TypeRebuilder` returns the original ID when children and kind are unchanged.
Changed paths allocate through `Tycker` with the caller's environment and classifier;
rebuilt labels transfer Builtin roles.
Nominal data and codata definitions receive fresh IDs only when their arms change.
Filled normalization keeps nominal definition IDs and finalizes their arm classifiers through the arena-wide loop.
This preserves the normalized lookup view attached to the existing arm IDs.

The folder introduces no implicit memo table. Substitution can depend on an environment or assignment suffix,
whereas finalization shares caches only after inference closes, as specified below.
Occurrence-sensitive support collection retains its own scope-aware traversal.
[Folder regressions](../../../../lang/statics/tests/folders.rs) check unchanged identity
and allocation counts on shared tails; [substitution](../../../../lang/statics/tests/substitution.rs),
[dependent arrows](../../../../lang/statics/tests/pack_pi.rs),
and [scope rejection](../../../../lang/statics/tests/existential_scope.rs) exercise the semantic client rules.

## Finalization

After inference closes, hole solutions are stable for the remainder of finalization.
[Resolution and normalization](../../../../lang/statics/src/normalize) share memo tables across every arena root,
rebuilding changed paths and reusing results for shared tails.
Original IDs remain valid lookup keys; `kinds_normalized` and `types_normalized` store changed finalized views,
while unchanged IDs expose their existing nodes.
Missing solutions and sort errors remain diagnostics.

The shared context bounds structural traversal by the graph's nodes and edges, plus reductions that create new nodes.
Starting a fresh memo at every root would repeatedly traverse shared signature tails.
Compiler-owned identity maps use the repository's fast internal hashing;
changing storage must preserve key-space identity.
Finalized annotations feed coverage, residualization, and editor facts.
[Inference fixtures](../../../../lang/tests/cases/inference),
[classifier-query fixtures](../../../../lang/tests/cases/typeof),
and [statics tests](../../../../lang/statics/tests) cover closure, scope, rejection, and reuse.
The Rust [inference tests](../../../../lang/tests/tests/inference.rs) retain import boundaries and numeric matrices;
[classifier-query tests](../../../../lang/tests/tests/typeof.rs) retain root classification and source dependencies.
