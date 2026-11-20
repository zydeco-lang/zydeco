# C6. Typed Elaboration, Residualization, and Validation

Elaboration turns checked source mechanisms into a form shared by execution backends.
It runs in the unpublished arena so the source root and source facts remain available
while generated nodes acquire fresh identities and provenance.
The coarse check orders judgments, hole resolution, type normalization, coverage,
and static-root elaboration before publishing its result.

## Monadic and Copattern Elaboration

[Monadic checking](../../../../lang/statics/src/check/monadic.rs) checks the supplied `Monad` and `Algebra` basis,
synthesizes the payload once under its monadic environment, and passes a `CheckedTerm`
to the [algebra translation](../../../../lang/statics/src/elaborate/monadic).
The translator constructs lifted types, structures, and terms, rebinding their static evidence into the output context.
It dispatches the retained root to the existing type, value, or computation translation and preserves that sort.
The outer wrapper is a carrier-parameterized type abstraction, a carrier-and-dictionary value abstraction,
or the existing carrier-and-dictionary computation abstraction, respectively.
Kinds are rejected before nominal preparation.
It translates the retained checked payload rather than rechecking its source under unrelated expectations.
[L11](../../language/src/translations/relative-monads.md#relative-monads) specifies the source translation and basis;
generated code must satisfy the same finalization and residual representation boundaries as ordinary terms.
The [closed translation boundary](../../language/src/translations/algebra-translation.md#closed-translation-boundary)
requires explicit function and type parameters for external dependencies.
`TyEnv::monadic_new` still retains lexical type bindings;
the remaining [closure-validation repair](../../../todos/compiler-boundaries.md#monadic-closure-validation)
must distinguish closed definitions from captured witnesses before translation.
[Nominal translation](#nominal-algebra-construction) operates within that boundary.

[Value-function translation](../../../../lang/statics/src/elaborate/monadic/value_functions.rs) implements
[L11's structure-passing rules](../../language/src/translations/algebra-translation.md#polymorphic-value-functions)
for `ValPi`, `ValAbs`, and `ValApp`.
Type binders introduce fresh witnesses, preserve named-pattern shape,
and scope witness renaming separately from structure lookup.
An abstraction constructs its classifier and term from the same binder scope; its body
and generated application evidence are checked against the translated classifiers.
The child environment ends at the abstraction, so its structure associations cannot escape to siblings.

Package-opening value parameters reuse the existing layout-driven package-pattern translator.
Its layout counts physical static fields, including manifest entries,
while its canonical telescope counts abstract witnesses.
Ignored product components consume neither witnesses nor structures. The `Explicit` witness-route folder rebuilds routes
across the inserted structure fields using owned continuation frames.
[Field translation](../../../../lang/statics/src/elaborate/monadic/projection.rs) follows
the already resolved source route in a loop and inserts payload-selection steps beside those structures.
It retains translated product classifiers for residual layout without rerunning field search.

These semantic cases extend the existing recursive monadic traversal and reuse its type,
term, signature, structure, and package constructors.
They do not introduce a second whole-term pass or change the traversal's overall depth guarantees.
The payload is checked once, then translated, finalized, and statically eliminated.
The static evaluator can forward opaque runtime monad dictionaries through known value closures;
it never executes their structure thunks to recover a type witness.
Translated terms retain source provenance so residualization failures identify the source construct.
[Source fixtures](../../../../lang/tests/cases/monadic-value-functions) cover signatures, witness scope,
and rejection; [Rust assertions](../../../../lang/tests/tests/monadic_value_functions.rs) retain typed-arena invariant
and lowering checks.
The [State fixture](../../../../lib/tests/monadic/value-functions.zy) checks effect order
through the interpreter and the Wasm backend.

### Nominal Algebra Construction

[Nominal translation](../../../../lang/statics/src/elaborate/monadic/nominal.rs) implements
[L11's applicative families](../../language/src/translations/algebra-translation.md#applicative-nominal-translation).
Ownership comes from declaration identities bound in the resolved payload and its admitted closed source expansions.
A declaration's recorded source identity survives in its translated family.
An external seal or a local declaration capturing an external witness is rejected when translation reaches it;
an external type supplied through a payload parameter stays abstract.

Reachability and ownership are separate analyses.
The raw `CheckedNode` view in [typed traversal](../../../../lang/statics/src/traverse.rs) exposes terms, patterns,
classifiers, and erased type arguments without selecting executable replacements.
Declaration discovery follows those edges and admitted closed definitions.
The runtime graph intentionally omits the classifier evidence needed here.
Only reachable declarations and their dependencies become translation roots;
previously generated families do not become roots merely because their source owner is visible.

A `TypeFolder` analysis gathers free witnesses and nominal edges, preserving binder scope.
A finite fixed point propagates free dependencies through declaration edges.
The translator reserves every reachable target family, then rebuilds every body with explicit parameters
for the carrier, algebra constructor, and free type dependencies.
Only completed families enter the checker-local cache keyed by source declaration identity.
Bodies never capture the current monad implementation or an enclosing translated witness.
Failure may leave unpublished arena allocations for diagnostics, but cannot publish an incomplete executable root.

These analyses use explicit worklists over finite graphs.
Reconstruction uses a flat reserve-then-define schedule because nominal back-edges already denote reserved families;
there are no recursive reconstruction calls along declaration edges requiring `Explicit` continuation frames.
Ordinary classifier reconstruction reuses the existing monadic traversal and its depth limits.
Type applications reuse the declared constructor rather than generating specializations for recursive arguments.

Structure generation reserves one private codata dictionary and builds it with the existing computation `fix`.
Its methods quantify each declaration's free type dependencies and their structure evidence;
higher-kinded declarations additionally quantify their ordinary type arguments and structures.
Recursive requests select methods on the fixed dictionary instead of rebuilding their bodies.
Translated value and computation roots bind the dictionary thunk under their carrier and runtime monad arguments,
so dynamic evaluation and inlining use the same generated code and nominal families.
Type roots prepare the same applicative families but skip dictionary construction entirely.
[Root-sort regressions](../../../../lang/tests/tests/monadic_roots.rs) check that distinction, source imports,
typed-arena invariants, and shared lowering.

The `comatch` constructor accepts an expected classifier, exposes its codata shape to check each arm,
and retains the nominal classifier on the result.
This keeps codata introductions, observations, algebra signatures, and coverage hints consistent.
[Nominal fixtures](../../../../lang/tests/cases/monadic-nominal) cover mutual recursion, growing type arguments,
scoped and higher-kinded dependencies, packages, nested translation, and applicative equality and rejection.
[Rust regressions](../../../../lang/tests/tests/monadic_nominal.rs) retain typed-arena invariant checks,
lowering, source sharing, and bounded family counts on a 160-declaration cycle.
The [State fixture](../../../../lib/tests/monadic/nominal-types.zy) checks runtime dictionary selection,
finite protocol observations, and absence of eager effects in the interpreter, native backend, and Wasm backend.

### Copattern Elaboration

[Copattern elaboration](../../../../lang/statics/src/check/copattern.rs) follows the expected residual classifier:
a codata step groups destructor clauses, an arrow step introduces a shared argument
and accumulates its patterns, and a universal step introduces a type binder.
On reaching bodies, pending argument patterns become a correlated match.
A witness-dependent boundary currently admits one clause whose witnesses can scope the dependent result.
The output has one typed arm per destructor and hints identifying generated argument matches and witness binders.
Repeated source destructors can therefore be exhaustive alternatives over arguments
without becoming duplicate typed arms.

## Coverage

[Coverage validation](../../../../lang/statics/src/validate/coverage.rs) consumes normalized typed syntax
and its data/codata hints.
It converts variables, holes, and admitted alias groups to wildcards, preserves structural heads and product arities,
erases type witnesses, and treats literal observations as opaque to structural coverage.
Opaque patterns contribute no wildcard row.
This implements the conservative source policy in [L7](../../language/src/patterns-and-coverage.md).

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
[Coverage fixtures](../../../../lang/tests/cases/coverage) pair accepted matrices
with correlated gaps, empty types, and nested observation failures.
[Remaining pattern decisions](../../../proposals/exhaustiveness.md) cover usefulness, literal matching extensions,
and refutable conjunctions.

## Static Elimination

[Static elaboration](../../../../lang/statics/src/elaborate/static_values) uses a lexical evaluator
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
in [L10](../../language/src/static-elimination.md).
A residualization failure reports `tyck.static-elimination` at its source site;
exhausted witness inspection supplies no evidence.
Both linking and SPS lowering select the stored residual root.
[ExecutionReadiness](../../../../lang/statics/src/validate) rejects reachable executable holes before effects;
typed holes may still be inspected during ordinary checking.
[Static-elimination fixtures](../../../../lang/tests/cases/static-elimination) distinguish erased library structure,
runtime payloads, and failed evidence recovery.
[Rust regressions](../../../../lang/tests/tests/static_elimination.rs) inspect residual sharing and source facts,
retain invariant and lowering assertions, and generate boundary and reduction-limit matrices.

## Residual Runtime Traversal

Execution readiness asks which remaining values and computations can reach execution.
The [runtime graph view](../../../../lang/statics/src/traverse.rs) starts from the shared residual root selected
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
[Runtime traversal tests](../../../../lang/statics/tests/runtime_traversal.rs) pair shared reachable holes
with the accepted residual root that erases them; foreign-interface tests cover supplied implementations.

## Typed-Arena Invariants

A successful check leaves invariants that later phases rely on without rechecking.
Every kind and type cell has left its inference-hole form.
Every allocated value, computation, value pattern, type pattern, and type carries an annotation,
including allocations that inference retries abandoned.
Abstract-type witnesses are exempt, because their kind can come from their own table,
from a type node that denotes them, or from an enclosing binder.
Value and value-pattern annotations have kind `VType`, and computation annotations have kind `CType`.
No residual value or computation hole survives, except the placeholder of a foreign import,
and neither the root nor any recorded term annotation is a hole.

Tests check these invariants; compilation does not.
[`ArenaInvariants`](../../../../lang/tests/src/invariants.rs) reports every violation in a finished arena,
and `SourceCase::check_invariants` asserts that the arena of each source it checks has none.
[Mutation tests](../../../../lang/tests/tests/arena_invariants.rs) corrupt one fact of a checked fixture
and require the matching violation, paired with the unmodified fixture.
