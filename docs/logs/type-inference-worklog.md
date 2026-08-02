# Type Inference Worklog

Status: first local-inference implementation complete and validated, 2026-08-02.

## Objective

Extend Zydeco's bidirectional checker with local type inference for unannotated value-pattern binders.
The checker remains the elaboration core; inference contributes fresh flexible metavariables and constraints,
then requires those metavariables to be resolved at a deliberate boundary.

The paper experiment is formalized in `lang/statics/type-system.typ`. The first Rust slice now implements its local,
monomorphic core while retaining the bidirectional checker as the elaboration engine.

## Prior Static-Semantics Work

The current checker has been formalized in `lang/statics/type-system.typ` as an implementation-derived CBPV calculus.
The document uses the following settled notation:

- `K`, `S`, `A`, and `B` range over kinds, arbitrary types, value types, and computation types.
- `V` and `M` range over values and computations; `N` ranges over unsorted terms.
- `P`, `Q`, `R`, and `U` range over value, type, kind, and static patterns.
- One letter denotes one syntactic sort; numeric subscripts distinguish different instances of that sort.
- `/` denotes projection and substitution, while `\/` denotes the least-upper-bound constraint operation.
- The fat downward arrow is reserved for shape views, not least upper bounds.

The grammar and each rule figure use the same display-math font size. Context syntax occupies two lines,
static projections and substitutions use an inline slash, and the monadic algebra premise has its own line.

## Starting Checker Position

The surface `Pattern::Var` carries no sort. Before this experiment, `PatternAction::Syn` reported
`MissingAnnotation` for a variable without a previously recorded classifier. Unit and named wrappers can synthesize;
constructors and tuple spines require an expected classifier.

`FillId` already represented a flexible unknown, accumulated compatible candidates through `Lub`, and recorded the
visible `SkolemScope`. It was allocated from a surface term ID, so a pattern binder had no first-class
inference origin. `AbstId` separately represents rigid abstract identities and must remain distinct from `FillId`.

The solver constrained a solution to its recorded skolem scope, but lacked an explicit occurs check, recoverable
constraint updates, and a pattern-aware origin for unsolved-variable diagnostics.

Recursive block SCCs currently elaborate recursive sealed types. Term recursion remains explicit through `fix`.
Consequently, the first call-site inference experiment applies only to acyclic value bindings.

## Settled Short-Term Design

### Binder default

A bare unannotated pattern variable in a synthesizing binder position defaults to a value pattern. It creates a fresh
flexible metavariable `?A : VType`, records the pattern as its inference site, and binds the variable at `?A` while
checking the body. Type and kind parameters remain annotation-directed.

The question is whether a metavariable is constrained, rather than whether its source variable is used. A variable
may occur only in a way that contributes no useful constraint, and a later call site may still constrain its domain.

### Constraint lifetime

Uses in the body and compatible call sites contribute constraints through `\/`. The inferred function domain retains
`?A` while the defining block is checked. At the chosen block or source-interface boundary, every metavariable must be
solved, deliberately generalized, or rejected as unconstrained.

Exported interfaces must not depend accidentally on later downstream uses. The exact closing boundary remains part
of the broader design; this slice closes at blocks, imported sources, and the root source. Inference must be
order-independent within each boundary.

### Pattern synthesis up to constraints

The initial synthesizing fragment contains variables, unit, named patterns, and ordinary tuples. A bare tuple is an
ordinary product pattern. Constructors, package patterns, existential openings, and other patterns whose ownership
or telescope cannot be recovered unambiguously continue to check against an expected value type.

Explicit annotations retain their existing behavior and take precedence over the value-pattern default.

### Shape refinement

Shape-directed operations may refine a flexible metavariable only to the structural shape they require. The first
shapes are value-to-computation arrows, thunks, returns, and products. Refinement creates fresh component
metavariables of the required CBPV sorts and immediately records the corresponding `\/` constraint.

Shape refinement does not guess a data or codata definition, constructor owner, existential package, nominal seal,
or package-dependent telescope. Those forms remain annotation-directed.

### Solver invariants

The formal model gives each flexible metavariable a CBPV kind, inference origins, skolem scope, and closing level.
The first implementation records one source site and its skolem scope, while region snapshots provide local
ownership. Explicit origin sets and closing levels remain deferred. Filling `?S` with `S` is allowed only when `?S`
does not occur in `S` and the free skolems of `S` are visible in the scope of `?S`. Merging two flexible
metavariables intersects their admissible scopes.

Constraint application must be atomic or recoverable. A failed speculative check cannot leave a partial fill that
changes later results. Conflicts should report the binder site and the incompatible body or call sites that supplied
the constraints.

## Paper Experiment

The prospective section of `lang/statics/type-system.typ` adds:

- declarations for fresh flexible value- and computation-type metavariables;
- a synthesizing value-pattern judgment for variables, unit, named patterns, and products;
- value-function synthesis from the inferred pattern schema;
- application synthesis that joins the inferred domain with a synthesizing argument;
- guarded fill and metavariable-merge rules with occurs and skolem-scope conditions;
- structural refinement rules for arrows, thunks, returns, and products; and
- an explicit closing rule that rejects remaining unconstrained metavariables.

The section remains visually separated because symmetric fresh merges, origin unions, generalization, and broader
constraint collection are still prospective.

## First Rust Implementation

`InferenceSite` now distinguishes term holes from pattern-origin metavariables. A bare variable synthesized as a
value pattern receives a fresh `VType` metavariable; unit, named, and ordinary tuple patterns synthesize
componentwise. Explicit type and kind annotations still take precedence.

Function domains retain those metavariables while their enclosing region is checked. Blocks, imported source
boundaries, and the root source close the pattern metavariables created within them. A nested region may solve a new
metavariable to one inherited from its parent, while a call outside a closed block or imported source cannot complete
the earlier inference.

Arrow and product views now refine an unresolved metavariable before destructuring it. Existing thunk and return
checking already constructs the required shape and contributes the same `Lub` constraints. Refinement components
inherit their source site and skolem restrictions; data, codata, constructor, package, and existential ownership is
never guessed.

Filling now performs direct and indirect occurs checks. Failed joins restore the previous solution and fill-scope
tables, and metavariable aliases can be constrained in either order without forming a cycle. Unconstrained pattern
metavariables receive a dedicated diagnostic at their pattern sites.

Focused tests cover body and call-site inference, multiple compatible uses, body and call-site conflicts, unused
binders, explicit annotations, ordinary and named patterns, all four structural shapes, alias ordering, region
closure, constructor-pattern rejection, self-application, and existential escape. The full `zydeco-statics` and
`zydeco-tests` suites pass.

## Validation Cases

The implemented test matrix now covers:

- inference from one body use and from several compatible body uses;
- inference from one call site and from several compatible call sites;
- incompatible body uses and incompatible call sites;
- an unconstrained parameter, including a parameter that occurs without constraining its type;
- alias constraints applied in either order;
- explicit annotations retaining their previous checking behavior;
- direct and indirect occurs-check failures;
- existential escape through an inferred domain or result;
- rejection of inference that would leak through an exported boundary; and
- constructors, packages, existential patterns, and recursive boundaries remaining annotation-directed.

Before inference expands further, diagnostics should retain every contributing constraint origin, and dedicated
tests should exercise metavariable merges across distinct nonempty skolem scopes.

## Deferred Direction

Whole-binding or whole-program constraint inference may later solve acyclic dependency groups and recursive SCCs
for which a sound annotation discipline has been chosen. Eligible value bindings may receive controlled
generalization. Polymorphic recursion, higher-rank types, exported interfaces, recursive type groups, and generative
package boundaries should retain annotations unless a later design gives them an explicit inference rule.

The main unresolved questions are whether closing should move from lexical blocks to whole binding groups, how
constraint origins should be retained, the elaborated form of any generalized CBPV value, and the ownership syntax
required before constructor or package-pattern synthesis can be considered.
