# Type Inference Worklog

Status: paper design in progress; no checker implementation changes, 2026-08-02.

## Objective

Extend Zydeco's bidirectional checker with local type inference for unannotated value-pattern binders.
The checker remains the elaboration core; inference contributes fresh flexible metavariables and constraints,
then requires those metavariables to be resolved at a deliberate boundary.

The first experiment belongs in `lang/statics/type-system.typ`. It should make the proposed rules precise before
the Rust implementation changes.

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

## Current Checker Position

The surface `Pattern::Var` carries no sort. During pattern synthesis, `PatternAction::Syn` currently reports
`MissingAnnotation` for a variable without a previously recorded classifier. Unit and named wrappers can synthesize;
constructors and tuple spines require an expected classifier.

`FillId` already represents a flexible unknown, accumulates compatible candidates through `Lub`, and records the
visible `SkolemScope`. It is currently allocated from a surface term ID, so a pattern binder has no first-class
inference origin. `AbstId` separately represents rigid abstract identities and must remain distinct from `FillId`.

The current solver constrains a solution to its recorded skolem scope. Before the inference surface grows, it also
needs an explicit occurs check, reliable metavariable-to-metavariable scope merging, transactional constraint updates,
and diagnostics that retain the sites contributing each constraint.

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
of the paper experiment, but inference must be order-independent within that boundary.

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

Each flexible metavariable records its CBPV kind, inference origin, skolem scope, and closing level. Filling `?S` with
`S` is allowed only when `?S` does not occur in `S` and the free skolems of `S` are visible in the scope of `?S`.
Merging two flexible metavariables intersects their admissible scopes.

Constraint application must be atomic or recoverable. A failed speculative check cannot leave a partial fill that
changes later results. Conflicts should report the binder site and the incompatible body or call sites that supplied
the constraints.

## Paper Experiment

The prospective section of `lang/statics/type-system.typ` should add:

- declarations for fresh flexible value- and computation-type metavariables;
- a synthesizing value-pattern judgment for variables, unit, named patterns, and products;
- value-function synthesis from the inferred pattern schema;
- application synthesis that joins the inferred domain with a synthesizing argument;
- guarded fill and metavariable-merge rules with occurs and skolem-scope conditions;
- structural refinement rules for arrows, thunks, returns, and products; and
- an explicit closing rule that rejects remaining unconstrained metavariables.

These rules are prospective and must be visually separated from the implementation-derived calculus.

## Validation Cases

The implementation phase should cover:

- inference from one body use and from several compatible body uses;
- inference from one call site and from several compatible call sites;
- incompatible body uses and incompatible call sites with both origins reported;
- an unconstrained parameter, including a parameter that occurs without constraining its type;
- order independence for compatible constraints;
- explicit annotations producing the same checked core as before;
- direct and indirect occurs-check failures;
- metavariable merges across different skolem scopes;
- existential escape through an inferred domain or result;
- rejection of inference that would leak through an exported boundary; and
- constructors, packages, existential patterns, and recursive boundaries remaining annotation-directed.

## Deferred Direction

Whole-binding or whole-program constraint inference may later solve acyclic dependency groups and recursive SCCs
for which a sound annotation discipline has been chosen. Eligible value bindings may receive controlled
generalization. Polymorphic recursion, higher-rank types, exported interfaces, recursive type groups, and generative
package boundaries should retain annotations unless a later design gives them an explicit inference rule.

The main unresolved questions are the precise closing boundary, whether and how local call sites are collected before
closing, the elaborated form of any generalized CBPV value, and the ownership syntax required before constructor or
package-pattern synthesis can be considered.
