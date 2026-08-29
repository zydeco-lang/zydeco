# Local Type Inference

## Motivation

Zydeco's checker is bidirectional: every binder carries an annotation, and every pattern is
checked against an expected type. That discipline gives predictable errors and a simple
elaboration core, but it taxes the smallest bindings. A helper that forwards one argument,
a tuple pattern that merely names components, or a thunk whose type is fully determined by
its body all demand annotations that state what the surrounding check already knows.

Local inference removes that tax without changing the elaboration architecture. The
bidirectional checker remains the engine; inference contributes fresh flexible
metavariables and constraints, then requires every metavariable to be resolved at a
deliberate boundary. Nothing is inferred across an exported interface, and no
annotation-directed form changes meaning.

## Delivered Rules

- **Binder default.** A bare unannotated pattern variable in a synthesizing binder position
  defaults to a value pattern: it creates a fresh flexible metavariable `?A : VType`, records
  the pattern as its inference site, and binds the variable at `?A` while the body is checked.
  Type and kind parameters remain annotation-directed.
- **Constraint lifetime.** Body uses and compatible call sites constrain `?A` through the
  least-upper-bound operation. While the defining block is checked, the inferred domain
  retains `?A`; at the closing boundary — a block, an imported source, or the root source —
  every metavariable must be solved, deliberately generalized, or rejected as unconstrained.
  Inference is order-independent within each boundary, so exported interfaces cannot depend
  on later downstream uses.
- **Pattern synthesis.** Variables, unit, named patterns, and ordinary tuples synthesize.
  Constructors, package patterns, existential openings, and other patterns whose ownership or
  telescope cannot be recovered unambiguously remain checked against an expected type.
  Explicit annotations keep their existing behavior and take precedence.
- **Shape refinement.** Shape-directed operations may refine a flexible metavariable only to
  the structural shape they require — value-to-computation arrows, thunks, returns, and
  products — creating fresh component metavariables of the required CBPV sorts and recording
  the corresponding constraint. Refinement never guesses a data or codata definition,
  constructor owner, existential package, nominal seal, or package-dependent telescope.
- **Solver invariants.** Filling `?S` with `S` is allowed only when `?S` does not occur in `S`
  and the free skolems of `S` are visible in the scope of `?S`. Merging two flexible
  metavariables intersects their admissible scopes. A failed speculative check cannot leave a
  partial fill; conflicts report the binder site together with the body or call sites that
  supplied the incompatible constraints.

The formal model lives in `lang/statics/type-system.typ`, which adds the prospective rules to
the implementation-derived CBPV calculus; the Rust checker implements its local, monomorphic
core.

## Deferred Direction

Whole-binding or whole-program constraint inference may later solve acyclic dependency groups
and recursive SCCs for which a sound annotation discipline has been chosen. Eligible value
bindings may receive controlled generalization. Polymorphic recursion, higher-rank types,
exported interfaces, recursive type groups, and generative package boundaries keep their
annotations until a later design gives them an explicit inference rule. The unresolved
questions are whether closing should move from lexical blocks to whole binding groups, how
constraint origins should be retained, the elaborated form of any generalized CBPV value, and
the ownership syntax required before constructor or package-pattern synthesis can be
considered.
