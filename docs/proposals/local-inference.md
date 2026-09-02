# Local Type Inference

Zydeco's checker is bidirectional: every binder carries an annotation, and every pattern is
checked against an expected type. Local inference contributes flexible metavariables and constraints
within a deliberate term boundary. Every metavariable introduced by that boundary must be resolved
before the boundary closes, and annotation-directed forms retain their ordinary meaning.

## Rules

- **Binder default.** A bare unannotated pattern variable in a synthesizing binder position
  defaults to a value pattern: it creates a fresh flexible metavariable `?A : VType`, records
  the pattern as its inference site, and binds the variable at `?A` while the body is checked.
  Type and kind parameters remain annotation-directed.
- **Constraint lifetime.** Body uses and compatible call sites constrain `?A` through the
  least-upper-bound operation. While the defining block is checked, the inferred domain
  retains `?A`; at the closing boundary — a block, an imported source, or the root source —
  every metavariable must be solved or rejected as unconstrained. Inference is order-independent
  within each boundary.
- **Source closure.** A source root is synthesized in its own inference region after its imports
  and optional companion annotation have been assembled. An expected classifier at an import site
  is compared with the synthesized result; it cannot constrain metavariables inside the source.
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

The formal rules live in `lang/statics/type-system.typ` alongside the implementation-derived CBPV calculus.
