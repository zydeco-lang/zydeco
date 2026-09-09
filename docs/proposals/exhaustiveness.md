# Coverage and pattern extensions

Coverage must preserve relationships among fields.
Arms `(+True(_), _)` and `(_, +False(_))` mention both Boolean constructors but leave `(+False(_), +True(_))` uncovered.
This motivates the typed pattern matrix described in the [compiler reference](../references/compiler.md#coverage):
typing determines constructor spaces, product arities, and erased package components
before the matrix enumerates missing shapes.
The [language reference](../references/language.md#7-patterns-and-coverage) owns source acceptance and catch-all rules;
[high SPS lowering](../references/compiler.md#pattern-decisions-and-validation) owns compiled pattern decisions.

The remaining extensions change what a row can observe or how rows interact.
They should preserve correlated coverage and source-order fallthrough across interpreter and compiled execution.

## Usefulness and redundant rows

The current validator checks completeness without rejecting redundant rows.
A usefulness query can ask whether a row accepts any value that preceding rows leave uncovered,
using the same specialization operations as coverage.
The policy still needs to choose whether redundancy is an error, a warning, or an optional lint.
Repeated integer literals are accepted today;
their detection needs literal equality information beyond the structural matrix's opaque observations.
Pair complete matches with repeated, overlapping, and impossible rows when adding this query.

## Refutable conjunctions

Admitted alias groups contain only irrefutable patterns, so their coverage conversion is a wildcard.
Refutable conjunctions need an acceptance and fallthrough design before that conversion can change.
Several constructor members may refine the same payload or make the conjunction impossible;
failure of a later member must have a defined relationship to the following match arm.
Views introduce another observation whose duplication and ordering must remain explicit.
Review these cases together with usefulness checking
and the [shared-bindee demand rule](../references/compiler.md#consumer-demands).

An unrestricted binary as-pattern remains an alternative when only a whole-value alias
and one refutable member are needed.
The [syntax rationale](../style.md#pattern-alias-syntax) explains the current semicolon choice.
A syntax extension should first settle how many observations occur and which bindings remain available after failure.

## Additional literal forms

Float literal patterns remain excluded pending a choice between IEEE equality
and bitwise matching, especially for NaN and signed zero.
The choice affects both branch selection and whether two rows are redundant.
Supporting another literal family needs a coverage policy and a shared equality role usable by each backend;
it need not add a structural SPS pattern node.

## Dependent copattern matrices

Package-dependent arrows currently accept one copattern clause because the result can depend
on existential witnesses opened by the argument.
Several clauses require a dependent form of the generated argument match
that preserves those witnesses across alternatives.
Ordinary arrows, universal arguments, and nested destructor paths already use
[shared argument elaboration](../references/compiler.md#monadic-and-copattern-elaboration).

A proposed extension should show complete and incomplete clause matrices, witness non-escape,
and agreement between generated matching and direct package-dependent abstraction.
[Copattern checking](../../lang/statics/src/check/copattern.rs),
[coverage validation](../../lang/statics/src/validate/coverage.rs),
and [coverage regressions](../../lang/tests/tests/coverage.rs) are the implementation and validation entry points.

## Refutable projection payloads

Projection patterns currently require irrefutable payloads, including under `@[partial]`.
Supporting `/field = p` with a refutable `p` would need nested failure to reach the next arm
while retaining one package opening and the selected field's type evidence.
Review that together with the existing refutable conjunction questions;
accepted nesting must have a corresponding missing-case rejection.
[Refutable value-view coverage](../todos/deferred-designs.md#value-views) remains a separate todo.
