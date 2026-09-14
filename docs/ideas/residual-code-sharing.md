# Residual code sharing

Measure code growth from repeated static value-function applications before adding residual-code factoring.
Compare direct block sharing with inlining while preserving captured runtime values and effect multiplicity.
This is an optimization of the residual program, not a new runtime representation for `val pi`.
[C6](../references/compiler.md#static-elimination) owns elimination;
[C8](../references/compiler.md#sharing-and-discardability) owns residual sharing constraints.
