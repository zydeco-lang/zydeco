# Integer Literal Patterns

## Problem

Matching a number requires the Church-encoded comparison even for simple dispatch:

```zydeco
do equal <- ! int64/eq n 0;
match equal | +True() => ... | +False() => ... end
```

The pattern language can name every data constructor but no number.
This proposal admits integer literals as refutable patterns:

```zydeco
match n
| 0 => ...
| -7 => ...
| _ => ...
end
```

## Position under CBPV

A literal pattern adds no value-level elimination and moves nothing across the value/computation boundary.
Matching remains a computation whose arms are computations; the pattern itself is pure and refutable,
exactly like a constructor pattern.

Its meaning is the equality decision Zydeco already exposes: the arm `| k => c` selects `c`
when the integer equality branch, forced with the scrutinee and `k`, selects its true continuation.
The pattern is sugar over an existing computation, so `ValPi` totality and value views are untouched.

## Formation and Checking

```text
p ::= ... | integer-literal
```

A literal pattern checks only against a primitive integer type; the expected type is peeled
through named wrappers and inference fills, as for literal terms.
The literal is range-checked against the scrutinee's representation,
reusing the term-level `IntegerLiteral::with_type` judgment, so `| 300` against `Int8` is rejected as out of range.
Float, string, and character literals in pattern position are rejected: float equality
over NaN and signed zero needs its own decision, recorded below.

Literal patterns are always refutable, so they are rejected wherever irrefutable patterns are required,
such as value-function parameters.

## Coverage

Literal rows contribute no structural coverage, like the nested patterns of refutable views:
the scrutinee of a primitive integer type is only covered by a variable or hole row.
A literal-only match is therefore non-exhaustive until a catch-all arm closes it.

## Lowering

The reference interpreter compares the evaluated scrutinee against the pattern literal.

Compiled lowering rewrites each literal row, at the point where match plans become Stack IR, into a forcing
of the raw host equality branch (`BuiltinValueRole::Integer(t, Eq)`, the same closure the Builtin package materializes
for `eq`) with the scrutinee and the literal as operands and the success and failure plans as continuations.
No intermediate representation gains a pattern or instruction; demand analysis keeps literal-matched scrutinees whole.

## Remaining Uncertainty

- Float literal patterns are excluded pending a decision between IEEE equality and bitwise matching.
- Duplicate literal arms are not yet redundancy-checked; a later arm shadowed by an equal earlier literal is accepted.
