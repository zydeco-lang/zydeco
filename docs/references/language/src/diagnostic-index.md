# Diagnostic Index

Rejection examples throughout this reference identify the intended `tyck.*` diagnostic code where relevant.
Diagnostic wording and rendering are implementation details.

## Diagnostic Families

| Diagnostic family | Relevant rule |
| --- | --- |
| Missing annotation/solution, unconstrained inference, occurs check | [Bidirectional Checking and Inference](classification-and-inference.md#bidirectional-checking-and-inference) |
| Invalid binding cycle, missing seal | [Mutual Recursion](bindings-and-scope.md#mutual-recursion) and [Nominal Seals](classification-and-inference.md#nominal-seals) |
| Sort/kind/type mismatch, unknown constructor/destructor | [Classification](classification-and-inference.md), [Values and Data](values-products-and-data.md), and [Computations](computations-and-control.md) |
| Refutable binding/alias/projection, coverage, overlapping copatterns | [Patterns and Coverage](patterns-and-coverage.md) |
| Missing/ambiguous/sealed field, unavailable/escaping witnesses | [Opening and Witness Scope](polymorphism-and-packed-values.md#opening-and-witness-scope) and [Field Search](polymorphism-and-packed-values.md#field-search-and-projection-patterns) |
| Static elimination | [Static Elimination](static-elimination.md) |
| Missing or ambiguous contextual `OS` witness | [Explicit Providers and Shared Openings](sources/core-prelude.md#explicit-providers-and-shared-openings) |
| Integer/float literal range errors | [Scalar Ranges and Conversions](primitives-and-capabilities.md#scalar-ranges-and-conversions) |

## Cited Diagnostic Codes

| Code | Trigger | Owning section |
| --- | --- | --- |
| `tyck.unconstrained-inference` | Unconstrained flexible classifier at an inference boundary | [Bidirectional Checking and Inference](classification-and-inference.md#bidirectional-checking-and-inference) |
| `tyck.typeof-kind` | `@[typeof]` applied to a kind operand | [Classifier Queries](classification-and-inference.md#classifier-queries) |
| `tyck.sort-mismatch` | Term sort incompatible with binding or translation context | [Classifier Queries](classification-and-inference.md#classifier-queries), [Outer Interface and Basis](translations/monadic-blocks.md#outer-interface-and-basis) |
| `tyck.refutable-binding` | Refutable pattern in an ordinary binding without `@[partial]` | [Irrefutability and Partial Binders](patterns-and-coverage.md#irrefutability-and-partial-binders) |
| `tyck.refutable-pattern-alias` | Refutable member inside a pattern alias group `(p; q)` | [Pattern Aliases](patterns-and-coverage.md#pattern-aliases) |
| `tyck.missing-named-field` | Field selection `v/field` with no matching public field | [Field Search and Projection Patterns](polymorphism-and-packed-values.md#field-search-and-projection-patterns) |
| `tyck.static-elimination` | Unresolved static value match, integer op, escaping value function, or reduction limit exceeded | [Static Elimination](static-elimination.md) |
| `tyck.escaping-declaration` | Local `def` declaration escaping the result type of a call-by-value block | [Sealed Declarations and Mutual Recursion](translations/call-by-value-and-call-by-name.md#sealed-declarations-and-mutual-recursion) |
| `tyck.missing-builtin-type-role` | Contextual `OS` with no visible `@[builtin(os)]` provider witness | [Explicit Providers and Shared Openings](sources/core-prelude.md#explicit-providers-and-shared-openings) |
| `tyck.ambiguous-builtin-type-role` | Contextual `OS` with multiple visible `@[builtin(os)]` provider witnesses | [Explicit Providers and Shared Openings](sources/core-prelude.md#explicit-providers-and-shared-openings) |
