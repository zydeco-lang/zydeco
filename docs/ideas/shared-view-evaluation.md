# Shared view evaluation

[Grouping view arms](view-coverage.md) also raises an evaluation question.

Decide whether grouped arms should evaluate the transformation once.
Preserve arm order, bindings, and runtime value sharing; purity alone does not define an equivalence key or cost policy.

[L8](../references/language.md#8-value-functions-and-views) owns current value-function and view rules.
