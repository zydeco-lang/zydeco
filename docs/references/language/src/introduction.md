# Zydeco Language Reference

This reference specifies the current source language and its execution boundaries.
It is organized into five parts: foundations (syntax, scope, and checking), core call-by-push-value (values,
computations, and patterns), abstraction and staging (value functions, packed values, and static elimination),
surface translations (`@[monadic]`, `@[cbv]`, and `@[cbn]`), and program boundaries (sources, packages,
primitives, foreign interfaces, and execution profiles).

It assumes basic familiarity with typed lambda calculi, polymorphism, and operational semantics.
The [language guide](../../../tutorial/zydeco-guide.md) provides a walkthrough introduction,
the [standard library reference](../../library/src/introduction.md) specifies canonical library contracts
for effects and manual memory,
the [compiler implementation reference](../../compiler/src/introduction.md) documents the phase representations
and algorithms implementing these rules, the [rule ownership map](../../README.md#rule-ownership) connects contracts
across all three references, and [CONTRIBUTING](../../../../CONTRIBUTING.md) covers command-line workflows.
