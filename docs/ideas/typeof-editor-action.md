# An editor action for typeof

Decide whether editors should offer an action constructing `@[typeof]` expressions.
This convenience would not identify the source construct with the REPL's display-only `@[type]` command.

[L4](../references/language.md#4-classification-and-inference) owns classifier extraction;
[C15](../references/compiler.md#interactive-engine) owns REPL inspection.
