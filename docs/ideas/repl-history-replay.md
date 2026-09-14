# REPL history and replay

Define persistence and replay around immutable numbered sources, including working-directory-relative imports.
An edited historical entry should receive a new identity; replay must state which effects it executes again.

[C15](../references/compiler.md#interactive-engine) owns the current submission and retry model.
