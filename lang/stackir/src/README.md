# Stack IR (ZIR)

This crate implements the Zydeco Intermediate Representation (ZIR), a stack-passing IR that makes control flow
and continuations explicit while preserving a close mapping to Zydeco terms.

## Role in the pipeline

```markdown
textual -> bitter -> scoped -> tyck -> stack
```

The stack IR is built from one checked typed expression and is a convenient target for closure conversion and
later backend work. A completed `StackirProgram` separates its single computation `root` from the arena that stores
the root's nodes; auxiliary code is reachable through explicit closure and continuation syntax rather than a
top-level declaration collection.

## Key components

- The `syntax` and `arena` modules define the stack-passing AST, its node arenas, and the single-root program.
- `lower` translates typed syntax into stack form, preserving source mappings.
- `cps` translates `Ret`/`do` continuations into explicit thunk calls.
- `convert` performs closure conversion by making captures explicit.
- `substitution` and `free` provide utilities for in-place substitution and free-variable analysis in the stack IR.
