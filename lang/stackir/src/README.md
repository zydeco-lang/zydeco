# Stack IR (ZIR)

This crate implements the Zydeco Intermediate Representation (ZIR), a
stack-passing IR that makes control flow and continuations explicit while
preserving a close mapping to Zydeco terms.

## Role in the pipeline

```
textual -> bitter -> scoped -> tyck -> stack
```

The stack IR is built from typed syntax and is a convenient target for
closure conversion and later backend work.

## Key components

- The `syntax` and `arena` modules define the stack-passing AST and its arenas.
- `lower` translates typed syntax into stack form, preserving source mappings.
- `convert` performs closure conversion by making captures explicit.
- `substitution` and `free` provide utilities for in-place substitution and
  free-variable analysis in the stack IR.
