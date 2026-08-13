# Stack IR (ZIR)

This crate implements the Zydeco Intermediate Representation (ZIR), a stack-passing IR that makes control flow
and continuations explicit while preserving a close mapping to Zydeco terms.

## Role in the pipeline

```markdown
textual -> bitter -> scoped -> tyck -> stack
```

The stack IR is built from one checked typed expression. Lowering is indexed by the consuming stack and constructs
the paper's branch-join fragment directly: stack lets occur exactly around value-coproduct matches. A completed
`StackirProgram` separates its single computation `root` from the arena that stores the root's nodes; auxiliary code
is reachable through explicit closure and continuation syntax rather than a top-level declaration collection.

## Key components

- The `syntax` and `arena` modules define the stack-passing AST, its node arenas, and the single-root program.
- `lower` translates typed syntax into a validated `BranchJoinProgram`, preserving source mappings.
- `cps` structurally translates `Ret`/`do` continuations into explicit thunk calls in a fresh arena.
- `convert` performs a fresh structural closure conversion that makes captures explicit without rewriting its input
  nodes.
- `variables` provides free-variable analysis, and `check` validates closed roots and branch-join placement.

The design target and the remaining transition to a distinct first-order `SPS_l` are recorded in
[`docs/ideas/paper-aligned-stackir.md`](../../../docs/ideas/paper-aligned-stackir.md).
