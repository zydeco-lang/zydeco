# Stack IR (ZIR)

This crate implements the Zydeco Intermediate Representation (ZIR), a stack-passing IR that makes control flow
and continuations explicit while preserving a close mapping to Zydeco terms.

## Role in the pipeline

```markdown
textual -> bitter -> scoped -> tyck -> stack
```

The stack IR is built from one checked typed expression. High-level lowering is indexed by the consuming stack and
constructs the paper's branch-join fragment directly: stack lets occur exactly around value-coproduct matches.
Closure conversion then consumes that lexical tree and produces a distinct `SpsLowProgram` with blocks, jumps, and
explicit closure and continuation packages. Both representations have one computation root rather than a top-level
declaration collection.

## Key components

- `sps::syntax` and `sps::arena` define lexical high SPS; `sps::lower` constructs a validated
  `BranchJoinProgram` directly from checked syntax.
- `sps::variables` provides free-variable analysis, and `sps::check` validates closed roots, lexical ownership, and
  branch-join placement.
- `sps_low::syntax` and `sps_low::arena` define first-order SPS with typed package forms.
- `sps_low::convert` performs fresh structural closure conversion; `sps_low::check` validates the resulting lexical
  ownership and retained branch-join invariant.
- `SpsLowPipeline` is the consuming boundary between high SPS and assembly-ready SPSLow.

The paper correspondence and implementation history are recorded in
[`docs/logs/paper-aligned-stackir.md`](../../../docs/logs/paper-aligned-stackir.md).
