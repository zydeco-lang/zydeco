# Stack IR (ZIR)

This crate implements the Zydeco Intermediate Representation (ZIR), a stack-passing IR that makes control flow
and continuations explicit while preserving a close mapping to Zydeco terms.

## Role in the pipeline

```markdown
textual -> bitter -> scoped -> tyck -> stack
```

The stack IR is built from one checked typed expression.
High-level lowering is indexed by the consuming stack and constructs the paper's branch-join fragment directly:
stack lets occur exactly around value-coproduct matches.
Normalization combines local β/η-reductions with field-sensitive demand analysis,
resolving known calls and pruning unused bindings and package fields before allocation.
Closure conversion then consumes that lexical tree and produces a distinct `SpsLowProgram` with blocks,
jumps, and explicit closure and continuation packages.
Both representations have one computation root rather than a top-level declaration collection.

## Key components

- `sps::syntax` and `sps::arena` define lexical high SPS;
  `sps::lower` constructs a validated `BranchJoinProgram` directly from checked syntax.
- `sps::variables` provides free-variable analysis, and `sps::check` validates closed roots, lexical ownership,
  and branch-join placement.
- `sps::normalize` rebuilds high SPS using local reductions and the consumer demands defined by `sps::demand`,
  following the [residual normalization rules](../../../docs/proposals/normalization.md#residual-sps-normalization).
- `sps_low::syntax` and `sps_low::arena` define first-order SPS with typed package forms.
- `sps_low::convert` performs fresh structural closure conversion;
  `sps_low::check` validates the resulting lexical ownership and retained branch-join invariant.
- `SpsLowPipeline` is the consuming boundary between high SPS and assembly-ready SPSLow.

The paper correspondence and the phase invariants are summarized in the implementation architecture section
of [`DESIGN.md`](../../../DESIGN.md); the stack-passing-style paper remains authoritative for the formal presentations.
