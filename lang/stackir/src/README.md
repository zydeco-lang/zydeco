# Stack IR (ZIR)

This crate implements the Zydeco Intermediate Representation (ZIR), a stack-passing IR that makes control flow
and continuations explicit while preserving a close mapping to Zydeco terms.

## Role in the pipeline

```markdown
textual -> bitter -> scoped -> tyck -> stack
```

The stack IR is built from one checked typed expression.
High-level lowering is indexed by the consuming stack and constructs the fragment of its scheme directly:
under branch-join, stack lets occur exactly around value-coproduct matches and scalar comparisons;
under duplicating, no stack let occurs and every arm consumes its own copy of the stack; under full,
a stack let names every pushed frame and every consumer receives the ambient stack.
Normalization combines local β/η-reductions with field-sensitive demand analysis,
resolving known calls and pruning unused bindings and package fields before allocation.
Known arithmetic calls become typed primitive values with direct result bindings,
and numeric comparisons have intrinsic branching bodies.
Closure conversion then consumes that lexical tree and produces a distinct `SpsLowProgram` with blocks,
jumps, and explicit closure and continuation packages.
Both representations have one computation root rather than a top-level declaration collection.

## Key components

- `syntax` holds the shared node forms, parameterized by phase-specific IDs,
  and `arena` holds shared builder and definition-name lookup traits.
  Each phase adds its own control-flow forms to the common syntax.
- `scheme` defines where stack-passing compilation names the current stack and the placement rule
  of each scheme's fragment, which `high::check` and `low::check` both apply.
- `high::syntax` and `high::arena` define lexical SPS; `high::lower` constructs a validated `FragmentProgram` directly
  from checked syntax.
- `high::copy` deep-copies a stack so that every arm of a duplicating branch owns its consumer.
- `high::variables` provides free-variable analysis, and `high::check` validates closed roots, lexical ownership,
  and placement in the scheme's fragment.
- `high::normalize` rebuilds SPS using local reductions and the consumer demands defined by `high::demand`.
- `low::syntax` and `low::arena` define first-order SPS with typed package forms.
- `low::entry` defines the explicit environment/result word roles consumed by block entries and supplied by jumps.
- `protocol` extracts partial source value/stack protocols.
  A shared graph retains recursive codata observations and owns their canonical tag numbering.
  Its source interpreter retains regular type-family instances using captured checked arguments.
  Scoped value/computation parameters preserve relationships for checking each polymorphic transfer.
  High rebuilding preserves retained evidence; closure conversion records it at low entries.
  `low::protocols` checks known call components against those partial source protocols,
  keeping continuation stack extent opaque.
- `low::convert` performs fresh structural closure conversion; `low::check` validates lexical ownership,
  the scheme's stack placement, and native continuation metadata.
  `low::contracts` checks code provenance and package agreement against the word entry roles.
- `SpsLowPipeline` composes lowering, SPS passes, and closure conversion into first-order SPSLow.

SPS and SPSLow implement the stack-passing-style paper's SPS and `SPS_l`.
