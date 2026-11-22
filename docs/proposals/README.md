# Ongoing Design Proposals

These documents contain unimplemented extensions or unresolved choices
with concrete alternatives and validation criteria.
The [references](../references/README.md) own approved, implemented semantics, invariants, and their rationale.
When a proposal is implemented, transfer its settled account there and remove the superseded sections and links.
Exploratory questions from retired proposals live in [design ideas](../ideas/README.md).

Promotion requires completed behavior within the stated scope, including the claimed rejection cases and target support.
An experimental helper or partially integrated design is insufficient; keep that work here until its scope is complete.
References can identify current limitations and link to extensions without specifying those extensions
as current behavior.

| Proposal | Remaining work | Implemented starting point |
| --- | --- | --- |
| [Master architecture refactor](architecture-refactor.md) | Six-phase refactor across dense arenas, `zydeco-ir`, per-file Salsa frontend, shared `zydeco-host` (WASI), and `zydeco-cranelift` | [C1 architecture](../references/compiler/src/architecture.md) and the [reference ownership map](../references/README.md#rule-ownership) |
| [Constructor-aware normalization](constructor-normalization.md) | Safe branch selection through aliases, trivial or unused payloads, and evaluated payload recovery | [Local reductions](../references/compiler/src/sps/normalization-and-demand.md#local-reductions) and [sharing](../references/compiler/src/sps/normalization-and-demand.md#sharing-and-discardability) |
| [Join-point optimization](join-point-optimization.md) | Sharing-preserving case-of-case conversion, stack abstraction, and selective procedure expansion | [Second-class procedures](../references/compiler/src/sps/normalization-and-demand.md#second-class-procedures) and the short-circuit fixtures |
| [Procedure environment sharing](procedure-environments.md) | Proven same-activation native entries without repeated capture transport | [Procedure labels](../references/compiler/src/closure-conversion.md#correspondence-with-sps_l) and [native preparation](../references/compiler/src/native-emission.md) |
| [Foreign interfaces](c-ffi.md) | Code pointers, pointer results/exports, callbacks, and richer ABI shapes | [C imports and exports](../references/language/src/foreign-interfaces.md) |
| [Zydeco ABI](zydeco-abi.md) | Richer unit interfaces, shared initialization, and host bindings including optional UniFFI integration | [Native units](../references/language/src/foreign-interfaces.md#native-zydeco-units) and [unit artifacts](../references/compiler/src/native-emission.md#native-unit-artifacts) |
| [Escape and representation](escape-unboxing.md) | Contification, interprocedural constraints, frame cells, allocation reuse, and layout-directed calls | [Local policies](../references/compiler/src/zasm.md#policy-selection) and [source stored calls](../references/library/src/memory/views-and-buffers.md#stored-call-interfaces) |
| [Coverage](exhaustiveness.md) | Usefulness, refutable conjunctions/projections, and dependent copattern matrices | [Coverage checking](../references/compiler/src/elaboration-and-validation.md#coverage) |
| [Native environments](native-frames.md) | Alternative environment representations, reclamation, backend integration, and detached control | [Retained activation lifetime](../references/compiler/src/native-emission.md#activation-lifetime) and [default environment](../references/compiler/src/native-runtime.md#environment-actions-and-roots) |
| [Reachability regions](reachability-regions.typ) | Proposed support typing, region operations, and retirement obligations | [Current local representation](../references/compiler/src/zasm.md) |
| [Traversals](traversals.md) | Resumable classifier environments and surface rebuilding, owned semantic facts, remaining execution adapters, diagnostic recovery, performance validation, synthesized results, and independent pruning | [Traversal selection and composition](../references/compiler/src/compiler-data.md#choosing-and-composing-traversals), [resumable execution](../references/compiler/src/compiler-data.md#resumable-folder-execution), and [diagnostic collection](../references/compiler/src/tooling.md#diagnostic-collection) |
| [WebAssembly](wasm-backends.md) | Default-target selection, reclamation, optional features, and host ABI evolution | [The current backend](../references/compiler/src/webassembly.md) |
| [Compiling memory abstractions](memory-compilation.md) | Storage/codec interfaces, explicit allocators, worked layouts, typed memory IR, CPS/component lowering, and cost verification | [Static erasure](../references/language/src/static-elimination.md) and [local representation](../references/compiler/src/zasm.md#policy-selection) |

Historical measurements retained by proposals are evidence for their open comparisons, with their original limits.
Their presence does not establish current performance or select a new default.

## Short-circuit implementation order

Start with constructor-aware normalization, then use it to simplify the dispatch exposed by join-point optimization.
Together these should make the original short-circuit source produce the shared direct false target already achieved
by its manually commuted control.
Procedure environment sharing is a separate, later cost improvement for captured values;
it is not a prerequisite for obtaining that control-flow shape.
These are proposed implementation directions, not adopted changes to the current reference contracts or default plan.
