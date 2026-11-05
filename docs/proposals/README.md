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
| [Artifact boundary and mainline admission](artifact-boundary.md) | Owner decision on the core workspace, extras destination, and admission rule, then the staged split itself | [C1 architecture](../references/compiler.md#c1-architecture-and-a-programs-path-through-the-compiler) and the [reference ownership map](../references/README.md#rule-ownership) |
| [Checker query retirement](checker-query-retirement.md) | Re-measure the checker overhead, then remove the per-judgment producers and decide the derived-identifier registry separately | [Query ownership](../references/compiler.md#query-and-checker-ownership) and [inference and reuse](../references/compiler.md#inference-and-reuse) |
| [Constructor-aware normalization](constructor-normalization.md) | Safe branch selection through aliases, trivial or unused payloads, and evaluated payload recovery | [Local reductions](../references/compiler.md#local-reductions) and [sharing](../references/compiler.md#sharing-and-discardability) |
| [Join-point optimization](join-point-optimization.md) | Sharing-preserving case-of-case conversion, stack abstraction, and selective procedure expansion | [Second-class procedures](../references/compiler.md#second-class-procedures) and the short-circuit fixtures |
| [Procedure environment sharing](procedure-environments.md) | Proven same-activation native entries without repeated capture transport | [Procedure labels](../references/compiler.md#correspondence-with-sps_l) and [native preparation](../references/compiler.md#c11-native-preparation-activation-frames-and-amd64-emission) |
| [Foreign interfaces](c-ffi.md) | Code pointers, pointer results/exports, callbacks, and richer ABI shapes | [C imports and exports](../references/language.md#14-foreign-interfaces) |
| [Zydeco ABI](zydeco-abi.md) | Richer unit interfaces, shared initialization, and host bindings including optional UniFFI integration | [Native units](../references/language.md#native-zydeco-units) and [unit artifacts](../references/compiler.md#native-unit-artifacts) |
| [Escape and representation](escape-unboxing.md) | Contification, interprocedural constraints, frame cells, allocation reuse, and layout-directed calls | [Local policies](../references/compiler.md#policy-selection) and [source stored calls](../references/language.md#stored-call-interfaces) |
| [Coverage](exhaustiveness.md) | Usefulness, refutable conjunctions/projections, and dependent copattern matrices | [Coverage checking](../references/compiler.md#coverage) |
| [Native environments](native-frames.md) | Alternative environment representations, reclamation, backend integration, and detached control | [Retained activation lifetime](../references/compiler.md#activation-lifetime) and [default environment](../references/compiler.md#environment-actions-and-roots) |
| [Reachability regions](reachability-regions.typ) | Proposed support typing, region operations, and retirement obligations | [Current local representation](../references/compiler.md#c10-zasm-stack-analysis-and-local-representation-choices) |
| [Traversals](traversals.md) | Resumable classifier environments and surface rebuilding, owned semantic facts, remaining execution adapters, diagnostic recovery, performance validation, synthesized results, and independent pruning | [Traversal selection and composition](../references/compiler.md#choosing-and-composing-traversals), [resumable execution](../references/compiler.md#resumable-folder-execution), and [diagnostic collection](../references/compiler.md#diagnostic-collection) |
| [WebAssembly](wasm-backends.md) | Default-target selection, reclamation, optional features, and host ABI evolution | [The current backend](../references/compiler.md#c13-webassembly-backend-and-embedding) |
| [Compiling memory abstractions](memory-compilation.md) | Storage/codec interfaces, explicit allocators, worked layouts, typed memory IR, CPS/component lowering, and cost verification | [Static erasure](../references/language.md#10-static-elimination) and [local representation](../references/compiler.md#policy-selection) |

Historical measurements retained by proposals are evidence for their open comparisons, with their original limits.
Their presence does not establish current performance or select a new default.

## Short-circuit implementation order

Start with constructor-aware normalization, then use it to simplify the dispatch exposed by join-point optimization.
Together these should make the original short-circuit source produce the shared direct false target already achieved
by its manually commuted control.
Procedure environment sharing is a separate, later cost improvement for captured values;
it is not a prerequisite for obtaining that control-flow shape.
These are proposed implementation directions, not adopted changes to the current reference contracts or default plan.
