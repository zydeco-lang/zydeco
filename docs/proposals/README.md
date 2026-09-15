# Ongoing design proposals

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
| [Compiler memory](arena-gc.md) | Memo reclamation, compact facts, and consumer-driven normalization | [Retained analysis facts](../references/compiler.md#analysis-facts-and-materialization) |
| [Binary products](binary-products.md) | Unadopted right-spine alternative | [Current n-ary products](../references/language.md#5-values-products-and-data) |
| [Immutable bytes](bytes.md) | Functional updates, ownership-based reuse, and alternative representations | [Retained bytes](../references/language.md#immutable-owners-and-source-bytes) |
| [Foreign interfaces](c-ffi.md) | Code pointers, pointer results/exports, callbacks, and richer ABI shapes | [C imports and exports](../references/language.md#14-foreign-interfaces) |
| [Rust hosts](rust-host.md) | Rust consumption of C exports and native Zydeco interfaces, binding scope, and workload | [C exports](../references/language.md#compiled-libraries-and-c-exports) and [native units](../references/language.md#native-zydeco-units) |
| [Zydeco ABI](zydeco-abi.md) | Richer unit interfaces, shared initialization, and host bindings including optional UniFFI integration | [Native units](../references/language.md#native-zydeco-units) and [unit artifacts](../references/compiler.md#native-unit-artifacts) |
| [Completion](completion.md) | Structural candidates, snippets, and optional last-successful revision state | [Recovery](../references/compiler.md#recovering-parsing) and [completion](../references/compiler.md#completion-and-documentation) |
| [Source packages](package-management.md) | Explicit entry identity, shared project context, package validation, and relationship policy | [Source packages](../references/language.md#source-packages) and [sessions](../references/compiler.md#c3-source-loading-sessions-queries-and-memory-retention) |
| [Documentation](documentation.md) | Package-aware selection, public-subject routes, independent verification, standard-library coverage, and later authoring/client extensions | [Subjects](../references/compiler.md#documentation-subjects-and-provenance) and [publication](../references/compiler.md#documentation-publication-and-verification) |
| [Escape and representation](escape-unboxing.md) | Contification, interprocedural constraints, frame cells, allocation reuse, and layout-directed calls | [Local policies](../references/compiler.md#policy-selection) and [source stored calls](../references/language.md#stored-call-interfaces) |
| [Coverage](exhaustiveness.md) | Usefulness, refutable conjunctions/projections, and dependent copattern matrices | [Coverage checking](../references/compiler.md#coverage) |
| [Memory boundaries](memory.md) | Optional codecs, access/state operations, additional layouts, target facts, storage lifetimes, and low-level capabilities | [Layouts, fields, arrays, and views](../references/language.md#manual-memory) |
| [Streams](filesystem.md) | Growable memory-backed writers, incremental processing, seeking, asynchronous protocols, and native paths | [Stream capabilities](../references/language.md#streams-and-process-arguments) |
| [Native environments](native-frames.md) | Compact-storage and moving-root experiments, reclamation, backend integration, and detached control | [Retained activation lifetime](../references/compiler.md#activation-lifetime) and [default environment](../references/compiler.md#environment-actions-and-roots) |
| [Reachability regions](reachability-regions.typ) | Proposed support typing, region operations, and retirement obligations | [Current local representation](../references/compiler.md#c10-zasm-stack-analysis-and-local-representation-choices) |
| [Traversals](traversals.md) | Resumable classifier environments and surface rebuilding, owned semantic facts, remaining execution adapters, diagnostic recovery, performance validation, synthesized results, and independent pruning | [Traversal selection and composition](../references/compiler.md#choosing-and-composing-traversals), [resumable execution](../references/compiler.md#resumable-folder-execution), and [diagnostic collection](../references/compiler.md#diagnostic-collection) |
| [Typed-arena verification](tyck-lint.md) | Use-site evidence, finer witness provenance, and stronger checks | [Current lint](../references/compiler.md#typed-arena-lint) |
| [WebAssembly](wasm-backends.md) | Default-target selection, reclamation, optional features, and host ABI evolution | [Both current backends](../references/compiler.md#c13-webassembly-backends-and-embedding) |
| [Compiling memory abstractions](memory-compilation.md) | Storage/codec interfaces, explicit allocators, worked layouts, typed memory IR, CPS/component lowering, and cost verification | [Static erasure](../references/language.md#10-static-elimination) and [local representation](../references/compiler.md#policy-selection) |

Historical measurements retained by proposals are evidence for their open comparisons, with their original limits.
Their presence does not establish current performance or select a new default.
