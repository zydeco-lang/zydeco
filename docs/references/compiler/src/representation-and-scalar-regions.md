# C10. Local Representations and Scalar Regions

First-order closure-converted SPSLow ([`SpsLowProgram`](../../../../lang/stackir/src/lib.rs)) already makes allocation
candidates, closure captures, and stack frames explicit while retaining lexical scopes and producer-consumer structure.
Both native Cranelift emission and structured WebAssembly emission therefore run local unboxing
and bounded scalar-region planning directly over `SpsLowProgram`, without lowering
through an intermediate assembly graph.

## Product Layout and Local Unboxing

[`ProductLayout`](../../../../lang/ir/src/syntax.rs) distinguishes logical arity from physically stored fields.
Tuple tails and projections must respect that distinction; a suffix pointer refers into an existing payload.
Closure record layout is derived from [`ClosureField`](../../../../lang/machine/src/closure.rs)
in the shared machine crate rather than a separately maintained field-order convention.
Changing packing must update pattern binding, closure opening, Cranelift/Wasm emitters,
and collector interior-pointer handling together.

[`LocalUnboxing`](../../../../lang/stackir/src/low/unbox.rs) runs in `zydeco-stackir`
over `SpsLowProgram` while producer/consumer relationships remain explicit.
It marks immediate product construction/elimination pairs, direct closure opening, variable-bound products
whose uses are all compatible projections, and local closure bindings whose uses all open the closure.
Backends then omit the corresponding heap cell allocation or expand an unboxed variable into field words
across local SSA variables or Wasm locals.

The collector observes entry events
from the [shared SPSLow traversal](closure-conversion.md#spslow-traversal-and-analyzers).
Its variable-use folder (`Explicit` over `VarUseCollector`) reuses that traversal's structural child enumeration
with a different semantic view: closed blocks and binding patterns are boundaries,
while immediate projection (`ProductMatch`, `LetValue`) and `OpenClosure` children receive their respective use roles.
Other value uses are escapes, including captures carried by closure environments and continuation residuals.
Both walks run through the shared resumable driver so deep nests stay bounded on small stacks.

Bounded tests cover static and dynamic policies, accepted expansions,
escaping counterparts, and stateful policy call ordering.
Small-stack fixtures collect through 16,384 bindings and classify a 16,384-level nested escaping value,
including dropping the arena and results cleanly.
Alias uses and escaping or unknown consumers retain the ordinary boxed representation.
The explicit environment and result arguments of a jump remain escaping uses
under the [word entry contract](closure-conversion/word-contracts-and-protocols.md#word-entry-contracts).

## Policy Selection

[`RepresentationPolicy`](../../../../lang/stackir/src/low/representation.rs) separates a preference
from the analysis that justifies it.
`LocalUnboxing::with_policy` first establishes a compatible producer/consumer shape and the required local use evidence,
then queries the policy with an `UnboxingOpportunity` (its reason and field-word count).
Accepting every opportunity cannot waive an escape, width, or calling-contract restriction,
and rejecting a closure's expansion also prevents expanding an environment transported inside that boxed closure.
Product fields retain their ordinary tagged-word representation;
[scalar regions](native-runtime.md#scalar-value-boundaries) separately coordinate raw scalar arithmetic
without changing field layouts.

| Policy | Selected opportunities |
| --- | --- |
| `Boxed` | Keep residual product, closure, and scalar operation boxes after selected SPS transformations. |
| `Direct` | Immediate product elimination, syntactic closure opening, and intra-region scalar box elimination. |
| `Local` (default) | `Direct`, plus variable-bound products used only through compatible projections. |
| `Shared` (experimental) | `Local`, plus variable-bound closures used only through closure openings. |

Each policy is a Rust type, and `RepresentationStrategy` exposes the same four choices as a runtime enum.
`CommandCompiler::with_representation` and `BackendProgram::with_representation` provide per-compilation enum selection
without re-running frontend analysis or SPS lowering.
Custom Rust policies can restrict unboxing by opportunity reason or field width without modifying the collector
or either backend.

The CLI's `build --representation` applies to `exe`, `wasm-sps`, `object`, `staticlib`, and `sharedlib`;
`zir` rejects an explicit selection because SPSLow text precedes backend physical representation planning.
[ScalarPlans](../../../../lang/stackir/src/low/scalar.rs) consumes `RepresentationPolicy::scalar_boxing()`
alongside `LocalUnboxing` across both native Cranelift and WebAssembly emitters.

Local representation selection remains intraprocedural.
Multi-way selection among unboxed fields, frame cells, and region-allocated products,
as well as interprocedural escape propagation, remains in [escape and unboxing](../../../proposals/escape-unboxing.md).
The [policy experiment](../../../../cli/examples/representations.rs) reports static eliminated cells
and remaining payload words across all built-in policies and custom `FieldLimit` wrappers.
The [execution fixture](../../../../lib/tests/core/representation-policies.zy) exercises
locally opened recursive closures with live captured values across collection;
see [the CLI guide](../../../../cli/README.md#representation-experiments) for reproducible commands.

## Bounded Scalar Regions and Raw Memory Kernels

[`ScalarPlans`](../../../../lang/stackir/src/low/scalar.rs) groups bounded primitive chains
and closed load-arithmetic-store memory sequences once in `zydeco-stackir` so both `zydeco-cranelift`
and `zydeco-wasm-sps` consume identical verified representation plans:

- Ordinary wide arithmetic (`Int64`, `UInt64`, `Float64`) lowers
  through [`ScalarProgram`](../../../../lang/syntax/src/scalar.rs), which inserts explicit `Decode`/`Encode` steps
  around raw operations and cancels redundant internal box pairs while retaining trapping division/remainder order.
- Closed load-arithmetic-store chains compile through [`MemoryCall`](../../../../lang/stackir/src/low/scalar/memory.rs)
  and [`ScalarKernel`](../../../../lang/syntax/src/scalar/kernel.rs),
  keeping intermediate bits raw between one unaligned load and one unaligned store.

[Scalar value boundaries](native-runtime.md#scalar-value-boundaries)
and [raw memory kernels](native-runtime.md#raw-memory-kernels) define the verification rules, error ordering,
and backend lowering invariants.

## Storage Evidence and Machine Calls

Source storage and compiler representation choices provide complementary evidence.
A logical `UInt8 * UInt32` can have an eight-byte natural encoding or a 64-byte aligned stored representation
while crossing ordinary Zydeco calls through the established word convention.
Local unboxing removes cells where justified without interpreting a source dictionary's padding or alignment.

| Description | Established evidence | Limit |
| --- | --- | --- |
| [Source `Representation A`](../../library/src/memory/manual-memory.md#typed-pointers-and-slices) | One layout witness shared by typed pointers and CPS operations | No selected register class or argument width |
| [Source `Plan A`](../../library/src/memory/layouts-and-aggregates.md#static-layout-plans) | Validated byte placement and inspectable field offsets | No type identity for each placement, reference map, or calling convention |
| SPSLow `ProductLayout` | Logical arity and producer/consumer structure | No byte padding or scalar register classification |
| [Word entries](closure-conversion/word-contracts-and-protocols.md#word-entry-contracts) | Ordered administrative environment/result words and code/record provenance | No different component transport or complete source stack protocol |
| [Partial source protocols](closure-conversion/word-contracts-and-protocols.md#partial-source-protocols) | Known components, scoped parameters, and regular recursive codata | No nominal storage identity, explicit type-application evidence, or physical stack extent |
| [Cranelift module and liveness plans](native-emission.md) | `CaseLiveness` root maps, `CallConv::Tail` entry signatures, and raw scalar regions | No layout mixing raw scalar bits with managed references across general calls |

[Stored source calls](../../library/src/memory/views-and-buffers.md#stored-call-interfaces) share a carrier
and its dictionary without changing transport.
Selecting another machine ABI requires the
[remaining call-boundary work](../../../proposals/escape-unboxing.md#remaining-machine-call-boundary)
to establish representation identity, agreement at both ends, target placement, and tracing together.
