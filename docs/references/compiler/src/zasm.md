# C10. ZASM, Stack Analysis, and Local Representation Choices

[Assembly lowering](../../../../lang/assembly/src/lower.rs) consumes SPSLow into a control-flow graph
with explicit operand and control stacks, environment variables, labels, and instructions.
[StackAnalyzer](../../../../lang/assembly/src/analyze.rs) assigns the stack/environment locations needed by that graph.
The portable result is an `AssemblyProgram` for ZASM rendering and representation analysis.
Native lowering selects a distinct frame-aware path before preparation.

## CPS Assembly Lowering

Assembly lowering uses continuation-passing style (CPS): a value or pattern receives a consumer describing what
to compile after its stack operations, under the resulting context.
Some handlers also need a child's entry ID before they can finish their own construction.
The [assembly folder](../../../../lang/assembly/src/lower/folder.rs) represents these two kinds
of unfinished work separately.
A typed `Continuation` is a compilation consumer; a `Frame` resumes a caller after a child returns its entry ID.
This separation preserves instruction scheduling while allowing every syntax descent
to use the [shared folder driver](compiler-data.md#resumable-folder-execution).

Consumers are defunctionalized: records for subsequent values, patterns, instructions, computation bodies,
terminators, sequences, and branch tables replace captured Rust functions.
Internal `ContId` links refer to a flat per-run vector, independent of the assembly allocator.
Applying a consumer takes its slot once and moves its payload into the next operation.
Links are affine, meaning they can be used at most once: a value hole emits `Abort`
and can abandon an entire consumer chain, including an unfinished field sequence.
Unused records drop with the run without recursively following their links.

Labels are lowered before the tail that jumps to them, so every jump finds its target program.
A local label becomes one labeled program in the binding activation, compiled in the binding context;
its jumps push their stack and end in a static `Jump`, so several programs of one activation reach it.
A closed label becomes a fresh frame entry like a block, and its jumps clear the context before their static `Jump`.
`ContId` is neither `Copy` nor `Clone`, so applying a consumer also consumes its handle.
Consumption removes a vacant suffix from the vector, allowing subsequent consumers to reuse that storage.
Interior vacancies stay in place until consumers above them finish; outstanding IDs never shift.
Abandoned consumers remain occupied until the run ends, and the vector retains its allocated capacity.
This bounds storage across sequential completed steps without promising space proportional only
to live consumers for every schedule.
This assembly-local storage policy does not change the generic driver.

Instruction construction reserves a `ProgId` and queues a typed pending instruction.
Completion pops this queue in LIFO order, applies `ContextUpdate::{Keep, Bind, Clear}`,
and calls the instruction's consumer through `Step::Call` to obtain its successor ID.
It then publishes the instruction with the original context, before completing any new jobs queued by that consumer.
Obtaining an entry ID therefore does not imply that its body is published.
One driver run includes initial syntax lowering, root-entry marking, and draining the pending queue.
Tail transfers use `Step::TailCall`; blocks, native resume entries, branch arms,
and publication jobs retain explicit return frames only when they need a child's ID.
Consumers dispatch directly to the selected value, pattern, or computation rule.
Those syntax rules return their descendants as driver work; they do not invoke consumer application directly.
This acyclic call boundary avoids a redundant dispatch for the selected rule
while keeping syntax-depth-dependent execution on the driver.

Sequence cursors preserve the stack machine's ordering: value fields and primitive operands are pushed right to left,
patterns and alias members are consumed left to right, and a residual stack precedes its argument or tag.
Constructor patterns are consumed at coproduct match-arm heads before ordinary payload-pattern lowering.
A constructor encountered in an ordinary binder is an internal compiler error, not another branch to emit.
Closure words come from the shared machine model, and block parameters follow `EntryParameters::words()`.
Branch entries and their symbols are reserved in source order, while deferred arm bodies may finish in reverse order.
Extern discovery therefore remains attached to visiting the computation that requests it.
An ordinary block reserves its recursive symbol before lowering entry parameters;
its return frame fills that symbol and records the label after receiving the entry ID.
Native resume lowering allocates capture bindings before its child call, then creates the resume symbol,
records frame-entry metadata, and emits `RetainFrame` followed by the code address.

`LoweringPipeline` runs CPS lowering through the driver for both portable lowering and `with_native_frames()`;
representation analysis, stack analysis, and checked native preparation keep their own phase boundaries.
The [lowering tests](../../../../lang/assembly/src/lower/tests.rs) cover all representation policies,
recursive symbols, context resets, literals, primitives, external discovery, alias and branch order,
abandoned consumers, and rejected multiple irrefutable arms.
A saved trace from before defunctionalization fixes allocation slots, publication order, definition associations,
contexts, successor links, and native entry metadata, erasing only per-run key spaces.
Native pipeline tests prepare checked activation layouts, owners, slots, and liveness maps under every policy.

The [depth regression](../../../../lang/assembly/src/lower/tests/depth.rs) lowers
and drops 16,384-level argument/tag stacks, constructors, unboxed patterns, nested branch tables,
and portable/native continuation entries on a 256 KiB stack.
It also abandons and destroys a deep consumer chain at a value hole.
The [storage regression](../../../../lang/assembly/src/lower/folder/tests.rs) checks
that 16,384 sequential completed consumers retain the same vector capacity as 32 steps,
while preserving the emitted terminator.
These direct fixtures isolate folder execution from semantic validation and stack analysis.
The guarantee covers lowering's control flow and flat continuation teardown; context cloning,
retained assembly contexts, and the separate validators and analyzers still determine other costs.

## Product Layout and Local Unboxing

[ProductLayout](../../../../lang/assembly/src/syntax.rs) distinguishes logical arity from the physically stored fields.
Tuple tails and projections must respect that distinction; a suffix pointer refers into an existing payload.
Closure record layout is derived from the shared machine model rather
than a separately maintained field-order convention.
Changing packing must update patterns, closure opening, stack analysis,
and collector interior-pointer handling together.

[LocalUnboxing](../../../../lang/assembly/src/unbox.rs) runs over SPSLow
while producer/consumer relationships remain explicit.
It marks immediate product construction/elimination pairs, direct closure forcing, variable-bound products
whose uses are all suitable projections, and local closure bindings whose uses all open the closure.
Lowering then omits the corresponding pack/unpack pair or expands a variable into field slots.

The collector observes entry events
from the [shared SPSLow traversal](closure-conversion.md#spslow-traversal-and-analyzers).
Its variable-use folder reuses that traversal's child cursor with a different semantic view:
closed blocks and binding patterns are boundaries, while immediate projection
and closure-opening children receive their respective use roles.
Other value uses are escapes, including captures carried by closure environments and continuation residuals.
This preserves the closed-block rule without duplicating structural child enumeration.
Both walks run through the shared driver.
The use folder does not invoke the collector, so a candidate's subwalk introduces no recursive callback cycle.

Bounded tests cover static and dynamic policies, accepted expansions, escaping counterparts,
and the order of calls to a stateful policy.
Small-stack fixtures collect through 16,384 bindings and classify a 16,384-level nested escaping value,
including destruction of the arena and results.
The latter fixture isolates classification from semantic validation.
Variable candidates still require their own use scans.
Alias uses and escaping or unknown consumers retain the ordinary boxed representation.
Escape classification follows occurrences of the candidate variable through values and residual stacks;
an unrelated primitive, constructor, or closed block does not constitute an escape.
The explicit environment/result arguments of a jump are escaping uses
under the [word entry contract](closure-conversion/word-contracts-and-protocols.md#word-entry-contracts).
SPSLow's closed-block invariant places captures in explicit environments and continuation residuals.

## Policy Selection

[RepresentationPolicy](../../../../lang/assembly/src/representation.rs) separates a preference
from the analysis that justifies it.
The collector first establishes a compatible producer/consumer shape and the required local use evidence,
then asks the policy about an `UnboxingOpportunity`: its reason and field-word count.
Accepting every opportunity cannot waive an escape, width, or calling-contract restriction.
Rejecting a closure's expansion also prevents expansion of an environment transported inside that boxed closure.
Product fields retain their ordinary tagged-word representation.
[Scalar regions](native-runtime.md#scalar-value-boundaries) separately justify local raw arithmetic
without changing field layouts.

| Policy | Selected opportunities |
| --- | --- |
| `Boxed` | Keep residual product, closure, and scalar operation boxes after the selected SPS transformations. |
| `Direct` | Immediate product elimination and opening of a syntactic closure record. |
| `Local` (default) | `Direct`, plus a variable-bound product used only through compatible projections. |
| `Shared` (experimental) | `Local`, plus a variable-bound closure used only through closure openings. |

Each policy is a Rust type. `RepresentationStrategy` selects the same policies through a runtime enum.
`LoweringPipeline::with_representation` accepts a policy type or the enum and produces an immutable assembly program;
the policy is consumed during compilation and adds no runtime representation dispatch.
Custom Rust policies can restrict selection by reason or width without replacing the collector or the emitters.

`CommandCompiler::with_representation` and `BackendProgram::with_representation` provide per-compilation enum selection.
Changing a backend program's strategy invalidates its cached portable assembly.
Native frame preparation consumes the same policy before establishing its frame and root maps.
The CLI's `build --representation` applies to ZASM, AMD64 assembly/executables, and the Wasm backend.
SPS Wasm consumes its [scalar choice](native-runtime.md#scalar-value-boundaries);
product policies remain assembly-specific.
Zir rejects an explicit selection because it precedes physical representation lowering.
The old process-wide `ZYDECO_DISABLE_UNBOXING` switch has been removed; select `Boxed` explicitly instead.

The delivered analysis is local representation selection.
It does not implement the proposed three-way choice among unboxed fields, stack-allocated products,
and region-allocated products, or interprocedural escape propagation.
Those choices and their constraints remain in [escape and unboxing](../../../proposals/escape-unboxing.md).
Local analysis tests and core execution cases should verify both allocation removal and the uses that retain boxing.
The [policy experiment](../../../../cli/examples/representations.rs) compares residual allocation sites and field words,
including a Rust const-generic width limit; these are static code counts, not executed allocations or peak memory.
The [execution fixture](../../../../lib/tests/core/representation-policies.zy) exercises
a locally opened recursive closure with live captured values.
See [the CLI guide](../../../../cli/README.md#representation-experiments) for reproducible commands.

## Storage Evidence and Machine Calls

Source storage and compiler representation choices provide complementary evidence.
A logical `UInt8 * UInt32` can have an eight-byte natural encoding or a 64-byte aligned stored representation;
both still cross ordinary Zydeco calls through the established word convention.
Local unboxing removes cells where justified, without interpreting a source dictionary's padding or alignment.

| Description | Established evidence | Limit |
| --- | --- | --- |
| [Source `Representation A`](../../library/src/memory/manual-memory.md#typed-pointers-and-slices) | One layout witness shared by typed pointers and CPS operations | No selected register class or argument width |
| [Source `Plan A`](../../library/src/memory/layouts-and-aggregates.md#static-layout-plans) | Validated byte placement and inspectable field offsets | No type identity for each placement, reference map, or calling convention |
| SPSLow `ProductLayout` | Logical arity and producer/consumer structure | No byte padding or scalar register classification |
| [Word entries](closure-conversion/word-contracts-and-protocols.md#word-entry-contracts) | Ordered administrative environment/result words and code/record provenance | No different component transport or complete source stack protocol |
| [Partial source protocols](closure-conversion/word-contracts-and-protocols.md#partial-source-protocols) | Known components, scoped parameters, and regular recursive codata | No nominal storage identity, explicit type-application evidence, or physical stack extent |
| [Native frame plans](native-emission.md) | Live tagged-word slots, entry roles, and suspension ownership | No layout mixing raw scalar bits with managed references |

[Stored source calls](../../library/src/memory/views-and-buffers.md#stored-call-interfaces) share a carrier
and its dictionary without changing transport.
Selecting another machine ABI requires the
[remaining call-boundary work](../../../proposals/escape-unboxing.md#remaining-machine-call-boundary)
to establish representation identity, agreement at both ends, target placement, and tracing together.
