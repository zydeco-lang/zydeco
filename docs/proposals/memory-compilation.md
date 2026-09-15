# Compiling memory abstractions

The memory redesign gives the programmer control over payload layout, allocation, initialization, and release.
The remaining question is whether composing those operations adds machine work that a direct implementation can avoid.
This document records the gaps identified on 2026-09-14 and acceptance criteria for closing them.
It indexes unfinished work; the linked references continue to define current guarantees.

## Cost criterion and current boundary

Compare an abstraction with a direct implementation of the same observable behavior on the same target:
the same bounds and failure behavior, alias obligations, representation, and explicitly selected runtime dispatch.
Required dynamic checks, requested logical values, and deliberately materialized runtime metadata count as useful work.
Additional packaging, allocation, copying, dispatch, or retained control storage needs justification.
Matching the raw Zydeco API is an intermediate milestone because that path also has avoidable overhead.

[Static elimination](../references/language.md#10-static-elimination) already removes fixed recipes and type witnesses.
`Ptr L S` has no state or layout fields, and fixed fields need no carried displacement.
Ordinary products, operation thunks, captured values,
and continuation frames still follow the [compiler representation policy](../references/compiler.md#policy-selection).
The [small View comparison](../evaluations/2026-09-14-memory-abstractions/README.md) shows one fixed view
matching its raw baseline, while a library header view retains extra generated work.
Its counts are static code sites, not executed allocations or timings.

## 1. Specialize known operations across calls

Fixed layout construction can still produce ordinary operation packages containing integers and thunks.
Erasing the static recipe does not prove that all of those residual values disappear.
Current [local reductions](../references/compiler.md#local-reductions)
and [consumer demands](../references/compiler.md#consumer-demands) have limited sharing and call visibility;
they do not provide general recursive or interprocedural specialization.

Propagate known layout operations and callbacks through helpers, shared calls, and recursive loops.
Remove operation dictionaries, captured constants, and indirect calls when their selection is known.
Preserve explicit runtime selection at unknown boundaries, and bound specialization by code size and compilation cost.
The [escape and representation proposal](escape-unboxing.md) owns the supporting analysis;
a specialized entry must also respect its [machine-call boundary](escape-unboxing.md#remaining-machine-call-boundary).

## 2. Compile eligible CPS callbacks as local control flow

Residual success and failure thunks can require closure environments and retained activation storage.
The proposed [contification analysis](escape-unboxing.md#local-cps-continuations-proposed) would turn eligible known,
fully applied, nonescaping uses into blocks and jumps.
Compatibility of ambient stacks and entry contracts must survive lowering.
Host operations need explicit retention and invocation contracts before their callbacks qualify.

Preserve invocation multiplicity, effects, and escaping or unknown uses.
The [Ret/CPS convention](../references/language.md#ret-and-explicit-cps) does not prove purity to the optimizer,
single use, nonescape, cleanup, or bounded stack extent.
Frame reclamation belongs to the [native environment proposal](native-frames.md#remaining-decisions).

## 3. Remove unnecessary scalar and aggregate boxing

[Local unboxing](../references/compiler.md#product-layout-and-local-unboxing) removes some product cells
while retaining the tagged-word field convention.
Wide scalars can still box; pairs, fat handles, array builders, and closure environments can still allocate,
especially when passed or captured.
Selecting the layout of manually allocated payloads does not select the representation of these ordinary values.

Extend representation evidence across producers, consumers, calls, returns, recursion, and captures.
Support raw scalar registers and aggregate components where valid, with matching caller/callee contracts,
explicit conversion boundaries, and exact live-reference maps.
Source layout plans alone do not establish a machine ABI or reference-scanning contract.
The [machine-call proposal](escape-unboxing.md#remaining-machine-call-boundary) owns these prerequisites;
interprocedural escape and demand evidence must justify stack storage or cell elimination.

## 4. Lower memory primitives directly

Current native address arithmetic and scalar loads/stores use runtime calls.
For example, [offset](../../runtime/memory.rs) decodes a tagged integer, and wide scalar loads receive a spare box
under the [builtin contract](../references/compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls).
Even a known zero displacement can survive as a call in the header-view probe.

Introduce typed memory operations in the compiler IR and target lowering for address calculations and scalar accesses.
Expose known arithmetic and redundant checks to simplification while preserving required runtime validation.
Preserve byte width, little-endian interpretation, unaligned access, effects, aliases, and failure order.
Deleting or moving an access needs semantic evidence; neither an unsafe API nor `Ret` supplies it.
This extends the existing [primitive-call boundary](../references/compiler.md#primitive-calls).
Memory target and embedding costs must be assessed separately, including the current Wasm virtual-address host.

## 5. Make optional library work independently selectable

The current `Operations L A` couples storage geometry and allocation with whole-value codecs.
Array `Values` uses a managed list, so whole-array reads/takes construct that logical representation;
direct element access and `init_each` already avoid it.
A requested list is useful work, but embedding array storage in a record should not require selecting list conversion.

The [memory interface proposal](memory.md#independently-selectable-storage-operations) owns storage-only composition,
optional codecs, typed checked/unchecked indexing, and no-read state discard.
These choices must expose distinct obligations and behavior.
Omitting required checks from only one side of a benchmark does not establish zero-cost abstraction.

## 6. Verify and expose costs

Functional tests do not establish allocation, copying, dispatch, or space bounds.
The [representation comparison](../../cli/examples/representations.rs) counts portable product/closure
construction sites; it does not count their executions, all native allocations, or retained frames.
Add focused code-generation oracles and executed allocation/copy counters,
then measure representative optimized native workloads, peak retained storage, code size, and compilation cost.
Pair each abstraction with an equivalent direct implementation and keep target/profile information with results.

For selected hot paths, explore compiler diagnostics or enforceable requirements
for no residual implicit allocation or dispatch within a defined scope.
Such a contract needs explicit dynamic boundaries.
An explicit strict requirement must reject a path the compiler cannot establish;
ordinary compilation retains a correct fallback.
Do not infer a universal optimal-code guarantee from a few examples or from static erasure.

## Further control boundaries

The [memory proposal](memory.md#additional-control-and-storage-boundaries) also records missing capabilities:
packed and overlapping typed layouts, checked dynamic field paths, target facts, stack/static/arena storage conventions,
managed-reference storage, pointer operations, atomics, volatile access, and memory ordering.
These extend what programmers can express; they are distinct from overhead in operations already expressible.
That proposal links byte ownership/reuse, growable storage, foreign ABI work, and backend-specific costs.
The approved retained-`Bytes` policy remains deliberate.
Early specialization and lowering work requires no new source lifetimes or linearity rules.

## First acceptance target and order

Start with a typed indexed update using a known layout and callback.
Against a direct implementation with identical checks, it should perform the necessary bounds checks,
address calculation, one scalar load, and one scalar store, with no additional managed allocation,
operation dispatch, or callback packaging.
Also record avoidable costs shared by both paths so the raw baseline does not become the final ceiling.

1. Retain paired fixtures and establish code-generation and executed-cost baselines.
   Cover fixed and runtime layouts, zero-sized elements, overflow and out-of-bounds rejection,
   unchanged storage on rejected operations, and effect order.
2. Close the residual primitive, known-call, CPS, and local-representation gaps exposed by that case.
   Pair optimizable callbacks with unknown, retained, and repeatedly invoked counterparts;
   preserve aliases and general control behavior when optimization is unavailable.
3. Extend the same checks to loops, builders, modular calls, recursion, and mixed raw/reference values.
   Introduce component transport and stronger cost contracts only with the corresponding entry, lifetime,
   and collection evidence.

Treat each successful target as a scoped guarantee before expanding its domain.
