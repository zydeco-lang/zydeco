# Primitive Calls, Memory Access, and Patterns

## Primitive Calls

A known primitive thunk initially has the form `closure • => extern f •`.
Recognizing it through aliases, projections, or constructor payloads exposes the external call even
at several call sites: only the operation identity is copied, with each site's original stack.
Known integer or float arithmetic with two visible arguments becomes `Primitive { operation, operands }`:

```text
extern add (arg(x) :: arg(y) :: (kont z => M)) ==> let z = PrimitiveAdd(x, y) in M
```

The argument stack's construction order evaluates the second operand before the first.
The remaining stack must satisfy the movement conditions above.
The result stays shared, and exposing its continuation as a binding avoids allocating a continuation record.
Literal folding obeys [L13's numeric rules](../../../language/src/primitives-and-capabilities.md).
A zero divisor remains a runtime operation at its original position, even if its result is unused.

When optional normalization is disabled, retained arithmetic calls execute through the ordinary host-call ABI.
The native runtime and WebAssembly host implement those calls with the same numeric rules
and spare-box contracts as their corresponding primitive instructions.

Escaping arithmetic retains its thunk interface, with an inline primitive in its body.
An unknown callee remains indirect.
Scalar comparisons, address calculation, and scalar memory accesses have the intrinsic bodies described below;
other known Builtin operations remain external calls.
The typed primitive survives SPSLow;
[scalar region lowering](../native-runtime.md#scalar-value-boundaries) then makes representation conversions explicit
for ZASM and direct Wasm instruction selection.

## Scalar Comparisons

Numeric `eq`, `lt`, and `gt` choose between two computations of the same arbitrary computation type.
Their primitive representation therefore exposes control flow directly, without constructing a source `Bool`.
The shared [comparison domain](../../../../../lang/syntax/src/primitive.rs) pairs a predicate with an integer
or float type; SPS and SPSLow use `CompareBranch { operation, operands, when_true, when_false }`.
An immediately enclosing stack let supplies one shared ambient stack to both successors.
The ownership checks require distinct lexical occurrences for operands and successors,
and low protocol checking verifies known operand types and both successors against that same stack.
The stack may include arguments or codata observations as well as a return continuation.

Builtin materialization gives every numeric comparison an intrinsic body,
including when normalization is disabled or the function escapes.
The body consumes two scalar arguments and two thunks, then forces the selected thunk.
Compiler provenance lets normalization expose this fixed body through aliases and field projections at several sites.
Known local branch thunks can then reduce under the ordinary closure and stack movement rules;
unknown callees and callbacks retain their thunk interfaces.

Literal comparisons select one successor only when operand evaluation is discardable and the shared stack can move.
Trapping operand or stack construction remains at its original position.
The [numeric semantics](../../../language/src/primitives-and-capabilities.md) apply at the declared width:
integer ordering follows signedness, floating-point equality treats signed zeros as equal,
and all three predicates are false when either float is NaN.
Constant folding and the interpreter share `ComparisonOp::evaluate`.

Closure conversion retains the comparison and its shared stack.
ZASM evaluates the second operand before the first, then its comparison terminator consumes both
and selects one successor.
AMD64 emits signed or unsigned conditional jumps, with an explicit unordered check for floating-point comparisons.
The Wasm backend emits typed comparison instructions and structured branches.
The comparison itself allocates no result and makes no host call.
It consumes ordinary scalar values, decoding existing wide scalar boxes at that boundary;
raw scalar regions do not yet extend through comparisons.
Source library wrappers that return `Bool` still use the ordinary return and coproduct machinery.

The [comparison regressions](../../../../../cli/tests/comparisons.rs) cover all numeric widths, NaNs, infinities,
signed zero, escaping functions, argument stacks, shared continuations, and generated instruction/import checks.
Native code and the Wasm backend execute with normalization enabled and disabled.
Folder regressions preserve trap ordering and pair valid branches with ownership and protocol errors.

## Address Calculations

Byte displacement is a pure calculation on an unmanaged address.
Builtin lowering materializes `memory_offset` as a closed function returning `AddrOffset { base, displacement }`,
even with optional normalization disabled.
Compiler provenance identifies this fixed builtin body, so normalization can expose it
through aliases and field projections at several call sites.
General source closures retain their existing sharing restrictions.
An escaping or unknown function keeps the ordinary thunk calling convention.

`AddrOffset` takes an `Addr` and a signed `Int` byte displacement and produces an `Addr`.
The SPSLow protocol checker distinguishes address evidence from numeric evidence and rejects known operand mismatches.
Partial protocol compatibility does not establish allocation validity or access permission.
The calculation wraps at the execution profile's address width, without dereferencing,
checking a range, or asserting an in-bounds pointer.
It preserves the base pointer's provenance in the interpreter.
Normalization removes a known zero displacement only when evaluating that displacement is total;
trapping operand evaluation remains at its original position.

Closure conversion preserves the operation.
Its ZASM instruction consumes the address and tagged displacement and produces one address word.
AMD64 decodes the signed `Int` and adds it directly to the address bits;
the Wasm backend uses wrapping 64-bit arithmetic on its virtual addresses.
The operation itself allocates nothing and makes no host call.
Its operands evaluate displacement first, then base, matching argument-stack construction.
This does not remove ordinary call or continuation costs at unknown boundaries.

The [address-offset regressions](../../../../../cli/tests/passes.rs) check zero-offset elimination,
signed offsets, wrapping without invalid dereferences, and execution with normalization enabled and disabled
in native code and the Wasm backend.

## Ordered Scalar Memory Accesses

Builtin lowering materializes integer and float `load_le`/`store_le` and unmanaged address-slot operations
as ordered `MemoryStep` computations, including when optional normalization is disabled.
A load binds one result before its successor; a store has only a successor.
The shared [access domain](../../../../../lang/syntax/src/memory.rs) identifies the scalar role
and load/store direction.
It determines the exact carrier width, numeric little-endian or native address interpretation, and access alignment one.
Stronger alignment, volatile accesses, and atomics require additional contracts.
The SPSLow checker validates known address, result, and stored-value protocols.

SPS normalization exposes the fixed builtin body through known aliases and field projections.
It preserves every access, including a load whose result is unused, and sequences the successor after it.
Each expanded load gets a fresh result binding. Reusing a builtin wrapper must preserve earlier observations
that remain live across later stores and loads of the same address.
Memory effects do not enter pure value commoning or arithmetic evaluation.
An unknown callback remains an ordinary thunk invocation after the access;
primitive recognition alone asserts no callback lifetime, uniqueness, or contification guarantee.

Closure conversion and ZASM preserve the ordered operation.
AMD64 emits one unaligned, exact-width load or store, with scalar decoding and encoding at the ordinary value boundary.
`Int` and `UInt` use eight-byte carriers but reject out-of-domain bits before publishing a loaded result.
Signed narrow loads extend their sign, and float loads/stores preserve IEEE bits, including NaN payloads.
Unmanaged address slots retain address semantics rather than becoming scalar integer conversions.
The caller remains responsible for allocation validity, initialization, and access permission.

Wide loads allocate an opaque scalar box when producing an ordinary value.
Their raw bits remain outside the GC root range across that allocation; existing live values remain roots.
Stores decode their ordinary input before writing.
[Raw memory kernels](../native-runtime.md#raw-memory-kernels) remove these conversions
when a bounded local chain closes at a store.

The Wasm backend calls `raw_*` memory imports with virtual 64-bit addresses and raw bits:
loads return one `i64`, and stores return nothing.
No source callback or spare box crosses these imports.
Generated code performs the ordinary value conversions and carrier validation.
The Node host retains its virtual-memory lookup cost; these operations do not address managed Wasm linear memory.
Interpreter adapters implement the same carrier and effect contract with their own memory representation.

The [scalar-memory regressions](../../../../../cli/tests/passes.rs) cover unaligned integer extremes,
little-endian bytes and adjacent sentinels, invalid carriers before callbacks and later effects,
float bit preservation through unknown callbacks, and live native roots during collection.
Both normalization modes and all three compiled targets are exercised.

## Pattern Decisions and Validation

A known scrutinee permits branch selection only after every earlier arm has been ruled out.
The [decision folder](../../../../../lang/stackir/src/high/normalize/decision.rs) compares a pattern
with borrowed producer facts and returns true, false, or uncertainty.
Alias members and product fields are considered in source order.
The first false or uncertain child ends that decision; a later mismatch does not override earlier uncertainty.
Unknown product shape remains uncertain, while distinct known constructor tags decide false.

The folder runs through the normalizer's driver.
Requests retain pattern IDs and borrowed fact views; product suffixes are slices of the existing physical fields.
They create no temporary product fact or reference-counted field copies.
Constructor payloads transfer directly to the next decision; alias and product cursors retain the next position.
Deciding a pattern creates no syntax or binder identities.

Bounded comparisons cover accepted, rejected, and uncertain decisions, suffix layouts,
empty and wide patterns, and early termination before later children are inspected.
An alias chain of 16,384 levels executes and drops on a 512 KiB stack.
A separate 8,192-level product comparison borrows facts whose fixture retains and releases each level explicitly.
That fixture establishes decision depth, not depth-independent destruction of production `KnownValue` trees.
Owned fact construction
and demand operations remain [separate work](../../../../proposals/traversals.md#normalization-fact-ownership).

Integer literal match plans use the `BuiltinValueRole::Integer(t, Eq)` intrinsic body
and its scalar comparison with success and failure continuations.
There is no structural literal-pattern node in SPS.
Structural aliases survive SPS and SPSLow; assembly saves and reloads their bindee, while direct SPS Wasm uses a local.
Resolved field routes already consist of ordinary structural patterns and erased witness evidence.

Normalizer and pipeline tests pair reductions with shared closures, traps, unknown branches, and suspended recursion.
[Demand tests](../../../../../lang/tests/tests/demand.rs),
[pattern fixtures](../../../../../lang/tests/cases/literal-pattern),
and [core fixtures](../../../../../lang/tests/tests/core.rs) exercise field pruning and compiled decisions.
A new reduction needs both a reducible example and a case where sharing, stack movement, or effects prevent it.
