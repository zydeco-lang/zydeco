# C12. Shared Runtime Model, Allocation, and Collection

Three shared `#![no_std]`-capable crates factor runtime layout and host support out of compiler backends:
[zydeco-machine](../../../../lang/machine/src/lib.rs) defines tagged words, closure records,
manual memory primitives, and the [`clif` ABI contract](../../../../lang/machine/src/clif.rs);
[zydeco-abi](../../../../lang/abi/src/lib.rs) defines the canonical builtin role catalog,
calling modes, spare-box markers, C FFI/unit boundary specs, `RuntimeFailure`, and `HostIoErrorKind`;
and [zydeco-host](../../../../lang/host/src/lib.rs) implements the growable copying collector,
managed string layout, I/O handle table, OS entropy helper, and WASI runtime exports.
Code generation derives byte displacements from 64-bit carriers (`Word`
and [`ClifVmCtxHeader`](../../../../lang/machine/src/clif.rs));
target stubs verify their `usize` offsets with compile-time assertions.
The bundled `zydeco-machine` sources determine the fingerprinted
[`CLIF_ENTRY_SYMBOL`](../../../../lang/machine/src/clif.rs), so mismatched compiler
and runtime source bundles fail to link.
This is artifact pairing, not verification of generated machine instructions.

Odd words are immediate. Even words are pointer-shaped.
`Int`/`UInt`, narrower integers, `Float32`, characters, and tags fit in an immediate word.
`Int64`, `UInt64`, and `Float64` payloads use opaque scalar boxes.
Integer arithmetic wraps at the source payload width via typed inline primitive instructions emitted directly
by the compiler backends.
Products and closures occupy scanned blocks.
Source numeric domains and immediate ranges are specified in [L13](../../language/src/primitives-and-capabilities.md);
storage and C carriers are specified in [L14](../../language/src/foreign-interfaces.md#storage-and-foreign-transport).
Aligned host-owned objects outside the managed spaces remain unchanged by tracing.

## Scalar Value Boundaries

A primitive's numeric domain, ordinary value representation, and storage/C carrier answer different questions.
[`IntegerType`](../../../../lang/syntax/src/lib.rs) keeps those facts separate as `bits`,
`representation`, and `storage_bits`.
An eight-byte carrier therefore does not imply either a full-width source domain or an unboxed value word.

Every `Int64`, `UInt64`, or `Float64` at a general value boundary is a pointer to a one-word opaque box,
even when its bits could fit in an immediate.
This includes arguments, returns, products, captures, existential/polymorphic values, and native unit interfaces.
The payload contains raw bits and the collector never traces it; only the enclosing pointer is a root.
These compiler-owned primitive representations require no source meta annotation.

[Scalar regions](../../../../lang/syntax/src/scalar.rs) make representation changes explicit as `Decode`,
`Encode`, and raw `Arithmetic` instructions.
Their verifier derives each definition's `Value(T)` or `Raw(T)` representation, checks width and signedness
through its scalar type, rejects unavailable operands, and checks the declared ordinary result type.
Regions contain no calls, branches, or scanned-field construction.
Only a verified, immutable `ScalarProgram` reaches instruction selection.
Source typing and SPSLow protocol validation supply the input classifiers;
region verification checks their representation-preserving use, rather than reconstructing source typing.

The shared [SPSLow analysis](../../../../lang/stackir/src/low/scalar.rs) groups `Int64`, `UInt64`,
and `Float64` primitive trees and adjacent single-use primitive bindings, up to 32 operations per region.
Leaves must be scalar literals or variables.
Calls, branches, captures, shared bindings, and intervening constructions preserve ordinary value boundaries.
Arithmetic retains its evaluation order, including division/remainder failures.
The rewrite cancels `Decode(Encode(raw))`, removes unused conversions, and verifies the result again.
`Boxed` retains individual primitive boundaries; `Direct`, `Local`, and `Shared` enable this elimination.
The [execution regression](../../../../cli/tests/passes.rs) compares both policies with the interpreter
and all compiled backends, and counts the removed scalar allocation sites.

Cranelift keeps additional temporary values live inside virtual registers and stack root slots across allocations.
[`scalar::emit_scalar_call`](../../../../lang/cranelift/src/scalar.rs) evaluates inputs in operand order
before inline arithmetic and keeps raw bits in unspilled SSA registers outside the GC root slot (`roots_slot`).
Live box pointers can therefore move during collection while raw numeric bits remain unchanged.
The [Wasm emitter](../../../../lang/wasm-sps/src/word.rs) uses distinct locals; its heap currently does not collect.
Signed minimum divided by `-1` is handled before target overflow traps.
Host builtins still receive spare boxes before decoding arguments, so their conversion helpers do not collect.

Negative tests reject representation mismatches, raw region exits, and incorrect or aliased native homes.
Runtime regressions cover full-width C round trips, source-free unit calls across collection,
and numeric payloads equal to heap addresses.
These checks cover the implemented lowering; machine instruction selection remains trusted and tested.
[Further scalar unboxing](../../../proposals/escape-unboxing.md#further-scalar-unboxing) needs explicit evidence
at joins, source calls, captures, and mixed raw/reference layouts.

## Raw Memory Kernels

A [memory kernel](../../../../lang/syntax/src/scalar/kernel.rs) encloses raw scalar arithmetic between one load
and one store.
The shared [SPSLow planner](../../../../lang/stackir/src/low/scalar/memory.rs) recognizes `Int64`, `UInt64`,
and `Float64` loads followed by a primitive tree or adjacent single-use scalar bindings
and a store of the same scalar type, with at most 32 arithmetic operations and 32 intervening bindings.
Every eliminated scalar binding must be consumed inside the kernel.
Calls, branches, shared or escaping scalar results, other effects, and incompatible scalar types end eligibility.
An independent destination variable or bounded total wrapping address calculation may move to the entry.
Intervening `AddrOffset` bindings may also move there when their bases and displacements are independent
of the loaded value and scalar intermediates; their original order and bindings remain available to later uses.
These address bindings count toward the same 32-binding limit and may be shared because they are retained.
A possibly trapping displacement calculation retains ordinary lowering in its original order.
The boxed policy disables kernel formation; other scalar-eliminating policies enable it.

The immutable `ScalarKernel` has an explicit memory boundary: input zero is the loaded scalar,
other inputs are decoded ordinary scalar variables, constants become raw literals, and the raw result feeds the store.
Its verifier reuses the scalar arithmetic representation checker, requires matching load/store types,
and rejects unavailable or mismatched operands.
Its instruction set contains no source calls, managed values, scanned fields, or representation-changing steps.
Ordinary `ScalarProgram` inputs and results continue to require the value ABI.
No raw entry convention is inferred for a source function or unknown callback.

[`ScalarPlans`](../../../../lang/stackir/src/low/scalar.rs) keeps the complete kernel
in one `MemoryCall` consuming two addresses and its ordinary scalar arguments.
[`scalar::emit_memory_call`](../../../../lang/cranelift/src/scalar.rs) decodes ordinary arguments once,
loads from the unaligned little-endian address, executes raw Cranelift arithmetic instructions,
and performs one exact-width store, with no heap allocations or calls along the fast path.
Raw SSA temporaries remain untraced, and fatal trapping division helpers call `zydeco_clif_fail`
and abort before the store or later arithmetic.
This eliminates boxing sites in generated code without adding a separate assembly peephole pass.
Wasm uses raw locals and emits unaligned little-endian linear-memory load and store instructions directly
over WASI linear memory, with no virtual-memory host call or lookup cost.

The [regressions](../../../../cli/tests/passes.rs) pair optimized kernels with ordinary and unknown-callback paths,
check full-width wrapping and runtime operands across all backends, preserve error order,
and exercise the size limit, overlapping unaligned addresses, and retained pure address bindings.
The [typed field update](../../../../lib/tests/std/typed-memory-kernel.zy) composes record state transitions,
field projection, and a fixed view with these same scalar operations.
The [typed field/header comparison](../../../evaluations/2026-09-15-typed-memory/README.md) exercises this boundary
through std recipes and records whole-program allocation sites outside the allocation-free kernel.
Calls, joins, managed components, and broader contification remain
in [memory compilation](../../../proposals/memory-compilation.md#44-carry-raw-components-only-across-agreeing-entries).

## Runtime Instances

[`ClifRuntimeInstance`](../../../../runtime/clif.rs) places `ClifVmCtxHeader` at offset 0
and owns the [`CheneyHeap`](../../../../lang/host/src/gc.rs), growable SPS stack buffer (`sps_storage`),
[`RetainedMemory`](../../../../lang/machine/src/memory.rs), `HostIoRuntime`,
invocation arguments, and C reentrancy guard pointer.
A thread-local pointer tracks the active instance across C entry boundaries.
`zydeco_clif_entry_begin` saves any predecessor or parked state and installs a fresh or reset instance,
while normal completion restores that predecessor and releases the unit guard.
Nested calls into distinct compiled units therefore isolate their heaps, SPS stacks, transfer slots,
and I/O handles cleanly; [manual memory helpers](../../../../runtime/clif_memory.rs) expose `allocate`,
`free`, `retain`, `release`, `null`, and byte codecs over the same instance.

The process launcher in [`runtime/main.rs`](../../../../runtime/main.rs) initializes the runtime archive
with process arguments and invokes `zydeco_machine::clif::clif_entry`.
Library support builds disable the `process-entry` feature.
On normal C FFI exit, [`zydeco_clif_entry_end`](../../../../runtime/clif.rs) resets the active heap,
shrinks oversized SPS storage exceeding `SHRINK_SPS_WORDS` (8,192 words) back
to `INITIAL_SPS_WORDS` (1,024 words), clears retained memory and I/O handles,
and parks the instance in a thread-local `ParkedClifInstance` for reuse on subsequent calls.
Semispace buffers and block indices are heap-allocated, avoiding multi-megabyte stack frames on C entry.
Static string literals are emitted into 8-byte-aligned `.rodata` as `(byte_len: u64 LE, utf8_bytes...)`;
dynamic strings allocate opaque GC-managed blocks using the same layout
so Cheney GC reclaims unreachable strings automatically.

## Safepoint Roots and Control Stack

Every compiled `Case` maintains `sps_sp` in a virtual register and receives `vmctx` pointing to `ClifVmCtxHeader`.
Before any slow-path collection (`zydeco_clif_alloc_slow`) or collection-capable host helper,
generated code writes live `DefId` bindings and active temporary `Variable`s
into the function's explicit `roots_slot` and flushes `sps_sp` to `vmctx.sps_sp`.
Host control builtins write the target closure and up to two arguments
into `vmctx.transfer` ([`ClifHostTransfer`](../../../../lang/machine/src/clif.rs)), then return to Cranelift glue
in [`externs::emit_extern_call`](../../../../lang/cranelift/src/externs.rs), which grows `sps_storage` if needed,
pushes the transfer words, and executes `return_call_indirect` to the closure code pointer.

There is no separate Rust-managed activation frame stack (`frames::Environment` was retired
with the ZASM/AMD64 backend).
Continuation captures use ordinary heap-allocated `ContinuationPackage` records or explicit CPS closures tracked
through `sps_storage` (`sps_base..sps_sp`) and per-case `roots_slot`.

## Managed Allocation

[`ClifRuntimeInstance`](../../../../runtime/clif.rs) uses [`CheneyHeap`](../../../../lang/host/src/gc.rs)
with two geometrically growing semispaces starting at `DEFAULT_INITIAL_SPACE_BYTES` (64 KiB) by default.
When live objects and an allocation request outgrow the current semispace,
collection doubles target semispace capacity (up to any configured ceiling in `CheneyHeap::with_limits`,
or unbounded by default for normal execution) and preserves the relocated live graph before completing the allocation.
Reusable instances shrink semispaces exceeding `REUSE_SHRINK_THRESHOLD_BYTES` (4 MiB) back
to initial capacity on `CheneyHeap::reset`.
Inline allocation checks `alloc_limit - alloc_ptr` directly in Cranelift IR,
advancing the cursor in a few instructions without touching Rust or enumerating roots on the fast path;
only [`zydeco_clif_alloc_slow`](../../../../runtime/clif.rs) flushes roots and triggers collection
over `[sps_base..sps_sp)`, the active function's `roots_slot` slice (`roots_ptr`,
`roots_len`), and registered host roots, updating them in place.
An oversized request exceeding an explicit ceiling is rejected before enumeration;
a collection that still leaves insufficient room preserves the relocated live graph cleanly.

A block-start index finds the allocation owning a word-aligned interior payload pointer.
Copying installs forwarding information and preserves sharing, cycles, and the interior offset.
Cheney scanning traverses pointer-bearing payloads; opaque scalar bits are copied without tracing.
Unmanaged pointers remain stable.
Scanning order and forwarding must never reinterpret scalar bits as managed references.

Host helpers that can trigger collection must publish `sps_sp`, use deferred or registered root slots
for newly live objects, and read relocated words afterward.
Raw borrowed C argument slots stay outside the traced SPS stack range before result allocation runs.
[Host GC tests](../../../../lang/host/src/gc.rs)
and [native GC regressions](../../../../lang/tests/tests/suite/native_gc.rs) cover roots,
interior pointers, aliasing, cycles, capacity failure, and allocation after collection.
