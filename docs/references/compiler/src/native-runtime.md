# C12. Shared Runtime Model, Allocation, and Collection

Three shared `#![no_std]`-capable crates factor runtime layout and host support out of compiler backends:
[zydeco-machine](../../../../lang/machine/src/lib.rs) defines tagged words,
closure records, host-transfer records, manual memory primitives, and frame actions;
[zydeco-abi](../../../../lang/abi/src/lib.rs) defines the canonical builtin role catalog,
calling modes, spare-box markers, C FFI/unit boundary specs, `RuntimeFailure`, and `HostIoErrorKind`;
and [zydeco-host](../../../../lang/host/src/lib.rs) implements the growable copying collector,
managed string layout, I/O handle table, OS entropy helper, and WASI runtime exports.
Code generation derives layout from 64-bit carriers; native stubs check their `usize` representation against them.
The bundled model sources determine fingerprinted process and library entry symbols,
so mismatched compiler/runtime source bundles fail to link.
This is artifact pairing, not verification of handwritten instruction selection.

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

Native preparation assigns checked, distinct homes for value words and raw bits.
The [AMD64 emitter](../../../../lang/amd64/src/emit/primitive.rs) initializes additional value homes
before allocation and places raw homes below an explicit collector root cursor.
Live box pointers can therefore move during collection while raw numeric bits remain unchanged.
The [Wasm emitter](../../../../lang/wasm-common/src/word.rs) uses distinct locals; its heap currently does not collect.
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

ZASM keeps the complete kernel in one instruction, consuming two addresses and its ordinary scalar arguments.
Native lowering loads once, executes the checked arithmetic sequence, and stores once;
literal and intermediate scalars introduce no boxes, callback objects, or successful-path calls inside the kernel.
Raw definitions have distinct temporary stack homes, with no collection inside that area.
Fatal arithmetic helpers do not return; required failures precede the store and later arithmetic.
This removes allocation sites without claiming register allocation or minimal spill traffic.
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

[RuntimeInstance](../../../../runtime/stub.rs) owns the managed heap, frame store, stack root bound,
host-transfer record, retained immutable allocations, I/O handles, invocation arguments, and host-owned strings.
A thread-local pointer dispatches helpers to the active instance; it owns no language state itself.
Entry saves its predecessor and installs a fresh instance, while normal completion restores
that predecessor and drops the completed instance before releasing its unit guard.
Nested calls into different units therefore preserve the suspended caller's heap, roots, transfer state, and resources.
The [source entry profile](../../language/src/foreign-interfaces.md#compiled-libraries-and-c-exports)
defines concurrency and fault behavior.

The process launcher in [runtime/main.rs](../../../../runtime/main.rs) uses the same instance API,
with process arguments and the established executable entry.
Reusable support disables the `process-entry` feature.
On normal C FFI completion, [runtime/stub.rs](../../../../runtime/stub.rs) resets the active instance's heap,
frame store, retained allocations, and I/O handles and parks it in a single-slot reusable `ParkedInstance` cache
for subsequent FFI calls into the library.
Semispace words and block indices are heap-allocated, avoiding multi-megabyte stack temporaries on each C entry.
Static string literals are emitted into 8-byte-aligned `.rodata` as `(byte_len: u64, utf8_bytes...)`;
dynamic strings allocate opaque GC-managed blocks on the same `(byte_len: u64, utf8_bytes...)` layout
so managed GC reclaims dead dynamic strings automatically.
String-allocating host builtins receive the live control-stack cursor and root live managed arguments
across any triggered collection, while CLI arguments remain cached outside the managed heap
as `StaticHeapString` blocks with no managed pointers.

## Environment Actions and Roots

[frames::Environment](../../../../lang/machine/src/frames.rs) exposes Enter, Suspend, Resume, and Roots actions.
Enter reserves before changing state and returns the possibly relocated active base.
Suspend validates captures and creates a tagged suspension ID; Resume validates its owner
and nesting, then restores the frame.
Roots returns mutable addresses for the active map joined with all pending suspension maps.
Only initialized live slots are roots; reserved capacity and dead tagged words are insufficient evidence of liveness.

The retained store grows geometrically and caches capacity at its high-water mark.
Only successful Enter may relocate retained frame words; Suspend keeps the active base stable.
Slot access uses raw pointers without constructing Rust references over generated-code storage.
Metadata vectors hold indices and checked suspension IDs, so their growth cannot invalidate logical frame references.
Failed word reservation preserves existing words, bases, and suspension IDs;
zeroed capacity does not count as an initialized binding.
Transitions may allocate Rust metadata but do not collect the managed heap;
metadata allocation still has the Rust allocator's ordinary failure behavior.
Root addresses expire at the next attempted environment transition, including a failed reservation.
Managed collection may rewrite those values but cannot relocate their published addresses.
A model declaration generates action headers and serialization order;
emitted records and target carriers share checked alignment.
A return prologue removes `suspension(F)` while preserving `result :: S`,
resumes the owner, restores `rbp`, and then binds the result.
Returning host and C calls enter that same prologue.

The [native environment proposal](../../../proposals/native-frames.md) compares alternative environment representations;
they do not extend the environment contract described here.

## Managed Allocation

The [native stub](../../../../runtime/stub.rs) uses [CheneyHeap](../../../../lang/host/src/gc.rs)
with two geometrically growing semispaces starting at `DEFAULT_INITIAL_SPACE_BYTES` (64 KiB) by default.
When live objects and an allocation request outgrow the current semispace,
collection doubles target semispace capacity (up to any configured ceiling in `CheneyHeap::with_limits`,
or unbounded by default for normal execution) and preserves the relocated live graph before completing the allocation.
Reusable instances shrink semispaces exceeding `REUSE_SHRINK_THRESHOLD_BYTES` (4 MiB) back
to initial capacity on `CheneyHeap::reset`.
Allocation receives a deferred root source: a successful fast allocation does not enumerate roots;
collection requests the control-stack range, registered host roots, and live frame slots and updates them in place.
An oversized request exceeding an explicit ceiling is rejected before enumeration;
a collection that still leaves insufficient room preserves the relocated live graph cleanly.

A block-start index finds the allocation owning a word-aligned interior payload pointer.
Copying installs forwarding information and preserves sharing, cycles, and the interior offset.
Cheney scanning traverses pointer-bearing payloads; opaque scalar bits are copied without tracing.
Unmanaged pointers remain stable.
Scanning order and forwarding must never reinterpret scalar bits as managed references.

Host helpers that can trigger collection must publish every live managed word and reload relocated roots afterward.
Raw borrowed C argument frames are discarded before any result-box allocation that can collect.
[GC regressions](../../../../lang/tests/tests/suite/native_gc.rs) cover roots, interior pointers,
aliasing, cycles, capacity failure, and allocation after collection.
Frame storage reclamation and managed-value liveness are separate obligations.
