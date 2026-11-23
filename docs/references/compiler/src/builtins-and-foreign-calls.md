# C14. Builtin Contracts, Primitive Operations, and Foreign Calls

The [ABI crate](../../../../lang/abi/src/lib.rs) (`zydeco-abi`, re-exported
by `zydeco-syntax`) owns the canonical `#![no_std]` builtin role catalog, host-call modes (`Returning` vs `Control`),
spare-box policies, C FFI/unit boundary descriptors, `RuntimeFailure` codes, and `HostIoErrorKind`.
[Static Builtin validation](../../../../lang/statics/src/builtin.rs) checks the authored
[Builtin signature](../../../../lib/std/builtin.zy) against those roles.
Interpreter linking and [SPS Builtin lowering](../../../../lang/stackir/src/builtin.rs)
materialize the validated structural plan.
Host calls retain `BuiltinValueRole` through dynamic and Stack IR syntax.
The role supplies arity and calling mode; emission derives the external symbol at the target ABI boundary.

Canonical representation types have shared intrinsic identities;
opening a provider's packed value introduces witnesses for its resource capabilities.
Named structural routes and static fields erase before backend layout.
[L13](../../language/src/primitives-and-capabilities.md) owns source observations,
and [module interfaces](../../language/src/polymorphism-and-packed-values.md#module-interfaces-and-shared-openings)
explain dependency choices.
Returning and continuation-selecting operations have distinct host call plans.
C8 owns [arithmetic exposure](sps/operations-and-patterns.md#primitive-calls)
and [scalar comparisons](sps/operations-and-patterns.md#scalar-comparisons);
C11–C13 own the resulting target words and calls.
The interpreter shares scalar evaluation with constant folding.

Strings are immutable UTF-8 text. `Bytes` is a source-defined abstraction
whose [owning design](../../library/src/memory/views-and-buffers.md#immutable-owners-and-source-bytes)
specifies its representation and operations.
The compiler and host recognize general memory capabilities, without a byte-sequence type or operation family.

[Manual memory](../../../../lang/machine/src/memory.rs) is shared by the interpreter,
the native host, and the [WASI host](../../../../lang/host/src/wasm.rs).
`Address` wraps a raw pointer; `MemoryLayout` validates allocation requests using the platform allocator layout.
Allocation is uninitialized, and explicit release receives the exact base and original layout.
Native and WASI `Addr` words carry linear pointer bits directly (sign-free `i64` zero-extended on `wasm32`,
addressed via inline little-endian linear-memory loads and stores).
The tracing collector leaves unmanaged addresses outside its managed semispaces unchanged;
raw storage must not contain unrooted managed references.
There are no grant records, liveness tables, pointer-slot maps, or initialization bitmaps.
`RetainedMemory` owns explicitly transferred immutable allocations until runtime teardown.
It reserves retention bookkeeping before accepting ownership, so failure leaves ownership with the caller.
[Manual Memory](../../library/src/memory/manual-memory.md#manual-memory) owns caller obligations
and the distinction between static evidence and runtime data.
The source [view](../../../../lib/std/memory/view.zy),
[field](../../../../lib/std/memory/field.zy), [record](../../../../lib/std/memory/record.zy),
and [array](../../../../lib/std/memory/array.zy) factories use these same primitives.
C6 eliminates fixed view functions and field recipes; runtime-selected views and layouts use ordinary source thunks.
`Fields` states and layout witnesses introduce no runtime tags or new builtin roles.
Array whole-value reads explicitly build managed logical contents; direct initialization and indexing do not.

Scalar `store_le` takes an address, value, and completion; `load_le` takes an address and result successor.
These roles use [ordered scalar memory accesses](sps/operations-and-patterns.md#ordered-scalar-memory-accesses),
which own their instruction selection, representation adapters, and carrier validation.
They have no CPS host-call or hidden spare-box ABI.
The [source codecs](../../../../lib/std/memory/codecs.zy) write products directly into destination fields.
Padding contributes no load or store. Fixed realization requires statically evaluable size and alignment;
dynamic realization captures the required placement in its source operations.
Types and state witnesses erase, while explicit queries may materialize constants and ordinary closures may allocate.

String imports and primitive I/O reads return a retained address and byte count through a two-argument successor.
UTF-8 decoding validates the bytes in a caller-established readable range.
I/O writes receive a writer, raw address, count, error successor, and completion.
The source system library mediates these raw interfaces through its shared packed value for immutable bytes.
Byte codecs validate exact lengths before raw scalar reads.

Argument lookup returns one string or selects the missing branch; it retains no Zydeco continuation.
The native host caches argument strings outside the managed heap, with no managed references in the snapshot.
The [source argument library](../../../../lib/std/system/arguments.zy) supplies traversal and lazy tails.
These use the ordinary closure, activation, and collection protocols, including reuse and abandonment.
The former native host closure and fixed host-root table have been removed.
[Argument regressions](../../../../lib/tests/builtin/argument-contract.zy) exercise repeated forcing
across collection, live captured `Float64` values, discarded tails, and invalid indices on all four backends.
Source observations belong to [L13](../../language/src/primitives-and-capabilities.md).

[HostIoRuntime](../../../../lang/host/src/io.rs) and OS entropy helpers
in [zydeco-host](../../../../lang/host/src/random.rs) are shared by the interpreter,
the native stub, and the linked WASI host.
Host resource tables allocate monotonically increasing handle IDs and validate reader and writer operations.
Closing removes the resource, so every alias subsequently observes `Closed`;
identifiers are not source-visible pointers.
Reserved standard-stream capabilities remain open.
The interpreter backs them with injected streams, while native and WASI executables use the process streams.
Primitive success/error branches carry a result or an error kind and message;
[the library](../../../../lib/std/README.md#streams-and-files) constructs `Either`, `Option`, and typed paths.
Adapters distinguish EOF, empty data, invalid text, I/O errors, and closed resources. Stream extensions and
resource-lifetime choices remain [open ideas](../../../ideas/README.md#growable-byte-builders-and-incremental-streams).

## Foreign Calls

[ForeignSignature](../../../../lang/statics/src/foreign.rs) is a checked call plan for a returning C thunk.
Arguments are supported integers or `Addr`. An address contributes one raw pointer.
Bindings supply any length or capacity as a separate integer.
At most six C arguments are accepted. Results are supported integers or `Unit` (C `void`).
Its constructor enforces the flattened bound, and the validated fields remain private.
Expansion yields ordered `ForeignArgument` entries identifying the source parameter and its integer,
or pointer component; both execution paths consume that plan.
Checking validates the declared shape, not the external symbol's actual ABI.
The trust and borrowing obligations belong to [L14](../../language/src/foreign-interfaces.md).

The Unix [interpreter adapter](../../../../lang/dynamics/src/foreign.rs) lazily loads libraries and symbols,
caches call interfaces by target and signature, and calls through libffi while borrowing scalar argument storage.
Missing libraries and symbols are runtime errors; generated native programs do not depend on libffi.
Cranelift marshals the retained source arguments into the target C ABI signature (`c_abi_signature`,
handling both SysV AMD64 and AAPCS64 integer extensions) and executes the direct C call
before boxing any full-width integer result.
A valid result resumes the ordinary return continuation; an invalid `Int` or `UInt` result reports a range error.
Integer components retain their source type and signedness through the call plan.
`IntegerType::bits` describes arithmetic width; `storage_bits` describes the byte and C carrier width.
Native encoders truncate C return registers to the carrier width before checking the source range;
the [SysV ABI clarification](https://gitlab.com/x86-psABIs/x86-64-ABI/-/merge_requests/61)
leaves excess integer register bits unspecified.
`Int64` and `UInt64` results are boxed after the C call completes; during result allocation,
the unspilled raw return value stays in an untraced SSA register.
Other integer results and unit fit immediate words.
For C exports, [`emit_c_export_wrapper`](../../../../lang/cranelift/src/externs.rs) keeps incoming C arguments out
of the traced SPS stack range, encodes each argument onto `sps_storage` (relying on allocation root maps
for earlier boxed arguments), decodes the `Ret` result after case 0 returns, and tears down the instance cleanly.
The libffi adapter uses exact scalar storage and return types for integers,
and its explicit void-return operation avoids reading nonexistent result storage.
Native address marshalling moves the pointer word directly, with no host conversion call.
The interpreter verifies that the source value is an `Address` and passes its raw pointer to libffi.
Neither path validates the allocation or establishes a borrow: memory validity
and foreign ownership are caller obligations.
Marshalling helpers do not collect. Explicit retention protects `Bytes` storage across the call;
manual allocations remain live until their owner releases them.
The `Ret` classifier alone establishes neither termination nor a cleanup scope.
Callbacks into the active instance remain unsupported. Explicit compiled-library manifests select exact artifacts
through the [unit linker](native-emission.md#compilation-unit-preparation-and-artifacts);
otherwise native linking uses the library's linker name and interpreter loading uses platform shared-library names.
Native foreign imports are unsupported in Wasm.

[Foreign signature tests](../../../../lang/statics/tests/foreign.rs)
and [FFI integration tests](../../../../lang/tests/tests/suite/ffi.rs) pair valid shapes
with unsupported arities, argument sorts, results, loader failures, and borrowing cases.
Scalar exports use the [C entry adapter](native-emission.md#compilation-unit-preparation-and-artifacts)
and the same scalar call plan.
Incoming pointers remain outside the scalar export profile; they need a separate entry adapter and binding contract.
[Callbacks and retained values](../../../proposals/c-ffi.md#closures-callbacks-and-reentry) still require ownership
and entry protocols beyond fresh scalar calls.
