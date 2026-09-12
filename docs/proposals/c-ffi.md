# Foreign interfaces: concrete boundaries and next extensions

Returning C imports are implemented.
Their source obligations belong to [L14](../references/language.md#14-foreign-interfaces),
and their validated call plan and target adapters to [C14](../references/compiler.md#foreign-calls).
The [xxHash binding](../../lib/ffi/xxhash.zy) demonstrates the current pointer-and-length borrow.
This record owns explicit memory-window transport and the next independently reviewable foreign boundaries.

## Examples and observed gaps

The immediate goal is to let a binding describe the storage and argument transport its C function actually expects.
Source protocol preservation helps validate lowering, but does not supply either contract.
The following examples separate missing foreign operations from operations already expressible in source.
The [C specimen](../../lib/tests/ffi/contracts.c) implements the first four interfaces and executes them
with C callers; [source probes](../../lang/tests/tests/ffi_examples.rs) check the current Zydeco boundary.
Executing a C caller is evidence about the specimen, not evidence that Zydeco can import every function in it.

| Example | Current result | Missing capability or next decision |
| --- | --- | --- |
| A checksum taking `const void *`, `size_t`, and `uint64_t` | `Thk ((Access * Addr * Int64) -> Int64 -> UInt64 -> Ret UInt64)` supplies a pointer, length, and seed. The source xxHash wrapper accepts `Bytes`. | No new mechanism is needed for an immutable, synchronous borrow. |
| An inspector taking a `sample_record`, by pointer or by value | A source storage contract constructs the record, and a checked window supplies exactly one pointer. A logical product is rejected for the by-value form. | Pointer transport is implemented. Passing an aggregate by value still needs target ABI classification. |
| A writer taking `void *`, capacity, and scalar fields | `Buffer` is rejected as a foreign parameter. A C caller can receive valid fields with nonzero padding. | A checked mutable call borrow, plus a foreign decoder that distinguishes fields from padding. |
| A visitor taking an array, a function pointer, and `void *context` | A capturing `Thk (Int64 -> Ret Int64)` is rejected as a foreign parameter. The C visitor invokes a context-bearing callback repeatedly. | A rooted callback environment and a runtime entry that returns to each C invocation. |
| A Zydeco service passing an abstract stored record to another module | The [stored-call example](../../lib/tests/std/represented-call/main.zy) already shares a carrier, converts representations, and selects a service at runtime. | Source modularity works; loading a separately compiled service needs a negotiated external ABI and runtime ownership. |

### A record has storage and transport contracts

The record has a `UInt8` tag and a `UInt32` payload.
On the specimen's checked layout it occupies eight bytes, with the payload at offset four.
An ordinary value function already computes this placement:

```zydeco
memory/product UInt8 UInt32 memory/uint8 memory/uint32
```

Realizing the plan supplies an abstract `Stored` and its operations.
A source wrapper can therefore expose `Thk (Stored -> Ret UInt64)`, extract bytes internally,
and call the checked pointer adapter with the required extent.
The probes check that this wrapper lowers, while using `Stored` directly as a foreign parameter is rejected.
`sample_inspect_with_options` has one pointer and five integer parameters.
The explicit window contributes exactly one pointer, so this six-argument declaration is accepted.
The native [boundary fixture](../../lib/tests/ffi/boundary.zy) executes both pointer-only
and pointer-plus-five-integer calls, as well as bindings with explicitly supplied lengths.

`sample_inspect_value` takes the same record by value.
Its C caller succeeds, while `Thk ((UInt8 * UInt32) -> Ret UInt64)` is rejected by the current foreign checker.
Choosing a byte layout does not determine whether a target ABI splits an aggregate across registers,
passes it in memory, or introduces a hidden result pointer.
The view adapters below implement pointer arguments; aggregate classification remains a separate, later extension.

Writing six `UInt64` parameters passes source checking because declarations are trusted.
It describes integers rather than a retained pointer and must not be executed against that symbol.
This is a declaration mismatch, not a pointer encoding technique. The unchecked correspondence
to C headers remains the existing [L14 trust boundary](../references/language.md#14-foreign-interfaces).

### Valid foreign output need not be canonical storage

`sample_write` writes the two fields and deliberately fills padding with `0x58`.
The C inspector reads the expected logical record.
The test feeds these actual C-produced bytes into Zydeco: the existing whole-record `from_bytes` rejects them,
because that operation checks canonical encoding, including zero padding.
This rejection is correct for its [storage contract](bytes.md#layout-laws).

The [record input example](../../lib/tests/ffi/record-input.zy) demonstrates a separate foreign decoder
in ordinary CBPV.
It obtains the size and field positions from the existing source layout plan, checks exact extent,
decodes the two fixed-width integer fields, and ignores only the layout's padding.
The caller then uses `store` to construct canonical `Stored`.
The same test checks that these canonical bytes pass `from_bytes`; short and oversized inputs select failure
before any logical fields are exposed to the success continuation.
The canonicalization program executes on all four backends with the bytes produced by the C specimen.

This example needs no new compiler feature.
It establishes the direction for C output adapters: decode the foreign representation to `A`,
then store through the selected `Storage A Stored` contract.
Keep strict `from_bytes` for callers that require canonical bytes.
Other foreign decoders must account for their own endianness, valid field encodings, active union alternative,
and length conventions; the integer record does not establish a generic decoder for arbitrary C objects.
The fixture checks its native layout and little-endian byte order explicitly.

## Source-defined views at foreign boundaries

The implemented readable-window adapter lets `sample_inspect` receive one pointer using
the general [address, cell, and view interfaces](bytes.md#addresses-cells-and-views).
That section owns `Addr`, `Access`, `Cell A`, `Fat M`, and `View H M`, including phase and access rules.
Thin, fat, prefix-header, and object-header handles are source library choices.
The compiler recognizes general memory leaves and an explicit call transport, with no `Bytes` identity.
Foreign-owned grants, mutable and retained pointers, code-pointer calls,
and aggregate-by-value adapters remain proposed.

The current transport is the source product `Access * Addr * Int64`.
The final integer states the extent the binding promises C may read.
It is used only for preflight validation; the product contributes one C pointer and no implicit length argument.
A binding whose C signature includes a length supplies that integer separately in the correct position.
The current targets use 64-bit `size_t`; a checked nonnegative `Int64` byte count has the same argument bits
within its supported range.
Other typedefs and target widths need an explicit matching declaration.

### A handle layout does not determine a C call

A view interprets a handle; the foreign signature determines what its C callee receives.
For example, opening a `Fat Int64` produces an address and an element count.
The binding chooses among the following explicit transports:

| C parameter shape | Source adapter | Native arguments |
| --- | --- | --- |
| `const sample_record *` | Obtain the record address through the binding's selected view and validate the known record extent. | One data pointer. |
| `const void *, size_t` | Open the view, validate the extent, and supply a separate integer with the target byte-count representation. | Pointer followed by byte count. |
| A struct containing pointer and length, passed by value | Encode the handle with an explicit native aggregate cell and classify that aggregate for the target ABI. | The aggregate's target register/stack classes. |
| An interface pointer with an initial vtable slot | Open the object view, load the selected table slot under a suitable grant, and call its code pointer with the original interface address. | The method's declared arguments, including its interface argument. |

A source product is not automatically a C aggregate or a sequence of C arguments.
`Cell H` determines storage when `H` is stored; it does not determine aggregate register splitting,
parameter expansion, or hidden result pointers.
The call plan must preserve ordered leaves and aggregate grouping.
Likewise, a prefix-header handle can cross C as just its original payload pointer
without first loading the header, when the binding already knows the required extent.
The general `open` operation is available when its runtime metadata is needed;
an adapter need not perform unused reads merely to pass an address.

The binding for `sample_inspect` continues to accept its abstract `Stored` record.
It obtains an address and retained access authority for that storage and invokes a pointer-argument adapter.
Arbitrary bytes must first satisfy the binding's record contract, through its existing canonical constructor
or through foreign decoding followed by canonical storage.
The storage dictionary's layout laws and correspondence to the actual C declaration remain explicit obligations.

### Signature identity and code addresses

Data pointers and code pointers need distinct leaves.
Use a proposed `Code S : VType`, where `S : VType` is an abstract static witness introduced
by a checked foreign-signature package.
The package supplies the ordered native argument/result description, its source conversion operations,
and an invocation operation accepting `Code S`.
An independently opened signature witness cannot silently reuse that code value.
`S` erases; the code address remains.
The target determines the code-pointer representation, which is not assumed to be interchangeable with a data address.

The native signature description must resolve before target call lowering.
It records the calling convention, primitive widths, aggregate grouping, and result transport.
Source value functions can build its fixed layouts; the compiler validates and classifies the resulting plan.
Equal source computation types or equal aggregate sizes do not establish equal foreign signatures.
A dynamically selected foreign method can vary the `Code S` value under one known signature,
or package distinct witnesses together with their matching invocation operations.
A runtime integer cannot dynamically determine a new native calling convention at a compiled call site.

A `Thk B` may include a captured environment and is not a `Code S`.
Source thunks call code pointers through their matching adapters.
Turning a capturing thunk into a foreign callback additionally requires the runtime-entry and rooting protocol below.
The implemented source view reads a vtable data address.
Method invocation and code-pointer loading require the separate foreign-call extension.

### Access and returning-call cleanup

The primitive foreign declaration is a returning thunk.
Its readable-window transport validates liveness, read permission, complete extent, and initialization
before entering C; invalid memory terminates the call path with the corresponding memory error and no foreign effect.
The adapter retains the allocation across the call.
It does not infer an element alignment or relationship to another integer parameter from this generic window.
Those stronger requirements belong to the binding's explicit declaration and source adapter.

A source wrapper can expose a recoverable preflight path by using `Memory.check` and the required leaf reads
before invoking the foreign thunk, for example:

```text
inspect : forall R. Access -> Addr -> Thk (Fault -> R) -> Thk (UInt64 -> R) -> R
```

The wrapper can retain the grant and specialize this interface to its chosen record handle.
A synchronous read-only binding permits C to read the validated range during the call;
it grants neither mutation nor retention after return.
A raw pointer result needs a separately specified allocation, extent, and ownership policy
before it can yield usable access authority.

The bridge is single-threaded and non-reentrant, with no admitted nonlocal exit.
After the actual C return it releases call borrows before resuming the source continuation,
while preserving roots needed to encode the result.
Cleanup belongs to this bridge operation; `Ret` does not imply purity, termination, or one invocation.
Empty ranges contain no readable byte and do not imply a null address.
Nullability and sentinel conventions belong to the binding's source contract.

[`Bytes`](bytes.md#immutable-owners-and-source-bytes) is now defined in std.
Its `with_window` operation supplies the shared memory adapter with an immutable grant, visible address, and count.
Other source view forms can supply the same transport without a new compiler identity.

### Acceptance criteria

The native boundary fixture executes pointer-only and six-component calls, explicit lengths,
aligned records, selected slices, empty windows, and repeated calls with retained owners.
Interpreter adapter tests check window decoding and preflight rejection;
source view tests exercise handle interpretation.
Check field contents, alignment, argument ordering, empty windows, selected slices,
and repeated calls with retained owners.
Closed or revoked grants, insufficient extents, uninitialized storage,
and wrong permissions must prevent C entry and preserve handle state.
The native rejection fixture asserts failure before an observable C effect.
Signature-witness mismatch tests will accompany the future code-pointer extension.
A value from an unrelated `Stored` opening must still fail source checking.

Then exercise a fat pointer whose runtime metadata is a record, a prefix-header pointer,
and a vtable-bearing object using the same leaf operations and explicit transport plans.
Arbitrary source runtime metadata must not create new compiler cases.
Aggregate-by-value transport and actual code-pointer invocation require their respective target adapters;
source view tests alone do not establish either capability.

## Mutable output through access grants

`sample_write` provides a separate next milestone once immutable views work.
A mutable wrapper retains a `Buffer` capability or writable `Access` grant
and chooses pointer-only or pointer-and-capacity transport.
Creating the view need not establish exclusivity; the call bridge must revalidate the handle and acquire the borrow
on every invocation, since an alias may have closed or frozen it since view construction.
Keep this boundary in an explicit effectful protocol compatible with the existing `OS` buffer operations,
with a structured preflight-error continuation and a C-result continuation.

Validate every argument before invoking C and reject conflicting mutable arguments before changing any handle state.
For the initial single-threaded, non-reentrant bridge, acquire a dynamic call borrow,
invoke C, release the borrow, and then resume the chosen continuation.
Snapshots remain detached, and immutable `Bytes` cannot be used as mutable storage.
Bounds and closed-handle failures before C entry preserve buffer contents.
Once C has run, its error result may accompany partial writes; the generic bridge cannot promise transactional rollback.

The wrapper must interpret the particular C API's status and written-length conventions before inspecting output.
For `sample_write`, a successful status permits a snapshot of the known record extent,
followed by the field decoder and canonical store above.
Only the fields established by the C contract may be read.
General `out`/`inout` classification, aliased ranges, and APIs that retain the pointer need distinct policies.
A reusable thunk around a buffer does not itself establish a lexical or affine lifetime.

## Following boundary

A C caller entering Zydeco must establish a runtime and a CBPV return continuation.
An export returning an ordinary C scalar could initialize that context and resume the caller after `Ret`,
but the entry's allocation roots, failure behavior, and repeated-call lifetime must be specified first.
An `OS` export instead needs a root-stack protocol and an explicit answer about process termination.
Neither follows merely from reversing the current import marshalling.

### Zydeco's own host interface

An adapter implementing a Zydeco classifier must preserve the operations that classifier permits.
The old native argument fold returned `Thk R` tails backed by a consumed Rust iterator and a freed environment.
Repeated forcing could access a freed closure, while abandoning tails retained registered roots indefinitely.
The Wasm host could not manufacture a corresponding tail for two or more arguments.

The accepted extension moves traversal into [ordinary source computations](../../lib/std/system/arguments.zy)
over a checked indexed host operation.
[L13](../references/language.md#13-primitive-values-and-capabilities) owns lookup and reuse semantics;
[C14](../references/compiler.md#c14-builtin-contracts-primitive-operations-and-foreign-calls) owns the adapters.
The host now supplies data and resumes supplied continuations without owning a lazy tail.
This removes bespoke closure lifetime machinery while retaining the unrestricted computation interface.

External entry remains deferred. The current native executable initializes process-global heap,
frame, and transfer state and enters a root computation that ends through `OS`.
An embeddable interface needs an explicit runtime instance, a return delimiter, root ownership for retained values,
and entry/reentry rules before it can expose arbitrary Zydeco computations.
A computation classifier describes the expected stack protocol; it does not itself provide a foreign runtime instance
or a releasable handle to captured values.

The stored-call example already solves sharing an abstract carrier inside one compiled program.
Extending that example across independently loaded artifacts should begin with an explicit runtime instance
and a small, versioned entry protocol using owned buffers or opaque registered handles.
The component must retain its code and runtime while such handles exist.
An entry must check the agreed representation schema and ABI version before dispatch;
equal sizes or equal partial stack protocols do not establish that agreement.
Keep logical decoding and representation conversion in the source adapters already exercised by the example.
Directly exporting tagged words, source code pointers, or arbitrary codata stacks remains deferred.

## Closures, callbacks, and reentry

A callback needs both executable code and its captured environment.
Choose who owns that environment, how a foreign caller releases it,
and whether the callback may outlive the import call.
Captured managed values must remain rooted while foreign code retains the callback.
The current transient byte borrow does not provide an ownership protocol for such values.

Reentry must define which runtime and stack context it enters, whether an earlier call is suspended,
and how nested calls preserve roots and return continuations.
Unwinding and nonlocal exits need an explicit boundary protocol before they can be admitted.
A callback's source computation type alone does not establish thread safety, purity, termination, or single invocation.
These decisions should precede syntax and be exercised with nested calls and retained environments.

`sample_visit` narrows the first useful callback experiment to one host thread, synchronous calls,
an explicit context pointer, and the returning protocol `Int64 -> Ret Int64`.
Root a reusable Zydeco closure for the entire outer C invocation, including a call that never invokes it.
Each actual callback invocation gets a fresh runtime return context; repeated invocations reuse the rooted closure.
Before allowing a nested returning C import from the callback, preserve
and restore the suspended outer transfer state and its roots explicitly.
The specimen checks repeated invocation and independent captured contexts on the C side;
Zydeco runtime reentry remains unimplemented.
Retained callbacks, callback release, other threads, and unwinding are subsequent boundaries.

## Additional ABI shapes

Fixed-width signed and unsigned integer parameters/results and `void` results now extend the typed call plan.
They reuse integer registers without requiring a new source representation or lifetime protocol;
[L14](../references/language.md#14-foreign-interfaces) owns their accepted classifiers.
The [integer fixture](../../lib/tests/ffi/integers.zy) checks extrema, mixed widths in all six registers,
unit continuation resumption after an observable C operation, and deliberately dirty upper return bits on AMD64.

The remaining shapes are deferred with distinct prerequisites:

| Direction | Required extension before implementation |
| --- | --- |
| Floating-point scalars | Classify and allocate SSE argument/result registers independently of integer registers; preserve payload bits across marshalling and mixed calls. |
| More than six integer components | Plan stack arguments, their alignment, and cleanup together with the existing temporary frame and return continuation. |
| Aggregates by value | Derive target ABI classes from an explicit storage contract, including register splitting, memory arguments, and hidden result pointers. |
| Foreign-owned read-only pointers | Establish a trusted owner, extent, permissions, and release contract before creating a grant. Owned readable windows already cross C. |
| Mutable or retained pointers | Implement the per-call buffer borrow above; retained pointers additionally need an ownership and release protocol. |
| Callbacks and exports | Establish runtime entry, retained roots, completion, and reentry as described above. |

The [storage access extension](bytes.md#access-through-existing-representation-contracts) already constructs
an aligned C record in a caller-provided buffer and passes the frozen result through the existing immutable borrow.
That supplies a useful aggregate-pointer path while aggregate calling conventions remain open.
The source static builder also computes `Plan A`, but its inspectable shape is not target ABI classification evidence.
The [storage design](bytes.md#static-layout-plans) owns this distinction.
Keep argument order and flattening in one validated representation consumed by every target as these shapes are added.

Header-based validation could check some declaration mistakes but would introduce a separate source of ABI evidence.
Specify how it interacts with the trusted declaration and platform configuration.
Native imports remain outside the Wasm and ZASM-interpreter execution profiles until a separate adapter is designed.

## Validation criteria

[Signature tests](../../lang/statics/tests/foreign.rs)
and [integration fixtures](../../lang/tests/tests/ffi.rs) cover the current subset.
New boundaries need accepted and rejected shapes plus observable lifetime checks:
argument order, full-width results, empty buffers, loader failure, collection with live captures,
and invalid entry or release without partial runtime mutation.
Installed-library and native-toolchain runs remain opt-in under the repository's testing workflow.

Reproduce the concrete boundary review with focused targets:

```sh
cargo test -p zydeco-tests --test memory_views --test ffi_examples --test ffi --test represented_calls
cargo test -p zydeco-tests --test ffi native_c_boundary_executes_the_compositional_protocol -- --ignored
```

On Unix, `ffi_examples` compiles and runs its own small C specimen with the host C compiler;
it needs no installed foreign library.
The separate ignored target exercises existing native C imports.
