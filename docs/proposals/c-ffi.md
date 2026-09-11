# Foreign interfaces: concrete boundaries and next extensions

Returning C imports are implemented.
Their source obligations belong to [L14](../references/language.md#14-foreign-interfaces),
and their validated call plan and target adapters to [C14](../references/compiler.md#foreign-calls).
The [xxHash binding](../../lib/ffi/xxhash.zy) demonstrates the current pointer-and-length borrow.
This record owns the next independently reviewable foreign boundaries.

## Examples and observed gaps

The immediate goal is to let a binding describe the storage and argument transport its C function actually expects.
Source protocol preservation helps validate lowering, but does not supply either contract.
The following examples separate missing foreign operations from operations already expressible in source.
The [C specimen](../../lib/tests/ffi/contracts.c) implements the first four interfaces and executes them
with C callers; [source probes](../../lang/tests/tests/ffi_examples.rs) check the current Zydeco boundary.
Executing a C caller is evidence about the specimen, not evidence that Zydeco can import every function in it.

| Example | Current result | Missing capability or next decision |
| --- | --- | --- |
| A checksum taking `const void *`, `size_t`, and `uint64_t` | `Thk (Bytes -> UInt64 -> Ret UInt64)` describes the call directly. The existing xxHash binding exercises this pattern. | No new mechanism is needed for an immutable, synchronous borrow. |
| An inspector taking a `sample_record`, by pointer or by value | A source storage contract constructs the record, but `Bytes` always adds a length. The specimen's `sample_inspect_bytes` wrapper adapts the pointer signature; a logical product is rejected for the by-value form. | Select pointer-only transport first. Passing an aggregate by value additionally needs target ABI classification. |
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
and call the existing pointer-and-length adapter.
The probes check that this wrapper lowers, while using `Stored` directly as a foreign parameter is rejected.
They also check a less ambiguous witness for the transport gap: `sample_inspect_with_options` has one pointer
and five integer parameters, but using `Bytes` describes seven components and exceeds the current six-component limit.
Raising that limit would still leave the extra length in the wrong signature.

`sample_inspect_value` takes the same record by value.
Its C caller succeeds, while `Thk ((UInt8 * UInt32) -> Ret UInt64)` is rejected by the current foreign checker.
Choosing a byte layout does not determine whether a target ABI splits an aggregate across registers,
passes it in memory, or introduces a hidden result pointer.
The read view below addresses pointer arguments; aggregate classification remains a separate, later extension.

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

## Proposed next extension: retained read views

The next compiler capability should remove the C adapter for `sample_inspect`.
Add one explicit source argument view for pointer-only immutable transport.
In the proposed interface, a trusted foreign package exports an abstract value type `Read`
and a total value function `read` that wraps `Bytes`.
The view retains the bytes; it contains no user-observable address.
It can be copied, captured, and supplied to several calls with the ordinary lifetime of its backing value.
Constructing the view does not run C code or create a borrow that needs lexical cleanup.

The proposed use is below; `Read` and `c/read` are design names and are not implemented:

```zydeco
let raw : Thk (Read -> Ret UInt64) =
  @(ffi(c, library("zyffi_examples"), symbol("sample_inspect"))) in
let inspect : Thk (Stored -> Ret UInt64) = {
  fn value =>
    do data <- ! storage/bytes value;
    ! raw (c/read data)
} in
...
```

The public binding accepts the `Stored` from its chosen contract.
The low-level view only selects argument transport: it does not make arbitrary bytes a valid record.
An API accepting unclassified bytes must validate them or decode and store them before calling that binding.
Source-authored storage dictionaries retain their existing layout-law obligations;
the view adds no proof of those laws or of the actual C declaration.

### Source construction and call interpretation

Keep placement, padding, alignment, and decoding in their existing source value functions and computations.
The view itself needs only an ordinary wrapper carrying the backing `Bytes`.
Its identity and payload projection must be supplied by the trusted foreign package and validated
at the compiler boundary, rather than inferred from a user-chosen field name or an isomorphic product.
The implementation must retain that identity until foreign signature checking
and retain the backing value after erasure.
Whether the checked wrapper can reuse current nominal data lowering
or needs a dedicated residual constructor is an implementation question for this one boundary;
it does not require a general layout annotation mechanism.

The checked call plan gains a pointer-only read parameter, expanding to exactly one pointer component.
Its adapter obtains the backing window from the view.
Existing `Bytes` parameters keep their pointer-and-length meaning, using the same window-borrowing implementation.
Both interpreter and AMD64 paths must consume this single validated expansion, including mixed parameter order.
A view preserves the address alignment and visible window established by its backing bytes.
Taking a byte slice or applying another byte transformation still requires the usual revalidation
when alignment matters.

The bridge retains owners for all arguments until the C call actually returns.
The C callee may read only the range promised by its binding and may neither modify nor retain the pointer.
The initial extension inherits the current prohibition on reentry and nonlocal exits.
An empty view supplies no readable byte and does not promise a null pointer;
nullable arguments and sentinel-terminated strings need their own explicit contracts.
`Read` is initially argument-only at the foreign boundary: a returned C pointer has no such backing owner,
so `Ret Read` cannot be obtained by reversing this marshalling operation.

Borrow cleanup belongs to the returning C bridge.
It occurs before resuming the supplied Zydeco continuation, including when result encoding can allocate.
It is not inferred from the source `Ret` type or an assumed source frame size:
[the installed-continuation rule](../references/language.md#ret-and-stack-extent) remains authoritative.
No source thunk is required to invoke a cleanup continuation exactly once.

### Alternatives and acceptance criteria

A C shim is the current useful workaround, but puts a language-level transport choice in every affected binding.
Integer addresses lose ownership and range evidence.
A source-visible scoped raw pointer would require lifetime restrictions the language does not yet express.
A general algebra of C signatures would be premature for this example;
the existing compositional classifier can accommodate this one additional argument view.

Accept the extension when both supported C adapters execute the original pointer-only inspector,
the six-component inspector with options, and repeated calls using a captured view.
Check field contents and address alignment at C entry; retain tests for empty windows and selected byte slices.
Pair those with rejected foreign pointer results, unwrapped storage carriers,
and unsupported mutable or callback arguments.
The source binding must still reject a value from a different `Stored` opening.
The argument owner must remain live until C returns.
Subsequent result allocation must not expose raw scratch words as roots or invalidate an independently retained view.
C must observe neither a spurious length nor a shifted scalar parameter.

These tests are the next implementation milestone.
The current increment supplies the specimens, source gap probes, and the working foreign-decoder example;
it does not yet implement `Read` or change a native ABI.

## Mutable output after read views

`sample_write` provides a separate next milestone once immutable views work.
A mutable view should retain a `Buffer` capability and describe pointer-only or pointer-and-capacity transport.
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
| Read-only pointer arguments | Implement the retained read view and its owner-preserving marshalling described above. |
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
cargo test -p zydeco-tests --test ffi_examples --test ffi --test represented_calls
cargo test -p zydeco-tests --test ffi native_c_boundary_executes_the_compositional_protocol -- --ignored
```

On Unix, `ffi_examples` compiles and runs its own small C specimen with the host C compiler;
it needs no installed foreign library.
The separate ignored target exercises existing native C imports.
