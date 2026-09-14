# Foreign interfaces: concrete boundaries and next extensions

Returning C imports and named scalar C export libraries are implemented.
Their source obligations belong to [L14](../references/language.md#14-foreign-interfaces),
and their validated call plan and target adapters to [C14](../references/compiler.md#foreign-calls).
The [xxHash binding](../../lib/ffi/xxhash.zy) demonstrates the current pointer-and-length borrow.
The implemented [storage and transport boundary](../references/language.md#storage-and-foreign-transport)
also owns readable windows and canonical foreign decoding.
This proposal contains the next foreign boundaries.

Review extensions here through the
[shared design of compilation units, FFI, and package management](../references/compiler.md#compilation-unit-preparation-and-artifacts).

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
| A writer taking `void *`, capacity, and scalar fields | `Buffer` is rejected as a foreign parameter. A C caller can receive valid fields with nonzero padding. | A checked mutable call borrow, followed by the implemented source field decoder and canonical storage. |
| A visitor taking an array, a function pointer, and `void *context` | A capturing `Thk (Int64 -> Ret Int64)` is rejected as a foreign parameter. The C visitor invokes a context-bearing callback repeatedly. | A rooted callback environment and a runtime entry that returns to each C invocation. |
| A Zydeco service passing an abstract stored record to another module | The [stored-call example](../../lib/tests/std/represented-call/main.zy) shares a carrier and selects a service at runtime; compiled libraries currently export scalars. | Transporting that retained carrier across artifacts needs an ownership-bearing ABI beyond fresh scalar entry. |

## External handle conventions

The implemented source views suggest bindings for familiar external handle formats.
These examples motivate extensions; they are not implemented foreign ABIs.

COM-style object access first loads a vtable address, then uses a separate table cell and access grant
to load a method pointer; invocation is a foreign-call operation, not a data load.

These layouts cover familiar external formats without baking their conventions into `View`.
A [BSTR](https://learn.microsoft.com/en-us/previous-versions/windows/desktop/automat/bstr) has a
four-byte byte-length prefix before its character pointer; its length excludes the terminating character.
A BSTR binding must also preserve its allocation/release convention and distinguish byte counts from character counts.
A [COM interface](https://learn.microsoft.com/en-us/office/client-developer/outlook/mapi/implementing-objects-in-c)
starts with a vtable pointer and supplies the interface pointer as the method's first argument.
The source-view model uses 8-byte sample headers and does not implement either ABI.

## Code-pointer signatures

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

## Source continuation adapters

The [FFI reference](../references/language.md#14-foreign-interfaces) owns how raw C returns coexist
with the [public Ret and CPS convention](../references/language.md#ret-and-explicit-cps).
Status interpretation belongs to the individual binding, including which failures occurred before C entry
and which were reported after C may have changed memory.

The proposed mutable-output adapter below can then continue work on its destination without first producing `Bytes`.
Its final consumer may choose to inspect, decode, or freeze according to the particular C API's contract.
Release per-call borrows and restore runtime state before invoking an external source successor
on any normal result path.
A continuation carrying a borrowed address is not permission to retain it beyond the call.

The reverse boundary still needs a C return: a callback invocation must deliver its ABI result to that C caller.
An internal CPS worker can use a bridge continuation to produce that result,
but cannot bypass callback rooting, reentry, or the return delimiter described below.
Source CPS wrappers should be exercised with different caller protocols, preflight rejection without C entry,
and result successors that immediately perform another allowed operation on the same resource.

## Mutable output through access grants

`sample_write` is the next mutable-output milestone after the implemented immutable window adapter.
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
followed by [foreign decoding and canonical storage](../references/language.md#foreign-decoding-and-canonical-storage).
Only the fields established by the C contract may be read.
General `out`/`inout` classification, aliased ranges, and APIs that retain the pointer need distinct policies.
A reusable thunk around a buffer does not itself establish a lexical or affine lifetime.

## Following boundary

The first scalar export boundary is implemented in [L14](../references/language.md#compiled-libraries-and-c-exports),
with [artifact preparation](../references/compiler.md#compilation-unit-preparation-and-artifacts)
and [instance ownership](../references/compiler.md#runtime-instances) defined in the compiler reference.
The next profiles may admit recoverable status returns, retained instances, and incoming ownership-bearing handles.
An `OS` export additionally needs an explicit process-termination contract.

### Retained host interfaces

Fresh scalar entry now has instance ownership and a return delimiter.
Arbitrary computation and retained-value interfaces remain deferred:
a computation classifier describes its stack protocol but does not provide a releasable foreign handle
to captured values.

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
The specimen checks repeated invocation and independent captured contexts on the C side.
Callbacks into an active Zydeco instance remain unimplemented; fresh entry
into an independent compiled unit is covered by the scalar export profile.
Retained callbacks, callback release, other threads, and unwinding are subsequent boundaries.

## Additional ABI shapes

The remaining shapes are deferred with distinct prerequisites:

| Direction | Required extension before implementation |
| --- | --- |
| Floating-point scalars | Classify and allocate SSE argument/result registers independently of integer registers; preserve payload bits across marshalling and mixed calls. |
| More than six integer components | Plan stack arguments, their alignment, and cleanup together with the existing temporary frame and return continuation. |
| Aggregates by value | Derive target ABI classes from an explicit storage contract, including register splitting, memory arguments, and hidden result pointers. |
| Foreign-owned read-only pointers | Establish a trusted owner, extent, permissions, and release contract before creating a grant. Owned readable windows already cross C. |
| Mutable or retained pointers | Implement the per-call buffer borrow above; retained pointers additionally need an ownership and release protocol. |
| Callbacks and retained exports | Extend fresh scalar entry with retained roots, ownership/release, and a callback reentry protocol. |

The [storage access extension](../references/language.md#access-through-existing-representation-contracts) already
constructs an aligned C record in a caller-provided buffer and passes the frozen result
through the existing immutable borrow.
That supplies a useful aggregate-pointer path while aggregate calling conventions remain open.
The source static builder also computes `Plan A`, but its inspectable shape is not target ABI classification evidence.
The [storage design](../references/language.md#static-layout-plans) owns this distinction.
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
