# Foreign interfaces: concrete boundaries and next extensions

Returning C imports and named scalar C export libraries are implemented.
Their source obligations belong to [L14](../references/language.md#14-foreign-interfaces),
and their validated call plan and target adapters to [C14](../references/compiler.md#foreign-calls).
The [xxHash binding](../../lib/ffi/xxhash.zy) demonstrates the current pointer-and-length borrow.
The implemented [storage and transport boundary](../references/language.md#storage-and-foreign-transport)
also owns raw pointer obligations and foreign field decoding.
This proposal contains the next foreign boundaries.

The [Rust host survey](rust-host.md) compares Rust consuming the existing C export ABI with bindings
for Zydeco's native ABI, including their minimum integration work and validation.
The [CBPV interoperability research proposal](../ideas/cbpv-universal-ffi.md) investigates how these
boundaries could participate in a language for composing adapters across multiple ABI profiles.

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
| A checksum taking `const void *`, `size_t`, and `uint64_t` | `Thk (Addr -> Int -> UInt64 -> Ret UInt64)` supplies a pointer, length, and full-width seed/result. The xxHash wrappers accept `Bytes`. | Implemented scalar transport and synchronous borrow; aggregate and callback boundaries remain independent. |
| An inspector taking a `sample_record`, by pointer or by value | A typed layout constructs the record, and explicit address exposure supplies one pointer. A logical product is rejected for the by-value form. | Pointer transport is implemented. Passing an aggregate by value still needs target ABI classification. |
| A writer taking `void *`, capacity, and scalar fields | A raw `Addr` supplies a manually managed writable destination. A CPS binding interprets C status before asserting initialized fields. | Pointer transport is implemented; each binding supplies its extent, alias, and partial-write contract. |
| A visitor taking an array, a function pointer, and `void *context` | A capturing `Thk (Int -> Ret Int)` is rejected as a foreign parameter. The C visitor invokes a context-bearing callback repeatedly. | A rooted callback environment and a runtime entry that returns to each C invocation. |
| A Zydeco service passing an abstract stored record to another module | The [stored-call example](../../lib/tests/std/represented-call/main.zy) shares typed pointer operations; compiled libraries currently export scalars. | Passing that typed pointer across artifacts needs an agreed ABI and caller-owned lifetime contract beyond scalar entry. |

## External handle conventions

Manual address arithmetic and pointer loads suggest bindings for familiar external handle formats.
These examples motivate extensions; they are not implemented foreign ABIs.

COM-style object access first loads a vtable data address, then loads a method pointer at its documented offset.
Invoking that pointer needs a code-pointer ABI operation.

These layouts cover familiar external formats through ordinary source wrappers.
A [BSTR](https://learn.microsoft.com/en-us/previous-versions/windows/desktop/automat/bstr) has a
four-byte byte-length prefix before its character pointer; its length excludes the terminating character.
A BSTR binding must also preserve its allocation/release convention and distinguish byte counts from character counts.
A [COM interface](https://learn.microsoft.com/en-us/office/client-developer/outlook/mapi/implementing-objects-in-c)
starts with a vtable pointer and supplies the interface pointer as the method's first argument.
Neither external ABI is implemented by the current memory examples.

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
A raw pointer-slot load can read a vtable data address.
Method invocation and code-pointer loading require the separate foreign-call extension.

## Source continuation adapters

The [FFI reference](../references/language.md#14-foreign-interfaces) owns how raw C returns coexist
with the [public Ret and CPS convention](../references/language.md#ret-and-explicit-cps).
Status interpretation belongs to the individual binding, including which failures occurred before C entry
and which were reported after C may have changed memory.

The [mutable-output example](../../lib/tests/ffi/mutable-output.zy) continues work
on the same allocation without first producing `Bytes`.
Its CPS successor receives the initialized pointer only after the C status establishes the promised fields.
A foreign error may leave partial writes; a generic adapter cannot roll them back.
The caller controls release and any permitted retention.
A callback-shaped source wrapper does not establish a borrow.

The reverse boundary still needs a C return: a callback invocation must deliver its ABI result to that C caller.
An internal CPS worker can use a bridge continuation to produce that result,
but cannot bypass callback rooting, reentry, or the return delimiter described below.
Source CPS wrappers should be exercised with different caller protocols, preflight rejection without C entry,
and result successors that immediately perform another allowed operation on the same resource.

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
Manual pointers can remain caller-owned across artifacts when their ABI and release convention agree.
Interfaces retaining managed Zydeco values additionally need an explicit runtime instance and a small,
versioned entry protocol using opaque registered handles.
The component must retain its code and runtime while such handles exist.
An entry must check the agreed representation schema and ABI version before dispatch;
equal sizes or equal partial stack protocols do not establish that agreement.
Keep logical decoding and representation conversion in source adapters sharing the selected layout witnesses.
Directly exporting tagged words, source code pointers, or arbitrary codata stacks remains deferred.

## Closures, callbacks, and reentry

A callback needs both executable code and its captured environment.
Choose who owns that environment, how a foreign caller releases it,
and whether the callback may outlive the import call.
Captured managed values must remain rooted while foreign code retains the callback.
Raw byte pointer transport does not provide an ownership protocol for managed captured values.

Reentry must define which runtime and stack context it enters, whether an earlier call is suspended,
and how nested calls preserve roots and return continuations.
Unwinding and nonlocal exits need an explicit boundary protocol before they can be admitted.
A callback's source computation type alone does not establish thread safety, purity, termination, or single invocation.
These decisions should precede syntax and be exercised with nested calls and retained environments.

`sample_visit` narrows the first useful callback experiment to one host thread, synchronous calls,
an explicit context pointer, and the returning protocol `Int -> Ret Int`.
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
| Pointer results and incoming pointer exports | Add directional ABI adapters and binding-specific extent, ownership, retention, and release contracts. Raw pointer import arguments already support manual mutable storage. |
| Callbacks and retained exports | Extend fresh scalar entry with retained roots, ownership/release, and a callback reentry protocol. |

The [aligned record example](../../lib/tests/ffi/static-layout.zy) constructs a record in manual storage
and passes its address to C. This aggregate-pointer path is independent of aggregate calling conventions.
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
