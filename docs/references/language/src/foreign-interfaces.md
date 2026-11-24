# Foreign Interfaces

## C Imports

A foreign implementation is an annotated hole:

```zydeco
param val (/Addr) : @(import("../../lib/std/builtin/memory.zy")) in
(@(ffi(c, library("xxhash"), symbol("XXH64"))) : Thk (Addr -> Int -> UInt64 -> Ret UInt64))
```

For `ffi(c, ...)`, the supported classifier is `Thk (A1 -> ... -> An -> Ret B)`, including zero arguments.
`Int8/16/32/64` and `UInt8/16/32/64` contribute their matching C `intN_t` or `uintN_t`.
`Int` and `UInt` use `int64_t` and `uint64_t` carriers in the current profiles; their source ranges remain those
in [Scalar Ranges and Conversions](primitives-and-capabilities.md#scalar-ranges-and-conversions).
Values entering Zydeco, whether a C import result or an export argument, are range-checked.
An out-of-range value terminates execution before source code resumes.
`Addr` contributes one raw data pointer.
Any C length or capacity parameter is a separate integer argument.
The result is a supported integer or `Unit`; `Ret Unit` corresponds to C `void`.
At most six C arguments are accepted. The declaration chooses carriers and signedness; C `int`, `long`,
enums, and typedefs require platform-specific agreement.

The declaration author supplies the real symbol's ABI and pointer contract.
For every pointer argument, the binding establishes lifetime, extent, alignment, initialized readable fields,
writable ranges, alias compatibility, and any foreign retention or release requirements.
There is no automatic bounds, liveness, or permission check before C entry.
An empty range has no readable byte and need not have a null address; nullability belongs to the binding.
A function receiving immutable `Bytes` storage must not modify or free it.
Manual mutable allocations can be passed directly when their foreign contract permits it.

Here `Ret` records the C return protocol even when the function has effects.
The [public Ret/CPS convention](computations-and-control.md#ret-and-explicit-cps) applies to source wrappers:
keep pure observations returning; bind an effectful raw result, interpret its status, then invoke an explicit successor.
The classifier establishes neither purity nor termination.
C may leave partial writes on a reported failure; only the particular binding can promise failure before mutation.
A normal return uses the C return protocol.
Unwinding, nonlocal jumps, and callbacks into an active instance are unsupported.
An import may enter a separate compiled library under its [entry discipline](#compiled-libraries-and-c-exports).

Checking validates the declared classifier without loading libraries or inspecting headers.
The Unix interpreter loads symbols lazily; AMD64 links the named library.
Missing libraries or symbols fail at loading or linking.
Wasm rejects native imports. Callbacks, pointer results, floating-point and aggregate ABI values,
and larger signatures remain [extensions](../../../proposals/c-ffi.md).
C exports currently admit only scalar arguments and scalar or unit results.
The `zydeco` convention instead names a [native unit initializer](#native-zydeco-units).

## Storage and Foreign Transport

`Int` and `UInt` use eight-byte little-endian storage in the current profiles.
Signed storage sign-extends the 63-bit value; unsigned storage leaves its top bit zero.
`Int64` and `UInt64` use eight bytes with every bit available to the payload; every eight-byte pattern is valid.
Their C carriers are `int64_t` and `uint64_t`, respectively, regardless of the ordinary value representation.
Raw `Int`/`UInt` loads reject out-of-range carrier bits.
The safe numeric `from_le_bytes` codecs check both length and payload range
and select their failure continuation on invalid input.
The raw memory operations still require a valid address and initialized readable extent.

A storage layout determines byte placement; the foreign signature independently determines argument transport.
`Ptr L S` requires explicit address exposure before a C import; its abstract library type is not a foreign classifier.
A logical product or `Slice L S` does not become a C aggregate or expand into arguments automatically.
A binding passes the address and any required count separately.
The current 64-bit profiles use a 64-bit `size_t`; a validated nonnegative `Int` count has the same bits.
Callers passing an aligned record to C code that inspects every byte must initialize padding explicitly,
and callers passing an uninitialized output buffer to C must assert `Init` only after the foreign contract reports
that the promised fields were written; raw adapters supply no ownership inference for these transitions.

### Foreign Decoding

When decoding foreign record buffers from a static layout plan, callers validate the exact byte extent
before reading fields, rejecting short or oversized inputs.
Padding is not part of the logical record and need not be canonicalized before typed use.
Bindings must establish their own endianness, valid field encodings, union alternatives,
and length conventions; canonical serialization, when required, is a separate codec contract.
See the [standard library guide](../../../../lib/std/README.md#explicit-storage) for worked C input,
mutable output, and decoding examples.

## Compiled Libraries and C Exports

A [compiled library package](sources/packages.md#source-packages) exposes selected functions through an FFI entry
profile. Its evolution follows the
[shared design of compilation units, FFI, and package management](../../compiler/src/native-emission.md#compilation-unit-preparation-and-artifacts).
A compiled library declares a source implementation and its complete public C interface:

```zydeco
@[package(
  library(c,
    export(field(add), symbol("example_add")),
    export(field(identity), symbol("example_identity"))),
  name(example/arithmetic))]
let (/numeric) = @(import("../../lib/std/primitive/package.zy")) in
(
  #add = ({ fn x y => ! numeric/int/add x y } : Thk (Int -> Int -> Ret Int)),
  #identity = ({ fn x => ret x } : Thk (Int -> Ret Int))
)
```

`library(c, ...)` marks source intended to produce a compiled C library.
It requires at least one `export(selector, symbol("c_name"))`.
`root` selects the prepared value itself; `field(api/add)` follows ordinary named projections through that value.
Selectors and symbols must be distinct, and a root export cannot coexist with field exports.
Each symbol is an unmangled ASCII C identifier; the `zydeco_` prefix is reserved for runtime support.
Only the selected symbols are public.
Export a constant through a zero-argument returning thunk; global data exports have no contract in this profile.

Independent selection follows the ordinary lexical binding and inference rules
at [source boundaries](sources.md#source-boundaries).
Preparation accepts a closed value or exactly one leading `param val` over a validated Builtin value contract.
Aliases and source imports may supply that factory; preparation uses ordinary static application.
Builtin validation examines its typed roles, independently of the process-only `OS` codomain requirement.
The provider's runtime values are created at each external entry.
Supply other static arguments in source before the boundary; an unapplied generic factory is not an export.

Field selection and static specialization precede runtime readiness.
Unselected static helpers may remain without runtime representations, while each selected thunk,
its captures, and its reachable body must be complete and representable.
Missing or ambiguous projections, escaping static values, and reachable executable holes reject the unit.
Preparing the interface never executes the exported computation.

The export classifier is `Thk (I1 -> ... -> In -> Ret R)`, with zero through six supported integer parameters
and a supported integer or `Unit` result.
It shares the import signature's widths and scalar conversions, but reverses the transport direction.
Incoming pointer adapters and their binding-specific ownership contracts remain unimplemented;
therefore the outgoing readable-window adapter cannot serve as an export parameter.
Abstract source values, closures, floating-point and aggregate values, `OS` results,
and extra arguments are rejected at this boundary.

Every call creates a fresh runtime instance and supplies a C return delimiter for `Ret R`.
Normal return converts the result while the instance is alive, releases its resources, and resumes the C caller.
Arguments exposed through Builtin are empty.
Standard streams borrow the process streams; newly opened resources belong to the call.
There is no cross-call source state or exported runtime handle.
Runtime faults terminate the process with a diagnostic; this profile neither unwinds
through C nor fabricates a return value.
A nonterminating body need not return.

One guard covers all exports of a unit in its link image.
Concurrent entry or reentry into an active unit is rejected before source initialization.
Sequential calls and ordinary internal recursion are supported.
A call from unit A to independent unit B preserves A's complete suspended instance and restores it on return,
even when the units share runtime support in one native image.
Calling A again while it is active is rejected. Entry from a signal handler is unsupported.
Retained callbacks, persistent instances, recoverable errors, and incoming memory ownership remain separate extensions.

`build --target object`, `staticlib`, and `sharedlib` emit AMD64 Linux or macOS libraries.
The [artifact workflow](../../../../cli/README.md#c-libraries) produces a C header,
a generated Zydeco import interface, and a manifest alongside the native output.
Ordinary `@(import(...))` still imports source, including a compiled library's original factory;
it does not choose an artifact or implicitly apply Builtin.
A consumer imports the generated `.imports.zy` interface and explicitly supplies `--link-library MANIFEST`
to `build`, `run`, or `test`.
This supports separate compilation without the producer's implementation source.
The interpreter accepts only shared libraries matching its own host; Wasm has no adapter for these artifacts.

## Native Zydeco Units

`@[package(library(zydeco), name(example/math))]` selects a native library
whose initializer returns the complete prepared source value.
Select the intended public value in source; all its fields participate in the interface.
Preparation accepts a closed value or one leading value function over a validated Builtin contract,
applies that provider statically, and checks the resulting value, captures, and reachable computations for readiness.
An initializer materializes the value without forcing its exported thunks.

The initial interface profile supports `Unit`, the supported integers, `Float32`, `Float64`, `Char`, `String`,
named value types, n-ary products, and `Thk B`, where `B` consists of arrows and `Ret`.
These forms can nest, so an exported function may accept a thunk or return a capturing thunk.
Product order, nesting, names, scalar widths, and all argument/result classifiers remain in the interface.
Nominal data, codata, abstract types and capabilities, universals, existentials, witness-dependent arrows,
and static functions are rejected in public classifiers in this profile.
Their use inside an implementation remains governed by ordinary typing and static elimination.
The interface has a maximum structural depth of 48 and 4,096 nodes.

A generated `.imports.zy` is a self-contained declaration
of type `Thk (Ret Exports)` using `ffi(zydeco, library("example.math"), symbol("..."))` and canonical type intrinsics.
Its symbol is generated by the compiler. Only that initializer classifier is accepted for a `zydeco` import;
ordinary exported functions are obtained from the returned value and invoked through their source types.
Consumers supply the matching manifest with `--link-library` before native linking or execution.

Initialization is explicit:

```text
let initialize = @(import("build/example.math.imports.zy")) in
do math <- ! initialize;
do add_seven <- ! math/make_adder 7;
! add_seven 35
```

Forcing `initialize` again materializes another export value.
A consumer wanting one shared initialization binds the result once and passes or captures it normally.
Importing the generated declaration alone does not initialize anything.
Source imports retain their ordinary execution behavior; there is no implicit module cache or initialization scheduler.

Units and their consumers execute within one native runtime instance.
Returned values and closures can survive initializer returns and subsequent collection through ordinary source roots.
The process owns the heap, resource provider, frame store, and code lifetime;
an initializer neither creates nor destroys an instance.
Runtime faults retain the native process's fatal behavior.
The initializer symbol is a Zydeco stack entry, not an ordinary C-callable function.

Publication currently supports AMD64 Linux and macOS objects through `build --target object`.
Consumers use the native `exe` backend; the interpreter and Wasm reject native unit execution.
Units require matching compiler, runtime model, runtime source, target, and entry profile.
Native unit dependencies are supported; dependencies between native units
and C library artifacts inside a library remain unsupported.
A process may separately link both kinds of library.
The [workflow](../../../../cli/README.md#native-zydeco-units) shows the commands;
the [compiler contract](../../compiler/src/native-emission.md#native-unit-artifacts) owns artifact and linkage details.
Broader type interfaces and host-language bindings remain in the [ABI proposal](../../../proposals/zydeco-abi.md).
