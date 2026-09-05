# Returning C imports

A foreign import supplies the implementation of a Zydeco thunk.
The first example, `XXH64`, needs two source arguments but three C arguments:
its byte buffer becomes a pointer and a length.
Adding `XXH3_64bits`, which needs no seed argument, motivates a compositional protocol shared by checking and execution.

## Classifier and boundary contract

The source form is an annotated implementation hole:

```zydeco
@(ffi(c, library("xxhash"), symbol("XXH3_64bits"))) : Thk (Bytes -> Ret UInt64)
```

`@(meta)` abbreviates `@[meta] _`.
The ABI, library linker name, and unmangled symbol identify the foreign implementation;
its classifier is supplied by the surrounding typing context.
Checking does not load a library or inspect a C header.

The accepted classifier is `Thk (A1 -> ... -> An -> Ret UInt64)`, including `Thk (Ret UInt64)` when `n = 0`.
Each argument is independently interpreted by these rules, preserving source order:

| Source argument | C arguments | Boundary contract |
| --- | --- | --- |
| `UInt64` | `uint64_t` | Preserve all 64 bits, independently of Zydeco's tagged representation. |
| `Bytes` | `const void *`, `size_t` | Borrow contiguous immutable bytes for this call. |

The result is a C `uint64_t`, re-encoded as a Zydeco `UInt64`.
The common subset allows at most six flattened C arguments, matching the integer/pointer registers used
by the native AMD64 implementation.
A byte borrow counts as two arguments.
Other parameter types, result types, and control protocols receive type errors;
the six-argument bound is checked before either execution path is selected.

The classifier determines the CBPV control protocol; the boundary contract determines its C representation.
In particular, expanding `Bytes` into two arguments is an explicit FFI convention,
not an intrinsic property of the value type.
The pointer may be read only within the supplied length and during the call.
A slice lends its visible window, not the entire backing allocation.
The callee must not mutate the buffer, retain the pointer, or dereference it when the length is zero.

The declaration author must ensure that the real symbol obeys the declared C signature and borrowing contract.
If a call returns, it must return through the C ABI; unwinding, nonlocal jumps, and reentry into Zydeco are forbidden.
These are trusted obligations, not properties established by checking a classifier.
`Ret` specifies how a result is delivered; it does not imply purity or termination.

## One typed call plan

Checking walks the normalized arrow spine and records a `ForeignSignature`.
Its constructor enforces the argument bound, and its fields are private.
Expanding the signature yields ordered `ForeignArgument` entries, each naming a source parameter
and a scalar component: an integer, byte pointer, or byte length.
The interpreter and native emitter consume this same plan.
Neither recognizes a whole xxHash signature.

The Unix interpreter uses libffi to construct the platform C call interface from those components.
Scalar argument storage borrows the source values until the call finishes.
Libraries are loaded lazily with `dlopen`, symbols with `dlsym`, and call interfaces are cached by target and signature.
Missing libraries and symbols are runtime errors.
libffi is an interpreter dependency, not a generated-program dependency.

Native AMD64 code retains the source arguments while marshalling into a temporary stack frame,
then loads the six System V argument registers as needed and calls the external symbol.
Marshalling helpers do not allocate.
The frame contains raw C values and is discarded before the result allocation can trigger GC;
the full-width result survives that allocation in a callee-saved register.
Finally the bridge consumes the `Ret UInt64` continuation and resumes it with the encoded result.

Native linking adds `-l<library>`; the interpreter searches for `lib<library>.so` or `lib<library>.dylib`.
Library installation and platform linker/loader search paths remain the caller's responsibility.
WebAssembly emission currently rejects native imports explicitly.

## Examples and checks

The [xxHash binding](../../lib/ffi/xxhash.zy) exports `xxh64` and `xxh3_64bits` from its Builtin-parameterized factory.
Their classifiers differ only by the seed argument.
The [upstream API](https://xxhash.com/doc/v0.8.3/group___x_x_h3__family.html) specifies the pointer-and-length contract
for `XXH3_64bits`.

Focused tests pair valid signatures with unsupported arguments, results, residual computations, and excessive C arities.
A small C fixture checks zero arguments, scalar-only calls, multiple byte borrows,
argument order, empty input, and full-width values.
Repeated native calls exceed the copying heap's capacity while retaining live integers and a byte buffer.
Loader failures and invalid runtime arguments must produce their intended errors without entering foreign code.
Installed-xxHash and native execution tests remain explicit opt-in checks where external libraries
or native toolchains are required.

```sh
cargo test -p zydeco-statics --test foreign
cargo test -p zydeco-dynamics foreign::tests
cargo test -p zydeco-tests --test ffi
# With xxHash on the loader search path and the native toolchain installed:
cargo test -p zydeco-tests --test ffi -- --ignored
```

## Following boundary

C-to-Zydeco exports are the next independent question: a C caller must establish a runtime
and a CBPV return continuation.
Closures and callbacks also need environment ownership, lifetime, and reentry rules;
`OS` entry points need a root-stack protocol instead of an ordinary C return.
Those choices, additional scalar representations, stack-passed arguments, aggregates,
and header-based validation are outside this subset.
The current plan provides a place to describe them without committing to their source syntax.
