# Rust hosts: C exports and native Zydeco bindings

Rust can use Zydeco either as a provider of ordinary foreign functions or as a runtime
whose values and thunks remain available between host calls.
These lead to two useful integration paths with different amounts of work.
This survey records their scope and the proposed next steps; it does not introduce a new source annotation
or settle the host memory-ownership design.

An **ABI** is the binary agreement about value representations, entry points, and control transfers.
An **FFI binding** makes that agreement usable in a particular language.
The precise names here are **Rust consuming Zydeco's C export ABI** and **Rust bindings for Zydeco's native ABI**.
Both are Rust–Zydeco FFI. In both cases Rust is the host and initiates calls into Zydeco.

| Direction | Interface Rust consumes | Current foundation | Useful first outcome |
| --- | --- | --- | --- |
| C export ABI | Declared C function symbols and scalar signatures | Implemented C export libraries | Call an integer function from an ordinary Rust program |
| Native Zydeco ABI | A unit initializer returning typed values and thunks | Implemented structural native units; host adapters remain proposed | Retain a returned capturing thunk and call it repeatedly from Rust |

The source contracts remain in the references
for [C exports](../references/language.md#compiled-libraries-and-c-exports)
and [native units](../references/language.md#native-zydeco-units).
The broader [Zydeco ABI proposal](zydeco-abi.md) owns host ownership, general protocols, and richer type interfaces.
This document owns the comparison and staging of the Rust bindings.
For foreign-language applications calling a Rust component through UniFFI,
see [foreign-language hosts through a Rust adapter](zydeco-abi.md#foreign-language-hosts-through-a-rust-adapter).
Rust is the callee at that outer FFI boundary.

## Rust consuming Zydeco's C export ABI

This is the smallest route when the application needs independent scalar calls.
Rust declares the symbols from a `library(c, ...)` artifact with `unsafe extern "C"` and links the native library.
For example, the existing [arithmetic library](../../lib/ffi/arithmetic.zy) supplies this entry:

```rust
unsafe extern "C" {
    fn example_add(left: i64, right: i64) -> i64;
}

fn main() {
    // The linked library declares this signature; these inputs fit Zydeco Int.
    let sum = unsafe { example_add(19, 23) };
    assert_eq!(sum, 42);
}
```

The [C library workflow](../../CONTRIBUTING.md#compile-and-consume-c-libraries) builds the artifact
and explains its dependencies and runtime support.
Rust's [external blocks](https://doc.rust-lang.org/reference/items/external-blocks.html) supply the declarations;
a [Cargo build script](https://doc.rust-lang.org/cargo/reference/build-scripts.html) can supply link inputs
and generated bindings.
No C source wrapper is necessary.

The binary call path is available today.
The repository does not yet provide a Rust binding generator or Cargo helper.
A minimal supported integration would generate declarations from the manifest's checked export signatures,
validate the artifact and dependency closure, and link the unit code with the required runtime support.
Shared libraries already contain their private support; raw objects
and static libraries need matching support linked once, as specified by the artifact contract.

A safe convenience wrapper should validate source ranges before entry, including the narrower payload range
of `Int` and `UInt`, and respect the library's entry guard.
The current profile admits up to six integer arguments and an integer or `Unit` result.
It creates fresh state for each call and terminates the process on a runtime fault.
It cannot return a persistent Zydeco value or thunk.
These are properties of the current export profile; Rust declarations alone do not extend it.
Additional C signature shapes belong in the [foreign-interface proposal](c-ffi.md).

## Rust bindings for Zydeco's native ABI

This direction is useful when Rust needs to work with a Zydeco library's ordinary interface.
Consider a factory returning an adder that captures its first argument.
A proposed Rust binding could expose:

```rust
let runtime = Runtime::new();
let math = math::instantiate(&runtime)?;

let add_seven = math.make_adder(7)?;
assert_eq!(add_seven.call(35)?, 42);
assert_eq!(add_seven.call(100)?, 107);
```

This API is schematic. The returned handle keeps the source closure and its environment available across calls.
The ownership form of that handle remains subject
to the [runtime binding design](zydeco-abi.md#5-runtime-bindings-and-ownership).
The `?` operations illustrate binding validation errors; native faults retain their current fatal behavior.

The first binding can use the complete current
[`UnitValueType` / `UnitStackType` schema](../../lang/syntax/src/unit.rs): primitives,
named and nested products, and thunks whose protocols contain argument arrows and `Ret`.
This supports returning Zydeco-created thunks and passing them back into Zydeco.
Nominal types, general codata, and dependent interfaces can be added when their public schemas are implemented.
They are independent of this first structural binding.

### The implementation boundary

The initial scope is synchronous calls on one host thread with statically linked native units.
Current native targets are AMD64 Linux and macOS, so the Rust host must target the same architecture and OS.
An ARM-native Rust process needs another backend or embedding route.

| Part | Required work |
| --- | --- |
| Runtime instance | Separate instance creation/destruction from invocation activation/return, preserving heap and host state between calls |
| Host values | Register writable roots for retained values and intermediates; connect handle release to the chosen ownership API |
| Invocation bridge | Save platform registers, arrange the environment and arguments, install a host return delimiter, enter Zydeco, and preserve the result before restoring Rust |
| Typed Rust wrappers | Generate projections and returning-call methods from the unit schema; retain source type evidence and check instance identity |
| Build integration | Validate unit manifests and their transitive dependencies, link their objects with one compatible runtime, and generate bindings |

The compiler already publishes each ordinary source value as one word.
Primitive conversions and product field routes therefore make most wrapper generation mechanical.
A Zydeco closure code address still needs an invocation bridge:
an ordinary Rust function call does not establish the required environment and continuation stack.
The bridge can have a platform `extern "C"` entry
and use [Rust assembly facilities](https://doc.rust-lang.org/reference/inline-assembly.html) for the native transfer.
Its argument-word transport can be shared across signatures.
Existing [closure records and resumption bridges](../../lang/machine/src/native.rs) provide a starting point.

There are two different invocation cases: a native unit initializer receives a return stack
without an extra closure environment; calling a returned thunk also supplies its captured environment.
Both must preserve argument order, stack alignment, and the caller's registers.
Every allocation path must see host roots, including partial argument construction and the result before return.
Keeping a runtime alive alone does not establish these invariants.

### Later extensions

Implementing a returning Zydeco thunk in Rust needs the reverse bridge: enter a Rust implementation
through a runtime-compatible closure, then deliver its result to the source continuation.
The [host-thunk design](zydeco-abi.md#7-implementing-a-zydeco-thunk-in-another-language) owns context retention,
panic containment, and transfer-driven dispatch.

General computation protocols add difficulty beyond memory ownership.
A codata observation may consume further observations before returning,
so method-like syntax may need to build an invocation plan.
Explicit CPS can forward an unknown residual stack; a transfer-driven dispatcher must preserve
that stack without accumulating Rust call frames.
Polymorphic and abstract interfaces also need type relationships that cannot be reconstructed from erased words.
These are separate milestones after returning-thunk invocation.

## Workload and validation

The following are planning estimates for one developer familiar with the compiler,
one existing target, and focused tests and documentation.
They are not measured delivery times.

| Scope | Estimated engineering days |
| --- | --- |
| Rust declarations, basic Cargo integration, and regressions for current C exports | 1–3 |
| A minimal structural native binding, including persistent runtime and external roots | 10–20 |
| The native bridge, wrappers, and build integration when instance/root APIs are already implemented | 5–10 |

The last row estimates the remaining portion of the native binding, not an additional phase to add to its total.
Merely choosing an ownership policy does not implement its root and activation machinery.
The broader host-platform estimates
in the [ABI proposal](zydeco-abi.md#10-workload-and-pitfalls) include richer interfaces and lifecycle behavior.
Rust callbacks, general protocols, dynamic unloading, concurrency, recoverable native faults,
and new targets require separate estimates.

Use the following acceptance gates before describing either integration as supported:

| Accepted case | Rejected counterpart or failure check | Evidence |
| --- | --- | --- |
| Rust calls C exports with valid integer edge values through shared and static linking | Safe wrapper rejects out-of-range source integers; raw entry retains its documented fatal behavior | Run a Rust consumer and isolate fatal cases in subprocesses |
| A consumer links from producer artifacts without implementation source | Missing, corrupt, wrong-target, or incompatible dependencies | Reject before source entry |
| Rust retains a capturing thunk across later calls and forced GC | A value or thunk from another instance | Capture remains correct after relocation; invalid invocation changes no source state |
| Products and call arguments follow their declared types and order | Incorrect field selection, type, or arity | Binding rejects before dispatch |
| Repeated calls return through both argument-stack parities | Lost caller state or accumulating invocation frames | Host computation remains correct and activation storage stays bounded |

The first C milestone is a maintained Rust consumer of the existing scalar library.
The first native milestone is the returned-adder example with collection between calls,
using only the producer's published artifacts.
Use it to validate the Rust binding before adding richer interfaces or Rust-implemented thunks to that binding.
