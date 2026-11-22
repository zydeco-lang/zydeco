# Execution Profiles

## Backend Profiles

The execution paths share source checking and static elimination, but have different allocation and host boundaries.
The native layout is an implementation ABI, not source-level control over addresses or object layout.

| Profile | Control and memory | Host boundary |
| --- | --- | --- |
| Interpreter | Explicit evaluator state; Rust-owned values and environments | CLI/REPL I/O; returning C imports on Unix |
| Native AMD64 | Machine control stack; growable retained environments; two fixed 1 MiB copying semispaces | Supplied runtime, Linux/macOS toolchain, returning C imports and scalar exports |
| `wasm-sps` | Block trampoline and persistent stack frames; growing non-collecting heap | Imports from a `zydeco` embedding |

## Memory, Cost, and Host Boundaries

Native managed live values must fit in one semispace, including headers.
Host-owned strings and byte buffers remain allocated within their runtime instance, independently of managed collection.
Environment growth and managed-heap capacity are distinct limits.
Native tail transfers reclaim dead activations, but live continuations and escaping values can retain storage.
The Wasm trampoline avoids growth of the host call stack without guaranteeing constant heap use.

Static value-function application expands residual bodies and can increase code size.
Product and thunk allocation depends on optimization.
Source byte slicing shares retained immutable storage on every backend.
Repeated concatenation can be quadratic.
Immutability guarantees observations, not identical costs on every backend.

The supplied Node host implements I/O, checked resources, and random integers for CLI execution and tests.
It reads stdin on demand and shares the supplied argument sequence with the program.
Native FFI requires installed libraries; Wasm does not support native C imports.
Source memory capabilities support checked allocation, addresses,
and the explicit storage contracts in [Primitive Values and Capabilities](primitives-and-capabilities.md)
and the [standard library reference](../../library/src/memory/manual-memory.md#manual-memory).
Ordinary compiler-managed values have no source-controlled ABI layout; primitive concurrency remains absent.
Runtime-managed capabilities provide the current resource boundary.

Command-line backend selection (`run -t`, `test -t`, `--dry`), test execution order,
and toolchain configuration are documented in the [CLI guide](../../../../cli/README.md#selecting-an-execution-backend).
