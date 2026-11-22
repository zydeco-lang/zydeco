# Execution Profiles

## Backend Profiles

The execution paths share source checking and static elimination, but have different allocation and host boundaries.
The native layout is an implementation ABI, not source-level control over addresses or object layout.

| Profile | Control and memory | Host boundary |
| --- | --- | --- |
| Interpreter | Explicit evaluator state; Rust-owned values and environments | CLI/REPL I/O; returning C imports on Unix |
| Native AMD64 | Machine control stack; growable retained environments; geometrically growing copying semispaces from 64 KiB | Supplied runtime, Linux/macOS toolchain, returning C imports and scalar exports |
| `wasm-sps` | Direct Wasm tail calls (`return_call` / `return_call_indirect`) and persistent stack frames; growing non-collecting linear-memory arena | Linked standalone `wasi_snapshot_preview1` executable with `zydeco-host` (`wasmtime`, or `node:wasi` fallback) |

## Memory, Cost, and Host Boundaries

Native managed live values occupy two geometrically growing copying semispaces starting
at 64 KiB and doubling on demand; reusable C FFI instances reset and cache spaces up to 4 MiB across calls.
Dynamic strings are managed heap objects reclaimed on collection, while static literals sit
in `.rodata` and CLI arguments stay outside the managed heap.
Host-owned byte buffers remain retained within their runtime instance independently of managed collection.
Environment growth and managed-heap capacity are distinct limits.
Native tail transfers reclaim dead activations, and Wasm tail calls (`return_call`
and `return_call_indirect`) execute unbounded source recursion in constant Wasm call-stack depth,
though live continuations and escaping values can retain frame or heap storage.

Static value-function application expands residual bodies and can increase code size.
Product and thunk allocation depends on optimization.
Source byte slicing shares retained immutable storage on every backend.
Repeated concatenation can be quadratic.
Immutability guarantees observations, not identical costs on every backend.

Both native and Wasm executables share `zydeco-host` implementations for I/O, checked resources, and OS random integers.
Standalone `.sps.wasm` binaries execute directly on WASI runtimes (`wasmtime` or Node.js `node:wasi`),
reading stdin on demand and receiving forwarded CLI arguments.
Native FFI requires installed libraries; Wasm does not support native C imports.
Source memory capabilities support checked allocation, addresses,
and the explicit storage contracts in [Primitive Values and Capabilities](primitives-and-capabilities.md)
and the [standard library reference](../../library/src/memory/manual-memory.md#manual-memory).
Ordinary compiler-managed values have no source-controlled ABI layout; primitive concurrency remains absent.
Runtime-managed capabilities provide the current resource boundary.

Command-line backend selection (`run -t`, `test -t`, `--dry`), test execution order,
and toolchain configuration are documented in the [CLI guide](../../../../cli/README.md#selecting-an-execution-backend).
