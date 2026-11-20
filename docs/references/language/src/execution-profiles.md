# Execution Profiles

The execution paths share source checking and static elimination, but have different allocation and host boundaries.
The native layout is an implementation ABI, not source-level control over addresses or object layout.

| Profile | Control and memory | Host boundary |
| --- | --- | --- |
| Interpreter | Explicit evaluator state; Rust-owned values and environments | CLI/REPL I/O; returning C imports on Unix |
| Native AMD64 | Machine control stack; growable retained environments; two fixed 1 MiB copying semispaces | Supplied runtime, Linux/macOS toolchain, returning C imports and scalar exports |
| `wasm-sps` | Block trampoline and persistent stack frames; growing non-collecting heap | Imports from a `zydeco` embedding |

Native managed live values must fit in one semispace, including headers.
Host-owned strings and byte buffers remain allocated within their runtime instance, independently of managed collection.
Environment growth and managed-heap capacity are distinct limits.
Native tail transfers reclaim dead activations, but live continuations and escaping values can retain storage;
The Wasm trampoline avoids growth of the host call stack without guaranteeing constant heap use.

Static value-function application expands residual bodies and can increase code size.
Product and thunk allocation depends on optimization.
Source byte slicing shares retained immutable storage on every backend.
Repeated concatenation can be quadratic.
Immutability guarantees observations, not identical costs on every backend.

The supplied Node host implements I/O, checked resources, and random integers for CLI execution and tests.
It reads stdin on demand and shares the supplied argument sequence with the program.
Native FFI requires installed libraries; Wasm does not support native C imports.
Source memory capabilities support checked allocation, addresses, and the explicit storage contracts in §13.
Ordinary compiler-managed values have no source-controlled ABI layout; primitive concurrency remains absent.
Runtime-managed capabilities provide the current resource boundary.

## Selecting an Execution Backend

Backend selection is command-line execution policy, independent of package meta annotations.
`run SOURCE -t TARGET` selects one of `interpreter`, `exe` (AMD64), or `wasm-sps`.
`--target` is the long spelling; omitting it selects `interpreter`.
`run` inherits terminal streams, forwards arguments after `--`, and returns the program's exit status.
`--dry` checks and selects the executable root without linking, emitting code, or requiring backend tools.

`test SOURCE` accepts the same targets and allows repeated `-t` options.
The test-only selection `-t all` expands in place to `interpreter`, `exe`, and `wasm-sps`, in that order.
Explicit choices replace the interpreter default; duplicates are removed, keeping first-occurrence order.
Discovery and source checking are shared across backends, with one lowering per test for compiled targets.
All selected tests and requested backend artifacts are prepared before any test executes.
Source errors, unsupported lowering, and missing toolchains fail preparation rather than skipping a backend.

Execution is sequential: each test in package-plan order runs on its backends in requested order.
Every test/backend pair reports a labeled result.
Zero exits pass; nonzero exits display captured stdout and stderr.
Runtime errors also fail their pair, and remaining pairs still run.
The summary counts pairs, and the command fails if any pair fails.
Wasm requires Node.js and uses the bundled host; native execution requires the AMD64 tools and runtime sources.
See the [workflow](../../../../cli/README.md#run-and-test-programs) for tool and runtime configuration.
