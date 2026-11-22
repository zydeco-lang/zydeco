# C13. WebAssembly Backend and Standalone WASI Executables

The [wasm-sps](../../../../lang/wasm-sps/src/emit.rs) emitter produces relocatable `wasm32` object files (`.o`
with Linking Specification v2 `linking`, `reloc.CODE`, `reloc.DATA`,
and `target_features` `+tail-call`) using [wasm-common](../../../../lang/wasm-common/src).
`wasm-common` owns the shared WebAssembly contract: relocatable object scaffolding (`WasmObjectBuilder`,
`RelocFunction`), tagged-word encoding, inline scalar/memory helpers, the host-call ABI,
and `.rodata.strings` table packing.
The CLI links the emitted object against cached `wasm32-wasip1` `rlib` builds of `zydeco-machine`,
`zydeco-abi`, and [zydeco-host](../../../../lang/host/src/wasm.rs) (`wasm-runtime` feature) using `rustc`
and `rust-lld` (`-C target-feature=+tail-call -C link-arg=--no-check-features`),
producing a standalone `.sps.wasm` WASI executable whose only import module is `wasi_snapshot_preview1`.

Emission starts from SPSLow, after shared closure conversion and before ZASM lowering.
Starting at SPS would duplicate closure conversion, and recovering structure
from ZASM would require reconstructing lexical regions and stack effects from machine program points.
SPSLow retains lexical blocks and closed labels,
so each becomes one WebAssembly case function taking its ambient stack parameter (`(i64) -> ()`),
with structured WebAssembly instructions and per-case locals (`LocalPlan::for_case`) carrying its lexical bindings.
Control transfers use WebAssembly tail calls directly without a dispatcher loop or mutable globals:
a local label wraps its tail in a `block` and branches within the same frame; a closed label emits `return_call`
to its case function; and closure/continuation jumps load linked `__indirect_function_table` slots
from `.rodata.cases` (`R_WASM_TABLE_INDEX_I32`) and emit `return_call_indirect`.
`zydeco_wasm_entry` tail-calls `sps.root`.

Closure code handles are tagged case indices. Persistent `[head, tail]` stack frames,
boxed products, boxed `Int64`/`UInt64`/`Float64` scalars, and dynamic strings allocate 8-byte-aligned blocks
through `zydeco_wasm_alloc` from a bump arena over WASI linear memory, while manual `Addr` loads
and stores execute inline unaligned `i64.load*`/`i64.store*` instructions directly in linear memory.
Partial products use a pointer into the product suffix; constructors use `[tag, payload]`.
Emission sorts arena-derived IDs before assigning functions, imports, locals,
and static-data offsets; unsupported forms report typed emission errors.
Native C imports and Zydeco units remain rejected by the emitter.

## Module and Host ABI

Relocatable objects import linear memory (`env.__linear_memory`), the indirect table (`env.__indirect_function_table`),
and host functions from `"env"`, linked statically with `zydeco-host::wasm`:

| Import shape | Contract |
| --- | --- |
| Returning operation | Zydeco arguments as `i64`, one `i64` result |
| Control operation | Zydeco arguments as `i64`; returns `i32` pointer to `#[repr(C)] WasmHostTransfer` (`count`, `closure`, `first`, `second`) |
| Potential full-width scalar result | One trailing `i32` spare-box address allocated with `zydeco_wasm_alloc(1)` |
| `zydeco_wasm_alloc` | Requested 8-byte word count as `i32`, aligned linear-memory pointer as `i32` |
| `zydeco_runtime_error` | Fault code (`RuntimeFailure`) as `i32`, never returns |

Static string literals are packed at 8-byte-aligned offsets
in `.rodata.strings` using the same `(byte_len: u64, utf8_bytes...)` header as heap-allocated Wasm strings
and referenced via relocatable `i32.const` (`R_WASM_MEMORY_ADDR_SLEB`) extended with `i64.extend_i32_u`.
Control arity is at most two; spare boxes belong to the module's allocation protocol
so the host never invents closure layouts.

[zydeco_host::wasm](../../../../lang/host/src/wasm.rs) reuses shared I/O handles (`HostIoRuntime`),
`RetainedMemory`, string algorithms, `RuntimeFailure`, `HostIoErrorKind`,
and WASI entropy (`random_get`) from `zydeco-host`, `zydeco-abi`, and `zydeco-machine`.
For CLI execution and tests, `WasmRunner` runs the linked `.sps.wasm` binary with `wasmtime` (`WASMTIME`,
with `-W tail-call=y` and `--dir` preopens) and falls back to Node.js `node:wasi` (`NODE`) when `wasmtime` is absent.
`run` inherits terminal streams; `test` captures output with explicit input through the shared execution runner.
Conformance tests verify relocatable object imports, standalone `wasi_snapshot_preview1` imports,
100k-iteration tail recursion without stack overflow, memory faults, and cross-backend numeric/I/O parity.
