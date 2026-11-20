# C13. WebAssembly Backend and Embedding

The [wasm-sps](../../../../lang/wasm-sps/src/emit.rs) emitter produces core `wasm32` modules
with `memory`, `entry`, and `_start` exports.
An embedding supplies the `zydeco` import namespace; the module is not a standalone WASI program.
[wasm-common](../../../../lang/wasm-common/src) owns the runtime contract: the tagged-word encoding,
the host-call ABI, the allocator body, and the module scaffolding.

Emission starts from SPSLow, after shared closure conversion and before ZASM lowering.
Starting at SPS would duplicate closure conversion, and recovering structure
from ZASM would require reconstructing lexical regions and stack effects from machine program points.
SPSLow still retains lexical blocks, so the emitter uses structured Wasm and locals
within a block and remains independent of lazy ZASM construction and native preparation.
The root and each SPSLow block become one function: structured code
and locals carry the block's lexical computations, and a trampoline transfers control between blocks,
because recursive host calls would consume the engine's stack on unbounded source transfers.
Closure code handles are tagged table indices. Persistent `[head, tail]` stack frames,
boxed products, and closure records live in a bump heap that grows without collection.
Partial products use a pointer into the product suffix; constructors use `[tag, payload]`.
Emission sorts arena-derived IDs before assigning functions, imports, locals,
and static-data offsets; unsupported forms report typed emission errors.
The emitter currently uses a whole-program local plan and uniform product boxing,
without ZASM's local-unboxing analysis.
A local label wraps its tail in a `block` and follows it with the label body,
so a jump sets the ambient-stack local and branches out of the `block` within the same function and locals.
A closed label becomes a function of its own, and a jump to it sets the ambient stack and a static program counter
before returning to the trampoline.

## Module and Host ABI

Source values cross as tagged `i64` words. Pointer-shaped values address module or host-owned representations;
module-created closures and stack records remain opaque to the host except through the shared protocol.

| Import shape | Contract |
| --- | --- |
| Returning operation | Zydeco arguments as `i64`, one `i64` result |
| Control operation | Arguments as `i64`; result is count, closure, and two argument slots, all `i64` |
| Potential full-width scalar result | A trailing `i32` spare-box address where the operation signature requires it |
| `string_literal` | UTF-8 byte offset and length as `i32`, opaque host string word as `i64` |

Control arity is at most two. Spare boxes belong to the module's allocation protocol;
the host must not invent closure layouts or return unregistered control code.
Native C imports are rejected by the emitter.

The [Node host](../../../../cli/wasm/wasm-host.mjs) supplies I/O, resources, random integers, and scalar adapters.
The CLI bundles it with its numeric and checked-memory helpers for source execution outside the checkout.
`run` inherits terminal streams; `test` captures output with explicit input through the shared execution runner.
Argument lookup uses the invocation's supplied sequence; lazy folds use module-created source closures,
so multi-argument traversal needs no host-created closure layout.
[Backend questions](../../../proposals/wasm-backends.md) retain default-target criteria and reclamation questions.
Conformance tests should distinguish module emission, embedding failures, memory exhaustion,
and source runtime failures.
