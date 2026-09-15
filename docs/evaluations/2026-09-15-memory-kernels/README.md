# Bounded raw memory kernels

This 2026-09-15 study measures the raw-memory kernel change following `c6e5ad99`.
The source and probe are committed with that implementation so the exact measured version can be checked out.
The [compiler reference](../../references/compiler.md#raw-memory-kernels) owns the supported contract.

The [probe](raw-int64.zy) loads an `Int64`, adds a runtime argument, multiplies by a literal, and stores it.
It then reloads and checks the full-width wrapping result through an ordinary continuation.
The allocation, initialization, argument construction, final observation, comparison,
and release remain outside the kernel.

| Representation policy | Whole-program scanned allocation sites | Whole-program opaque allocation sites | Raw kernels |
| --- | ---: | ---: | ---: |
| `boxed` | 10 | 8 | 0 |
| `local` | 10 | 4 | 1 |

The native kernel has zero calls and zero allocation sites on its successful path.
Four opaque allocation sites disappear: the load result, two arithmetic results, and the arithmetic literal.
The remaining ordinary boundaries and scanned allocation sites are visible costs of this program.
Counts are static code sites, not executed allocations, timings, or a whole-program zero-allocation claim.
Distinct raw stack homes still create spill traffic.
Wasm kernels retain two virtual-memory imports.

[results.json](results.json) records the source hash and counts produced by [probe.py](probe.py).
Reproduce from the repository root:

```sh
cargo build --offline --bin zydeco
python3 docs/evaluations/2026-09-15-memory-kernels/probe.py
cargo test --offline -p zydeco-cli --test passes scalar_memory_kernel -- --nocapture
cargo test --offline -p zydeco-assembly memory_kernel_writes --lib
```

The execution regressions compare boxed and local policies on native AMD64 and both Wasm backends.
They check `Int64` and `UInt64` wraparound, `Float64`, runtime operands, division before later remainder/store,
unknown callbacks, shared results, and the 32-operation bound with a rejected 33-operation counterpart.
An overlapping, unaligned-address case exercises load-before-store semantics.
The assembly interpreter pairs a successful write with division failure
and checks all destination bytes remain unchanged on failure.
Existing wide-load stress tests also retain live boxed roots across collection.

This study isolates the raw primitive boundary. Fixed fields, views, storage-only array composition,
and the header-array abstraction still need comparison after the source interface split.
Broader contification, raw source-call entries, managed/reference mixtures,
and register allocation remain separate work.
