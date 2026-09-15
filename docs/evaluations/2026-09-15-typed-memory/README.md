# Typed fields and checked header-array updates

This 2026-09-15 study extends the [raw-kernel study](../2026-09-15-memory-kernels/README.md)
through the implemented storage/codec split, typed fields, fixed views, and checked array access.
The source migration is `7d3e3772`; the address-binding planner change and these measurements are committed together.
Check out the commit containing this study to reproduce its compiler and library.
The [compiler reference](../../references/compiler.md#raw-memory-kernels) owns the implemented contract;
[memory compilation](../../proposals/memory-compilation.md#first-acceptance-target-and-order) owns the remaining costs.

## Workloads and boundaries

The [typed field fixture](../../../lib/tests/std/typed-memory-kernel.zy) constructs two `Int64` fields,
takes and increments the right field, initializes it again, and observes it through a composed fixed view.
It checks full-width wrapping and preservation of the sibling before taking both fields and releasing storage.
Its state transitions and fixed field calculation expose one local load/add/store kernel.

[Raw](header-raw.zy) and [typed](header-typed.zy) header updates share the same [driver](header-driver.zy).
The driver allocates 48 bytes aligned to 16: an eight-byte `Int` length,
eight bytes of padding, and four eight-byte `Int64` elements.
Filling with `99` initializes all payload slots and makes padding observable;
it then stores length three and the signed maximum in slot two.
The typed implementation composes record/array geometry, an inline header view,
checked counted access, and the selected scalar codec.
It selects no whole-array logical codec.

Both updates load the header, check `0 <= length <= 4` and `0 <= index < length`,
check displacement multiplication, and update the chosen scalar with wrapping addition.
The direct implementation retains the same checks, failure variants, access order, and address obligations.
The driver performs one successful update at index two, then rejects indices three, minus one, and `Int` maximum.
It also rejects lengths five and minus one.
After every rejection it checks that slot two retains the successful result,
and it checks a padding byte before releasing the original base and geometry.
Allocation, test observations, ordinary call boundaries, and release are outside the measured kernel.

The existing [header-array fixture](../../../lib/tests/std/header-array.zy) additionally exercises direct
initialization, inline/prefix interpretations, saved versus fresh length observations, and capacity rejection.
The [storage-only array](../../../lib/tests/std/storage-arrays.zy) checks size-eight/alignment-64 elements:
stride 64, total extent 128, and untouched inter-element padding.
Neither selects a whole-array codec.
These are different workloads; their total counts are context, not comparisons with the header-update pair.

## Static code-generation results

| Workload | Scanned allocation sites, boxed / local | Opaque allocation sites, boxed / local | Local raw kernels |
| --- | ---: | ---: | ---: |
| Earlier raw scalar update | 10 / 10 | 8 / 4 | 1 |
| Typed field and fixed view | 30 / 30 | 10 / 7 | 1 |
| Direct checked header update | 55 / 55 | 6 / 3 | 1 |
| Typed checked header update | 76 / 76 | 6 / 3 | 1 |
| Existing std header-array | 114 / 114 | 0 / 0 | 0 |
| Storage-only padded array | 90 / 90 | 0 / 0 | 0 |

The boxed policy forms no raw kernels. Each local kernel above has one load, arithmetic, and one store,
with zero successful-path calls and zero implicit allocation sites inside that boundary.
The typed field and both header updates each remove three opaque sites: the loaded value,
increment literal, and arithmetic result.
Pure field-address bindings may move before the load while retaining their later uses;
possibly failing displacement calculations retain their original order.

The complete typed header update has **21 additional scanned allocation sites** relative to the direct implementation.
Both retain substantial surrounding control and ordinary-value costs.
The bounded kernel criterion is satisfied; the complete checked-path zero-overhead criterion remains open.
The next compiler comparison should target known view/indexing helpers and their residual packages
and continuations, preserving the direct path's checks.
This site difference does not itself assign each allocation to a particular optimization mechanism.

Counts describe static generated sites, not executed allocation counts, peak retained storage, or timings.
Requested allocation and memory fill remain explicit host operations; raw scalar stack homes still incur spill traffic.
Wasm retains virtual-memory imports and address lookup.
The study establishes neither minimal instruction counts nor universal zero-cost abstraction across loops,
unknown callbacks, managed references, or source-call entries.

## Validation and reproduction

[results.json](results.json) records source hashes, host/toolchain information,
and the counts from [probe.py](probe.py).
The CLI is built in the development profile; assembly uses AMD64, default high-SPS normalization,
and explicit `boxed`/`local` representation policies.

The [CLI regression](../../../cli/tests/passes.rs) checks kernel count, the three-site opaque reduction,
one kernel store, and absence of kernel calls/allocations for the field fixture and both header updates.
It executes all three with both policies on native AMD64 and both Wasm backends.
The new header pair also passes the reference interpreter; all 37 general-memory tests pass,
including the existing header and storage-only array fixtures on all four backends.
Related memory regressions retain invalid-carrier checks, callback fallbacks, unaligned accesses,
arithmetic failure before store, and live managed roots across collection.

From the repository root:

```sh
cargo build --offline --bin zydeco
python3 docs/evaluations/2026-09-15-typed-memory/probe.py
target/debug/zydeco run docs/evaluations/2026-09-15-typed-memory/header-raw.zy
target/debug/zydeco run docs/evaluations/2026-09-15-typed-memory/header-typed.zy
cargo test --offline -p zydeco-cli --test passes typed_memory_kernels
cargo test --offline -p zydeco-cli --test passes scalar_memory
cargo test --offline -p zydeco-tests --test general_memory
```

Keep later measurements separately; the earlier dated studies describe their recorded revisions.
