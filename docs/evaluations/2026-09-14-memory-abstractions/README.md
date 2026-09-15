# Memory abstraction code-generation comparison

This 2026-09-14 probe compares a raw scalar read, a hand-written fixed `View`, and a library header view.
The fixed view matches the raw path's measured code-site counts.
The header view retains two zero-displacement address calls and one additional native managed-allocation call site.
This is evidence for the [memory compilation proposal](../../proposals/memory-compilation.md),
not a runtime performance measurement or a general ranking of view representations.

## Workload and measurement boundary

Each fixture allocates eight bytes with alignment eight, stores `Int64` value `123`,
reads it, frees the original allocation, and exits successfully only when the observed value is `123`.

- [Raw](raw.zy) calls the scalar load through an ordinary helper.
- [Fixed view](fixed-view.zy) adds a statically selected `View Cps Addr Int64` around that load.
- [Header view](header-view.zy) realizes a fixed scalar layout and opens an inline header with displacement zero.
  Its callback ignores the returned payload pointer and observes only the loaded value.

The comparison concerns these complete programs and their used result.
The header interface additionally constructs a pointer/value result whose unused pointer is an elimination candidate;
the full interfaces do not have identical result protocols.
The fixtures isolate the successful read path rather than testing indexed bounds, loops, or general callbacks.

The compiler and standard library are from `6ddbca3f1cb4196e1939b8b9877d5a6e698c7b14`;
only documentation and these evaluation fixtures were added during measurement.
The host is arm64 Darwin with Rust/Cargo 1.98.0.
The CLI uses the repository's development profile, default normalization, and default `Local` representation
for generated AMD64 assembly (`--target asm --target-arch x86-64`).
All three fixtures ran in the interpreter with exit status zero.
Native assembly was generated and inspected, without linking or native execution.
The [manifest](manifest.json) records the full compiler revision, toolchain, profile, and fixture SHA-256 hashes.

## Results

| Fixture | Portable product/closure sites | Portable payload words | Native scanned-allocation call sites | Native opaque-allocation call sites | Native offset call sites |
| --- | ---: | ---: | ---: | ---: | ---: |
| Raw | 12 | 25 | 12 | 1 | 0 |
| Fixed view | 12 | 25 | 12 | 1 | 0 |
| Header view | 15 | 37 | 13 | 1 | 2 |

[Portable counts](representations.csv) come
from the [representation comparison](../../../cli/examples/representations.rs),
which counts `PackProduct` sites and fields.
All six compared policies produce the same counts for each fixture,
with no reported cell elimination or variable expansion at that pass.
[Native counts](native-call-sites.csv) count direct calls to the named runtime symbols after native preparation.
That later boundary has fewer construction sites in the header case; the portable
and native columns are distinct metrics and must not be summed.
Every native fixture also has one scalar load call and one scalar store call.

Assembly inspection shows source displacement zero at both header-view offset calls.
The results identify removable address work and residual packaging in this particular composition.
They also expose costs shared with the raw baseline, including scalar-load boxing machinery.
Static sites do not measure executed allocations, runtime-internal work, retained frames, peak memory, or speed.
The probe does not establish optimized native runtime behavior, Wasm costs, or a guarantee for arbitrary fixed views.

## Reproduction

Run from the repository root with the recorded compiler revision and these fixtures present.
Keep new results in a separate directory when comparing a later revision.

```sh
cargo build --quiet --bin zydeco
memory_study=docs/evaluations/2026-09-14-memory-abstractions
memory_audit_output=$(mktemp -d)
for name in raw fixed-view header-view; do
  target/debug/zydeco run "$memory_study/$name.zy"
  target/debug/zydeco build "$memory_study/$name.zy" --target asm --target-arch x86-64 \
    > "$memory_audit_output/$name.asm"
done
cargo run --quiet -p zydeco-cli --example representations -- \
  "$memory_study/raw.zy" "$memory_study/fixed-view.zy" "$memory_study/header-view.zy" \
  > "$memory_audit_output/representations.csv"
python3 - "$memory_audit_output" <<'PY' > "$memory_audit_output/native-call-sites.csv"
import csv
import re
import sys
from pathlib import Path

output = Path(sys.argv[1])
symbols = ('zydeco_alloc_scanned', 'zydeco_alloc_opaque', 'zydeco_memory_offset',
           'zydeco_int64_load_le_branch', 'zydeco_int64_store_le_branch')
writer = csv.writer(sys.stdout, lineterminator='\n')
writer.writerow(['source', *symbols])
for name in ('raw', 'fixed-view', 'header-view'):
    assembly = (output / f'{name}.asm').read_text()
    writer.writerow([name] + [len(re.findall(r'^\s*call\s+_?' + symbol + r'\s*$', assembly, re.M))
                              for symbol in symbols])
PY
diff -u "$memory_study/representations.csv" "$memory_audit_output/representations.csv"
diff -u "$memory_study/native-call-sites.csv" "$memory_audit_output/native-call-sites.csv"
```
