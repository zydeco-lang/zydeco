# Isolated assembly lowering evaluation

This study measures the CPS assembly-lowering refactor on 2026-09-14.
The current folder takes about **5–10% longer inside lowering** on the six nontrivial source fixtures,
while making substantially fewer heap allocations and retaining more peak heap storage.
The absolute differences are microseconds on these inputs.
Earlier whole-debug-CLI observations of 2–9% slower builds do not establish an effect caused by this pass:
their millisecond-scale differences are much larger than the isolated lowering differences measured here.

The [compiler reference](../references/compiler.md#cps-assembly-lowering) owns the implemented scheduling,
ownership, and stack-safety contracts.
This report owns the measurements and their interpretation.

## Comparison and measurement boundary

All four implementations run in the same executable on the same prepared SPSLow arena and root.
The surrounding compiler and shared assembly types come from `370195d8`.
Historical lowerers are extracted from Git into a temporary source archive;
they are absent from the production compiler.

| Variant | Implementation | Question answered |
| --- | --- | --- |
| Original | `9c1d3000`: boxed consumers, context updates, and pending instructions | What did the complete refactor change? |
| Typed jobs | `8e508e36`: typed context updates and pending instructions, boxed consumers | What did the first step contribute? |
| Explicit | `370195d8`: typed consumers and the default `Explicit` driver | What does the current compiler pay? |
| Recursive | `370195d8`: the same folder with `Recursive` | How much comes from driver choice within the new representation? |

Each source is compiled to SPSLow once per measurement process.
Before each timed invocation, the harness constructs a fresh lowerer and completes local representation analysis.
The timer covers `Lowerer::run`, including destruction of its internal scratch storage and owned analysis data.
Destruction of the returned assembly is timed separately.
Source preparation, representation analysis, later stack analysis, native frame preparation,
code emission, and execution of the generated program are outside the lowering timer.
Both portable and native-frame lowering use the `Local` representation policy.

Before measuring each source and mode, the harness compares all variants' emitted instructions, contexts,
dependency edges, variables, symbols, definitions, labels, external declarations, root, and frame-entry metadata.
It normalizes only the fresh output ID key spaces.
All comparisons passed for all seven sources and both modes.

Timings use an uninstrumented allocator. Each process warms every variant eight times,
then calibrates a common batch size to approximately 5 ms of original lowering, capped at 1,024 invocations.
Every batch measures each variant, rotating their order across 32 batches.
There are two separate timing processes per build profile, giving 64 paired batch observations per comparison.
The retained data contains 7,168 timed batches across both profiles, both modes, and all variants.

Allocation counts come from separate executables that wrap Rust's `System` allocator.
Counters are active only during the same lowering boundary, with eight observations
per source, mode, and variant in each profile.
All 896 allocation observations agreed within their respective cases, and debug and release allocation counts agreed.

## Timing results

The machine runs arm64 macOS 26.6.2; the compiler is Rust 1.98.0 with LLVM 22.1.8.
Debug means the repository's development profile, including its existing dependency overrides;
the assembly crate itself is unoptimized.
Release uses the repository's optimized profile with thin LTO.
No build or other benchmark was launched concurrently with the timed processes.

Percentage changes below are the median of paired batch ratios across both runs,
where each batch duration is divided by its iteration count first.
They are descriptive sample statistics, not confidence intervals.
Absolute release times show the second run's medians, in microseconds.
The first release run was more than twice as slow on some early fixtures and had wider within-run variation;
both runs remain in the evidence, and both contribute to the relative comparisons.
The experiment did not pin CPU cores or identify the cause of that variation.

| Source | Mode | Assembly nodes | Debug change | Release change | Release original → explicit, µs |
| --- | --- | ---: | ---: | ---: | ---: |
| `fact.zy` | Portable | 276 | +8.6% | +7.5% | 46.73 → 50.51 |
| `fact.zy` | Native | 265 | +7.2% | +6.9% | 45.48 → 49.05 |
| `native-frames.zy` | Portable | 104 | +8.6% | +10.0% | 15.93 → 17.58 |
| `native-frames.zy` | Native | 97 | +8.0% | +7.4% | 15.12 → 16.38 |
| `host-return.zy` | Portable | 93 | +8.7% | +10.4% | 15.35 → 16.78 |
| `host-return.zy` | Native | 66 | +6.4% | +8.8% | 11.45 → 12.47 |
| `host-runtime.zy` | Portable | 525 | +7.9% | +7.6% | 93.10 → 100.09 |
| `host-runtime.zy` | Native | 383 | +7.3% | +7.2% | 67.38 → 72.11 |
| `reset-shift-k.zy` | Portable | 191 | +7.4% | +8.1% | 31.11 → 33.66 |
| `reset-shift-k.zy` | Native | 184 | +7.5% | +8.0% | 30.54 → 32.93 |
| `gc-stress.zy` | Portable | 439 | +8.9% | +6.6% | 71.54 → 76.90 |
| `gc-stress.zy` | Native | 422 | +8.1% | +5.3% | 68.75 → 73.13 |

The seventh fixture, `pattern-alias.zy`, reduces to only two output nodes.
Its fixed overhead is proportionally larger: about 17–18% in debug and 26–30% in release.
The second release run changed from roughly 0.24 to 0.31 µs per lowering.
It is retained as a small-input control and excluded from the six-fixture headline range.
Across the larger fixtures, the differences between each run's original and explicit medians were about 5–53 µs
in debug and 1–7 µs in release.

The intermediate variants help separate two implementation decisions.
Typed jobs alone reduced time by approximately 2–3% in debug and 6–10% in release on the larger fixtures.
Within the new folder, `Explicit` took about 2–3% longer than `Recursive` in debug and 4–5% in release.
The full old-to-new comparison therefore includes the consumer representation and scheduling changes,
as well as the generic driver's choice of storage for return frames.
Switching drivers does not restore the original closure-based implementation.

## Allocation and retention results

On the larger fixtures, the current folder makes approximately 36–45% fewer allocation calls.
It makes more reallocations as vectors grow, requests about 7–38% more bytes cumulatively,
and adds roughly 13–97 KiB to peak live heap above the prepared-lowerer baseline.
Fewer allocation calls therefore do not imply a smaller memory footprint.

The following native-mode cases illustrate the tradeoff.
Allocation calls exclude reallocations; peak columns report requested heap payload bytes converted to KiB.

| Source | Allocation calls, original → explicit | Reallocation calls | Peak heap increment, original → explicit |
| --- | ---: | ---: | ---: |
| `native-frames.zy` | 495 → 286 | 1 → 6 | 41.0 → 53.7 KiB |
| `host-runtime.zy` | 2,069 → 1,194 | 7 → 14 | 188.6 → 236.7 KiB |
| `reset-shift-k.zy` | 967 → 551 | 2 → 8 | 80.9 → 105.8 KiB |
| `gc-stress.zy` | 2,233 → 1,231 | 2 → 9 | 231.7 → 280.7 KiB |

For every measured case, changing the new folder from `Recursive` to `Explicit` added exactly one allocation,
512 cumulatively requested bytes, and 512 bytes of peak heap increment.
The larger memory difference from the original lowerer is consequently shared by both new drivers.
The append-only consumer vector is a plausible target for reducing retention:
consumed payloads are released, but their slots remain allocated until lowering finishes.
This study does not profile allocation sites, so it does not attribute every extra byte to that vector.

Cumulative requested bytes sum allocation sizes and the full new size of each successful reallocation.
Peak heap increment is the highest live requested heap size during lowering minus the live size just before lowering.
The baseline includes prepared analysis data that can be released during the call.
These figures include output construction and lowering scratch storage,
but exclude allocator bookkeeping, the Rust stack, and unchanged live input data.
They are neither process RSS nor a measure of generated-program memory.

## Interpretation and further measurements

The measured cost is a modest slowdown of this isolated folder, with higher heap retention.
The existing depth regressions establish the refactor's stack-safety benefit;
these timing samples do not establish a throughput improvement or a general scaling result.
The fixtures contain 4–692 SPSLow nodes and produce 2–525 assembly nodes.
Their runtime loops do not make compilation inputs correspondingly large.

The earlier whole-CLI measurements cannot support the claim that this pass caused a 2–9% whole-build regression.
Warm isolated lowering has different cache and allocator conditions from one invocation inside a complete build,
but its measured microsecond deltas provide no explanation for those earlier millisecond deltas.
A causal whole-build estimate would require controlled repeated end-to-end measurements
with these phase boundaries instrumented separately.
Multiplying the isolated percentage by a whole-build duration would be incorrect.

If further optimization is warranted, first measure a larger SPSLow size series and profile consumer-slot retention.
Reusing consumed slots or reducing record size can then be evaluated while preserving affine consumption,
abandoned-chain teardown, and instruction publication order.
Driver-specific tuning should be compared with the same consumer representation;
the four-way comparison here provides that baseline.
No compiler optimization is part of this study.

## Reproduction and evidence

The [runner](../../lang/tests/assembly-lowering-study.py) creates an archived checkout at the pinned revision,
injects the measurement boundary and historical lowerers, builds both profiles offline,
and writes a manifest plus timing and allocation CSV files.
The destination must be new. It needs Python with tar extraction filters, Rust, the pinned Git commits,
and cached workspace dependencies.

```sh
python3 lang/tests/assembly-lowering-study.py --output /tmp/assembly-lowering-study
```

Use `--target-dir "$PWD/target"` to reuse the repository's Cargo cache, `--profiles release` to select one profile,
or `--prepare-only` to inspect the generated sources without building.
The frozen harness measures the three historical revisions above, including after the production compiler changes.
It adds no production API and retains no duplicate historical lowerer source in the repository.
The generated examples pass focused Clippy with warnings denied.

The [manifest](assembly-study-2026-09-14/manifest.json) records revisions, toolchain,
source and harness hashes, commands, measurement boundaries, and output checks.
The [timing samples](assembly-study-2026-09-14/timings.csv) retain every batch from both runs,
including separate output-destruction durations.
The [allocation samples](assembly-study-2026-09-14/allocations.csv) retain every counter observation.
`run` and `round` are zero-based; `native` selects frame-entry lowering; `output_counts` lists programs,
variables, symbols, and frame entries in that order.
`lower_ns` and `drop_ns` are batch totals and must be divided by `iterations` before comparing samples.
