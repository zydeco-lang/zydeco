# Evaluations

Experiments inform implementation choices under particular workloads, revisions, and toolchains.
This directory retains dated reports, measurements, and reproduction artifacts so
that their methods, findings, and limits can be assessed together.
Results describe the measured revisions; they do not establish a current performance ranking.

Each study lives in a `YYYY-MM-DD-topic/` directory, with its report in `README.md` beside its evidence files,
reproduction scripts, and study-only supporting sources.
Keep original measurements and manifests intact, including recorded commands, paths, and source hashes.
Reusable measurement tooling remains beside the code it exercises,
such as the [pipeline comparison](../../cli/examples/pipelines.rs)
and the [representation comparison](../../cli/examples/representations.rs);
this directory keeps only the dated runs, evidence, and reproduction instructions.
The [references](../references/README.md) own current contracts and settled design rationale;
concrete unfinished designs belong in [proposals](../proposals/README.md) and open conceptual questions
in [ideas](../ideas/README.md).

| Date | Study | Scope |
| --- | --- | --- |
| 2026-09-07 | [Bug-hunt evidence](2026-09-07-bug-hunt/README.md) | Method and retained historical measurements from the `e00b91f0` audit: interpreter value lookup, checker host stack, long-running host retention, and product-suffix layouts. |
| 2026-09-08 | [CBPV runtime evaluation](2026-09-08-cbpv-runtime/README.md) | Environment storage, capture copying, root discovery, native runtime comparisons, and Wasm memory. |
| 2026-09-14 | [Isolated assembly lowering evaluation](2026-09-14-assembly-lowering/README.md) | Boxed CPS and folder comparisons, isolated pass timing and allocation costs, consumer-slot reuse, reduced dispatch, and rejected optimization experiments. |
| 2026-09-14 | [Memory abstraction comparison](2026-09-14-memory-abstractions/README.md) | Raw, fixed-view, and header-view code sites, interpreter checks, and reproducible limits of erasure. |
| 2026-09-15 | [Bounded raw memory kernels](2026-09-15-memory-kernels/README.md) | Raw wide-scalar load–arithmetic–store kernels, allocation sites, ordinary fallbacks, and execution checks. |
| 2026-09-15 | [Typed fields and header-array updates](2026-09-15-typed-memory/README.md) | Independent storage/codecs, allocation-free local kernels, checked raw/typed header comparison, and remaining whole-path allocation sites. |
