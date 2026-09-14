# Evaluations

Experiments inform implementation choices under particular workloads, revisions, and toolchains.
This directory retains dated reports, measurements, and reproduction artifacts so
that their methods, findings, and limits can be assessed together.
Results describe the measured revisions; they do not establish a current performance ranking.

Each study lives in a `YYYY-MM-DD-topic/` directory, with its report in `README.md` beside its evidence files.
Keep original measurements and manifests intact, including recorded commands, paths, and source hashes.
The [references](../references/README.md) own current contracts and settled design rationale;
concrete unfinished designs belong in [proposals](../proposals/README.md)
and open conceptual questions in [ideas](../ideas/README.md).
Temporary investigation notes remain in [logs](../logs).

| Date | Study | Scope |
| --- | --- | --- |
| 2026-09-08 | [CBPV runtime evaluation](2026-09-08-cbpv-runtime/README.md) | Environment storage, capture copying, root discovery, native runtime comparisons, and Wasm memory. |
| 2026-09-14 | [Isolated assembly lowering evaluation](2026-09-14-assembly-lowering/README.md) | Boxed CPS and folder comparisons, isolated pass timing and allocation costs, consumer-slot reuse, reduced dispatch, and rejected optimization experiments. |
