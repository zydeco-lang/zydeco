#!/usr/bin/env python3
"""Compare explicit compiler/runtime pairs for the environment studies.

Reuses the first round's workload generation, exact oracles, build isolation, and
rotating sample order. See docs/evaluations/2026-09-08-cbpv-runtime/README.md for interpretation.
"""

import argparse
import importlib.util
import json
from pathlib import Path
import re
import shutil


SPEC = importlib.util.spec_from_file_location("runtime_study", Path(__file__).with_name("runtime-study.py"))
RUNTIME = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(RUNTIME)


class EnvironmentStudy(RUNTIME.Study):
    def inspect(self):
        super().inspect()
        self.metadata["input_hashes"]["environment-study.py"] = self.sha256(Path(__file__))
        if self.args.fragments:
            self.metadata["input_hashes"]["environment-probe.rs"] = self.sha256(
                Path(__file__).with_name("environment-probe.rs"))
        self.save()

    def variants(self):
        return {name: (Path(compiler).resolve(), Path(runtime).resolve())
                for name, compiler, runtime in self.args.variant}

    def workloads(self):
        workloads = super().workloads()
        width = 128
        steps = "\n".join("    do value <- ! int/add value 1;" for _ in range(width))
        iterations = max(1, round(1000 * self.args.scale))
        body = f"""
  def ! chain (value : Int) : Ret Int =
{steps}
    ret value
  in
  def fix loop (remaining : Int) (acc : Int) : Ret Int =
    ! int/eq (Ret Int) remaining 0 {{ ret acc }} {{
      do value <- ! chain 0;
      do acc <- ! int/add acc value;
      do next <- ! int/sub remaining 1;
      ! loop next acc
    }}
  in
  do result <- ! loop {iterations} 0;
  do status <- ! int/sub result {iterations * width};
  ! process/exit status"""
        workloads["sequential-locals"] = self.source("sequential-locals", body)
        if self.args.fragments:
            workloads["sparse-repeated-64"] = self.source(
                "sparse-repeated-64", self.sparse_repeated(64, max(1, round(2000 * self.args.scale))))
            workloads["sparse-deep-64"] = self.source(
                "sparse-deep-64", self.sparse_nested(64, 128, max(1, round(10000 * self.args.scale))))
        return workloads

    def sparse_repeated(self, width, iterations):
        # Reduce the wide live set before calling churn. The wide frame layout
        # still exists, but the continuation needs only the completed subtotal.
        uses = "\n".join(f"    do result <- ! int/add result x{i};" for i in range(width))
        original = f"    do result <- ! churn 3 seed;\n{uses}"
        replacement = (f"    let result = 0 in\n{uses}\n"
                       "    do returned <- ! churn 3 seed;\n"
                       "    do result <- ! int/add result returned;")
        body = self.repeated(width, iterations)
        assert body.count(original) == 1
        return body.replace(original, replacement)

    def sparse_nested(self, width, depth, iterations):
        bindings = "\n".join(f"      do x{i} <- ! int/add seed {i};" for i in range(width))
        uses = "\n".join(f"      do subtotal <- ! int/add subtotal x{i};" for i in range(width))
        churn = self.churn(iterations).split("  do result <- ! churn")[0]
        expected = depth * width * (width + 1) // 2 + iterations + 1
        return churn + f"""
  def fix nest (remaining : Int) (seed : Int) : Ret Int =
    ! int/eq (Ret Int) remaining 0 {{ ! churn {iterations} seed }} {{
{bindings}
      let subtotal = 0 in
{uses}
      do next <- ! int/sub remaining 1;
      do returned <- ! nest next seed;
      ! int/add returned subtotal
    }}
  in
  do result <- ! nest {depth} 1;
  do status <- ! int/sub result {expected};
  ! process/exit status"""

    def layouts(self):
        # The shared Action declaration emits kind, layout ID, word count. Read
        # only Enter descriptors (kind 0); Resume records reserve no new layout.
        pattern = re.compile(r"frame_entry_[\w]+:\s+dq 0\s+dq (\d+)\s+dq (\d+)")
        for record in list(self.records):
            if record["kind"] != "build" or not record["success"]:
                continue
            profile, variant, workload = (record[key] for key in ["profile", "variant", "workload"])
            path = self.output / "native" / profile / variant / f"{workload}.s"
            layouts = {int(layout): int(words) for layout, words in pattern.findall(path.read_text())}
            if not layouts:
                raise ValueError(f"no frame entry descriptors found in {path}")
            self.record("layouts", profile=profile, variant=variant, workload=workload,
                        words_by_layout=layouts, total_static_words=sum(layouts.values()),
                        maximum_frame_words=max(layouts.values()))

    def capacity(self):
        source = self.source("deep-frame-growth", self.nested(7000, 16, 50))
        for variant, (compiler, runtime) in self.variants().items():
            directory = self.output / variant
            result = self.run([compiler, "build", source, "--target", "exe", "--target-arch", "x86-64",
                               "--build-dir", directory, "--runtime-dir", runtime],
                              timeout=self.args.build_timeout, env=self.environment | RUNTIME.OPTIMIZED_RUNTIME)
            success = result["exit"] == 0
            self.record("capacity-build", variant=variant, success=success, **result)
            if not success:
                continue
            result = self.run([directory / "deep-frame-growth.exe"], timeout=self.args.timeout)
            growing = variant in self.args.growable_variant
            success = self.valid(result) if growing else (
                result["exit"] == 1 and result["stdout"] == ""
                and "environment stack overflow:" in result["stderr"])
            self.record("capacity", variant=variant, expected="success" if growing else "capacity-rejected",
                        success=success, **result)

    def probes(self):
        # Use separate executables and runtime copies after all timing. This
        # instrumentation is deliberately absent from production and timed code.
        for profile in self.args.profiles:
            env = self.environment | (RUNTIME.OPTIMIZED_RUNTIME if profile == "optimized" else {})
            for variant, (compiler, runtime) in self.variants().items():
                original = self.output / "native" / profile / variant
                model = (original / "machine/src/frames.rs").read_text()
                reserved = "frames.reserved_words()" if "fn reserved_words" in model else "131072usize"
                probe_runtime = self.output / "probe-runtimes" / profile / variant
                probe_runtime.mkdir(parents=True, exist_ok=True)
                for path in runtime.iterdir():
                    if path.is_file() and (path.suffix == ".rs" or path.name == "Cargo.toml"):
                        shutil.copy2(path, probe_runtime / path.name)
                stub = probe_runtime / "stub.rs"
                source = stub.read_text()
                marker = 'extern "sysv64" fn zydeco_exit(code: Word) -> ! {'
                if source.count(marker) != 1:
                    raise ValueError(f"cannot instrument exit in {stub}")
                source = source.replace(marker, marker + '\n    let frames = unsafe { &*FRAMES.get() };\n'
                                        '    eprintln!("zydeco_environment: {} {} {}", frames.used_words(), '
                                        f'frames.high_water_words(), {reserved});')
                if self.args.fragments:
                    metadata = "Some(frames.metadata_reserved_bytes())" if "fn metadata_reserved_bytes" in model else "None"
                    source = self.instrument_transitions(source, metadata)
                stub.write_text(source)
                directory = self.output / "probes" / profile / variant
                for record in list(self.records):
                    if record["kind"] != "build" or not record["success"] or record["profile"] != profile or record["variant"] != variant:
                        continue
                    name = record["workload"]
                    print(f"probe {profile}/{variant}/{name}", flush=True)
                    result = self.run([compiler, "build", self.output / "sources" / f"{name}.zy",
                                       "--target", "exe", "--target-arch", "x86-64",
                                       "--build-dir", directory, "--runtime-dir", probe_runtime],
                                      timeout=self.args.build_timeout, env=env)
                    success = result["exit"] == 0
                    self.record("probe-build", profile=profile, variant=variant, workload=name,
                                success=success, runtime_sha256=self.sha256(stub), **result)
                    if not success:
                        continue
                    result = self.run([directory / f"{name}.exe"], timeout=self.args.timeout)
                    lines = result["stderr"].splitlines()
                    match = re.fullmatch(r"zydeco_environment: (\d+) (\d+) (\d+)", lines[0]) if lines else None
                    transitions = None
                    if self.args.fragments and len(lines) == 2 and lines[1].startswith("zydeco_transitions: "):
                        transitions = json.loads(lines[1].removeprefix("zydeco_transitions: "))
                    success = (result["exit"] == 0 and result["stdout"] == "" and match is not None
                               and (transitions is not None if self.args.fragments else len(lines) == 1))
                    metrics = dict(zip(["final_words", "high_water_words", "reserved_words"],
                                       map(int, match.groups()))) if match else None
                    self.record("environment", profile=profile, variant=variant, workload=name,
                                success=success, metrics=metrics, transitions=transitions, **result)

    @staticmethod
    def instrument_transitions(source, metadata):
        hooks = {
            # Exit prints the word-buffer metrics first, then the independent
            # transition probe. It observes actions without knowing storage layout.
            'extern "sysv64" fn zydeco_frame_step(action: &\'static Action<Word>, token: Word) -> Word {':
                '    unsafe { &mut *ENVIRONMENT_PROBE.get() }.step(action);',
            '        slots.extend(unsafe { &*HOST_ROOTS.get() }.slots.iter().copied());':
                '        unsafe { &mut *ENVIRONMENT_PROBE.get() }.roots(slots.len());',
        }
        for marker, insertion in hooks.items():
            if source.count(marker) != 1:
                raise ValueError(f"cannot instrument runtime marker: {marker}")
            source = source.replace(marker, marker + "\n" + insertion)
        marker = "frames.high_water_words(), "
        start = source.index(marker)
        end = source.index(";", start) + 1
        source = source[:end] + f'\n    unsafe {{ &*ENVIRONMENT_PROBE.get() }}.report({metadata});' + source[end:]
        return source + "\n" + Path(__file__).with_name("environment-probe.rs").read_text()

    @classmethod
    def main(cls):
        parser = argparse.ArgumentParser(description=__doc__)
        parser.add_argument("--variant", nargs=3, action="append", required=True,
                            metavar=("NAME", "COMPILER", "RUNTIME"))
        parser.add_argument("--output", type=Path, required=True)
        parser.add_argument("--profiles", nargs="+", choices=["debug", "optimized"], default=["optimized"])
        parser.add_argument("--samples", type=int, default=5)
        parser.add_argument("--scale", type=float, default=10)
        parser.add_argument("--fragments", action="store_true",
                            help="include sparse wide-frame workloads and separate transition probes")
        parser.add_argument("--timeout", type=float, default=30)
        parser.add_argument("--build-timeout", type=float, default=300)
        parser.add_argument("--capacity-only", action="store_true", help="run the deep environment boundary case")
        parser.add_argument("--growable-variant", action="append", default=[],
                            help="variant expected to pass the capacity case; repeat as needed")
        args = parser.parse_args()
        args.workloads = None
        names = [name for name, _, _ in args.variant]
        if len(set(names)) != len(names) or any(not re.fullmatch(r"[a-z0-9-]+", name) for name in names):
            parser.error("variant names must be unique lowercase words separated by hyphens")
        if min(args.samples, args.scale, args.timeout, args.build_timeout) <= 0:
            parser.error("sample count, scale, and timeouts must be positive")
        study = cls(args)
        study.inspect()
        if args.capacity_only:
            if not args.growable_variant or set(args.growable_variant) - set(names):
                parser.error("capacity comparisons require valid --growable-variant names")
            study.capacity()
        else:
            study.native(study.workloads())
            study.layouts()
            study.probes()
        failed = [record for record in study.records if record.get("success") is False]
        print(json.dumps({"results": str(study.output / "results.json"), "failed_checks": len(failed)}))
        return bool(failed)


if __name__ == "__main__":
    raise SystemExit(EnvironmentStudy.main())
