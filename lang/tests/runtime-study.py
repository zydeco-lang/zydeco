#!/usr/bin/env python3
"""Bounded runtime experiments; see docs/ideas/cbpv-runtime-evaluation.md.

Builds are outside timing, outputs have exact oracles, and every attempted run is
recorded. Historical compilers and matching runtime directories are explicit
inputs; this script never checks out or changes repository revisions.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import statistics
import subprocess
import time


ROOT = Path(__file__).resolve().parents[2]
OPTIMIZED_RUNTIME = {
    "CARGO_PROFILE_DEV_OPT_LEVEL": "3",
    "CARGO_PROFILE_DEV_DEBUG": "0",
    "CARGO_PROFILE_DEV_DEBUG_ASSERTIONS": "false",
    "CARGO_PROFILE_DEV_OVERFLOW_CHECKS": "false",
    "CARGO_PROFILE_DEV_LTO": "off",
}


class Study:
    def __init__(self, args):
        self.args = args
        self.output = args.output.resolve()
        self.output.mkdir(parents=True, exist_ok=True)
        self.records = []
        self.environment = os.environ.copy()
        # The CLI expects its runtime executable under build-dir/target. Neither
        # an inherited target directory nor profile settings may cross variants.
        self.environment.pop("CARGO_TARGET_DIR", None)
        for key in list(self.environment):
            if key.startswith("CARGO_PROFILE_DEV_"):
                del self.environment[key]
        self.metadata = {
            "schema": 1,
            "platform": platform.platform(),
            "host_machine": platform.machine(),
            "native_target": "x86_64",
            "translation_expected": platform.system() == "Darwin"
            and platform.machine() == "arm64",
            "optimized_runtime_environment": OPTIMIZED_RUNTIME,
            "options": {key: str(value) if isinstance(value, Path) else value
                        for key, value in vars(args).items()},
            "tools": {},
            "input_hashes": {},
        }

    @staticmethod
    def sha256(path):
        return hashlib.sha256(path.read_bytes()).hexdigest()

    def save(self):
        contents = json.dumps({"metadata": self.metadata, "records": self.records}, indent=2)
        (self.output / "results.json").write_text(contents.replace(str(ROOT), "${REPO}") + "\n")

    def run(self, command, *, timeout, env=None):
        started = time.perf_counter_ns()
        try:
            result = subprocess.run(
                list(map(str, command)), cwd=ROOT, env=env or self.environment,
                input=b"", capture_output=True, timeout=timeout,
            )
            return {
                "command": list(map(str, command)),
                "seconds": (time.perf_counter_ns() - started) / 1e9,
                "exit": result.returncode,
                "stdout": result.stdout.decode(errors="replace"),
                "stderr": result.stderr.decode(errors="replace"),
                "timed_out": False,
            }
        except subprocess.TimeoutExpired as error:
            return {
                "command": list(map(str, command)),
                "seconds": (time.perf_counter_ns() - started) / 1e9,
                "exit": None,
                "stdout": (error.stdout or b"").decode(errors="replace"),
                "stderr": (error.stderr or b"").decode(errors="replace"),
                "timed_out": True,
            }

    def record(self, kind, **fields):
        self.records.append({"kind": kind, **fields})
        self.save()

    def inspect(self):
        for name, command in {
            "rustc": ["rustc", "--version", "--verbose"],
            "cargo": ["cargo", "--version"],
            "node": ["node", "--version"],
            "nasm": ["nasm", "--version"],
            "revision": ["git", "rev-parse", "HEAD"],
            "working_tree": ["git", "status", "--short"],
        }.items():
            self.metadata["tools"][name] = self.run(command, timeout=10)
        if platform.system() == "Darwin":
            self.metadata["tools"]["cpu"] = self.run(
                ["sysctl", "-n", "machdep.cpu.brand_string"], timeout=10
            )
            hardware = self.run(["system_profiler", "SPHardwareDataType", "-json"], timeout=15)
            if hardware["exit"] == 0:
                # Retain hardware characteristics, not device serials or UUIDs.
                self.metadata["hardware"] = [
                    {key: value for key, value in item.items() if key in {
                        "chip_type", "cpu_type", "number_processors", "physical_memory", "machine_model"
                    }}
                    for item in json.loads(hardware["stdout"]).get("SPHardwareDataType", [])
                ]
        for path in dict.fromkeys(compiler for compiler, _ in self.variants().values()):
            self.metadata["input_hashes"][str(path)] = self.sha256(path)
        for directory in dict.fromkeys([runtime for _, runtime in self.variants().values()]
                                       + [ROOT / "lang/machine/src"]):
            for path in sorted(directory.rglob("*.rs")) + [directory / "Cargo.toml"]:
                if not path.exists():
                    continue
                if "target" not in path.relative_to(directory).parts:
                    self.metadata["input_hashes"][str(path)] = self.sha256(path)
        self.metadata["input_hashes"]["builtin.zy"] = self.sha256(ROOT / "lib/std/builtin.zy")
        self.metadata["input_hashes"]["runtime-study.py"] = self.sha256(Path(__file__))
        self.metadata["input_hashes"]["wasm-host.mjs"] = self.sha256(ROOT / "lang/tests/wasm-host.mjs")
        self.save()

    def source(self, name, body):
        path = self.output / "sources" / f"{name}.zy"
        path.parent.mkdir(parents=True, exist_ok=True)
        builtin = os.path.relpath(ROOT / "lib/std/builtin.zy", path.parent)
        # POSIX exit status keeps only eight bits. Compare the complete result
        # before exiting so an error of 256 cannot satisfy the output oracle.
        body = body.replace(
            "! process/exit status",
            "do status <- ! int64/eq (Ret Int64) status 0 { ret 0 } { ret 1 };\n"
            "  ! process/exit status",
        )
        path.write_text(
            "begin\n  param (/VType; /CType; /Ret; /Thk; /Int64; /numeric; /process; /system)"
            f" : @(import({json.dumps(builtin)})) in\n"
            "  let int64 = numeric/int64 in\n" + body + "\nend\n"
        )
        self.record("source", workload=name, path=str(path), sha256=self.sha256(path))
        return path

    @staticmethod
    def churn(iterations):
        return f"""
  def fix churn (remaining : Int64) (acc : Int64) : Ret Int64 =
    ! int64/eq (Ret Int64) remaining 0 {{ ret acc }} {{
      do next <- ! int64/sub remaining 1;
      do acc <- ! int64/add acc 1;
      ! churn next acc
    }}
  in
  do result <- ! churn {iterations} 0;
  do status <- ! int64/sub result {iterations};
  ! process/exit status"""

    def nested(self, depth, width, iterations):
        bindings = "\n".join(f"    do x{i} <- ! int64/add seed {i};" for i in range(width))
        uses = "\n".join(f"      do result <- ! int64/add result x{i};" for i in range(width))
        churn_definition = self.churn(iterations).split("  do result <- ! churn")[0]
        expected = iterations + depth * sum(5 + i for i in range(width))
        return churn_definition + f"""
  def fix descend (depth : Int64) (seed : Int64) : Ret Int64 =
{bindings}
    ! int64/eq (Ret Int64) depth 0 {{ ! churn {iterations} 0 }} {{
      do next <- ! int64/sub depth 1;
      do result <- ! descend next seed;
{uses}
      ret result
    }}
  in
  do result <- ! descend {depth} 5;
  do status <- ! int64/sub result {expected};
  ! process/exit status"""

    def workloads(self):
        scale = lambda count: max(1, round(count * self.args.scale))
        saved = (ROOT / "lib/tests/core/native-frames.zy").read_text()
        # Reuse the regression body with its own builtin declaration removed.
        saved = saved[saved.index("  -- The returned"):saved.rindex("end")]
        saved = saved.replace("100000", str(scale(100000)))
        protocols = f"""
  let Done = codata | .done : Ret Int64 end in
  let Last = codata | .more : Int64 -> Done end in
  let Chain = codata | .more : Int64 -> Last end in
  def ! consume (sum : Int64) : Chain =
    comatch
    | .more first =>
      do sum <- ! int64/add sum first;
      comatch
      | .more second =>
        do sum <- ! int64/add sum second;
        comatch | .done => ret sum end
      end
    end
  in
  def fix loop (remaining : Int64) (acc : Int64) : Ret Int64 =
    ! int64/eq (Ret Int64) remaining 0 {{ ret acc }} {{
      do result <- ! consume 0 .more remaining .more 1 .done;
      do one <- ! int64/sub result remaining;
      do acc <- ! int64/add acc one;
      do next <- ! int64/sub remaining 1;
      ! loop next acc
    }}
  in
  do result <- ! loop {scale(20000)} 0;
  do status <- ! int64/sub result {scale(20000)};
  ! process/exit status"""
        bodies = {
            "retained-closure": saved,
            "zero-captures": self.nested(128, 0, scale(30000)),
            "shallow-captures": self.nested(8, 4, scale(40000)),
            "deep-captures": self.nested(128, 16, scale(10000)),
            "mixed-protocol": protocols,
            "repeated-16": self.repeated(16, scale(5000)),
            "repeated-64": self.repeated(64, scale(2000)),
        }
        if self.args.workloads and set(self.args.workloads) - bodies.keys():
            raise ValueError(f"unknown workloads: {set(self.args.workloads) - bodies.keys()}")
        return {name: self.source(name, body) for name, body in bodies.items()
                if not self.args.workloads or name in self.args.workloads}

    def repeated(self, width, iterations):
        bindings = "\n".join(f"    do x{i} <- ! int64/add seed {i};" for i in range(width))
        uses = "\n".join(f"    do result <- ! int64/add result x{i};" for i in range(width))
        churn_definition = self.churn(3).split("  do result <- ! churn")[0]
        expected = ((width + 1) * iterations * (iterations + 1) // 2
                    + iterations * (width * (width - 1) // 2 + 3))
        return churn_definition + f"""
  def ! keep (seed : Int64) : Ret Int64 =
{bindings}
    do result <- ! churn 3 seed;
{uses}
    ret result
  in
  def fix loop (remaining : Int64) (acc : Int64) : Ret Int64 =
    ! int64/eq (Ret Int64) remaining 0 {{ ret acc }} {{
      do value <- ! keep remaining;
      do acc <- ! int64/add acc value;
      do next <- ! int64/sub remaining 1;
      ! loop next acc
    }}
  in
  do result <- ! loop {iterations} 0;
  do status <- ! int64/sub result {expected};
  ! process/exit status"""

    @staticmethod
    def valid(result):
        return result["exit"] == 0 and result["stdout"] == "" and result["stderr"] == ""

    def variants(self):
        return {
            "captures": (self.args.capture_compiler, self.args.capture_runtime),
            "frames-eager": (self.args.frame_compiler, self.args.eager_runtime),
            "frames-deferred": (self.args.frame_compiler, self.args.deferred_runtime),
        }

    def native(self, workloads):
        variants = self.variants()
        ready = {}
        # Finish every build before timing any program.
        for profile in self.args.profiles:
            env = self.environment | (OPTIMIZED_RUNTIME if profile == "optimized" else {})
            for variant, (compiler, runtime) in variants.items():
                directory = self.output / "native" / profile / variant
                for name, source in workloads.items():
                    print(f"build {profile}/{variant}/{name}", flush=True)
                    result = self.run([
                        compiler, "build", source, "--target", "exe", "--target-arch", "x86-64",
                        "--build-dir", directory, "--runtime-dir", runtime,
                    ], timeout=self.args.build_timeout, env=env)
                    executable = directory / f"{name}.exe"
                    assembly = directory / f"{name}.s"
                    success = result["exit"] == 0 and executable.exists()
                    self.record("build", profile=profile, variant=variant, workload=name,
                                success=success, assembly_sha256=self.sha256(assembly)
                                if assembly.exists() else None,
                                assembly_bytes=assembly.stat().st_size if assembly.exists() else None,
                                executable_bytes=executable.stat().st_size if success else None,
                                **result)
                    if success:
                        ready[(profile, variant, name)] = executable
        runnable = {}
        for key, executable in ready.items():
            result = self.run([executable], timeout=self.args.timeout)
            success = self.valid(result)
            self.record("warmup", profile=key[0], variant=key[1], workload=key[2],
                        success=success, **result)
            print(f"warmup {'/'.join(key)}: {'ok' if success else 'FAILED'} "
                  f"{result['seconds']:.4f}s", flush=True)
            if success:
                runnable[key] = executable
        keys = list(runnable)
        samples = {key: [] for key in keys}
        groups = list(dict.fromkeys((profile, name) for profile, _, name in keys))
        for sample in range(self.args.samples):
            group_order = groups if sample % 2 == 0 else list(reversed(groups))
            order = []
            for profile, name in group_order:
                candidates = [key for key in keys if key[0] == profile and key[2] == name]
                offset = sample % len(candidates)
                order.extend(candidates[offset:] + candidates[:offset])
            for key in order:
                result = self.run([runnable[key]], timeout=self.args.timeout)
                success = self.valid(result)
                self.record("sample", profile=key[0], variant=key[1], workload=key[2],
                            sample=sample, success=success, **result)
                if success:
                    samples[key].append(result["seconds"])
        for key, values in samples.items():
            if values:
                summary = dict(profile=key[0], variant=key[1], workload=key[2],
                               successful_samples=len(values), requested_samples=self.args.samples,
                               median_seconds=statistics.median(values),
                               min_seconds=min(values), max_seconds=max(values))
                self.record("summary", **summary)
                print(json.dumps(summary), flush=True)

    def wasm(self):
        for iterations, adjustment in [(100, 0), (10000, 0), (100000, 0), (100, 256)]:
            suffix = "-reject-256" if adjustment else ""
            body = self.churn(iterations).replace(
                f"sub result {iterations}", f"sub result {iterations + adjustment}"
            )
            source = self.source(f"wasm-churn-{iterations}{suffix}", body)
            expected_exit = int(adjustment != 0)
            for backend in ["wasm-am", "wasm-sps"]:
                directory = self.output / backend
                result = self.run([
                    self.args.frame_compiler, "build", source, "--target", backend,
                    "--build-dir", directory,
                ], timeout=self.args.build_timeout)
                module = directory / f"{source.stem}.{backend.removeprefix('wasm-')}.wasm"
                success = result["exit"] == 0 and module.exists()
                self.record("wasm-build", backend=backend, iterations=iterations,
                            expected_exit=expected_exit, success=success, **result)
                if success:
                    result = self.run([
                        "node", ROOT / "lang/tests/wasm-host.mjs", module,
                    ], timeout=self.args.timeout,
                        env=self.environment | {"ZYDECO_WASM_MEMORY_REPORT": "1"})
                    try:
                        memory = json.loads(result["stderr"])["zydeco_wasm_memory"]
                    except (ValueError, KeyError):
                        memory = None
                    success = (result["exit"] == expected_exit and result["stdout"] == ""
                               and memory is not None)
                    self.record("wasm-oracle" if adjustment else "wasm-memory",
                                backend=backend, iterations=iterations, expected_exit=expected_exit,
                                success=success, module_bytes=module.stat().st_size,
                                memory=memory, **result)
                    print(f"{backend} {iterations}: {memory}", flush=True)

    @classmethod
    def main(cls):
        parser = argparse.ArgumentParser(description=__doc__)
        for name in ["capture-compiler", "capture-runtime", "frame-compiler",
                     "eager-runtime", "deferred-runtime", "output"]:
            parser.add_argument(f"--{name}", type=lambda path: Path(path).resolve(), required=True)
        parser.add_argument("--profiles", nargs="+", choices=["debug", "optimized"],
                            default=["optimized"])
        parser.add_argument("--samples", type=int, default=7)
        parser.add_argument("--scale", type=float, default=1)
        parser.add_argument("--timeout", type=float, default=30)
        parser.add_argument("--build-timeout", type=float, default=300)
        parser.add_argument("--wasm-only", action="store_true")
        parser.add_argument("--workloads", nargs="+", help="optional subset of native workload names")
        args = parser.parse_args()
        if args.samples < 1 or args.scale <= 0 or args.timeout <= 0 or args.build_timeout <= 0:
            parser.error("sample count, scale, and timeouts must be positive")
        study = cls(args)
        study.inspect()
        if not args.wasm_only:
            study.native(study.workloads())
        study.wasm()
        failed = [record for record in study.records if record.get("success") is False]
        print(f"Results: {study.output / 'results.json'}; {len(failed)} failed checks", flush=True)
        return bool(failed)


if __name__ == "__main__":
    raise SystemExit(Study.main())
