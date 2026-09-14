#!/usr/bin/env python3
"""Reproduce the isolated assembly-lowering comparison of 2026-09-14.

Builds four historical variants together in an archived checkout. Production
sources stay unchanged; timings use the normal allocator, and allocation counts
come from a separate executable. Requires the pinned commits and cached Cargo
dependencies. See docs/ideas/assembly-lowering-evaluation.md for the boundaries.
"""

import argparse
import csv
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import subprocess
import tarfile


ROOT = Path(__file__).resolve().parents[2]
SUPPORT = Path(__file__).with_suffix("")
REVISIONS = {"original": "9c1d3000", "jobs": "8e508e36", "folder": "370195d8"}
SOURCES = [
    "lib/tests/core/fact.zy",
    "lib/tests/core/pattern-alias.zy",
    "lib/tests/core/native-frames.zy",
    "lib/tests/builtin/host-return.zy",
    "lib/tests/builtin/host-runtime.zy",
    "lib/tests/delimcc/reset-shift-k.zy",
    "lib/tests/core/gc-stress.zy",
]


class Study:
    def __init__(self, args):
        self.args = args
        self.output = args.output.resolve()
        self.output.mkdir(parents=True, exist_ok=False)
        self.checkout = self.output / "checkout"
        self.target = (args.target_dir or self.output / "target").resolve()
        self.revisions = {
            name: self.git("rev-parse", revision).strip() for name, revision in REVISIONS.items()
        }
        self.environment = os.environ.copy()
        self.environment["CARGO_TARGET_DIR"] = str(self.target)

    @staticmethod
    def git(*args):
        return subprocess.check_output(["git", *args], cwd=ROOT, text=True)

    @staticmethod
    def sha256(path):
        return hashlib.sha256(path.read_bytes()).hexdigest()

    def prepare(self):
        archive = self.output / "source.tar"
        with archive.open("wb") as stream:
            subprocess.run(["git", "archive", self.revisions["folder"]],
                           cwd=ROOT, stdout=stream, check=True)
        with tarfile.open(archive) as source:
            source.extractall(self.checkout, filter="data")
        archive.unlink()

        boundary = self.checkout / "lang/assembly/src/benchmark"
        boundary.mkdir()
        shutil.copy2(SUPPORT / "boundary.rs", boundary / "mod.rs")
        for name in ["original", "jobs"]:
            directory = boundary / name
            directory.mkdir()
            revision = self.revisions[name]
            arena = self.git("show", f"{revision}:lang/assembly/src/arena.rs")
            aliases = arena[arena.index("pub type Kont"):arena.index("/// Allocate a program")]
            (directory / "mod.rs").write_text(
                "use crate::syntax;\nmod arena {\npub(crate) use crate::syntax::*;\n"
                + aliases + "\n}\npub(super) mod lower;\n")
            lower = self.git("show", f"{revision}:lang/assembly/src/lower.rs")
            # This identical conversion impl is already supplied by the current
            # lowerer. Remove only the duplicate impl and the historical tests.
            lower, conversion = lower.split("impl From<sk::HostCallMode> for ExternMode {")
            assert conversion.count("impl ") == 0
            lower = lower.replace("#[cfg(test)]\nmod tests;\n", "")
            if name == "jobs":
                lower = lower.replace("arena::{AssemblyArena, AssemblyBuild, CxKont, Kont}",
                                      "arena::{AssemblyArena, AssemblyBuild, ContextUpdate, CxKont, Kont}")
            (directory / "lower.rs").write_text(lower)
        with (self.checkout / "lang/assembly/src/lib.rs").open("a") as stream:
            stream.write("\n#[doc(hidden)]\npub mod benchmark;\n")

        examples = self.checkout / "cli/examples"
        (examples / "assembly_bench").mkdir(parents=True)
        for source, destination in {
            "common.rs": "assembly_bench/common.rs",
            "timing.rs": "bench_assembly.rs",
            "allocations.rs": "bench_assembly_alloc.rs",
        }.items():
            shutil.copy2(SUPPORT / source, examples / destination)

        manifest = {
            "schema": 1,
            "revisions": self.revisions,
            "platform": platform.platform(),
            "machine": platform.machine(),
            "rustc": subprocess.check_output(["rustc", "-vV"], text=True),
            "profiles": self.args.profiles,
            "timing_runs_per_profile": self.args.runs,
            "timing_batches_per_run": 32,
            "allocation_rounds": 8,
            "sources": SOURCES,
            "input_hashes": {
                name: self.sha256(self.checkout / name)
                for name in [*SOURCES, "Cargo.lock", "Cargo.toml", "lib/std/builtin.zy"]
            },
            "harness_hashes": {
                path.name: self.sha256(path)
                for path in [Path(__file__), *sorted(SUPPORT.glob("*.rs"))]
            },
            "build_environment": {
                key: value for key, value in self.environment.items()
                if key in {"RUSTFLAGS", "CARGO_ENCODED_RUSTFLAGS"}
                or key.startswith("CARGO_PROFILE_")
            },
            "representation_policy": "local",
            "timing_scope": "prepared lowerer.run(), including internal scratch teardown",
            "excluded": ["source and SPSLow preparation", "representation analysis",
                         "output destruction (measured separately)", "stack analysis",
                         "native frame preparation", "code emission"],
            "oracle": "all assembly fields compared after normalizing output key-space IDs",
            "output_counts_order": ["programs", "variables", "symbols", "frame_entries"],
            "allocations": "separate System-allocator wrapper; requested heap payload bytes",
            "peak_extra_bytes": "peak live heap increment over the prepared-lowerer baseline",
        }
        (self.output / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")

    def measure(self):
        with ((self.output / "timings.csv").open("w", newline="") as timing_stream,
              (self.output / "allocations.csv").open("w", newline="") as allocation_stream):
            writers = {}
            for profile in self.args.profiles:
                command = ["cargo", "build", "--offline", "-p", "zydeco-cli",
                           "--example", "bench_assembly", "--example", "bench_assembly_alloc"]
                if profile == "release":
                    command.append("--release")
                print(f"build {profile}", flush=True)
                subprocess.run(command, cwd=self.checkout, env=self.environment, check=True)
                for kind, executable, runs, stream in [
                    ("timing", "bench_assembly", self.args.runs, timing_stream),
                    ("allocation", "bench_assembly_alloc", 1, allocation_stream),
                ]:
                    for run in range(runs):
                        print(f"measure {profile} {kind} {run + 1}/{runs}", flush=True)
                        rows = subprocess.check_output(
                            [self.target / profile / "examples" / executable,
                             *(self.checkout / source for source in SOURCES)],
                            cwd=self.checkout, env=self.environment, text=True)
                        for line in rows.splitlines():
                            row = {"profile": profile, "run": run, **json.loads(line)}
                            row["source"] = str(Path(row["source"]).relative_to(self.checkout))
                            if kind not in writers:
                                writers[kind] = csv.DictWriter(stream, fieldnames=list(row),
                                                              lineterminator="\n")
                                writers[kind].writeheader()
                            writers[kind].writerow(row)
                        stream.flush()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True, help="new output directory")
    parser.add_argument("--target-dir", type=Path, help="optional shared Cargo cache directory")
    parser.add_argument("--profiles", choices=["debug", "release"], nargs="+",
                        default=["debug", "release"])
    parser.add_argument("--runs", type=int, default=2, help="timing runs per profile (default: 2)")
    parser.add_argument("--prepare-only", action="store_true", help="prepare sources without building")
    args = parser.parse_args()
    if args.runs < 1:
        parser.error("--runs must be positive")
    study = Study(args)
    study.prepare()
    if not args.prepare_only:
        study.measure()


if __name__ == "__main__":
    main()
