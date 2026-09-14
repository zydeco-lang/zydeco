#!/usr/bin/env python3
"""Reproduce the isolated assembly-lowering comparison of 2026-09-14.

Builds historical variants together in an archived checkout, optionally comparing
a compatible assembly-folder candidate. Production sources stay unchanged;
timings use the normal allocator, and allocation counts come from a separate
executable. Requires the pinned commits and cached Cargo dependencies.
See docs/ideas/assembly-lowering-evaluation.md for the boundaries.
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
        if args.candidate:
            self.revisions["candidate"] = self.git("rev-parse", args.candidate).strip()
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
            revision = self.revisions.get("candidate", self.revisions["folder"])
            subprocess.run(["git", "archive", revision],
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
        if self.args.candidate:
            self.add_baseline(boundary)
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
            "timing_batches_per_run": 40 if self.args.candidate else 32,
            "variants": (["original", "jobs", "baseline", "explicit", "recursive"]
                         if self.args.candidate else ["original", "jobs", "explicit", "recursive"]),
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

    def add_baseline(self, boundary):
        """Compare the pinned folder and a compatible candidate in one binary.

        The driver and shared IR/assembly definitions come from the candidate.
        This option isolates assembly-folder changes, not changes to those APIs.
        """
        directory = boundary / "baseline"
        (directory / "lower").mkdir(parents=True)
        (directory / "mod.rs").write_text("use crate::{arena, syntax};\npub(super) mod lower;\n")
        revision = self.revisions["folder"]
        lower = self.git("show", f"{revision}:lang/assembly/src/lower.rs")
        lower = lower.split("impl From<sk::HostCallMode> for ExternMode {")[0]
        lower = lower.replace("#[cfg(test)]\nmod tests;\n", "")
        (directory / "lower.rs").write_text(lower)
        (directory / "lower/folder.rs").write_text(
            self.git("show", f"{revision}:lang/assembly/src/lower/folder.rs"))

        path = boundary / "mod.rs"
        source = path.read_text()
        replacements = {
            "mod original;": "mod original;\nmod baseline;",
            "    Jobs,": "    Jobs,\n    Baseline,",
            "pub const ALL: [Self; 4]": "pub const ALL: [Self; 5]",
            "Self::Jobs, Self::Explicit": "Self::Jobs, Self::Baseline, Self::Explicit",
            '            | Self::Jobs => "jobs",':
                '            | Self::Jobs => "jobs",\n            | Self::Baseline => "baseline",',
            "    Explicit(Lowerer<'a>),":
                "    Baseline(baseline::lower::Lowerer<'a>),\n    Explicit(Lowerer<'a>),",
            "            | Variant::Explicit | Variant::Recursive => {": """
            | Variant::Baseline => {
                let lo = baseline::lower::Lowerer::with_policy(
                    self.spans, self.scoped, self.statics, self.program, &Local);
                PreparedInner::Baseline(if native { lo.with_native_frames() } else { lo })
            }
            | Variant::Explicit | Variant::Recursive => {""",
            "            | PreparedInner::Explicit(lo)":
                "            | PreparedInner::Baseline(lo) => lo.run_with_driver::<Explicit>(),\n"
                "            | PreparedInner::Explicit(lo)",
        }
        for before, after in replacements.items():
            assert source.count(before) == 1, before
            source = source.replace(before, after)
        path.write_text(source)

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
    parser.add_argument("--candidate", help="compatible Git revision to compare with the pinned folder")
    args = parser.parse_args()
    if args.runs < 1:
        parser.error("--runs must be positive")
    study = Study(args)
    study.prepare()
    if not args.prepare_only:
        study.measure()


if __name__ == "__main__":
    main()
