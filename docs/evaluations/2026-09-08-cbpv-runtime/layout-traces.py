#!/usr/bin/env python3
"""Build and run the runtime study's standalone storage/collection traces.

Uses the current checkout's machine model and collector, not historical timing
variants. The trace sources stay outside Cargo's example and test discovery.
"""

import argparse
import json
import os
from pathlib import Path
import subprocess


STUDY = Path(__file__).resolve().parent
ROOT = STUDY.parents[2]
TRACES = ["frame_layouts", "environment_layouts", "environment_fragments"]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True,
                        help="new directory for executables and CSV output")
    parser.add_argument("--release", action="store_true", help="build optimized traces")
    parser.add_argument("--trace", action="append", choices=TRACES,
                        help="select a trace; repeat as needed (default: all three)")
    args = parser.parse_args()
    output = args.output.resolve()
    if output.exists():
        parser.error("--output must name a new directory")
    output.mkdir(parents=True)

    # Cargo owns build-script execution and library placement. Read its artifact
    # record rather than assuming a target directory or a hashed library name.
    command = ["cargo", "build", "--offline", "-p", "zydeco-machine", "--lib",
               "--message-format=json-render-diagnostics"]
    if args.release:
        command.append("--release")
    messages = subprocess.check_output(command, cwd=ROOT, text=True)
    artifacts = [json.loads(line) for line in messages.splitlines()]
    libraries = [Path(name) for artifact in artifacts
                 if artifact["reason"] == "compiler-artifact"
                 and artifact["target"]["name"] == "zydeco_machine"
                 for name in artifact["filenames"] if name.endswith(".rlib")]
    if len(libraries) != 1:
        raise RuntimeError(f"expected one machine library, got {libraries}")
    library = libraries[0]

    for name in dict.fromkeys(args.trace or TRACES):
        executable = output / (name + (".exe" if os.name == "nt" else ""))
        command = [os.environ.get("RUSTC", "rustc"), "--edition=2024", str(STUDY / f"{name}.rs"),
                   "--extern", f"zydeco_machine={library}", "-L", f"dependency={library.parent}",
                   "-o", str(executable)]
        if args.release:
            # Match the workspace's ThinLTO profile: its library may contain
            # LLVM bitcode, which rustc must consume before the native linker.
            command.extend(["-C", "opt-level=3", "-C", "lto=thin"])
        subprocess.run(command, cwd=ROOT, check=True)
        destination = output / f"{name}.csv"
        with destination.open("w") as stream:
            subprocess.run([executable], cwd=ROOT, stdout=stream, check=True)
        print(destination)


if __name__ == "__main__":
    main()
