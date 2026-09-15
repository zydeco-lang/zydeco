"""Reproduce static AMD64 allocation-site counts; this is not an execution profiler."""

import hashlib
import json
from pathlib import Path
import subprocess


def main():
    study = Path(__file__).resolve().parent
    repository = study.parents[2]
    source = study / "raw-int64.zy"
    results = {}
    for policy in ["boxed", "local"]:
        assembly = subprocess.run(
            [str(repository / "target/debug/zydeco"), "build", str(source),
             "--target", "asm", "--target-arch", "x86-64", "--representation", policy],
            cwd=repository, check=True, capture_output=True, text=True,
        ).stdout
        kernels = [section.split("raw memory kernel: end", 1)[0]
                   for section in assembly.split("raw memory kernel: begin")[1:]]
        results[policy] = {
            "scanned_allocation_sites": assembly.count("allocate scanned block in the copying heap"),
            "opaque_allocation_sites": assembly.count("allocate opaque block in the copying heap"),
            "kernels": len(kernels),
            "kernel_calls": sum("call " in line for kernel in kernels for line in kernel.splitlines()),
            "kernel_allocation_sites": sum(kernel.count("allocate ") for kernel in kernels),
        }
    print(json.dumps({"source_sha256": hashlib.sha256(source.read_bytes()).hexdigest(),
                      "static_amd64_sites": results}, indent=2))


if __name__ == "__main__":
    main()
