"""Count generated AMD64 sites; executions are covered by the linked Rust regressions."""

import hashlib
import json
from pathlib import Path
import platform
import re
import subprocess


def main():
    study = Path(__file__).resolve().parent
    repository = study.parents[2]
    sources = {
        "raw_scalar": study.parent / "2026-09-15-memory-kernels/raw-int64.zy",
        "typed_field_view": repository / "lib/tests/std/typed-memory-kernel.zy",
        "raw_header_update": study / "header-raw.zy",
        "typed_header_update": study / "header-typed.zy",
        "std_header_array": repository / "lib/tests/std/header-array.zy",
        "storage_only_array": repository / "lib/tests/std/storage-arrays.zy",
    }
    results = {}
    for name, source in sources.items():
        results[name] = {}
        for policy in ["boxed", "local"]:
            assembly = subprocess.run(
                [str(repository / "target/debug/zydeco"), "build", str(source),
                 "--target", "asm", "--target-arch", "x86-64", "--representation", policy],
                cwd=repository, check=True, capture_output=True, text=True,
            ).stdout
            kernels = [section.split("raw memory kernel: end", 1)[0]
                       for section in assembly.split("raw memory kernel: begin")[1:]]
            results[name][policy] = {
                "scanned_allocation_sites": assembly.count("allocate scanned block in the copying heap"),
                "opaque_allocation_sites": assembly.count("allocate opaque block in the copying heap"),
                "kernels": len(kernels),
                "kernel_calls": sum(len(re.findall(r"^\s*call\b", kernel, re.M)) for kernel in kernels),
                "kernel_allocation_sites": sum(kernel.count("allocate ") for kernel in kernels),
            }
    inputs = [*sources.values(), study / "header-driver.zy"]
    print(json.dumps({
        "host": f"{platform.system()} {platform.machine()}",
        "rustc": subprocess.check_output(["rustc", "--version"], text=True).strip(),
        "compiler_build": "cargo build --offline --bin zydeco (dev profile)",
        "target": "AMD64; default high-SPS normalization",
        "source_sha256": {str(path.relative_to(repository)): hashlib.sha256(path.read_bytes()).hexdigest()
                          for path in inputs},
        "static_amd64_sites": results,
    }, indent=2))


if __name__ == "__main__":
    main()
