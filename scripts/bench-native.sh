#!/usr/bin/env bash
# Measure native code quality and runtime for representative Zydeco programs.
#
# Usage: scripts/bench-native.sh [runs] [file...]
#
# For each file, this script:
#   1. builds AMD64 assembly and reports the number of `call zydeco_gc_alloc`
#      sites;
#   2. builds a native executable and reports the best wall time over `runs`
#      executions.
#
# To compare against the previous implementation, run this script from two git
# worktrees at the relevant commits and diff the output.
#
# Set ZYDECO_DISABLE_UNBOXING=1 to build the same source without the unboxing
# optimization, providing an in-tree A/B baseline.
set -euo pipefail

cd "$(dirname "$0")/.."

runs="${1:-5}"
shift || true

if [[ $# -eq 0 ]]; then
    files=(
        lib/tests/compile/direct-tuple.zy
        lib/tests/compile/direct-closure.zy
        lib/tests/compile/tuple.zy
        lib/tests/compile/triple.zy
        lib/tests/compile/gc-stress.zy
        lib/tests/compile/fact.zy
    )
else
    files=("$@")
fi

bin="target/release/zydeco"
cargo build --release --bin zydeco

for file in "${files[@]}"; do
    if [[ ! -f "$file" ]]; then
        echo "skipping missing file: $file" >&2
        continue
    fi

    build_dir="$(mktemp -d)"
    trap 'rm -rf "$build_dir"' EXIT

    asm_file="$build_dir/out.s"
    if ! "$bin" build -t asm --target-arch x86-64 "$file" -b "$build_dir" >"$asm_file" 2>/dev/null; then
        echo "assembly build failed: $file" >&2
        continue
    fi
    alloc_sites="$(grep -c 'call zydeco_gc_alloc' "$asm_file" || true)"

    if ! "$bin" build -t exe --target-arch x86-64 "$file" -b "$build_dir" >/dev/null 2>&1; then
        echo "executable build failed: $file" >&2
        continue
    fi

    exe="$(find "$build_dir" -maxdepth 1 -name '*.exe' -print -quit)"
    if [[ -z "$exe" ]]; then
        echo "no executable produced for $file" >&2
        continue
    fi

    best=""
    for _ in $(seq 1 "$runs"); do
        start="$(python3 -c 'import time; print(time.perf_counter())')"
        "$exe" >/dev/null 2>&1 || {
            echo "run failed: $file" >&2
            continue 2
        }
        end="$(python3 -c 'import time; print(time.perf_counter())')"
        elapsed="$(python3 - "$start" "$end" <<'INNER'
import sys
print(f"{float(sys.argv[2]) - float(sys.argv[1]):.6f}")
INNER
)"
        if [[ -z "$best" ]] || python3 -c "exit(0 if $elapsed < $best else 1)"; then
            best="$elapsed"
        fi
    done

    printf '%-40s gc_alloc=%-4s best_of_%s=%ss\n' "$file" "$alloc_sites" "$runs" "$best"
done
