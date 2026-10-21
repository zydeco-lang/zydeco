#!/usr/bin/env bash
# Time `zydeco check` on the standard library and representative test files.
#
# Usage: scripts/bench-check.sh [runs]
#
# Rebuilds the release CLI from this checkout, then prints the best wall time
# of `runs` runs per file. Used to track `zydeco check` latency.
set -euo pipefail

cd "$(dirname "$0")/.."

runs="${1:-3}"
bin="target/release/zydeco"
cargo build --release --quiet --bin zydeco

files=(
    lib/std/std.zy
    lib/tests/oopsla/migrated/core.zy
    lib/tests/monadic/shadow.zy
    lib/tests/delimcc/try-catch.zy
)

for file in "${files[@]}"; do
    best=""
    for _ in $(seq 1 "$runs"); do
        start="$(python3 -c 'import time; print(time.perf_counter())')"
        "$bin" check "$file" >/dev/null 2>&1 || { echo "check failed: $file"; exit 1; }
        end="$(python3 -c 'import time; print(time.perf_counter())')"
        elapsed="$(python3 - "$start" "$end" <<'INNER'
import sys
print(f"{float(sys.argv[2]) - float(sys.argv[1]):.3f}")
INNER
)"
        if [[ -z "$best" ]] || python3 -c "exit(0 if $elapsed < $best else 1)"; then
            best="$elapsed"
        fi
    done
    printf '%-40s best of %s: %ss\n' "$file" "$runs" "$best"
done
