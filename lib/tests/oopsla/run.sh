#!/bin/sh

# Run the maintained OOPSLA artifact examples from the repository root.
# Executable terms are executed with `zydeco run`; library terms are checked
# with `zydeco check`. Invoke this script as `./lib/tests/oopsla/run.sh`.

root=$(CDPATH= cd "$(dirname "$0")/../../.." && pwd)
cd "$root" || exit 1

cargo build --release --bin=zydeco || exit 1
zydeco=target/release/zydeco

# demo <run|check> <source> <expected output>
demo() {
  mode=$1
  file=$2
  expected=$3
  echo "Running ${file##*/}"
  echo "Expected output: $expected"
  echo "Actual output:"
  "$zydeco" "$mode" "$file"
  echo "$mode exited with code $?"
  echo "Press Enter to continue"
  read
}

demo run lib/tests/oopsla/polynomial.zydeco "exits with code 0"
demo run lib/tests/oopsla/cc.zydeco "exits with code 0"
demo run lib/tests/oopsla/cbv.zydeco "prints \"true\" and then exits with code 0"
demo run lib/tests/oopsla/monads.zydeco "prints \"Hello, world!\" and then exits with code 0"
demo run lib/tests/oopsla/exn.zydeco "prints \"2 != 1\" and then exits with code 0"
demo run lib/tests/oopsla/free.zydeco "reads a line from stdin, prints it, and then exits with code 0"
demo check lib/tests/oopsla/algebra.zydeco "checks successfully and then exits with code 0"
demo check lib/tests/oopsla/exnt.zydeco "checks successfully and then exits with code 0"
demo check lib/tests/oopsla/exnkt.zydeco "checks successfully and then exits with code 0"
echo "Demo ends here."
