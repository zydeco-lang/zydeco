#!/bin/sh

cargo build --release --bin=zydeco
PATH=target/release:$PATH

echo "Running polynomial.zydeco"
echo "Expected output: exits with code 0"
echo "Actual output:"
zydeco run lib/tests/oopsla/polynomial.zydeco
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running cc.zydeco"
echo "Expected output: exits with code 0"
echo "Actual output:"
zydeco run lib/tests/oopsla/cc.zydeco
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running cbv.zydeco"
echo "Expected output: prints true and then exits with code 0"
echo "Actual output:"
zydeco run lib/tests/oopsla/cbv.zydeco
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running monads.zydeco"
echo "Expected output: prints \"Hello, world!\" and then exits with code 0"
echo "Actual output:"
zydeco run lib/tests/oopsla/monads.zydeco
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running exn.zydeco"
echo "Expected output: prints \"2 != 1\" and then exits with code 0"
echo "Actual output:"
zydeco run lib/tests/oopsla/exn.zydeco
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running free.zydeco"
echo "Expected output: **reads** a line from stdin, prints it, and then exits with code 0"
echo "Actual output:"
zydeco run lib/tests/oopsla/free.zydeco
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running algebra.zydeco"
echo "Expected output: exits with code 0"
echo "Actual output:"
zydeco run lib/examples/algebra.zydeco
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running exnt.zydeco"
echo "Expected output: prints the original and generated code and then exits with code 0"
echo "Actual output:"
zydeco run lib/tests/oopsla/exception-transformers.zy
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running exnkt.zydeco"
echo "Expected output: prints the original and generated code and then exits with code 0"
echo "Actual output:"
zydeco run lib/tests/oopsla/exception-transformers.zy
echo "Program exited with code $?"
echo "Demo ends here."
