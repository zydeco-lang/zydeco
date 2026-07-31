#!/bin/sh

cargo build --release --bin=zydeco
PATH=target/release:$PATH

echo "Running polynomial.zydeco"
echo "Expected output: exits with code 0"
echo "Actual output:"
zydeco run lib/tests/source/oopsla-polynomial.zy
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running cc.zydeco"
echo "Expected output: exits with code 0"
echo "Actual output:"
zydeco run lib/tests/source/oopsla-calling-conventions.zy
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running cbv.zydeco"
echo "Expected output: prints true and then exits with code 0"
echo "Actual output:"
zydeco run lib/tests/source/oopsla-cbv.zy
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running monads.zydeco"
echo "Expected output: prints \"Hello, world!\" and then exits with code 0"
echo "Actual output:"
zydeco run lib/tests/source/oopsla-monads.zy
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running exn.zydeco"
echo "Expected output: prints \"2 != 1\" and then exits with code 0"
echo "Actual output:"
zydeco run lib/tests/source/oopsla-exn.zy
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running free.zydeco"
echo "Expected output: **reads** a line from stdin, prints it, and then exits with code 0"
echo "Actual output:"
zydeco run lib/tests/source/oopsla-free.zy
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
zydeco run lib/tests/source/oopsla-exception-transformers.zy
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running exnkt.zydeco"
echo "Expected output: prints the original and generated code and then exits with code 0"
echo "Actual output:"
zydeco run lib/tests/source/oopsla-exception-transformers.zy
echo "Program exited with code $?"
echo "Demo ends here."
