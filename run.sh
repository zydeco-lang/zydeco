#!/bin/sh

cargo build --release --bin=zydeco
PATH=target/release:$PATH

echo "Running polynomial.zydeco"
echo "Expected output: exits with code 0"
echo "Actual output:"
zydeco run lib/oopsla/proj.toml --bin=polynomial
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running cc.zydeco"
echo "Expected output: exits with code 0"
echo "Actual output:"
zydeco run lib/oopsla/proj.toml --bin=cc
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running cbv.zydeco"
echo "Expected output: prints true and then exits with code 0"
echo "Actual output:"
zydeco run lib/oopsla/proj.toml --bin=cbv
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running monads.zydeco"
echo "Expected output: prints \"Hello, world!\" and then exits with code 0"
echo "Actual output:"
zydeco run lib/oopsla/proj.toml --bin=monads
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running exn.zydeco"
echo "Expected output: prints \"2 != 1\" and then exits with code 0"
echo "Actual output:"
zydeco run lib/oopsla/proj.toml --bin=exn
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running free.zydeco"
echo "Expected output: **reads** a line from stdin, prints it, and then exits with code 0"
echo "Actual output:"
zydeco run lib/oopsla/proj.toml --bin=free
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running algebra.zydeco"
echo "Expected output: exits with code 0"
echo "Actual output:"
zydeco run lib/oopsla/proj.toml --bin=algebra
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running exnt.zydeco"
echo "Expected output: prints the original and generated code and then exits with code 0"
echo "Actual output:"
zydeco run lib/oopsla/proj.toml --bin=exnt
echo "Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running exnkt.zydeco"
echo "Expected output: prints the original and generated code and then exits with code 0"
echo "Actual output:"
zydeco run lib/oopsla/proj.toml --bin=exnkt
echo "Program exited with code $?"
echo "Demo ends here."
