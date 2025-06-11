cargo build --release --bin=zydeco
alias zydeco="target/release/zydeco"

echo "Running polynomial.zydeco"
echo "Expected output: Program exited with code 0"
zydeco run lib/oopsla/proj.toml --bin=polynomial
echo "Actual output: Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running sum_and_mult.zydeco"
echo "Expected output: Program exited with code 0"
zydeco run lib/oopsla/proj.toml --bin=sum_and_mult
echo "Actual output: Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running cbv.zydeco"
echo "Expected output: Prints true and then exits with code 0"
zydeco run lib/oopsla/proj.toml --bin=cbv
echo "Actual output: Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running monads.zydeco"
echo "Expected output: Program exited with code 0"
zydeco run lib/oopsla/proj.toml --bin=monads
echo "Actual output: Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running exn.zydeco"
echo "Expected output: Program exited with code 0"
zydeco run lib/oopsla/proj.toml --bin=exn
echo "Actual output: Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running free.zydeco"
echo "Expected output: Program exited with code 0"
zydeco run lib/oopsla/proj.toml --bin=free
echo "Actual output: Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running algebra.zydeco"
echo "Expected output: Program exited with code 0"
zydeco run lib/oopsla/proj.toml --bin=algebra
echo "Actual output: Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running exnt.zydeco"
echo "Expected output: Program exited with code 0"
zydeco run lib/oopsla/proj.toml --bin=exnt
echo "Actual output: Program exited with code $?"
echo "Press Enter to continue"
read

echo "Running exnkt.zydeco"
echo "Expected output: Program exited with code 0"
zydeco run lib/oopsla/proj.toml --bin=exnkt
echo "Actual output: Program exited with code $?"
echo "Demo ends here."
