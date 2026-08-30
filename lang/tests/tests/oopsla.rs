use zydeco_tests::{check_source, runtime_source};

runtime_source!(polynomial, "tests/oopsla/polynomial.zydeco");
runtime_source!(cc, "tests/oopsla/cc.zydeco");
runtime_source!(cbv, "tests/oopsla/cbv.zydeco");
runtime_source!(monads, "tests/oopsla/monads.zydeco");
runtime_source!(exn, "tests/oopsla/exn.zydeco");
runtime_source!(free, "tests/oopsla/free.zydeco");
runtime_source!(algebra, "examples/algebra.zydeco");
check_source!(exnt, "tests/oopsla/exnt.zydeco");
check_source!(exnkt, "tests/oopsla/exnkt.zydeco");
