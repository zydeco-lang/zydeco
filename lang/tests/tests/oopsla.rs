use zydeco_tests::{check_source, interp_source};

interp_source!(polynomial, "tests/oopsla/polynomial.zydeco");
interp_source!(cc, "tests/oopsla/cc.zydeco");
interp_source!(cbv, "tests/oopsla/cbv.zydeco");
interp_source!(monads, "tests/oopsla/monads.zydeco");
interp_source!(exn, "tests/oopsla/exn.zydeco");
interp_source!(free, "tests/oopsla/free.zydeco");
interp_source!(algebra, "examples/algebra.zydeco");
check_source!(exnt, "tests/oopsla/exnt.zydeco");
check_source!(exnkt, "tests/oopsla/exnkt.zydeco");
