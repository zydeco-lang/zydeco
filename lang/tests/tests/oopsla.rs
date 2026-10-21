use zydeco_tests::{check_source, e2e_io_source, e2e_sources};

e2e_sources!({
    polynomial => "tests/oopsla/polynomial.zydeco",
    cc => "tests/oopsla/cc.zydeco",
});

e2e_io_source!(cbv, "tests/oopsla/cbv.zydeco", "", "true\n");
e2e_io_source!(monads, "tests/oopsla/monads.zydeco", "", "Hello, world!\n");
e2e_io_source!(exn, "tests/oopsla/exn.zydeco", "", "2 != 1\n");
e2e_io_source!(free, "tests/oopsla/free.zydeco", "", "\n");

// These fixtures annotate the generated monadic-block translation with
// `@[debug]`, so checking them also exercises the debug printout.
check_source!(exnt, "tests/oopsla/exnt.zydeco");
check_source!(exnkt, "tests/oopsla/exnkt.zydeco");
check_source!(oopsla_algebra, "tests/oopsla/algebra.zydeco");
check_source!(oopsla_core, "tests/oopsla/core.zydeco");
check_source!(oopsla_data, "tests/oopsla/data.zydeco");
