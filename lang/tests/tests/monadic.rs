use zydeco_tests::{check_source, runtime_source};

check_source!(exnt, "tests/oopsla/exnt.zydeco");
check_source!(exnkt, "tests/oopsla/exnkt.zydeco");
runtime_source!(mobin, "tests/monadic/mobin.zy");
