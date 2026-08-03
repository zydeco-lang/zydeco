use zydeco_tests::{check_source, interp_source};

check_source!(exnt, "tests/oopsla/exnt.zydeco");
check_source!(exnkt, "tests/oopsla/exnkt.zydeco");
interp_source!(mobin, "tests/monadic/mobin.zy");
