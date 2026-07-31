use zydeco_tests::e2e_sources;

e2e_sources!({
    invert => "tests/stack/invert.zydeco",
    backtrace => "tests/stack/backtrace.zydeco",
    cbv => "tests/oopsla/cbv.zydeco",
    merge => "tests/stack/merge.zydeco",
});
