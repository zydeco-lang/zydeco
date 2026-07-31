use zydeco_tests::e2e_sources;

e2e_sources!({
    invert => "tests/source/stack-invert.zy",
    backtrace => "tests/source/stack-backtrace.zy",
    cbv => "tests/source/oopsla-cbv.zy",
    merge => "tests/source/stack-merge.zy",
});
