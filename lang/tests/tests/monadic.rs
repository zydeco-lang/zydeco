use zydeco_tests::e2e_sources;

e2e_sources!({
    mobin => "tests/monadic/mobin.zy",
    alg => "tests/monadic/alg.zy",
    backtrack => "tests/monadic/backtrack.zydeco",
    cbv => "tests/monadic/cbv.zy",
    cbpv => "tests/monadic/cbpv.zy",
    cbpv_monadic => "tests/monadic/cbpv-monadic.zy",
    free => "tests/monadic/free.zy",
    free_ => "tests/monadic/free'.zy",
    optiont => "tests/monadic/optiont.zy",
    trans => "tests/monadic/trans.zy",
});

// `algtrans`, `monadic-int`, and `monadic-ret` are driven by the session
// crate's port tests, and `shadow.zy` by its basis-shadowing check.
