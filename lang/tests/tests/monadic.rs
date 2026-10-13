use zydeco_tests::{e2e_io_source, e2e_sources};

e2e_sources!({
    mobin => "tests/monadic/mobin.zy",
    alg => "tests/monadic/alg.zy",
    free => "tests/monadic/free.zy",
    free_ => "tests/monadic/free'.zy",
    optiont => "tests/monadic/optiont.zy",
    trans => "tests/monadic/trans.zy",
});

e2e_io_source!(backtrack, "tests/monadic/backtrack.zydeco", "", "1 + 4\n2 + 3\n");
e2e_io_source!(cbv, "tests/monadic/cbv.zy", "", "\\^o^/\n");
e2e_io_source!(cbpv, "tests/monadic/cbpv.zy", "", "\\^o^/\n\\^o^/\n");
e2e_io_source!(
    cbpv_monadic,
    "tests/monadic/cbpv-monadic.zy",
    "",
    "\\^o^/\n\\^o^/\n\\^o^/\n\\^o^/\n"
);

// `algtrans`, `monadic-int`, and `monadic-ret` are driven by the session
// crate's port tests, and `shadow.zy` by its basis-shadowing check.
