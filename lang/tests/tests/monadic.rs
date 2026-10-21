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

// `algtrans`, `monadic-int`, and `monadic-ret` run through the directive suite.
// `shadow.zy` has no execution contract, so it is checked directly: the inner
// basis must win over the outer one without changing the root's meaning.
#[test]
fn nested_monadic_block_resolves_the_nearest_basis() {
    let root =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../lib/tests/monadic/shadow.zy");
    let compiler = zydeco_cli::CommandCompiler::default();
    let analysis = compiler.analyze(&root).expect("the shadowing fixture loads");
    compiler.checked_program(&analysis).expect("the shadowing fixture checks");
}
