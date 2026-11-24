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

#[test]
fn monadic_blocks_translate_returning_and_branching_intrinsic_operations() {
    use zydeco_tests::invariants::ArenaInvariants;
    use zydeco_tests::utils::SourceCase;

    let std = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../lib/std");
    let source = format!(
        r#"
let (/Monad; /Algebra) = @(import("{std}/effect/monad.zy")) in
let (/instance = ret_instance) = @(import("{std}/effect/ret.zy")) in
def ! m = @[monadic] begin
  do sum <- ! (@(intrinsic(int/add))) 20 22;
  do cp <- ! (@(intrinsic(char/codepoint))) 'A';
  do tag <- ! (@(intrinsic(string/append))) "k" "!";
  ! (@(intrinsic(int/eq))) (Ret Int) sum 42 {{
    ! (@(intrinsic(int/eq))) (Ret Int) cp 65 {{
      ! (@(intrinsic(string/eq))) (Ret Int) tag "k!" {{ ret 0 }} {{ ret 3 }}
    }} {{ ret 2 }}
  }} {{ ret 1 }}
end in
do code <- ! m Ret ret_instance/monad;
! exit code
"#,
        std = std.display()
    );
    let (statics, root) = SourceCase::checked_arena(&source).unwrap();
    let violations = ArenaInvariants { statics: &statics }.violations(root);
    assert!(violations.is_empty(), "monadic intrinsic check left violations: {violations:?}");
    SourceCase::assert_accepted(SourceCase::lower(&source));
    SourceCase::assert_accepted(SourceCase::run(&source));
}
