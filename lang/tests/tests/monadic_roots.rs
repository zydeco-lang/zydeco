//! Source cases cover root interfaces; Rust checks sort preservation, imports,
//! arena invariants, and shared lowering of the new roots.

use zydeco_cli::CommandCompiler;
use zydeco_statics::syntax::TermAnnId;
use zydeco_tests::utils::SourceCase;

struct MonadicRootCase;

impl MonadicRootCase {
    fn source(body: &str) -> String {
        let library = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../lib/std/effect");
        format!(
            r#"
let VType = @(intrinsic(vtype)) in
let CType = @(intrinsic(ctype)) in
let Thk = @(intrinsic(thk)) in
let Ret = @(intrinsic(ret)) in
let Int = @(intrinsic(int)) in
let Unit = @(intrinsic(unit)) in
let (/Monad; /Algebra) = @(import("{}/monad.zy")) in
{body}
"#,
            library.display(),
        )
    }

    fn check(body: &str) -> zydeco_session::CheckedProgram {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("root.zy");
        std::fs::write(&path, Self::source(body)).unwrap();
        let compiler = CommandCompiler::default().with_lint_types(true);
        let analysis = compiler.analyze(&path).unwrap();
        compiler.checked_program(&analysis).unwrap()
    }
}

#[test]
fn monadic_preserves_value_and_computation_root_sorts() {
    for source in ["1", "{ ret 1 }", "val (x : Int) => x", "ret 1"] {
        let before = MonadicRootCase::check(source);
        let after = MonadicRootCase::check(&format!("@[monadic] ({source})"));
        assert_eq!(
            std::mem::discriminant(&before.root),
            std::mem::discriminant(&after.root),
            "monadic translation must preserve the sort of {source}",
        );
    }
}

#[test]
fn monadic_type_results_are_manifest_exports() {
    for source in ["Int", "Ret", "Ret Int"] {
        for body in [source.to_owned(), format!("@[monadic] ({source})")] {
            let checked =
                MonadicRootCase::check(&format!("pack (= Result as ({body})) where () end"));
            assert!(matches!(checked.root, TermAnnId::Value(_, _)));
            assert_eq!(checked.statics.compus.iter().count(), 0, "type exports require no code");
        }
    }
}

#[test]
fn nominal_type_roots_do_not_generate_a_runtime_dictionary() {
    let checked = MonadicRootCase::check(
        r#"pack (= StreamFamily as (@[monadic] begin
  def Stream : CType = codata | .head : Ret Int | .tail : Stream end that
  Stream
end)) where () end"#,
    );
    assert!(matches!(checked.root, TermAnnId::Value(_, _)));
    assert_eq!(checked.statics.seals.iter().count(), 2, "source and translated nominal family");
    assert_eq!(checked.statics.compus.iter().count(), 0, "type roots only construct types");
}

#[test]
fn translated_nominal_type_imports_share_their_family() {
    SourceCase::assert_accepted(SourceCase::check_with_import(
        &MonadicRootCase::source(
            r#"
let First = @[monadic] @(import("imported.zy")) in
let Second = @[monadic] @(import("imported.zy")) in
let first : Thk (First Ret) = {
  fix self => comatch | .head => ret 1 | .tail => ! self end
} in
let second : Thk (Second Ret) = first in
! second .head
"#,
        ),
        r#"begin
  let CType = @(intrinsic(ctype)) that
  let Ret = @(intrinsic(ret)) that
  let Int = @(intrinsic(int)) that
  def Stream : CType = codata | .head : Ret Int | .tail : Stream end that
  Stream
end"#,
    ));
}

#[test]
fn translated_value_imports_export_value_functions() {
    SourceCase::assert_accepted(SourceCase::check_with_import(
        &MonadicRootCase::source(
            r#"
let mo : Thk (Monad Ret) = {
  comatch
  | .return A value => ret value
  | .bind A B computation continuation => do value <- ! computation; ! continuation value
  end
} in
let make = (@[monadic] @(import("imported.zy"))) Ret mo in
let thunk : Thk (Ret Int) = make 3 in
! thunk
"#,
        ),
        "val (value : @(intrinsic(int))) => { ret value }",
    ));
}

#[test]
fn new_roots_lint_and_lower_through_the_shared_pipeline() {
    for fixture in [
        include_str!("../cases/monadic-roots/translates_types_and_type_constructors.zy"),
        include_str!("../cases/monadic-roots/derives_the_monad_interface.zy"),
        include_str!("../cases/monadic-roots/translates_value_functions_and_their_classifiers.zy"),
        include_str!("../cases/monadic-roots/translates_nominal_type_roots.zy"),
        include_str!("../cases/monadic-roots/value_roots_pass_computation_type_algebras.zy"),
        include_str!("../cases/monadic-roots/value_modules_preserve_manifest_exports.zy"),
        include_str!(
            "../cases/monadic-roots/value_roots_use_the_runtime_dictionary_without_an_extra_return.zy"
        ),
        include_str!(
            "../cases/monadic-roots/value_modules_preserve_nominal_packages_and_lazy_algebras.zy"
        ),
    ] {
        let source = MonadicRootCase::source(fixture);
        SourceCase::assert_accepted(SourceCase::check_linted(&source));
        let executable = if fixture.contains("-- stage: run") {
            source
        } else {
            MonadicRootCase::source(&format!("do _ <- begin\n{fixture}\nend;\n! exit 0"))
        };
        SourceCase::assert_accepted(SourceCase::lower(&executable));
    }
}
