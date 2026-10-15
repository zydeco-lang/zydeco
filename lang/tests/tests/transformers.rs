use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::{SourceCase, SourceTemplate};

zydeco_tests::check_source!(transformer_module, "std/effect/transformer.zy");
zydeco_tests::e2e_io_source!(laws, "tests/effects/transformer-laws.zy", "", "");
zydeco_tests::e2e_io_source!(operations, "tests/effects/transformer-operations.zy", "", "");
zydeco_tests::e2e_io_source!(translated_clients, "tests/effects/transformer-monadic.zy", "", "");

struct TransformerCase;

impl TransformerCase {
    fn source(body: &str) -> String {
        let library = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/std")
            .display()
            .to_string();
        SourceTemplate::parse(include_str!("../templates/transformers.zy.in"))
            .and_then(|template| template.render([("std", library.as_str()), ("body", body)]))
            .expect("the transformer template and its arguments must agree")
    }

    fn reject(body: &str, code: TyckDiagnosticCode) {
        let source = Self::source(body);
        SourceCase::assert_rejected(SourceCase::check(&source), code);
        SourceCase::assert_rejected(SourceCase::lower(&source), code);
    }
}

#[test]
fn common_interface_derives_a_closed_source_and_preserves_packed_witnesses() {
    let source = TransformerCase::source(
        r#"
let _ : Thk (MonadTrans (StateT Int)) = { ! state/transformer Int } in
let _ : Thk (MonadTrans (ExceptionT Unit)) = { ! exception/transformer Unit } in
let _ : WithMonad (ITransformer W) = (SW, #monad = sw/monad, #lift = sw/lift) in
let _ : WithMonad (ITransformer W) = (EW, #monad = ew/monad, #lift = ew/lift) in
let _ : WithMonad (IState Int) = (SW, #monad = sw/monad, #state = sw/state) in
let _ : WithMonad (fn (T : VType -> CType) => ITransformer W T * IState Int T) =
  (SW, #monad = sw/monad, (#lift = sw/lift, #state = sw/state)) in
let _ : WithMonad (IException Unit) =
  (EW, #monad = ew/monad, (#exception = ew/exception, #catch = ew/catch)) in
let _ : Thk (NonDetT W Int) = { ! nw/monad .return Int 1 } in
do (answer, written) <- ! identity W w/monad .monad .bind Int Int {
  ! identity W w/monad .lift Int { ret (5, 1) }
} { fn answer => ! identity W w/monad .lift Int { ret (answer, 2) } };
match (answer, written) | (5, 3) => ! exit 0 | _ => ! exit 1 end
"#,
    );
    SourceCase::assert_accepted(SourceCase::check_linted(&source));
    SourceCase::assert_accepted(SourceCase::lower(&source));
    SourceCase::assert_accepted(SourceCase::run(&source));
}

#[test]
fn transformer_interfaces_reject_wrong_bases_carriers_and_lifts() {
    for body in [
        "let _ : Thk (Lift Ret SW) = sw/lift in ! exit 0",
        "let _ : Thk (Lift W SW) = rw/lift in ! exit 0",
        "let _ : WithMonad (ITransformer W) = (SW, #monad = sw/monad, #lift = rw/lift) in ! exit 0",
        "let _ : WithMonad (ITransformer W) = (SW, #monad = rw/monad, #lift = sw/lift) in ! exit 0",
        "let _ : Thk (MonadTrans (ReaderT Int)) = optional/transformer in ! exit 0",
        "let _ : WithMonad (IState Int) = (EW, #monad = ew/monad, #state = sw/state) in ! exit 0",
        "let _ : WithMonad (IException Unit) = (SW, #monad = sw/monad, (#exception = ew/exception, #catch = ew/catch)) in ! exit 0",
    ] {
        TransformerCase::reject(body, TyckDiagnosticCode::TypeMismatch);
    }
}

#[test]
fn adaptation_does_not_erase_the_generated_answer_type_algebra() {
    TransformerCase::reject(
        r#"
let WithoutAlgebra (A : VType) =
  forall (R : CType) . Thk (A -> Thk R -> R) -> Thk R -> R in
let _ : Thk (WithoutAlgebra Int) = { ! nw/monad .return Int 1 } in
! exit 0
"#,
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn transformer_derivation_rejects_an_opaque_captured_dictionary() {
    TransformerCase::reject(
        r#"
let invalid = {
  fn (source : Thk (Monad Ret)) => @[monadic] ! source .return Int 1
} in
! exit 0
"#,
        TyckDiagnosticCode::NotInlinable,
    );
}
