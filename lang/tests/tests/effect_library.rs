use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::{SourceCase, SourceTemplate};

zydeco_tests::check_source!(ret_module, "std/effect/ret.zy");
zydeco_tests::check_source!(reader_module, "std/effect/reader.zy");
zydeco_tests::check_source!(writer_module, "std/effect/writer.zy");
zydeco_tests::check_source!(option_module, "std/effect/option.zy");

zydeco_tests::e2e_io_source!(operations, "tests/effects/effect-operations.zy", "", "");
zydeco_tests::e2e_io_source!(laws, "tests/effects/effect-laws.zy", "", "");
zydeco_tests::e2e_io_source!(
    law_counterexamples,
    "tests/effects/effect-law-counterexamples.zy",
    "",
    ""
);
zydeco_tests::e2e_io_source!(
    translated_clients,
    "tests/effects/effect-monadic.zy",
    "",
    "effect monadic clients passed\n"
);

struct EffectLibraryCase;

impl EffectLibraryCase {
    fn source(body: &str) -> String {
        let effect = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/std/effect")
            .display()
            .to_string();
        SourceTemplate::parse(include_str!("../templates/effect-library.zy.in"))
            .and_then(|template| template.render([("effect", effect.as_str()), ("body", body)]))
            .expect("the effect library template and its arguments must agree")
    }
}

#[test]
fn concrete_and_transformed_instances_preserve_their_carriers() {
    let source = EffectLibraryCase::source(
        r#"
let _ : WithMonad (IReader Int) = (R, r) in
let _ : WithMonad (IWriter Int) = (W, w) in
let _ : WithMonad IOption = (O, o) in
let (RW, rw) = reader/transformer_instance Int W w/monad in
let (WR, wr) = writer/transformer_instance Int output R r/monad in
let (OW, ow) = optional/transformer_instance W w/monad in
let _ : WithMonad (IReader Int) = (RW, #monad = rw/monad, #reader = rw/reader) in
let _ : WithMonad (IWriter Int) = (WR, #monad = wr/monad, #writer = wr/writer) in
let _ : WithMonad IOption = (OW, #monad = ow/monad, #option = ow/option) in
! exit 0
"#,
    );
    SourceCase::assert_accepted(SourceCase::check_linted(&source));
    SourceCase::assert_accepted(SourceCase::lower(&source));
}

#[test]
fn effect_interfaces_reject_mixed_carriers() {
    for body in [
        "let _ : WithMonad (IReader Int) = (R, #monad = w/monad, #reader = r/reader) in ! exit 0",
        "let _ : WithMonad (IReader Int) = (W, #monad = w/monad, #reader = r/reader) in ! exit 0",
        "let _ : WithMonad (IWriter Int) = (W, #monad = r/monad, #writer = w/writer) in ! exit 0",
        "let _ : WithMonad (IWriter Int) = (R, #monad = r/monad, #writer = w/writer) in ! exit 0",
        "let _ : WithMonad IOption = (O, #monad = w/monad, #option = o/option) in ! exit 0",
        "let _ : WithMonad IOption = (W, #monad = w/monad, #option = o/option) in ! exit 0",
    ] {
        let source = EffectLibraryCase::source(body);
        SourceCase::assert_rejected(SourceCase::check(&source), TyckDiagnosticCode::TypeMismatch);
        SourceCase::assert_rejected(SourceCase::lower(&source), TyckDiagnosticCode::TypeMismatch);
    }
}

#[test]
fn option_values_are_not_computation_carriers() {
    let source = EffectLibraryCase::source("let _ : Thk (Monad Option) = o/monad in ! exit 0");
    SourceCase::assert_rejected(SourceCase::check(&source), TyckDiagnosticCode::KindMismatch);
    SourceCase::assert_rejected(SourceCase::lower(&source), TyckDiagnosticCode::KindMismatch);
}

#[test]
fn writer_rejects_an_append_with_the_wrong_output_type() {
    let source = EffectLibraryCase::source(
        "let _ = writer/instance Int (#empty = 0, #append = numeric/int/to_string) in ! exit 0",
    );
    SourceCase::assert_rejected(SourceCase::check(&source), TyckDiagnosticCode::TypeMismatch);
    SourceCase::assert_rejected(SourceCase::lower(&source), TyckDiagnosticCode::TypeMismatch);
}
