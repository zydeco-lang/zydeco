use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::{e2e_sources, utils::SourceCase};

e2e_sources!({
    buffer => "tests/std/buffer.zy",
    storage_access => "tests/std/storage-access.zy",
});

#[test]
fn a_buffer_handle_cannot_be_forged_from_an_integer() {
    SourceCase::assert_rejected(
        SourceCase::check(
            "let (/buffer) = builtin in \
             ! buffer/close 0 { fn _ => ! exit 1 } { ! exit 0 }",
        ),
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn buffer_effects_do_not_inhabit_a_returning_computation() {
    SourceCase::assert_rejected(
        SourceCase::check(
            "let (/buffer) = builtin in \
             let pure : Thk (Ret Unit) = { \
               ! buffer/allocate 4 4 { fn _ => ! exit 1 } { fn _ => ! exit 0 } \
             } in ! exit 0",
        ),
        TyckDiagnosticCode::TypeMismatch,
    );
}
