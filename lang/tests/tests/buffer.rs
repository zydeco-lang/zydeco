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
            "let (/memory) = builtin in \
             ! memory/close OS 0 { fn _ => ! exit 1 } { ! exit 0 }",
        ),
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn memory_operations_obey_their_selected_answer_protocol() {
    SourceCase::assert_rejected(
        SourceCase::check(
            "let (/memory) = builtin in \
             let pure : Thk (Ret Unit) = { \
               ! memory/allocate OS 4 4 { fn _ => ! exit 1 } { fn _ => ! exit 0 } \
             } in ! exit 0",
        ),
        TyckDiagnosticCode::TypeMismatch,
    );
}
