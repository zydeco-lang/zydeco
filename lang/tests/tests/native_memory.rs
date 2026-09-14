use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::{e2e_sources, utils::SourceCase};

e2e_sources!({
    source_bytes => "tests/std/source-bytes.zy",
    memory_views => "tests/std/memory-views.zy",
    memory_faults => "tests/std/memory-faults.zy",
});

#[test]
fn raw_addresses_cannot_be_forged_from_integers() {
    SourceCase::assert_rejected(
        SourceCase::check("let (/Addr) = builtin in let forged : Addr = 0 in ! exit 0"),
        TyckDiagnosticCode::TypeMismatch,
    );
}
