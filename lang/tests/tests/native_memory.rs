use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::{e2e_sources, utils::SourceCase};

e2e_sources!({
    memory_views => "tests/std/memory-views.zy",
    memory_faults => "tests/std/memory-faults.zy",
});

#[test]
fn addresses_and_access_grants_cannot_be_forged_or_interchanged() {
    for body in [
        "let (/Addr) = builtin in let forged : Addr = 0 in ! exit 0",
        "let (/Access) = builtin in let forged : Access = 0 in ! exit 0",
        "let (/Addr; /Access) = builtin in let wrong : Thk (Access -> Ret Addr) = { fn x => ret x } in ! exit 0",
    ] {
        SourceCase::assert_rejected(SourceCase::check(body), TyckDiagnosticCode::TypeMismatch);
    }
}
