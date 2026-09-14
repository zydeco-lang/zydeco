#[path = "support/memory.rs"]
mod memory;
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::{e2e_sources, utils::SourceCase};

e2e_sources!({ represented_call => "tests/std/represented-call/main.zy" });

#[test]
fn pointer_call_protocols_require_a_shared_layout_and_state() {
    for (parameter, accepted) in [("Ptr L Init", true), ("Ptr L Uninit", false), ("UInt8", false)] {
        let body = format!(
            r#"
match memory/uint8 | +Err(_) => ! fail | +Ok(plan) =>
  let (= L, repr) = memory/realize UInt8 plan in
  let use : Thk ({parameter} -> OS) = {{ fn value =>
    ! repr/unsafe/take OS value {{ fn vacant _ => ! repr/unsafe/free OS heap vacant no {{ ! exit 0 }} }}
  }} in
  ! repr/allocate OS heap no {{ fn vacant =>
    ! repr/unsafe/init OS vacant 7 use
  }}
end
"#
        );
        let source = memory::source(&body);
        if accepted {
            SourceCase::assert_accepted(SourceCase::check_linted(&source));
            SourceCase::assert_accepted(SourceCase::run(&source));
        } else {
            SourceCase::assert_rejected(
                SourceCase::check(&source),
                TyckDiagnosticCode::TypeMismatch,
            );
        }
    }
}
