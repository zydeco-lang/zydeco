#[path = "support/memory.rs"]
mod memory;
use memory::MemoryCase;
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::SourceCase;

#[test]
fn pointer_call_protocols_require_a_shared_layout_and_state() {
    for (parameter, accepted) in [("Ptr L Init", true), ("Ptr L Uninit", false), ("UInt8", false)] {
        let body = format!(
            r#"
match memory/uint8 | +Left(_) => ! fail | +Right(plan) =>
  let (= L, repr) = memory/realize UInt8 plan in
  let use : Thk ({parameter} -> OS) = {{ fn value =>
    ! (codecs/unsafe/take L UInt8 repr/codec value) OS {{ fn vacant _ => ! (allocation/unsafe/release L Unit allocation/static_heap () repr/storage vacant) OS no {{ ! exit 0 }} }}
  }} in
  ! (allocation/reserve L Unit allocation/static_heap () repr/storage) OS no {{ fn vacant =>
    ! (repr/codec/init vacant 7) OS use
  }}
end
"#
        );
        let source = MemoryCase::source(&body);
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
