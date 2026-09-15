#[path = "support/memory.rs"]
mod memory;

use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::SourceCase;

#[test]
fn storage_geometry_is_validated_and_sealed() {
    for (count, alignment, pattern) in [
        (8, 8, "+Ok(_)"),
        (0, 1, "+Ok(_)"),
        (-1, 8, "+Err(+NegativeSize())"),
        (8, 0, "+Err(+InvalidAlignment())"),
        (8, 3, "+Err(+InvalidAlignment())"),
    ] {
        SourceCase::assert_accepted(SourceCase::run(&memory::source(&format!(
            "match storage/constant/create {count} {alignment} \
             | {pattern} => ! exit 0 | _ => ! exit 1 end"
        ))));
        SourceCase::assert_accepted(SourceCase::run(&memory::source(&format!(
            "do checked <- ! storage/runtime/create {count} {alignment}; \
             match checked | {pattern} => ! exit 0 | _ => ! exit 1 end"
        ))));
    }
    for (body, code) in [
        ("let forged : Storage Unit = () in ! exit 0", TyckDiagnosticCode::TypeMismatch),
        (
            "let forged : DynamicStorage Unit = (#size = 8, #alignment = 3) in ! exit 0",
            TyckDiagnosticCode::TypeExpected,
        ),
        (
            "let promote : Thk (DynamicStorage Unit -> Ret (Storage Unit)) = { fn value => ret value } in ! exit 0",
            TyckDiagnosticCode::TypeMismatch,
        ),
    ] {
        SourceCase::assert_rejected(SourceCase::check(&memory::source(body)), code);
    }
    SourceCase::assert_rejected(
        SourceCase::check(&memory::source(
            "let build : Thk (Int -> OS) = { fn count => \
         let checked = storage/constant/create count 8 in ! exit 0 } in ! exit 0",
        )),
        TyckDiagnosticCode::StaticElimination,
    );
}

#[test]
fn codecs_require_matching_states_values_and_layout_witnesses() {
    let prefix = "match storage/constant/create 8 8 | +Err(_) => ! fail | +Ok(= L, layout) => \
        let codec = codecs/unsafe/scalar L Int64 numeric/int64/store_le numeric/int64/load_le in \
        ! (allocation/reserve L Unit allocation/static_heap () layout) OS no { fn vacant => ";
    let source = |body: &str| memory::source(&format!("{prefix} {body} }} end"));
    let valid = "! (codec/init vacant (7 : Int64)) OS { fn initialized => \
        ! (codecs/unsafe/take L Int64 codec initialized) OS { fn vacant _ => \
        ! (allocation/unsafe/release L Unit allocation/static_heap () layout vacant) OS no { ! exit 0 } } }";
    SourceCase::assert_accepted(SourceCase::check_linted(&source(valid)));
    SourceCase::assert_accepted(SourceCase::run(&source(valid)));
    for (invalid, code) in [
        ("! (codec/read vacant) OS { fn _ => ! exit 0 }", TyckDiagnosticCode::TypeMismatch),
        (
            "! (codec/init vacant (7 : UInt64)) OS { fn _ => ! exit 0 }",
            TyckDiagnosticCode::TypeMismatch,
        ),
        (
            "! (codec/init vacant (7 : Int64)) OS { fn initialized => \
         ! (allocation/unsafe/release L Unit allocation/static_heap () layout initialized) OS no { ! exit 0 } }",
            TyckDiagnosticCode::TypeMismatch,
        ),
        (
            "match storage/constant/create 8 8 | +Err(_) => ! fail | +Ok(= Other, _) => \
         let other = codecs/unsafe/scalar Other Int64 numeric/int64/store_le numeric/int64/load_le in \
         ! (other/init vacant (7 : Int64)) OS { fn _ => ! exit 0 } end",
            TyckDiagnosticCode::NamedLabelMismatch,
        ),
    ] {
        SourceCase::assert_rejected(SourceCase::check(&source(invalid)), code);
    }
}
