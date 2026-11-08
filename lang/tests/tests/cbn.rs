//! `@[cbn]`, reserved for call-by-name regions and refused until they exist.
//!
//! The annotation is registered so that it is not an unrecognized one: the host
//! accepts an unrecognized annotation and ignores it, so a body written for
//! call-by-name would run as plain call-by-push-value without a word.

use zydeco_session::DesugarError;
use zydeco_tests::utils::SourceCase;

/// This file's case helpers, a namespace as `SourceCase` is.
struct RegionCase;

impl RegionCase {
    /// A region is a computation, so it needs a computation position.
    fn block(source: &str) -> String {
        format!("do case_result <- {source};\nret ()")
    }
}

/// The same payload is refused under `@[cbn]` and accepted under `@[cbv]`, so
/// the refusal is the annotation's and not the body's.
#[test]
fn a_call_by_name_region_is_refused_until_implemented() {
    SourceCase::assert_accepted(SourceCase::check_invariants(&RegionCase::block("@[cbv] 5")));
    SourceCase::assert_desugar_error(SourceCase::check(&RegionCase::block("@[cbn] 5")), |error| {
        matches!(error, DesugarError::CbnNotImplemented(_))
    });
}

/// Before registration `@[cbn] (ret 5)` ran as `ret 5`. An unrecognized name
/// still does, which is what the registration takes `cbn` out of.
#[test]
fn an_unrecognized_annotation_is_still_ignored() {
    SourceCase::assert_accepted(SourceCase::check_invariants(&RegionCase::block(
        "@[cbn_unregistered] (ret 5)",
    )));
    SourceCase::assert_desugar_error(
        SourceCase::check(&RegionCase::block("@[cbn] (ret 5)")),
        |error| matches!(error, DesugarError::CbnNotImplemented(_)),
    );
}
