//! The structural-source-form decisions of `@[cbv]`, as tests.
//!
//! Some source forms are not type formers at all: wrappers, block structure and
//! metadata. Each takes a **decision** rather than a derivation from the
//! translation's rule for types. A decision recorded only in prose is a claim
//! nothing re-runs, so each test below asserts what the implementation *does*,
//! and a change to the behaviour turns this file red.

use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::{CbvBlock, SourceCase};

/// This file's case helpers, a namespace as `SourceCase` is.
struct CbvCase;

impl CbvCase {
    /// A call-by-value block is a computation, so it needs a computation
    /// position. Each block in `source` opens with the source built-ins, by
    /// [`CbvBlock::closed`].
    fn block(source: &str) -> String {
        CbvBlock::closed(&format!("do case_result <- {source};\nret ()"))
    }

    fn accept(source: &str) {
        SourceCase::assert_accepted(SourceCase::check_invariants(&CbvCase::block(source)));
    }

    fn reject(source: &str, expected: TyckDiagnosticCode) {
        SourceCase::assert_rejected(SourceCase::check(&CbvCase::block(source)), expected);
    }
}

/* ---------------------------------- holes --------------------------------- */

/// The table decides `_` is **admitted**, analysis direction only, and it is: a
/// typed hole at the translated type, which checking reports and running
/// refuses, as in the host. It is a value hole, `ret _` as a term, so a report
/// names `A'` rather than `Ret A'`. This test asserted the refusal until the
/// decision was implemented, on 2026-10-06.
///
/// Checked without the arena invariants, which report any unfilled hole as an
/// internal error, the host's own `ret (_ : Int)` included.
#[test]
fn a_hole_is_admitted_where_a_type_is_expected() {
    SourceCase::assert_accepted(SourceCase::check(&CbvCase::block("@[cbv] ((_ : Int))")));
}

/// With nothing expected there is nothing to report the hole at.
#[test]
fn a_hole_is_refused_with_nothing_expected() {
    CbvCase::reject("@[cbv] (_)", TyckDiagnosticCode::MissingAnnotation);
}

/* -------------------------------- metadata -------------------------------- */

/// The table decides `@[doc]`, `@[debug]` and `@[format]` are admitted
/// **transparently**, re-attached to the emitted term so that `@[debug]`'s
/// observation survives the block, and they are: the body is translated as
/// though the annotation were absent, and `@[debug]` observes the translation.
/// This test asserted the refusal until the decision was implemented, on
/// 2026-10-06.
#[test]
fn transparent_metadata_is_admitted() {
    for annotation in ["doc", "debug", "format"] {
        CbvCase::accept(&format!("@[cbv] (@[{annotation}] 5)"));
    }
}

/// The table's sibling decision, and the one the implementation keeps, by the
/// catch-all rather than by a clause.
#[test]
fn other_metadata_is_rejected() {
    CbvCase::reject("@[cbv] (@[typeof] 5)", TyckDiagnosticCode::Expressivity);
}

/* --------------------------- nominal definitions -------------------------- */

/// A `def` name declared outside the block is **refused**, whatever it
/// unfolds to: a block is closed over types as over terms, and an outside type
/// enters by type abstraction. The decision was that such a name is admitted
/// when its unfolding is in the image of the type translation, which an image
/// test decided by unfolding the nominal, until blocks were closed.
#[test]
fn a_nominal_name_declared_outside_is_refused() {
    SourceCase::assert_rejected(
        SourceCase::check(&format!(
            "begin
  def Nat : VType = data | +Zero : Unit | +Succ : Nat end that
  {}
end",
            CbvCase::block("@[cbv] (param n : Nat that 1)")
        )),
        TyckDiagnosticCode::Expressivity,
    );
}

/// Its accepted neighbour, the same declaration made in the block.
#[test]
fn a_nominal_declared_in_the_block_is_admitted() {
    CbvCase::accept(
        "@[cbv] (def Nat : VType = data | +Zero : Unit | +Succ : Nat end that let f = fn (n : Nat) => 1 that 0)",
    );
}

/* ----------------------------- block structure ---------------------------- */

/// ⚠️ **Disagrees with the table**, which decides `begin … end` and `that`
/// bindings are **rejected**. A ⚠️ block in the same document already narrowed
/// that decision on evidence — admit an inner block, reject only a `that` that
/// would escape — and the row was never updated. Both are accepted.
#[test]
fn a_that_binding_inside_a_block_is_accepted() {
    CbvCase::accept("@[cbv] (let x = 5 that x)");
}

#[test]
fn an_inner_block_is_accepted() {
    CbvCase::accept("@[cbv] (begin 5 end)");
}

/// The premise the narrowing rested on is **also stale**: it recorded that a bare
/// `that` hoists past the block, leaving its name visible outside. It does not,
/// since a block became a binding boundary. The binding stays inside, so there is
/// nothing for the rule to reject — and the error is a scoping one from outside the
/// block rather than an expressivity one from within it.
#[test]
fn a_that_binding_does_not_escape_the_block() {
    SourceCase::assert_resolve_error(SourceCase::check(&CbvBlock::closed(
        "do a <- @[cbv] (let y = 7 that 1);\ndo b <- ret y;\nret ()",
    )));
}

/* ------------------------------- target forms ----------------------------- */

/// The translation's own output vocabulary is not source syntax. A block that
/// could mention a shift would not be call-by-value, so each of these is refused
/// where it would otherwise be read as the thing the translation produces. The
/// shifts are refused as names the block did not bind, and written with their
/// intrinsics as intrinsics that name no source primitive.
#[test]
fn the_translations_own_vocabulary_is_rejected() {
    CbvCase::reject(
        "@[cbv] (param x : Thk (Int -> Ret Int) that 1)",
        TyckDiagnosticCode::Expressivity,
    );
    CbvCase::reject(
        "@[cbv] (param x : (@(intrinsic(thk))) (Int -> (@(intrinsic(ret))) Int) that 1)",
        TyckDiagnosticCode::Expressivity,
    );
    CbvCase::reject("@[cbv] (ret 5)", TyckDiagnosticCode::Expressivity);
    CbvCase::reject("@[cbv] (do x <- ret 5; ret x)", TyckDiagnosticCode::Expressivity);
    CbvCase::reject("@[cbv] (fix g => 5)", TyckDiagnosticCode::Expressivity);
}

/* --------------------------- interleaved copattern ------------------------ */

/// One observation per clause. An interleaved copattern binds a value argument in
/// the same clause as the field it observes, and the un-interleaved spelling is
/// required instead.
#[test]
fn an_interleaved_copattern_is_rejected() {
    CbvCase::reject(
        "@[cbv] (let Fn : VType = codata | .at : Int -> Int end that ((comatch | .at n => n end) : Fn))",
        TyckDiagnosticCode::Expressivity,
    );
}

/// Its accepted neighbour, differing only in where the parameter is bound — so the
/// rejection above is about interleaving and not about the field.
#[test]
fn the_un_interleaved_spelling_is_accepted() {
    CbvCase::accept(
        "@[cbv] (let Fn : VType = codata | .at : Int -> Int end that
  ((comatch | .at => fn (n : Int) => n end) : Fn))",
    );
}
