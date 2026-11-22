//! Call-by-value blocks: the translation, and the restrictions on its source.
//!
//! The accepted cases use `check_invariants`, so each one also asserts that the
//! arena the translation produced satisfies the arena invariants. Every
//! restriction is paired with an accepted neighbour that differs by one thing,
//! because a rejection on its own does not show that the rule is narrow enough.

use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::{CaseError, CbvBlock, SourceCase};

// Every block in this file opens with the source built-ins, by
// `CbvBlock::closed`, and reads as though they were the source language's own
// names. The wrappers below close a case before handing it on; a test about the
// closure itself calls `SourceCase` directly, so that what it names stays
// outside.

/// This file's case helpers, a namespace as `SourceCase` is.
struct CbvCase;

impl CbvCase {
    fn check(source: &str) -> Result<(), CaseError> {
        SourceCase::check(&CbvBlock::closed(source))
    }

    fn check_invariants(source: &str) -> Result<(), CaseError> {
        SourceCase::check_invariants(&CbvBlock::closed(source))
    }

    fn check_monadic(source: &str) -> Result<(), CaseError> {
        SourceCase::check_monadic(&CbvBlock::closed(source))
    }

    fn check_with_import(source: &str, imported: &str) -> Result<(), CaseError> {
        SourceCase::check_with_import(&CbvBlock::closed(source), imported)
    }

    fn run(source: &str) -> Result<(), CaseError> {
        SourceCase::run(&CbvBlock::closed(source))
    }

    fn debug_observations(source: &str) -> Result<Vec<String>, CaseError> {
        SourceCase::debug_observations(&CbvBlock::closed(source))
    }

    fn type_definition_bodies(source: &str, name: &str) -> Result<Vec<String>, CaseError> {
        SourceCase::type_definition_bodies(&CbvBlock::closed(source), name)
    }

    fn unannotated_terms(source: &str, shapes: &[&str]) -> Result<Vec<String>, CaseError> {
        SourceCase::unannotated_terms(&CbvBlock::closed(source), shapes)
    }

    fn unrecorded_pattern_facts(
        source: &str, name: &str,
    ) -> Result<(Vec<String>, usize), CaseError> {
        SourceCase::unrecorded_pattern_facts(&CbvBlock::closed(source), name)
    }

    /// A call-by-value block is a computation, so it needs a computation position.
    fn block(source: &str) -> String {
        format!("do case_result <- {source};\nret ()")
    }

    fn accept(source: &str) {
        SourceCase::assert_accepted(CbvCase::check_invariants(&CbvCase::block(source)));
    }

    /// `source` with every block in it opening with `declarations`, after the
    /// built-ins, each as a `that` binding, so a recursive one can name itself.
    ///
    /// A block is closed over types, so a type a test needs is declared in the
    /// block; an outside one would have to arrive by type abstraction. These
    /// were declarations made outside the block and named inside it, which the
    /// translation's image test admitted, until blocks were closed.
    fn within(declarations: &[&str], source: &str) -> String {
        let declarations = declarations
            .iter()
            .map(|declaration| format!("{declaration} that\n  "))
            .collect::<String>();
        source.replace("@[cbv] (", &format!("@[cbv] ({declarations}"))
    }

    /// [`CbvCase::within`], for a block in a computation position.
    fn declaring(declarations: &[&str], source: &str) -> String {
        CbvCase::within(declarations, &CbvCase::block(source))
    }

    fn accept_declaring(declarations: &[&str], source: &str) {
        SourceCase::assert_accepted(CbvCase::check_invariants(&CbvCase::declaring(
            declarations,
            source,
        )));
    }

    fn reject_declaring(declarations: &[&str], source: &str, expected: TyckDiagnosticCode) {
        SourceCase::assert_rejected(
            CbvCase::check(&CbvCase::declaring(declarations, source)),
            expected,
        );
    }

    fn reject(source: &str, expected: TyckDiagnosticCode) {
        SourceCase::assert_rejected(CbvCase::check(&CbvCase::block(source)), expected);
    }
}

/// Two data types, one of them recursive and so sealed.
const DATA: [&str; 2] = [
    "let Flag : VType = data | +Off : Unit | +On : Int end",
    "def Nat : VType = data | +Zero : Unit | +Succ : Nat end",
];

impl CbvCase {
    fn accept_with_data(source: &str) {
        SourceCase::assert_accepted(CbvCase::check_invariants(&CbvCase::declaring(&DATA, source)));
    }

    fn reject_with_data(source: &str, expected: TyckDiagnosticCode) {
        SourceCase::assert_rejected(CbvCase::check(&CbvCase::declaring(&DATA, source)), expected);
    }
}

/// Two codata types.
const CODATA: [&str; 2] = [
    "let Pair : VType = codata | .fst : Int | .snd : Int end",
    "let Fn : VType = codata | .at : Int -> Int end",
];

impl CbvCase {
    fn accept_with_codata(source: &str) {
        SourceCase::assert_accepted(CbvCase::check_invariants(&CbvCase::declaring(
            &CODATA, source,
        )));
    }

    fn reject_with_codata(source: &str, expected: TyckDiagnosticCode) {
        SourceCase::assert_rejected(CbvCase::check(&CbvCase::declaring(&CODATA, source)), expected);
    }
}

/// Existential types, abstract and manifest, bound to names. A block may also
/// write both forms where it uses them; the tests that do are in the section on
/// named fields and packages written in place.
const EXISTS: [&str; 3] = [
    "let Boxed : VType = exists (X : VType) . X",
    "let Clear : VType = exists (X as Int : VType) . X",
    "let Showable : VType = exists (X : VType) . (X * (X -> Int))",
];

impl CbvCase {
    fn accept_with_exists(source: &str) {
        SourceCase::assert_accepted(CbvCase::check_invariants(&CbvCase::declaring(
            &EXISTS, source,
        )));
    }

    fn reject_with_exists(source: &str, expected: TyckDiagnosticCode) {
        SourceCase::assert_rejected(CbvCase::check(&CbvCase::declaring(&EXISTS, source)), expected);
    }
}

/// Record types bound to names, for the same reason. `Pkg` is a package whose
/// telescope binds at a *labeled* kind, and `CInt` is a name for the witness
/// `(#Counter = Int)` that inhabiting it needs.
const RECORD: [&str; 4] = [
    "let Rec : VType = (#fst :: Int, #snd :: Int)",
    "let Lib : VType = (#id :: Int -> Int, #base :: Int)",
    "let Pkg : VType = exists (#Counter = ((Rep as Int) : VType)) . (#zero :: Rep)",
    "let CInt : (#Counter :: VType) = (#Counter = Int)",
];

impl CbvCase {
    fn accept_with_record(source: &str) {
        SourceCase::assert_accepted(CbvCase::check_invariants(&CbvCase::declaring(
            &RECORD, source,
        )));
    }

    fn reject_with_record(source: &str, expected: TyckDiagnosticCode) {
        SourceCase::assert_rejected(CbvCase::check(&CbvCase::declaring(&RECORD, source)), expected);
    }

    /// A checked `source` beside a term `outside` the block, for the tests that a
    /// classifier cannot read one.
    fn reject_beside_outside(source: String, expected: TyckDiagnosticCode) {
        SourceCase::assert_rejected(
            CbvCase::check(&format!("let outside = 7 in\n{source}")),
            expected,
        );
    }
}

/* ------------------------------- the clauses ------------------------------ */

#[test]
fn translates_a_literal() {
    CbvCase::accept("@[cbv] (5)");
}

#[test]
fn translates_unit() {
    CbvCase::accept("@[cbv] (())");
}

#[test]
fn translates_the_identity_function() {
    CbvCase::accept("@[cbv] (fn (x : Int) => x)");
}

#[test]
fn translates_a_wildcard_binder() {
    CbvCase::accept("@[cbv] (fn (_ : Int) => 7)");
}

#[test]
fn translates_a_nested_arrow_domain() {
    CbvCase::accept("@[cbv] (fn (f : Int -> Int) => f)");
}

#[test]
fn translates_an_application() {
    CbvCase::accept("@[cbv] ((fn (x : Int) => x) 7)");
}

#[test]
fn translates_a_higher_order_application() {
    CbvCase::accept("@[cbv] ((fn (f : Int -> Int) => f) (fn (y : Int) => y))");
}

#[test]
fn translates_a_variable_bound_by_an_enclosing_lambda() {
    CbvCase::accept("@[cbv] (fn (x : Int) => fn (y : Int) => x)");
}

/// The block's own type is the term translation's, so a value body still
/// produces a returner. Checking it in a computation position is what the
/// accepted cases above already assert; this one states the type outright.
#[test]
fn a_value_body_still_produces_a_returner() {
    SourceCase::assert_accepted(CbvCase::check_invariants(
        "do case_result <- (@[cbv] (fn (x : Int) => x) : Ret (Thk (Int -> Ret Int)));\nret ()",
    ));
}

/* ---------------------------- the source types ---------------------------- */

/// A name the block binds for a primitive is the primitive.
#[test]
fn admits_an_alias_of_a_primitive_source_type() {
    CbvCase::accept("@[cbv] (let MyInt : VType = Int that fn (x : MyInt) => x)");
}

/// A name bound outside the block is not the block's to name, even for a type
/// the block could write itself: an outside type enters by type abstraction.
/// While blocks were open over types this was admitted, because the name
/// resolved to a type in the translation's image.
#[test]
fn rejects_a_type_name_bound_outside_the_block() {
    SourceCase::assert_rejected(
        CbvCase::check(
            "let MyInt = Int in\ndo case_result <- @[cbv] (fn (x : MyInt) => x);\nret ()",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

/// The built-ins are no exception: `Int` named outside is a host name like any
/// other. Every other block in this file binds them inside, by
/// [`CbvBlock::closed`].
#[test]
fn rejects_a_built_in_type_named_outside_the_block() {
    SourceCase::assert_rejected(
        SourceCase::check("do case_result <- @[cbv] (fn (x : Int) => x);\nret ()"),
        TyckDiagnosticCode::Expressivity,
    );
    SourceCase::assert_rejected(
        SourceCase::check(
            "do case_result <- @[cbv] (let Int : VType = @(intrinsic(int)) that fn (x : Int) => x);\nret ()",
        ),
        TyckDiagnosticCode::Expressivity,
    );
    SourceCase::assert_accepted(SourceCase::check_invariants(
        "do case_result <- @[cbv] (fn (x : @(intrinsic(int))) => x);\nret ()",
    ));
}

/// A refused outside name is reported at the name, whichever judgment asked.
/// Raised where the kind judgment asked about a binder's annotation, before
/// anything had established a site for it, the refusal pointed at whatever the
/// caller was checking: for a definition's parameter, the declaration's body.
#[test]
fn reports_an_outside_name_at_the_name() {
    SourceCase::assert_rejected_at(
        SourceCase::check(
            "let OutsideKind = @(intrinsic(vtype)) in
do case_result <- @[cbv] (
  let V = @(intrinsic(vtype)) in
  def D (X : OutsideKind) : V = data | +Mk : X end
  in 0);
ret ()",
        ),
        TyckDiagnosticCode::Expressivity,
        "OutsideKind",
    );
    SourceCase::assert_rejected_at(
        SourceCase::check(
            "let HostI = @(intrinsic(int)) in
do case_result <- @[cbv] (fn (x : HostI) => x);
ret ()",
        ),
        TyckDiagnosticCode::Expressivity,
        "HostI",
    );
    SourceCase::assert_rejected_at(
        CbvCase::check(
            "let HostI = Int in
do case_result <- @[cbv] (let Id : VType -> VType = fn (X : VType) => X that fn (x : Id HostI) => x);
ret ()",
        ),
        TyckDiagnosticCode::Expressivity,
        "HostI",
    );
    // the same definition at a kind the block binds
    CbvCase::accept(
        "@[cbv] (let V = @(intrinsic(vtype)) in def D (X : V) : V = data | +Mk : X end in 0)",
    );
}

/// A block binds a kind as the host does, `let K = VType -> VType`, and uses it
/// wherever it writes a kind; the bindee decides, since a kind has no
/// classifier. A kind binding may name another kind the block bound.
#[test]
fn binds_a_kind_in_the_block() {
    CbvCase::accept("@[cbv] (let K = VType -> VType that fn (F : K) => fn (x : F Int) => x)");
    CbvCase::accept(
        "@[cbv] (let J = VType that let Id : J -> J = fn (X : J) => X that fn (x : Id Int) => x)",
    );
    CbvCase::accept("@[cbv] (let K = @(intrinsic(vtype)) that fn (X : K) => fn (x : X) => x)");
}

/// A kind named outside is a host name like a type is, so a kind binding cannot
/// launder one, and an intrinsic stands only for a source primitive or a kind.
#[test]
fn rejects_a_kind_named_outside_and_an_intrinsic_that_is_no_primitive() {
    SourceCase::assert_rejected(
        SourceCase::check("do case_result <- @[cbv] (let K = VType that 0);\nret ()"),
        TyckDiagnosticCode::Expressivity,
    );
    SourceCase::assert_rejected(
        SourceCase::check("do case_result <- @[cbv] (fn (X : VType) => 0);\nret ()"),
        TyckDiagnosticCode::Expressivity,
    );
    CbvCase::reject("@[cbv] (fn (x : @(intrinsic(ret))) => 0)", TyckDiagnosticCode::Expressivity);
}

/// A shift is no source type. Written with intrinsics it reaches nothing
/// outside the block, so the rule that refuses it is that a block's
/// intrinsics are its primitives; written with the host's names, as below, it
/// is refused before that, as names the block did not bind.
#[test]
fn rejects_a_shift_written_with_intrinsics() {
    CbvCase::reject(
        "@[cbv] (fn (x : (@(intrinsic(thk))) ((@(intrinsic(ret))) Int)) => x)",
        TyckDiagnosticCode::Expressivity,
    );
}

#[test]
fn rejects_a_shift_named_outside() {
    CbvCase::reject("@[cbv] (fn (x : Thk (Ret Int)) => x)", TyckDiagnosticCode::Expressivity);
}

/// A classifier must be a *name*. Admitting any other shape means handing a
/// subtree to ordinary synthesis, which defeats both restrictions this version
/// claims: an annotation wrapper smuggled an import past the import rule, and a
/// classifier expression read a term variable from outside past the closedness
/// rule. Both produced a primitive, so the resolved-type test admitted them and
/// the arena invariants had nothing to object to — the emitted term was well
/// formed and only its provenance was forbidden.
#[test]
fn rejects_an_import_wrapped_in_an_annotation() {
    SourceCase::assert_rejected(
        CbvCase::check_with_import(
            "do case_result <- @[cbv] (fn (x : (@(import(\"imported.zy\")) : VType)) => x);
ret ()",
            "@(intrinsic(int))",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

#[test]
fn rejects_a_classifier_expression_reading_an_outside_variable() {
    SourceCase::assert_rejected(
        CbvCase::check(
            "let outside = 42 in
do case_result <- @[cbv] (fn (x : (@[typeof] outside)) => x);
ret ()",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

/// Two spellings of one type must not get opposite verdicts. An earlier form of
/// the classifier rule admitted only a bare name, which rejected this while
/// accepting the same type bound to a name first.
#[test]
fn admits_a_classifier_built_by_type_application() {
    CbvCase::accept(
        "@[cbv] (let Id : VType -> VType = fn (X : VType) => X that fn (x : Id Int) => x)",
    );
}

/// The shape test is a provenance guard, so it has to hold under nesting: the
/// forbidden forms are refused inside an application or an ascription too, not
/// only at the classifier's root.
#[test]
fn rejects_a_classifier_expression_nested_in_an_application() {
    CbvCase::reject_beside_outside(
        CbvCase::block(
            "@[cbv] (let Id : VType -> VType = fn (X : VType) => X that
  fn (x : Id ((@[typeof] outside))) => x)",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

#[test]
fn rejects_an_import_nested_in_an_application() {
    SourceCase::assert_rejected(
        CbvCase::check_with_import(
            "do case_result <- @[cbv] (let Id : VType -> VType = fn (X : VType) => X that
  fn (x : Id (@(import(\"imported.zy\")))) => x);
ret ()",
            "@(intrinsic(int))",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

/// A function type's binder may name its parameter, since no source type
/// mentions a term: `pi (y : Int) . Int` is `Int -> Int`. A later binder or a
/// result that mentions the name is refused by sort, at any position of a
/// group.
#[test]
fn admits_a_named_arrow_binder_that_nothing_mentions() {
    CbvCase::accept("@[cbv] (fn (f : pi (y : Int) . Int) => f 1)");
    CbvCase::accept("@[cbv] (fn (f : pi (x : Int) (y : Int) . Int) => f 1 2)");
    for mention in ["pi (y : Int) . y", "pi (x : Int) (y : x) . Int", "pi (x : Int) (y : Int) . x"]
    {
        CbvCase::reject(
            &format!("@[cbv] (fn (f : {mention}) => 0)"),
            TyckDiagnosticCode::SortMismatch,
        );
    }
}

/// Below its outermost annotation, a telescope's binder is checked as a
/// parameter is: an annotation inside it is translated and compared with the
/// domain, and how deep an annotation sits decides nothing.
#[test]
fn checks_every_annotation_of_a_telescope_binder() {
    CbvCase::accept("@[cbv] (fn (f : pi ((_ : Int) : Int) . Int) => 0)");
    CbvCase::accept("@[cbv] (fn (f : pi ((y : Int) : Int) . Int) => 0)");
    CbvCase::reject(
        "@[cbv] (fn (f : pi ((_ : Unit) : Int) . Int) => 0)",
        TyckDiagnosticCode::TypeMismatch,
    );
}

/* ------------------------------- the binders ------------------------------ */

#[test]
fn rejects_an_unannotated_binder() {
    CbvCase::reject("@[cbv] (fn x => x)", TyckDiagnosticCode::Expressivity);
}

/// A parameter's type may come from the annotations inside it, when they give
/// the type of every part, as the host's synthesis reads them:
/// `fn ((x : Int), (y : Int)) => x` takes `Int * Int`, in any position of a
/// group. A part with no annotation would need inference, which a block does
/// not do, so it is refused as a bare parameter is; and each annotation is
/// translated and guarded as any is.
#[test]
fn takes_a_parameter_type_from_the_annotations_inside_it() {
    for accepted in [
        "(fn ((x : Int), (y : Int)) => x) (1, 2)",
        "(fn ((x : Int) : _) => x) 1",
        "(fn () => 0) ()",
        "(fn (z : Int) ((x : Int), (y : Int)) => z) 0 (1, 2)",
    ] {
        CbvCase::accept(&format!("@[cbv] ({accepted})"));
    }
    CbvCase::reject(
        "@[cbv] ((fn ((x : Int), (y : Int)) => x) (1, ()))",
        TyckDiagnosticCode::TypeMismatch,
    );
    for refused in ["fn ((x : Int), y) => x", "fn ((x : Int), _) => x", "fn (z : Int) (x, y) => z"]
    {
        CbvCase::reject(&format!("@[cbv] ({refused})"), TyckDiagnosticCode::Expressivity);
    }
    SourceCase::assert_rejected(
        SourceCase::check(&CbvBlock::closed(
            "let outside = 0 in
do case_result <- @[cbv] (fn ((x : @[typeof] outside), (y : Int)) => y);
ret ()",
        )),
        TyckDiagnosticCode::Expressivity,
    );
}

/// The ordinary abstraction rule rejects a refutable binder with
/// `tyck.refutable-binding`. Before the source binder shape was validated this
/// translation accepted it, and so emitted a term the ordinary rules forbid.
#[test]
fn rejects_a_refutable_binder() {
    CbvCase::reject("@[cbv] (fn (0 : Int) => 1)", TyckDiagnosticCode::Expressivity);
}

/* ----------------------------- the applications --------------------------- */

#[test]
fn rejects_applying_a_non_function() {
    CbvCase::reject("@[cbv] (1 2)", TyckDiagnosticCode::Expressivity);
}

#[test]
fn rejects_applying_a_function_to_the_wrong_type() {
    CbvCase::reject("@[cbv] ((fn (x : Int) => x) ())", TyckDiagnosticCode::TypeMismatch);
}

/* -------------------------------- the leaves ------------------------------ */

/// Reusing the ordinary literal judgment is what resolves a literal's
/// representation and checks its range; copying the parsed literal would skip
/// both.
#[test]
fn rejects_an_out_of_range_literal() {
    CbvCase::reject(
        "@[cbv] (99999999999999999999999)",
        TyckDiagnosticCode::IntegerLiteralOutOfRange,
    );
}

/* ------------------------- the closed-term restriction -------------------- */

/// A variable the block did not bind resolves successfully to an outer
/// definition, and would otherwise be used at its host type as though it were a
/// translated one.
#[test]
fn rejects_a_variable_bound_outside_the_block() {
    SourceCase::assert_rejected(
        CbvCase::check(
            "let outside = 42 in\ndo case_result <- @[cbv] (fn (x : Int) => outside);\nret ()",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

#[test]
fn rejects_an_outside_variable_as_an_argument() {
    SourceCase::assert_rejected(
        CbvCase::check(
            "let outside = 42 in\ndo case_result <- @[cbv] ((fn (x : Int) => x) outside);\nret ()",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

/// An imported term can be closed and still be call-by-push-value code from
/// another source, so the free-variable check does not catch it. By the time
/// the elaboration runs it is a source boundary and carries no metadata, so
/// this is the only place it can be rejected.
#[test]
fn rejects_an_import_in_term_position() {
    SourceCase::assert_rejected(
        CbvCase::check_with_import(
            "do case_result <- @[cbv] @(import(\"imported.zy\"));\nret ()",
            "ret 0",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

#[test]
fn rejects_an_import_in_a_binder_classifier() {
    SourceCase::assert_rejected(
        CbvCase::check_with_import(
            "do case_result <- @[cbv] (fn (x : @(import(\"imported.zy\"))) => x);\nret ()",
            "@(intrinsic(int))",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

/* -------------------------- forms outside the fragment -------------------- */

/// A thunk is call-by-push-value's own syntax, so a call-by-value source
/// cannot contain one. The case used to be a tuple, which products made
/// admissible without this test being revisited.
#[test]
fn rejects_a_source_form_outside_the_fragment() {
    CbvCase::reject("@[cbv] ({ 5 })", TyckDiagnosticCode::Expressivity);
}

/* --------------------------- bindings and products ------------------------ */

#[test]
fn translates_a_let() {
    CbvCase::accept("@[cbv] (let y = 5 in y)");
}

/// A binding's bindee is wrapped in an annotation by desugaring whether or not
/// the source wrote one, so handling that wrapper is structural. An authored
/// classifier goes through the type translation and is compared with what the
/// bindee synthesized.
#[test]
fn translates_an_annotated_let() {
    CbvCase::accept("@[cbv] (let y : Int = 5 in y)");
}

#[test]
fn rejects_a_let_whose_annotation_disagrees() {
    CbvCase::reject("@[cbv] (let y : Unit = 5 in y)", TyckDiagnosticCode::TypeMismatch);
}

#[test]
fn translates_a_let_over_an_application() {
    CbvCase::accept("@[cbv] (let y = (fn (x : Int) => x) 7 in y)");
}

#[test]
fn translates_a_tuple_of_values() {
    CbvCase::accept("@[cbv] (5, 6)");
}

/// A tuple whose components are not all values takes the term clause, which
/// sequences them left to right. This is the first source form to land in both
/// judgments.
#[test]
fn translates_a_tuple_that_sequences() {
    CbvCase::accept("@[cbv] ((fn (x : Int) => x) 7, 8)");
}

#[test]
fn translates_a_product_classifier() {
    CbvCase::accept("@[cbv] (fn (p : Int * Int) => p)");
}

/// A product binder is irrefutable when its components are, which is what the
/// ordinary abstraction rule asks, so the binder rule recurses rather than
/// admitting a second shape.
#[test]
fn translates_a_product_binder() {
    CbvCase::accept("@[cbv] (let (a, b) = (5, 6) in a)");
}

#[test]
fn rejects_a_refutable_component_in_a_product_binder() {
    CbvCase::reject("@[cbv] (let (a, 0) = (5, 6) in a)", TyckDiagnosticCode::Expressivity);
}

/* --------------------------- the binding boundary ------------------------- */

/// Name resolution lifts a `that` binding to the block that owns it and leaves
/// a residual where the body continues, so what the translation receives is an
/// elaborated chain rather than the binding the source wrote. For a block the
/// owning block is the one desugaring inserts around its payload.
#[test]
fn translates_a_that_binding() {
    CbvCase::accept("@[cbv] (let y = 5 that y)");
}

#[test]
fn translates_a_chain_of_that_bindings() {
    CbvCase::accept("@[cbv] (let p = 5 that let q = (fn (x : Int) => x) p that q)");
}

#[test]
fn translates_a_that_binding_with_a_product_binder() {
    CbvCase::accept("@[cbv] (let (m, n) = (5, 6) that n)");
}

/// The owning block is the nearest one, as in the host, so a `that` binding in a
/// lambda's body is lifted to the block's own level, where the lambda's
/// parameter is not in scope. The reference said such a binding is translated
/// where it was written.
#[test]
fn lifts_a_that_binding_out_of_a_lambda_body() {
    SourceCase::assert_resolve_error(CbvCase::check(&CbvCase::block(
        "@[cbv] (fn (x : Int) => let y = x that y)",
    )));
}

/// A `begin … end` around the body is a nearer block, and keeps it inside.
#[test]
fn keeps_a_that_binding_in_a_lambda_body_under_a_block() {
    CbvCase::accept("@[cbv] (let f = fn (x : Int) => begin let y = x that y end in f 2)");
}

/// Inside a block `param` is the call-by-value lambda, so the block returns a
/// thunk rather than becoming a function itself and its classifier is still
/// `Ret [[A]]v`.
#[test]
fn translates_a_param_as_a_call_by_value_lambda() {
    CbvCase::accept("@[cbv] (param x : Int that x)");
}

/// The boundary itself: before the inserted `Block`, a `that` written inside a
/// call-by-value block was owned by the *enclosing* block and its name stayed
/// in scope after the call-by-value block.
///
/// The enclosing block is written explicitly, and that is what makes the case
/// a witness. Without one, a `that` inside a transparent call-by-value block would be
/// rejected for having no boundary to belong to at all, which is a resolution
/// failure as well and would let this pass for the wrong reason.
#[test]
fn a_that_binding_does_not_escape_the_block() {
    SourceCase::assert_resolve_error(CbvCase::check(
        "begin
  do case_result <- @[cbv] (let y = 5 that 1);
  ret y
end",
    ));
}

/// A `that` bindee is resolved in the block's scope, which includes the names
/// around the block, so the closed-term restriction has to reach into the
/// elaborated chain as well.
#[test]
fn rejects_a_that_bindee_reading_an_outside_variable() {
    SourceCase::assert_rejected(
        CbvCase::check(
            "let outside = 42 in
do case_result <- @[cbv] (let y = outside that y);
ret ()",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

/// A binding whose bindee mentions the name it binds elaborates to a recursive
/// group instead of a `Let`. A group of values is refused as the host refuses
/// it, with a diagnostic that directs the author to `let fix`.
#[test]
fn rejects_a_recursive_that_binding() {
    CbvCase::reject(
        "@[cbv] (let f = fn (x : Int) => f x that f 3)",
        TyckDiagnosticCode::InvalidBindingCycle,
    );
}

/// A value parameter binds a type, and quantifiers are outside this version's
/// fragment.
#[test]
fn rejects_a_value_param() {
    CbvCase::reject("@[cbv] (param val X : VType that 5)", TyckDiagnosticCode::Expressivity);
}

/* ----------------------- what the fifth review found ---------------------- */

/// Review's input, kept. The classifier guard split a kind binder into its
/// outermost annotation and discarded the rest, and the ordinary pattern
/// checker then descended into what had been discarded. So this read a term
/// variable bound outside the block, defeating the closedness rule, and still
/// produced a well-formed core term — which is why the arena invariants passed it.
#[test]
fn rejects_a_classifier_expression_inside_a_kind_binder() {
    SourceCase::assert_rejected(
        CbvCase::check(
            "let outside = 5 in
let Id = fn (T : VType) => T in
do case_result <- @[cbv] (fn (x : ((Id : pi ((X : @[typeof] (@[typeof] outside)) : VType) . VType) Int)) => x);
ret ()",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

/// The same hole with an import in it, which defeated the import rule the same
/// way.
#[test]
fn rejects_an_import_inside_a_kind_binder() {
    SourceCase::assert_rejected(
        CbvCase::check_with_import(
            "let Id = fn (T : VType) => T in
do case_result <- @[cbv] (fn (x : ((Id : pi ((X : @(import(\"imported.zy\"))) : VType) . VType) Int)) => x);
ret ()",
            "@(intrinsic(vtype))",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

/// An ascription around a type the block already admits. The leaf rule used
/// to hand the whole ascription to ordinary synthesis, where a source arrow is
/// not a host type, so the unascribed spelling was accepted and this one came
/// back with a host diagnostic about `val pi`.
#[test]
fn admits_an_ascribed_product_source_type() {
    CbvCase::accept("@[cbv] (fn (x : ((Int * Int) : VType)) => x)");
}

#[test]
fn admits_an_ascribed_arrow_source_type() {
    CbvCase::accept("@[cbv] (fn (x : ((Int -> Int) : VType)) => x)");
}

/// A type operator's argument is a source type and is translated. Before, the
/// whole application went to ordinary synthesis, so `Id Pair` was admitted
/// with `Pair = Int * Int` while `Id (Int * Int)` was not.
#[test]
fn admits_a_composite_argument_to_a_type_operator() {
    CbvCase::accept(
        "@[cbv] (let Id : VType -> VType = fn (X : VType) => X that fn (x : Id (Int * Int)) => x)",
    );
}

/// An operator cannot launder a type the block may not write: its body is a
/// source type and is translated as one, so a shift is refused where it is
/// written. While an operator could be named outside, the applied result was
/// tested for membership in the image instead.
#[test]
fn rejects_a_type_operator_aliasing_a_shift() {
    CbvCase::reject(
        "@[cbv] (let Launder : VType -> VType = fn (X : VType) => (@(intrinsic(thk))) ((@(intrinsic(ret))) X) that
  fn (x : Launder Int) => x)",
        TyckDiagnosticCode::Expressivity,
    );
}

/// A source program's **width** must not cost the elaboration a stack frame
/// per component. The sequencing clause used to spend one, and review's input
/// — a flat tuple of twelve thousand, with no nesting in it at all — aborted
/// `check` with a stack overflow.
///
/// The width here is far below that, because the witness and the regression
/// test cannot be the same program. A tuple of width *n* translates to *n*
/// nested `do`s, and every recursive pass over the finished term costs depth
/// *n* — which a `cargo test` thread, with a smaller stack than the main
/// thread and debug-sized frames, cannot carry at twelve thousand. Neither can
/// it carry a *hand-written* chain of that depth, so what remains is the
/// host's limit on nesting and not this translation's on width.
///
/// The witness is therefore a command, not a case: `zydeco check` on a tuple
/// of twelve thousand aborted before `e35303d6` and passes at a hundred
/// thousand after it. This case only guards against the frame-per-component
/// shape coming back.
#[test]
fn translates_a_wide_tuple() {
    let mut components = vec!["(let x = 0 in x)".to_string()];
    components.extend((0..1_999).map(|_| "1".to_string()));
    CbvCase::accept(&format!("@[cbv] ({})", components.join(", ")));
}

/* -------------------------- the analysis direction ------------------------ */

/// A literal's representation is resolved when it is checked, so an authored
/// classifier has to reach the literal. Synthesizing `Int` and comparing
/// afterwards rejected this, which the host accepts — the direction fixes a
/// defect in the fragment as it stands, before any of the formers it gates.
#[test]
fn checks_a_literal_against_an_authored_classifier() {
    CbvCase::accept("@[cbv] ((5 : Int64))");
}

/// An abstraction may take its domain from the expectation rather than from an
/// annotation. Paired with `rejects_an_unannotated_binder`, which is the same
/// program with nothing to take it from.
#[test]
fn checks_an_unannotated_binder_against_an_authored_arrow() {
    CbvCase::accept("@[cbv] ((fn x => x) : Int -> Int)");
}

#[test]
fn pushes_an_expectation_into_a_product() {
    CbvCase::accept("@[cbv] ((fn x => x, 2) : (Int -> Int) * Int64)");
}

#[test]
fn pushes_an_expectation_through_a_binding() {
    CbvCase::accept("@[cbv] ((let y = 5 in fn x => x) : Int -> Int)");
}

/// The expectation is checked even when no clause could use it: an abstraction
/// looks for an arrow, finds none here, and must not then synthesize
/// unchecked.
#[test]
fn rejects_an_expectation_no_clause_could_use() {
    CbvCase::reject("@[cbv] ((fn (x : Int) => x) : Int)", TyckDiagnosticCode::TypeMismatch);
}

#[test]
fn rejects_a_binder_annotation_disagreeing_with_the_expectation() {
    CbvCase::reject(
        "@[cbv] ((fn (x : Unit) => x) : Int -> Unit)",
        TyckDiagnosticCode::TypeMismatch,
    );
}

/// The expectation describes an application's result, not its function, and is
/// not turned into one: recovering a function's type from its argument and its
/// result is inference rather than analysis.
#[test]
fn rejects_an_unannotated_binder_under_an_application() {
    CbvCase::reject("@[cbv] (((fn x => x) 5) : Int)", TyckDiagnosticCode::Expressivity);
}

/// The decision that analysis stops at the boundary, stated as a program. The
/// ascription here is the *host's*, outside the block, so it is reconciled
/// with what the block synthesized and never reaches the binder — where the
/// same ascription written inside the block does.
#[test]
fn a_host_expectation_does_not_reach_into_a_block() {
    SourceCase::assert_rejected(
        CbvCase::check(
            "do case_result <- (@[cbv] (fn x => x) : Ret (Thk (Int -> Ret Int)));\nret ()",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

/* ------------------------ the image of the translation -------------------- */

/// Four spellings of one source type, which the provenance test that preceded
/// membership split two against two: it refused every `Thk`, so a block-bound
/// arrow name needed an exemption and the same name under an operator was still
/// refused. A fifth, a host name for the arrow's translation, was admitted by
/// membership until blocks were closed over types.
#[test]
fn admits_every_spelling_of_one_arrow_type() {
    CbvCase::accept(
        "@[cbv] (let Id : VType -> VType = fn (X : VType) => X that
  let Fn : VType = Int -> Int that
  let a : Int -> Int = fn (p : Int) => p that
  let b : Fn = fn (q : Int) => q that
  let c : Id Fn = fn (r : Int) => r that
  let d : Id (Int -> Int) = fn (s : Int) => s that
  d 7)",
    );
}

#[test]
fn admits_an_arrow_inside_a_product_under_an_operator() {
    CbvCase::accept(
        "@[cbv] (let Id : VType -> VType = fn (X : VType) => X that
  fn (x : Id (Int * (Int -> Int))) => x)",
    );
}

#[test]
fn rejects_a_returner_as_a_source_type() {
    CbvCase::reject(
        "@[cbv] (fn (x : (@(intrinsic(ret))) Int) => x)",
        TyckDiagnosticCode::Expressivity,
    );
}

/// The translation of `Int -> Int` written out in call-by-push-value syntax:
/// refused, because an operator and its argument are translated as source types,
/// and neither the shift nor the returner is one.
#[test]
fn rejects_a_translated_arrow_written_out() {
    CbvCase::reject(
        "@[cbv] (fn (x : (@(intrinsic(thk))) (Int -> (@(intrinsic(ret))) Int)) => x)",
        TyckDiagnosticCode::Expressivity,
    );
}

/* ------------------------------ type bindings ----------------------------- */

/// A block can name a source type. The declared kind is what tells the
/// translation which judgment the bindee belongs to, since the host's way of
/// deciding — synthesize it and read off the sort — is not available on source
/// syntax.
#[test]
fn translates_a_type_binding() {
    CbvCase::accept("@[cbv] (let MyInt : VType = Int that fn (x : MyInt) => x)");
}

/// The one way to name a source arrow: `Int -> Int` is ill-kinded in the host,
/// so no host binding can stand for it. The name denotes
/// `Thk (Int -> Ret Int)`, which the leaf rule has to admit although it
/// refuses a `Thk` the author wrote.
#[test]
fn translates_an_arrow_type_binding() {
    CbvCase::accept(
        "@[cbv] (let Fn : VType = Int -> Int that let g : Fn = fn (x : Int) => x in g 7)",
    );
}

#[test]
fn translates_a_product_type_binding() {
    CbvCase::accept("@[cbv] (let P : VType = Int * Int that fn (p : P) => p)");
}

#[test]
fn translates_a_chain_of_type_bindings() {
    CbvCase::accept("@[cbv] (let A : VType = Int that let B : VType = A * A that fn (x : B) => x)");
}

/// A call-by-push-value type cannot be bound inside a block and used from
/// there: the bindee is a source type, and a shift is not one.
#[test]
fn rejects_a_type_binding_to_a_host_type() {
    CbvCase::reject(
        "@[cbv] (let F : VType = (@(intrinsic(thk))) ((@(intrinsic(ret))) Int) that fn (x : F) => x)",
        TyckDiagnosticCode::Expressivity,
    );
}

/// Without a declared kind the binding is a term binding, and a type name in
/// term position is a reference outside the block. Both rejections are right;
/// the diagnostic says which one the author ran into.
#[test]
fn rejects_a_type_binding_without_its_kind() {
    CbvCase::reject(
        "@[cbv] (let MyInt = Int that fn (x : MyInt) => x)",
        TyckDiagnosticCode::Expressivity,
    );
}

#[test]
fn rejects_a_type_binding_whose_kind_disagrees() {
    CbvCase::reject("@[cbv] (let C : CType = Int that 5)", TyckDiagnosticCode::KindMismatch);
}

/// The host tells a value cycle from a type one by syntax, and `D * Int` is
/// written with the tuple's node, so it reads a value body and refuses the
/// cycle. A block gives the host's verdict.
#[test]
fn rejects_a_recursive_sealed_product_as_the_host_does() {
    CbvCase::reject(
        "@[cbv] (def D : VType = D * Int that 5)",
        TyckDiagnosticCode::InvalidBindingCycle,
    );
}

/// A returned function nests a `Pi` in the telescope, and so does a second
/// parameter. The binder tells them apart: a source arrow is `pi (_ : A) . B`
/// over a hole. Refusing every `Pi` confused the two.
#[test]
fn translates_a_recursive_binding_returning_a_function() {
    CbvCase::accept("@[cbv] (let fix f (x : Int) : Int -> Int = fn (y : Int) => y in (f 0) 7)");
}

/// Desugaring freshens one pattern into both the parameter and the telescope's
/// binder, so a real header has them equal and comparing them is a tautology.
/// It is not a tautology for the `let !` spelling, which produces the same
/// shape — and before this the signature was read from one copy and the other
/// ignored, so a declared domain that disagreed with its own parameter was
/// accepted.
#[test]
fn rejects_a_recursive_signature_disagreeing_with_its_parameter() {
    CbvCase::reject(
        "@[cbv] (let ! f : pi (x : Int * Int) . Int = fix g => fn (x : Int) => x in f 1)",
        TyckDiagnosticCode::TypeMismatch,
    );
}

/// A function that **does** call itself and stops, with no comparison
/// anywhere: a higher-order argument decides whether to recur. The earlier
/// terminating case only built a `fix` without using it.
#[test]
fn a_recursive_function_that_recurs_and_stops_runs() {
    SourceCase::assert_accepted(CbvCase::run(
        "do case_result <- @[cbv] (
  let fix f (k : (Int -> Int) -> Int) : Int =
    k (fn (n : Int) => f (fn (r : Int -> Int) => n))
  in f (fn (r : Int -> Int) => r 0)
);
! exit case_result",
    ));
}

/* ----------------------------- data and match ----------------------------- */

/// A nominal type declared outside the block is not the block's to name; the
/// image test used to admit it by unfolding the nominal and testing its arms. A
/// block declares its own, as the tests below do.
#[test]
fn rejects_a_data_type_declared_outside() {
    SourceCase::assert_rejected(
        CbvCase::check(
            "begin
  def Flag : VType = data | +Off : Unit | +On : Int end that
  do case_result <- @[cbv] (fn (x : Flag) => x);
  ret ()
end",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

#[test]
fn admits_a_data_type_declared_in_the_block() {
    CbvCase::accept_with_data("@[cbv] (fn (x : Flag) => x)");
}

/// A constructor cannot synthesize — nothing about `+On` says which
/// declaration it belongs to — so the expectation names the type. This is the
/// first row that *requires* the analysis direction.
#[test]
fn translates_a_constructor() {
    CbvCase::accept_with_data("@[cbv] ((+On(7) : Flag))");
}

#[test]
fn rejects_a_constructor_without_an_expected_type() {
    CbvCase::reject_with_data("@[cbv] (+On(7))", TyckDiagnosticCode::MissingAnnotation);
}

/// The function's domain is pushed into the argument, so a constructor
/// argument needs no annotation of its own.
#[test]
fn pushes_an_application_domain_into_a_constructor_argument() {
    CbvCase::accept_with_data("@[cbv] ((fn (x : Flag) => 0) (+On(7)))");
}

#[test]
fn translates_a_match() {
    CbvCase::accept_with_data("@[cbv] (fn (x : Flag) => match x | +Off(_) => 0 | +On(n) => n end)");
}

/// An empty match has no arm to synthesize from.
#[test]
fn rejects_an_empty_match_without_an_expected_type() {
    CbvCase::reject_with_data("@[cbv] (match 5 end)", TyckDiagnosticCode::MissingAnnotation);
}

/// An arm's binder is the one refutable shape the source grammar has: a
/// constructor tag over a plain binder. A literal inside it is not.
#[test]
fn rejects_a_refutable_arm_payload() {
    CbvCase::reject_with_data(
        "@[cbv] (fn (x : Flag) => match x | +On(0) => 0 | _ => 1 end)",
        TyckDiagnosticCode::Expressivity,
    );
}

/// `data` closes the loop that recursion opened: a decreasing structure makes
/// a terminating recursive function writable with nothing but what the block
/// already has — no arithmetic, and so no boundary coercion.
#[test]
fn a_structurally_recursive_function_computes() {
    SourceCase::assert_accepted(CbvCase::run(&CbvCase::within(
        &DATA,
        "do case_result <- @[cbv] (
  let fix parity (n : Nat) : Int =
    match n
    | +Zero(_) => 0
    | +Succ(m) => match m | +Zero(_) => 1 | +Succ(k) => parity k end
    end
  in parity (+Succ(+Succ(+Succ(+Succ(+Zero())))))
);
! exit case_result",
    )));
}

/* ------------------------- type binders in the source --------------------- */

/// `[[Λ(X : K) . M]]v = { fn (X : K) => [[M]]v_term }`. Written `fn (X : K)
/// => M`, the same `fn` as a value lambda: the binder's **sort** distinguishes
/// them, which is how the host reads `pi` too.
///
/// The clause **synthesizes**. The table marked this row as needing the
/// expected type on the strength of `{ fn X => ret 0 }` failing, but that is
/// the kind being absent, not the expectation being needed.
#[test]
fn translates_a_type_abstraction_and_uses_it() {
    CbvCase::accept("@[cbv] (let id = fn (X : VType) => fn (x : X) => x in (id Int) 7)");
}

#[test]
fn synthesizes_a_type_abstraction_alone() {
    CbvCase::accept("@[cbv] (fn (X : VType) => fn (x : X) => x)");
}

/// `[[X]]v = X` holds for the block's own binder, which the scope tracks: a
/// skolem says nothing about where it came from, and a host opaque type is one
/// too.
#[test]
fn a_type_variable_does_not_escape_its_binder() {
    SourceCase::assert_resolve_error(CbvCase::check(&CbvCase::block(
        "@[cbv] (let id = fn (X : VType) => fn (x : X) => x in fn (y : X) => y)",
    )));
}

#[test]
fn a_type_binder_whose_body_ignores_it() {
    CbvCase::accept("@[cbv] (fn (X : VType) => fn (x : Int) => x)");
}

/// The whole point of the machinery: defined inside the block, instantiated
/// inside it, and run.
#[test]
fn a_block_internal_polymorphic_function_runs() {
    SourceCase::assert_accepted(CbvCase::run(
        "do case_result <- @[cbv] (let id = fn (X : VType) => fn (x : X) => x in (id Int) 0);
! exit case_result",
    ));
}

/* --------------------------- polymorphic types ----------------------------- */

/// Instantiation and application are the same clause with a different thing
/// pushed on the stack, and the **function's** type says which: the host
/// spells both as one application, so the argument's own syntax cannot decide
/// it.
#[test]
fn translates_an_instantiation_followed_by_an_application() {
    CbvCase::accept("@[cbv] (fn (f : forall (X : VType) . X -> X) => (f Int) 7)");
}

/// The type-application recipe reaches `unreachable!()` on a kind mismatch, so
/// the kinds are compared before it is invoked. The binder is over a source
/// kind and the *argument* is the operator, since a `forall` over `CType` is
/// refused at the classifier and can no longer reach this check.
#[test]
fn rejects_an_instantiation_whose_kind_disagrees() {
    CbvCase::reject(
        "@[cbv] (fn (f : forall (F : VType -> VType) . F Int) => f Int)",
        TyckDiagnosticCode::KindMismatch,
    );
}

/// Review's input, which overflowed the process stack while the image test
/// admitted a codata type named outside: the branch that unrolled a nominal to
/// find a codata never registered it, so a recursive declaration unrolled once
/// per field. A block no longer names an outside type, so the input is refused
/// at the name; a recursive codata type a block declares is unfolded by
/// nothing.
#[test]
fn refuses_a_recursive_codata_type_named_outside() {
    SourceCase::assert_rejected(
        CbvCase::check(&format!(
            "begin
  def Stream : CType = codata | .next : Ret (Thk Stream) end that
  let S = Thk Stream that
  {}
end",
            CbvCase::block("@[cbv] (fn (x : S) => x)")
        )),
        TyckDiagnosticCode::Expressivity,
    );
}

/// A recursive codata type the block declares, built by `let fix` and
/// observed twice.
#[test]
fn a_recursive_stream_is_built_and_observed() {
    SourceCase::assert_accepted(CbvCase::run(
        "do case_result <- @[cbv] (
  def Stream : VType = codata | .head : Int | .next : Stream end that
  let fix ones (n : Int) : Stream = comatch | .head => n | .next => ones n end in
  let s : Stream = ones 0 in
  let t : Stream = s.next in
  t.head
);
! exit case_result",
    ));
}

/// The source's types are all value types, so its kinds are `VType` and arrows
/// of those. A `forall` over `CType` has no source preimage.
#[test]
fn rejects_a_type_binder_whose_kind_is_not_a_source_kind() {
    CbvCase::reject(
        "@[cbv] (fn (f : forall (R : CType) . Int) => f)",
        TyckDiagnosticCode::Expressivity,
    );
}

/// A bound operator variable **applied**, `F Int` under `forall (F : VType ->
/// VType)`: the application translates as `F Int`, with the variable itself.
#[test]
fn admits_a_higher_kinded_type_variable_applied() {
    CbvCase::accept("@[cbv] (fn (f : forall (F : VType -> VType) . F Int) => f)");
}

/// An outside type enters by type abstraction: the block takes `O` and the two
/// operations it needs, and the host instantiates `O`. Inside, `O` is abstract,
/// so the host may instantiate it at a host `data` type or at `Thk (Ret Int)`,
/// which no source type translates to, and the block computes the same answer:
/// `0`, through the operation for a present payload, where the other gives `1`.
#[test]
fn a_block_abstracts_over_an_outside_type_and_runs_at_any_instance() {
    let block = "do f <- @[cbv] (
  fn (O : VType) =>
  fn (some : Int -> O) =>
  fn (caseO : O -> (Unit -> Int) -> (Int -> Int) -> Int) =>
    caseO (some 0) (fn (u : Unit) => 1) (fn (n : Int) => n));";
    SourceCase::assert_accepted(CbvCase::run(&format!(
        "begin
  def Opt : VType = data | +None : Unit | +Some : Int end that
  {block}
  do g <- ! f Opt;
  do h <- ! g {{ fn (n : Int) => ret +Some n }};
  do r <- ! h {{
    fn (o : Opt) => ret {{
      fn (none : Thk (Unit -> Ret Int)) => ret {{
        fn (some : Thk (Int -> Ret Int)) =>
          match o | +None u => ! none u | +Some n => ! some n end
      }}
    }}
  }};
  ! exit r
end"
    )));
    SourceCase::assert_accepted(CbvCase::run(&format!(
        "begin
  {block}
  do g <- ! f (Thk (Ret Int));
  do h <- ! g {{ fn (n : Int) => ret {{ ret n }} }};
  do r <- ! h {{
    fn (o : Thk (Ret Int)) => ret {{
      fn (none : Thk (Unit -> Ret Int)) => ret {{
        fn (some : Thk (Int -> Ret Int)) => do n <- ! o; ! some n
      }}
    }}
  }};
  ! exit r
end"
    )));
}

/// A polymorphic function crosses the boundary the way an operation does — as
/// the block's own parameter, at a type the block writes — and is
/// instantiated inside.
#[test]
fn a_polymorphic_parameter_is_instantiated_and_runs() {
    SourceCase::assert_accepted(CbvCase::run(
        "let Ident = Thk (forall (X : VType) . Ret (Thk (X -> Ret X))) in
let ident : Ident = { fn (X : VType) => ret { fn (x : X) => ret x } } in
do f <- @[cbv] (param (id : forall (X : VType) . X -> X) that (id Int) 0);
do case_result <- ! f ident;
! exit case_result",
    ));
}

/* ---------------------------- codata and comatch --------------------------- */

/// A codata type is **negative**, so its translation keeps the shift outside:
/// `[[codata {.di : Ai}]]v = Thk (codata {.di : Ret [[Ai]]v})`. A block meets
/// it as a thunk and has to look through that thunk before the declaration is
/// visible.
#[test]
fn translates_a_comatch_and_an_observation() {
    CbvCase::accept_with_codata(
        "@[cbv] (let p : Pair = comatch | .fst => 3 | .snd => 4 end in p.snd)",
    );
}

/// A comatch cannot synthesize: nothing in its arms says which declaration
/// they answer.
#[test]
fn rejects_a_comatch_without_an_expected_type() {
    CbvCase::reject_with_codata(
        "@[cbv] (comatch | .fst => 1 | .snd => 2 end)",
        TyckDiagnosticCode::MissingAnnotation,
    );
}

#[test]
fn rejects_an_unknown_destructor() {
    CbvCase::reject_with_codata(
        "@[cbv] (let p : Pair = comatch | .nope => 1 end in p.fst)",
        TyckDiagnosticCode::UnknownCoDataDestructor,
    );
}

/// An interleaved spine, `.at n`, is the same clause with its arguments pushed
/// into the copattern. Un-interleaving it into `.at => fn n => …` is
/// mechanical, and is deferred rather than guessed at.
#[test]
fn rejects_an_interleaved_copattern() {
    CbvCase::reject_with_codata(
        "@[cbv] (let p : Fn = comatch | .at (n : Int) => n end in 0)",
        TyckDiagnosticCode::Expressivity,
    );
}

#[test]
fn translates_an_un_interleaved_field_taking_an_argument() {
    CbvCase::accept_with_codata(
        "@[cbv] (let p : Fn = comatch | .at => fn (n : Int) => n end in (p.at) 1)",
    );
}

/// **The by-name corner, as a program.** A field runs when observed, so a
/// diverging field that is never observed never runs. Its counterpart, which
/// observes `.snd` instead, does not terminate and so cannot be a test — the
/// pair is recorded in the translation table.
#[test]
fn an_unobserved_field_never_runs() {
    SourceCase::assert_accepted(CbvCase::run(&CbvCase::within(
        &CODATA,
        "do case_result <- @[cbv] (
  let fix diverge (x : Int) : Int = diverge x in
  let p : Pair = comatch | .fst => 0 | .snd => diverge 1 end in
  p.fst
);
! exit case_result",
    )));
}

/* --------------------- data and codata written in a block ------------------ */

/// A block writes a `data` type in place and binds it with a kind:
/// `[[data {+Ci : Ai}]]v = data {+Ci : [[Ai]]v}`. The type is transparent, so the
/// block builds and matches its values with no declaration outside.
#[test]
fn translates_a_data_type_written_in_place() {
    CbvCase::accept(
        "@[cbv] (let Opt : VType = data | +None : Unit | +Some : Int end in
  match (+Some 4 : Opt) | +Some n => n | +None _ => 0 end)",
    );
}

/// The arms are source types, so one written as a call-by-push-value type is
/// refused.
#[test]
fn rejects_a_data_arm_written_as_a_call_by_push_value_type() {
    CbvCase::reject(
        "@[cbv] (let Bad : VType = data | +Mk : (@(intrinsic(thk))) ((@(intrinsic(ret))) Int) end in 0)",
        TyckDiagnosticCode::Expressivity,
    );
}

/// The match picks the arm the constructor built: `+None` would answer 1.
#[test]
fn a_data_type_written_in_place_computes() {
    SourceCase::assert_accepted(CbvCase::run(
        "do case_result <- @[cbv] (
  let Opt : VType = data | +None : Unit | +Some : Int end in
  match (+Some 0 : Opt) | +Some n => n | +None _ => 1 end
);
! exit case_result",
    ));
}

/// Transparent data is compared by structure, so a host value of the same
/// declaration is a value of the block's type, and the block can match on it.
#[test]
fn a_host_value_meets_the_same_data_type_written_in_place() {
    SourceCase::assert_accepted(CbvCase::run(
        "let Opt = data | +None : Unit | +Some : Int end in
do f <- @[cbv] (
  let O : VType = data | +None : Unit | +Some : Int end in
  fn (o : O) => match o | +Some n => n | +None _ => 1 end
);
let v : Opt = +Some 0 in
do case_result <- ! f v;
! exit case_result",
    ));
}

/// A host declaration whose arm differs is a different type.
#[test]
fn rejects_a_host_value_of_a_different_data_type() {
    SourceCase::assert_rejected(
        CbvCase::check(
            "let Opt = data | +None : Unit | +Some : Unit end in
do f <- @[cbv] (
  let O : VType = data | +None : Unit | +Some : Int end in
  fn (o : O) => match o | +Some n => n | +None _ => 1 end
);
let v : Opt = +Some () in
do case_result <- ! f v;
ret ()",
        ),
        TyckDiagnosticCode::TypeMismatch,
    );
}

/// A `let` binding is not recursive, so a declaration that mentions itself is
/// an unbound name; recursion needs a sealed definition.
#[test]
fn rejects_a_data_type_that_mentions_itself_through_let() {
    SourceCase::assert_resolve_error(CbvCase::check(&CbvCase::block(
        "@[cbv] (let N : VType = data | +Z : Unit | +S : N end in 0)",
    )));
}

/// A codata type written in place keeps the shift outside and gives every
/// field a returner: `[[codata {.di : Ai}]]v = Thk (codata {.di : Ret [[Ai]]v})`.
/// Observing `.fst` answers 0, and `.snd` would answer 1.
#[test]
fn a_codata_type_written_in_place_computes() {
    SourceCase::assert_accepted(CbvCase::run(
        "do case_result <- @[cbv] (
  let P : VType = codata | .fst : Int | .snd : Int end in
  let p : P = comatch | .fst => 0 | .snd => 1 end in
  p.fst
);
! exit case_result",
    ));
}

/// A field is a source type, so one written as a returner is refused: the
/// translation supplies the `Ret`.
#[test]
fn rejects_a_codata_field_written_as_a_returner() {
    CbvCase::reject(
        "@[cbv] (let Bad : VType = codata | .f : (@(intrinsic(ret))) Int end in 0)",
        TyckDiagnosticCode::Expressivity,
    );
}

#[test]
fn a_codata_field_taking_an_argument_written_in_place_computes() {
    SourceCase::assert_accepted(CbvCase::run(
        "do case_result <- @[cbv] (
  let F : VType = codata | .at : Int -> Int end in
  let p : F = comatch | .at => fn (n : Int) => n end in
  (p.at) 0
);
! exit case_result",
    ));
}

/// The desugarer wraps `codata … end` in a `CType` ascription for the host,
/// which is not the source kind: a source codata type is a value type. That
/// marker is passed over, while an ascription the author writes is checked.
#[test]
fn checks_an_ascription_written_around_a_codata_type() {
    CbvCase::accept(
        "@[cbv] (let P : VType = (codata | .fst : Int end : VType) in
  let p : P = comatch | .fst => 2 end in p.fst)",
    );
    CbvCase::reject(
        "@[cbv] (let P : VType = (codata | .fst : Int end : CType) in 0)",
        TyckDiagnosticCode::KindMismatch,
    );
}

/* -------------------------- sealed declarations --------------------------- */

// `def X : K = D` in a block, for `D` a `data` or `codata` declaration under
// optional type parameters: a seal over `D`'s translation, nominal as the
// host's is, and recursive when `D` mentions `X`. A `codata` seal sits on the
// computation type inside the thunk, and `X` stands for `Thk` of it.

impl CbvCase {
    /// Runs `body` as a block and exits with its result, which must be `0`.
    fn run_block(body: &str) {
        SourceCase::assert_accepted(CbvCase::run(&format!(
            "do case_result <- @[cbv] (
  {body}
);
! exit case_result"
        )));
    }
}

const BLOCK_OPT: &str = "def Opt : VType = data | +None : Unit | +Some : Int end that";

#[test]
fn a_sealed_data_type_declared_in_place_computes() {
    CbvCase::run_block(&format!(
        "{BLOCK_OPT} match (+Some 0 : Opt) | +Some n => n | +None _ => 1 end"
    ));
}

/// A seal is nominal: two declarations of one shape are two types, where the
/// transparent `let` declarations of one shape above are one.
#[test]
fn rejects_one_sealed_declaration_at_another_of_the_same_shape() {
    CbvCase::reject(
        "@[cbv] (def A : VType = data | +Mk : Int end that def B : VType = data | +Mk : Int end that
  let a : A = +Mk 1 in let b : B = a in 0)",
        TyckDiagnosticCode::TypeMismatch,
    );
}

/// And a host's declaration of the same shape is another type again.
#[test]
fn rejects_a_host_value_at_a_sealed_type_declared_in_place() {
    CbvCase::reject_with_data(
        "@[cbv] (def Flag2 : VType = data | +Off : Unit | +On : Int end that
  fn (f : Flag) => let g : Flag2 = f in 0)",
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn a_recursive_data_type_declared_in_place_computes() {
    CbvCase::run_block(
        "def Nat : VType = data | +Z : Unit | +S : Nat end that
  match (+S (+Z ()) : Nat) | +Z _ => 1 | +S m => match m | +Z _ => 0 | +S _ => 1 end end",
    );
}

/// A `let` declaration is transparent and cannot mention itself: written with
/// `that`, so that it could, it is refused for the seal it lacks, as the
/// host refuses it.
#[test]
fn rejects_a_transparent_declaration_that_mentions_itself() {
    CbvCase::reject(
        "@[cbv] (let N : VType = data | +Z : Unit | +S : N end that 0)",
        TyckDiagnosticCode::MissingSeal,
    );
}

/// A parameterized recursive family, `std`'s `List` written in the block.
#[test]
fn a_parameterized_recursive_data_type_declared_in_place_computes() {
    CbvCase::run_block(
        "def List (X : VType) : VType = data | +Nil : Unit | +Cons : X * List X end that
  match (+Cons (0, +Nil ()) : List Int) | +Nil _ => 1 | +Cons c => let (h, t) = c in h end",
    );
}

/// The kind has to be known before the group is, as the host requires: with
/// parameters and no `: K`, the desugared definition carries none.
#[test]
fn rejects_a_recursive_declaration_without_its_kind() {
    CbvCase::reject(
        "@[cbv] (def List (X : VType) = data | +Nil : Unit | +Cons : X * List X end that 0)",
        TyckDiagnosticCode::InvalidBindingCycle,
    );
}

/// A nested family: the recursive mention is at another argument,
/// `Nest (X * X)`, which the seal's generic definition admits.
#[test]
fn a_nested_data_type_declared_in_place_computes() {
    CbvCase::run_block(
        "def Nest (X : VType) : VType = data | +Leaf : X | +Node : Nest (X * X) end that
  match (+Node (+Leaf (1, 0)) : Nest Int)
  | +Leaf x => x
  | +Node n => match n | +Leaf p => let (a, b) = p in b | +Node _ => 1 end
  end",
    );
}

/// The arms are source types under a seal as well.
#[test]
fn rejects_a_sealed_arm_outside_the_image() {
    CbvCase::reject(
        "@[cbv] (def Bad : VType = data | +Mk : (@(intrinsic(thk))) ((@(intrinsic(ret))) Int) end that 0)",
        TyckDiagnosticCode::Expressivity,
    );
}

#[test]
fn a_recursive_codata_type_declared_in_place_computes() {
    CbvCase::run_block(
        "def Stream : VType = codata | .head : Int | .tail : Stream end that
  let fix zeros (n : Int) : Stream = comatch | .head => n | .tail => zeros n end in
  let s : Stream = zeros 0 in let t : Stream = s.tail in t.head",
    );
}

/// The codata seal is applied inside its thunk: `Stream Int` is
/// `Thk (σ Int)`, through the name's `fn X => Thk (σ X)`.
#[test]
fn a_parameterized_recursive_codata_type_declared_in_place_computes() {
    CbvCase::run_block(
        "def Stream (X : VType) : VType = codata | .head : X | .tail : Stream X end that
  let fix zeros (n : Int) : Stream Int = comatch | .head => n | .tail => zeros n end in
  let s : Stream Int = zeros 0 in let t : Stream Int = s.tail in t.head",
    );
}

/// A source codata type is a value type, so its declared kind ends in `VType`:
/// the seal's `CType` is the translation's business, not the author's.
#[test]
fn rejects_a_sealed_codata_type_at_a_computation_kind() {
    CbvCase::reject(
        "@[cbv] (def S : CType = codata | .head : Int end that 0)",
        TyckDiagnosticCode::Expressivity,
    );
}

/// The declared kind has one arrow per parameter.
#[test]
fn rejects_a_sealed_codata_type_whose_kind_has_a_parameter_it_lacks() {
    CbvCase::reject(
        "@[cbv] (def S : VType -> VType = codata | .head : Int end that 0)",
        TyckDiagnosticCode::KindMismatch,
    );
}

/// A seal over a body that is no declaration could never be inhabited, since
/// a seal is entered only through the `data` or `codata` shape it exposes.
#[test]
fn rejects_a_sealed_definition_that_declares_no_data_or_codata() {
    CbvCase::reject("@[cbv] (def D : VType = Int that 5)", TyckDiagnosticCode::Expressivity);
}

/// **Mutual recursion**: declarations that mention one another form one group,
/// and every member's name is bound before any member is translated. A rose
/// tree's nodes hold a forest and its forest holds trees; the rightmost leaf's
/// label is the answer, and a forest read in the wrong order gives `2`.
#[test]
fn mutually_recursive_data_types_compute() {
    CbvCase::run_block(
        "def Tree : VType = data | +Node : Int * Forest end that
  def Forest : VType = data | +Nil : Unit | +Cons : Tree * Forest end that
  let fix rightmost (t : Tree) : Int =
    match t | +Node p => let (n, f) = p in
      match f
      | +Nil _ => n
      | +Cons c => let (h, rest) = c in
        match rest | +Nil _ => rightmost h | +Cons _ => rightmost (+Node (n, rest)) end
      end
    end
  in rightmost (+Node (1, +Cons (+Node (2, +Nil ()), +Cons (+Node (0, +Nil ()), +Nil ()))))",
    );
}

/// A group may mix `data` and `codata` and take parameters: the stream's tail
/// is a cell, and the cell's `+More` is a stream, whose name denotes
/// `Thk (σ X)` before the stream's own declaration has been translated.
#[test]
fn a_mutually_recursive_group_of_data_and_codata_computes() {
    CbvCase::run_block(
        "def Stream (X : VType) : VType = codata | .head : X | .tail : Cell X end that
  def Cell (X : VType) : VType = data | +End : Unit | +More : Stream X end that
  let fix alt (a : Int) (b : Int) : Stream Int =
    comatch | .head => a | .tail => +More (alt b a) end
  in let s : Stream Int = alt 1 0 in match s.tail | +End _ => 1 | +More t => t.head end",
    );
}

/// Each member is its own seal: two members of one shape are two types, as
/// two separate declarations are.
#[test]
fn rejects_one_member_of_a_group_at_another_of_the_same_shape() {
    CbvCase::reject(
        "@[cbv] (def A : VType = data | +Mk : Unit | +Next : B end that
  def B : VType = data | +Mk : Unit | +Next : A end that
  let a : A = +Mk () in let b : B = a in 0)",
        TyckDiagnosticCode::TypeMismatch,
    );
}

/// No member may escape, not only the first: `B` escapes through the result
/// here, and packing it, below, exports it.
#[test]
fn rejects_a_later_member_of_a_group_escaping_through_the_result() {
    CbvCase::reject(
        "@[cbv] (def A : VType = data | +Mk : Unit | +Next : B end that
  def B : VType = data | +Mk : Unit | +Next : A end that
  (+Mk () : B))",
        TyckDiagnosticCode::EscapingDeclaration,
    );
}

#[test]
fn exports_a_later_member_of_a_group_behind_an_abstract_package() {
    CbvCase::accept(
        "@[cbv] (def A : VType = data | +Mk : Unit | +Next : B end that
  def B : VType = data | +Mk : Unit | +Next : A end that
  ((B, +Next (+Mk ())) : exists (X : VType) . X))",
    );
}

/// Every member's kind must be known before the group is, as in the host:
/// here the second member's, which has parameters and no `: K`.
#[test]
fn rejects_a_mutually_recursive_member_without_its_kind() {
    CbvCase::reject(
        "@[cbv] (def A : VType = data | +Mk : B Int end that
  def B (X : VType) = data | +Mk : A end that 0)",
        TyckDiagnosticCode::InvalidBindingCycle,
    );
}

/// And every member must be sealed: a transparent `let` in a cycle is
/// missing its seal, as one that mentions only itself is.
#[test]
fn rejects_a_transparent_member_of_a_mutually_recursive_group() {
    CbvCase::reject(
        "@[cbv] (def A : VType = data | +Mk : B end that
  let B : VType = data | +Mk : A end that 0)",
        TyckDiagnosticCode::MissingSeal,
    );
}

/// When two members are at fault in different ways, the first member's fault
/// is the one reported, as in the host, whichever way round they come.
#[test]
fn reports_a_groups_admission_faults_member_by_member_as_the_host_does() {
    CbvCase::reject(
        "@[cbv] (let A : VType = data | +Mk : B Int end that
  def B (X : VType) = data | +Mk : A end that 0)",
        TyckDiagnosticCode::MissingSeal,
    );
    SourceCase::assert_rejected(
        CbvCase::check(
            "begin
  let A : VType = data | +Mk : B Int end that
  def B (X : VType) = data | +Mk : A end that
  ret ()
end",
        ),
        TyckDiagnosticCode::MissingSeal,
    );
    CbvCase::reject(
        "@[cbv] (def A (X : VType) = data | +Mk : B end that
  let B : VType = data | +Mk : A Int end that 0)",
        TyckDiagnosticCode::InvalidBindingCycle,
    );
    SourceCase::assert_rejected(
        CbvCase::check(
            "begin
  def A (X : VType) = data | +Mk : B end that
  let B : VType = data | +Mk : A Int end that
  ret ()
end",
        ),
        TyckDiagnosticCode::InvalidBindingCycle,
    );
}

/// What only a block requires of a member comes after every member's
/// admission, so the second member's missing seal is reported before the
/// first member's binder, which is a product. The host checks each binder
/// before the next member, and reports the binder.
#[test]
fn reports_a_later_members_admission_before_an_earlier_binders_shape() {
    CbvCase::reject(
        "@[cbv] (def (A, X) : VType = data | +A : B end that
  let B : VType = data | +B : A end that 0)",
        TyckDiagnosticCode::MissingSeal,
    );
    SourceCase::assert_rejected(
        CbvCase::check(
            "begin
  def (A, X) : VType = data | +A : B end that
  let B : VType = data | +B : A end that
  ret ()
end",
        ),
        TyckDiagnosticCode::SortMismatch,
    );
}

/// **Mutually recursive functions** are one `let fix` over a record of them,
/// as the host writes a fixed point of codata: each field is a function, and
/// a call to another goes through the record. The two calls end at opposite
/// base cases: an `.odd` that called itself answers `1`, and an `.even` that
/// called itself answers `2`.
#[test]
fn mutually_recursive_functions_are_a_recursive_record() {
    CbvCase::run_block(
        "def Nat : VType = data | +Zero : Unit | +Succ : Nat end that
  let Bool : VType = data | +True : Unit | +False : Unit end that
  let Parity : VType = codata | .even : Nat -> Bool | .odd : Nat -> Bool end that
  let fix parity (u : Unit) : Parity =
    comatch
    | .even => fn (n : Nat) => match n | +Zero _ => +True () | +Succ m => (parity ()).odd m end
    | .odd => fn (n : Nat) => match n | +Zero _ => +False () | +Succ m => (parity ()).even m end
    end
  in
  match (parity ()).even (+Succ (+Succ (+Zero ())))
  | +False _ => 1
  | +True _ => match (parity ()).even (+Succ (+Zero ())) | +True _ => 2 | +False _ => 0 end
  end",
    );
}

/// Two `let fix` bindings that name each other are a value cycle, which the
/// host refuses and directs to `fix`, and so does a block.
#[test]
fn rejects_two_recursive_functions_naming_each_other() {
    CbvCase::reject(
        "@[cbv] (def Nat : VType = data | +Zero : Unit | +Succ : Nat end that
  let fix even (n : Nat) : Int = match n | +Zero _ => 0 | +Succ m => odd m end that
  let fix odd (n : Nat) : Int = match n | +Zero _ => 1 | +Succ m => even m end that
  even (+Succ (+Zero ())))",
        TyckDiagnosticCode::InvalidBindingCycle,
    );
}

/// **A declaration's scope is the rest of its block**, so the block's type may
/// not mention it, as an unpack's witness may not escape. An abstract package
/// exports it, hiding it behind its witness.
#[test]
fn exports_a_declared_type_behind_an_abstract_package() {
    CbvCase::accept(&format!("@[cbv] ({BLOCK_OPT} ((Opt, +Some 7) : exists (X : VType) . X))"));
}

/// A manifest package discloses it, and is refused.
#[test]
fn rejects_a_manifest_package_disclosing_a_declared_type() {
    CbvCase::reject(
        &format!("@[cbv] ({BLOCK_OPT} ((Opt, +Some 7) : exists (X as Opt : VType) . X))"),
        TyckDiagnosticCode::EscapingDeclaration,
    );
}

#[test]
fn rejects_a_declared_type_escaping_through_the_result() {
    CbvCase::reject(
        &format!("@[cbv] ({BLOCK_OPT} (+Some 4 : Opt))"),
        TyckDiagnosticCode::EscapingDeclaration,
    );
}

/// Escape is a mention anywhere in the type: here in a translated arrow's
/// codomain.
#[test]
fn rejects_a_declared_type_escaping_inside_a_larger_type() {
    CbvCase::reject(
        &format!("@[cbv] ({BLOCK_OPT} fn (x : Int) => (+Some x : Opt))"),
        TyckDiagnosticCode::EscapingDeclaration,
    );
}

/// A codata name stands for `Thk σ`, which mentions the seal as well.
#[test]
fn rejects_a_declared_codata_type_escaping_through_the_result() {
    CbvCase::reject(
        "@[cbv] (def S : VType = codata | .head : Int end that (comatch | .head => 1 end : S))",
        TyckDiagnosticCode::EscapingDeclaration,
    );
}

/// The package leaves the block and is used outside it: the host unpacks it
/// and applies the operation it carries, which is what exporting is for.
#[test]
fn a_declared_type_exported_behind_a_package_is_used_outside() {
    SourceCase::assert_accepted(CbvCase::run(
        "do p <- @[cbv] (
  def Opt : VType = data | +None : Unit | +Some : Int end that
  ((Opt, +Some 0, fn (o : Opt) => match o | +Some n => n | +None _ => 1 end)
    : exists (X : VType) . X * (X -> Int))
);
let (X, x, f) = p in
do case_result <- ! f x;
! exit case_result",
    ));
}

// What the second review of the closed-types steps found.

/// The definition table holds a declaration's checked body, which the editor
/// shows as its definition. It held the seal, so a block's `D` was shown as
/// `D`; the host's holds the `data` type.
#[test]
fn records_a_sealed_declarations_body_as_the_host_does() {
    let host = CbvCase::type_definition_bodies(
        "begin
  def D : VType = data | +Mk : Int end that
  ret ()
end",
        "D",
    )
    .unwrap();
    let block = CbvCase::type_definition_bodies(
        &CbvCase::block("@[cbv] (def D : VType = data | +Mk : Int end in 0)"),
        "D",
    )
    .unwrap();
    assert_eq!(block, host);
}

/// A codata declaration's body is its translation: the thunk of the
/// computation type the seal is on, at the source name's kind.
#[test]
fn records_a_sealed_codata_declarations_body_as_its_thunk() {
    let bodies = CbvCase::type_definition_bodies(
        &CbvCase::block("@[cbv] (def S : VType = codata | .head : Int end in 0)"),
        "S",
    )
    .unwrap();
    assert!(
        matches!(bodies.as_slice(), [body] if body.starts_with("Thk") && body.contains("codata")),
        "{bodies:?}"
    );
}

/// A composite kind a block reads records its source facts, as the host's
/// checking of the same annotation does. Built directly, the named kind and the
/// arrow had none, which an editor reading those nodes would show.
#[test]
fn records_source_facts_for_a_composite_kind() {
    let shapes = ["Pi(", "Label("];
    let host = CbvCase::unannotated_terms(
        "let f = { fn (F : (#C :: VType) -> VType) => ret 0 } in ret ()",
        &shapes,
    )
    .unwrap();
    assert_eq!(host, Vec::<String>::new());
    let block = CbvCase::unannotated_terms(
        &CbvCase::block("@[cbv] (fn (F : (#C :: VType) -> VType) => 0)"),
        &shapes,
    )
    .unwrap();
    assert_eq!(block, Vec::<String>::new());
}

/// A value inside a named field or a package's payload records its source
/// facts, as a component of an ordinary tuple does: an editor reads them to
/// show a lambda's type.
#[test]
fn records_source_facts_for_named_field_and_package_payloads() {
    for source in [
        "@[cbv] ((fn (x : Int) => x, 1))",
        "@[cbv] ((#f = fn (x : Int) => x, 1))",
        "@[cbv] (((Int, fn (x : Int) => x) : exists (X : VType) . X -> X))",
        "@[cbv] (((Int, fn (x : Int) => x, 1) : exists (X : VType) . (X -> X) * Int))",
    ] {
        let missing = CbvCase::unannotated_terms(&CbvCase::block(source), &["Abs("]).unwrap();
        assert_eq!(missing, Vec::<String>::new(), "{source}");
    }
}

/// A declaration that writes no kind takes its parameters' kinds over `VType`.
/// Desugaring writes an omitted classifier as a hole, which was read as an
/// authored kind and refused, so the inference never ran.
#[test]
fn infers_a_sealed_declarations_kind_from_its_parameters() {
    CbvCase::accept("@[cbv] (def D = data | +Mk : Int end in 0)");
    CbvCase::accept("@[cbv] (def S = codata | .head : Int end in 0)");
    CbvCase::run_block(
        "def D (X : VType) = data | +Mk : X end in match (+Mk 0 : D Int) | +Mk n => n end",
    );
}

#[test]
fn rejects_an_inferred_kind_whose_parameter_declares_none() {
    CbvCase::reject("@[cbv] (def D X = data | +Mk : X end in 0)", TyckDiagnosticCode::Expressivity);
}

/// An ascription around the declaration is the ordinary one, checked against
/// the declaration's source kind. Only desugaring's own ascription was looked
/// through, so any other hid the declaration.
#[test]
fn accepts_an_ascription_around_a_sealed_declaration() {
    CbvCase::accept("@[cbv] (def S : VType = (codata | .head : Int end : VType) in 0)");
    CbvCase::accept("@[cbv] (def D (X : VType) : VType = (data | +Mk : X end : VType) in 0)");
    CbvCase::accept(
        "@[cbv] (def D : VType -> VType = ((fn (X : VType) => data | +Mk : X end) : VType -> VType) in 0)",
    );
}

/// A source codata type is a value type, here as anywhere.
#[test]
fn rejects_a_computation_kind_ascribed_to_a_sealed_codata_declaration() {
    CbvCase::reject(
        "@[cbv] (def S : VType = (codata | .head : Int end : CType) in 0)",
        TyckDiagnosticCode::KindMismatch,
    );
    CbvCase::reject(
        "@[cbv] (def D : VType -> VType = ((fn (X : VType) => data | +Mk : X end) : VType) in 0)",
        TyckDiagnosticCode::KindMismatch,
    );
}

// What the third review found.

/// **A hole stands for the part left to inference, and only that part.** The
/// classifier `pi (X : VType) . _`, which `: _` after a parameter desugars to,
/// infers its result from the parameters.
#[test]
fn infers_the_result_of_a_written_telescope() {
    CbvCase::run_block(
        "def D (X : VType) : _ = data | +Mk : X end in match (+Mk 0 : D Int) | +Mk n => n end",
    );
}

/// Everything written before the hole is read and checked as any kind is. A
/// classifier ending in `_` was taken for an omitted one whole, and its prefix
/// was never read: `CType -> _` and `Int -> _` were accepted.
#[test]
fn rejects_a_written_prefix_before_an_inferred_result() {
    CbvCase::reject(
        "@[cbv] (def D : CType -> _ = data | +Mk : Int end in 0)",
        TyckDiagnosticCode::Expressivity,
    );
    CbvCase::reject(
        "@[cbv] (def D : Int -> _ = data | +Mk : Int end in 0)",
        TyckDiagnosticCode::SortMismatch,
    );
}

/// And it is guarded as any kind is: a prefix that reads outside the block was
/// admitted unread.
#[test]
fn rejects_a_prefix_before_an_inferred_result_that_reaches_outside() {
    SourceCase::assert_rejected(
        CbvCase::check(
            "let outside = 7 in
             do case_result <- @[cbv] (def D : pi (Y : @[typeof] outside) . _ = data | +Mk : Int end in 0);
             ret ()",
        ),
        TyckDiagnosticCode::Expressivity,
    );
    SourceCase::assert_rejected(
        CbvCase::check_with_import(
            "do case_result <- @[cbv] (def D : pi (_ : @(import(\"imported.zy\"))) . _ = data | +Mk : Int end in 0);
ret ()",
            "@(intrinsic(vtype))",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

/// An ascription under a parameter is read where the parameter is bound. Read
/// before, `X` reached the checker ahead of its binder, which it refuses by
/// panicking; here it is a type where a kind belongs. Beside
/// `accepts_an_ascription_around_a_sealed_declaration`.
#[test]
fn rejects_an_ascription_naming_a_parameter_by_sort() {
    CbvCase::reject(
        "@[cbv] (def D (X : VType) : VType = (data | +Mk : X end : X) in 0)",
        TyckDiagnosticCode::SortMismatch,
    );
    CbvCase::reject(
        "@[cbv] (def S (X : VType) : VType = (codata | .head : X end : X) in 0)",
        TyckDiagnosticCode::SortMismatch,
    );
}

/// An inferred kind is read off the parameters' annotations, and a kind mentions
/// no type, so one naming an earlier parameter is refused before it is read: read,
/// it reached the parameter ahead of its binder.
#[test]
fn rejects_a_parameter_kind_naming_an_earlier_parameter() {
    CbvCase::reject(
        "@[cbv] (def D (X : VType) (Y : X) = data | +Mk : Int end in 0)",
        TyckDiagnosticCode::SortMismatch,
    );
    // read off the telescope `: _` after the parameters desugars to
    CbvCase::reject(
        "@[cbv] (def D (X : VType) (Y : X) : _ = data | +Mk : Int end in 0)",
        TyckDiagnosticCode::SortMismatch,
    );
}

#[test]
fn infers_a_kind_from_two_parameters() {
    CbvCase::run_block(
        "def D (X : VType) (Y : VType) = data | +Mk : X * Y end in
  match (+Mk (0, 1) : D Int Int) | +Mk p => let (a, b) = p in a end",
    );
}

/// A kind arrow's binder records the facts ordinary checking records there: a
/// representative for the annotated binder pattern, and the annotation of the
/// name it binds. The arrow and its domain had theirs; the binder had none.
#[test]
fn records_binder_facts_for_a_kind_arrow() {
    let host = CbvCase::unrecorded_pattern_facts(
        "let f = { fn (F : pi (X : VType) . VType) => ret 0 } in
let g = { fn (G : (#C :: VType) -> VType) => ret 0 } in
ret ()",
        "X",
    )
    .unwrap();
    assert_eq!(host, (Vec::<String>::new(), 0));
    let block = CbvCase::unrecorded_pattern_facts(
        &CbvCase::block(
            "@[cbv] (let f = fn (F : pi (X : VType) . VType) => 0 in
  let g = fn (G : (#C :: VType) -> VType) => 0 in 0)",
        ),
        "X",
    )
    .unwrap();
    assert_eq!(block, (Vec::<String>::new(), 0));
}

// What the fourth review found.

/// A telescope binder before an inferred result is checked whole, every
/// annotation in it guarded as a type binder's is. Only its outermost one was,
/// and the pattern rule read an inner one unguarded: a query of a term bound
/// outside the block, and an import, were admitted. With `VType` in place of
/// the `_` the same binders were refused.
#[test]
fn rejects_an_inner_annotation_of_a_telescope_binder_that_reaches_outside() {
    for declaration in ["data | +Mk : Y end", "codata | .head : Y end"] {
        SourceCase::assert_rejected(
            CbvCase::check(&format!(
                "let outside = 7 in
             do case_result <- @[cbv] (
               def D : pi ((X : @[typeof] (@[typeof] outside)) : VType) . _ =
                 fn (Y : VType) => {declaration}
               in 0);
             ret ()"
            )),
            TyckDiagnosticCode::Expressivity,
        );
    }
    SourceCase::assert_rejected(
        CbvCase::check_with_import(
            "do case_result <- @[cbv] (
  def D : pi ((X : @(import(\"imported.zy\"))) : VType) . _ =
    fn (Y : VType) => data | +Mk : Y end
  in 0);
ret ()",
            "@(intrinsic(vtype))",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

/// Beside it: an inner annotation that stays inside the block is admitted, in
/// a telescope written whole and in one desugared from the parameters.
#[test]
fn accepts_an_inner_annotation_of_a_telescope_binder() {
    CbvCase::run_block(
        "def D : pi ((X : VType) : VType) . _ = fn (Y : VType) => data | +Mk : Y end in
  match (+Mk 0 : D Int) | +Mk n => n end",
    );
    CbvCase::run_block(
        "def D ((X : VType) : VType) : _ = data | +Mk : X end in
  match (+Mk 0 : D Int) | +Mk n => n end",
    );
}

/// The binder of a telescope before an inferred result records its facts too.
/// The host refuses `: _` after parameters, so there is nothing to compare it
/// with; it is held to having none missing.
#[test]
fn records_binder_facts_for_a_written_telescope() {
    let block = CbvCase::unrecorded_pattern_facts(
        &CbvCase::block("@[cbv] (def D (X : VType) : _ = data | +Mk : X end in 0)"),
        "X",
    )
    .unwrap();
    assert_eq!(block, (Vec::<String>::new(), 0));
}

/// A recursive declaration's classifier must be a kind before anything else
/// about the group is decided, as the host has it: a known name is not a known
/// kind. Beside `rejects_a_recursive_declaration_without_its_kind`, which has
/// none.
#[test]
fn rejects_a_recursive_declaration_at_a_type_as_the_host_does() {
    CbvCase::reject(
        "@[cbv] (let N : Int = data | +Mk : N end that 0)",
        TyckDiagnosticCode::InvalidBindingCycle,
    );
    CbvCase::reject(
        "@[cbv] (def N : Int = data | +Mk : N end that 0)",
        TyckDiagnosticCode::InvalidBindingCycle,
    );
    CbvCase::reject(
        "@[cbv] (def A : Int = data | +A : B end that def B : VType = data | +B : A end that 0)",
        TyckDiagnosticCode::InvalidBindingCycle,
    );
}

/* ------------------------------- recursion -------------------------------- */

/// `let fix` is host syntax already; what is new is its reading inside a
/// block. `: Int` is the **source** return type and the recursive call is
/// written `f x` — the `Ret`, the `Thk` and the force are the translation's.
/// The emitted term is
/// `do f <- ret { fix f => fn x => do g <- ret f; do a <- ret x; ! g a }; …`,
/// so the recursive name maps straight onto the `fix` binder with no coercion.
#[test]
fn translates_a_recursive_binding() {
    CbvCase::accept("@[cbv] (let fix f (x : Int) : Int = f x in f)");
}

/// The clause builds a `fix` whether or not the body calls itself, so the
/// terminating case is a recursive binding that does not recur. A block
/// cannot reach a comparison, so there is no way yet to write one that recurs
/// and stops.
#[test]
fn a_recursive_binding_that_does_not_recur_runs() {
    SourceCase::assert_accepted(CbvCase::run(
        "do case_result <- @[cbv] (let fix f (x : Int) : Int = x in f 0);
! exit case_result",
    ));
}

/// A directly written `fix` stays refused: the source language is
/// call-by-value and `fix` is call-by-push-value's own form.
#[test]
fn rejects_a_directly_written_fix() {
    CbvCase::reject("@[cbv] (fix f => f)", TyckDiagnosticCode::Expressivity);
}

/// And a thunked fixed point that is *not* a header is not mistaken for one.
/// Recognition requires the whole shape desugaring builds, including the
/// `Internal::Thk` node that only it writes into a classifier.
///
/// The diagnostic is the source-type rule's rather than the unsupported-form
/// one, and the path is: desugaring gives a bare thunk the classifier `Thk _`,
/// the translation reads that classifier first, and the `Thk` desugaring writes
/// there is an intrinsic that names no source primitive — so the term dispatch
/// never reaches `Thunk` at all.
#[test]
fn rejects_a_thunked_fix_that_is_not_a_header() {
    CbvCase::reject(
        "@[cbv] (let f = { fix g => fn (x : Int) => x } in f 1)",
        TyckDiagnosticCode::Expressivity,
    );
}

/// A header is recognized by its shape alone. A first parameter written without
/// a type is then refused for missing it, as a later one is, at the binding;
/// recognition used to require the type, and the header fell through to the term
/// rules, which refused the `Thk` desugaring writes into its classifier. In the
/// `let !` spelling the signature supplies the parameter's type, as an expected
/// arrow does, and is still compared with the body.
#[test]
fn recognizes_a_header_whose_first_parameter_has_no_type() {
    for header in ["let fix g x : Int = 0 in 0", "let fix g (x : Int) y : Int = 0 in 0"] {
        SourceCase::assert_rejected_at(
            CbvCase::check(&CbvCase::block(&format!("@[cbv] ({header})"))),
            TyckDiagnosticCode::Expressivity,
            header,
        );
    }
    CbvCase::accept("@[cbv] (let ! f : Int -> Int = fix g => fn x => x in f 1)");
    CbvCase::reject(
        "@[cbv] (let ! f : Int -> Int = fix g => fn x => (x : Unit) in f 1)",
        TyckDiagnosticCode::TypeMismatch,
    );
}

/// A function header that does not recur is the host's own: desugaring gives it
/// a signature whose binders name the parameters, and nothing reads the names.
/// A signature that mentions one is refused by sort.
#[test]
fn translates_a_function_header() {
    CbvCase::accept("@[cbv] (let f (x : Int) : Int = x in f 1)");
    CbvCase::accept("@[cbv] (let f (x : Int) (y : Int) : Int = y in f 1 2)");
    CbvCase::accept("@[cbv] (let f (X : VType) (x : X) : X = x in f Int 3)");
    CbvCase::reject("@[cbv] (let f (x : Int) : Int = () in 0)", TyckDiagnosticCode::TypeMismatch);
    CbvCase::reject("@[cbv] (let f (x : Int) : x = x in 0)", TyckDiagnosticCode::SortMismatch);
}

/// The `let !` spelling writes a signature's binders apart from the
/// parameters, so each is checked as a parameter is: every annotation inside it
/// is translated, guarded and compared with its domain, and its shape is a
/// binder's, in any position of the group. They were read for their outermost
/// annotation alone, which let a conflicting annotation, a literal, and a
/// classifier reading outside the block through.
#[test]
fn checks_a_let_bang_signature_binder_as_a_parameter() {
    const ONE: &str = "fix g => fn (x : Int) => x in f 0";
    const TWO: &str = "fix g => fn (x : Int) (y : Int) => y in f 0 1";
    CbvCase::accept(&format!("@[cbv] (let ! f : pi ((x : Int) : Int) . Int = {ONE})"));
    CbvCase::accept(&format!("@[cbv] (let ! f : pi (x : Int) ((y : Int) : Int) . Int = {TWO})"));
    for (signature, body, expected) in [
        ("pi ((x : Unit) : Int) . Int", ONE, TyckDiagnosticCode::TypeMismatch),
        ("pi (x : Int) ((y : Unit) : Int) . Int", TWO, TyckDiagnosticCode::TypeMismatch),
        ("pi (0 : Int) . Int", ONE, TyckDiagnosticCode::Expressivity),
        ("pi ((x : @[typeof] outside) : Int) . Int", ONE, TyckDiagnosticCode::Expressivity),
    ] {
        SourceCase::assert_rejected(
            SourceCase::check(&CbvBlock::closed(&format!(
                "let outside = 0 in
do case_result <- @[cbv] (let ! f : {signature} = {body});
ret ()"
            ))),
            expected,
        );
    }
}

/// Several parameters are one function: `let fix f (x : A) (y : B) : C = M` is
/// `pi (_ : A) (_ : B) . C`, and a call passes all of them. The signature's
/// binders are only parameters' names, which nothing in a source type can
/// mention, so they are read for their domains.
#[test]
fn translates_a_recursive_binding_with_several_parameters() {
    CbvCase::accept("@[cbv] (let fix f (x : Int) (y : Int) : Int = y in f 1 2)");
    CbvCase::accept("@[cbv] (let fix f (x : Int) (y : Int) (z : Int) : Int = z in f 1 2 3)");
    CbvCase::accept("@[cbv] (let fix f (x : Int) (X : VType) (y : X) : X = y in f 1 Int 2)");
}

/// Consecutive parameters are one function, so a call passes all of them.
#[test]
fn refuses_a_recursive_call_with_too_few_arguments() {
    CbvCase::reject(
        "@[cbv] (let fix f (x : Int) (y : Int) : Int = f x in 0)",
        TyckDiagnosticCode::Expressivity,
    );
    CbvCase::reject(
        "@[cbv] (let fix f (x : Int) (y : Int) : Int = y in f 1)",
        TyckDiagnosticCode::Expressivity,
    );
}

const NEST: &str = "def Nest (X : VType) : VType = data | +Flat : X | +Deep : Nest (X * X) end";

/// A first parameter that is a type makes the fixed point a type abstraction,
/// `fix g => fn X => …`: polymorphic recursion. The recursive name stands at the
/// whole quantified signature, so a recursive call may instantiate it at another
/// type, here `X * X` through a nested data type. The first parameter had to be
/// a term.
#[test]
fn translates_polymorphic_recursion() {
    CbvCase::accept("@[cbv] (let fix f (X : VType) (x : X) : X = x in f Int 3)");
    CbvCase::accept_declaring(
        &[NEST],
        "@[cbv] (let fix pick (X : VType) (n : Nest X) : X = match n | +Flat(x) => x | +Deep(m) => let (a, b) = pick (X * X) m in b end in pick Int (+Deep(+Flat((1, 2)))))",
    );
    CbvCase::accept(
        "@[cbv] (let fix ap (F : VType -> VType) (x : F Int) : F Int = x in ap (fn X => X) 9)",
    );
}

/// The `let !` spelling reaches this clause too, and there the signature's
/// binder and the abstraction's are written separately. The abstraction's type
/// is compared with the quantifier, so a binder kind that disagrees is a kind
/// error even when the body never mentions the variable.
#[test]
fn refuses_a_let_bang_signature_whose_type_binder_disagrees() {
    CbvCase::reject(
        "@[cbv] (let ! f : pi (X : VType) . Int = fix g => fn (X : VType -> VType) => 5 in 0)",
        TyckDiagnosticCode::KindMismatch,
    );
}

/// The `let !` spelling can also put a type abstraction under a signature whose
/// first binder is a term, or the reverse. Both are sort mismatches. The first
/// reached the quantifier's destructors with an arrow, and the checker panicked.
#[test]
fn refuses_a_let_bang_signature_that_disagrees_about_a_type_parameter() {
    CbvCase::reject(
        "@[cbv] (let ! f : pi (x : Int) . Int = fix g => fn (X : VType) => 5 in 0)",
        TyckDiagnosticCode::SortMismatch,
    );
    CbvCase::reject(
        "@[cbv] (let ! f : pi (X : VType) . Int = fix g => fn (x : Int) => 5 in 0)",
        TyckDiagnosticCode::SortMismatch,
    );
}

/// The body is checked at the quantifier's instance, and the binder's kind is a
/// source kind, as at any type binder.
#[test]
fn refuses_polymorphic_recursion_outside_the_rules() {
    CbvCase::reject(
        "@[cbv] (let fix f (X : VType) (x : X) : X = 3 in 0)",
        TyckDiagnosticCode::TypeMismatch,
    );
    CbvCase::reject(
        "@[cbv] (let fix f (R : CType) (x : Int) : Int = x in 0)",
        TyckDiagnosticCode::Expressivity,
    );
}

/* -------------------------- annotated sub-patterns ------------------------- */

const FN_BOX: &str = "def FnBox : VType = data | +F : Int -> Int end";

/// An annotation below a binder's top level. Its classifier is a source type,
/// which ordinary pattern checking cannot read — `Int -> Int` is ill-kinded in
/// the host — so the block translates it first and the annotation rule takes
/// the translation. The shape guard refused every such annotation. Each case
/// annotates at a source arrow, so each site that hands a binder to ordinary
/// checking is witnessed: a `let`, a lambda, a `let fix` parameter, an arm.
#[test]
fn translates_an_annotated_sub_pattern() {
    CbvCase::accept("@[cbv] (let (f : Int -> Int, n) = (fn (x : Int) => x, 3) that f n)");
    CbvCase::accept(
        "@[cbv] (let g = fn ((f : Int -> Int, y) : (Int -> Int) * Int) => f y that g (fn (x : Int) => x, 4))",
    );
    CbvCase::accept(
        "@[cbv] (let fix k ((f : Int -> Int, n) : (Int -> Int) * Int) : Int = f n in k (fn (x : Int) => x, 5))",
    );
    CbvCase::accept_declaring(
        &[FN_BOX],
        "@[cbv] (let h = fn (b : FnBox) => match b | +F((f : Int -> Int)) => f 6 end that
  h (+F (fn (x : Int) => x)))",
    );
}

/// The annotation is compared with the type its position expects.
#[test]
fn refuses_an_annotated_sub_pattern_that_disagrees() {
    CbvCase::reject("@[cbv] (let (a : Unit, b) = (1, 2) that b)", TyckDiagnosticCode::TypeMismatch);
}

/// It is translated under the block's guards, so it reaches nothing outside the
/// block and denotes no type the block could not write.
#[test]
fn refuses_an_annotated_sub_pattern_outside_the_rules() {
    CbvCase::reject_beside_outside(
        CbvCase::block("@[cbv] (let (a : @[typeof] outside, b) = (1, 2) that b)"),
        TyckDiagnosticCode::Expressivity,
    );
    CbvCase::reject(
        "@[cbv] (let (a : (@(intrinsic(thk))) ((@(intrinsic(ret))) Int), b) = (1, 2) that b)",
        TyckDiagnosticCode::Expressivity,
    );
}

/// An annotation is translated before its binder binds anything, so it may not
/// mention a variable the binder introduces. This one reached ordinary synthesis
/// before its variable's binder had been checked, and the checker panicked.
///
/// Written with `in`: with `that`, name resolution lifts the two bindings into a
/// group that is refused as a binding cycle first, which would keep this case
/// green whatever happened to the annotation.
#[test]
fn refuses_an_annotation_that_mentions_its_own_binder() {
    CbvCase::reject(
        "@[cbv] (let p = ((Int, 3) : exists (X : VType) . X) in let (X, (x : X)) = p in 0)",
        TyckDiagnosticCode::Expressivity,
    );
}

/* ------------------------------- existentials ----------------------------- */

/// Naming one is the first thing the family needs, and it was the first thing
/// missing: the image test had no `exists` clause, so a block could not even
/// mention an existential type, let alone build a value at one.
#[test]
fn names_an_abstract_existential() {
    CbvCase::accept_with_exists("@[cbv] (param p : Boxed that 7)");
}

#[test]
fn names_a_manifest_existential() {
    CbvCase::accept_with_exists("@[cbv] (param p : Clear that 7)");
}

/// A manifest binder declares its kind before its definition is read, so an
/// operator definition takes its binder's kind from it, as a pack's witness
/// does; under a field name the definition is the payload's.
#[test]
fn reads_a_manifest_definition_against_its_declared_kind() {
    CbvCase::accept(
        "@[cbv] (let P : VType = exists (F as (fn X => X) : VType -> VType) . F Int in 0)",
    );
    CbvCase::accept(
        "@[cbv] (let P : VType = exists (#C = ((F as (fn X => X)) : VType -> VType)) . F Int in 0)",
    );
    CbvCase::reject(
        "@[cbv] (let P : VType = exists (F as (fn X => X) : VType) . F in 0)",
        TyckDiagnosticCode::KindMismatch,
    );
}

/// `[[(T, V)]]v = ([[T]]v, [[V]]v)`. The same `Cons` node as a tuple: the
/// expected type is what says the first component is a type.
#[test]
fn builds_a_pack_at_a_manifest_existential() {
    CbvCase::accept_with_exists("@[cbv] ((Int, 7) : Clear)");
}

#[test]
fn builds_a_pack_at_an_abstract_existential() {
    CbvCase::accept_with_exists("@[cbv] ((Int, 7) : Boxed)");
}

/// A pack with more components than witnesses: the rest are the body product.
#[test]
fn builds_a_pack_whose_body_is_a_product() {
    CbvCase::accept_with_exists("@[cbv] ((Int, 7, fn (x : Int) => x) : Showable)");
}

/// `[[(T, M)]]v_term = do a <- [[M]]v; ret ([[T]]v, a)`, the term row: a
/// payload that is not a source value is sequenced, and the witness is not.
#[test]
fn builds_a_pack_whose_payload_is_a_computation() {
    CbvCase::accept_with_exists("@[cbv] ((Int, (fn (x : Int) => x) 7) : Clear)");
}

/// The manifest equation is checked, not assumed — which is what makes the
/// disclosed witness worth translating rather than ignoring.
#[test]
fn rejects_a_pack_whose_witness_disagrees_with_the_manifest_definition() {
    CbvCase::reject_with_exists("@[cbv] ((Unit, ()) : Clear)", TyckDiagnosticCode::TypeMismatch);
}

/// Nothing in `(Int, 7)` says which existential it inhabits, so the row needs
/// an expectation and without one the product clause applies and refuses the
/// first component as a value.
#[test]
fn rejects_a_pack_with_no_expected_type() {
    CbvCase::reject_with_exists("@[cbv] (Int, 7)", TyckDiagnosticCode::Expressivity);
}

/// `[[let (X, x) = M in N]]term`. The binder is a product whose leading
/// component binds a type; the closedness walk had no arm for that shape, so
/// the body's own variable read as one bound outside the block.
#[test]
fn unpacks_a_manifest_package() {
    CbvCase::accept_with_exists("@[cbv] (param p : Clear that let (X, v) = p that v)");
}

/// The substance of the row: the opened witness is admissible as a *source*
/// type, so a source lambda can be written at it.
#[test]
fn uses_an_opened_witness_as_a_source_type() {
    CbvCase::accept_with_exists(
        "@[cbv] (param p : Boxed that let (X, v) = p that (fn (y : X) => 7) v)",
    );
}

/// And as the domain of a source arrow carried in the package itself, which is
/// the shape an existential is actually used in.
#[test]
fn applies_a_packaged_operation_to_its_own_opaque_payload() {
    CbvCase::accept_with_exists("@[cbv] (param p : Showable that let (X, v, k) = p that k v)");
}

/// A witness may not outlive its unpack. Paired with the two accepted cases
/// above, which differ only in whether the result mentions `X`.
#[test]
fn rejects_a_witness_that_escapes_the_unpack() {
    CbvCase::reject_with_exists(
        "@[cbv] (param p : Boxed that let (X, v) = p that v)",
        TyckDiagnosticCode::EscapingExistential,
    );
}

#[test]
fn packs_and_unpacks_in_one_block() {
    CbvCase::accept_with_exists(
        "@[cbv] (let p = ((Int, 7, fn (x : Int) => x) : Showable) that let (X, v, k) = p that k v)",
    );
}

/// A pack labels its value with the *expected* existential rather than with a
/// type it synthesized, so the net compares the expectation against itself and
/// cannot see a payload that disagrees. The seventh review found the consequence:
/// this program was accepted, satisfied the arena invariants, and unpacking the alleged
/// `Int` into `int/add` reached `unreachable!()` in the interpreter.
#[test]
fn rejects_a_pack_whose_payload_disagrees_with_the_witness() {
    CbvCase::reject_with_exists(
        "@[cbv] ((Int, fn (x : Int) => x) : Clear)",
        TyckDiagnosticCode::TypeMismatch,
    );
}

/// The same defect by a second route: an arity the body does not have. The
/// expected-components helper degrades to *no* expectation when the count
/// disagrees, so every component synthesized and the product was stamped with the
/// existential anyway.
#[test]
fn rejects_a_pack_with_excess_components() {
    CbvCase::reject_with_exists("@[cbv] ((Int, 7, 8) : Clear)", TyckDiagnosticCode::TypeMismatch);
    CbvCase::reject_with_exists(
        "@[cbv] ((Int, (fn (x : Int) => x) 7, 8) : Clear)",
        TyckDiagnosticCode::TypeMismatch,
    );
}

/// A block is closed, and the type-abstraction entrance did not enforce it. The
/// clause is recognized by the binder's *outer* annotation, and the whole pattern
/// then went to ordinary synthesis, which descends into the rest — so an inner
/// annotation could read a term bound outside the block. This is the same defect
/// the fifth review found in the image test's classifier guard, at an entrance
/// added after that guard was written.
#[test]
fn rejects_a_type_binder_whose_annotation_reads_outside_the_block() {
    SourceCase::assert_rejected(
        CbvCase::check(
            "let outside = 7 in\n             do case_result <- @[cbv] (fn ((X : @[typeof] (@[typeof] outside)) : VType) => 0);\n             ret ()",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

/// And the binder's kind must be one the source has, as it must where a `forall`
/// is *named*: no source type is a computation.
#[test]
fn rejects_a_type_binder_at_a_computation_kind() {
    CbvCase::reject("@[cbv] (fn (R : CType) => 0)", TyckDiagnosticCode::Expressivity);
}

/// A witness opened by a **lambda parameter** may not outlive it, as one opened by
/// a `let` may not. The lambda clause was given one half of that rule — the opened
/// witnesses enter the body's scope — and not the other, so this built an ordinary
/// arrow whose codomain mentioned a free `X`, which the arena invariants did not object
/// to.
///
/// **This case pins the rule, not the fix.** Removing `close_scope_k` from the
/// lambda clause leaves it passing, because this harness's wrapper rejects the
/// free witness for its own reasons whatever the block decides — verified by
/// deleting the call and re-running. The discriminating witness is a standalone
/// program, where the block's result is the whole program and nothing else
/// constrains it:
///
/// ```text
/// @[cbv] (let VType = @(intrinsic(vtype)) that
///   let Boxed : VType = exists (X : VType) . X that
///   fn ((X, v) : Boxed) => v)
/// ```
///
/// `zydeco check` on that file was accepted before the fix and is
/// refused after it.
#[test]
fn rejects_a_witness_that_escapes_a_lambda_parameters_unpack() {
    SourceCase::assert_rejected(
        CbvCase::check(
            "@[cbv] (let Boxed : VType = exists (X : VType) . X that fn ((X, v) : Boxed) => v)",
        ),
        TyckDiagnosticCode::EscapingExistential,
    );
}

/// Its accepted neighbour, differing only in whether the result mentions `X`.
#[test]
fn admits_a_lambda_parameters_unpack_whose_result_is_closed() {
    SourceCase::assert_accepted(CbvCase::check_invariants(
        "@[cbv] (let Boxed : VType = exists (X : VType) . X that fn ((X, v) : Boxed) => 7)",
    ));
}

/// `[[S/l]]v = [[S]]v/l`, witnessed. The projection is **residual**: `P` is a
/// binder of labeled kind, so `P/item` cannot normalize. Recorded because the
/// row was previously marked as having a clause and no witness — on the strength
/// of two spellings that the *host* rejects, when what the clause needs is a
/// projection of a bound variable rather than of a concrete record.
#[test]
fn admits_a_residual_type_projection() {
    CbvCase::accept("@[cbv] (fn (p : forall (P : (#item :: VType)) . P/item) => p)");
}

/* ----------------------- the type-level abstraction ----------------------- */
//
// The type-level STLC was translated in its application and not in its
// abstraction, which showed up three ways: an applied seal, an operator passed
// as a type argument, and an operator written inside a block. The cases below
// are paired so that each acceptance differs from a refusal by one thing.

const OPT: &str = "def Opt (X : VType) : VType = data | +None : Unit | +Some : X end";
const LIST: &str = "def List (X : VType) : VType = data | +Nil : Unit | +Cons : X * List X end";
const HIGHER: &str = "let Higher : VType = forall (F : VType -> VType) . F Int";

/// `[[F A]]v = F [[A]]v` with `F` a **sealed** operator, which is how std writes
/// `List`. A transparent operator normalizes away and a sealed one applied to
/// nothing unfolds once; only the two together were refused.
#[test]
fn admits_an_applied_seal() {
    CbvCase::accept_declaring(&[OPT], "@[cbv] (let f = fn (p : Opt Int) => 0 that f (+None ()))");
}

/// A recursive family, applied and matched on.
#[test]
fn admits_a_recursive_parameterized_seal_and_matches_on_it() {
    CbvCase::accept_declaring(
        &[LIST],
        "@[cbv] (let f = fn (p : List Int) =>
    match p | +Nil(_) => 0 | +Cons(c) => let (h, t) = c in h end
  that f (+Cons (3, +Nil ())))",
    );
}

const STREAM: &str = "def Stream (X : VType) : VType = codata | .head : X | .tail : Stream X end";

/// An applied recursive **codata** operator, whose seal sits inside the `Thk`.
/// Named from outside, it overflowed the image test's stack: an applied one was
/// unrolled at its arguments, and the recursive `.tail` field unrolled it again
/// at every mention.
#[test]
fn admits_an_applied_recursive_codata_seal() {
    CbvCase::accept_declaring(&[STREAM], "@[cbv] (let f = fn (s : Stream Int) => 0 that 0)");
}

/// And usable: built by `let fix` and observed twice.
#[test]
fn an_applied_recursive_stream_is_built_and_observed() {
    SourceCase::assert_accepted(CbvCase::run(&CbvCase::within(
        &[STREAM],
        "do case_result <- @[cbv] (
  let fix ones (n : Int) : Stream Int = comatch | .head => n | .tail => ones n end in
  let s : Stream Int = ones 0 in
  let t : Stream Int = s.tail in
  t.head
);
! exit case_result",
    )));
}

/// `[[fn (X : K) => T]]v`: an operator passed as a type argument.
#[test]
fn admits_an_operator_as_a_type_argument() {
    CbvCase::accept_declaring(
        &[HIGHER, "let Id : VType -> VType = fn (X : VType) => X"],
        "@[cbv] (param f : Higher that f Id)",
    );
}

#[test]
fn refuses_an_operator_argument_whose_body_is_outside_the_image() {
    CbvCase::reject_declaring(
        &[
            HIGHER,
            "let BadOp : VType -> VType = fn (X : VType) => (@(intrinsic(thk))) ((@(intrinsic(ret))) X)",
        ],
        "@[cbv] (param f : Higher that f BadOp)",
        TyckDiagnosticCode::Expressivity,
    );
}

/// A sealed operator passed unapplied.
#[test]
fn admits_a_sealed_operator_as_a_type_argument() {
    CbvCase::accept_declaring(&[HIGHER, OPT], "@[cbv] (let g = fn (f : Higher) => f Opt that 0)");
}

/// A block's **own** higher-kinded binder, applied in a classifier, at a kind
/// the block binds. The image test admitted a bound operator applied, but this
/// path seeded it with no binders at all.
#[test]
fn applies_a_blocks_own_higher_kinded_binder() {
    CbvCase::accept_declaring(
        &["let K = VType -> VType"],
        "@[cbv] (fn (F : K) => fn (x : F Int) => x)",
    );
}

/// The forward clause, reached through a type binding whose kind is an arrow
/// the block binds to a name.
#[test]
fn translates_a_type_operator_bound_inside_a_block() {
    CbvCase::accept_declaring(
        &["let K = VType -> VType"],
        "@[cbv] (let F : K = fn (X : VType) => X * X that let p : F Int = (1, 2) that p)",
    );
}

#[test]
fn translates_an_inline_operator_as_a_type_argument() {
    CbvCase::accept_declaring(
        &[HIGHER],
        "@[cbv] (param f : Higher that f (fn (X : VType) => X * X))",
    );
}

/// `[[F A]]v = [[F]]v [[A]]v`: the head is translated like the arguments. It went
/// to ordinary checking as a name, so a type-level redex written in place was
/// refused while the same operator bound to a name and applied was accepted.
#[test]
fn translates_an_operator_written_at_the_head_of_an_application() {
    CbvCase::accept("@[cbv] (let p : (fn (X : VType) => X * X) Int = (1, 2) that p)");
    CbvCase::accept(
        "@[cbv] (let p : (fn (X : VType) => fn (Y : VType) => X * Y) Int Unit = (1, ()) that p)",
    );
}

/// A written head passes through the clause that translates it, so its binder's
/// kind and its body are checked as an operator's are anywhere else.
#[test]
fn refuses_a_written_head_over_a_computation_kind_or_outside_the_image() {
    CbvCase::reject(
        "@[cbv] (let p : (fn (R : CType) => Int) Int = 0 that 0)",
        TyckDiagnosticCode::Expressivity,
    );
    CbvCase::reject(
        "@[cbv] (let p : (fn (X : VType) => (@(intrinsic(thk))) ((@(intrinsic(ret))) X)) Int = 0 that 0)",
        TyckDiagnosticCode::Expressivity,
    );
}

/// And a head that is not a type form at all reaches nothing outside the block.
#[test]
fn refuses_a_head_that_reads_outside_the_block() {
    CbvCase::reject_beside_outside(
        CbvCase::block("@[cbv] (let p : (@[typeof] outside) Int = 0 that 0)"),
        TyckDiagnosticCode::Expressivity,
    );
}

/// β through a written head avoids capture. Substituting the outer `X` for `Y`
/// under an inner `forall (X : VType)` renames the inner binder, so the type is
/// `forall X . forall X' . X * X' -> X` and the first component inhabits it.
#[test]
fn substitutes_without_capture_through_a_written_head() {
    CbvCase::accept(
        "@[cbv] (let g : forall (X : VType) . (fn (Y : VType) => forall (X : VType) . Y * X -> Y) X = fn (A : VType) => fn (B : VType) => fn (p : A * B) => let (a, b) = p in a that 0)",
    );
}

/// The second component does not, and would have under capture, when the type
/// would have read `forall X . forall X . X * X -> X`.
#[test]
fn refuses_the_component_capture_would_have_admitted() {
    CbvCase::reject(
        "@[cbv] (let g : forall (X : VType) . (fn (Y : VType) => forall (X : VType) . Y * X -> Y) X = fn (A : VType) => fn (B : VType) => fn (p : A * B) => let (a, b) = p in b that 0)",
        TyckDiagnosticCode::TypeMismatch,
    );
}

/// **Both guards hold at this entrance too**, because both abstractions now open
/// their binder through one function. This is the input that read a term bound
/// outside the block at the term-level abstraction before that was fixed.
#[test]
fn refuses_a_type_operator_whose_binder_reads_outside_the_block() {
    CbvCase::reject_beside_outside(
        CbvCase::declaring(
            &["let K = VType -> VType"],
            "@[cbv] (let F : K = fn ((X : @[typeof] (@[typeof] outside)) : VType) => X that 0)",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

#[test]
fn refuses_a_type_operator_over_a_computation_kind() {
    CbvCase::reject_declaring(
        &["let K = CType -> VType"],
        "@[cbv] (let F : K = fn (R : CType) => Thk R that 0)",
        TyckDiagnosticCode::Expressivity,
    );
}

/// The operator's body is a source type like any other.
#[test]
fn refuses_a_type_operator_whose_body_is_outside_the_image() {
    CbvCase::reject_declaring(
        &["let K = VType -> VType"],
        "@[cbv] (let F : K = fn (X : VType) => (@(intrinsic(thk))) ((@(intrinsic(ret))) X) that 0)",
        TyckDiagnosticCode::Expressivity,
    );
}

/// **A type operator in a value-type position is a kind error, found by the
/// check.** Once the image test admitted type operators, the arrow and product
/// clauses assumed kinds they no longer got: `Int -> Opt` was *accepted* with
/// `Ret Opt` in the arena, ill-kinded, and only the arena invariants objected — by
/// panicking. These run without the lint on purpose, because the defect was that
/// the check itself accepted them.
#[test]
fn refuses_an_operator_in_a_value_type_position() {
    for source in
        ["fn (f : Int -> Opt) => 0", "fn (f : Opt -> Int) => 0", "fn (p : Int * Opt) => 0"]
    {
        CbvCase::reject_declaring(
            &[OPT],
            &format!("@[cbv] ({source})"),
            TyckDiagnosticCode::KindMismatch,
        );
    }
}

/// **The `let fix` header built its arrow by hand**, so it did not get the
/// checks the arrow clause gained: this was accepted with `Ret Opt` in the arena
/// and the lint panicked. Both now share one construction.
#[test]
fn refuses_an_operator_as_a_recursive_bindings_codomain() {
    CbvCase::reject_declaring(
        &[OPT],
        "@[cbv] (let fix f (x : Int) : Opt = f x in 0)",
        TyckDiagnosticCode::KindMismatch,
    );
}

#[test]
fn admits_a_recursive_binding_at_a_saturated_operator() {
    CbvCase::accept_declaring(&[OPT], "@[cbv] (let fix f (x : Int) : Opt Int = f x in 0)");
}

/// A term's type is a value type; an ascription at an operator is a kind error
/// rather than a mismatch found by comparing an ill-kinded `Ret Opt`.
#[test]
fn refuses_an_ascription_at_an_operator() {
    CbvCase::reject_declaring(&[OPT], "@[cbv] ((5 : Opt))", TyckDiagnosticCode::KindMismatch);
}

/// Its neighbour: the same positions at a value type are fine.
#[test]
fn admits_a_saturated_operator_in_a_value_type_position() {
    CbvCase::accept_declaring(&[OPT], "@[cbv] (let g = fn (f : Int -> Opt Int) => 0 that 0)");
}

/* -------------------------- quantifiers written inside --------------------- */

/// `[[forall (X : K) . A]]v = Thk (forall (X : K) . Ret [[A]]v)`, written inside
/// the block. A block could only *name* a `forall` declared outside, though the
/// former is structural and needs neither a seal nor a query.
#[test]
fn translates_a_quantifier_written_inside_a_block() {
    CbvCase::accept("@[cbv] (fn (p : forall (X : VType) . X -> X) => p Int 7)");
}

/// The type abstraction's synthesized type and the written quantifier's
/// translation agree, up to the binder's name.
#[test]
fn checks_a_type_abstraction_against_a_written_quantifier() {
    CbvCase::accept("@[cbv] ((fn (X : VType) => fn (x : X) => x) : forall (X : VType) . X -> X)");
}

#[test]
fn refuses_a_written_quantifier_over_a_computation_kind() {
    CbvCase::reject(
        "@[cbv] (let P : VType = forall (R : CType) . Int that 0)",
        TyckDiagnosticCode::Expressivity,
    );
}

/// A quantifier's body is a value-type position before the returner goes on.
#[test]
fn refuses_a_written_quantifier_whose_body_is_an_operator() {
    CbvCase::reject(
        "@[cbv] (let P : VType = forall (X : VType) . (fn (Y : VType) => Y) that 0)",
        TyckDiagnosticCode::KindMismatch,
    );
}

/// The binder goes through the same opener as both type abstractions, so the
/// shape guard holds here as well.
#[test]
fn refuses_a_written_quantifier_whose_binder_reads_outside_the_block() {
    CbvCase::reject_beside_outside(
        CbvCase::block(
            "@[cbv] (let P : VType = forall ((X : @[typeof] (@[typeof] outside)) : VType) . X that 0)",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

/// `[[exists (X : K) . A]]v = exists (X : K) . [[A]]v`, written inside, then
/// packed and opened.
#[test]
fn translates_an_existential_written_inside_a_block() {
    CbvCase::accept(
        "@[cbv] (let b = ((Int, 7) : exists (X : VType) . X) that let (X, v) = b that 0)",
    );
}

/// The witness of a written existential escapes no more than a named one's.
#[test]
fn refuses_a_witness_escaping_a_written_existential() {
    CbvCase::reject(
        "@[cbv] (let b = ((Int, 7) : exists (X : VType) . X) that let (X, v) = b that v)",
        TyckDiagnosticCode::EscapingExistential,
    );
}

#[test]
fn refuses_a_written_existential_whose_body_is_an_operator() {
    CbvCase::reject(
        "@[cbv] (let E : VType = exists (X : VType) . (fn (Y : VType) => Y) that 0)",
        TyckDiagnosticCode::KindMismatch,
    );
}

/* ------------------------------ kinds written inside ------------------------- */

/// An **arrow kind written inside** a block, at a type abstraction's binder. The
/// probe deciding whether an annotation is a kind admitted a bare name alone, so
/// `VType -> VType` was read as a source arrow over `VType` and refused as a sort
/// mismatch, while the same binder at a name for it declared outside was accepted.
#[test]
fn admits_an_arrow_kind_written_at_a_type_binder() {
    CbvCase::accept("@[cbv] (fn (F : VType -> VType) => fn (x : F Int) => x)");
}

/// A kind arrow's binder may carry an annotation inside its outermost one, as
/// any binder may, and each is a kind the domain must agree with.
#[test]
fn reads_a_kind_arrow_whose_binder_is_annotated_twice() {
    CbvCase::accept(
        "@[cbv] (let K = pi ((_ : VType) : VType) . VType in fn (F : K) => fn (x : F Int) => x)",
    );
    CbvCase::reject(
        "@[cbv] (let K = pi ((_ : VType -> VType) : VType) . VType in 0)",
        TyckDiagnosticCode::KindMismatch,
    );
}

/// The same kind at a type binding, which the same probe decides.
#[test]
fn admits_an_arrow_kind_written_at_a_type_binding() {
    CbvCase::accept(
        "@[cbv] (let F : VType -> VType -> VType = fn (X : VType) => fn (Y : VType) => X * Y that let p : F Int Unit = (1, ()) that p)",
    );
}

/// And at a quantifier's and an existential's binder, with the quantifier
/// instantiated at an operator written in place.
#[test]
fn admits_an_arrow_kind_written_at_a_quantifier() {
    CbvCase::accept(
        "@[cbv] (fn (p : forall (F : VType -> VType) . F Int -> F Int) => p (fn (X : VType) => X) 5)",
    );
    CbvCase::accept("@[cbv] (fn (p : exists (F : (VType -> VType) -> VType) . Int) => 0)");
}

/// A kind written as an arrow is still a source kind only when its parts are.
#[test]
fn refuses_an_arrow_kind_written_over_a_computation_kind() {
    CbvCase::reject("@[cbv] (fn (R : VType -> CType) => 0)", TyckDiagnosticCode::Expressivity);
}

/// A binding's written kind is compared with its bindee's, as a named one is.
#[test]
fn refuses_a_type_binding_whose_written_arrow_kind_disagrees() {
    CbvCase::reject(
        "@[cbv] (let F : VType -> VType = fn (X : VType) => fn (Y : VType) => X that 0)",
        TyckDiagnosticCode::KindMismatch,
    );
}

/// `K -> A` with `A` a type is the host's spelling of a quantifier over an
/// anonymous binder, `forall (_ : K) . A`, and the block reads it the same way:
/// only the parts of an arrow are probed, so this is a quantifier and not a kind.
#[test]
fn reads_a_kind_to_type_arrow_as_a_quantifier() {
    CbvCase::accept("@[cbv] (let k : VType -> Int = fn (X : VType) => 5 that 0)");
}

/* ------------------------ kinds taken from an expectation -------------------- */

/// A type operator under a declared arrow kind may leave its binder's kind
/// unwritten, as the host's `let F : VType -> VType = fn X => X * X` does. The
/// type translation had no analysis direction, so the host's spelling was refused
/// with a complaint about how a source type may be built.
#[test]
fn takes_a_type_operators_binder_kind_from_a_declared_kind() {
    CbvCase::accept(
        "@[cbv] (let F : VType -> VType = fn X => X * X that let p : F Int = (1, 2) that p)",
    );
    CbvCase::accept(
        "@[cbv] (let P : VType -> VType -> VType = fn X => fn Y => X * Y that let p : P Int Unit = (1, ()) that p)",
    );
}

/// The same at each other place a kind is known first: an instantiation, an
/// operator's argument, and an ascription.
#[test]
fn takes_a_type_operators_binder_kind_from_each_expected_kind() {
    CbvCase::accept(
        "@[cbv] (fn (p : forall (F : VType -> VType) . F Int -> F Int) => p (fn X => X) 5)",
    );
    CbvCase::accept(
        "@[cbv] (let Ap : (VType -> VType) -> VType = fn (F : VType -> VType) => F Int that let x : Ap (fn X => X * X) = (1, 2) that x)",
    );
    CbvCase::accept("@[cbv] (let p : ((fn X => X * X) : VType -> VType) Int = (1, 2) that p)");
}

/// And at a package's witness, whose kind the expected existential's binder gives
/// before the witness is translated, as a quantifier's does at an instantiation.
/// The telescope synthesized the witness and compared kinds afterwards, so the
/// host's `(fn X => X * X, (1, 2))` was refused here.
#[test]
fn takes_a_pack_witness_operators_binder_kind_from_the_existential() {
    CbvCase::accept(
        "@[cbv] (let p = ((fn X => X * X, (1, 2)) : exists (F : VType -> VType) . F Int) that 0)",
    );
}

/// With nothing expected there is nothing to take the kind from.
#[test]
fn refuses_a_type_operator_with_no_kind_to_take() {
    CbvCase::reject(
        "@[cbv] (let p : (fn X => X * X) Int = (1, 2) that p)",
        TyckDiagnosticCode::Expressivity,
    );
}

/// With a kind expected that is not an arrow, an operator stands where a
/// non-operator is expected, which is a kind error. It was reported as a binder
/// missing its kind.
#[test]
fn refuses_a_type_operator_where_a_non_arrow_kind_is_expected() {
    CbvCase::reject("@[cbv] (let F : VType = fn X => X that 0)", TyckDiagnosticCode::KindMismatch);
    CbvCase::reject(
        "@[cbv] (let p = ((fn X => X, 5) : exists (X : VType) . Int) that 0)",
        TyckDiagnosticCode::KindMismatch,
    );
}

/// A kind taken from the expectation is still a source kind, and a written one
/// has to agree with the expected domain.
#[test]
fn refuses_a_taken_computation_kind_or_a_disagreeing_written_one() {
    CbvCase::reject(
        "@[cbv] (let G : CType -> VType = fn R => Thk R that 0)",
        TyckDiagnosticCode::Expressivity,
    );
    CbvCase::reject(
        "@[cbv] (let F : VType -> VType = fn (X : VType -> VType) => Int that 0)",
        TyckDiagnosticCode::KindMismatch,
    );
}

/// A type abstraction may leave its binder's kind unwritten against an expected
/// quantifier, as `fn x => M` leaves its domain against an expected arrow; the
/// host reads `fn X => M` that way too.
#[test]
fn takes_a_type_abstractions_binder_kind_from_an_expected_quantifier() {
    CbvCase::accept(
        "@[cbv] (let id : forall (X : VType) . X -> X = fn X => fn (x : X) => x that id Int 5)",
    );
    CbvCase::accept(
        "@[cbv] (let ap : forall (F : VType -> VType) . F Int -> F Int = fn F => fn (x : F Int) => x that ap (fn X => X) 5)",
    );
}

#[test]
fn refuses_a_type_abstraction_with_no_kind_to_take_or_a_body_that_disagrees() {
    CbvCase::reject(
        "@[cbv] (let id = fn X => fn (x : X) => x that 0)",
        TyckDiagnosticCode::Expressivity,
    );
    CbvCase::reject(
        "@[cbv] (let id : forall (X : VType) . X -> X = fn X => fn (x : Int) => x that 0)",
        TyckDiagnosticCode::TypeMismatch,
    );
}

/* --------------------------- holes and annotations ------------------------- */

/// A term hole is a value hole at the translated type, so it is a component like
/// any other value, and at a function type it stands for the thunk the type
/// translates to. Checked without the arena invariants, which report any
/// unfilled hole as an internal error, the host's included.
#[test]
fn admits_a_hole_wherever_a_type_is_expected() {
    for source in [
        "@[cbv] ((1, (_ : Int)))",
        "@[cbv] ((_ : Int -> Int) 3)",
        "@[cbv] (let f : Int -> Int = _ in f 4)",
    ] {
        SourceCase::assert_accepted(CbvCase::check(&CbvCase::block(source)));
    }
}

/// `@[debug]` inside a block observes the translation of what it annotates,
/// not of the whole block, so it shows what one part of a block became.
#[test]
fn debug_observes_the_translation_of_what_it_annotates() {
    let observed = CbvCase::debug_observations(&CbvCase::block(
        "@[cbv] (let f : Int -> Int * Int = fn (x : Int) => @[debug] (x, x) in f 2)",
    ))
    .unwrap();
    assert_eq!(observed, vec!["ret (x, x) : Ret (Int * Int)".to_owned()]);
}

/// The annotation is transparent to checking: an error inside it is the error
/// it would be without it.
#[test]
fn refuses_inside_a_transparent_annotation_as_without_it() {
    CbvCase::reject(
        "@[cbv] (let f : Int -> Int = fn (x : Int) => @[debug] (x, x) in f 2)",
        TyckDiagnosticCode::TypeMismatch,
    );
}

/* -------------------------------- binders --------------------------------- */

const BOX: &str = "def Box (X : VType) : VType = data | +Box : X end";

/// A single-constructor pattern binds in a `let` as it does in the host's `do`:
/// the core calls `+C p` irrefutable when `C` is its declaration's only
/// constructor and `p` is irrefutable. The binder guard refused every
/// constructor tag outside a match arm, so the block refused what the core
/// admits.
#[test]
fn binds_a_single_constructor_pattern_in_a_let() {
    CbvCase::accept_declaring(
        &[BOX],
        "@[cbv] (let b : Box Int = +Box 3 that let +Box x = b that x)",
    );
    CbvCase::accept_declaring(
        &[BOX],
        "@[cbv] (let bb : Box (Box Int) = +Box (+Box 4) that let +Box (+Box x) = bb that x)",
    );
    CbvCase::accept_declaring(
        &[BOX],
        "@[cbv] (let p : Box Int * Int = (+Box 5, 9) that let (+Box x, y) = p that x)",
    );
}

/// And as a lambda's or a `let fix`'s parameter, which the core's abstraction
/// rule constrains the same way.
#[test]
fn binds_a_single_constructor_pattern_as_a_parameter() {
    CbvCase::accept_declaring(
        &[BOX],
        "@[cbv] (let f = fn (+Box x : Box Int) => x that f (+Box 6))",
    );
    CbvCase::accept_declaring(
        &[BOX],
        "@[cbv] (let fix f (+Box x : Box Int) : Int = x in f (+Box 7))",
    );
}

/// A tag whose declaration has another constructor can fail, and the host's
/// own check refuses it, as it refuses the same `do` outside a block. Without
/// that check the hand-built binding carried a refutable binder.
#[test]
fn refuses_a_constructor_binder_that_can_fail() {
    CbvCase::reject_with_data(
        "@[cbv] (fn (x : Flag) => let +On(n) = x in n)",
        TyckDiagnosticCode::RefutableBinding,
    );
    CbvCase::reject_with_data(
        "@[cbv] (fn (+On(n) : Flag) => n)",
        TyckDiagnosticCode::RefutableBinding,
    );
    CbvCase::reject_with_data(
        "@[cbv] (let fix f (+On(n) : Flag) : Int = n in 0)",
        TyckDiagnosticCode::RefutableBinding,
    );
}

/// `(x : _)` asks for its domain as `x` does. It was handed to the leaf guard and
/// refused, while `fn x => x` and `let x : _ = 5` both checked.
#[test]
fn takes_a_hole_annotated_binders_domain_from_the_expectation() {
    CbvCase::accept("@[cbv] (let F : VType = Int -> Int that ((fn (x : _) => x) : F))");
}

#[test]
fn refuses_a_hole_annotated_binder_with_no_expectation() {
    CbvCase::reject("@[cbv] (fn (x : _) => x)", TyckDiagnosticCode::Expressivity);
}

/// `()` binds nothing and cannot fail, as the core's P-UNIT has it.
#[test]
fn admits_a_unit_binder() {
    CbvCase::accept("@[cbv] ((fn (() : Unit) => 0) ())");
}

#[test]
fn refuses_a_unit_binder_at_a_non_unit_type() {
    CbvCase::reject("@[cbv] (let () = 5 that 7)", TyckDiagnosticCode::TypeMismatch);
}

/* ------------------------------- named fields ----------------------------- */

/// `[[#f :: A]]v = #f :: [[A]]v`. A field name is not a type former, so it
/// takes no shift and the translation commutes with it.
#[test]
fn names_a_record_type() {
    CbvCase::accept_with_record("@[cbv] (param r : Rec that 7)");
}

/// `[[#f = V]]v = #f = [[V]]v`, the value row.
#[test]
fn builds_a_record() {
    CbvCase::accept_with_record("@[cbv] ((#fst = 1, #snd = 2) : Rec)");
}

/// `[[#f = M]]v_term = do a <- [[M]]v; ret (#f = a)`, the term row the value
/// row lacks: a field whose payload is a computation is sequenced.
#[test]
fn builds_a_record_with_a_computation_field() {
    CbvCase::accept_with_record("@[cbv] ((#fst = (fn (x : Int) => x) 1, #snd = 2) : Rec)");
}

#[test]
fn rejects_a_record_field_with_the_wrong_name() {
    CbvCase::reject_with_record(
        "@[cbv] ((#oops = 1, #snd = 2) : Rec)",
        TyckDiagnosticCode::NamedLabelMismatch,
    );
}

/// Reading a record by pattern, which is the route that needs no field
/// resolution: the binder names each field and the payload binds.
#[test]
fn reads_a_record_by_pattern() {
    CbvCase::accept_with_record("@[cbv] (param r : Rec that let (= fst, = snd) = r that fst)");
}

/// `[[M/f]]v_term = do a <- [[M]]v; ret (a/f)`. The row synthesizes, and the
/// field is resolved by the host resolver, which owns the route to a field
/// sitting behind a product prefix.
#[test]
fn projects_a_record_field() {
    CbvCase::accept_with_record("@[cbv] (param r : Rec that r/snd)");
}

/// The head is sequenced, so it need not be a value.
#[test]
fn projects_a_field_from_a_computation_head() {
    CbvCase::accept_with_record(
        "@[cbv] (param l : Lib that let g = (fn (x : Int) => l) that (g 0)/base)",
    );
}

#[test]
fn rejects_a_projection_of_a_missing_field() {
    CbvCase::reject_with_record(
        "@[cbv] (param r : Rec that r/nope)",
        TyckDiagnosticCode::MissingNamedField,
    );
}

/// Built and read in one block, with a field at a translated source arrow —
/// which is what makes the family more than a relabelled product.
#[test]
fn builds_and_reads_a_record_with_a_source_arrow_field() {
    CbvCase::accept_with_record(
        "@[cbv] (let l = ((#id = fn (x : Int) => x, #base = 7) : Lib) that (l/id) (l/base))",
    );
}

/// A package whose telescope binds at a **labeled** kind. The kind translation
/// is the identity, so a label adds nothing to decide and the payload settles
/// whether the binder's kind is one the source has.
#[test]
fn names_a_package_at_a_labeled_kind() {
    CbvCase::accept_with_record("@[cbv] (param p : Pkg that 7)");
}

/// `[[(l = S)]]v = (l = [[S]]v)`, reached through an outside alias for the
/// witness; `writes_a_package_witness_in_place` spells it inside.
#[test]
fn builds_a_package_at_a_labeled_kind() {
    CbvCase::accept_with_record("@[cbv] ((CInt, #zero = 0) : Pkg)");
}

#[test]
fn unpacks_a_package_at_a_labeled_kind() {
    CbvCase::accept_with_record(
        "@[cbv] (param p : Pkg that let (= Counter, = zero) = p that zero)",
    );
}

/// The field route and the unpack route agree on the same package.
#[test]
fn projects_a_field_of_a_package_at_a_labeled_kind() {
    CbvCase::accept_with_record("@[cbv] (param p : Pkg that p/zero)");
}

/* ------------------ named fields and packages written in place ------------------ */

// Everything the section above names, written where it is used instead: the
// forward rows for `#f :: A`, `(l = S)`, `S/l` and the manifest existential,
// and named kinds wherever a block writes a kind.

/// `[[#f :: A]]v = #f :: [[A]]v`, a record type in a binder's annotation.
#[test]
fn writes_a_record_type_in_place() {
    CbvCase::accept("@[cbv] (fn (r : (#fst :: Int, #snd :: Int)) => r/snd)");
}

/// A field's payload is a source type, so a returner is no field type.
#[test]
fn rejects_a_record_field_written_as_a_returner() {
    CbvCase::reject(
        "@[cbv] (fn (r : (#f :: (@(intrinsic(ret))) Int)) => 0)",
        TyckDiagnosticCode::Expressivity,
    );
}

#[test]
fn a_record_type_written_in_place_computes() {
    SourceCase::assert_accepted(CbvCase::run(
        "do case_result <- @[cbv] (
  let R : VType = (#fst :: Int, #snd :: Int) in
  let r : R = (#fst = 3, #snd = 0) in
  r/snd
);
! exit case_result",
    ));
}

/// Field types are structural, so a host record and the same record type
/// written in place are one type.
#[test]
fn a_host_record_meets_the_same_record_type_written_in_place() {
    CbvCase::accept_with_record(
        "@[cbv] (fn (r : Rec) => let s : (#fst :: Int, #snd :: Int) = r in s/fst)",
    );
}

#[test]
fn rejects_a_host_record_at_a_record_type_with_another_field() {
    CbvCase::reject_with_record(
        "@[cbv] (fn (r : Rec) => let s : (#fst :: Int, #two :: Int) = r in s/fst)",
        TyckDiagnosticCode::NamedLabelMismatch,
    );
}

/// `[[(l = S)]]v = (l = [[S]]v)`: the witness `CInt` stood in for above.
#[test]
fn writes_a_package_witness_in_place() {
    CbvCase::accept_with_record("@[cbv] (((#Counter = Int), #zero = 0) : Pkg)");
}

#[test]
fn rejects_a_package_witness_with_another_field_name() {
    CbvCase::reject_with_record(
        "@[cbv] (((#Count = Int), #zero = 0) : Pkg)",
        TyckDiagnosticCode::NamedLabelMismatch,
    );
}

/// A named kind at a type binder, and `S/l` projecting its payload.
#[test]
fn binds_a_type_at_a_named_kind() {
    CbvCase::accept("@[cbv] (fn (X : (#C :: VType)) => fn (x : X/C) => x)");
}

/// A label adds nothing to decide, so its payload must be a source kind.
#[test]
fn rejects_a_named_kind_over_a_computation_kind() {
    CbvCase::reject("@[cbv] (fn (X : (#C :: CType)) => 0)", TyckDiagnosticCode::Expressivity);
}

/// A named kind at a type binding, whose name is then projected.
#[test]
fn a_type_bound_at_a_named_kind_computes() {
    SourceCase::assert_accepted(CbvCase::run(
        "do case_result <- @[cbv] (
  let C : (#C :: VType) = (#C = Int) in
  let x : C/C = 0 in
  x
);
! exit case_result",
    ));
}

#[test]
fn rejects_a_type_binding_whose_named_kind_disagrees() {
    CbvCase::reject(
        "@[cbv] (let C : (#C :: VType) = (#D = Int) in 0)",
        TyckDiagnosticCode::NamedLabelMismatch,
    );
}

/// A named kind in an ascription, read by the same kind judgment.
#[test]
fn ascribes_a_type_at_a_named_kind() {
    CbvCase::accept("@[cbv] (fn (x : (((#C = Int) : (#C :: VType))/C)) => x)");
}

#[test]
fn rejects_an_ascription_whose_named_kind_disagrees() {
    CbvCase::reject(
        "@[cbv] (fn (x : (((#C = Int) : (#D :: VType))/C)) => x)",
        TyckDiagnosticCode::NamedLabelMismatch,
    );
}

/// `#C :: Int` is a field *type*, written with the same node as a named kind,
/// so where a kind belongs it is the wrong sort.
#[test]
fn rejects_a_field_type_where_a_kind_belongs() {
    CbvCase::reject(
        "@[cbv] (fn (x : (((#C = Int) : (#C :: Int))/C)) => x)",
        TyckDiagnosticCode::SortMismatch,
    );
}

#[test]
fn rejects_a_type_projection_of_a_missing_field() {
    CbvCase::reject(
        "@[cbv] (fn (x : ((#C = Int)/D)) => x)",
        TyckDiagnosticCode::MissingNamedTypeField,
    );
}

/// `[[exists (X as S : K) . A]]v = exists (X as [[S]]v : K) . [[A]]v`. The
/// equation survives the translation, so opening the package shows `X = Int`.
#[test]
fn writes_a_manifest_existential_in_place() {
    CbvCase::accept(
        "@[cbv] (fn (p : exists (X as Int : VType) . X) => let (X, x) = p in (x : Int))",
    );
}

/// The same opening at an abstract existential shows nothing.
#[test]
fn rejects_reading_an_abstract_witness_as_its_representation() {
    CbvCase::reject(
        "@[cbv] (fn (p : exists (X : VType) . X) => let (X, x) = p in (x : Int))",
        TyckDiagnosticCode::TypeMismatch,
    );
}

/// The disclosed definition is a source type and is translated as one.
#[test]
fn rejects_a_manifest_definition_outside_the_image() {
    CbvCase::reject(
        "@[cbv] (fn (p : exists (X as (@(intrinsic(thk))) ((@(intrinsic(ret))) Int) : VType) . X) => 0)",
        TyckDiagnosticCode::Expressivity,
    );
}

#[test]
fn a_manifest_package_written_in_place_computes() {
    SourceCase::assert_accepted(CbvCase::run(
        "do case_result <- @[cbv] (
  let M : VType = exists (X as Int : VType) . X in
  let m : M = (Int, 0) in
  let (X, x) = m in
  (x : Int)
);
! exit case_result",
    ));
}

/// `Pkg` itself, whose binder names its field: `#Counter = Rep`.
#[test]
fn writes_a_package_at_a_labeled_kind_in_place() {
    CbvCase::accept(
        "@[cbv] (fn (p : exists (#Counter = ((Rep as Int) : VType)) . (#zero :: Rep)) => p/zero)",
    );
}

#[test]
fn a_host_package_meets_the_same_package_type_written_in_place() {
    CbvCase::accept_with_record(
        "@[cbv] (fn (p : Pkg) => let q : exists (#Counter = ((Rep as Int) : VType)) . (#zero :: Rep) = p in q/zero)",
    );
}

/// The equation is part of the type: another disclosed definition is another
/// type.
#[test]
fn rejects_a_host_package_at_another_disclosed_definition() {
    CbvCase::reject_with_record(
        "@[cbv] (fn (p : Pkg) => let q : exists (#Counter = ((Rep as Unit) : VType)) . (#zero :: Rep) = p in 0)",
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn a_package_at_a_labeled_kind_written_in_place_computes() {
    SourceCase::assert_accepted(CbvCase::run(
        "do case_result <- @[cbv] (
  let P : VType = exists (#Counter = ((Rep as Int) : VType)) . (#zero :: Rep) in
  let p : P = ((#Counter = Int), #zero = 0) in
  let (= Counter, = zero) = p in
  zero
);
! exit case_result",
    ));
}

/// The abstract form takes a field binder too.
#[test]
fn writes_an_abstract_package_at_a_labeled_kind_in_place() {
    CbvCase::accept(
        "@[cbv] (fn (p : exists (#C = (X : VType)) . (#zero :: X)) => let (= C, = zero) = p in 0)",
    );
}

/// A field reached through a package nested under a label is bound by the
/// projection, so the block may use it. The checker joins that route with an
/// alias pattern, which the closedness rule has to see through. A variable the
/// projection did not bind is still refused.
#[test]
fn uses_a_field_projected_through_a_labeled_nested_package() {
    let package =
        "exists (#I = (Item : VType)) . (#box :: exists (#T = (Token : VType)) . (#value :: Int))";
    CbvCase::accept(&format!("@[cbv] (fn (p : {package}) => let (/value) = p in value)"));
    CbvCase::accept(&format!("@[cbv] (fn (p : {package}) => let (/I, /value) = p in value)"));
    SourceCase::assert_rejected(
        CbvCase::check(&format!(
            "let other = 7 in
             do case_result <- @[cbv] (fn (p : {package}) => let (/value) = p in other);
             ret ()"
        )),
        TyckDiagnosticCode::Expressivity,
    );
}

/// **Only a package's binder may name its field.** A term-level type
/// abstraction is built from its witness, `Abs(cs::Ty(witness), …)`, which
/// makes a plain binder at the witness's kind, so the field name would vanish
/// from the type the block builds.
#[test]
fn rejects_a_field_binder_at_a_type_abstraction() {
    CbvCase::reject("@[cbv] (fn (#C = (X : VType)) => 0)", TyckDiagnosticCode::Expressivity);
}

/// A quantifier keeps its binder's pattern, so here the refusal is not forced
/// by the core: it is the source language's, since what instantiating one at a
/// named argument does has not been specified.
#[test]
fn rejects_a_field_binder_at_a_quantifier() {
    CbvCase::reject(
        "@[cbv] (fn (f : forall (#C = (X : VType)) . X) => 0)",
        TyckDiagnosticCode::Expressivity,
    );
}

/// The forms this section admits are pure structure, and each is still
/// guarded: a label, a field binder and a disclosed definition reach only what
/// their parts reach, and a part that reads a term bound outside is refused.
#[test]
fn rejects_a_named_kind_whose_payload_reads_outside() {
    SourceCase::assert_rejected(
        CbvCase::check(
            "let outside = 7 in
             do case_result <- @[cbv] (fn (X : (#C :: @[typeof] outside)) => 0);
             ret ()",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

/// The inner annotation sits under one that declares the kind, so the binder
/// is recognized as a package's and its annotations are settled one by one —
/// the route on which the inner one has to be guarded. Written without the
/// outer kind, the binder declares none, the `exists` is no package, and the
/// shape guard on the whole classifier refuses it before this route is taken.
#[test]
fn rejects_a_field_binder_whose_annotation_reads_outside() {
    SourceCase::assert_rejected(
        CbvCase::check(
            "let outside = 7 in
             do case_result <- @[cbv] (fn (p : exists (#C = ((X : @[typeof] outside) : VType)) . X) => 0);
             ret ()",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

#[test]
fn rejects_a_manifest_definition_that_reads_outside() {
    SourceCase::assert_rejected(
        CbvCase::check(
            "let outside = 7 in
             do case_result <- @[cbv] (fn (p : exists (X as (@[typeof] outside) : VType) . X) => 0);
             ret ()",
        ),
        TyckDiagnosticCode::Expressivity,
    );
}

/* -------------------------------- behaviour ------------------------------- */

/// The translated program is run, not only checked, and its observation is the
/// one a hand-written call-by-push-value program of the same meaning produces.
#[test]
fn an_application_runs_to_its_source_value() {
    SourceCase::assert_accepted(CbvCase::run(
        "do case_result <- @[cbv] ((fn (x : Int) => x) 0);\n! exit case_result",
    ));
}

#[test]
fn the_hand_written_counterpart_runs_the_same_way() {
    SourceCase::assert_accepted(CbvCase::run(
        "do case_result <- (do f <- ret { fn (x : Int) => ret x }; do a <- ret 0; ! f a);\n! exit case_result",
    ));
}

/* --------------------------- inside a monadic block -------------------------- */

impl CbvCase {
    /// A call-by-value block nested in `@[monadic]` is translated twice, the
    /// monadic block reinterpreting the `Ret` and `do` the call-by-value block
    /// emitted. A type the call-by-value block declares is carried
    /// along, transparent or sealed: `lib/tests/cbv/effects.zy` and
    /// `lib/tests/cbv/effects-on-data.zy` run both under a carrier with effects.
    fn nested_in_monadic(declaration: &str) -> String {
        format!(
            "@[monadic] begin
  do n <- @[cbv] ({declaration} that match (+Some 4 : Opt) | +Some n => n | +None _ => 0 end);
  ret n
end"
        )
    }
}

#[test]
fn composes_with_a_monadic_block_over_a_transparent_data_type() {
    SourceCase::assert_accepted(CbvCase::check_monadic(&CbvCase::nested_in_monadic(
        "let Opt : VType = data | +None : Unit | +Some : Int end",
    )));
}

#[test]
fn composes_with_a_monadic_block_over_a_sealed_data_type() {
    SourceCase::assert_accepted(CbvCase::check_monadic(&CbvCase::nested_in_monadic(
        "def Opt : VType = data | +None : Unit | +Some : Int end",
    )));
}

/// What the block refuses is a seal declared outside it, by its own rule: an
/// outside type enters a closed payload by type abstraction.
#[test]
fn refuses_a_seal_declared_outside_a_monadic_block_as_the_block_does() {
    SourceCase::assert_rejected(
        CbvCase::check_monadic(
            "begin
  def Opt : VType = data | +None : Unit | +Some : Int end that
  @[monadic] begin
    let o : Opt = +Some 4 in
    ret 0
  end
end",
        ),
        TyckDiagnosticCode::NotInlinableSeal,
    );
}

/* ------------------------ a parameter's domain ------------------------- */

const OPERATOR: &str = "let F : VType -> VType = fn (X : VType) => X that";

impl CbvCase {
    /// Every place a parameter is written, for one binder and one classifier.
    /// A group of parameters is checked one binder at a time, so its first,
    /// middle, and last positions are separate sites.
    fn parameter_sites(binder: &str, operator: &str, applied: &str) -> [String; 10] {
        [
            format!("fn ({binder} : {operator}) => 0"),
            format!("param ({binder} : {operator}) that 0"),
            format!("let fix g ({binder} : {operator}) : Int = 0 in 0"),
            format!("let fix g (y : Int) ({binder} : {operator}) : Int = 0 in 0"),
            format!("let fix g ({binder} : {operator}) (y : Int) : Int = 0 in 0"),
            format!("fn (y : Int) ({binder} : {operator}) => 0"),
            format!("fn ({binder} : {operator}) (y : Int) => 0"),
            format!("fn (y : Int) ({binder} : {operator}) (z : Int) => 0"),
            format!("fn (G : VType -> VType) => fn ({binder} : {applied}) => 0"),
            format!("let ({binder} : {operator}) = (0 : {operator}) in 0"),
        ]
    }
}

/// A parameter's domain is a value type whatever binds it. The rule was left to
/// the binder, and a wildcard binds nothing and so compares nothing:
/// `fn (_ : F) => 0` was accepted with a type operator as its domain, while
/// `fn (x : F) => 0` was refused. Every binder shape meets every site here,
/// because the bug was in one cell of that table.
#[test]
fn rejects_a_type_operator_as_a_parameter_type_for_every_binder_and_site() {
    for binder in ["x", "_", "()", "(a, b)", "(#l = x)"] {
        for site in CbvCase::parameter_sites(binder, "F", "G") {
            CbvCase::reject(
                &format!("@[cbv] ({OPERATOR} {site})"),
                TyckDiagnosticCode::KindMismatch,
            );
        }
    }
}

/// The neighbour that differs by one thing: the operator applied.
#[test]
fn accepts_an_applied_operator_as_a_parameter_type() {
    for binder in ["x", "_"] {
        let [lambda, param, .., quantified, _] = CbvCase::parameter_sites(binder, "F Int", "G Int");
        for site in [lambda, param, quantified] {
            CbvCase::accept(&format!("@[cbv] ({OPERATOR} {site})"));
        }
    }
}

/* ------------------------- an annotated match arm ----------------------- */

const FLAG: &str = "let Flag : VType = data | +On : Int | +Off : Int end that";

/// Any part of a binder may carry an annotation, a match arm's whole included:
/// the annotation sent the tag beneath it to the rule for plain binders, which
/// has no tag.
#[test]
fn an_annotated_match_arm_is_the_arm_it_annotates() {
    for arms in [
        "(+On x : Flag) => x | (+Off y : Flag) => y",
        "((+On x : Flag) : Flag) => x | +Off y => y",
        "(+On x : Flag) => x | other => 0",
        "+On (x : Int) => x | +Off y => y",
        "(other : Flag) => 0",
    ] {
        CbvCase::accept(&format!("@[cbv] ({FLAG} match (+On 1 : Flag) | {arms} end)"));
    }
}

/// The annotation does not change what the arm binds.
#[test]
fn an_annotated_match_arm_binds_its_payload() {
    CbvCase::run_block(&format!(
        "{FLAG} match (+On 0 : Flag) | (+On x : Flag) => x | (+Off y : Flag) => 1 end"
    ));
}

/// An annotation is compared with the scrutinee, and what it annotates still
/// has to be an arm.
#[test]
fn rejects_an_annotated_match_arm_that_disagrees_or_is_no_arm() {
    for (arms, expected) in [
        ("(+On x : Int) => x | +Off y => y", TyckDiagnosticCode::TypeMismatch),
        ("(+On 0 : Flag) => 0 | +Off y => y", TyckDiagnosticCode::Expressivity),
        ("(+Nope x : Flag) => x | +Off y => y", TyckDiagnosticCode::UnknownDataConstructor),
    ] {
        CbvCase::reject(&format!("@[cbv] ({FLAG} match (+On 1 : Flag) | {arms} end)"), expected);
    }
}

/// An arm is checked against the scrutinee's unrolled type, in a block as in
/// the host, so an annotation naming a seal is a different type there.
#[test]
fn rejects_an_annotated_arm_at_a_sealed_type_as_the_host_does() {
    CbvCase::reject(
        &format!(
            "@[cbv] ({BLOCK_OPT} match (+Some 1 : Opt) | (+Some x : Opt) => x | +None _ => 0 end)"
        ),
        TyckDiagnosticCode::TypeMismatch,
    );
    SourceCase::assert_rejected(
        CbvCase::check(
            "begin
  def Opt : VType = data | +None : Unit | +Some : Int end that
  match (+Some 1 : Opt) | (+Some x : Opt) => ret x | +None _ => ret 0 end
end",
        ),
        TyckDiagnosticCode::TypeMismatch,
    );
}

/* ------------------- a parameter's name in a signature ------------------ */

const SIGNATURE_TYPES: &str = "def Box (X : VType) : VType = data | +Box : X end that
  let Single : VType = data | +Only : Int end that";

/// Desugaring puts the rest of a `let fix` signature in each parameter's scope,
/// so a term parameter's name resolves there, and it names no type. Each of
/// these panicked, reading the name ahead of its binder.
#[test]
fn rejects_a_term_parameter_named_in_a_recursive_signature() {
    for site in [
        "let fix g (x : Int) : x = 0 in 0",
        "let fix g (x : Int) (y : x) : Int = 0 in 0",
        "let fix g (x : Int) : Box x = g x in 0",
        "let fix g (x : Int) : x -> Int = g x in 0",
        "let fix g (x : Int) : Int -> x = g x in 0",
        "let fix g (x : Int) : x * Int = g x in 0",
        "let fix g ((a, b) : Int * Int) : a = 0 in 0",
        "let fix g ((a, b) : Int * Int) (c : a) : Int = 0 in 0",
        "let fix g (x : Int) : forall (Y : x) . Int = g x in 0",
        "let fix g (x : Int) : exists (Y : x) . Int = g x in 0",
        "let fix g (x : Int) : (#a :: x) = g x in 0",
        "let fix g (x : Int) : (x : VType) = 0 in 0",
        "let fix g (x : Int) : (Int : x) = 0 in 0",
        "let fix g (+Only x : Single) : x = 0 in 0",
        "let fix g (X : VType) (x : X) : x = x in 0",
        "let fix g (X : VType) (x : X) (y : x) : X = x in 0",
        "let ! f : pi (x : Int) . x = fix g => fn (x : Int) => 0 in 0",
    ] {
        CbvCase::reject(
            &format!("@[cbv] ({SIGNATURE_TYPES} {site})"),
            TyckDiagnosticCode::SortMismatch,
        );
    }
}

/// The refusal points at the name, not at the binding around it.
#[test]
fn reports_a_term_parameter_in_a_signature_at_the_name() {
    SourceCase::assert_rejected_at(
        CbvCase::check(&CbvCase::block("@[cbv] (let fix g (shown : Int) : shown = 0 in 0)")),
        TyckDiagnosticCode::SortMismatch,
        "shown",
    );
}

/// A type parameter's name is a type, so the same positions admit it.
#[test]
fn accepts_a_type_parameter_named_in_a_recursive_signature() {
    for site in [
        "let fix g (X : VType) : X = g X in 0",
        "let fix g (X : VType) (x : X) : X = x in 0",
        "let fix g (x : Int) (X : VType) : X = g x X in 0",
        "let fix g (x : Int) (X : VType) (y : X) : X = y in 0",
        "let fix g (X : VType) : Box X = g X in 0",
        "let fix g (F2 : VType -> VType) (x : F2 Int) : F2 Int = x in 0",
    ] {
        CbvCase::accept(&format!("@[cbv] ({SIGNATURE_TYPES} {site})"));
    }
}

/* ------------------ what the whole-feature review found ----------------- */

/// The `let !` spelling writes the parameter under the `fix`, so its annotation
/// can name the recursive binder. That name is a term, and reading it panicked
/// because the binder is bound after the annotation is read.
#[test]
fn rejects_the_recursive_name_as_a_parameter_annotation() {
    for site in [
        "let ! f : Int -> Int = fix g => fn (x : g) => 0 in 0",
        "let ! f : forall (X : VType) . Int = fix g => fn (X : g) => 0 in 0",
    ] {
        CbvCase::reject(&format!("@[cbv] ({site})"), TyckDiagnosticCode::SortMismatch);
    }
}

#[test]
fn accepts_the_recursive_name_used_as_a_term() {
    CbvCase::accept("@[cbv] (let ! f : Int -> Int = fix g => fn (x : Int) => g x in 0)");
}

/// An expected quantifier's body reaches the abstraction's body, under the
/// abstraction's own name for the variable.
#[test]
fn a_type_abstraction_takes_the_expected_quantifiers_body() {
    for program in [
        "((fn X => fn x => x) : forall (X : VType) . X -> X)",
        "((fn (X : VType) => fn x => x) : forall (Y : VType) . Y -> Y)",
        "((fn X => fn Y => fn x => fn y => y) : forall (A : VType) . forall (B : VType) . A -> B -> B)",
        "((fn X Y => fn x => fn y => y) : forall (A : VType) (B : VType) . A -> B -> B)",
        "let O : VType -> VType = fn (X : VType) => (X * Int) that ((fn X => fn x => (x, 0)) : forall (A : VType) . A -> O A)",
    ] {
        CbvCase::accept(&format!("@[cbv] ({program})"));
    }
}

/// A constructor cannot synthesize, so one under a type abstraction needs this.
#[test]
fn a_constructor_under_a_type_abstraction_takes_its_type_from_the_quantifier() {
    CbvCase::run_block(
        "let O (X : VType) : VType = data | +None : Unit | +Some : X end that
  let none : forall (A : VType) . O A = fn A => +None () in
  match none Int | +Some n => n | +None _ => 0 end",
    );
}

/// The variables are matched by position: a body that returns the wrong one
/// of two is refused, and the right one is accepted however they are named.
#[test]
fn a_type_abstractions_expected_body_avoids_capture() {
    CbvCase::accept(
        "@[cbv] (((fn Y => fn X => fn (a : Y) => fn (b : X) => a) : forall (X : VType) . forall (Y : VType) . X -> Y -> X))",
    );
    CbvCase::reject(
        "@[cbv] (((fn Y => fn X => fn (a : Y) => fn (b : X) => b) : forall (X : VType) . forall (Y : VType) . X -> Y -> X))",
        TyckDiagnosticCode::TypeMismatch,
    );
}

/// The expectation is binding, not a hint.
#[test]
fn rejects_a_type_abstraction_that_disagrees_with_its_quantifier() {
    CbvCase::reject(
        "@[cbv] (((fn X => fn (x : X) => 0) : forall (X : VType) . X -> X))",
        TyckDiagnosticCode::TypeMismatch,
    );
    CbvCase::reject(
        "@[cbv] (((fn (X : VType -> VType) => 0) : forall (Y : VType) . Int))",
        TyckDiagnosticCode::KindMismatch,
    );
}

/// An expected named kind gives its payload's kind to the operator inside.
#[test]
fn a_named_type_takes_its_payloads_kind_from_the_expected_named_kind() {
    CbvCase::accept("@[cbv] (let F : (#C :: (VType -> VType)) = (#C = fn X => X) that 0)");
    CbvCase::accept(
        "@[cbv] (let F : (#C :: (VType -> VType)) = (#C = fn (X : VType) => X) that 0)",
    );
}

#[test]
fn rejects_a_named_type_that_disagrees_with_its_named_kind() {
    CbvCase::reject(
        "@[cbv] (let F : (#C :: VType) = (#C = fn X => X) that 0)",
        TyckDiagnosticCode::KindMismatch,
    );
    CbvCase::reject(
        "@[cbv] (let F : (#D :: (VType -> VType)) = (#C = fn (X : VType) => X) that 0)",
        TyckDiagnosticCode::NamedLabelMismatch,
    );
}

/* ------------------------ multi-argument functions ------------------------ */

// Consecutive parameters are one function and consecutive arguments one call;
// nesting either one spells the curried form instead. Desugaring keeps both
// shapes and the translation decides between them, so the tables below cross
// every spelling of a function with every spelling of what meets it.

/// A function of `Int`s in each spelling, beside the calls it accepts. `f 1 2`
/// is one call and `(f 1) 2` two; a call passes exactly the parameters of one
/// group, and arguments left over apply to what it returns.
#[test]
fn a_call_passes_exactly_the_parameters_of_one_group() {
    const CALLS: [&str; 5] = ["f 1 2", "f 1", "(f 1) 2", "f 1 2 3", "(f 1 2) 3"];
    for (function, accepted) in [
        ("fn (x : Int) (y : Int) => y", [true, false, false, false, false]),
        ("fn (x : Int) => fn (y : Int) => y", [true, true, true, false, false]),
        ("fn (x : Int) (y : Int) => fn (z : Int) => z", [true, false, false, true, true]),
        ("fn (x : Int) => fn (y : Int) (z : Int) => z", [false, true, false, true, false]),
    ] {
        for (call, accepted) in CALLS.into_iter().zip(accepted) {
            let program = format!("@[cbv] (let f = {function} in {call})");
            if accepted {
                CbvCase::accept(&program);
            } else {
                CbvCase::reject(&program, TyckDiagnosticCode::Expressivity);
            }
        }
    }
}

/// A type argument belongs to the call of its group like any other, though it
/// is not evaluated.
#[test]
fn a_type_argument_is_passed_with_its_group() {
    const CALLS: [&str; 4] = ["f Int 1", "f Int", "(f Int) 1", "f Int 1 2"];
    for (function, accepted) in [
        ("fn (X : VType) (x : X) => x", [true, false, false, false]),
        ("fn (X : VType) => fn (x : X) => x", [true, true, true, false]),
    ] {
        for (call, accepted) in CALLS.into_iter().zip(accepted) {
            let program = format!("@[cbv] (let f = {function} in {call})");
            if accepted {
                CbvCase::accept(&program);
            } else {
                CbvCase::reject(&program, TyckDiagnosticCode::Expressivity);
            }
        }
    }
}

/// A source telescope is one function type, and a chain of them is curried.
#[test]
fn a_telescope_translates_to_one_function_type() {
    for (source, translated) in [
        ("pi (_ : Int) (_ : Int) . Int", "Thk (Int -> Int -> Ret Int)"),
        ("Int -> Int -> Int", "Thk (Int -> Ret (Thk (Int -> Ret Int)))"),
        ("pi (_ : Int) . pi (_ : Int) . Int", "Thk (Int -> Ret (Thk (Int -> Ret Int)))"),
    ] {
        let bodies = CbvCase::type_definition_bodies(
            &CbvCase::block(&format!("@[cbv] (let T : VType = {source} in 0)")),
            "T",
        )
        .unwrap();
        assert_eq!(bodies, [translated], "{source}");
    }
}

/// A function meets an expected type that groups its parameters alike. Where
/// one of the two runs out of parameters first they disagree on arity, which is
/// refused at the function whether or not its binders carry their types.
#[test]
fn a_function_meets_an_expected_type_that_groups_its_parameters_alike() {
    const TELESCOPE: &str = "pi (_ : Int) (_ : Int) . Int";
    const CHAINS: [&str; 3] =
        ["Int -> Int -> Int", "pi (_ : Int) . pi (_ : Int) . Int", "pi (_ : Int) . Int -> Int"];
    for (group, chain) in [
        ("fn (x : Int) (y : Int) => y", "fn (x : Int) => fn (y : Int) => y"),
        ("fn x y => y", "fn x => fn y => y"),
        ("fn (x : _) (y : _) => y", "fn (x : _) => fn (y : _) => y"),
    ] {
        CbvCase::accept(&format!("@[cbv] (({group}) : {TELESCOPE})"));
        SourceCase::assert_rejected_at(
            CbvCase::check(&CbvCase::block(&format!("@[cbv] (({chain}) : {TELESCOPE})"))),
            TyckDiagnosticCode::Expressivity,
            chain,
        );
        for curried in CHAINS {
            CbvCase::accept(&format!("@[cbv] (({chain}) : {curried})"));
            SourceCase::assert_rejected_at(
                CbvCase::check(&CbvCase::block(&format!("@[cbv] (({group}) : {curried})"))),
                TyckDiagnosticCode::Expressivity,
                group,
            );
        }
    }
}

/// Type parameters group as value parameters do: with the quantifiers of one
/// `forall`, or with the binders of a `pi` that mixes the two.
#[test]
fn a_type_parameter_groups_with_the_binders_it_meets() {
    for (function, accepted, refused) in [
        (
            "fn X Y => fn (x : X) => x",
            "forall (A : VType) (B : VType) . A -> A",
            "forall (A : VType) . forall (B : VType) . A -> A",
        ),
        (
            "fn X => fn Y => fn (x : X) => x",
            "forall (A : VType) . forall (B : VType) . A -> A",
            "forall (A : VType) (B : VType) . A -> A",
        ),
        ("fn X x => x", "pi (A : VType) (_ : A) . A", "forall (A : VType) . A -> A"),
        ("fn X => fn x => x", "forall (A : VType) . A -> A", "pi (A : VType) (_ : A) . A"),
    ] {
        CbvCase::accept(&format!("@[cbv] (({function}) : {accepted})"));
        CbvCase::reject(
            &format!("@[cbv] (({function}) : {refused})"),
            TyckDiagnosticCode::Expressivity,
        );
    }
}

/// The `let !` spelling writes a group's parameters apart from its signature,
/// so a later pair may disagree. Only the first pair is compared before the
/// parameters are bound, since a later annotation may name an earlier
/// parameter; a later pair is compared when the function meets its signature.
#[test]
fn compares_every_parameter_of_a_let_bang_group_with_its_signature() {
    CbvCase::accept(
        "@[cbv] (let ! f : pi (X : VType) (_ : X) . Int = fix g => fn (X : VType) (y : X) => 5 in f Int 1)",
    );
    for (signature, parameters) in [
        ("pi (x : Int) (Y : VType) . Int", "(x : Int) (y : Int)"),
        ("pi (x : Int) (_ : Int) . Int", "(x : Int) (Y : VType)"),
        ("pi (X : VType) (_ : X) . Int", "(X : VType) (y : Int)"),
    ] {
        CbvCase::reject(
            &format!("@[cbv] (let ! f : {signature} = fix g => fn {parameters} => 5 in 0)"),
            TyckDiagnosticCode::TypeMismatch,
        );
    }
}

/// Every binder of a group passes the guards the first one does: an annotation
/// that reads a term bound outside the block is refused in any position, and
/// the same annotation written inside is not.
#[test]
fn refuses_an_annotation_reading_outside_in_any_position_of_a_group() {
    const TYPE: &str = "(@[typeof] outside)";
    const KIND: &str = "(@[typeof] (@[typeof] outside))";
    for (site, inside) in [
        (format!("fn (x : {TYPE}) (y : Int) => 0"), "Int"),
        (format!("fn (x : Int) (y : {TYPE}) => 0"), "Int"),
        (format!("fn (x : Int) (y : {TYPE}) (z : Int) => 0"), "Int"),
        (format!("let fix g (x : Int) (y : {TYPE}) : Int = 0 in 0"), "Int"),
        (format!("let T : VType = pi (_ : Int) (_ : {TYPE}) . Int in 0"), "Int"),
        (format!("fn (X : VType) ((Y : {KIND}) : VType) => 0"), "VType"),
        (format!("let T : VType = forall (X : VType) ((Y : {KIND}) : VType) . X in 0"), "VType"),
    ] {
        CbvCase::reject_beside_outside(
            CbvCase::block(&format!("@[cbv] ({site})")),
            TyckDiagnosticCode::Expressivity,
        );
        let inside = site.replace(KIND, inside).replace(TYPE, inside);
        CbvCase::accept(&format!("@[cbv] ({inside})"));
    }
}

/// A telescope's domains are value types in every position, as a parameter's
/// are.
#[test]
fn rejects_a_type_operator_in_any_position_of_a_telescope() {
    for telescope in ["pi (_ : F) (_ : Int) . Int", "pi (_ : Int) (_ : F) . Int"] {
        let program = format!("@[cbv] ({OPERATOR} let T : VType = {telescope} in 0)");
        CbvCase::reject(&program, TyckDiagnosticCode::KindMismatch);
        CbvCase::accept(&program.replace("(_ : F)", "(_ : F Int)"));
    }
}
