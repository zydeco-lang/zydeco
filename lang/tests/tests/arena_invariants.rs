//! Mutation tests for the finished-arena invariants.
//!
//! Every test first shows the fixture satisfies the invariants, then corrupts exactly one
//! fact in a clone of the finished arena and asserts that the matching violation is reported.

use zydeco_statics::{
    arena::StaticsArena,
    syntax::{Computation, Fillable, Hole, TermAnnId, Value},
};
use zydeco_tests::{
    invariants::{AnnotatedNode, AnnotationSite, AnnotationSort, ArenaInvariants, ArenaViolation},
    utils::SourceCase,
};

// The `fn` parameters are annotated on purpose: an annotation-free parameter is
// checked before its use sites and the check dies with `MissingSolution`.
const FIXTURE: &str = r#"
begin
  let Boolean =
    data
    | +False : Unit
    | +True : Unit
    end
    that
  let Boxed =
    exists (X : VType) (value : X) . X
    that
  let selected : Boolean = +True() that
  let duplicate = { fn value => ret (value, value) } that
  do pair <- ! duplicate ();
  ret pair
end
"#;

struct Fixture;

impl Fixture {
    /// The checked fixture, which satisfies the invariants before any mutation.
    fn checked() -> (StaticsArena, TermAnnId) {
        let (statics, root) = SourceCase::checked_arena(FIXTURE).expect("fixture must check");
        let violations = ArenaInvariants { statics: &statics }.violations(root);
        assert!(violations.is_empty(), "the unmodified fixture reports {violations:?}");
        (statics, root)
    }

    fn reports(statics: &StaticsArena, root: TermAnnId, expected: ArenaViolation) {
        let violations = ArenaInvariants { statics }.violations(root);
        assert!(violations.contains(&expected), "expected {expected:?}, found {violations:?}");
    }
}

#[test]
fn checked_programs_satisfy_the_invariants() {
    SourceCase::check_invariants(FIXTURE).expect("the fixture checks");
}

#[test]
fn removing_a_value_annotation_is_reported() {
    let (mut statics, root) = Fixture::checked();
    let value =
        *statics.annotations_value.iter().next().expect("a checked arena annotates values").0;
    assert!(statics.annotations_value.remove(&value).is_some());
    Fixture::reports(
        &statics,
        root,
        ArenaViolation::MissingAnnotation { node: AnnotatedNode::Value(value) },
    );
}

#[test]
fn crossing_annotation_sorts_is_reported() {
    let (mut statics, root) = Fixture::checked();
    let value =
        *statics.annotations_value.iter().next().expect("a checked arena annotates values").0;
    let computation_ty =
        *statics.annotations_compu.iter().next().expect("a checked arena annotates computations").1;
    statics.annotations_value.replace_existing(value, computation_ty);
    Fixture::reports(
        &statics,
        root,
        ArenaViolation::WrongSort {
            node: AnnotatedNode::Value(value),
            ty: computation_ty,
            expected: AnnotationSort::VType,
        },
    );
}

#[test]
fn reintroducing_a_type_hole_is_reported() {
    let (mut statics, root) = Fixture::checked();
    let ty = statics.types_pre.iter().next().map(|(ty, _)| ty).expect("a checked arena has types");
    let fill = *statics.fills.iter().next().expect("the fixture infers, so fills exist").0;
    statics.types_pre.replace_existing(ty, Fillable::Fill(fill));
    Fixture::reports(&statics, root, ArenaViolation::UnfilledType { ty, fill });
}

#[test]
fn turning_a_value_into_a_hole_is_reported() {
    let (mut statics, root) = Fixture::checked();
    let value = *statics.values.iter().next().expect("a checked arena allocates values").0;
    // The fixture has no foreign imports, so every hole node is residual.
    statics.values[&value] = Value::Hole(Hole);
    Fixture::reports(&statics, root, ArenaViolation::ResidualHoleValue { value });
}

#[test]
fn turning_a_computation_into_a_hole_is_reported() {
    let (mut statics, root) = Fixture::checked();
    let compu = *statics.compus.iter().next().expect("a checked arena allocates computations").0;
    statics.compus[&compu] = Computation::Hole(Hole);
    Fixture::reports(&statics, root, ArenaViolation::ResidualHoleComputation { compu });
}

#[test]
fn a_hole_root_annotation_is_reported() {
    let (statics, _) = Fixture::checked();
    let fill = *statics.fills.iter().next().expect("the fixture infers, so fills exist").0;
    Fixture::reports(
        &statics,
        TermAnnId::Hole(fill),
        ArenaViolation::ResidualHoleAnnotation { site: AnnotationSite::Root },
    );
}
