//! Mutation tests for the type lint.
//!
//! Every test first proves the fixture lints clean, then corrupts exactly one
//! fact in a clone of the finished arena and asserts the lint reports the
//! matching error variant. A lint that cannot be shown to catch a seeded
//! defect has no credibility, per `docs/proposals/tyck-lint.md`.
//!
//! `DanglingReference` has no dedicated mutation: removing a node needs a
//! `remove` on `ArenaSparse`, which the container does not expose, and
//! identifiers cannot be fabricated through the public arena API. The variant
//! is exercised indirectly whenever a reference target loses its row.

use zydeco_statics::{
    arena::StaticsArena,
    syntax::{CompuId, Fillable, Hole, Kind, TermAnnId, Thunk, Value, ValueId},
    validate::{LintChecker, LintError, LintNode, LintSite, LintSort},
};
use zydeco_tests::utils::SourceCase;

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

/// The fixture must lint clean before any mutation is meaningful.
fn linted_fixture() -> (StaticsArena, TermAnnId) {
    let (statics, root) = SourceCase::checked_arena(FIXTURE).expect("fixture must check");
    assert!(
        LintChecker::new(&statics).validate(root).is_empty(),
        "fixture must satisfy the lint invariants before mutation"
    );
    (statics, root)
}

fn assert_reports(statics: &StaticsArena, root: TermAnnId, matches: impl Fn(&LintError) -> bool) {
    let errors = LintChecker::new(statics).validate(root);
    assert!(
        errors.iter().any(matches),
        "expected the seeded corruption to be reported, but the lint found: {errors:?}"
    );
}

#[test]
fn clean_programs_pass_the_gated_check() {
    SourceCase::check_linted(FIXTURE).expect("a consistent arena must lint clean");
}

#[test]
fn removing_a_value_annotation_is_reported() {
    let (mut statics, root) = linted_fixture();
    let value = *statics
        .annotations_value
        .iter()
        .next()
        .expect("a checked arena records value annotations")
        .0;
    assert!(
        statics.annotations_value.remove(&value).is_some(),
        "the picked annotation row must exist"
    );
    assert_reports(
        &statics,
        root,
        |error| matches!(error, LintError::MissingAnnotation { node } if *node == LintNode::Value(value)),
    );
}

#[test]
fn crossing_annotation_sorts_is_reported() {
    let (mut statics, root) = linted_fixture();
    let value = *statics
        .annotations_value
        .iter()
        .next()
        .expect("a checked arena records value annotations")
        .0;
    let computation_ty = *statics
        .annotations_compu
        .iter()
        .next()
        .expect("a checked arena records computation annotations")
        .1;
    statics.annotations_value.replace_existing(value, computation_ty);
    assert_reports(&statics, root, |error| {
        matches!(
            error,
            LintError::AnnotationSort {
                node: LintNode::Value(found),
                expected: LintSort::VType,
                ..
            } if *found == value
        )
    });
}

#[test]
fn reintroducing_a_type_hole_is_reported() {
    let (mut statics, root) = linted_fixture();
    let ty =
        statics.types_pre.iter().next().map(|(ty, _)| ty).expect("a checked arena allocates types");
    let fill =
        *statics.fills.iter().next().expect("the fixture performs inference, so fills exist").0;
    statics.types_pre.replace_existing(ty, Fillable::Fill(fill));
    assert_reports(
        &statics,
        root,
        |error| matches!(error, LintError::UnfilledType { ty: found, .. } if *found == ty),
    );
}

#[test]
fn dropping_a_variable_annotation_is_reported() {
    let (mut statics, root) = linted_fixture();
    // Bound type variables are referenced through abstract witnesses after
    // elaboration, so a value variable is the dependable reference site.
    let (value, def) = statics
        .values
        .iter()
        .find_map(|(value, node)| match node {
            | Value::Var(def) => Some((*value, *def)),
            | _ => None,
        })
        .expect("the fixture references its bound variables");
    statics.annotations_var.remove(&def);
    assert_reports(&statics, root, |error| {
        matches!(
            error,
            LintError::UnresolvedDef {
                referenced_by: LintNode::Value(site),
                def: found,
            } if *site == value && *found == def
        )
    });
}

#[test]
fn desynchronizing_the_root_annotation_is_reported() {
    let (mut statics, root) = linted_fixture();
    let other_ty = statics
        .annotations_value
        .iter()
        .map(|(_, ty)| *ty)
        .find(|candidate| match root {
            | TermAnnId::Value(_, recorded) => *candidate != recorded,
            | _ => true,
        })
        .expect("a checked arena has more than one value annotation");
    match root {
        | TermAnnId::Value(value, _) => {
            statics.annotations_value.replace_existing(value, other_ty);
        }
        | TermAnnId::Compu(compu, _) => {
            statics.annotations_compu.replace_existing(compu, other_ty);
        }
        | _ => panic!("the fixture root is a value or computation term"),
    }
    assert_reports(&statics, root, |error| {
        matches!(error, LintError::AnnotationDisagreement { site: LintSite::Root, .. })
    });
}

#[test]
fn turning_a_value_into_a_hole_is_reported() {
    let (mut statics, root) = linted_fixture();
    let value = *statics.values.iter().next().expect("a checked arena allocates values").0;
    // The fixture has no foreign imports, so every hole node is residual.
    statics.values[&value] = Value::Hole(Hole);
    assert_reports(
        &statics,
        root,
        |error| matches!(error, LintError::ResidualHoleValue { value: found } if *found == value),
    );
}

#[test]
fn desynchronizing_a_thunk_shape_is_reported() {
    let (mut statics, root) = linted_fixture();
    // A suspension with a closed payload exercises a constructor-shape
    // judgment: the recorded annotation must be `Thk` applied to exactly
    // the payload computation's type.
    let thunks: Vec<(ValueId, CompuId)> = statics
        .values
        .iter()
        .filter_map(|(value, node)| match node {
            | Value::Thunk(Thunk(body)) => Some((*value, *body)),
            | _ => None,
        })
        .collect();
    let Some((thunk, body)) = thunks.first().copied() else {
        panic!("the fixture suspends a computation");
    };
    let wrong_ty = statics
        .annotations_value
        .iter()
        .map(|(_, ty)| *ty)
        .find(|ty| {
            let Some(kind) = statics.type_kind_at(*ty) else {
                return false;
            };
            matches!(statics.normalized_kind_at(kind), Some(Kind::VType(_)))
                && *ty != statics.annotations_compu[&body]
        })
        .expect("the arena has a closed value type distinct from the payload");
    statics.annotations_value.replace_existing(thunk, wrong_ty);
    assert_reports(&statics, root, |error| {
        matches!(
            error,
            LintError::TypeMismatch {
                node: LintNode::Value(found),
                ..
            } if *found == thunk
        )
    });
}
