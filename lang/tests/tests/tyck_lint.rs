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
    ArenaAccess,
    arena::StaticsArena,
    syntax::{Fillable, Hole, TermAnnId, Thunk, Type, Value},
    validate::{LintChecker, LintError, LintNode, LintSite, LintSort},
};
use zydeco_tests::utils::SourceCase;

#[test]
fn copied_codata_annotations_agree_but_changed_results_are_rejected() {
    let source = r#"
let C (R : CType) = codata | .go : Thk R -> R end in
let make : Thk (forall (R : CType) . C R) = {
  fn R => comatch | .go next => ! next end
} in
let left : Thk (C (Ret Int64)) = { ! make (Ret Int64) } in
let right : Thk (C (Ret Unit)) = { ! make (Ret Unit) } in
! left
"#;
    let (mut statics, root) = SourceCase::checked_arena(source).unwrap();
    assert!(LintChecker::new(&statics).validate(root).is_empty());
    let variants: Vec<_> = statics
        .annotations_compu
        .iter()
        .filter_map(|(compu, ty)| {
            let Some(Type::CoData(codata)) = statics.normalized_at(*ty) else { return None };
            let payload =
                statics.codatas[codata].get(&zydeco_statics::syntax::DtorName(".go".into()))?;
            let Some(Type::Arrow(zydeco_statics::syntax::Arrow(_, result))) =
                statics.normalized_at(payload)
            else {
                return None;
            };
            let Some(Type::App(zydeco_statics::syntax::App(head, payload))) =
                statics.normalized_at(*result)
            else {
                return None;
            };
            if !matches!(statics.normalized_at(*head), Some(Type::Ret(_))) {
                return None;
            }
            let returns_integer = match statics.normalized_at(*payload) {
                | Some(Type::Primitive(_)) => true,
                | Some(Type::Unit(_)) => false,
                | _ => return None,
            };
            Some((*compu, *ty, returns_integer))
        })
        .collect();
    let TermAnnId::Compu(compu, _) = root else { panic!("the fixture is a computation") };
    let (_, wrong, _) = variants.iter().find(|(_, _, integer)| !*integer).unwrap();
    statics.annotations_compu.replace_existing(compu, *wrong);
    assert_reports(&statics, root, |error| {
        matches!(error, LintError::AnnotationDisagreement { site: LintSite::Root, .. })
    });
}

#[test]
fn named_data_shapes_compare_structure_and_still_reject_different_payloads() {
    let source = r#"
let I = @(intrinsic(i64)) in
let item : data | +Ok : I | +Err : Unit end = +Ok(0) in
let named : (#item :: data | +Ok : I | +Err : Unit end) = (#item = item) in
let wrong : (#item :: data | +Ok : Unit | +Err : Unit end) = (#item = +Ok()) in
ret (named, wrong)
"#;
    let (mut statics, root) = SourceCase::checked_arena(source).unwrap();
    assert!(LintChecker::new(&statics).validate(root).is_empty());
    let named: Vec<_> = statics
        .values
        .iter()
        .filter_map(|(id, node)| matches!(node, Value::Named(_)).then_some(*id))
        .collect();
    let pair = named
        .iter()
        .find_map(|left| {
            named.iter().find_map(|right| {
                let left_ty = statics.annotations_value[left];
                let right_ty = statics.annotations_value[right];
                let Some(Type::Label(zydeco_statics::syntax::Label(_, left_data))) =
                    statics.normalized_at(left_ty)
                else {
                    return None;
                };
                let Some(Type::Label(zydeco_statics::syntax::Label(_, right_data))) =
                    statics.normalized_at(right_ty)
                else {
                    return None;
                };
                let Some(Type::Data(left_data)) = statics.normalized_at(*left_data) else {
                    return None;
                };
                let Some(Type::Data(right_data)) = statics.normalized_at(*right_data) else {
                    return None;
                };
                let left_payload = statics.datas[left_data]
                    .get(&zydeco_statics::syntax::CtorName("+Ok".into()))?;
                let right_payload = statics.datas[right_data]
                    .get(&zydeco_statics::syntax::CtorName("+Ok".into()))?;
                (matches!(statics.normalized_at(left_payload), Some(Type::Primitive(_)))
                    && matches!(statics.normalized_at(right_payload), Some(Type::Unit(_))))
                .then_some((*left, right_ty))
            })
        })
        .expect("fixture contains labeled data with distinct payload types");
    statics.annotations_value.replace_existing(pair.0, pair.1);
    assert_reports(&statics, root, |error| {
        matches!(error,
        LintError::TypeMismatch { node: LintNode::Value(value), .. } if *value == pair.0)
    });
}

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
    // elaboration. Source value definitions depend on their annotation entry;
    // generated residual definitions additionally have a generated_defs entry.
    let (value, def) = statics
        .values
        .iter()
        .find_map(|(value, node)| match node {
            | Value::Var(def) if statics.generated_defs.get(def).is_none() => Some((*value, *def)),
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
    let thunk = statics
        .values
        .iter()
        .find_map(|(value, node)| match node {
            | Value::Thunk(Thunk(_)) => Some(*value),
            | _ => None,
        })
        .expect("the fixture suspends a computation");
    // Pick a concrete non-thunk shape. Choosing an arbitrary value type can
    // select this thunk's existing annotation and turn the mutation into a no-op.
    let wrong_ty = statics
        .annotations_value
        .iter()
        .map(|(_, ty)| *ty)
        .find(|ty| matches!(statics.normalized_annotation_at(*ty), Some(Type::Unit(_))))
        .expect("the arena has a unit type distinct from the thunk shape");
    assert_ne!(
        wrong_ty, statics.annotations_value[&thunk],
        "the selected annotation must change the thunk's recorded shape"
    );
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
