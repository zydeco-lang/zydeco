//! Source cases live in `cases/static-elimination/`; Rust retains arena assertions and generated matrices.

use std::collections::HashSet;
use zydeco_ir::{CoreArena, CoreRoot, syntax as ir};
use zydeco_statics::{TyckDiagnosticCode, syntax::*};
use zydeco_tests::invariants::ArenaInvariants;
use zydeco_tests::utils::SourceCase;

/// Inspect executable reachability, leaving the source graph available for
/// classifier queries and inspection of unapplied static library interfaces.
struct ResidualValues<'a> {
    core: &'a CoreArena,
    values: HashSet<ir::ValueId>,
    computations: HashSet<ir::CompuId>,
}

#[test]
fn calculated_values_leave_only_literals_in_the_residual_program() {
    let source = r#"
let add = @(intrinsic(int_add)) in
let sub = @(intrinsic(int_sub)) in
let result = match add 12 4 | 16 => sub 16 16 | _ => 1 end in
! exit result
"#;
    let (statics, root) = SourceCase::checked_arena(source).unwrap();
    assert!(ArenaInvariants { statics: &statics }.violations(root).is_empty());
    assert!(statics.values.iter().any(|(_, node)| matches!(node, Value::Match(_))));
    assert!(statics.values.iter().any(|(_, node)| matches!(node, Value::IntOp(_))));
    let core = statics.core.as_ref().expect("calculation has an executable residual");
    let CoreRoot::Compu(root) = core.root() else {
        panic!("calculation has an executable residual");
    };
    let mut reachable =
        ResidualValues { core: core.arena(), values: HashSet::new(), computations: HashSet::new() };
    reachable.computation(root);
    assert!(reachable.values.iter().any(|value| matches!(
        core.values[value],
        ir::Value::Lit(Literal::Integer(IntegerLiteral::Int(0)))
    )));
}

#[test]
fn known_named_witnesses_bind_the_payload_type_during_static_elimination() {
    let source = include_str!(
        "../../cases/static-elimination/known_named_witnesses_bind_the_payload_type_during_static_elimination.zy"
    );
    SourceCase::assert_accepted(SourceCase::check_invariants(source));
    SourceCase::assert_accepted(SourceCase::lower(source));
}

#[test]
fn pack_preserves_named_type_arguments_for_both_disclosed_and_sealed_evidence() {
    // Keep the explicitly annotated form as the original named-type regression.
    for evidence in ["(= Item as Int)", "(= Item as Int : VType)", "(= Item : VType) is Int"] {
        let source = format!(
            r#"
let package = pack {evidence} where #value = (0 : Int) end in
let (= Item, fields) = package in
let val keep (A : VType) (value : Unit) : Unit = value in
let _ = keep Item () in
! exit fields/value
"#
        );
        SourceCase::assert_accepted(SourceCase::check_invariants(&source));
        SourceCase::assert_accepted(SourceCase::run(&source));
        SourceCase::assert_accepted(SourceCase::lower(&source));
    }
}

impl ResidualValues<'_> {
    fn value(&mut self, value: ir::ValueId) {
        if !self.values.insert(value) {
            return;
        }
        match &self.core.values[value] {
            | ir::Value::Ctor { payload, .. } | ir::Value::Proj { head: payload, .. } => {
                self.value(*payload)
            }
            | ir::Value::Let(Let { binder: _, bindee, tail }) => {
                self.value(*bindee);
                self.value(*tail);
            }
            | ir::Value::Thunk(Thunk(body)) => self.computation(*body),
            | ir::Value::VCons(items) => items.items.iter().for_each(|item| self.value(*item)),
            | ir::Value::Hole(_) => panic!("a hole survived checking"),
            | ir::Value::Var(_)
            | ir::Value::Triv(_)
            | ir::Value::Lit(_)
            | ir::Value::Foreign(_) => {}
        }
    }

    fn computation(&mut self, computation: ir::CompuId) {
        if !self.computations.insert(computation) {
            return;
        }
        match &self.core.compus[computation] {
            | ir::Computation::VAbs(Abs(_, body))
            | ir::Computation::Fix { body, .. }
            | ir::Computation::Dtor { body, .. } => self.computation(*body),
            | ir::Computation::VApp(App(body, argument)) => {
                self.value(*argument);
                self.computation(*body);
            }
            | ir::Computation::Force(Force(value)) | ir::Computation::Ret(Return(value)) => {
                self.value(*value)
            }
            | ir::Computation::Do(Bind { binder: _, bindee, tail }) => {
                self.computation(*bindee);
                self.computation(*tail);
            }
            | ir::Computation::Let(Let { binder: _, bindee, tail }) => {
                self.value(*bindee);
                self.computation(*tail);
            }
            | ir::Computation::Match(Match { scrut, arms }) => {
                self.value(*scrut);
                arms.iter().for_each(|arm| self.computation(arm.tail));
            }
            | ir::Computation::CoMatch { arms } => {
                arms.iter().for_each(|arm| self.computation(arm.tail));
            }
            | ir::Computation::Hole(_) => panic!("a hole survived checking"),
        }
    }
}

#[test]
fn static_composition_shares_runtime_values_and_retains_source_facts() {
    let source = r#"
begin
  let val duplicate (A : VType) (value : A) : A * A = (value, value) that
  let pair = duplicate (Thk (Ret Int)) { ret 0 } that
  let (first, second) = pair that
  do left <- ! first;
  do right <- ! second;
  ! exit right
end
"#;
    let (statics, source_root) =
        SourceCase::checked_arena(source).expect("static composition checks");
    assert!(
        statics.values.iter().any(|(_, node)| matches!(node, Value::ValAbs(_))),
        "source abstractions remain available for inspection"
    );
    assert!(
        statics.values.iter().any(|(_, node)| matches!(node, Value::ValApp(_))),
        "source applications remain available for classifier queries"
    );
    assert!(ArenaInvariants { statics: &statics }.violations(source_root).is_empty());
    let core = statics.core.as_ref().expect("checked sources are elaborated");
    let CoreRoot::Compu(root) = core.root() else {
        panic!("the executable needs a residual computation")
    };
    let mut reachable =
        ResidualValues { core: core.arena(), values: HashSet::new(), computations: HashSet::new() };
    reachable.computation(root);
    let suspensions = reachable
        .values
        .iter()
        .filter(|&&value| {
            matches!(core.values[value], ir::Value::Thunk(Thunk(body)) if matches!(core.compus[body], ir::Computation::Ret(_)))
        })
        .count();
    assert_eq!(suspensions, 1, "duplicating a value shares its runtime suspension");
    let forced_vars = reachable
        .computations
        .iter()
        .filter_map(|&compu| match core.compus[compu] {
            | ir::Computation::Force(Force(target)) => match core.values[target] {
                | ir::Value::Var(var) => Some(var),
                | _ => None,
            },
            | _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(forced_vars.len(), 3, "two user forces plus exit");
    assert_eq!(
        forced_vars
            .iter()
            .filter(|var| forced_vars.iter().filter(|other| *other == *var).count() == 2)
            .count(),
        2,
        "both forced uses must refer to the single shared runtime binding"
    );
    SourceCase::assert_accepted(SourceCase::run(source));
    SourceCase::assert_accepted(SourceCase::lower(source));
}

#[test]
fn static_arguments_and_results_can_be_functions_and_packages() {
    let source = include_str!(
        "../../cases/static-elimination/static_arguments_and_results_can_be_functions_and_packages.zy"
    );
    SourceCase::assert_accepted(SourceCase::check_invariants(source));
    SourceCase::assert_accepted(SourceCase::lower(source));
}

#[test]
fn inferred_factory_fields_specialize_solved_result_holes() {
    let source = include_str!(
        "../../cases/static-elimination/inferred_factory_fields_specialize_solved_result_holes.zy"
    );
    SourceCase::assert_accepted(SourceCase::check_invariants(source));
    SourceCase::assert_accepted(SourceCase::lower(source));
}

#[test]
fn dependent_applications_recover_forwarded_and_constructed_package_witnesses() {
    let source = include_str!(
        "../../cases/static-elimination/dependent_applications_recover_forwarded_and_constructed_package_witnesses.zy"
    );
    SourceCase::assert_accepted(SourceCase::check_invariants(source));
    SourceCase::assert_accepted(SourceCase::lower(source));
}

#[test]
fn dependent_product_arguments_preserve_each_forwarded_witness() {
    let source = include_str!(
        "../../cases/static-elimination/dependent_product_arguments_preserve_each_forwarded_witness.zy"
    );
    SourceCase::assert_accepted(SourceCase::check_invariants(source));
    SourceCase::assert_accepted(SourceCase::lower(source));
}

#[test]
fn witness_inspection_keeps_runtime_constructor_payloads_opaque() {
    let source = include_str!(
        "../../cases/static-elimination/witness_inspection_keeps_runtime_constructor_payloads_opaque.zy"
    );
    SourceCase::assert_accepted(SourceCase::check_invariants(source));
    SourceCase::assert_accepted(SourceCase::lower(source));
}

#[test]
fn static_forwarding_does_not_reveal_runtime_package_witnesses() {
    for application in [
        "take (keep Box hidden)",
        "! unbox (keep Box hidden)",
        "take (repack hidden)",
        "! unbox (repack hidden)",
    ] {
        let use_package = if application.starts_with('!') {
            format!("do _ <- {application}; ret 0")
        } else {
            format!("let _ = {application} in ret 0")
        };
        let source = format!(
            r#"
begin
  let Box = exists (A : VType) . A that
  let val keep (A : VType) (value : A) : A = value that
  let val repack (package : Box) : Box =
    let (A, value) = package in (A, value)
  that
  let val take ((A, value) : Box) : A = value that
  let unbox : Thk (pi ((A, _) : Box) . Ret A) = {{
    fn ((A, value) : Box) => ret value
  }} that
  let consumer : Thk (Box -> Ret Int) = {{
    fn (hidden : Box) => {use_package}
  }} that
  ! exit 0
end
"#
        );
        for result in
            [SourceCase::check(&source), SourceCase::run(&source), SourceCase::lower(&source)]
        {
            SourceCase::assert_rejected(result, TyckDiagnosticCode::PackageWitnessesUnavailable);
        }
    }
}

#[test]
fn specialization_keeps_type_arguments_and_lexical_captures_distinct() {
    let source = include_str!(
        "../../cases/static-elimination/specialization_keeps_type_arguments_and_lexical_captures_distinct.zy"
    );
    SourceCase::assert_accepted(SourceCase::check_invariants(source));
    SourceCase::assert_accepted(SourceCase::lower(source));
}

#[test]
fn runtime_boundaries_reject_static_requirements_consistently() {
    let cases = [
        "do escaped <- ret keep; ! exit 0",
        "let stored = { ret keep } in ! exit 0",
        "let Stored = data | +Store : Function end in do escaped <- ret (+Store(keep) : Stored); ! exit 0",
        "let Box = exists (A : VType) . A in do escaped <- ret ((Function, keep) : Box); ! exit 0",
        "let apply : Thk (Function -> Ret Unit) = { fn (function : Function) => ret (function ()) } in ! exit 0",
        "let apply : Thk (forall (A : VType) . Function -> Ret Unit) = { fn (A : VType) (function : Function) => ret (function ()) } in ! exit 0",
        "let produce : Thk (forall (A : VType) . Ret (Thk (A -> Ret Unit))) = { fn (A : VType) => ret { fn (value : A) => ret () } } in do escaped <- ! produce Function; ! exit 0",
    ];
    for case in cases {
        let source = format!(
            "let Function = val pi (_ : Unit) . Unit in let val keep (value : Unit) : Unit = value in {case}"
        );
        for result in
            [SourceCase::check(&source), SourceCase::run(&source), SourceCase::lower(&source)]
        {
            SourceCase::assert_rejected(result, TyckDiagnosticCode::StaticElimination);
        }
    }
}

#[test]
fn aliases_of_partial_value_applications_preserve_bound_arguments() {
    let source = include_str!(
        "../../cases/static-elimination/aliases_of_partial_value_applications_preserve_bound_arguments.zy"
    );
    SourceCase::assert_accepted(SourceCase::check_invariants(source));
    SourceCase::assert_accepted(SourceCase::lower(source));
}

#[test]
fn exponentially_composed_value_functions_reach_the_static_reduction_limit() {
    for depth in [10, 16, 20] {
        let mut source = "let val g0 (x : Int) : Int = x in\n".to_owned();
        source.extend((1..=depth).map(|level| {
            let previous = level - 1;
            format!("let val g{level} (x : Int) : Int = g{previous} (g{previous} x) in\n")
        }));
        source += &format!("let result = g{depth} 0 in ! exit result");
        if depth == 10 {
            SourceCase::assert_accepted(SourceCase::lower(&source));
        } else {
            SourceCase::assert_rejected(
                SourceCase::lower(&source),
                TyckDiagnosticCode::StaticElimination,
            );
        }
    }
}

#[test]
fn reified_polymorphic_members_of_opaque_type_families_lint_cleanly() {
    let source = include_str!(
        "../../cases/static-elimination/reified_polymorphic_members_of_opaque_type_families_lint_cleanly.zy"
    );
    SourceCase::assert_accepted(SourceCase::check_invariants(source));
}
