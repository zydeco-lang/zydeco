use std::collections::HashSet;
use zydeco_statics::{TyckDiagnosticCode, arena::StaticsArena, syntax::*, validate::LintChecker};
use zydeco_tests::utils::SourceCase;

/// Inspect executable reachability, leaving the source graph available for
/// classifier queries and inspection of unapplied static library interfaces.
struct ResidualValues<'a> {
    statics: &'a StaticsArena,
    values: HashSet<ValueId>,
    computations: HashSet<CompuId>,
}

impl ResidualValues<'_> {
    fn pattern(&self, pattern: VPatId) {
        match &self.statics.vpats[&pattern] {
            | ValuePattern::View(_) => panic!("a view survived static elimination"),
            | ValuePattern::Named(Named(_, inner))
            | ValuePattern::Ctor(Ctor(_, inner))
            | ValuePattern::SCons(ConsN(_, inner)) => self.pattern(*inner),
            | ValuePattern::Alias(Alias(items)) => {
                items.iter().for_each(|item| self.pattern(*item));
            }
            | ValuePattern::VCons(items) => {
                items.iter().for_each(|item| self.pattern(*item));
            }
            | _ => {}
        }
    }

    fn value(&mut self, value: ValueId) {
        if !self.values.insert(value) {
            return;
        }
        match self.statics.values[&value].clone() {
            | Value::ValAbs(_) | Value::ValApp(_) => {
                panic!("a value function survived static elimination")
            }
            | Value::Named(Named(_, inner))
            | Value::Ctor(Ctor(_, inner))
            | Value::SCons(ConsN(_, inner))
            | Value::Proj(Proj(inner, _)) => self.value(inner),
            | Value::Let(Let { binder, bindee, tail }) => {
                self.pattern(binder);
                self.value(bindee);
                self.value(tail);
            }
            | Value::Thunk(Thunk(body)) => self.computation(body),
            | Value::VCons(items) => items.into_iter().for_each(|item| self.value(item)),
            | Value::Hole(_) => panic!("a hole survived checking"),
            | Value::Var(_) | Value::Triv(_) | Value::Lit(_) => {}
        }
    }

    fn computation(&mut self, computation: CompuId) {
        if !self.computations.insert(computation) {
            return;
        }
        match self.statics.compus[&computation].clone() {
            | Computation::VAbs(Abs(binder, body)) | Computation::Fix(Fix(binder, body)) => {
                self.pattern(binder);
                self.computation(body);
            }
            | Computation::TAbs(Abs(_, body))
            | Computation::TApp(App(body, _))
            | Computation::Dtor(Dtor(body, _)) => self.computation(body),
            | Computation::VApp(App(body, argument)) => {
                self.value(argument);
                self.computation(body);
            }
            | Computation::Force(Force(value)) | Computation::Ret(Return(value)) => {
                self.value(value)
            }
            | Computation::Do(Bind { binder, bindee, tail }) => {
                self.pattern(binder);
                self.computation(bindee);
                self.computation(tail);
            }
            | Computation::Let(Let { binder, bindee, tail }) => {
                self.pattern(binder);
                self.value(bindee);
                self.computation(tail);
            }
            | Computation::Match(Match { scrut, arms }) => {
                self.value(scrut);
                arms.into_iter().for_each(|arm| {
                    self.pattern(arm.binder);
                    self.computation(arm.tail);
                });
            }
            | Computation::CoMatch(CoMatch { arms }) => {
                arms.into_iter().for_each(|arm| self.computation(arm.tail));
            }
            | Computation::Hole(_) => panic!("a hole survived checking"),
        }
    }
}

#[test]
fn static_composition_shares_runtime_values_and_retains_source_facts() {
    let source = r#"
begin
  let val duplicate (A : VType) (value : A) : A * A = (value, value) that
  let pair = duplicate (Thk (Ret Int64)) { ret 0 } that
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
    assert!(LintChecker::new(&statics).validate(source_root).is_empty());
    let elaboration = statics.static_elaboration.as_ref().expect("checked sources are elaborated");
    assert_eq!(elaboration.source, source_root);
    let Some(TermAnnId::Compu(root, _)) = elaboration.residual else {
        panic!("the executable needs a residual computation")
    };
    let mut reachable =
        ResidualValues { statics: &statics, values: HashSet::new(), computations: HashSet::new() };
    reachable.computation(root);
    let suspensions = reachable.values.iter().filter(|value| {
        matches!(statics.values[value], Value::Thunk(Thunk(body)) if matches!(statics.compus[&body], Computation::Ret(_)))
    }).count();
    assert_eq!(suspensions, 1, "duplicating a value shares its runtime suspension");
    assert!(reachable.values.iter().any(|value| {
        let Value::VCons(fields) = &statics.values[value] else { return false };
        let [first, second] = fields.as_slice() else { return false };
        matches!((&statics.values[first], &statics.values[second]), (Value::Var(left), Value::Var(right)) if left == right)
    }), "both product fields must refer to one runtime binding");
    assert_ne!(TermAnnId::Compu(root, statics.annotations_compu[&root]), source_root);
    SourceCase::assert_accepted(SourceCase::run(source));
    SourceCase::assert_accepted(SourceCase::lower(source));
}

#[test]
fn static_arguments_and_results_can_be_functions_and_packages() {
    let source = r#"
begin
  let Transform = val pi (_ : Int64) . Int64 that
  let val keep (A : VType) (value : A) : A = value that
  let val twice (function : Transform) : Transform =
    val (value : Int64) => function (function value)
  that
  let Package = exists (A : VType) . (val pi (_ : A) . A) * A that
  let val forward (package : Package) : Package = package that
  let package : Package = (Int64, twice (keep Int64), 0) that
  let (A, transform, value) = forward package that
  let transformed : A = transform value that
  ! exit 0
end
"#;
    SourceCase::assert_accepted(SourceCase::check_linted(source));
    SourceCase::assert_accepted(SourceCase::run(source));
    SourceCase::assert_accepted(SourceCase::lower(source));
}

#[test]
fn dependent_applications_recover_forwarded_and_constructed_package_witnesses() {
    let source = r#"
begin
  let Box = exists (A : VType) . A that
  let val keep (A : VType) (value : A) : A = value that
  let val box (A : VType) (value : A) : Box = (A, value) that
  let val take ((A, value) : Box) : A = value that
  let unbox : Thk (pi ((A, _) : Box) . Ret A) = {
    fn ((A, value) : Box) => ret value
  } that
  let direct : Box = (Int64, 0) that
  let forwarded = keep Box direct that
  let value : Int64 = take forwarded that
  let constructed = box Int64 value that
  let fields = (#box = keep Box constructed, #other = ()) that
  let selected : Int64 = take fields/box that
  do status <- ! unbox (keep Box (box Int64 selected));
  ! exit status
end
"#;
    SourceCase::assert_accepted(SourceCase::check_linted(source));
    SourceCase::assert_accepted(SourceCase::run(source));
    SourceCase::assert_accepted(SourceCase::lower(source));
}

#[test]
fn dependent_product_arguments_preserve_each_forwarded_witness() {
    let source = r#"
begin
  let Box = exists (A : VType) . A that
  let val keep (A : VType) (value : A) : A = value that
  let val pair ((A, first) : Box, (B, second) : Box) : A * B = (first, second) that
  let arguments = keep (Box * Box) ((Int64, 0), (Unit, ())) that
  let result : Int64 * Unit = pair arguments that
  let (status, _) = result that
  ! exit status
end
"#;
    SourceCase::assert_accepted(SourceCase::check_linted(source));
    SourceCase::assert_accepted(SourceCase::run(source));
    SourceCase::assert_accepted(SourceCase::lower(source));
}

#[test]
fn witness_inspection_keeps_runtime_constructor_payloads_opaque() {
    let source = r#"
begin
  let Box = exists (A : VType) . A that
  let Wrapped = data | +Wrap : Int64 end that
  let val box (wrapped : Wrapped) : Box =
    let +Wrap(value) = wrapped in (Int64, value)
  that
  let val take ((A, value) : Box) : A = value that
  do wrapped <- ret (+Wrap(0) : Wrapped);
  let status : Int64 = take (box wrapped) in
  ! exit status
end
"#;
    SourceCase::assert_accepted(SourceCase::check_linted(source));
    SourceCase::assert_accepted(SourceCase::run(source));
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
  let consumer : Thk (Box -> Ret Int64) = {{
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
    let source = r#"
begin
  let val suspend (A : VType) (value : A) : Thk (Ret A) = { ret value } that
  do captured <- ret 0;
  let integer = suspend Int64 captured in
  let unit = suspend Unit () in
  let captured : Int64 = 1 in
  do ignored <- ! unit;
  do status <- ! integer;
  ! exit status
end
"#;
    SourceCase::assert_accepted(SourceCase::check_linted(source));
    SourceCase::assert_accepted(SourceCase::run(source));
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
fn exponentially_composed_value_functions_reach_the_static_reduction_limit() {
    for depth in [10, 16, 20] {
        let mut source = "let val g0 (x : Int64) : Int64 = x in\n".to_owned();
        source.extend((1..=depth).map(|level| {
            let previous = level - 1;
            format!("let val g{level} (x : Int64) : Int64 = g{previous} (g{previous} x) in\n")
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
fn self_application_reports_a_static_reduction_limit() {
    let definition = r#"
      begin
      def Self : VType = val pi (_ : Self) . Unit that
      let loop : Self = val (function : Self) => function function that
    "#;
    SourceCase::assert_accepted(SourceCase::check(&format!("{definition} ! exit 0 end")));
    let source = format!("{definition} let result = loop loop in ! exit 0 end");
    SourceCase::assert_rejected(SourceCase::check(&source), TyckDiagnosticCode::StaticElimination);
}
