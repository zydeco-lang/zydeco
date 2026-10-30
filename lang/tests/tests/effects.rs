use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::{SourceCase, SourceTemplate};

zydeco_tests::check_source!(exception_module, "std/effect/exception.zy");
zydeco_tests::check_source!(state_module, "std/effect/state.zy");
zydeco_tests::check_source!(state_exn_module, "std/effect/state-exn.zy");
zydeco_tests::check_source!(monad_module, "std/effect/monad.zy");

zydeco_tests::e2e_io_source!(
    exception_state,
    "tests/effects/exception-state.zy",
    "",
    "-3 -2\n41 42\n"
);
zydeco_tests::e2e_io_source!(
    state_exception_stack,
    "tests/effects/state-exception-stack.zy",
    "",
    "8 8\n"
);

struct EffectCase;

impl EffectCase {
    fn source(body: &str) -> String {
        let effect = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/std/effect")
            .display()
            .to_string();
        SourceTemplate::parse(include_str!("../templates/effects.zy.in"))
            .and_then(|template| template.render([("effect", effect.as_str()), ("body", body)]))
            .expect("the effects source template and its arguments must agree")
    }

    fn reject(body: &str, code: TyckDiagnosticCode) {
        let source = Self::source(body);
        SourceCase::assert_rejected(SourceCase::check(&source), code);
        SourceCase::assert_rejected(SourceCase::lower(&source), code);
    }
}

#[test]
fn packed_instances_accept_dictionaries_for_one_carrier() {
    let source = EffectCase::source(
        r#"
let _ : WithMonad (IState Int) = (S, s) in
let _ : WithMonad (IException Unit) = (E, e) in
let _ : IStateExn Int Unit = (C, c) in
let _ : WithMonad (IState Int) = (C, #monad = c/monad, #state = c/state) in
let _ : WithMonad (IException Unit) =
  (C, #monad = c/monad, (#exception = c/exception, #catch = c/catch)) in
! exit 0
"#,
    );
    SourceCase::assert_accepted(SourceCase::check_invariants(&source));
    SourceCase::assert_accepted(SourceCase::lower(&source));
}

#[test]
fn carrier_indexed_interfaces_compose_under_one_monad() {
    let source = EffectCase::source(
        r#"
let state_ops : IState Int C = (#state = c/state) in
let exception_ops : IException Unit C = (#exception = c/exception, #catch = c/catch) in
let Composed = WithMonad <| fn (M : VType -> CType) =>
  IState Int M * IException Unit M
in
let ops : IStateExn Int Unit = (C, #monad = c/monad, (state_ops, exception_ops)) in
let _ : Composed = ops in
let run : Thk (pi ((M, _) : Composed) . M Int) = {
  fn ((M, (/monad; /state; /exception; /catch)) : Composed) =>
    ! catch Int { ! exception .throw Int () } { fn _ => ! state .get }
} in
let (M, payload) = ops in
let _ : WithMonad (IState Int) = (M, #monad = payload/monad, #state = payload/state) in
do status <- ! run_state_exn Int Unit Int (Ret Int) 7 { ! run (C, c) }
  { fn _ => ret 1 }
  { fn state answer => match (state, answer) | (7, 7) => ret 0 | _ => ret 1 end };
! exit status
"#,
    );
    SourceCase::assert_accepted(SourceCase::check_invariants(&source));
    SourceCase::assert_accepted(SourceCase::run(&source));
    SourceCase::assert_accepted(SourceCase::lower(&source));
}

#[test]
fn std_helpers_expose_packed_pi_signatures() {
    let source = EffectCase::source(
        r#"
let _ : Thk (forall (S : VType) .
  pi ((M, _) : WithMonad (IState S)) . Thk (S -> Ret S) -> M Unit) = modify in
let _ : Thk (forall (E : VType) . pi ((M, _) : WithMonad (IException E)) .
  forall (A : VType) . Thk Top -> E -> M A) = raise in
let _ : Thk (forall (E : VType) . pi ((M, _) : WithMonad (IException E)) .
  forall (A : VType) . Thk Top -> Thk (M A) -> Thk (E -> M A) -> M A) = catch in
! exit 0
"#,
    );
    SourceCase::assert_accepted(SourceCase::check_invariants(&source));
    SourceCase::assert_accepted(SourceCase::lower(&source));
}

#[test]
fn packed_interfaces_reject_a_named_carrier_wrapper() {
    for (interface, carrier, operations) in [
        ("WithMonad (IState Int)", "S", "s"),
        ("WithMonad (IException Unit)", "E", "e"),
        ("IStateExn Int Unit", "C", "c"),
    ] {
        EffectCase::reject(
            &format!("let _ : {interface} = (#M = {carrier}, {operations}) in ! exit 0"),
            TyckDiagnosticCode::KindMismatch,
        );
    }
}

#[test]
fn state_instance_rejects_a_monad_for_another_carrier() {
    EffectCase::reject(
        r#"
let _ : WithMonad (IState Int) = (S, #monad = e/monad, #state = s/state) in
! exit 0
"#,
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn exception_instance_rejects_throw_for_another_carrier() {
    EffectCase::reject(
        r#"
let _ : WithMonad (IException Unit) =
  (E, #monad = e/monad, (#exception = c/exception, #catch = e/catch)) in
! exit 0
"#,
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn combined_instance_rejects_state_for_another_carrier() {
    EffectCase::reject(
        r#"
let _ : IStateExn Int Unit =
  (C, #monad = c/monad, (#state = s/state, (#exception = c/exception, #catch = c/catch))) in
! exit 0
"#,
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn packed_pi_forwards_one_opening_and_its_result_carrier() {
    let source = EffectCase::source(
        r#"
let Read = pi ((M, _) : WithMonad (IState Int)) . M Int in
let read : Thk Read = { fn ((M, (/state)) : WithMonad (IState Int)) => ! state .get } in
let forward : Thk Read = {
  fn ((M, ops) : WithMonad (IState Int)) => ! read (M, ops)
} in
do (x, s) <- ! forward (S, s) 7;
match (x, s) | (7, 7) => ! exit 0 | _ => ! exit 1 end
"#,
    );
    SourceCase::assert_accepted(SourceCase::check_invariants(&source));
    SourceCase::assert_accepted(SourceCase::run(&source));
    SourceCase::assert_accepted(SourceCase::lower(&source));
}

#[test]
fn packed_pi_rejects_an_unopened_instance() {
    EffectCase::reject(
        r#"
let Read = pi ((M, _) : WithMonad (IState Int)) . M Int in
let read : Thk Read = { fn ((M, (/state)) : WithMonad (IState Int)) => ! state .get } in
let hidden = { fn (ops : WithMonad (IState Int)) => ! read ops } in
! exit 0
"#,
        TyckDiagnosticCode::PackageWitnessesUnavailable,
    );
}
