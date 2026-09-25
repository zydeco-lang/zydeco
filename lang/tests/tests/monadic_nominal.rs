use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::SourceCase;

zydeco_tests::e2e_sources!({
    state_recursive_protocol => "tests/monadic/nominal-types.zy",
});

struct MonadicCase;

impl MonadicCase {
    fn seal_count(body: &str) -> usize {
        let (arena, _) = SourceCase::checked_arena(&Self::source(body))
            .unwrap_or_else(|error| panic!("expected a checked arena: {error}"));
        arena.seals.iter().count()
    }
    fn source(body: &str) -> String {
        let basis =
            std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../lib/std/control/monad.zy");
        format!(
            r#"
let monadic_basis = @(import("{}")) in
let (/Monad; /Algebra) = builtin |> monadic_basis in
{body}
"#,
            basis.display()
        )
    }

    fn ret_program(body: &str) -> String {
        Self::source(&format!(
            r#"
let mo : Thk (Monad Ret) = {{
  comatch
  | .return A value => ret value
  | .bind A B computation continuation =>
      do value <- ! computation;
      ! continuation value
  end
}} in
do result <- (@[monadic] {body}) Ret mo;
do status <- ! int/sub result 1;
! exit status
"#
        ))
    }

    fn run(body: &str) {
        let source = Self::ret_program(body);
        SourceCase::assert_accepted(SourceCase::check_linted(&source));
        SourceCase::assert_accepted(SourceCase::run(&source));
        SourceCase::assert_accepted(SourceCase::lower(&source));
    }
}

#[test]
fn local_nominal_data_preserves_constructors_and_patterns() {
    MonadicCase::run(
        r#"
begin
  def Box : VType = data | +Box : Int end in
  let item = (+Box 1 : Box) in
  match item | +Box n => ret n end
end
"#,
    );
}

#[test]
fn recursive_type_constructor_reuses_its_family() {
    MonadicCase::run(
        r#"
begin
  def List (A : VType) : VType =
    data | +Nil : Unit | +Cons : A * List A end
  that
  let items = (+Cons (1, +Nil ()) : List Int) in
  match items
  | +Nil _ => ret 0
  | +Cons (head, tail) =>
      match tail | +Nil _ => ret head | +Cons _ => ret 0 end
  end
end
"#,
    );
}

#[test]
fn recursive_codata_uses_lazy_algebra_evidence() {
    MonadicCase::run(
        r#"
begin
  def Stream : CType = codata | .head : Ret Int | .tail : Stream end that
  let stream : Thk Stream = {
    fix self =>
      do _ <- ret ();
      comatch
      | .head => ret 1
      | .tail => ! self
      end
  } in
  ! stream .tail .tail .head
end
"#,
    );
}

#[test]
fn mutually_recursive_data_preserves_nominal_edges() {
    MonadicCase::run(
        r#"
begin
  def Left : VType = data | +Done : Int | +Right : Right end that
  def Right : VType = data | +Left : Left end that
  let item = (+Right (+Left (+Done 1)) : Left) in
  match item
  | +Done n => ret n
  | +Right (+Left tail) =>
      match tail | +Done n => ret n | +Right _ => ret 0 end
  end
end
"#,
    );
}

#[test]
fn mutually_recursive_codata_shares_its_dictionary() {
    MonadicCase::run(
        r#"
begin
  def Left : CType = codata | .right : Right | .done : Ret Int end that
  def Right : CType = codata | .left : Left end that
  let left : Thk Left = {
    fix self => do _ <- ret ();
    comatch
    | .right => do _ <- ret (); comatch | .left => ! self end
    | .done => ret 1
    end
  } in
  ! left .right .left .right .left .done
end
"#,
    );
}

#[test]
fn local_declarations_abstract_their_free_type_dependencies() {
    MonadicCase::run(
        r#"
begin
  let make = { fn (A : VType) (B : CType) (item : A) (finish : Thk (A -> B)) => begin
    def Box : VType = data | +Box : A end in
    def Stream : CType = codata | .next : Stream | .done : B end that
    let boxed = (+Box item : Box) in
    let stream : Thk Stream = {
      fix self => do _ <- ret ();
      comatch
      | .next => ! self
      | .done => match boxed | +Box value => ! finish value end
      end
    } in
    ! stream .next .done
  end } in
  ! make Int (Ret Int) 1 { fn value => ret value }
end
"#,
    );
}

#[test]
fn higher_kinded_dependencies_supply_their_structure() {
    MonadicCase::run(
        r#"
begin
  let run = { fn (P : VType -> CType) (operation : Thk (P Int)) => begin
    def Stream : CType = codata | .next : Stream | .done : P Int end that
    let stream : Thk Stream = {
      fix self => do _ <- ret ();
      comatch | .next => ! self | .done => ! operation end
    } in
    ! stream .next .done
  end } in
  ! run Ret { ret 1 }
end
"#,
    );
}

#[test]
fn captured_nominal_definitions_can_leave_through_existential_packages() {
    MonadicCase::run(
        r#"
begin
  let make = { fn (A : VType) (item : A) => begin
    def Box : VType = data | +Box : A end in
    ret ((Box, (+Box item, { fn boxed => match boxed | +Box value => ret value end }))
      : exists (B : VType) . B * Thk (B -> Ret A))
  end } in
  do (B, (boxed, read)) <- ! make Int 1;
  ! read boxed
end
"#,
    );
}

#[test]
fn nonuniform_recursive_codata_does_not_specialize_each_application() {
    MonadicCase::run(
        r#"
begin
  def Growing (A : VType) : CType =
    codata | .step : A -> Growing (A * A) | .done : Ret Int end
  that
  def fix consume (A : VType) : Growing A =
    do _ <- ret ();
    comatch | .step _ => ! consume (A * A) | .done => ret 1 end
  in
  ! consume Int .step 0 .step (0, 0) .step ((0, 0), (0, 0)) .done
end
"#,
    );
}

#[test]
fn nominal_packages_keep_their_operations_and_structures() {
    MonadicCase::run(
        r#"
begin
  def Box : VType = data | +Box : Int end in
  let package : exists (A : VType) . A * Thk (A -> Ret Int) =
    (Box, (+Box 1, { fn item => match item | +Box value => ret value end }))
  in
  let (A, (item, read)) = package in
  ! read item
end
"#,
    );
}

#[test]
fn nominal_payloads_carry_computation_kind_package_evidence() {
    MonadicCase::run(
        r#"
begin
  def Packed : VType = data
    | +Packed : exists (B : CType) . Thk B * Thk (Thk B -> Ret Int)
  end in
  let packed = (+Packed (Ret Int, ({ ret 1 }, { fn k => ! k })) : Packed) in
  match packed | +Packed (B, (operation, run)) => ! run operation end
end
"#,
    );
}

#[test]
fn nominal_families_parameterize_the_selected_algebra_constructor() {
    let source = MonadicCase::source(
        r#"
fn (Selected : (VType -> CType) -> CType -> CType) =>
  let Algebra = Selected in
  @[monadic] begin
    def Packed : VType = data | +Packed : exists (B : CType) . Thk B end in
    fn (packed : Packed) => ret ()
  end
"#,
    );
    SourceCase::assert_accepted(SourceCase::check_linted(&source));
}

#[test]
fn nominal_boundaries_retain_source_rejections() {
    for (source, expected) in [
        (
            r#"@[monadic] begin
          def First : VType = data | +Item : Int end in
          def Second : VType = data | +Item : Int end in
          let value = (+Item 1 : First) in ret (value : Second)
        end"#,
            TyckDiagnosticCode::TypeMismatch,
        ),
        (
            r#"@[monadic] begin
          def Stream : CType = codata | .done : Ret Int end in
          let stream : Thk Stream = { comatch | .done => ret 1 end } in
          ! stream .missing
        end"#,
            TyckDiagnosticCode::UnknownCoDataDestructor,
        ),
        (
            r#"@[monadic] begin
          let Loop : VType = data | +Loop : Loop end that ret ()
        end"#,
            TyckDiagnosticCode::MissingSeal,
        ),
        (
            r#"def Outside : VType = data | +Item : Int end in
          @[monadic] ret (+Item 1 : Outside)"#,
            TyckDiagnosticCode::NotInlinableSeal,
        ),
        (
            r#"fn (A : VType) => @[monadic] begin
          def Captured : VType = data | +Item : A end in
          fn (item : Captured) => ret item
        end"#,
            TyckDiagnosticCode::NotInlinableSeal,
        ),
        (
            r#"@[monadic] begin
          def Result : CType = Ret Int in fn (k : Thk Result) => ! k
        end"#,
            TyckDiagnosticCode::NotInlinableSeal,
        ),
    ] {
        let source = MonadicCase::source(source);
        SourceCase::assert_rejected(SourceCase::check(&source), expected);
        SourceCase::assert_rejected(SourceCase::lower(&source), expected);
    }
}

#[test]
fn closed_source_expansions_own_their_local_declarations() {
    let source = MonadicCase::ret_program("! helper");
    let source = format!(
        r#"
let helper = {{ begin
  let VType = @(intrinsic(vtype)) in
  let Int = @(intrinsic(int)) in
  def Box : VType = data | +Box : Int end in
  match (+Box 1 : Box) | +Box value => ret value end
end }} in
{source}
"#
    );
    SourceCase::assert_accepted(SourceCase::check_linted(&source));
    SourceCase::assert_accepted(SourceCase::run(&source));
}

#[test]
fn nested_translation_composes_nominal_families() {
    MonadicCase::run(
        r#"
begin
  let identity_mo : Thk (Monad Ret) = {
    comatch
    | .return A value => ret value
    | .bind A B computation continuation => do value <- ! computation; ! continuation value
    end
  } in
  (@[monadic] begin
    def Box : VType = data | +Box : Int end in
    match (+Box 1 : Box) | +Box value => ret value end
  end) Ret identity_mo
end
"#,
    );
}

#[test]
fn different_runtime_dictionaries_preserve_applicative_identity() {
    let source = MonadicCase::source(
        r#"
let first_mo : Thk (Monad Ret) = {
  comatch
  | .return A value => ret value
  | .bind A B computation continuation => do value <- ! computation; ! continuation value
  end
} in
let second_mo : Thk (Monad Ret) = {
  comatch
  | .return A value => ret value
  | .bind A B computation continuation => do value <- ! computation; ! continuation value
  end
} in
let program = { @[monadic] begin
  def Box : VType = data | +Box : Int end in
  ret (+Box 1 : Box)
end } in
do runtime_mo <- ! int/eq (Ret (Thk (Monad Ret))) 1 1
  { ret first_mo } { ret second_mo };
do first <- ! program Ret first_mo;
do second <- ! program Ret runtime_mo;
let T = @[typeof] first in
let same : T = second in
match same | +Box value => do status <- ! int/sub value 1; ! exit status end
"#,
    );
    SourceCase::assert_accepted(SourceCase::check_linted(&source));
    SourceCase::assert_accepted(SourceCase::run(&source));
    SourceCase::assert_accepted(SourceCase::lower(&source));
}

#[test]
fn shared_closed_definitions_reuse_one_applicative_family() {
    let source = |count: usize| {
        let blocks = (0..count)
            .map(|index| format!("let translated_{index} = {{ @[monadic] ! helper }} in\n"))
            .collect::<String>();
        format!(
            r#"
let helper = {{ begin
  let VType = @(intrinsic(vtype)) in
  let Int = @(intrinsic(int)) in
  def Box : VType = data | +Box : Int end in
  ret (+Box 1 : Box)
end }} in
{blocks} ret ()
"#
        )
    };
    let once = MonadicCase::seal_count(&source(1));
    assert_eq!(MonadicCase::seal_count(&source(4)), once);
}

#[test]
fn large_recursive_graph_allocates_one_family_per_declaration() {
    const DECLARATIONS: usize = 160;
    let definitions = (0..DECLARATIONS)
        .map(|index| {
            format!(
                "def Layer{index} : VType = data | +Next : Layer{} end that\n",
                (index + 1) % DECLARATIONS,
            )
        })
        .collect::<String>();
    let body = format!("begin {definitions} fn (item : Layer0) => ret () end");
    let source_count = MonadicCase::seal_count(&body);
    let target_count = MonadicCase::seal_count(&format!("@[monadic] {body}"));
    assert_eq!(target_count - source_count, DECLARATIONS);
}

#[test]
fn separate_translated_declarations_remain_distinct() {
    let source = MonadicCase::source(
        r#"
fn (mo : Thk (Monad Ret)) =>
  do first <- (@[monadic] begin
    def Box : VType = data | +Box : Int end in ret (+Box 1 : Box)
  end) Ret mo;
  do second <- (@[monadic] begin
    def Box : VType = data | +Box : Int end in ret (+Box 1 : Box)
  end) Ret mo;
  let First = @[typeof] first in
  ret (second : First)
"#,
    );
    SourceCase::assert_rejected(SourceCase::check(&source), TyckDiagnosticCode::TypeMismatch);
}

#[test]
fn different_carriers_select_different_nominal_applications() {
    let source = MonadicCase::source(
        r#"
let Reader (A : VType) = Unit -> Ret A in
fn (identity : Thk (Monad Ret)) (reader : Thk (Monad Reader)) =>
  let program = { @[monadic] begin
    def Box : VType = data | +Box : Int end in ret (+Box 1 : Box)
  end } in
  do first <- ! program Ret identity;
  do second <- ! program Reader reader ();
  let First = @[typeof] first in
  ret (second : First)
"#,
    );
    SourceCase::assert_rejected(SourceCase::check(&source), TyckDiagnosticCode::TypeMismatch);
}
