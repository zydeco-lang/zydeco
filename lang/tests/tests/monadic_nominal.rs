//! Source cases live in `cases/monadic-nominal/`; Rust retains compiler assertions and backend coverage.

use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::SourceCase;

zydeco_tests::e2e_sources!({
    state_recursive_protocol => "tests/monadic/nominal-types.zy",
    nominal_short_circuit => "tests/monadic/nominal-short-circuit.zy",
    expression_evaluator => "tests/monadic/nominal-expression-evaluator.zy",
    nominal_mutual => "tests/monadic/nominal-mutual.zy",
});

zydeco_tests::e2e_io_source!(state_lazy_list, "tests/monadic/lazy-list.zy", "", "0\n0\n3\n12\n");

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

    fn assert_linted_and_lowered(source: &str) {
        let source = Self::source(source);
        SourceCase::assert_accepted(SourceCase::check_linted(&source));
        SourceCase::assert_accepted(SourceCase::lower(&source));
    }
}

#[test]
fn local_nominal_data_preserves_constructors_and_patterns() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-nominal/local_nominal_data_preserves_constructors_and_patterns.zy"
    ));
}

#[test]
fn recursive_type_constructor_reuses_its_family() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-nominal/recursive_type_constructor_reuses_its_family.zy"
    ));
}

#[test]
fn recursive_codata_uses_lazy_algebra_evidence() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-nominal/recursive_codata_uses_lazy_algebra_evidence.zy"
    ));
}

#[test]
fn mutually_recursive_data_preserves_nominal_edges() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-nominal/mutually_recursive_data_preserves_nominal_edges.zy"
    ));
}

#[test]
fn mutually_recursive_codata_shares_its_dictionary() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-nominal/mutually_recursive_codata_shares_its_dictionary.zy"
    ));
}

#[test]
fn local_declarations_abstract_their_free_type_dependencies() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-nominal/local_declarations_abstract_their_free_type_dependencies.zy"
    ));
}

#[test]
fn higher_kinded_dependencies_supply_their_structure() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-nominal/higher_kinded_dependencies_supply_their_structure.zy"
    ));
}

#[test]
fn captured_nominal_definitions_can_leave_through_existential_packages() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-nominal/captured_nominal_definitions_can_leave_through_existential_packages.zy"
    ));
}

#[test]
fn nonuniform_recursive_codata_does_not_specialize_each_application() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-nominal/nonuniform_recursive_codata_does_not_specialize_each_application.zy"
    ));
}

#[test]
fn nominal_packages_keep_their_operations_and_structures() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-nominal/nominal_packages_keep_their_operations_and_structures.zy"
    ));
}

#[test]
fn nominal_payloads_carry_computation_kind_package_evidence() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-nominal/nominal_payloads_carry_computation_kind_package_evidence.zy"
    ));
}

#[test]
fn nominal_families_parameterize_the_selected_algebra_constructor() {
    let source = MonadicCase::source(include_str!(
        "../cases/monadic-nominal/nominal_families_parameterize_the_selected_algebra_constructor.zy"
    ));
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
    let source = MonadicCase::source(
        r#"let mo : Thk (Monad Ret) = {
  comatch
  | .return A value => ret value
  | .bind A B computation continuation =>
      do value <- ! computation;
      ! continuation value
  end
} in
do result <- (@[monadic] ! helper) Ret mo;
do status <- ! int/sub result 1;
! exit status"#,
    );
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
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-nominal/nested_translation_composes_nominal_families.zy"
    ));
}

#[test]
fn different_runtime_dictionaries_preserve_applicative_identity() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-nominal/different_runtime_dictionaries_preserve_applicative_identity.zy"
    ));
}

#[test]
fn nominal_algebras_use_the_selected_runtime_dictionary() {
    MonadicCase::assert_linted_and_lowered(include_str!(
        "../cases/monadic-nominal/nominal_algebras_use_the_selected_runtime_dictionary.zy"
    ));
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
