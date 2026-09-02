use zydeco_cli::{CompileError, DiagnosticRenderer};
use zydeco_session::{
    AnalysisError, AnalysisOutcome, CompilerSession, SourceLoadError, source::SourceDependencyKind,
};
use zydeco_statics::{TyckDiagnosticCode, syntax::TermAnnId};
use zydeco_tests::utils::{CaseError, SourceCase};

struct TypeOfCase;

impl TypeOfCase {
    fn check(source: &str) {
        Self::check_result(SourceCase::check(source));
    }

    fn check_result(result: Result<(), CaseError>) {
        if let Err(CaseError::Compile(error)) = &result {
            DiagnosticRenderer::error(error);
        }
        result.unwrap_or_else(|error| panic!("{error}"));
    }

    fn reject(source: &str, expected: TyckDiagnosticCode) {
        Self::reject_result(SourceCase::check(source), expected);
    }

    fn reject_result(result: Result<(), CaseError>, expected: TyckDiagnosticCode) {
        let Err(CaseError::Compile(CompileError::Rejected(analysis))) = result else {
            panic!("expected {expected:?}, found {result:?}")
        };
        let diagnostics = analysis.outcome().diagnostics().unwrap();
        assert!(
            diagnostics.iter().any(|diagnostic| diagnostic.code == expected),
            "expected {expected:?}, found {diagnostics:?}",
        );
        assert!(diagnostics.iter().all(|diagnostic| diagnostic.primary.is_some()));
    }
}

#[test]
fn typeof_distinguishes_value_computation_and_thunk_types() {
    TypeOfCase::check(
        r#"
begin
  let Value = @[typeof] 1 that
  let Computation = @[typeof] ret 1 that
  let Suspended = @[typeof] { ret 1 } that
  let value : Value = 2 that
  let same : Int64 = value that
  let computation : Thk Computation = { ret same } that
  let suspended : Suspended = computation that
  let exact : Thk (Ret Int64) = suspended that
  ! exact
end
"#,
    );
}

#[test]
fn typeof_returns_types_as_source_roots_and_kinds_of_type_operands() {
    ["@[typeof] 1", "@[typeof] @(intrinsic(i64))"].into_iter().for_each(|source| {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("classifier.zy");
        std::fs::write(&path, source).unwrap();
        let analysis = CompilerSession::default().analyze(&path).unwrap();
        let AnalysisOutcome::Checked { root } = analysis.outcome() else {
            panic!("classifier query was rejected: {:?}", analysis.outcome())
        };
        assert_eq!(matches!(root, TermAnnId::Type(_, _)), source == "@[typeof] 1");
        assert_eq!(matches!(root, TermAnnId::Kind(_)), source != "@[typeof] 1");
    });
    TypeOfCase::check(
        r#"
begin
  let ValueKind = @[typeof] Int64 that
  let ConstructorKind = @[typeof] Ret that
  let A : ValueKind = Unit that
  let F : ConstructorKind = Ret that
  let nested_kind = @[typeof] @[typeof] 1 that
  let B : nested_kind = Int64 that
  let value : A = () that
  let computation : Thk (F B) = { ret 0 } that
  ! computation
end
"#,
    );
}

#[test]
fn typeof_respects_operand_ascriptions_and_literal_defaults() {
    TypeOfCase::check("let value : (@[typeof] (1 : Int8)) = (2 : Int8) in ret value");
    TypeOfCase::reject(
        "let value : (@[typeof] 1) = (2 : Int8) in ret value",
        TyckDiagnosticCode::TypeMismatch,
    );
    TypeOfCase::reject(
        "let Wrong : CType = @[typeof] 1 in ret ()",
        TyckDiagnosticCode::KindMismatch,
    );
}

#[test]
fn typeof_preserves_polymorphism_and_named_projection_types() {
    TypeOfCase::check(
        r#"
begin
  let api = (
    #count = 1,
    #identity = { fn (A : VType) (value : A) => ret value }
  ) that
  let Identity = @[typeof] api/identity that
  let replacement : Identity = { fn (A : VType) (value : A) => ret value } that
  let Named = @[typeof] (#count = 1) that
  let field : Named = (#count = 2) that
  ! replacement Int64 field/count
end
"#,
    );
    TypeOfCase::reject(
        "let value : (@[typeof] (#count = 1)) = (#other = 2) in ret value",
        TyckDiagnosticCode::NamedLabelMismatch,
    );
}

#[test]
fn typeof_allows_erased_value_references_in_pi_val_pi_and_sigma() {
    TypeOfCase::check(
        r#"
begin
  let Call : CType = pi (x : Int64) . (@[typeof] ret x) that
  let call : Thk Call = { fn (x : Int64) => ret x } that
  let ValueCall = val pi (x : Int64) . (@[typeof] x) that
  let value_call : ValueCall = val (x : Int64) => x that
  let Pair = sigma (x : Int64) . (@[typeof] x) that
  let pair : Pair = (1, 2) that
  let exact : Int64 * Int64 = pair that
  ! call (value_call 0)
end
"#,
    );
    [
        "let Bad = pi (x : Int64) . x in ret ()",
        "let Bad = val pi (x : Int64) . x in ret ()",
        "let Bad = sigma (x : Int64) . x in ret ()",
        "let Bad = @[typeof] (pi (x : Int64) . x) in ret ()",
    ]
    .into_iter()
    .for_each(|source| TypeOfCase::reject(source, TyckDiagnosticCode::SortMismatch));
}

#[test]
fn typeof_allows_erased_type_references_in_kind_arrows() {
    TypeOfCase::check(
        r#"
begin
  let ConstructorKind = pi (A : VType) . (@[typeof] A) that
  let Identity : ConstructorKind = fn (A : VType) => A that
  let value : Identity Int64 = 0 that
  ret value
end
"#,
    );
}

#[test]
fn typeof_shares_local_inference_without_closing_a_new_region() {
    [
        r#"
begin
  let identity = { fn value => ret value } that
  let Signature = @[typeof] identity that
  let replacement : Signature = { fn (value : Int64) => ret value } that
  ! identity 0
end
"#,
        r#"
begin
  let replacement : Signature = { fn (value : Int64) => ret value } that
  let Signature = @[typeof] identity that
  let identity = { fn value => ret value } that
  ! identity 0
end
"#,
    ]
    .into_iter()
    .for_each(TypeOfCase::check);
    TypeOfCase::reject(
        "let Signature = @[typeof] (fn value => ret value) in ret ()",
        TyckDiagnosticCode::UnconstrainedInference,
    );
}

#[test]
fn typeof_retains_occurs_checks() {
    TypeOfCase::reject(
        r#"
begin
  let identity = { fn value => ret value } that
  let Impossible = @[typeof] (! identity identity) that
  ret ()
end
"#,
        TyckDiagnosticCode::OccursCheck,
    );
}

#[test]
fn typeof_rejects_unannotated_holes_and_kind_operands() {
    [
        "let T = @[typeof] _ in ret ()",
        "let T : VType = @[typeof] _ in ret ()",
        "let T = @(typeof) in ret ()",
    ]
    .into_iter()
    .for_each(|source| TypeOfCase::reject(source, TyckDiagnosticCode::MissingAnnotation));
    [
        "let K = @[typeof] VType in ret ()",
        "let K = @[typeof] CType in ret ()",
        "let K = @[typeof] (VType -> CType) in ret ()",
    ]
    .into_iter()
    .for_each(|source| TypeOfCase::reject(source, TyckDiagnosticCode::TypeOfKind));
    TypeOfCase::check("let T = @[typeof] (_ : Int64) in ret (0 : T)");
}

#[test]
fn typeof_keeps_constructor_synthesis_annotation_directed() {
    let prefix = "let Choice = data | +Here : Unit | +There : Unit end in\n";
    TypeOfCase::check(&format!(
        "{prefix}let T = @[typeof] (+Here() : Choice) in ret (+There() : T)"
    ));
    TypeOfCase::reject(
        &format!("{prefix}let T = @[typeof] +Here() in ret ()"),
        TyckDiagnosticCode::MissingAnnotation,
    );
}

#[test]
fn typeof_checks_coverage_in_erased_operands() {
    let prefix = "let Choice = data | +Here : Unit | +There : Unit end in\n";
    TypeOfCase::check(&format!(
        "{prefix}let T = @[typeof] (match (+Here() : Choice) | +Here(_) => ret 0 | +There(_) => ret 1 end) in ret ()"
    ));
    TypeOfCase::reject(
        &format!(
            "{prefix}let T = @[typeof] (match (+Here() : Choice) | +Here(_) => ret 0 end) in ret ()"
        ),
        TyckDiagnosticCode::Coverage,
    );
}

#[test]
fn typeof_resolves_names_in_erased_operands() {
    let error = SourceCase::check("let T = @[typeof] undefined_value in ret ()").unwrap_err();
    assert!(error.is_resolve_error(), "{error:?}");
}

#[test]
fn typeof_preserves_value_let_and_runtime_sort_boundaries() {
    TypeOfCase::check("let T = @[typeof] (let x = 1 in x) in ret (0 : T)");
    TypeOfCase::reject(
        "let T = (let x = 1 in @[typeof] x) in ret ()",
        TyckDiagnosticCode::SortMismatch,
    );
    TypeOfCase::reject("ret (@[typeof] 1)", TyckDiagnosticCode::SortMismatch);
}

#[test]
fn typeof_reuses_abstract_witnesses_within_one_opening() {
    TypeOfCase::check(
        r#"
begin
  let Box = exists (X : VType) . X that
  let boxed : Box = (Int64, 0) that
  let (X, value) = boxed in
  let T = @[typeof] value in
  let same : T = value in
  let original : X = same in
  ret ()
end
"#,
    );
    TypeOfCase::reject(
        r#"
begin
  let Box = exists (X : VType) . X that
  let boxed : Box = (Int64, 0) that
  let (X, first) = boxed in
  let (Y, second) = boxed in
  let wrong : (@[typeof] first) = second in
  ret ()
end
"#,
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn typeof_preserves_nominal_types() {
    TypeOfCase::check(
        r#"
begin
  def Token : VType = data | +Token : Unit end that
  let token : Token = +Token() that
  let Query = @[typeof] token that
  let same : Query = token that
  let original : Token = same that
  ret original
end
"#,
    );
    TypeOfCase::reject(
        r#"
begin
  def Token : VType = data | +Token : Unit end that
  def Other : VType = data | +Token : Unit end that
  let token : Token = +Token() that
  let other : Other = +Token() that
  let wrong : (@[typeof] token) = other that
  ret wrong
end
"#,
        TyckDiagnosticCode::TypeMismatch,
    );
}

#[test]
fn typeof_kind_queries_can_annotate_recursive_types() {
    TypeOfCase::check(
        r#"
begin
  def Node : (@[typeof] Int64) = data | +End : Unit | +Next : Node end that
  ret (+End() : Node)
end
"#,
    );
}

#[test]
fn typeof_cannot_extract_an_escaping_existential_witness() {
    TypeOfCase::reject(
        r#"
begin
  let Box = exists (X : VType) . X that
  let boxed : Box = (Int64, 0) that
  let Leaked = @[typeof] (let (X, value) = boxed in value) that
  ret ()
end
"#,
        TyckDiagnosticCode::EscapingExistential,
    );
    TypeOfCase::reject(
        r#"
begin
  let Box = exists (X : VType) . X that
  let Leaked = sigma ((X, value) : Box) . (@[typeof] value) that
  ret ()
end
"#,
        TyckDiagnosticCode::EscapingExistential,
    );
}

#[test]
fn typeof_keeps_package_witness_dependencies_in_pi() {
    TypeOfCase::check(
        r#"
begin
  let Box = exists (X : VType) . X that
  let Unbox = pi ((X, value) : Box) . (@[typeof] ret value) that
  let unbox : Thk Unbox = { fn ((X, value) : Box) => ret value } that
  let result : Thk (Ret Int64) = { ! unbox (Int64, 0) } that
  ! result
end
"#,
    );
}

#[test]
fn typeof_imports_the_complete_provider_classifier() {
    SourceCase::check_with_import(
        r#"
let Builder = @[typeof] @(import("imported.zy")) in
let make : Builder = val (value : Int64) => (#value = value) in
ret (make 0)
"#,
        "val (value : @(intrinsic(i64))) => (#value = value)",
    )
    .unwrap();
}

#[test]
fn typeof_does_not_infer_across_import_boundaries() {
    TypeOfCase::reject_result(
        SourceCase::check_with_import(
            r#"
let Signature = @[typeof] @(import("imported.zy")) in
let replacement : Signature = { fn (value : Int64) => ret value } in
ret ()
"#,
            "{ fn value => ret value }",
        ),
        TyckDiagnosticCode::UnconstrainedInference,
    );
}

#[test]
fn typeof_in_companion_signatures_retains_source_dependencies() {
    let directory = tempfile::tempdir().unwrap();
    let root = directory.path().join("main.zy");
    let signature = root.with_extension("zyi");
    std::fs::write(directory.path().join("value.zy"), "1").unwrap();
    std::fs::write(&root, "2").unwrap();
    std::fs::write(&signature, r#"@[typeof] @(import("value.zy"))"#).unwrap();
    let analysis = CompilerSession::default().analyze(&root).unwrap();
    assert!(matches!(analysis.outcome(), AnalysisOutcome::Checked { .. }));

    std::fs::write(&signature, r#"@[typeof] @(import("main.zy"))"#).unwrap();
    let error = CompilerSession::default().analyze(&root).unwrap_err();
    let AnalysisError::Source { error } = &error else {
        panic!("expected a source dependency error: {error}")
    };
    let SourceLoadError::Cycle(cycle) = error.as_ref() else {
        panic!("expected a source dependency cycle: {error}")
    };
    assert_eq!(cycle.steps.len(), 2);
    assert!(cycle.steps.iter().all(|step| !step.span.is_dummy()));
    assert!(cycle.steps.iter().any(|step| step.kind == SourceDependencyKind::Signature));
    assert!(cycle.steps.iter().any(|step| matches!(step.kind, SourceDependencyKind::Import(_))));
}

#[test]
fn typeof_composes_with_monadic_elaboration() {
    TypeOfCase::check_result(SourceCase::check_monadic(
        r#"
begin
  let translated = { @[monadic] fn (value : Int64) => ret (value : (@[typeof] value)) } that
  let Signature = @[typeof] translated that
  let same : Signature = translated that
  ret ()
end
"#,
    ));
}

zydeco_tests::runtime_source!(erasure, "tests/typeof/erasure.zy");
