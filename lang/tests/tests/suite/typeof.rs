//! Single-source cases live in `lang/tests/cases/typeof/`.
//! Rust tests retain root classification, imports, and source dependencies.

use zydeco_session::{
    AnalysisError, AnalysisOutcome, CompilerSession, SourceLoadError, source::SourceDependencyKind,
};
use zydeco_statics::{TyckDiagnosticCode, syntax::TermAnnId};
use zydeco_tests::utils::SourceCase;

#[test]
fn typeof_returns_types_as_source_roots_and_kinds_of_type_operands() {
    ["@[typeof] 1", "@[typeof] @(intrinsic(int))"].into_iter().for_each(|source| {
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
}

#[test]
fn typeof_imports_the_complete_provider_classifier() {
    SourceCase::check_with_import(
        r#"
let Builder = @[typeof] @(import("imported.zy")) in
let make : Builder = val (value : Int) => (#value = value) in
ret (make 0)
"#,
        "val (value : @(intrinsic(int))) => (#value = value)",
    )
    .unwrap();
}

#[test]
fn typeof_does_not_infer_across_import_boundaries() {
    SourceCase::assert_rejected(
        SourceCase::check_with_import(
            r#"
let Signature = @[typeof] @(import("imported.zy")) in
let replacement : Signature = { fn (value : Int) => ret value } in
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
    let Some(SourceLoadError::Cycle(cycle)) = error.iter().next() else {
        panic!("expected a source dependency cycle: {error}")
    };
    assert_eq!(cycle.steps.len(), 2);
    assert!(cycle.steps.iter().all(|step| !step.span.is_dummy()));
    assert!(cycle.steps.iter().any(|step| step.kind == SourceDependencyKind::Signature));
    assert!(cycle.steps.iter().any(|step| matches!(step.kind, SourceDependencyKind::Import(_))));
}

zydeco_tests::runtime_source!(erasure, "tests/typeof/erasure.zy");
