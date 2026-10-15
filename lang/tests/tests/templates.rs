use std::{fs, path::Path};
use zydeco_cli::{SourceFormatOutcome, SourceFormatter};
use zydeco_statics::TyckDiagnosticCode;
use zydeco_tests::utils::{
    CaseDirective, CaseExpectation, CaseStage, SourceCasePrelude, SourceTemplate,
};

#[test]
fn rendered_wrappers_preserve_acceptance_and_rejection_through_formatting() {
    for (source, parameter, directory) in [
        (include_str!("../templates/memory.zy.in"), "directory", "../../lib/std/memory"),
        (include_str!("../templates/effects.zy.in"), "effect", "../../lib/std/effect"),
        (include_str!("../templates/effect-library.zy.in"), "effect", "../../lib/std/effect"),
        (include_str!("../templates/nondet.zy.in"), "std", "../../lib/std"),
        (include_str!("../templates/transformers.zy.in"), "std", "../../lib/std"),
    ] {
        let directory = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join(directory)
            .canonicalize()
            .unwrap()
            .display()
            .to_string();
        let template = SourceTemplate::parse(source).unwrap();
        for (body, expectation) in [
            ("! exit 0", CaseExpectation::Accepted),
            (
                "let invalid : Int = () in ! exit 0",
                CaseExpectation::Rejected(TyckDiagnosticCode::TypeMismatch),
            ),
        ] {
            let source =
                template.render([(parameter, directory.as_str()), ("body", body)]).unwrap();
            let case = CaseDirective {
                stage: CaseStage::Check,
                prelude: SourceCasePrelude::Core,
                expectation,
            };
            case.assert(case.run(&source));

            let output = tempfile::tempdir().unwrap();
            let path = output.path().join("rendered.zy");
            fs::write(&path, &source).unwrap();
            SourceFormatter.format_path(&path).expect("rendered templates must parse");
            assert_eq!(SourceFormatter.check_path(&path).unwrap(), SourceFormatOutcome::Unchanged);
            let formatted = fs::read_to_string(path).unwrap();
            case.assert(case.run(&formatted));
        }
    }
}
