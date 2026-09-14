use super::*;
use crate::textual::StrictParser;

struct Fixture;

impl Fixture {
    fn errors(source: &str) -> Diagnostics<DesugarError> {
        let mut parser = t::Parser::new();
        let unit = StrictParser::source(source, &mut parser).unwrap();
        match (SourceUnitDesugarer { spans: &parser.spans, textual: &parser.arena }).run(unit) {
            | Ok(_) => panic!("expected lowering to reject {source}"),
            | Err(errors) => errors,
        }
    }

    fn accepted(source: &str) {
        let mut parser = t::Parser::new();
        let unit = StrictParser::source(source, &mut parser).unwrap();
        SourceUnitDesugarer { spans: &parser.spans, textual: &parser.arena }.run(unit).unwrap();
    }
}

#[test]
fn independent_children_report_all_errors_with_their_own_locations() {
    for source in [
        "(@[typeof(extra)] _, @[monadic(extra)] _)",
        "(@[typeof(extra)] _) (@[monadic(extra)] _)",
        "(@[typeof(extra)] _ : @[monadic(extra)] _)",
        "match _ | _ => @[typeof(extra)] _ | _ => @[monadic(extra)] _ end",
    ] {
        let errors = Fixture::errors(source);
        assert_eq!(errors.len(), 2, "{source}: {errors}");
        assert!(errors.iter().any(|error| matches!(error, DesugarError::InvalidTypeOfMeta { .. })));
        assert!(
            errors.iter().any(|error| matches!(error, DesugarError::InvalidMonadicMeta { .. }))
        );
        let mut sites =
            errors.iter().map(|error| &source[error.span().range()]).collect::<Vec<_>>();
        sites.sort();
        assert_eq!(sites, ["monadic(extra)", "typeof(extra)"]);
        Fixture::accepted(&source.replace("(extra)", ""));
    }
}

#[test]
fn shared_rejected_terms_do_not_replay_diagnostics() {
    let source = "@[typeof(extra)] _";
    let mut parser = t::Parser::new();
    let mut unit = StrictParser::source(source, &mut parser).unwrap();
    let span = *unit.root.span(&parser.spans);
    unit.root = parser.term(span.make(t::Paren(vec![unit.root, unit.root]).into()));
    let mut folder = DesugarFolder::new(&parser.spans, &parser.arena);
    assert!(folder.term(unit.root).is_err());
    assert!(folder.term(unit.root).is_err());
    assert_eq!(folder.diagnostics.len(), 1);
    assert_eq!(&source[folder.diagnostics[0].span().range()], "typeof(extra)");
}

#[test]
fn binding_inputs_collect_independent_failures() {
    let source = "let value : @[typeof(extra)] _ = @[monadic(extra)] _ in value";
    let errors = Fixture::errors(source);
    assert_eq!(errors.len(), 2, "{errors}");
    Fixture::accepted(&source.replace("(extra)", ""));
}

#[test]
fn separate_telescope_rules_reject_each_invalid_constructor() {
    let errors = Fixture::errors("(forall .one .two . _, val .get => ())");
    assert_eq!(errors.len(), 3, "{errors}");
    assert_eq!(
        errors
            .iter()
            .filter(|error| matches!(error, DesugarError::QuantifierParameterNotPattern(_)))
            .count(),
        2
    );
    assert_eq!(
        errors
            .iter()
            .filter(|error| matches!(error, DesugarError::ValueParameterNotPattern(_)))
            .count(),
        1
    );
    Fixture::accepted("(forall one two . _, val get => ())");
}
