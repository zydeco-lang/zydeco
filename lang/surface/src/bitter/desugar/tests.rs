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
fn intrinsic_inspection_requires_an_authored_hole_and_skips_rejected_payloads() {
    Fixture::accepted("@[intrinsic(unit)] _");
    for source in ["@[intrinsic(unit)] (_)", "@[intrinsic(unit)] @[typeof(extra)] _"] {
        let errors = Fixture::errors(source);
        assert_eq!(errors.len(), 1);
        assert!(matches!(errors.iter().next().unwrap(), DesugarError::IntrinsicPayloadNotHole(_)));
    }
    let mut parser = t::Parser::new();
    let unit = StrictParser::source("@[intrinsic(unit)] _", &mut parser).unwrap();
    let result =
        SourceUnitDesugarer { spans: &parser.spans, textual: &parser.arena }.run(unit).unwrap();
    assert_eq!(result.arena.terms.iter().count(), 1, "the authored hole is not lowered");
    assert!(matches!(result.arena.terms[&result.root], b::Term::Internal(b::Internal::Unit)));
}

#[test]
fn partial_actions_apply_to_cached_payloads_without_marking_nested_headers() {
    let mut parser = t::Parser::new();
    let mut unit = StrictParser::source("fn x => fn y => x", &mut parser).unwrap();
    let span = *unit.root.span(&parser.spans);
    let metadata = parser.meta(span.make(t::MetaNode::Ident("partial".into())));
    let annotated = parser.term(span.make(t::MetaTerm(metadata, unit.root).into()));
    unit.root = parser.term(span.make(t::Paren(vec![unit.root, annotated]).into()));
    let result =
        SourceUnitDesugarer { spans: &parser.spans, textual: &parser.arena }.run(unit).unwrap();
    for (pattern, value) in parser.arena.pats.iter() {
        let t::Pattern::Var(definition) = value else { continue };
        let name = &parser.arena.defs[definition].0;
        assert_eq!(result.arena.partial_binders.contains(pattern), name == "x", "{name}");
    }
    let errors = Fixture::errors("@[partial] 1");
    assert!(matches!(errors.iter().next().unwrap(), DesugarError::PartialPayloadNotBinding(_)));
}

#[test]
fn unknown_meta_annotations_keep_their_structure() {
    let mut parser = t::Parser::new();
    let unit = StrictParser::source("@[custom(option(\"value\"))] _", &mut parser).unwrap();
    let result =
        SourceUnitDesugarer { spans: &parser.spans, textual: &parser.arena }.run(unit).unwrap();
    let b::Term::Meta(annotation) = &result.arena.terms[&result.root] else { panic!() };
    let b::MetaT(meta, payload) = annotation.as_ref();
    assert_eq!(
        *meta,
        t::Meta::apply("custom", [t::Meta::apply("option", [t::Meta::string("value")])])
    );
    assert!(matches!(result.arena.terms[payload], b::Term::Hole(_)));
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

#[test]
fn prelude_is_exactly_one_projection_with_precise_origins() {
    let source = "@[prelude(from(view))] ret ()";
    let mut parser = t::Parser::new();
    let unit = StrictParser::source(source, &mut parser).unwrap();
    let result =
        SourceUnitDesugarer { spans: &parser.spans, textual: &parser.arena }.run(unit).unwrap();
    let b::Term::Let(binding) = &result.arena.terms[&result.root] else { panic!() };
    let b::Let { binder, bindee, tail } = **binding;
    assert!(matches!(&result.arena.terms[&bindee], b::Term::Var(name) if name.0 == "view"));
    let origin = result.arena.origins.source(&bindee.into()).unwrap();
    assert_eq!(&source[parser.spans[&origin].range()], "view");
    let origin = result.arena.origins.source(&tail.into()).unwrap();
    assert_eq!(&source[parser.spans[&origin].range()], "ret ()");
    let b::Pattern::Alias(b::Alias(patterns)) = &result.arena.pats[&binder] else { panic!() };
    let names = patterns
        .iter()
        .map(|pattern| {
            let b::Pattern::Project(b::ProjectionPattern(field, inner)) =
                &result.arena.pats[pattern]
            else {
                panic!()
            };
            let b::Pattern::Var(definition) = result.arena.pats[inner] else { panic!() };
            assert_eq!(field.0, result.arena.defs[&definition].0);
            let origin = result.arena.origins.source(&definition.into()).unwrap();
            assert!(matches!(origin, t::EntityId::Meta(_)));
            assert_eq!(&source[parser.spans[&origin].range()], "prelude(from(view))");
            field.0.as_str()
        })
        .collect::<Vec<_>>();
    assert_eq!(
        names,
        [
            "VType", "CType", "Thk", "Ret", "OS", "Unit", "Int8", "Int16", "Int32", "Int64", "Int",
            "UInt8", "UInt16", "UInt32", "UInt64", "UInt", "Float32", "Float64", "Char", "String",
            "Bool", "Option", "Either"
        ]
    );
    assert!(!result.arena.terms.iter().any(|(_, term)| matches!(term, b::Term::Meta(_))));
}

#[test]
fn prelude_accepts_exactly_one_exception_to_the_default() {
    for arguments in [
        "",
        "core",
        "core, from(view)",
        "from",
        "from()",
        "from(a, b)",
        "from(view), from(other)",
        "source(view)",
        "from(\"view\")",
        "from(/std/prelude)",
        "from(let)",
        "from(1)",
        "none, from(view)",
        "none, none",
        "none(extra)",
    ] {
        let source = format!("@[prelude({arguments})] ()");
        let errors = Fixture::errors(&source);
        assert_eq!(errors.len(), 1);
        assert!(matches!(errors.iter().next().unwrap(), DesugarError::InvalidPreludeMeta { .. }));
        assert_eq!(
            &source[errors.iter().next().unwrap().span().range()],
            format!("prelude({arguments})")
        );
    }
    Fixture::accepted("@[prelude(none)] ()");
    Fixture::accepted("@[prelude(from(view))] ()");
    Fixture::accepted("@[prelude(from(View))] ()");
    let errors = Fixture::errors("(@[prelude(core)] (), @[prelude(from(let))] ())");
    assert_eq!(errors.len(), 2);
}
