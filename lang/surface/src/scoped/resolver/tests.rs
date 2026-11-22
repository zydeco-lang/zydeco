use super::*;
use crate::{
    scoped::fmt::Formatter as ScopedFormatter,
    textual::{StrictParser, syntax as t},
};
use zydeco_syntax::Ugly;

struct Fixture {
    spans: t::SpanArena,
    textual: t::TextArena,
    root: t::TermId,
}

impl Fixture {
    fn parse(source: &str) -> Self {
        let mut parser = t::Parser::new();
        let unit = StrictParser::source(source, &mut parser).unwrap();
        let (spans, textual) = parser.finish();
        Self { spans, textual, root: unit.root }
    }

    fn observe(&self) -> ResolutionOutput<Audit> {
        ResolveFolder::new(&self.spans, &self.textual)
            .with_observer(Audit::default())
            .run_observed(self.root)
    }

    fn with_shared_provider(mut self) -> Self {
        let t::Term::Paren(t::Paren(children)) = self.textual.terms[&self.root].clone() else {
            panic!("three test slots")
        };
        let [provider, source, signature] = children.as_slice() else { panic!("three test slots") };
        self.textual.terms[source] = t::SourceBoundary(*provider).into();
        self.textual.terms[signature] = t::SignatureBoundary(*provider).into();
        self.textual.terms[&self.root] = t::Paren(vec![*source, *signature]).into();
        self
    }

    fn errors(source: &str) -> Diagnostics<ResolveError> {
        let fixture = Self::parse(source);
        match ResolveFolder::new(&fixture.spans, &fixture.textual).run_source(fixture.root) {
            | Ok(_) => panic!("expected lowering to reject {source}"),
            | Err(errors) => *errors,
        }
    }

    fn accepted(source: &str) {
        let fixture = Self::parse(source);
        ResolveFolder::new(&fixture.spans, &fixture.textual).run_source(fixture.root).unwrap();
    }

    fn accepted_open(source: &str) {
        let fixture = Self::parse(source);
        ResolveFolder::new(&fixture.spans, &fixture.textual)
            .run_open_fragment(fixture.root)
            .unwrap();
    }

    fn open_shape(source: &str) -> String {
        let fixture = Self::parse(source);
        let result = ResolveFolder::new(&fixture.spans, &fixture.textual)
            .run_open_fragment(fixture.root)
            .unwrap();
        result.root.ugly(&ScopedFormatter::plain_names(&result.arena))
    }
}

#[derive(Default, Debug, PartialEq, Eq)]
struct Audit {
    references: Vec<(TermId, DefId)>,
    scopes: Vec<(TermId, ScopeSnapshot)>,
}

impl ResolutionObserver for Audit {
    type Output = Self;

    fn reference(&mut self, event: &ResolvedReference<'_>) {
        self.references.push((event.occurrence, event.definition));
    }

    fn scope(&mut self, event: &ScopeEvent<'_>) {
        self.scopes.push((event.occurrence, event.scope.snapshot()));
    }

    fn finish(self) -> Self {
        self
    }
}

#[test]
fn composed_observers_share_visits_and_preserve_scope_timing() {
    let fixture = Fixture::parse("let outer = 1 in fn outer => (outer, _)");
    let single = ResolveFolder::new(&fixture.spans, &fixture.textual)
        .with_observer(Audit::default())
        .run_observed(fixture.root);
    let combined = ResolveFolder::new(&fixture.spans, &fixture.textual)
        .with_observer((Audit::default(), Audit::default()))
        .run_observed(fixture.root);
    assert!(single.program.is_ok());
    assert_eq!(combined.observations.0, combined.observations.1);
    let single_refs = single
        .observations
        .references
        .iter()
        .map(|(occ, def)| (occ.raw(), def.raw()))
        .collect::<Vec<_>>();
    let combined_refs = combined
        .observations
        .0
        .references
        .iter()
        .map(|(occ, def)| (occ.raw(), def.raw()))
        .collect::<Vec<_>>();
    assert_eq!(single_refs, combined_refs);
    assert_eq!(single.observations.scopes.len(), combined.observations.0.scopes.len());
    let audit = combined.observations.0;
    assert_eq!(audit.references.len(), 1);
    assert_eq!(audit.scopes.len(), 1, "only authored holes emit scope events");
    let program = combined.program.unwrap();
    assert_eq!(audit.scopes[0].1.definitions.len(), 1);
    assert_eq!(audit.scopes[0].1.definitions[0].definition, audit.references[0].1);
    assert_eq!(program.arena.users.forth(&audit.references[0].1).iter().count(), 1);
}

#[test]
fn shared_providers_emit_events_and_diagnostics_once() {
    let output = Fixture::parse("((fn x => (x, _)), (), ())").with_shared_provider().observe();
    assert!(output.program.is_ok());
    assert_eq!(output.observations.references.len(), 1);
    assert_eq!(output.observations.scopes.len(), 1);

    let output = Fixture::parse("((missing, absent), (), ())").with_shared_provider().observe();
    assert!(output.program.is_err());
    assert_eq!(output.diagnostics.len(), 2);
    assert!(output.observations.references.is_empty());
    assert!(output.observations.scopes.is_empty(), "recovered names are not authored holes");

    let output =
        Fixture::parse("(begin param (a : a) that _ end, (), ())").with_shared_provider().observe();
    assert!(output.program.is_err());
    assert_eq!(output.diagnostics.len(), 1, "a rejected shared provider is not replayed");

    let output = Fixture::parse("(@[typeof(extra)] _, (), ())").with_shared_provider().observe();
    assert!(output.program.is_err());
    assert_eq!(output.diagnostics.len(), 1);
    assert_eq!(
        &"(@[typeof(extra)] _, (), ())"[output.diagnostics[0].primary_span().range()],
        "typeof(extra)"
    );
}

#[test]
fn independent_children_continue_after_errors_without_valid_replacements() {
    let output = Fixture::parse("((param x that x) (param y that y), missing)").observe();
    assert!(output.program.is_err());
    assert_eq!(output.diagnostics.len(), 3);
    assert_eq!(
        output
            .diagnostics
            .iter()
            .filter(|error| matches!(error, ResolveError::UnenclosedThat(_)))
            .count(),
        2
    );
    assert_eq!(
        output
            .diagnostics
            .iter()
            .filter(|error| matches!(error, ResolveError::UnboundVar(_)))
            .count(),
        1
    );
    assert!(Fixture::parse("(fn x => x, fn y => y, 1)").observe().program.is_ok());
}

#[test]
fn duplicate_names_reject_the_ambiguous_scope_and_resume_outside_it() {
    let output =
        Fixture::parse("(begin let x = 1 that let x = 2 that let x = 3 that _ end, _)").observe();
    assert!(output.program.is_err());
    assert_eq!(output.diagnostics.len(), 2);
    assert!(
        output
            .diagnostics
            .iter()
            .all(|error| matches!(error, ResolveError::DuplicateDefinition(_, _)))
    );
    assert_eq!(output.observations.scopes.len(), 1);
    assert!(output.observations.scopes.iter().all(|(_, scope)| scope.definitions.is_empty()));
    assert!(
        Fixture::parse("(begin let x = 1 that let y = 2 that (x, y) end, _)")
            .observe()
            .program
            .is_ok()
    );
}

#[test]
fn recursive_parameter_components_are_all_rejected_before_elaboration() {
    let output = Fixture::parse("begin param (a : a) that param (b : b) that (a, b) end").observe();
    assert!(output.program.is_err());
    assert_eq!(output.diagnostics.len(), 2);
    assert!(
        output.diagnostics.iter().all(|error| matches!(error, ResolveError::RecursiveParameter(_)))
    );
    assert_ne!(output.diagnostics[0].primary_span(), output.diagnostics[1].primary_span());
    assert!(Fixture::parse("begin param a that param b that (a, b) end").observe().program.is_ok());
    assert!(
        Fixture::parse("begin let a = b that let b = a that (a, b) end").observe().program.is_ok(),
        "recursive definitions remain legal"
    );
}

#[test]
fn failed_nested_blocks_close_their_graphs_before_visiting_siblings() {
    let output = Fixture::parse(
        "(begin let x = begin param (a : a) that _ end that x end, begin let y = 1 that (y, _) end)",
    )
    .observe();
    assert!(output.program.is_err());
    assert_eq!(output.diagnostics.len(), 1);
    assert_eq!(output.observations.scopes.len(), 2);
    assert_eq!(output.observations.scopes[1].1.definitions[0].name, "y");
    assert!(
        Fixture::parse(
            "(begin let x = begin param a that a end that x end, begin let y = 1 that y end)"
        )
        .observe()
        .program
        .is_ok()
    );
}

#[test]
fn mobile_bindings_reject_lexical_dependencies_unavailable_at_the_block_boundary() {
    let output = Fixture::parse("begin let x = 1 in let y = x that y end").observe();
    assert!(output.program.is_err(), "an invalid block must not publish a resolved program");
    assert!(matches!(
        output.diagnostics.as_slice(),
        [ResolveError::UnboundVar(name)] if name.inner == "x"
    ));

    let output = Fixture::parse("begin let x = 1 in begin let y = x that y end end").observe();
    assert!(output.program.is_ok(), "a nested block keeps the lexical dependency in scope");
    assert!(output.diagnostics.is_empty());
}

#[test]
fn forward_and_nested_dependencies_remain_owned_by_their_blocks() {
    let fixture = Fixture::parse(
        "begin let a = begin let inner = b that inner end that let b = 1 that a end",
    );
    let output = fixture.observe();
    let program = output.program.unwrap();
    let blocks = program.arena.blocks.iter().collect::<Vec<_>>();
    assert_eq!(blocks.len(), 2);
    let outer =
        blocks.iter().find(|(_, block)| block.context.binding_ids().count() == 2).unwrap().1;
    let inner =
        blocks.iter().find(|(_, block)| block.context.binding_ids().count() == 1).unwrap().1;
    let dependencies =
        outer.context.binding_ids().map(|id| outer.context.dependencies(&id).len()).sum::<usize>();
    assert_eq!(dependencies, 1);
    assert!(inner.context.binding_ids().all(|id| inner.context.dependencies(&id).is_empty()));
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
        assert!(errors.iter().any(|error| matches!(error, ResolveError::InvalidTypeOfMeta { .. })));
        assert!(
            errors.iter().any(|error| matches!(error, ResolveError::InvalidMonadicMeta { .. }))
        );
        let mut sites =
            errors.iter().map(|error| &source[error.primary_span().range()]).collect::<Vec<_>>();
        sites.sort();
        assert_eq!(sites, ["monadic(extra)", "typeof(extra)"]);
        Fixture::accepted_open(&source.replace("(extra)", ""));
    }
}

#[test]
fn intrinsic_inspection_requires_an_authored_hole_and_skips_rejected_payloads() {
    Fixture::accepted("@[intrinsic(unit)] _");
    for source in ["@[intrinsic(unit)] (_)", "@[intrinsic(unit)] @[typeof(extra)] _"] {
        let errors = Fixture::errors(source);
        assert_eq!(errors.len(), 1);
        assert!(matches!(errors.iter().next().unwrap(), ResolveError::IntrinsicPayloadNotHole(_)));
    }
    let fixture = Fixture::parse("@[intrinsic(unit)] _");
    let result =
        ResolveFolder::new(&fixture.spans, &fixture.textual).run_source(fixture.root).unwrap();
    assert_eq!(result.arena.terms.iter().count(), 1, "the authored hole is not lowered");
    assert!(matches!(result.arena.terms[&result.root], Term::Internal(Internal::Unit)));
}

#[test]
fn partial_actions_apply_without_marking_nested_headers() {
    let fixture = Fixture::parse("@[partial] fn x => fn y => x");
    let result =
        ResolveFolder::new(&fixture.spans, &fixture.textual).run_source(fixture.root).unwrap();
    for (pattern, value) in fixture.textual.pats.iter() {
        let t::Pattern::Var(definition) = value else { continue };
        let name = fixture.textual.defs[definition].as_str();
        assert_eq!(result.arena.partial_binders.contains(pattern), name == "x", "{name}");
    }
    let errors = Fixture::errors("@[partial] 1");
    assert!(matches!(errors.iter().next().unwrap(), ResolveError::PartialPayloadNotBinding(_)));
}

#[test]
fn unknown_meta_annotations_keep_their_structure() {
    let fixture = Fixture::parse("@[custom(option(\"value\"))] _");
    let result =
        ResolveFolder::new(&fixture.spans, &fixture.textual).run_source(fixture.root).unwrap();
    let Term::Meta(annotation) = &result.arena.terms[&result.root] else { panic!() };
    let MetaTerm { meta, body } = annotation.as_ref();
    assert_eq!(
        *meta,
        ResolvedMeta::Opaque(t::Meta::apply(
            "custom",
            [t::Meta::apply("option", [t::Meta::string("value")])]
        ))
    );
    assert!(matches!(result.arena.terms[body], Term::Hole(_)));
}

#[test]
fn binding_inputs_collect_independent_failures() {
    let source = "let value : @[typeof(extra)] _ = @[monadic(extra)] _ in value";
    let errors = Fixture::errors(source);
    assert_eq!(errors.len(), 2, "{errors}");
    Fixture::accepted_open(&source.replace("(extra)", ""));
}

#[test]
fn separate_telescope_rules_reject_each_invalid_constructor() {
    let errors = Fixture::errors("(forall .one .two . _, val .get => ())");
    assert_eq!(errors.len(), 3, "{errors}");
    assert_eq!(
        errors
            .iter()
            .filter(|error| matches!(error, ResolveError::QuantifierParameterNotPattern(_)))
            .count(),
        2
    );
    assert_eq!(
        errors
            .iter()
            .filter(|error| matches!(error, ResolveError::ValueParameterNotPattern(_)))
            .count(),
        1
    );
    Fixture::accepted("(forall one two . _, val get => ())");
}

#[test]
fn prelude_is_exactly_one_projection_with_precise_origins() {
    let source = "@[prelude(from(view))] ret ()";
    let fixture = Fixture::parse(source);
    let result = ResolveFolder::new(&fixture.spans, &fixture.textual)
        .run_open_fragment(fixture.root)
        .unwrap();
    let Term::Let(binding) = &result.arena.terms[&result.root] else { panic!() };
    let Let { binder, bindee, tail } = **binding;
    assert!(
        matches!(result.arena.terms[&bindee], Term::Var(def) if result.arena.defs[&def] == "view")
    );
    let origin = result.arena.origins.source(&bindee.into()).unwrap();
    assert_eq!(&source[fixture.spans[&origin].range()], "view");
    let origin = result.arena.origins.source(&tail.into()).unwrap();
    assert_eq!(&source[fixture.spans[&origin].range()], "ret ()");
    let Pattern::Alias(Alias(patterns)) = &result.arena.pats[&binder] else { panic!() };
    let names = patterns
        .iter()
        .map(|pattern| {
            let Pattern::Project(ProjectionPattern(field, inner)) = &result.arena.pats[pattern]
            else {
                panic!()
            };
            let Pattern::Var(definition) = result.arena.pats[inner] else { panic!() };
            assert_eq!(field.as_str(), result.arena.defs[&definition].as_str());
            let origin = result.arena.origins.source(&definition.into()).unwrap();
            assert!(matches!(origin, t::EntityId::Meta(_)));
            assert_eq!(&source[fixture.spans[&origin].range()], "prelude(from(view))");
            field.as_str()
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
    assert!(!result.arena.terms.iter().any(|(_, term)| matches!(term, Term::Meta(_))));
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
        assert!(matches!(errors.iter().next().unwrap(), ResolveError::InvalidPreludeMeta { .. }));
        assert_eq!(
            &source[errors.iter().next().unwrap().primary_span().range()],
            format!("prelude({arguments})")
        );
    }
    Fixture::accepted("@[prelude(none)] ()");
    let fixture = Fixture::parse("@[prelude(from(view))] ()");
    ResolveFolder::new(&fixture.spans, &fixture.textual).run_open_fragment(fixture.root).unwrap();
    let fixture = Fixture::parse("@[prelude(from(View))] ()");
    ResolveFolder::new(&fixture.spans, &fixture.textual).run_open_fragment(fixture.root).unwrap();
    let errors = Fixture::errors("(@[prelude(core)] (), @[prelude(from(let))] ())");
    assert_eq!(errors.len(), 2);
}

#[test]
fn groups_and_chains_keep_their_shape() {
    for (source, desugared) in [
        ("fn x y => x", "fn x y => x"),
        ("fn x => fn y => x", "fn x => fn y => x"),
        ("f a b c", "(f a b c)"),
        ("(f a) b", "((f a) b)"),
        ("(f a b) c", "((f a b) c)"),
        ("forall (X : VType) (Y : VType) . X", "pi (X : VType) (Y : VType) . X"),
        ("forall (X : VType) . forall (Y : VType) . X", "pi (X : VType) . pi (Y : VType) . X"),
        ("pi (x : A) (y : B) . C", "pi (x : A) (y : B) . C"),
        ("pi (x : A) . pi (y : B) . C", "pi (x : A) . pi (y : B) . C"),
        ("A -> B -> C", "pi (_ : A) . pi (_ : B) . C"),
        (
            "let fix f (x : A) (y : B) : C = e in f",
            "let f = ({ fix f => fn (x : A) (y : B) => e } : (Thk pi (x : A) (y : B) . C)) in f",
        ),
        (
            "let f (x : A) (y : B) : C = e in f",
            "let f = (fn (x : A) (y : B) => e : pi (x : A) (y : B) . C) in f",
        ),
        ("fn x y .read z w => x", "fn x y => comatch | .read => fn z w => x end"),
        ("val (x : A) (y : B) => x", "val (x : A) => val (y : B) => x"),
    ] {
        assert_eq!(Fixture::open_shape(source), desugared, "{source}");
    }
}

#[test]
fn unit_resolution_records_imports_prelude_and_signature_boundaries() {
    let fixture = Fixture::parse("(@(import(\"a.zy\")), @(import(\"b.zy\")), Bool)");
    let out = ResolveFolder::new(&fixture.spans, &fixture.textual)
        .run_unit(
            fixture.root,
            UnitResolveConfig { literals: &[], has_prelude: true, is_signature: true },
        )
        .unwrap();
    assert_eq!(out.imports.len(), 2);
    assert!(out.prelude_boundary.is_some());
    assert!(out.arena.origins.source(&out.prelude_boundary.unwrap().into()).is_none());
    for &(_, boundary) in &out.imports {
        assert!(out.arena.origins.source(&boundary.into()).is_some());
        let Term::SourceBoundary(SourceBoundary(placeholder)) = out.arena.terms[&boundary] else {
            panic!("import boundary must be a SourceBoundary");
        };
        assert!(out.arena.origins.source(&placeholder.into()).is_none());
    }
    assert!(!out.has_monadic);
    assert!(matches!(out.arena.terms[&out.root], Term::SignatureBoundary(_)));
    assert!(out.arena.defs.iter().any(|(def, name)| {
        name.as_str() == "Bool" && out.arena.is_default_prelude_definition(*def)
    }));

    let bad = Fixture::parse("unbound_name");
    let err = ResolveFolder::new(&bad.spans, &bad.textual)
        .run_unit(
            bad.root,
            UnitResolveConfig { literals: &[], has_prelude: false, is_signature: false },
        )
        .err()
        .expect("unbound name must fail unit resolution");
    assert_eq!(err.len(), 1);
}
