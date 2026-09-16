use super::*;
use crate::{
    bitter::{SourceDesugarOut, SourceUnitDesugarer},
    textual::{StrictParser, syntax as t},
};
use zydeco_utils::pass::CompilerPass;

struct Fixture {
    spans: t::SpanArena,
    bitter: SourceDesugarOut,
}
impl Fixture {
    fn parse(source: &str) -> Self {
        let mut parser = t::Parser::new();
        let unit = StrictParser::source(source, &mut parser).unwrap();
        let (spans, arena) = parser.finish();
        let bitter = SourceUnitDesugarer { spans: &spans, textual: &arena }.run(unit).unwrap();
        Self { spans, bitter }
    }
    fn observe(self) -> ResolutionOutput<Audit> {
        ResolveFolder::new(&self.spans, self.bitter.arena)
            .with_observer(Audit::default())
            .run_observed(self.bitter.root)
    }
    fn with_shared_provider(mut self) -> Self {
        let mut arena = self.bitter.arena.into_inner();
        let Term::Cons(children) = arena.terms[&self.bitter.root].clone() else {
            panic!("three test slots")
        };
        let [provider, source, signature] = children.as_slice() else { panic!("three test slots") };
        arena.terms[source] = SourceBoundary(*provider).into();
        arena.terms[signature] = SignatureBoundary(*provider).into();
        arena.terms[&self.bitter.root] = Term::Cons(vec![*source, *signature]);
        self.bitter.arena = FrozenArena::new(arena);
        self
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
    let root = fixture.bitter.root;
    let arena = &fixture.bitter.arena;
    let copy = FrozenArena::new(BitterArena {
        defs: arena.defs.clone(),
        pats: arena.pats.clone(),
        terms: arena.terms.clone(),
        origins: arena.origins.clone(),
        partial_binders: arena.partial_binders.clone(),
    });
    let single =
        ResolveFolder::new(&fixture.spans, copy).with_observer(Audit::default()).run_observed(root);
    let combined = ResolveFolder::new(&fixture.spans, fixture.bitter.arena)
        .with_observer((Audit::default(), Audit::default()))
        .run_observed(root);
    assert!(single.program.is_ok());
    assert_eq!(single.observations, combined.observations.0);
    assert_eq!(combined.observations.0, combined.observations.1);
    let audit = combined.observations.0;
    assert_eq!(audit.references.len(), 1);
    assert_eq!(audit.scopes.len(), 2, "includes the inferred classifier hole");
    let program = combined.program.unwrap();
    assert!(audit.scopes[0].1.definitions.is_empty(), "the inferred classifier precedes bindings");
    assert_eq!(audit.scopes.last().unwrap().1.definitions[0].definition, audit.references[0].1);
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
    let fixture = Fixture::parse(
        "(begin let x = begin param (a : a) that _ end that x end, begin let y = 1 that y end)",
    );
    let y = fixture
        .bitter
        .arena
        .defs
        .iter()
        .find_map(|(id, name)| (name.0 == "y").then_some(*id))
        .unwrap();
    let output = fixture.observe();
    assert!(output.program.is_err());
    assert_eq!(output.diagnostics.len(), 1);
    assert!(output.observations.references.iter().any(|(_, definition)| *definition == y));
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
        [ResolveError::UnboundVar(name)] if name.inner.0 == "x"
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
