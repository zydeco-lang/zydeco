use super::*;
use crate::{
    bitter::{SourceUnitDesugarer, fmt::Formatter},
    scoped::Resolver,
    textual::{StrictParser, arena::TextualScope, syntax as t},
};
use std::collections::{HashMap, HashSet};
use zydeco_utils::pass::CompilerPass;

struct Fixture {
    textual: IdAllocator<TextualScope>,
    builder: BitterBuilder,
}

impl Fixture {
    fn new() -> Self {
        Self { textual: IdAllocator::new(), builder: BitterBuilder::new() }
    }

    fn definition(&mut self, name: &str) -> DefId {
        let origin: t::DefId = self.textual.alloc();
        Alloc::alloc(&mut self.builder, VarName(name.into()), origin.into())
    }

    fn pattern(&mut self, pattern: impl Into<Pattern>) -> PatId {
        let origin: t::PatId = self.textual.alloc();
        Alloc::alloc(&mut self.builder, pattern.into(), origin.into())
    }

    fn term(&mut self, term: impl Into<Term<VarName>>) -> TermId {
        let origin: t::TermId = self.textual.alloc();
        Alloc::alloc(&mut self.builder, term.into(), origin.into())
    }

    fn entities(&self) -> HashSet<EntityId> {
        self.builder
            .arena
            .defs
            .iter()
            .map(|(id, _)| (*id).into())
            .chain(self.builder.arena.pats.iter().map(|(id, _)| id.into()))
            .chain(self.builder.arena.terms.iter().map(|(id, _)| id.into()))
            .collect()
    }
}

#[test]
fn shared_pattern_annotations_copy_every_occurrence_and_retain_origins() {
    let mut fixture = Fixture::new();
    let definition = fixture.definition("x");
    let binder = fixture.pattern(definition);
    let occurrence = fixture.term(Term::Var(VarName("x".into())));
    let provider = fixture.term(Abs(binder, occurrence));
    let source = fixture.term(SourceBoundary(provider));
    let signature = fixture.term(SignatureBoundary(provider));
    let classifier = fixture.term(Term::Cons(vec![source, signature]));
    let root = fixture.pattern(Ann { tm: binder, ty: classifier });
    let _unreachable = fixture.term(Hole);
    let originals = fixture.entities();
    let original_text = root.ugly(&Formatter::new(&fixture.builder.arena));

    let copied = FreshenFolder { builder: &mut fixture.builder }.fold_pat(root);
    assert_ne!(copied, root);
    assert_eq!(copied.ugly(&Formatter::new(&fixture.builder.arena)), original_text);
    assert_eq!(root.ugly(&Formatter::new(&fixture.builder.arena)), original_text);

    let arena = &fixture.builder.arena;
    let expected: HashMap<_, _> = [
        (definition.into(), 3),
        (binder.into(), 3),
        (occurrence.into(), 2),
        (provider.into(), 2),
        (source.into(), 1),
        (signature.into(), 1),
        (classifier.into(), 1),
        (root.into(), 1),
    ]
    .into_iter()
    .map(|(id, count)| (arena.origins.source(&id).unwrap(), count))
    .collect();
    let copies = fixture.entities().difference(&originals).copied().collect::<Vec<_>>();
    let counts = copies.iter().fold(HashMap::new(), |mut counts, id| {
        *counts.entry(arena.origins.source(id).unwrap()).or_insert(0) += 1;
        counts
    });
    assert_eq!(counts, expected);
    assert_eq!(copies.len(), 14);
    for (id, name) in arena.defs.iter() {
        assert_eq!(name.0, "x");
        assert_eq!(arena.origins.source(&(*id).into()), arena.origins.source(&definition.into()));
    }
}

#[test]
fn copattern_spines_retain_order_and_freshen_all_binders() {
    let mut fixture = Fixture::new();
    let first = fixture.definition("A");
    let first_pattern = fixture.pattern(first);
    let second = fixture.definition("x");
    let second_pattern = fixture.pattern(second);
    let dependent_type = fixture.term(Term::Var(VarName("A".into())));
    let second_pattern = fixture.pattern(Ann { tm: second_pattern, ty: dependent_type });
    let tail = fixture.term(Term::Var(VarName("x".into())));
    let root = fixture.term(CoMatchClauses {
        clauses: vec![CoPatternClause {
            spine: CoPatternSpine {
                head: CoPatternItem::Dtor(DtorName("read".into())),
                tail: vec![
                    first_pattern.into(),
                    CoPatternItem::Dtor(DtorName("next".into())),
                    second_pattern.into(),
                ],
            },
            tail,
        }],
    });
    let copied = FreshenFolder { builder: &mut fixture.builder }.fold_term(root);
    let arena = &fixture.builder.arena;
    assert_eq!(root.ugly(&Formatter::new(arena)), copied.ugly(&Formatter::new(arena)));
    let Term::CoMatchClauses(CoMatchClauses { clauses }) = &arena.terms[&copied] else { panic!() };
    let [clause] = clauses.as_slice() else { panic!() };
    let items = clause.spine.iter().collect::<Vec<_>>();
    let [
        CoPatternItem::Dtor(read),
        CoPatternItem::Pat(a),
        CoPatternItem::Dtor(next),
        CoPatternItem::Pat(x),
    ] = items.as_slice()
    else {
        panic!()
    };
    assert_eq!(read.0, "read");
    assert_eq!(next.0, "next");
    assert_ne!(*a, first_pattern);
    assert_ne!(*x, second_pattern);
    assert_ne!(clause.tail, tail);
    assert_eq!(arena.defs.iter().count(), 4);
    assert_eq!(
        arena
            .terms
            .iter()
            .filter(|(_, term)| matches!(term, Term::Var(name) if name.0 == "A"))
            .count(),
        2
    );
}

#[test]
fn sealed_annotations_and_internal_leaves_are_preserved_by_copying() {
    let mut fixture = Fixture::new();
    let unit = fixture.term(Internal::Unit);
    let sealed = fixture.term(Sealed(unit));
    let binder = fixture.pattern(Hole);
    let root = fixture.pattern(Ann { tm: binder, ty: sealed });
    let copied = FreshenFolder { builder: &mut fixture.builder }.fold_pat(root);
    let arena = &fixture.builder.arena;
    let Pattern::Ann(Ann { ty, .. }) = arena.pats[&copied] else { panic!() };
    let Term::Sealed(Sealed(copied_unit)) = arena.terms[&ty] else { panic!() };
    assert_ne!(ty, sealed);
    assert_ne!(copied_unit, unit);
    assert!(matches!(arena.terms[&copied_unit], Term::Internal(Internal::Unit)));
    assert_eq!(arena.origins.source(&ty.into()), arena.origins.source(&sealed.into()));
    assert_eq!(arena.origins.source(&copied_unit.into()), arena.origins.source(&unit.into()));
}

#[test]
fn unannotated_abstractions_do_not_allocate_classifier_binders() {
    for (text, definitions) in [
        ("fn x => x", 1),
        ("fn x y z => (x, y, z)", 3),
        ("fn (x : (fn y => y)) => x", 2),
        ("fn x .read => x", 1),
    ] {
        let mut parser = t::Parser::new();
        let source = StrictParser::source(text, &mut parser).unwrap();
        let output = SourceUnitDesugarer { spans: &parser.spans, textual: &parser.arena }
            .run(source)
            .unwrap();
        assert_eq!(output.arena.defs.iter().count(), definitions, "{text}");
        Resolver::new(&parser.spans, output.arena).run_source(output.root).unwrap();
    }
}

#[test]
fn annotated_abstractions_resolve_each_generated_binder_independently() {
    let mut parser = t::Parser::new();
    let mut source = StrictParser::source("fn x => (x : x)", &mut parser).unwrap();
    // Exercise the direct textual annotation consumed by this lowering rule.
    // Parsing retains a Paren wrapper, which follows ordinary body lowering.
    let t::Term::Abs(t::Abs(parameters, body)) = parser.arena.terms[&source.root] else { panic!() };
    let t::Term::Paren(t::Paren(items)) = &parser.arena.terms[&body] else { panic!() };
    let [annotation] = items.as_slice() else { panic!() };
    let term = t::Abs(parameters, *annotation).into();
    let span = *source.root.span(&parser.spans);
    source.root = parser.term(span.make(term));
    let output =
        SourceUnitDesugarer { spans: &parser.spans, textual: &parser.arena }.run(source).unwrap();
    let Term::Ann(Ann { tm, ty }) = output.arena.terms[&output.root] else { panic!() };
    let Term::Abs(Abs(term_pattern, body)) = output.arena.terms[&tm] else { panic!() };
    let Term::Pi(Pi(type_pattern, classifier)) = output.arena.terms[&ty] else { panic!() };
    let Pattern::Var(term_definition) = output.arena.pats[&term_pattern] else { panic!() };
    let Pattern::Var(type_definition) = output.arena.pats[&type_pattern] else { panic!() };
    assert_ne!(term_definition, type_definition);
    assert_eq!(
        output.arena.origins.source(&term_pattern.into()),
        output.arena.origins.source(&type_pattern.into())
    );
    assert_eq!(
        output.arena.origins.source(&term_definition.into()),
        output.arena.origins.source(&type_definition.into())
    );
    let resolved = Resolver::new(&parser.spans, output.arena).run_source(output.root).unwrap();
    assert!(matches!(resolved.arena.terms[&body], Term::Var(id) if id == term_definition));
    assert!(matches!(resolved.arena.terms[&classifier], Term::Var(id) if id == type_definition));
}

#[test]
fn binding_classifiers_copy_parameter_binders_before_resolution() {
    let mut parser = t::Parser::new();
    let source = StrictParser::source("let f x : x = x in f", &mut parser).unwrap();
    let output =
        SourceUnitDesugarer { spans: &parser.spans, textual: &parser.arena }.run(source).unwrap();
    let Term::Let(binding) = &output.arena.terms[&output.root] else { panic!() };
    let Term::Ann(Ann { tm, ty }) = output.arena.terms[&binding.bindee] else { panic!() };
    let Term::Abs(Abs(term_pattern, body)) = output.arena.terms[&tm] else { panic!() };
    let Term::Pi(Pi(type_pattern, classifier)) = output.arena.terms[&ty] else { panic!() };
    let Pattern::Var(term_definition) = output.arena.pats[&term_pattern] else { panic!() };
    let Pattern::Var(type_definition) = output.arena.pats[&type_pattern] else { panic!() };
    assert_ne!(term_definition, type_definition);
    assert_eq!(
        output.arena.origins.source(&term_definition.into()),
        output.arena.origins.source(&type_definition.into())
    );
    let resolved = Resolver::new(&parser.spans, output.arena).run_source(output.root).unwrap();
    assert!(matches!(resolved.arena.terms[&body], Term::Var(id) if id == term_definition));
    assert!(matches!(resolved.arena.terms[&classifier], Term::Var(id) if id == type_definition));
}

#[test]
fn recursive_binding_sugar_keeps_the_fix_binder_distinct_from_the_let_binder() {
    let mut parser = t::Parser::new();
    let source = StrictParser::source("let fix loop = loop in loop", &mut parser).unwrap();
    let output =
        SourceUnitDesugarer { spans: &parser.spans, textual: &parser.arena }.run(source).unwrap();
    let Term::Let(binding) = &output.arena.terms[&output.root] else { panic!() };
    let Pattern::Var(outer) = output.arena.pats[&binding.binder] else { panic!() };
    let Term::Ann(Ann { tm, .. }) = output.arena.terms[&binding.bindee] else { panic!() };
    let Term::Thunk(Thunk(fix)) = output.arena.terms[&tm] else { panic!() };
    let Term::Fix(Fix(inner_pattern, body)) = output.arena.terms[&fix] else { panic!() };
    let Pattern::Var(inner) = output.arena.pats[&inner_pattern] else { panic!() };
    let tail = binding.tail;
    assert_ne!(outer, inner);
    assert_eq!(
        output.arena.origins.source(&outer.into()),
        output.arena.origins.source(&inner.into())
    );
    let resolved = Resolver::new(&parser.spans, output.arena).run_source(output.root).unwrap();
    assert!(matches!(resolved.arena.terms[&body], Term::Var(id) if id == inner));
    assert!(matches!(resolved.arena.terms[&tail], Term::Var(id) if id == outer));
}
