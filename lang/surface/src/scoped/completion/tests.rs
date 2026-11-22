use super::*;
use crate::scoped::scope::*;
use crate::{
    scoped::{ResolveError, ResolveFolder},
    textual::{ParsedHole, RecoveringParser, syntax as t},
};

struct Fixture {
    spans: t::SpanArena,
    textual: t::TextArena,
    root: t::TermId,
    target: t::TermId,
}

impl Fixture {
    fn new(marked: &str) -> Self {
        let offset = marked.find('¦').unwrap();
        let source = marked.replacen('¦', "", 1);
        let mut parser = t::Parser::new();
        let parsed = RecoveringParser::at(&source, offset).unwrap().source(&mut parser);
        let ParsedHole::Term(target) = parsed.completion.unwrap().hole.unwrap().entity else {
            panic!("a term cursor")
        };
        let (spans, textual) = parser.finish();
        Self { spans, textual, root: parsed.syntax.unwrap().root, target }
    }

    fn complete(self) -> CompletionResolution {
        ResolveFolder::new(&self.spans, &self.textual).run_completion(self.root, self.target)
    }
}

#[test]
fn enumeration_selects_the_same_definition_as_reference_resolution() {
    let completion = Fixture::new("let value = 1 in fn value => (value, ¦)").complete();
    let site = completion.site.expect("the exact resolved hole is retained");
    let scope = site.scope;
    let program = completion.program.unwrap();
    assert!(matches!(program.arena.terms[&site.target], Term::Hole(_)));
    let [candidate] = scope.definitions.as_slice() else {
        panic!("only the inner name is visible")
    };
    assert_eq!(candidate.name, "value");
    assert_eq!(program.arena.defs.iter().filter(|(_, name)| **name == "value").count(), 2);
    assert_eq!(program.arena.users.forth(&candidate.definition).iter().count(), 1);
    for user in program.arena.users.forth(&candidate.definition) {
        assert!(matches!(program.arena.terms[user], Term::Var(def) if def == candidate.definition));
    }
}

#[test]
fn shared_lookup_shadows_outer_and_prelude_names_while_preserving_dependencies() {
    let mut allocator = IdAllocator::<ScopedScope>::new();
    let shadowed: DefId = allocator.alloc();
    let visible_outer: DefId = allocator.alloc();
    let visible_local: DefId = allocator.alloc();
    let prelude_def: DefId = allocator.alloc();
    let owner: TermId = allocator.alloc();
    let dependency = BindingSite { owner, id: owner };
    let same = VarName::new("same");
    let other = VarName::new("other");
    let prelude_only = VarName::new("prelude_only");

    let mut state = ScopeState::new();
    state.prelude_definitions.insert(same, prelude_def);
    state.prelude_definitions.insert(prelude_only, prelude_def);
    state.under_map.insert(shadowed, dependency);
    state.under_map.insert(visible_outer, dependency);
    state.bind_group([(same, shadowed), (other, visible_outer)]);
    state.bind_group([(same, visible_local)]);

    let scope = state.view();
    assert_eq!(scope.lookup(&same).unwrap().definition, visible_local);
    assert!(scope.lookup(&same).unwrap().dependency.is_none());
    assert_eq!(scope.lookup(&other).unwrap().dependency.unwrap().owner, owner);
    assert_eq!(scope.lookup(&prelude_only).unwrap().definition, prelude_def);
    assert!(scope.lookup(&prelude_only).unwrap().distance > scope.lookup(&other).unwrap().distance);

    let snapshot = scope.snapshot();
    let defs =
        snapshot.definitions.iter().map(|definition| definition.definition).collect::<Vec<_>>();
    assert!(defs.contains(&visible_local));
    assert!(defs.contains(&visible_outer));
    assert!(defs.contains(&prelude_def));
    assert!(!defs.contains(&shadowed));
    assert!(scope.lookup(&VarName::new("absent")).is_none());
}

#[test]
fn unbound_references_are_diagnosed_and_replaced_only_for_completion() {
    let source = "let value = 1 in (missing, _¦, absent)";
    let completion = Fixture::new(source).complete();
    let program = completion.program.unwrap();
    assert_eq!(completion.site.unwrap().scope.definitions[0].name, "value");
    let unbound = completion
        .diagnostics
        .iter()
        .map(|error| match error {
            | ResolveError::UnboundVar(name) => name.inner.as_str(),
            | _ => panic!("an unbound-reference diagnostic"),
        })
        .collect::<Vec<_>>();
    assert_eq!(unbound, ["missing", "absent"]);
    assert!(
        program.arena.terms.iter().filter(|(_, term)| matches!(term, Term::Hole(_))).count() >= 3
    );

    let strict = Fixture::new(source);
    let error = ResolveFolder::new(&strict.spans, &strict.textual)
        .run_source(strict.root)
        .err()
        .expect("strict resolution must reject every unbound reference");
    assert_eq!(error.len(), 2);
    for expected in ["missing", "absent"] {
        assert!(error.iter().any(
            |error| matches!(error, ResolveError::UnboundVar(name) if name.inner == expected)
        ));
    }
    assert!(Fixture::new("let missing = 1 in (missing, ¦)").complete().diagnostics.is_empty());
}

#[test]
fn a_cursor_identity_from_another_parse_cannot_match_an_equal_span() {
    let old = Fixture::new("fn value => ¦").target;
    let current = Fixture::new("fn value => ¦");
    assert_ne!(old, current.target);
    let completion =
        ResolveFolder::new(&current.spans, &current.textual).run_completion(current.root, old);
    assert!(completion.site.is_none());
    assert!(completion.program.is_ok());
    let completion = Fixture::new("fn value => ¦").complete();
    assert!(completion.site.is_some());
}

#[test]
fn fatal_resolution_errors_allow_independent_sibling_scope_capture() {
    let completion = Fixture::new("(param invalid that invalid, fn value => ¦)").complete();
    assert_eq!(completion.site.unwrap().scope.definitions[0].name, "value");
    assert!(matches!(
        completion.program,
        Err(error) if error.len() == 1 && error.iter().any(|error| matches!(error, ResolveError::UnenclosedThat(_)))
    ));
}
