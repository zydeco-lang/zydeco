use super::*;
use crate::{
    bitter::{SourceDesugarOut, SourceUnitDesugarer},
    scoped::{ResolveError, Resolver},
    textual::{ParsedHole, RecoveringParser, syntax as t},
};
use zydeco_utils::pass::CompilerPass;

struct Fixture {
    spans: t::SpanArena,
    bitter: SourceDesugarOut,
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
        let (spans, arena) = parser.finish();
        let bitter =
            SourceUnitDesugarer::new(&spans, &arena, parsed.syntax.unwrap()).run().unwrap();
        Self { spans, bitter, target }
    }

    fn complete(self) -> CompletionResolution {
        Resolver::new(&self.spans, self.bitter.arena, self.bitter.prim)
            .run_completion(self.bitter.root, self.target)
    }
}

#[test]
fn enumeration_selects_the_same_definition_as_reference_resolution() {
    let completion = Fixture::new("let value = 1 in fn value => (value, ¦)").complete();
    let scope = completion.scope.unwrap();
    let program = completion.program.unwrap();
    let [candidate] = scope.definitions.as_slice() else {
        panic!("only the inner name is visible")
    };
    assert_eq!(candidate.name.0, "value");
    assert_eq!(program.arena.defs.iter().filter(|(_, name)| name.0 == "value").count(), 2);
    assert_eq!(program.arena.users.forth(&candidate.definition).iter().count(), 1);
    for user in program.arena.users.forth(&candidate.definition) {
        assert!(matches!(program.arena.terms[user], Term::Var(def) if def == candidate.definition));
    }
}

#[test]
fn shared_lookup_shadows_global_names_and_preserves_global_dependencies() {
    let mut allocator = IdAllocator::<BitterScope>::new();
    let shadowed: DefId = allocator.alloc();
    let visible_global: DefId = allocator.alloc();
    let visible_local: DefId = allocator.alloc();
    let owner: TermId = allocator.alloc();
    let dependency = BindingSite { owner, id: owner };
    let same = VarName("same".into());
    let other = VarName("other".into());
    let global = Global {
        var_to_def: [(same.clone(), shadowed), (other.clone(), visible_global)]
            .into_iter()
            .collect(),
        under_map: [(shadowed, dependency), (visible_global, dependency)].into_iter().collect(),
    };
    let local = Local {
        under: rpds::VectorSync::new_sync(),
        var_to_def: rpds::HashTrieMapSync::new_sync(),
        depth: 0,
        under_map: rpds::HashTrieMapSync::new_sync(),
        boundary: None,
    }
    .bind_group([(same.clone(), visible_local)]);
    let scope = NameScope { local: &local, global: &global };
    assert_eq!(scope.lookup(&same).unwrap().definition, visible_local);
    assert!(scope.lookup(&same).unwrap().dependency.is_none());
    assert_eq!(scope.lookup(&other).unwrap().dependency.unwrap().owner, owner);
    let snapshot = scope.snapshot();
    assert_eq!(
        snapshot.definitions.iter().map(|definition| definition.definition).collect::<Vec<_>>(),
        [visible_global, visible_local]
    );
    assert!(scope.lookup(&VarName("absent".into())).is_none());
}

#[test]
fn unbound_references_are_diagnosed_and_replaced_only_for_completion() {
    let source = "let value = 1 in (missing, _¦, absent)";
    let completion = Fixture::new(source).complete();
    let program = completion.program.unwrap();
    assert_eq!(completion.scope.unwrap().definitions[0].name.0, "value");
    let unbound = completion
        .unbound
        .iter()
        .map(|error| match error {
            | ResolveError::UnboundVar(name) => name.inner.0.as_str(),
            | _ => panic!("an unbound-reference diagnostic"),
        })
        .collect::<Vec<_>>();
    assert_eq!(unbound, ["missing", "absent"]);
    assert!(
        program.arena.terms.iter().filter(|(_, term)| matches!(term, Term::Hole(_))).count() >= 3
    );

    let strict = Fixture::new(source);
    let error = Resolver::new(&strict.spans, strict.bitter.arena, strict.bitter.prim)
        .run_source(strict.bitter.root)
        .err()
        .expect("strict resolution must reject the first unbound reference");
    assert!(matches!(*error, ResolveError::UnboundVar(ref name) if name.inner.0 == "missing"));
    assert!(Fixture::new("let missing = 1 in (missing, ¦)").complete().unbound.is_empty());
}

#[test]
fn a_cursor_identity_from_another_parse_cannot_match_an_equal_span() {
    let old = Fixture::new("fn value => ¦").target;
    let current = Fixture::new("fn value => ¦");
    assert_ne!(old, current.target);
    let completion = Resolver::new(&current.spans, current.bitter.arena, current.bitter.prim)
        .run_completion(current.bitter.root, old);
    assert!(completion.scope.is_none());
    assert!(completion.program.is_ok());
    assert!(Fixture::new("fn value => ¦").complete().scope.is_some());
}

#[test]
fn fatal_resolution_errors_do_not_invent_an_unvisited_scope() {
    let completion = Fixture::new("(param invalid that invalid, fn value => ¦)").complete();
    assert!(completion.scope.is_none());
    assert!(
        matches!(completion.program, Err(error) if matches!(*error, ResolveError::UnenclosedThat(_)))
    );
}
