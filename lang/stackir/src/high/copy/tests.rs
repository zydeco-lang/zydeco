use super::*;
use crate::{
    SpsScheme,
    high::check::{FragmentError, FragmentProgram},
    protocol::{StackProtocol, ValueProtocol},
};
use zydeco_statics::{arena::StaticsScope, syntax as ss};
use zydeco_utils::prelude::IdAllocator;

struct Fixture {
    arena: SpsArena,
    sites: IdAllocator<StaticsScope>,
}

/// The nodes of a consumer that carry evidence a copy must keep.
struct Evidence {
    closure: ValueId,
    binder: VPatId,
    body: CompuId,
}

impl Fixture {
    fn new() -> Self {
        Self { arena: SpsArena::default(), sites: IdAllocator::new() }
    }

    fn def(&mut self, name: &str) -> DefId {
        let def = self.arena.admin.fresh_def();
        self.arena.admin.insert_def(def, VarName(name.into()));
        def
    }

    fn term(&mut self) -> Option<ss::TermId> {
        Some(ss::TermId::Value(self.sites.alloc()))
    }

    /// `arg(v) :: tag(d) :: kont x -> let f = { • -> • @ x } in f ! •`, with sites,
    /// protocol evidence, and builtin provenance on its inner nodes.
    fn consumer(&mut self) -> (StackId, Evidence) {
        let [x, f, v] = ["x", "f", "v"].map(|name| self.def(name));
        let site = self.term();
        let returned = StackVar.build(&mut self.arena, None);
        let value = Value::Var(x).build(&mut self.arena, None);
        let body = SReturn { stack: returned, value }.build(&mut self.arena, None);
        let closure = Closure { stack: StackVar, body }.build(&mut self.arena, site);
        let thunk = ValueProtocol::Thunk(Box::default());
        self.arena.inner.value_protocols.insert_new(closure, thunk);
        self.arena.inner.builtin_functions.insert_new(closure, BuiltinValueRole::Exit);
        let site = Some(ss::PatId::Value(self.sites.alloc()));
        let binder = ValuePattern::Var(f).build(&mut self.arena, site);
        self.arena.inner.pattern_protocols.insert_new(binder, ValueProtocol::Unit);
        let thunk = Value::Var(f).build(&mut self.arena, None);
        let stack = StackVar.build(&mut self.arena, None);
        let force = SForce { thunk, stack }.build(&mut self.arena, None);
        let site = self.term();
        let body = Let { binder, bindee: closure, tail: force }.build(&mut self.arena, site);
        let protocol = StackProtocol::Continuation(Box::new(ValueProtocol::Unit));
        self.arena.inner.compu_protocols.insert_new(body, protocol);
        let result = ValuePattern::Var(x).build(&mut self.arena, None);
        let kont = Kont { binder: result, body }.build(&mut self.arena, None);
        let tag = DtorIdx { idx: 0, name: DtorName("d".into()) };
        let tagged = Cons(tag, kont).build(&mut self.arena, None);
        let site = self.term();
        let argument = Value::Var(v).build(&mut self.arena, site);
        let stack = Cons(argument, tagged).build(&mut self.arena, None);
        (stack, Evidence { closure, binder, body })
    }

    /// Follow `arg(_) :: tag(_) :: kont _ -> let p = c in _` to the evidence-carrying nodes.
    fn evidence(&self, stack: StackId) -> Evidence {
        let inner = &self.arena.inner;
        let Stack::Arg(Cons(_, tagged)) = inner.stacks[&stack] else { panic!("argument frame") };
        let Stack::Tag(Cons(_, kont)) = &inner.stacks[&tagged] else { panic!("tag frame") };
        let Stack::Ext(StackExt::Kont(Kont { body, .. })) = inner.stacks[kont] else {
            panic!("continuation")
        };
        let Computation::LetValue(Let { binder, bindee, .. }) = inner.compus[&body] else {
            panic!("value let")
        };
        Evidence { closure: bindee, binder, body }
    }

    fn render(&self, stack: StackId) -> String {
        let scoped = zydeco_surface::scoped::arena::ScopedArena::default();
        let statics = zydeco_statics::arena::StaticsArena::default();
        let formatter = crate::high::fmt::Formatter::new(
            &self.arena.admin,
            &self.arena.inner,
            &scoped,
            &statics,
        );
        stack.ugly(&formatter)
    }

    /// `case C() | C(_) -> h ! first | C(_) -> h ! second end` under the duplicating scheme.
    fn branch(mut self, first: StackId, second: StackId) -> Result<FragmentProgram, FragmentError> {
        let tag = CtorIdx { idx: 0, name: CtorName("C".into()) };
        let payload = Triv.build(&mut self.arena, None);
        let scrut = Ctor(tag.clone(), payload).build(&mut self.arena, None);
        let h = self.def("h");
        let arms = [first, second]
            .into_iter()
            .map(|stack| {
                let hole = Hole.build(&mut self.arena, None);
                let binder = Ctor(tag.clone(), hole).build(&mut self.arena, None);
                let thunk = Value::Var(h).build(&mut self.arena, None);
                let tail = SForce { thunk, stack }.build(&mut self.arena, None);
                Matcher { binder, tail }
            })
            .collect();
        let root = SCoprodMatch { scrut, arms }.build(&mut self.arena, None);
        FragmentProgram::try_new(SpsProgram::new(self.arena, root), SpsScheme::Duplicating)
            .map_err(|errors| errors.into_iter().next().unwrap())
    }
}

#[test]
fn copies_render_alike_and_keep_sites_protocols_and_provenance() {
    let mut fixture = Fixture::new();
    let (original, source) = fixture.consumer();
    let copy = fixture.arena.copy_stack(original);
    assert_ne!(copy, original);
    assert_eq!(fixture.render(copy), fixture.render(original));

    let copied = fixture.evidence(copy);
    assert_ne!(copied.closure, source.closure);
    let (admin, inner) = (&fixture.arena.admin, &fixture.arena.inner);
    let site = |value| admin.terms.back(&TermId::Value(value)).copied();
    assert_eq!(site(copied.closure), site(source.closure));
    assert!(site(copied.closure).is_some());
    assert_eq!(admin.pats.back(&copied.binder), admin.pats.back(&source.binder));
    assert_eq!(
        admin.terms.back(&TermId::Compu(copied.body)),
        admin.terms.back(&TermId::Compu(source.body))
    );
    assert_eq!(
        inner.value_protocols.get(&copied.closure),
        inner.value_protocols.get(&source.closure)
    );
    assert_eq!(inner.builtin_functions.get(&copied.closure), Some(&BuiltinValueRole::Exit));
    assert_eq!(inner.pattern_protocols.get(&copied.binder), Some(&ValueProtocol::Unit));
    assert_eq!(inner.compu_protocols.get(&copied.body), inner.compu_protocols.get(&source.body));
}

#[test]
fn a_copy_owns_its_nodes_where_reusing_the_original_is_rejected() {
    let mut fixture = Fixture::new();
    let (original, _) = fixture.consumer();
    let copy = fixture.arena.copy_stack(original);
    assert!(fixture.branch(original, copy).is_ok());

    let mut fixture = Fixture::new();
    let (original, _) = fixture.consumer();
    assert_eq!(
        fixture.branch(original, original).unwrap_err(),
        FragmentError::SharedStack { stack: original }
    );
}

#[test]
fn deep_stacks_copy_and_drop_on_a_small_native_stack() {
    std::thread::Builder::new()
        .stack_size(512 * 1024)
        .spawn(|| {
            let mut arena = SpsArena::default();
            let stack = StackVar.build(&mut arena, None);
            let mut body: CompuId = SHole(stack).build(&mut arena, None);
            for _ in 0..4_096 {
                let binder: VPatId = Hole.build(&mut arena, None);
                let bindee: ValueId = Triv.build(&mut arena, None);
                body = Let { binder, bindee, tail: body }.build(&mut arena, None);
            }
            let binder: VPatId = Hole.build(&mut arena, None);
            let mut stack: StackId = Kont { binder, body }.build(&mut arena, None);
            for _ in 0..16_384 {
                let argument: ValueId = Triv.build(&mut arena, None);
                stack = Cons(argument, stack).build(&mut arena, None);
            }
            let counts = |arena: &SpsArena| {
                let inner = &arena.inner;
                [inner.vpats.len(), inner.values.len(), inner.stacks.len(), inner.compus.len()]
            };
            let before = counts(&arena);
            let copy = arena.copy_stack(stack);
            assert_ne!(copy, stack);
            assert_eq!(counts(&arena), before.map(|count| count * 2));
            drop(arena);
        })
        .unwrap()
        .join()
        .unwrap();
}
