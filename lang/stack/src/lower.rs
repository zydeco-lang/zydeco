use super::arena::{StackArena, StackArenaLike};
use super::syntax::{Computation, Stack, *};
use zydeco_statics::{tyck::arena::StaticsArena, tyck::syntax as ss};
use zydeco_surface::{scoped::arena::ScopedArena, textual::syntax as t};
use zydeco_utils::arena::{ArcGlobalAlloc, ArenaAssoc, ArenaBijective};

pub trait Lower {
    type Kont;
    type Out;
    fn lower(&self, ctx: StackId, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out;
}

pub struct Lowerer<'a> {
    pub arena: StackArena,
    pub spans: &'a t::SpanArena,
    pub scoped: &'a ScopedArena,
    pub statics: &'a StaticsArena,
    pub entry: ArenaAssoc<ss::CompuId, ()>,
    pub externals: ArenaAssoc<ss::DefId, ()>,
    pub decls: ArenaBijective<ss::DeclId, ss::CompuId>,
}

impl<'a> Lowerer<'a> {
    pub fn new(
        alloc: ArcGlobalAlloc, spans: &'a t::SpanArena, scoped: &'a ScopedArena,
        statics: &'a StaticsArena, entry: ArenaAssoc<ss::CompuId, ()>,
        externals: ArenaAssoc<ss::DefId, ()>, decls: ArenaBijective<ss::DeclId, ss::CompuId>,
    ) -> Self {
        let arena = StackArena::new_arc(alloc);
        Self { arena, spans, scoped, statics, entry, externals, decls }
    }
    pub fn run(mut self) -> StackArena {
        let entries: Vec<_> = self.entry.iter().map(|(compu, _)| *compu).collect();
        for compu in entries {
            let initial_stack = StackArenaLike::stack(&mut self.arena, Stack::Var(Bullet));
            let _compu_id = compu.lower(initial_stack, &mut self, ());
            // TODO: map compu back to DeclId if needed for entry
        }
        self.arena
    }
}
impl AsMut<StackArena> for Lowerer<'_> {
    fn as_mut(&mut self) -> &mut StackArena {
        &mut self.arena
    }
}

impl Lower for ss::VPatId {
    type Kont = Box<dyn FnOnce(StackId, &mut Lowerer) -> CompuId>;
    type Out = CompuId;

    fn lower(&self, ctx: StackId, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let vpat = lo.statics.vpats[self].clone();
        use ss::ValuePattern as VPat;
        match vpat {
            | VPat::Hole(Hole) => {
                // TODO: implement panic/hole handling
                lo.arena.compu(Computation::Hole(Hole))
            }
            | VPat::Var(def) => {
                // TODO: implement variable pattern
                let _ = (def, ctx, kont);
                lo.arena.compu(Computation::Hole(Hole))
            }
            | VPat::Ctor(Ctor(_ctor, _body)) => {
                // TODO: implement constructor pattern
                unreachable!()
            }
            | VPat::Triv(Triv) => {
                // TODO: implement trivial pattern
                let _ = (ctx, kont);
                lo.arena.compu(Computation::Hole(Hole))
            }
            | VPat::VCons(Cons(a, b)) => {
                // TODO: implement value cons pattern
                let _ = (a, b, ctx, kont);
                lo.arena.compu(Computation::Hole(Hole))
            }
            | VPat::TCons(Cons(_ty, inner)) => {
                // Type cons patterns are erased
                inner.lower(ctx, lo, kont)
            }
        }
    }
}

impl Lower for ss::ValueId {
    type Kont = Box<dyn FnOnce(ValueId, StackId, &mut Lowerer) -> CompuId>;
    type Out = CompuId;

    fn lower(&self, ctx: StackId, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let value = lo.statics.values[self].clone();
        use ss::Value;
        match value {
            | Value::Hole(Hole) => {
                // TODO: implement panic/hole handling
                lo.arena.compu(Computation::Hole(Hole))
            }
            | Value::Var(def) => {
                // TODO: implement variable
                let _ = (def, ctx, kont);
                lo.arena.compu(Computation::Hole(Hole))
            }
            | Value::Thunk(Thunk(body)) => {
                // TODO: implement thunk
                let _ = (body, ctx, kont);
                lo.arena.compu(Computation::Hole(Hole))
            }
            | Value::Ctor(Ctor(ctor, body)) => {
                // TODO: implement constructor
                let _ = (ctor, body, ctx, kont);
                lo.arena.compu(Computation::Hole(Hole))
            }
            | Value::Triv(Triv) => {
                // TODO: implement trivial value
                let _ = (ctx, kont);
                lo.arena.compu(Computation::Hole(Hole))
            }
            | Value::VCons(Cons(a, b)) => {
                // TODO: implement value cons
                let _ = (a, b, ctx, kont);
                lo.arena.compu(Computation::Hole(Hole))
            }
            | Value::TCons(Cons(_ty, inner)) => {
                // Type cons values are erased
                inner.lower(ctx, lo, kont)
            }
            | Value::Lit(lit) => {
                // TODO: implement literal
                let _ = (lit, ctx, kont);
                lo.arena.compu(Computation::Hole(Hole))
            }
        }
    }
}

impl Lower for ss::CompuId {
    type Kont = ();
    type Out = CompuId;

    fn lower(&self, ctx: StackId, lo: &mut Lowerer, kont: Self::Kont) -> Self::Out {
        let compu = lo.statics.compus[self].clone();
        use ss::Computation as Compu;
        match compu {
            | Compu::Hole(Hole) => {
                // TODO: implement panic/hole handling
                lo.arena.compu(Computation::Hole(Hole))
            }
            | Compu::VAbs(Abs(param, body)) => {
                // TODO: implement value abstraction
                let _ = (param, body, ctx, kont);
                lo.arena.compu(Computation::Hole(Hole))
            }
            | Compu::VApp(App(body, arg)) => {
                // TODO: implement value application
                let _ = (body, arg, ctx, kont);
                lo.arena.compu(Computation::Hole(Hole))
            }
            | Compu::TAbs(Abs(_param, body)) => {
                // Type abstractions are erased
                body.lower(ctx, lo, kont)
            }
            | Compu::TApp(App(body, _arg)) => {
                // Type applications are erased
                body.lower(ctx, lo, kont)
            }
            | Compu::Fix(Fix(param, body)) => {
                // TODO: implement fixpoint
                let _ = (param, body, ctx, kont);
                lo.arena.compu(Computation::Hole(Hole))
            }
            | Compu::Force(Force(body)) => {
                // TODO: implement force
                let _ = (body, ctx, kont);
                lo.arena.compu(Computation::Hole(Hole))
            }
            | Compu::Ret(Return(body)) => {
                // TODO: implement return
                let _ = (body, ctx, kont);
                lo.arena.compu(Computation::Hole(Hole))
            }
            | Compu::Do(Bind { binder, bindee, tail }) => {
                // TODO: implement do/bind
                let _ = (binder, bindee, tail, ctx, kont);
                lo.arena.compu(Computation::Hole(Hole))
            }
            | Compu::Let(Let { binder, bindee, tail }) => {
                // TODO: implement let
                let _ = (binder, bindee, tail, ctx, kont);
                lo.arena.compu(Computation::Hole(Hole))
            }
            | Compu::Match(Match { scrut, arms }) => {
                // TODO: implement match
                let _ = (scrut, arms, ctx, kont);
                lo.arena.compu(Computation::Hole(Hole))
            }
            | Compu::CoMatch(CoMatch { arms }) => {
                // TODO: implement comatch
                let _ = (arms, ctx, kont);
                lo.arena.compu(Computation::Hole(Hole))
            }
            | Compu::Dtor(Dtor(body, dtor)) => {
                // TODO: implement destructor
                let _ = (body, dtor, ctx, kont);
                lo.arena.compu(Computation::Hole(Hole))
            }
        }
    }
}
