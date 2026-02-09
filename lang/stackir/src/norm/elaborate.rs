use super::syntax::*;
use crate::sps::syntax::*;
use derive_more::{AsMut, AsRef};
use zydeco_statics::tyck::arena::StaticsArena;
use zydeco_surface::textual::arena::SpanArena;
use zydeco_utils::pass::CompilerPass;

pub trait Elaborate {
    type Out;
    fn elaborate(self, el: &mut Elaborator) -> Self::Out;
}

/// Stateful lowering pass from typed syntax into stack IR.
#[derive(AsRef, AsMut)]
pub struct Elaborator<'a> {
    #[as_ref(SNormInnerArena)]
    #[as_mut(SNormInnerArena)]
    pub arena: SNormArena,
    pub spans: &'a SpanArena,
    pub statics: &'a StaticsArena,
    #[as_ref]
    #[as_mut]
    pub stackir: &'a mut StackirInnerArena,
}

impl<'a> Elaborator<'a> {
    pub fn new(
        admin: AdminArena, spans: &'a SpanArena, statics: &'a StaticsArena,
        stackir: &'a mut StackirInnerArena,
    ) -> Self {
        Self { arena: SNormArena::new(admin), spans, statics, stackir }
    }
}

impl<'a> CompilerPass for Elaborator<'a> {
    type Arena = SNormInnerArena;
    type Out = SNormArena;
    type Error = std::convert::Infallible;
    fn run(mut self) -> Result<SNormArena, Self::Error> {
        self.arena.inner.entry = self
            .stackir
            .entry
            .clone()
            .into_iter()
            .map(|(compu_id, ())| (compu_id.elaborate(&mut self), ()))
            .collect();
        Ok(self.arena)
    }
}

impl<'a> Elaborate for VPatId {
    type Out = Self;
    fn elaborate(self, el: &mut Elaborator) -> Self::Out {
        let vpat = el.stackir.vpats[&self].clone();
        match vpat {
            | ValuePattern::Hole(Hole) => Hole.sbuild(el, self, ()),
            | ValuePattern::Var(def_id) => def_id.sbuild(el, self, ()),
            | ValuePattern::Ctor(Ctor(name, tail)) => {
                let tail = tail.elaborate(el);
                Ctor(name, tail).sbuild(el, self, ())
            }
            | ValuePattern::Triv(Triv) => Triv.sbuild(el, self, ()),
            | ValuePattern::VCons(Cons(a, b)) => {
                let a = a.elaborate(el);
                let b = b.elaborate(el);
                Cons(a, b).sbuild(el, self, ())
            }
        }
    }
}

/// Elaborate a value with only traversal, returning the same id.
impl<'a> Elaborate for ValueId {
    type Out = Self;
    fn elaborate(self, el: &mut Elaborator) -> Self::Out {
        let value = el.stackir.values[&self].clone();
        match value {
            | Value::Hole(Hole) => Hole.sbuild(el, self, ()),
            | Value::Var(def) => {
                let user = el.arena.inner.users.entry(def).or_insert_with(Vec::new);
                user.push(self);
                def.sbuild(el, self, ())
            }
            | Value::Closure(Closure { capture, stack: Bullet, body }) => {
                assert!(capture.iter().count() == 0, "capture must be empty");
                let body = body.elaborate(el);
                Closure { capture, stack: Bullet, body }.sbuild(el, self, ())
            }
            | Value::Ctor(Ctor(name, body)) => {
                let body = body.elaborate(el);
                Ctor(name, body).sbuild(el, self, ())
            }
            | Value::Triv(Triv) => Triv.sbuild(el, self, ()),
            | Value::VCons(Cons(a, b)) => {
                let a = a.elaborate(el);
                let b = b.elaborate(el);
                Cons(a, b).sbuild(el, self, ())
            }
            | Value::Literal(lit) => lit.sbuild(el, self, ()),
            | Value::Complex(Complex { operator, operands }) => {
                let operands = operands.iter().map(|operand| operand.elaborate(el)).collect();
                Complex { operator, operands }.sbuild(el, self, ())
            }
        }
    }
}

impl<'a> Elaborate for StackId {
    type Out = StackId;
    fn elaborate(self, el: &mut Elaborator) -> Self::Out {
        let stack = el.stackir.stacks[&self].clone();
        match stack {
            | Stack::Kont(Kont { binder, body }) => {
                let binder = binder.elaborate(el);
                let body = body.elaborate(el);
                let hole = el.arena.admin.allocator.alloc();
                let hole = Bullet.sbuild(el, hole, hole);
                el.arena.inner.scompus[&body].assignments.cascade_stack(hole);
                Kont { binder, body }.sbuild(el, self, hole)
            }
            | Stack::Var(Bullet) => Bullet.sbuild(el, self, self),
            | Stack::Arg(Cons(value, stack)) => {
                let value = value.elaborate(el);
                let stack = stack.elaborate(el);
                let hole = el.arena.inner.holes[&stack];
                Cons(value, stack).sbuild(el, self, hole)
            }
            | Stack::Tag(Cons(dtor, stack)) => {
                let stack = stack.elaborate(el);
                let hole = el.arena.inner.holes[&stack];
                Cons(dtor, stack).sbuild(el, self, hole)
            }
        }
    }
}

impl<'a> Elaborate for CompuId {
    type Out = Self;
    fn elaborate(self, el: &mut Elaborator) -> Self::Out {
        use Computation as Compu;
        let compu = el.stackir.compus[&self].clone();
        let elaborated = match compu {
            | Compu::Hole(Hole) => Compu::Hole(Hole).sbuild(el, self, SubstAssignments::new()),
            | Compu::Force(SForce { thunk, stack }) => {
                let thunk = thunk.elaborate(el);
                let stack = stack.elaborate(el);
                Compu::Force(SForce { thunk, stack }).sbuild(el, self, SubstAssignments::new())
            }
            | Compu::Ret(SReturn { stack, value }) => {
                let stack = stack.elaborate(el);
                let value = value.elaborate(el);
                let mut assignments = SubstAssignments::new();
                assignments.cascade_stack(stack);
                let hole = el.arena.admin.allocator.alloc();
                let hole = Bullet.sbuild(el, hole, hole);
                Compu::Ret(SReturn { stack: hole, value }).sbuild(el, self, assignments)
            }
            | Compu::Fix(SFix { capture, param, body }) => {
                assert!(capture.iter().count() == 0, "capture must be empty");
                let body = body.elaborate(el);
                Compu::Fix(SFix { capture, param, body }).sbuild(el, self, SubstAssignments::new())
            }
            | Compu::Case(Match { scrut, arms }) => {
                let scrut = scrut.elaborate(el);
                let arms = arms
                    .iter()
                    .map(|Matcher { binder, tail }| {
                        let binder = binder.elaborate(el);
                        let tail = tail.elaborate(el);
                        Matcher { binder, tail }
                    })
                    .collect();
                Match { scrut, arms }.sbuild(el, self, SubstAssignments::new())
            }
            | Compu::Join(join) => match join {
                | LetJoin::Value(Let { binder, bindee, tail }) => {
                    let bindee = bindee.elaborate(el);
                    let binder = binder.elaborate(el);
                    let tail = tail.elaborate(el);
                    el.arena.inner.scompus[&tail].assignments.cascade_value(binder, bindee);
                    tail
                }
                | LetJoin::Stack(Let { binder: Bullet, bindee, tail }) => {
                    let bindee = bindee.elaborate(el);
                    let tail = tail.elaborate(el);
                    el.arena.inner.scompus[&tail].assignments.cascade_stack(bindee);
                    tail
                }
            },
            | Compu::LetArg(Let { binder: Cons(param, Bullet), bindee, tail }) => {
                let bindee = bindee.elaborate(el);
                let param = param.elaborate(el);
                let tail = tail.elaborate(el);
                let mut assignments = SubstAssignments::new();
                assignments.cascade_stack(bindee);
                let hole = el.arena.admin.allocator.alloc();
                let hole = Bullet.sbuild(el, hole, hole);
                Let { binder: Cons(param, Bullet), bindee: hole, tail }.sbuild(
                    el,
                    self,
                    assignments,
                )
            }
            | Compu::CoCase(CoMatch { arms }) => {
                let arms = arms
                    .into_iter()
                    .map(|CoMatcher { dtor, tail }| {
                        let tail = tail.elaborate(el);
                        CoMatcher { dtor, tail }
                    })
                    .collect();
                CoMatch { arms }.sbuild(el, self, SubstAssignments::new())
            }
            | Compu::ExternCall(ExternCall { function, stack: Bullet }) => {
                ExternCall { function, stack: Bullet }.sbuild(el, self, SubstAssignments::new())
            }
        };

        // Normalize the assignments.
        let assignments = el.arena.inner.scompus[&elaborated].assignments.clone();
        let mut arena_mut =
            SNormArenaMut { admin: &mut el.arena.admin, inner: &mut el.arena.inner };
        let normalized_assignments = assignments.normalize(&mut arena_mut);
        el.arena.inner.scompus[&elaborated].assignments = normalized_assignments;

        elaborated
    }
}
