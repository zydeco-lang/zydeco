use super::{
    substitute::{SubstVarInPlace, SubstVarMap},
    syntax::*,
};
use derive_more::{AsMut, AsRef};
use zydeco_surface::scoped::arena::ScopedArena;
use zydeco_utils::pass::CompilerPass;

pub trait InlineTopLevel<'a> {
    fn inline_top_level(self, il: &mut Inliner<'a>) -> Self;
}

#[derive(AsRef, AsMut)]
pub struct Inliner<'a> {
    #[as_ref(StackirArena)]
    #[as_mut(StackirArena)]
    pub stackir: &'a mut StackirArena,
    #[as_mut(ScopedArena)]
    pub scoped: &'a mut ScopedArena,
}

impl<'a> Inliner<'a> {
    pub fn new(stackir: &'a mut StackirArena, scoped: &'a mut ScopedArena) -> Self {
        Self { stackir, scoped }
    }
}

impl<'a> CompilerPass for Inliner<'a> {
    type Arena = StackirArena;
    type Out = ();
    type Error = std::convert::Infallible;
    fn run(mut self) -> Result<(), Self::Error> {
        let mut new_entry = ArenaAssoc::new();
        for (compu_id, ()) in self.stackir.inner.entry.clone() {
            new_entry.ensure(compu_id.inline_top_level(&mut self));
        }
        self.stackir.inner.entry = new_entry;
        Ok(())
    }
}

/// See crate doc [self] for more details.
impl<'a> InlineTopLevel<'a> for CompuId {
    fn inline_top_level(self, il: &mut Inliner<'a>) -> Self {
        let compu = il.stackir.inner.compus[&self].clone();
        use Computation as Compu;
        match compu {
            | Compu::Hole(SHole(tail)) => SHole(tail.inline_top_level(il)).build(il, None),
            | Compu::Force(SForce { thunk, stack }) => {
                let thunk = thunk.inline_top_level(il);
                let stack = stack.inline_top_level(il);
                SForce { thunk, stack }.build(il, None)
            }
            | Compu::Ret(SReturn { stack, value }) => {
                let stack = stack.inline_top_level(il);
                let value = value.inline_top_level(il);
                SReturn { stack, value }.build(il, None)
            }
            | Compu::Fix(SFix { param, body }) => {
                let body = body.inline_top_level(il);
                SFix { param, body }.build(il, None)
            }
            | Compu::Case(Match { scrut, arms }) => {
                let scrut = scrut.inline_top_level(il);
                let arms = arms
                    .iter()
                    .map(|Matcher { binder, tail }| {
                        let binder = binder.inline_top_level(il);
                        let tail = tail.inline_top_level(il);
                        Matcher { binder, tail }.into()
                    })
                    .collect();
                Match { scrut, arms }.build(il, None)
            }
            | Compu::Join(join) => match join {
                | LetJoin::Value(Let { binder, bindee, tail }) => {
                    // the binder must be a simply variable pattern
                    use ValuePattern as VPat;
                    let def_id = match il.stackir.inner.vpats[&binder].clone() {
                        | VPat::Var(def_id) => Some(def_id),
                        | _ => None,
                    };
                    if let Some(def_id) = def_id {
                        tail.subst_var_in_place(
                            il,
                            &mut SubstVarMap::from_iter([(def_id, bindee)]),
                        );
                        tail.inline_top_level(il)
                    } else {
                        let tail = tail.inline_top_level(il);
                        Let { binder, bindee, tail }.build(il, None)
                    }
                }
                | LetJoin::Stack(_) => self,
            },
            | Compu::LetArg(Let { binder: Cons(param, Bullet), bindee, tail }) => {
                let bindee = bindee.inline_top_level(il);
                let param = param.inline_top_level(il);
                let tail = tail.inline_top_level(il);
                Let { binder: Cons(param, Bullet), bindee, tail }.build(il, None)
            }
            | Compu::CoCase(SCoMatch { scrut, arms }) => {
                let scrut = scrut.inline_top_level(il);
                let arms = arms
                    .into_iter()
                    .map(|CoMatcher { dtor, tail }| {
                        let tail = tail.inline_top_level(il);
                        CoMatcher { dtor, tail }
                    })
                    .collect();
                SCoMatch { scrut, arms }.build(il, None)
            }
            | Compu::ExternCall(ExternCall { function, stack }) => {
                let stack = stack.inline_top_level(il);
                ExternCall { function, stack }.build(il, None)
            }
        }
    }
}

impl<'a> InlineTopLevel<'a> for StackId {
    fn inline_top_level(self, il: &mut Inliner<'a>) -> Self {
        let stack = il.stackir.inner.stacks[&self].clone();
        use Stack;
        match stack {
            | Stack::Kont(Kont { binder: _, body }) => {
                body.inline_top_level(il);
            }
            | Stack::Var(Bullet) => {}
            | Stack::Arg(Cons(value, stack)) => {
                value.inline_top_level(il);
                stack.inline_top_level(il);
            }
            | Stack::Tag(Cons(_, stack)) => {
                stack.inline_top_level(il);
            }
        }
        self
    }
}

impl<'a> InlineTopLevel<'a> for ValueId {
    fn inline_top_level(self, _il: &mut Inliner<'a>) -> Self {
        self
    }
}

impl<'a> InlineTopLevel<'a> for VPatId {
    fn inline_top_level(self, _il: &mut Inliner<'a>) -> Self {
        self
    }
}
