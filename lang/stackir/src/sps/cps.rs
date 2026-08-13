//! Transitional CPS translation for stack-passing ZIR.
//!
//! This bridge keeps assembly lowering simple by representing continuation
//! stacks as ordinary thunk values. It structurally rebuilds its input; the
//! paper-aligned `SPS_l` conversion will eventually replace the encoding with
//! explicit continuation packages.

use super::{check::BranchJoinProgram, syntax::*};
use derive_more::{AsMut, AsRef};
use std::convert::Infallible;
use zydeco_statics::syntax as ss;
use zydeco_surface::scoped::arena::ScopedArena;
use zydeco_syntax::VarName;
use zydeco_utils::pass::CompilerPass;

#[derive(AsRef, AsMut)]
pub struct CpsTranslator<'a> {
    source: StackirArena,
    #[as_ref(StackirArena)]
    #[as_mut(StackirArena)]
    arena: StackirArena,
    root: CompuId,
    #[as_mut(ScopedArena)]
    scoped: &'a mut ScopedArena,
}

impl<'a> CpsTranslator<'a> {
    pub fn new(program: BranchJoinProgram, scoped: &'a mut ScopedArena) -> Self {
        let StackirRebuild { source, target, root } = program.into_program().into_rebuild();
        Self { source, arena: target, root, scoped }
    }

    pub fn translate(mut self) -> BranchJoinProgram {
        let root = self.translate_compu(self.root);
        BranchJoinProgram::try_new(StackirProgram { arena: self.arena, root })
            .expect("CPS translation preserves lexical branch-join Stack IR")
    }

    fn compu_site(&self, id: CompuId) -> Option<ss::TermId> {
        self.source.admin.terms.back(&TermId::Compu(id)).copied()
    }

    fn value_site(&self, id: ValueId) -> Option<ss::TermId> {
        self.source.admin.terms.back(&TermId::Value(id)).copied()
    }

    fn stack_site(&self, id: StackId) -> Option<ss::TermId> {
        self.source.admin.terms.back(&TermId::Stack(id)).copied()
    }

    fn pattern_site(&self, id: VPatId) -> Option<ss::PatId> {
        self.source.admin.pats.back(&id).copied()
    }

    fn translate_pattern(&mut self, id: VPatId) -> VPatId {
        let site = self.pattern_site(id);
        match self.source.inner.vpats[&id].clone() {
            | ValuePattern::Hole(Hole) => Hole.build(self, site),
            | ValuePattern::Var(def) => def.build(self, site),
            | ValuePattern::Ctor(Ctor(ctor, pattern)) => {
                let pattern = self.translate_pattern(pattern);
                Ctor(ctor, pattern).build(self, site)
            }
            | ValuePattern::Alias(Alias(patterns)) => {
                let patterns =
                    patterns.into_iter().map(|pattern| self.translate_pattern(pattern)).collect();
                Alias(ConsN::from_vec(patterns).expect("an alias pattern is non-empty"))
                    .build(self, site)
            }
            | ValuePattern::Triv(Triv) => Triv.build(self, site),
            | ValuePattern::VCons(VCons { items: ConsN(items, tail), layout }) => {
                let items = items
                    .into_iter()
                    .chain([tail])
                    .map(|item| self.translate_pattern(item))
                    .collect();
                let items = ConsN::from_vec(items).expect("a product pattern is non-empty");
                VCons::new(items, layout).build(self, site)
            }
        }
    }

    fn translate_value(&mut self, id: ValueId) -> ValueId {
        let site = self.value_site(id);
        match self.source.inner.values[&id].clone() {
            | Value::Hole(Hole) => Hole.build(self, site),
            | Value::Var(def) => def.build(self, site),
            | Value::Closure(Closure { stack: Bullet, body }) => {
                let body = self.translate_compu(body);
                Closure { stack: Bullet, body }.build(self, site)
            }
            | Value::Ctor(Ctor(ctor, body)) => {
                let body = self.translate_value(body);
                Ctor(ctor, body).build(self, site)
            }
            | Value::Triv(Triv) => Triv.build(self, site),
            | Value::VCons(VCons { items: ConsN(items, tail), layout }) => {
                let items = items.into_iter().map(|item| self.translate_value(item)).collect();
                let tail = self.translate_value(tail);
                VCons::new(ConsN(items, tail), layout).build(self, site)
            }
            | Value::Literal(literal) => literal.build(self, site),
            | Value::Complex(Complex { operator, operands }) => {
                let operands =
                    operands.into_iter().map(|operand| self.translate_value(operand)).collect();
                Complex { operator, operands }.build(self, site)
            }
        }
    }

    fn translate_stack(&mut self, id: StackId) -> StackId {
        let site = self.stack_site(id);
        match self.source.inner.stacks[&id].clone() {
            | Stack::Kont(Kont { binder, body }) => self.translate_kont_stack(binder, body, site),
            | Stack::Var(Bullet) => Bullet.build(self, site),
            | Stack::Arg(Cons(value, stack)) => {
                let value = self.translate_value(value);
                let stack = self.translate_stack(stack);
                Cons(value, stack).build(self, site)
            }
            | Stack::Tag(Cons(dtor, stack)) => {
                let stack = self.translate_stack(stack);
                Cons(dtor, stack).build(self, site)
            }
        }
    }

    fn translate_kont_stack(
        &mut self, binder: VPatId, body: CompuId, site: Option<ss::TermId>,
    ) -> StackId {
        let binder = self.translate_pattern(binder);
        let body = self.translate_compu(body);
        let bindee = Bullet.build(self, site);
        let body = Let { binder: Cons(binder, Bullet), bindee, tail: body }.build(self, site);
        let kont = Closure { stack: Bullet, body }.build(self, site);
        let tail = Bullet.build(self, site);
        Cons(kont, tail).build(self, site)
    }

    fn translate_compu(&mut self, id: CompuId) -> CompuId {
        let site = self.compu_site(id);
        match self.source.inner.compus[&id].clone() {
            | Computation::Hole(SHole(stack)) => {
                let stack = self.translate_stack(stack);
                SHole(stack).build(self, site)
            }
            | Computation::Force(SForce { thunk, stack }) => {
                let thunk = self.translate_value(thunk);
                let stack = self.translate_stack(stack);
                SForce { thunk, stack }.build(self, site)
            }
            | Computation::Ret(SReturn { stack, value }) => {
                let stack = self.translate_stack(stack);
                let value = self.translate_value(value);
                self.translate_return(stack, value, site)
            }
            | Computation::Fix(SFix { param, stack, body }) => {
                let stack = self.translate_stack(stack);
                let body = self.translate_compu(body);
                SFix { param, stack, body }.build(self, site)
            }
            | Computation::ProductMatch(SProductMatch { scrut, binder, body }) => {
                let scrut = self.translate_value(scrut);
                let binder = self.translate_pattern(binder);
                let body = self.translate_compu(body);
                SProductMatch { scrut, binder, body }.build(self, site)
            }
            | Computation::CoprodMatch(SCoprodMatch { scrut, arms }) => {
                let scrut = self.translate_value(scrut);
                let arms = arms
                    .into_iter()
                    .map(|Matcher { binder, tail }| Matcher {
                        binder: self.translate_pattern(binder),
                        tail: self.translate_compu(tail),
                    })
                    .collect();
                SCoprodMatch { scrut, arms }.build(self, site)
            }
            | Computation::Join(LetJoin::Value(Let { binder, bindee, tail })) => {
                let binder = self.translate_pattern(binder);
                let bindee = self.translate_value(bindee);
                let tail = self.translate_compu(tail);
                Let { binder, bindee, tail }.build(self, site)
            }
            | Computation::Join(LetJoin::Stack(Let { binder: Bullet, bindee, tail })) => {
                let bindee = self.translate_stack(bindee);
                let tail = self.translate_compu(tail);
                Let { binder: Bullet, bindee, tail }.build(self, site)
            }
            | Computation::LetArg(Let { binder: Cons(binder, Bullet), bindee, tail }) => {
                let binder = self.translate_pattern(binder);
                let bindee = self.translate_stack(bindee);
                let tail = self.translate_compu(tail);
                Let { binder: Cons(binder, Bullet), bindee, tail }.build(self, site)
            }
            | Computation::CoCase(SCoMatch { scrut, arms }) => {
                let scrut = self.translate_stack(scrut);
                let arms = arms
                    .into_iter()
                    .map(|CoMatcher { dtor, tail }| CoMatcher {
                        dtor,
                        tail: self.translate_compu(tail),
                    })
                    .collect();
                SCoMatch { scrut, arms }.build(self, site)
            }
            | Computation::ExternCall(ExternCall { function, stack }) => {
                let stack = self.translate_stack(stack);
                ExternCall { function, stack }.build(self, site)
            }
        }
    }

    fn translate_return(
        &mut self, continuation_stack: StackId, value: ValueId, site: Option<ss::TermId>,
    ) -> CompuId {
        let kont = self.arena.admin.fresh();
        self.scoped.insert_def(kont, VarName("__k__".into()));
        let kont_pattern: VPatId = kont.build(self, None);
        let kont_value: ValueId = kont.build(self, site);
        let tail = Bullet.build(self, site);
        let call_stack = Cons(value, tail).build(self, site);
        let call = SForce { thunk: kont_value, stack: call_stack }.build(self, site);
        Let { binder: Cons(kont_pattern, Bullet), bindee: continuation_stack, tail: call }
            .build(self, site)
    }
}

impl CompilerPass for CpsTranslator<'_> {
    type Arena = StackirArena;
    type Out = BranchJoinProgram;
    type Error = Infallible;

    fn run(self) -> Result<Self::Out, Self::Error> {
        Ok(self.translate())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cps_translation_builds_a_fresh_output_arena() {
        let mut arena = StackirArena::default();
        let mut scoped = ScopedArena::default();
        let returned = arena.admin.fresh();
        scoped.insert_def(returned, VarName("returned".into()));

        let binder: VPatId = returned.build(&mut arena, None);
        let returned_value: ValueId = returned.build(&mut arena, None);
        let body_stack = Bullet.build(&mut arena, None);
        let body = SReturn { stack: body_stack, value: returned_value }.build(&mut arena, None);
        let continuation = Kont { binder, body }.build(&mut arena, None);
        let value = Triv.build(&mut arena, None);
        let root = SReturn { stack: continuation, value }.build(&mut arena, None);
        let program = BranchJoinProgram::try_new(StackirProgram { arena, root }).unwrap();

        super::super::check::check(program.as_program(), &scoped);
        let program = CpsTranslator::new(program, &mut scoped).translate();
        let output = program.as_program();

        assert_ne!(output.root, root);
        assert!(output.arena.inner.compus.get(&root).is_none());
        assert!(output.arena.inner.compus.get(&body).is_none());
        assert!(output.arena.inner.stacks.get(&continuation).is_none());
        assert!(output.arena.inner.values.get(&returned_value).is_none());
        super::super::check::check(output, &scoped);
    }
}
