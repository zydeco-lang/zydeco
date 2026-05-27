//! CPS translation for stack-passing ZIR.
//!
//! This pass keeps lowering simple and performs the Ret/do translation as an
//! explicit SPS-to-SPS middle-end step:
//!
//! - stack continuations become ordinary thunk values pushed onto the stack,
//! - returns pop that thunk and force it with the returned value.

use super::syntax::*;
use derive_more::{AsMut, AsRef};
use std::{collections::HashMap, convert::Infallible};
use zydeco_surface::scoped::arena::ScopedArena;
use zydeco_syntax::VarName;
use zydeco_utils::pass::CompilerPass;

#[derive(AsRef, AsMut)]
pub struct CpsTranslator<'a> {
    #[as_ref(StackirArena)]
    #[as_mut(StackirArena)]
    arena: &'a mut StackirArena,
    #[as_mut(ScopedArena)]
    scoped: &'a mut ScopedArena,
    values: HashMap<ValueId, ValueId>,
    stacks: HashMap<StackId, StackId>,
    compus: HashMap<CompuId, CompuId>,
}

impl<'a> CpsTranslator<'a> {
    pub fn new(arena: &'a mut StackirArena, scoped: &'a mut ScopedArena) -> Self {
        Self {
            arena,
            scoped,
            values: HashMap::new(),
            stacks: HashMap::new(),
            compus: HashMap::new(),
        }
    }

    pub fn translate(mut self) {
        let entries = self.arena.inner.entry.clone();
        let mut new_entries = ArenaAssoc::new();
        for (entry, ()) in entries {
            let entry = self.translate_compu(entry);
            new_entries.insert(entry, ());
        }
        self.arena.inner.entry = new_entries;
    }

    fn translate_value(&mut self, value_id: ValueId) -> ValueId {
        if let Some(value_id) = self.values.get(&value_id) {
            return *value_id;
        }

        let value = self.arena.inner.values[&value_id].clone();
        let new_value_id = match value {
            | Value::Hole(Hole) => Hole.build(self, None),
            | Value::Var(def_id) => def_id.build(self, None),
            | Value::Closure(Closure { stack: Bullet, body }) => {
                let body = self.translate_compu(body);
                Closure { stack: Bullet, body }.build(self, None)
            }
            | Value::Ctor(Ctor(ctor, body)) => {
                let body = self.translate_value(body);
                Ctor(ctor, body).build(self, None)
            }
            | Value::Triv(Triv) => Triv.build(self, None),
            | Value::VCons(Cons(a, b)) => {
                let a = self.translate_value(a);
                let b = self.translate_value(b);
                Cons(a, b).build(self, None)
            }
            | Value::Literal(literal) => literal.build(self, None),
            | Value::Complex(Complex { operator, operands }) => {
                let operands =
                    operands.into_iter().map(|operand| self.translate_value(operand)).collect();
                Complex { operator, operands }.build(self, None)
            }
        };
        self.values.insert(value_id, new_value_id);
        new_value_id
    }

    fn translate_stack(&mut self, stack_id: StackId) -> StackId {
        if let Some(stack_id) = self.stacks.get(&stack_id) {
            return *stack_id;
        }

        let stack = self.arena.inner.stacks[&stack_id].clone();
        let new_stack_id = match stack {
            | Stack::Kont(Kont { binder, body }) => self.translate_kont_stack(binder, body),
            | Stack::Var(Bullet) => Bullet.build(self, None),
            | Stack::Arg(Cons(value, stack)) => {
                let value = self.translate_value(value);
                let stack = self.translate_stack(stack);
                Cons(value, stack).build(self, None)
            }
            | Stack::Tag(Cons(dtor, stack)) => {
                let stack = self.translate_stack(stack);
                Cons(dtor, stack).build(self, None)
            }
        };
        self.stacks.insert(stack_id, new_stack_id);
        new_stack_id
    }

    fn translate_kont_stack(&mut self, binder: VPatId, body: CompuId) -> StackId {
        let body = self.translate_compu(body);
        let bindee = Bullet.build(self, None);
        let body = Let { binder: Cons(binder, Bullet), bindee, tail: body }.build(self, None);
        let kont = Closure { stack: Bullet, body }.build(self, None);
        let tail = Bullet.build(self, None);
        Cons(kont, tail).build(self, None)
    }

    fn translate_compu(&mut self, compu_id: CompuId) -> CompuId {
        if let Some(compu_id) = self.compus.get(&compu_id) {
            return *compu_id;
        }

        let compu = self.arena.inner.compus[&compu_id].clone();
        let new_compu_id = match compu {
            | Computation::Hole(SHole(tail)) => {
                let tail = self.translate_stack(tail);
                SHole(tail).build(self, None)
            }
            | Computation::Force(SForce { thunk, stack }) => {
                let thunk = self.translate_value(thunk);
                let stack = self.translate_stack(stack);
                SForce { thunk, stack }.build(self, None)
            }
            | Computation::Ret(SReturn { stack, value }) => {
                let stack = self.translate_stack(stack);
                let value = self.translate_value(value);
                self.translate_return(stack, value)
            }
            | Computation::Fix(SFix { param, body }) => {
                let body = self.translate_compu(body);
                SFix { param, body }.build(self, None)
            }
            | Computation::Case(Match { scrut, arms }) => {
                let scrut = self.translate_value(scrut);
                let arms = arms
                    .into_iter()
                    .map(|Matcher { binder, tail }| {
                        let tail = self.translate_compu(tail);
                        Matcher { binder, tail }
                    })
                    .collect();
                Match { scrut, arms }.build(self, None)
            }
            | Computation::Join(LetJoin::Value(Let { binder, bindee, tail })) => {
                let bindee = self.translate_value(bindee);
                let tail = self.translate_compu(tail);
                Let { binder, bindee, tail }.build(self, None)
            }
            | Computation::Join(LetJoin::Stack(Let { binder: Bullet, bindee, tail })) => {
                let bindee = self.translate_stack(bindee);
                let tail = self.translate_compu(tail);
                Let { binder: Bullet, bindee, tail }.build(self, None)
            }
            | Computation::LetArg(Let { binder: Cons(param, Bullet), bindee, tail }) => {
                let bindee = self.translate_stack(bindee);
                let tail = self.translate_compu(tail);
                Let { binder: Cons(param, Bullet), bindee, tail }.build(self, None)
            }
            | Computation::CoCase(SCoMatch { scrut, arms }) => {
                let scrut = self.translate_stack(scrut);
                let arms = arms
                    .into_iter()
                    .map(|CoMatcher { dtor, tail }| {
                        let tail = self.translate_compu(tail);
                        CoMatcher { dtor, tail }
                    })
                    .collect();
                SCoMatch { scrut, arms }.build(self, None)
            }
            | Computation::ExternCall(ExternCall { function, stack }) => {
                let stack = self.translate_stack(stack);
                ExternCall { function, stack }.build(self, None)
            }
        };
        self.compus.insert(compu_id, new_compu_id);
        new_compu_id
    }

    fn translate_return(&mut self, continuation_stack: StackId, value: ValueId) -> CompuId {
        let kont = self.scoped.defs.alloc(VarName("__k__".into()));
        let kont_pat: VPatId = kont.build(self, None);
        let kont_value: ValueId = kont.build(self, None);
        let tail = Bullet.build(self, None);
        let call_stack = Cons(value, tail).build(self, None);
        let call = SForce { thunk: kont_value, stack: call_stack }.build(self, None);
        Let { binder: Cons(kont_pat, Bullet), bindee: continuation_stack, tail: call }
            .build(self, None)
    }
}

impl<'a> CompilerPass for CpsTranslator<'a> {
    type Arena = StackirArena;
    type Out = ();
    type Error = Infallible;

    fn run(self) -> Result<Self::Out, Self::Error> {
        self.translate();
        Ok(())
    }
}
