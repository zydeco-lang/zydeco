//! Sanity checks for the stack-passing style ZIR.
//!
//! The closed-root check is intended for debugging. [`BranchJoinProgram`]
//! additionally exposes the paper's stack-join placement as a checked boundary.

use super::syntax::*;
use super::variables::FreeVars;
use std::collections::HashSet;
use zydeco_statics::surface_syntax::ScopedArena;

/// A lexical Stack IR tree whose stack joins occur exactly at value-coproduct branches.
#[derive(Debug)]
pub struct BranchJoinProgram {
    program: StackirProgram,
}

#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum BranchJoinError {
    #[error("coproduct match {compu:?} is not immediately guarded by a stack let-binding")]
    UnguardedCoprodMatch { compu: CompuId },
    #[error("stack let-binding {compu:?} has body {body:?}, which is not a coproduct match")]
    NonBranchStackLet { compu: CompuId, body: CompuId },
    #[error("computation node {compu:?} occurs more than once in lexical Stack IR")]
    SharedComputation { compu: CompuId },
    #[error("stack node {stack:?} occurs more than once in lexical Stack IR")]
    SharedStack { stack: StackId },
    #[error("value node {value:?} occurs more than once in lexical Stack IR")]
    SharedValue { value: ValueId },
    #[error("pattern node {pattern:?} occurs more than once in lexical Stack IR")]
    SharedPattern { pattern: VPatId },
}

impl BranchJoinProgram {
    pub fn try_new(program: StackirProgram) -> Result<Self, BranchJoinError> {
        BranchJoinValidator::validate(&program)?;
        Ok(Self { program })
    }

    pub fn as_program(&self) -> &StackirProgram {
        &self.program
    }

    pub fn into_program(self) -> StackirProgram {
        self.program
    }
}

impl AsRef<StackirProgram> for BranchJoinProgram {
    fn as_ref(&self) -> &StackirProgram {
        self.as_program()
    }
}

impl TryFrom<StackirProgram> for BranchJoinProgram {
    type Error = BranchJoinError;

    fn try_from(program: StackirProgram) -> Result<Self, Self::Error> {
        Self::try_new(program)
    }
}

impl From<BranchJoinProgram> for StackirProgram {
    fn from(program: BranchJoinProgram) -> Self {
        program.into_program()
    }
}

struct BranchJoinValidator<'a> {
    arena: &'a StackirInnerArena,
    compus: HashSet<CompuId>,
    stacks: HashSet<StackId>,
    values: HashSet<ValueId>,
    patterns: HashSet<VPatId>,
}

impl<'a> BranchJoinValidator<'a> {
    fn validate(program: &'a StackirProgram) -> Result<(), BranchJoinError> {
        let mut validator = Self {
            arena: &program.arena.inner,
            compus: HashSet::new(),
            stacks: HashSet::new(),
            values: HashSet::new(),
            patterns: HashSet::new(),
        };
        validator.compu(program.root, false)
    }

    fn compu(&mut self, id: CompuId, guarded: bool) -> Result<(), BranchJoinError> {
        if !self.compus.insert(id) {
            return Err(BranchJoinError::SharedComputation { compu: id });
        }

        match self.arena.compus[&id].clone() {
            | Computation::Hole(SHole(stack)) => self.stack(stack),
            | Computation::Force(SForce { thunk, stack }) => {
                self.value(thunk)?;
                self.stack(stack)
            }
            | Computation::Ret(SReturn { stack, value }) => {
                self.stack(stack)?;
                self.value(value)
            }
            | Computation::Fix(SFix { param: _, stack, body }) => {
                self.stack(stack)?;
                self.compu(body, false)
            }
            | Computation::ProductMatch(SProductMatch { scrut, binder, body }) => {
                self.value(scrut)?;
                self.pattern(binder)?;
                self.compu(body, false)
            }
            | Computation::CoprodMatch(SCoprodMatch { scrut, arms }) => {
                if !guarded {
                    return Err(BranchJoinError::UnguardedCoprodMatch { compu: id });
                }
                self.value(scrut)?;
                arms.into_iter().try_for_each(|Matcher { binder, tail }| {
                    self.pattern(binder)?;
                    self.compu(tail, false)
                })
            }
            | Computation::Join(LetJoin::Value(Let { binder, bindee, tail })) => {
                self.value(bindee)?;
                self.pattern(binder)?;
                self.compu(tail, false)
            }
            | Computation::Join(LetJoin::Stack(Let { binder: Bullet, bindee, tail })) => {
                self.stack(bindee)?;
                if !matches!(self.arena.compus[&tail], Computation::CoprodMatch(_)) {
                    return Err(BranchJoinError::NonBranchStackLet { compu: id, body: tail });
                }
                self.compu(tail, true)
            }
            | Computation::LetArg(Let { binder: Cons(binder, Bullet), bindee, tail }) => {
                self.stack(bindee)?;
                self.pattern(binder)?;
                self.compu(tail, false)
            }
            | Computation::CoCase(SCoMatch { scrut, arms }) => {
                self.stack(scrut)?;
                arms.into_iter().try_for_each(|CoMatcher { dtor: _, tail }| self.compu(tail, false))
            }
            | Computation::ExternCall(ExternCall { function: _, stack }) => self.stack(stack),
        }
    }

    fn stack(&mut self, id: StackId) -> Result<(), BranchJoinError> {
        if !self.stacks.insert(id) {
            return Err(BranchJoinError::SharedStack { stack: id });
        }

        match self.arena.stacks[&id].clone() {
            | Stack::Kont(Kont { binder, body }) => {
                self.pattern(binder)?;
                self.compu(body, false)
            }
            | Stack::Var(Bullet) => Ok(()),
            | Stack::Arg(Cons(value, stack)) => {
                self.value(value)?;
                self.stack(stack)
            }
            | Stack::Tag(Cons(_, stack)) => self.stack(stack),
        }
    }

    fn value(&mut self, id: ValueId) -> Result<(), BranchJoinError> {
        if !self.values.insert(id) {
            return Err(BranchJoinError::SharedValue { value: id });
        }

        match self.arena.values[&id].clone() {
            | Value::Hole(Hole) | Value::Var(_) | Value::Triv(Triv) | Value::Literal(_) => Ok(()),
            | Value::Closure(Closure { stack: Bullet, body }) => self.compu(body, false),
            | Value::Ctor(Ctor(_, value)) => self.value(value),
            | Value::VCons(VCons { items, layout: _ }) => {
                items.into_iter().try_for_each(|value| self.value(value))
            }
            | Value::Complex(Complex { operator: _, operands }) => {
                operands.into_iter().try_for_each(|value| self.value(value))
            }
        }
    }

    fn pattern(&mut self, id: VPatId) -> Result<(), BranchJoinError> {
        if !self.patterns.insert(id) {
            return Err(BranchJoinError::SharedPattern { pattern: id });
        }

        match self.arena.vpats[&id].clone() {
            | ValuePattern::Hole(Hole) | ValuePattern::Var(_) | ValuePattern::Triv(Triv) => Ok(()),
            | ValuePattern::Ctor(Ctor(_, pattern)) => self.pattern(pattern),
            | ValuePattern::Alias(Alias(patterns)) => {
                patterns.into_iter().try_for_each(|pattern| self.pattern(pattern))
            }
            | ValuePattern::VCons(VCons { items, layout: _ }) => {
                items.into_iter().try_for_each(|pattern| self.pattern(pattern))
            }
        }
    }
}

/// Check that the given stack IR arena is well-formed enough for debugging.
///
/// This function panics if the root computation has free variables.
pub fn check(program: &StackirProgram, scoped: &ScopedArena) {
    check_closed_root(program, scoped);
}

/// Ensure that the program root is closed (has no free variables).
fn check_closed_root(program: &StackirProgram, scoped: &ScopedArena) {
    let fv = program.root.free_vars(&program.arena);
    let fv_str = fv
        .iter()
        .map(|def| {
            let name = &scoped.defs[def];
            format!("{}{}", name.plain(), def.concise())
        })
        .collect::<Vec<_>>()
        .join(", ");
    if !fv.is_empty() {
        panic!("stack IR root {:?} is not closed; free variables: {}", program.root, fv_str);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Fixture {
        arena: StackirArena,
        scrut: ValueId,
        branch: CompuId,
    }

    impl Fixture {
        fn new() -> Self {
            let mut arena = StackirArena::default();
            let scrut = Triv.build(&mut arena, None);
            let stack = Bullet.build(&mut arena, None);
            let value = Triv.build(&mut arena, None);
            let branch = SReturn { stack, value }.build(&mut arena, None);
            Self { arena, scrut, branch }
        }

        fn coprod_match(&mut self) -> CompuId {
            let binder = Triv.build(&mut self.arena, None);
            SCoprodMatch { scrut: self.scrut, arms: vec![Matcher { binder, tail: self.branch }] }
                .build(&mut self.arena, None)
        }
    }

    #[test]
    fn branch_join_accepts_a_guarded_coproduct_match() {
        let mut fixture = Fixture::new();
        let body = fixture.coprod_match();
        let bindee = Bullet.build(&mut fixture.arena, None);
        let root = Let { binder: Bullet, bindee, tail: body }.build(&mut fixture.arena, None);

        let program = StackirProgram { arena: fixture.arena, root };
        assert!(BranchJoinProgram::try_new(program).is_ok());
    }

    #[test]
    fn branch_join_rejects_an_unguarded_coproduct_match() {
        let mut fixture = Fixture::new();
        let root = fixture.coprod_match();
        let program = StackirProgram { arena: fixture.arena, root };

        assert_eq!(
            BranchJoinProgram::try_new(program).unwrap_err(),
            BranchJoinError::UnguardedCoprodMatch { compu: root }
        );
    }

    #[test]
    fn branch_join_rejects_a_non_branch_stack_let() {
        let mut fixture = Fixture::new();
        let bindee = Bullet.build(&mut fixture.arena, None);
        let root =
            Let { binder: Bullet, bindee, tail: fixture.branch }.build(&mut fixture.arena, None);
        let program = StackirProgram { arena: fixture.arena, root };

        assert_eq!(
            BranchJoinProgram::try_new(program).unwrap_err(),
            BranchJoinError::NonBranchStackLet { compu: root, body: fixture.branch }
        );
    }

    #[test]
    fn branch_join_rejects_implicit_arena_sharing() {
        let mut fixture = Fixture::new();
        let value = Triv.build(&mut fixture.arena, None);
        let pair = VCons::new(ConsN(vec![value], value), ProductLayout { arity: 2 })
            .build(&mut fixture.arena, None);
        let stack = Bullet.build(&mut fixture.arena, None);
        let root = SReturn { stack, value: pair }.build(&mut fixture.arena, None);
        let program = StackirProgram { arena: fixture.arena, root };

        assert_eq!(
            BranchJoinProgram::try_new(program).unwrap_err(),
            BranchJoinError::SharedValue { value }
        );
    }
}
