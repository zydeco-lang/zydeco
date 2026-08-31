//! Structural validation for first-order SPS.

use super::syntax::*;
use super::variables::FreeVars as _;
use std::collections::HashSet;

/// A lexical first-order SPS tree whose joins remain attached to coproduct branches.
#[derive(Debug)]
pub struct SpsLowProgram {
    arena: FrozenArena<SpsLowArena>,
    root: CompuId,
}

#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum SpsLowError {
    #[error("coproduct match {compu:?} is not immediately guarded by a stack let-binding")]
    UnguardedCoprodMatch { compu: CompuId },
    #[error("stack let-binding {compu:?} has body {body:?}, which is not a coproduct match")]
    NonBranchStackLet { compu: CompuId, body: CompuId },
    #[error("computation node {compu:?} occurs more than once in lexical SPSLow")]
    SharedComputation { compu: CompuId },
    #[error("stack node {stack:?} occurs more than once in lexical SPSLow")]
    SharedStack { stack: StackId },
    #[error("value node {value:?} occurs more than once in lexical SPSLow")]
    SharedValue { value: ValueId },
    #[error("pattern node {pattern:?} occurs more than once in lexical SPSLow")]
    SharedPattern { pattern: VPatId },
    #[error("block label {label:?} is bound by more than one SPSLow block")]
    DuplicateBlockLabel { label: DefId },
    #[error("SPSLow block {label:?} still captures values implicitly: {captures:?}")]
    ImplicitBlockCapture { label: DefId, captures: Vec<DefId> },
    #[error("SPSLow root still has free value variables: {variables:?}")]
    OpenRoot { variables: Vec<DefId> },
}

impl SpsLowProgram {
    pub fn try_new(arena: SpsLowArena, root: CompuId) -> Result<Self, SpsLowError> {
        SpsLowValidator::validate(&arena.inner, root)?;
        let mut variables = root.free_vars(&arena.inner).into_iter().collect::<Vec<_>>();
        variables.sort_unstable();
        if !variables.is_empty() {
            return Err(SpsLowError::OpenRoot { variables });
        }
        Ok(Self { arena: FrozenArena::new(arena), root })
    }

    pub fn arena(&self) -> &SpsLowArena {
        &self.arena
    }

    pub fn root(&self) -> CompuId {
        self.root
    }

    pub fn into_parts(self) -> (SpsLowArena, CompuId) {
        (self.arena.into_inner(), self.root)
    }
}

impl AsRef<SpsLowArena> for SpsLowProgram {
    fn as_ref(&self) -> &SpsLowArena {
        self.arena()
    }
}

struct SpsLowValidator<'a> {
    arena: &'a SpsLowInnerArena,
    compus: HashSet<CompuId>,
    stacks: HashSet<StackId>,
    values: HashSet<ValueId>,
    patterns: HashSet<VPatId>,
    labels: HashSet<DefId>,
}

impl<'a> SpsLowValidator<'a> {
    fn validate(arena: &'a SpsLowInnerArena, root: CompuId) -> Result<(), SpsLowError> {
        let mut validator = Self {
            arena,
            compus: HashSet::new(),
            stacks: HashSet::new(),
            values: HashSet::new(),
            patterns: HashSet::new(),
            labels: HashSet::new(),
        };
        validator.compu(root, false)
    }

    fn compu(&mut self, id: CompuId, guarded: bool) -> Result<(), SpsLowError> {
        if !self.compus.insert(id) {
            return Err(SpsLowError::SharedComputation { compu: id });
        }

        match self.arena.compus[&id].clone() {
            | Computation::Hole(SHole(stack)) => self.stack(stack),
            | Computation::Jump(Jump { target, stack }) => {
                self.value(target)?;
                self.stack(stack)
            }
            | Computation::ProductMatch(SProductMatch { scrut, binder, body }) => {
                self.value(scrut)?;
                self.pattern(binder)?;
                self.compu(body, false)
            }
            | Computation::CoprodMatch(SCoprodMatch { scrut, arms }) => {
                if !guarded {
                    return Err(SpsLowError::UnguardedCoprodMatch { compu: id });
                }
                self.value(scrut)?;
                arms.into_iter().try_for_each(|Matcher { binder, tail }| {
                    self.pattern(binder)?;
                    self.compu(tail, false)
                })
            }
            | Computation::LetValue(LetValue { binder, bindee, body }) => {
                self.value(bindee)?;
                self.pattern(binder)?;
                self.compu(body, false)
            }
            | Computation::LetStack(LetStack { bindee, body }) => {
                self.stack(bindee)?;
                if !matches!(self.arena.compus[&body], Computation::CoprodMatch(_)) {
                    return Err(SpsLowError::NonBranchStackLet { compu: id, body });
                }
                self.compu(body, true)
            }
            | Computation::LetArg(LetArg { binder, bindee, body }) => {
                self.stack(bindee)?;
                self.pattern(binder)?;
                self.compu(body, false)
            }
            | Computation::CoCase(SCoMatch { scrut, arms }) => {
                self.stack(scrut)?;
                arms.into_iter().try_for_each(|CoMatcher { dtor: _, tail }| self.compu(tail, false))
            }
            | Computation::OpenClosure(OpenClosure { package, environment, code, body }) => {
                self.value(package)?;
                self.pattern(environment)?;
                self.pattern(code)?;
                self.compu(body, false)
            }
            | Computation::OpenContinuation(OpenContinuation { package, code, body }) => {
                self.stack(package)?;
                self.pattern(code)?;
                self.compu(body, false)
            }
            | Computation::ExternCall(ExternCall { function: _, stack }) => self.stack(stack),
        }
    }

    fn stack(&mut self, id: StackId) -> Result<(), SpsLowError> {
        if !self.stacks.insert(id) {
            return Err(SpsLowError::SharedStack { stack: id });
        }

        match self.arena.stacks[&id].clone() {
            | Stack::Var(Bullet) => Ok(()),
            | Stack::Arg(Cons(value, stack)) => {
                self.value(value)?;
                self.stack(stack)
            }
            | Stack::Tag(Cons(_, stack)) => self.stack(stack),
            | Stack::ContinuationPackage(ContinuationPackage { code, residual }) => {
                self.value(code)?;
                self.stack(residual)
            }
        }
    }

    fn value(&mut self, id: ValueId) -> Result<(), SpsLowError> {
        if !self.values.insert(id) {
            return Err(SpsLowError::SharedValue { value: id });
        }

        match self.arena.values[&id].clone() {
            | Value::Hole(Hole) | Value::Var(_) | Value::Triv(Triv) | Value::Literal(_) => Ok(()),
            | Value::Block(Block { label, body }) => {
                if !self.labels.insert(label) {
                    return Err(SpsLowError::DuplicateBlockLabel { label });
                }
                let mut captures = body.free_vars(self.arena).into_iter().collect::<Vec<_>>();
                captures.retain(|capture| *capture != label);
                captures.sort_unstable();
                if !captures.is_empty() {
                    return Err(SpsLowError::ImplicitBlockCapture { label, captures });
                }
                self.compu(body, false)
            }
            | Value::ClosurePackage(ClosurePackage { environment, code }) => {
                self.value(environment)?;
                self.value(code)
            }
            | Value::Ctor(Ctor(_, value)) => self.value(value),
            | Value::VCons(VCons { items, layout: _ }) => {
                items.into_iter().try_for_each(|value| self.value(value))
            }
            | Value::Complex(Complex { operator: _, operands }) => {
                operands.into_iter().try_for_each(|value| self.value(value))
            }
        }
    }

    fn pattern(&mut self, id: VPatId) -> Result<(), SpsLowError> {
        if !self.patterns.insert(id) {
            return Err(SpsLowError::SharedPattern { pattern: id });
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sps_low::arena::Construct as _;

    #[test]
    fn low_program_rejects_implicit_arena_sharing() {
        let mut arena = SpsLowArena::default();
        let shared = Triv.build(&mut arena, None);
        let package = ClosurePackage { environment: shared, code: shared }.build(&mut arena, None);
        let stack = Bullet.build(&mut arena, None);
        let root = Jump { target: package, stack }.build(&mut arena, None);

        assert_eq!(
            SpsLowProgram::try_new(arena, root).unwrap_err(),
            SpsLowError::SharedValue { value: shared }
        );
    }

    #[test]
    fn low_program_rejects_an_implicitly_capturing_block() {
        let mut arena = SpsLowArena::default();
        let label = arena.admin.fresh_def();
        let captured = arena.admin.fresh_def();
        let captured_value: ValueId = captured.build(&mut arena, None);
        let body_stack = Bullet.build(&mut arena, None);
        let body = Jump { target: captured_value, stack: body_stack }.build(&mut arena, None);
        let block = Block { label, body }.build(&mut arena, None);
        let root_stack = Bullet.build(&mut arena, None);
        let root = Jump { target: block, stack: root_stack }.build(&mut arena, None);

        assert_eq!(
            SpsLowProgram::try_new(arena, root).unwrap_err(),
            SpsLowError::ImplicitBlockCapture { label, captures: vec![captured] }
        );
    }

    #[test]
    fn low_program_rejects_an_open_root() {
        let mut arena = SpsLowArena::default();
        let free = arena.admin.fresh_def();
        let target: ValueId = free.build(&mut arena, None);
        let stack = Bullet.build(&mut arena, None);
        let root = Jump { target, stack }.build(&mut arena, None);

        assert_eq!(
            SpsLowProgram::try_new(arena, root).unwrap_err(),
            SpsLowError::OpenRoot { variables: vec![free] }
        );
    }

    #[test]
    fn low_program_rejects_duplicate_block_labels() {
        let mut arena = SpsLowArena::default();
        let label = arena.admin.fresh_def();

        let first_target: ValueId = label.build(&mut arena, None);
        let first_stack = Bullet.build(&mut arena, None);
        let first_body = Jump { target: first_target, stack: first_stack }.build(&mut arena, None);
        let first = Block { label, body: first_body }.build(&mut arena, None);

        let second_target: ValueId = label.build(&mut arena, None);
        let second_stack = Bullet.build(&mut arena, None);
        let second_body =
            Jump { target: second_target, stack: second_stack }.build(&mut arena, None);
        let second = Block { label, body: second_body }.build(&mut arena, None);

        let package = ClosurePackage { environment: first, code: second }.build(&mut arena, None);
        let root_stack = Bullet.build(&mut arena, None);
        let root = Jump { target: package, stack: root_stack }.build(&mut arena, None);

        assert_eq!(
            SpsLowProgram::try_new(arena, root).unwrap_err(),
            SpsLowError::DuplicateBlockLabel { label }
        );
    }
}
