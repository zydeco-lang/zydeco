//! Structural validation for first-order SPS.

use super::syntax::*;
use super::traverse::{Edge, Node, Occurrence, Together, Traversal, Visitor};
use super::variables::Variables;
use std::collections::HashSet;
use zydeco_utils::fold::{Driver, Explicit};

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
    #[error("continuation {stack:?} has an invalid capture or entry context")]
    ContinuationContext { stack: StackId },
    #[error(transparent)]
    EntryContract(#[from] super::contracts::EntryContractError),
    #[error(transparent)]
    Protocol(#[from] super::protocols::ProtocolError),
}

impl SpsLowProgram {
    pub fn try_new(arena: SpsLowArena, root: CompuId) -> Result<Self, SpsLowError> {
        let facts = SpsLowValidator::validate_with_driver::<Explicit>(&arena.inner, root)?;
        let mut variables = facts
            .free_variables(root.into())
            .expect("validated syntax")
            .iter()
            .copied()
            .collect::<Vec<_>>();
        variables.sort_unstable();
        if !variables.is_empty() {
            return Err(SpsLowError::OpenRoot { variables });
        }
        drop(facts);
        super::contracts::EntryValidator::validate(&arena.inner, root)?;
        super::protocols::ProtocolValidator::validate(&arena.inner, root)?;
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

#[derive(Default)]
struct SpsLowValidator {
    error: Option<SpsLowError>,
    stacks: HashSet<StackId>,
    labels: HashSet<DefId>,
    blocks: Vec<ValueId>,
}

impl ContinuationEntry {
    fn matches_package(
        &self, arena: &SpsLowInnerArena, stack: StackId, variables: &Variables,
    ) -> bool {
        let Some(Stack::ContinuationPackage(ContinuationPackage { code, residual })) =
            arena.stacks.get(&stack)
        else {
            return false;
        };
        let Some(Value::Block(Block { label, entry, body })) = arena.values.get(code) else {
            return false;
        };
        let EntryParameters::Continuation { result, environment } = *entry else { return false };
        if result != self.result || *body != self.body {
            return false;
        }
        let Stack::Arg(Cons(value, ambient)) = arena.stacks[residual] else { return false };
        if !matches!(arena.stacks[&ambient], Stack::Var(Bullet)) {
            return false;
        }
        let (values, patterns) = match (&arena.values[&value], &arena.vpats[&environment]) {
            | (Value::Triv(_), ValuePattern::Triv(_)) => (&[][..], &[][..]),
            | (Value::VCons(values), ValuePattern::VCons(patterns))
                if values.layout.arity == values.items.len()
                    && patterns.layout.arity == patterns.items.len() =>
            {
                (values.items.as_slice(), patterns.items.as_slice())
            }
            | _ => return false,
        };
        if values.len() != self.captures.len() || patterns.len() != self.captures.len() {
            return false;
        }
        if !values.iter().zip(patterns).zip(&self.captures).all(|((value, pattern), capture)| {
            matches!(arena.values[value], Value::Var(source) if source == capture.source)
                && matches!(arena.vpats[pattern], ValuePattern::Var(binding) if binding == capture.binding)
        }) { return false; }
        let bindings = self.captures.iter().map(|capture| capture.binding).collect::<HashSet<_>>();
        let free = variables
            .free_variables(self.body.into())
            .expect("validated continuation body")
            .clone()
            - variables
                .bound_variables(self.result)
                .expect("validated continuation result")
                .clone();
        bindings.len() == self.captures.len()
            && !free.iter().any(|variable| variable == label)
            && free.iter().all(|variable| bindings.contains(variable))
    }
}

impl SpsLowValidator {
    fn validate_with_driver<D: Driver>(
        arena: &SpsLowInnerArena, root: CompuId,
    ) -> Result<Variables, SpsLowError> {
        let mut analyses = Together { first: Self::default(), second: Variables::default() };
        Traversal { arena }.run_with_driver::<D>(root.into(), &mut analyses);
        let Together { first: validator, second: variables } = analyses;
        if let Some(error) = validator.error {
            return Err(error);
        }
        for block in validator.blocks {
            let Value::Block(Block { label, .. }) = arena.values[&block] else { unreachable!() };
            let captures = variables
                .free_variables(block.into())
                .expect("validated block")
                .iter()
                .copied()
                .collect::<Vec<_>>();
            if !captures.is_empty() {
                return Err(SpsLowError::ImplicitBlockCapture { label, captures });
            }
        }
        for (stack, entry) in &arena.continuations {
            if !validator.stacks.contains(stack)
                || !entry.matches_package(arena, *stack, &variables)
            {
                return Err(SpsLowError::ContinuationContext { stack: *stack });
            }
        }
        Ok(variables)
    }

    fn check(
        &mut self, node: Node<'_>, edge: Edge, occurrence: Occurrence,
    ) -> Result<(), SpsLowError> {
        if occurrence != Occurrence::First {
            return Err(match node {
                | Node::Pattern(pattern, _) => SpsLowError::SharedPattern { pattern },
                | Node::Value(value, _) => SpsLowError::SharedValue { value },
                | Node::Stack(stack, _) => SpsLowError::SharedStack { stack },
                | Node::Computation(compu, _) => SpsLowError::SharedComputation { compu },
            });
        }
        match node {
            | Node::Pattern(_, _) => {}
            | Node::Value(id, Value::Block(Block { label, .. })) => {
                if !self.labels.insert(*label) {
                    return Err(SpsLowError::DuplicateBlockLabel { label: *label });
                }
                self.blocks.push(id);
            }
            | Node::Value(_, _) => {}
            | Node::Stack(id, _) => {
                self.stacks.insert(id);
            }
            | Node::Computation(id, computation) => {
                if let Edge::BranchJoin(compu) = edge {
                    if !matches!(computation, Computation::CoprodMatch(_)) {
                        return Err(SpsLowError::NonBranchStackLet { compu, body: id });
                    }
                } else if matches!(computation, Computation::CoprodMatch(_)) {
                    return Err(SpsLowError::UnguardedCoprodMatch { compu: id });
                }
            }
        }
        Ok(())
    }
}

impl Visitor for SpsLowValidator {
    fn enter(&mut self, node: Node<'_>, edge: Edge, occurrence: Occurrence) {
        if self.error.is_none() {
            self.error = self.check(node, edge, occurrence).err();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use zydeco_utils::fold::Recursive;

    struct Fixture;

    impl Fixture {
        fn program(arena: SpsLowArena, root: CompuId) -> Result<SpsLowProgram, SpsLowError> {
            let explicit = SpsLowValidator::validate_with_driver::<Explicit>(&arena.inner, root);
            let recursive = SpsLowValidator::validate_with_driver::<Recursive>(&arena.inner, root);
            assert_eq!(explicit.err(), recursive.err());
            SpsLowProgram::try_new(arena, root)
        }

        fn argument(arena: &mut SpsLowArena) -> EntryArgument {
            EntryArgument::Closure { environment: Triv.build(arena, None) }
        }

        fn entry(arena: &mut SpsLowArena) -> EntryParameters {
            EntryParameters::Closure { environment: Hole.build(arena, None) }
        }
    }

    #[test]
    fn low_program_rejects_implicit_arena_sharing() {
        let mut arena = SpsLowArena::default();
        let shared = Triv.build(&mut arena, None);
        let package = ClosurePackage { environment: shared, code: shared }.build(&mut arena, None);
        let stack = Bullet.build(&mut arena, None);
        let root = Jump { argument: Fixture::argument(&mut arena), target: package, stack }
            .build(&mut arena, None);

        assert_eq!(
            Fixture::program(arena, root).unwrap_err(),
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
        let body = Jump {
            argument: Fixture::argument(&mut arena),
            target: captured_value,
            stack: body_stack,
        }
        .build(&mut arena, None);
        let block =
            Block { entry: Fixture::entry(&mut arena), label, body }.build(&mut arena, None);
        let root_stack = Bullet.build(&mut arena, None);
        let root =
            Jump { argument: Fixture::argument(&mut arena), target: block, stack: root_stack }
                .build(&mut arena, None);

        assert_eq!(
            Fixture::program(arena, root).unwrap_err(),
            SpsLowError::ImplicitBlockCapture { label, captures: vec![captured] }
        );
    }

    #[test]
    fn low_program_rejects_an_open_root() {
        let mut arena = SpsLowArena::default();
        let free = arena.admin.fresh_def();
        let target: ValueId = free.build(&mut arena, None);
        let stack = Bullet.build(&mut arena, None);
        let root =
            Jump { argument: Fixture::argument(&mut arena), target, stack }.build(&mut arena, None);

        assert_eq!(
            Fixture::program(arena, root).unwrap_err(),
            SpsLowError::OpenRoot { variables: vec![free] }
        );
    }

    #[test]
    fn low_program_rejects_duplicate_block_labels() {
        let mut arena = SpsLowArena::default();
        let label = arena.admin.fresh_def();

        let first_target: ValueId = label.build(&mut arena, None);
        let first_stack = Bullet.build(&mut arena, None);
        let first_body = Jump {
            argument: Fixture::argument(&mut arena),
            target: first_target,
            stack: first_stack,
        }
        .build(&mut arena, None);
        let first = Block { entry: Fixture::entry(&mut arena), label, body: first_body }
            .build(&mut arena, None);

        let second_target: ValueId = label.build(&mut arena, None);
        let second_stack = Bullet.build(&mut arena, None);
        let second_body = Jump {
            argument: Fixture::argument(&mut arena),
            target: second_target,
            stack: second_stack,
        }
        .build(&mut arena, None);
        let second = Block { entry: Fixture::entry(&mut arena), label, body: second_body }
            .build(&mut arena, None);

        let package = ClosurePackage { environment: first, code: second }.build(&mut arena, None);
        let root_stack = Bullet.build(&mut arena, None);
        let root =
            Jump { argument: Fixture::argument(&mut arena), target: package, stack: root_stack }
                .build(&mut arena, None);

        assert_eq!(
            Fixture::program(arena, root).unwrap_err(),
            SpsLowError::DuplicateBlockLabel { label }
        );
    }

    #[test]
    fn branch_join_edges_accept_only_an_immediate_coproduct_body() {
        let mut arena = SpsLowArena::default();
        let scrut = Triv.build(&mut arena, None);
        let branch = SCoprodMatch { scrut, arms: vec![] }.build(&mut arena, None);
        for error in [
            SpsLowValidator::validate_with_driver::<Explicit>(&arena.inner, branch).err(),
            SpsLowValidator::validate_with_driver::<Recursive>(&arena.inner, branch).err(),
        ] {
            assert_eq!(error, Some(SpsLowError::UnguardedCoprodMatch { compu: branch }));
        }
        let stack = Bullet.build(&mut arena, None);
        let root = LetStack { binder: Bullet, bindee: stack, tail: branch }.build(&mut arena, None);
        let program = Fixture::program(arena, root).unwrap();
        let (mut arena, root) = program.into_parts();
        let stack = Bullet.build(&mut arena, None);
        arena.inner.compus[&branch] = SHole(stack).into();
        assert_eq!(
            Fixture::program(arena, root).unwrap_err(),
            SpsLowError::NonBranchStackLet { compu: root, body: branch }
        );
    }

    #[test]
    fn cyclic_blocks_are_rejected_before_capture_summaries_are_consumed() {
        let mut arena = SpsLowArena::default();
        let stack = Bullet.build(&mut arena, None);
        let body = SHole(stack).build(&mut arena, None);
        let binder = Hole.build(&mut arena, None);
        let bindee = Triv.build(&mut arena, None);
        arena.inner.compus[&body] = LetValue { binder, bindee, tail: body }.into();
        let label = arena.admin.fresh_def();
        let block =
            Block { label, entry: Fixture::entry(&mut arena), body }.build(&mut arena, None);
        let ambient = Bullet.build(&mut arena, None);
        let stack = Cons(block, ambient).build(&mut arena, None);
        let root = SHole(stack).build(&mut arena, None);
        assert_eq!(
            Fixture::program(arena, root).unwrap_err(),
            SpsLowError::SharedComputation { compu: body }
        );
    }

    #[test]
    fn deep_block_validation_uses_completed_variable_summaries() {
        std::thread::Builder::new()
            .stack_size(512 * 1024)
            .spawn(|| {
                let mut arena = SpsLowArena::default();
                let stack = Bullet.build(&mut arena, None);
                let mut body = SHole(stack).build(&mut arena, None);
                for _ in 0..16_384 {
                    let binder = Hole.build(&mut arena, None);
                    let bindee = Triv.build(&mut arena, None);
                    body = LetValue { binder, bindee, tail: body }.build(&mut arena, None);
                }
                let label = arena.admin.fresh_def();
                let block = Block { label, entry: Fixture::entry(&mut arena), body }
                    .build(&mut arena, None);
                let ambient = Bullet.build(&mut arena, None);
                let stack = Cons(block, ambient).build(&mut arena, None);
                let root = SHole(stack).build(&mut arena, None);
                let variables =
                    SpsLowValidator::validate_with_driver::<Explicit>(&arena.inner, root).unwrap();
                assert!(variables.free_variables(root.into()).unwrap().is_empty());
            })
            .unwrap()
            .join()
            .unwrap();
    }
}
