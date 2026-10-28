//! Structural validation for first-order SPS.

use super::entry::EntryKind;
use super::syntax::*;
use super::traverse::{Edge, EntityId, Node, Occurrence, Together, Traversal, Visitor};
use super::variables::Variables;
use crate::scheme::{FrameShape, PlacementError, SpsScheme, StackUse};
use std::collections::{HashMap, HashSet};
use zydeco_utils::fold::{Driver, Explicit};

/// A lexical first-order SPS tree that keeps the stack placement of its scheme's fragment.
#[derive(Debug)]
pub struct SpsLowProgram {
    arena: FrozenArena<SpsLowArena>,
    root: CompuId,
    scheme: SpsScheme,
}

#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum SpsLowValidationError {
    #[error("computation {compu:?} leaves its scheme's fragment: {error}")]
    Placement { compu: CompuId, error: PlacementError },
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
    #[error("SPSLow label {label:?} is bound more than once")]
    DuplicateLabel { label: DefId },
    #[error("computation {compu:?} jumps to label {label:?} outside the tail that binds it")]
    UnboundLabel { compu: CompuId, label: DefId },
    #[error(
        "computation {compu:?} jumps from inside a block to local label {label:?}, whose body reads the frame of the block that binds it"
    )]
    LocalLabelAcrossBlock { compu: CompuId, label: DefId },
    #[error("computation {compu:?} jumps to label {label:?} from inside a closure block")]
    LabelUnderClosure { compu: CompuId, label: DefId },
    #[error("closed SPSLow label {label:?} still reads variables: {captures:?}")]
    ImplicitLabelCapture { label: DefId, captures: Vec<DefId> },
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
    pub fn try_new(
        arena: SpsLowArena, root: CompuId, scheme: SpsScheme,
    ) -> Result<Self, SpsLowValidationError> {
        let facts = SpsLowValidator::validate_with_driver::<Explicit>(&arena.inner, root, scheme)?;
        let mut variables = facts
            .free_variables(root.into())
            .expect("validated syntax")
            .iter()
            .copied()
            .collect::<Vec<_>>();
        variables.sort_unstable();
        if !variables.is_empty() {
            return Err(SpsLowValidationError::OpenRoot { variables });
        }
        drop(facts);
        super::contracts::EntryValidator::validate(&arena.inner, root)?;
        super::protocols::ProtocolValidator::validate(&arena.inner, root)?;
        Ok(Self { arena: FrozenArena::new(arena), root, scheme })
    }

    /// The scheme whose converted fragment contains this program.
    pub fn scheme(&self) -> SpsScheme {
        self.scheme
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
    scheme: SpsScheme,
    arena: &'a SpsLowInnerArena,
    error: Option<SpsLowValidationError>,
    stacks: HashSet<StackId>,
    labels: HashSet<DefId>,
    blocks: Vec<ValueId>,
    /// Closed labels and their bodies, which must read no variable.
    closed_labels: Vec<(DefId, CompuId)>,
    /// The scopes around the current node, innermost last, each with the node that opened it.
    scopes: Vec<(EntityId, LabelScope)>,
    /// Scopes that open when the traversal reaches the keyed node.
    openings: HashMap<EntityId, LabelScope>,
}

/// A region that a label jump resolves through.
#[derive(Clone, Copy, Debug)]
enum LabelScope {
    /// The tail of a label binding.
    Bound(DefId, LabelKind),
    /// The body of a block, which a jump to a label bound outside it leaves the label's frame to reach.
    Block(EntryKind),
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
        if !matches!(arena.stacks[&ambient], Stack::Var(StackVar)) {
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

impl<'a> SpsLowValidator<'a> {
    fn validate_with_driver<D: Driver>(
        arena: &'a SpsLowInnerArena, root: CompuId, scheme: SpsScheme,
    ) -> Result<Variables, SpsLowValidationError> {
        let validator = Self {
            scheme,
            arena,
            error: None,
            stacks: HashSet::new(),
            labels: HashSet::new(),
            blocks: Vec::new(),
            closed_labels: Vec::new(),
            scopes: Vec::new(),
            openings: HashMap::new(),
        };
        let mut analyses = Together { first: validator, second: Variables::default() };
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
                return Err(SpsLowValidationError::ImplicitBlockCapture { label, captures });
            }
        }
        for (label, body) in validator.closed_labels {
            let captures = variables
                .free_variables(body.into())
                .expect("validated label body")
                .iter()
                .copied()
                .collect::<Vec<_>>();
            if !captures.is_empty() {
                return Err(SpsLowValidationError::ImplicitLabelCapture { label, captures });
            }
        }
        for (stack, entry) in &arena.continuations {
            if !validator.stacks.contains(stack)
                || !entry.matches_package(arena, *stack, &variables)
            {
                return Err(SpsLowValidationError::ContinuationContext { stack: *stack });
            }
        }
        Ok(variables)
    }

    fn check(
        &mut self, node: Node<'_>, edge: Edge, occurrence: Occurrence,
    ) -> Result<(), SpsLowValidationError> {
        if occurrence != Occurrence::First {
            return Err(match node {
                | Node::Pattern(pattern, _) => SpsLowValidationError::SharedPattern { pattern },
                | Node::Value(value, _) => SpsLowValidationError::SharedValue { value },
                | Node::Stack(stack, _) => SpsLowValidationError::SharedStack { stack },
                | Node::Computation(compu, _) => SpsLowValidationError::SharedComputation { compu },
            });
        }
        if let Some(scope) = self.openings.remove(&node.id()) {
            self.scopes.push((node.id(), scope));
        }
        match node {
            | Node::Pattern(_, _) => {}
            | Node::Value(id, Value::Block(Block { label, entry, .. })) => {
                if !self.labels.insert(*label) {
                    return Err(SpsLowValidationError::DuplicateBlockLabel { label: *label });
                }
                self.blocks.push(id);
                self.scopes.push((node.id(), LabelScope::Block(entry.kind())));
            }
            | Node::Value(_, _) => {}
            | Node::Stack(id, _) => {
                self.stacks.insert(id);
            }
            | Node::Computation(id, computation) => {
                match computation {
                    | Computation::LetLabel(LetLabel { label, kind, body, tail }) => {
                        if !self.labels.insert(*label) {
                            return Err(SpsLowValidationError::DuplicateLabel { label: *label });
                        }
                        if *kind == LabelKind::Closed {
                            self.closed_labels.push((*label, *body));
                        }
                        self.openings.insert((*tail).into(), LabelScope::Bound(*label, *kind));
                    }
                    | Computation::JumpLabel(JumpLabel { label, .. }) => {
                        self.resolve(id, *label)?;
                    }
                    | _ => {}
                }
                if let Some(error) = self.scheme.check(self.usage(computation, edge)) {
                    return Err(SpsLowValidationError::Placement { compu: id, error });
                }
            }
        }
        Ok(())
    }

    /// A jump reaches a label bound around it. A local label's body reads its binding block's
    /// frame, so its jumps stay in that block; no jump leaves a closure block.
    fn resolve(&self, compu: CompuId, label: DefId) -> Result<(), SpsLowValidationError> {
        let mut crossed = false;
        for (_, scope) in self.scopes.iter().rev() {
            match *scope {
                | LabelScope::Block(EntryKind::Closure) => {
                    return Err(SpsLowValidationError::LabelUnderClosure { compu, label });
                }
                | LabelScope::Block(EntryKind::Continuation) => crossed = true,
                | LabelScope::Bound(bound, kind) if bound == label => {
                    return if crossed && kind == LabelKind::Local {
                        Err(SpsLowValidationError::LocalLabelAcrossBlock { compu, label })
                    } else {
                        Ok(())
                    };
                }
                | LabelScope::Bound(..) => {}
            }
        }
        Err(SpsLowValidationError::UnboundLabel { compu, label })
    }

    fn usage(&self, computation: &Computation, edge: Edge) -> StackUse {
        match computation {
            | Computation::LetStack(LetStack { bindee, tail, .. }) => StackUse::Let {
                bindee: self.shape(*bindee),
                tail_branch: self.arena.compus[tail].is_branch(),
            },
            | computation if computation.is_branch() => {
                StackUse::Branch { guarded: matches!(edge, Edge::StackLetTail(_)) }
            }
            | Computation::Hole(SHole(stack))
            | Computation::ExternCall(ExternCall { stack, .. })
            | Computation::Jump(Jump { stack, .. })
            | Computation::JumpLabel(JumpLabel { stack, .. })
            | Computation::OpenContinuation(OpenContinuation { package: stack, .. })
            | Computation::LetArg(LetArg { bindee: stack, .. })
            | Computation::CoCase(SCoMatch { scrut: stack, .. }) => {
                StackUse::Consumer(self.shape(*stack))
            }
            | _ => StackUse::Other,
        }
    }

    fn shape(&self, stack: StackId) -> FrameShape {
        let ambient = |stack: &StackId| matches!(self.arena.stacks[stack], Stack::Var(_));
        match &self.arena.stacks[&stack] {
            | Stack::Var(_) => FrameShape::Ambient,
            | Stack::Arg(Cons(_, rest)) | Stack::Tag(Cons(_, rest)) if ambient(rest) => {
                FrameShape::Frame
            }
            | Stack::ContinuationPackage(ContinuationPackage { residual, .. }) if matches!(&self.arena.stacks[residual], Stack::Arg(Cons(_, rest)) if ambient(rest)) => {
                FrameShape::Frame
            }
            | Stack::Arg(_) | Stack::Tag(_) | Stack::ContinuationPackage(_) => FrameShape::Compound,
        }
    }
}

impl Visitor for SpsLowValidator<'_> {
    fn enter(&mut self, node: Node<'_>, edge: Edge, occurrence: Occurrence) {
        if self.error.is_none() {
            self.error = self.check(node, edge, occurrence).err();
        }
    }

    fn exit(&mut self, node: Node<'_>) {
        while self.scopes.last().is_some_and(|(opener, _)| *opener == node.id()) {
            self.scopes.pop();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use zydeco_utils::fold::Recursive;

    struct Fixture;

    impl Fixture {
        fn program(
            arena: SpsLowArena, root: CompuId,
        ) -> Result<SpsLowProgram, SpsLowValidationError> {
            let scheme = SpsScheme::BranchJoin;
            let explicit =
                SpsLowValidator::validate_with_driver::<Explicit>(&arena.inner, root, scheme);
            let recursive =
                SpsLowValidator::validate_with_driver::<Recursive>(&arena.inner, root, scheme);
            assert_eq!(explicit.err(), recursive.err());
            SpsLowProgram::try_new(arena, root, scheme)
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
        let stack = StackVar.build(&mut arena, None);
        let root = Jump { argument: Fixture::argument(&mut arena), target: package, stack }
            .build(&mut arena, None);

        assert_eq!(
            Fixture::program(arena, root).unwrap_err(),
            SpsLowValidationError::SharedValue { value: shared }
        );
    }

    #[test]
    fn low_program_rejects_an_implicitly_capturing_block() {
        let mut arena = SpsLowArena::default();
        let label = arena.admin.fresh_def();
        let captured = arena.admin.fresh_def();
        let captured_value: ValueId = captured.build(&mut arena, None);
        let body_stack = StackVar.build(&mut arena, None);
        let body = Jump {
            argument: Fixture::argument(&mut arena),
            target: captured_value,
            stack: body_stack,
        }
        .build(&mut arena, None);
        let block =
            Block { entry: Fixture::entry(&mut arena), label, body }.build(&mut arena, None);
        let root_stack = StackVar.build(&mut arena, None);
        let root =
            Jump { argument: Fixture::argument(&mut arena), target: block, stack: root_stack }
                .build(&mut arena, None);

        assert_eq!(
            Fixture::program(arena, root).unwrap_err(),
            SpsLowValidationError::ImplicitBlockCapture { label, captures: vec![captured] }
        );
    }

    #[test]
    fn low_program_rejects_an_open_root() {
        let mut arena = SpsLowArena::default();
        let free = arena.admin.fresh_def();
        let target: ValueId = free.build(&mut arena, None);
        let stack = StackVar.build(&mut arena, None);
        let root =
            Jump { argument: Fixture::argument(&mut arena), target, stack }.build(&mut arena, None);

        assert_eq!(
            Fixture::program(arena, root).unwrap_err(),
            SpsLowValidationError::OpenRoot { variables: vec![free] }
        );
    }

    #[test]
    fn low_program_rejects_duplicate_block_labels() {
        let mut arena = SpsLowArena::default();
        let label = arena.admin.fresh_def();

        let first_target: ValueId = label.build(&mut arena, None);
        let first_stack = StackVar.build(&mut arena, None);
        let first_body = Jump {
            argument: Fixture::argument(&mut arena),
            target: first_target,
            stack: first_stack,
        }
        .build(&mut arena, None);
        let first = Block { entry: Fixture::entry(&mut arena), label, body: first_body }
            .build(&mut arena, None);

        let second_target: ValueId = label.build(&mut arena, None);
        let second_stack = StackVar.build(&mut arena, None);
        let second_body = Jump {
            argument: Fixture::argument(&mut arena),
            target: second_target,
            stack: second_stack,
        }
        .build(&mut arena, None);
        let second = Block { entry: Fixture::entry(&mut arena), label, body: second_body }
            .build(&mut arena, None);

        let package = ClosurePackage { environment: first, code: second }.build(&mut arena, None);
        let root_stack = StackVar.build(&mut arena, None);
        let root =
            Jump { argument: Fixture::argument(&mut arena), target: package, stack: root_stack }
                .build(&mut arena, None);

        assert_eq!(
            Fixture::program(arena, root).unwrap_err(),
            SpsLowValidationError::DuplicateBlockLabel { label }
        );
    }

    #[test]
    fn branch_join_stack_lets_accept_only_an_immediate_coproduct_body() {
        let mut arena = SpsLowArena::default();
        let scrut = Triv.build(&mut arena, None);
        let branch = SCoprodMatch { scrut, arms: vec![] }.build(&mut arena, None);
        let scheme = SpsScheme::BranchJoin;
        for error in [
            SpsLowValidator::validate_with_driver::<Explicit>(&arena.inner, branch, scheme).err(),
            SpsLowValidator::validate_with_driver::<Recursive>(&arena.inner, branch, scheme).err(),
        ] {
            assert_eq!(
                error,
                Some(SpsLowValidationError::Placement {
                    compu: branch,
                    error: PlacementError::UnguardedBranch,
                })
            );
        }
        let stack = StackVar.build(&mut arena, None);
        let root =
            LetStack { binder: StackVar, bindee: stack, tail: branch }.build(&mut arena, None);
        let program = Fixture::program(arena, root).unwrap();
        let (mut arena, root) = program.into_parts();
        let stack = StackVar.build(&mut arena, None);
        arena.inner.compus[&branch] = SHole(stack).into();
        assert_eq!(
            Fixture::program(arena, root).unwrap_err(),
            SpsLowValidationError::Placement {
                compu: root,
                error: PlacementError::NonBranchStackLet,
            }
        );
    }

    #[test]
    fn duplicating_programs_keep_bare_branches_and_no_stack_lets() {
        let mut arena = SpsLowArena::default();
        let scrut = Triv.build(&mut arena, None);
        let branch = SCoprodMatch { scrut, arms: vec![] }.build(&mut arena, None);
        let scheme = SpsScheme::Duplicating;
        for result in [
            SpsLowValidator::validate_with_driver::<Explicit>(&arena.inner, branch, scheme).err(),
            SpsLowValidator::validate_with_driver::<Recursive>(&arena.inner, branch, scheme).err(),
        ] {
            assert_eq!(result, None);
        }
        let stack = StackVar.build(&mut arena, None);
        let root =
            LetStack { binder: StackVar, bindee: stack, tail: branch }.build(&mut arena, None);
        assert_eq!(
            SpsLowProgram::try_new(arena, root, scheme).unwrap_err(),
            SpsLowValidationError::Placement {
                compu: root,
                error: PlacementError::UnexpectedStackLet,
            }
        );
    }

    #[test]
    fn full_programs_name_continuation_packages_and_jump_with_the_ambient_stack() {
        let full = SpsScheme::Full;
        let mut arena = SpsLowArena::default();
        let label = arena.admin.fresh_def();
        let result = Hole.build(&mut arena, None);
        let body_stack = StackVar.build(&mut arena, None);
        let body = SHole(body_stack).build(&mut arena, None);
        let entry =
            EntryParameters::Continuation { result, environment: Hole.build(&mut arena, None) };
        let code = Block { label, entry, body }.build(&mut arena, None);
        let environment: ValueId = Triv.build(&mut arena, None);
        let ambient = StackVar.build(&mut arena, None);
        let residual = Cons(environment, ambient).build(&mut arena, None);
        let package = ContinuationPackage { code, residual }.build(&mut arena, None);
        let ambient = StackVar.build(&mut arena, None);
        let tail = SHole(ambient).build(&mut arena, None);
        let root = LetStack { binder: StackVar, bindee: package, tail }.build(&mut arena, None);
        assert_eq!(
            SpsLowValidator::validate_with_driver::<Explicit>(&arena.inner, root, full).err(),
            None
        );

        let ambient = StackVar.build(&mut arena, None);
        let value: ValueId = Triv.build(&mut arena, None);
        let stack = Cons(value, ambient).build(&mut arena, None);
        let root = SHole(stack).build(&mut arena, None);
        assert_eq!(
            SpsLowValidator::validate_with_driver::<Explicit>(&arena.inner, root, full).err(),
            Some(SpsLowValidationError::Placement {
                compu: root,
                error: PlacementError::NonAmbientConsumer,
            })
        );
    }

    #[test]
    fn comparisons_require_a_join_and_distinct_successor_occurrences() {
        let mut arena = SpsLowArena::default();
        let operands = [1, 2]
            .map(|value| Literal::Integer(IntegerLiteral::Int(value)).build(&mut arena, None));
        let [when_true, when_false] = [(); 2].map(|()| {
            let stack = StackVar.build(&mut arena, None);
            SHole(stack).build(&mut arena, None)
        });
        let branch = CompareBranch {
            operation: ComparisonOp::Integer(IntegerType::Int, ComparisonPredicate::Lt),
            operands,
            when_true,
            when_false,
        }
        .build(&mut arena, None);
        let scheme = SpsScheme::BranchJoin;
        for error in [
            SpsLowValidator::validate_with_driver::<Explicit>(&arena.inner, branch, scheme).err(),
            SpsLowValidator::validate_with_driver::<Recursive>(&arena.inner, branch, scheme).err(),
        ] {
            assert_eq!(
                error,
                Some(SpsLowValidationError::Placement {
                    compu: branch,
                    error: PlacementError::UnguardedBranch,
                })
            );
        }
        let bindee = StackVar.build(&mut arena, None);
        let root = LetStack { binder: StackVar, bindee, tail: branch }.build(&mut arena, None);
        let program = Fixture::program(arena, root).unwrap();
        let (mut arena, root) = program.into_parts();
        let Computation::Compare(compare) = &mut arena.inner.compus[&branch] else {
            unreachable!()
        };
        compare.when_false = when_true;
        assert_eq!(
            Fixture::program(arena, root).unwrap_err(),
            SpsLowValidationError::SharedComputation { compu: when_true }
        );
    }

    #[test]
    fn cyclic_blocks_are_rejected_before_capture_summaries_are_consumed() {
        let mut arena = SpsLowArena::default();
        let stack = StackVar.build(&mut arena, None);
        let body = SHole(stack).build(&mut arena, None);
        let binder = Hole.build(&mut arena, None);
        let bindee = Triv.build(&mut arena, None);
        arena.inner.compus[&body] = LetValue { binder, bindee, tail: body }.into();
        let label = arena.admin.fresh_def();
        let block =
            Block { label, entry: Fixture::entry(&mut arena), body }.build(&mut arena, None);
        let ambient = StackVar.build(&mut arena, None);
        let stack = Cons(block, ambient).build(&mut arena, None);
        let root = SHole(stack).build(&mut arena, None);
        assert_eq!(
            Fixture::program(arena, root).unwrap_err(),
            SpsLowValidationError::SharedComputation { compu: body }
        );
    }

    #[test]
    fn deep_block_validation_uses_completed_variable_summaries() {
        std::thread::Builder::new()
            .stack_size(512 * 1024)
            .spawn(|| {
                let mut arena = SpsLowArena::default();
                let stack = StackVar.build(&mut arena, None);
                let mut body = SHole(stack).build(&mut arena, None);
                for _ in 0..16_384 {
                    let binder = Hole.build(&mut arena, None);
                    let bindee = Triv.build(&mut arena, None);
                    body = LetValue { binder, bindee, tail: body }.build(&mut arena, None);
                }
                let label = arena.admin.fresh_def();
                let block = Block { label, entry: Fixture::entry(&mut arena), body }
                    .build(&mut arena, None);
                let ambient = StackVar.build(&mut arena, None);
                let stack = Cons(block, ambient).build(&mut arena, None);
                let root = SHole(stack).build(&mut arena, None);
                let variables = SpsLowValidator::validate_with_driver::<Explicit>(
                    &arena.inner,
                    root,
                    SpsScheme::BranchJoin,
                )
                .unwrap();
                assert!(variables.free_variables(root.into()).unwrap().is_empty());
            })
            .unwrap()
            .join()
            .unwrap();
    }

    /// SPSLow programs that bind and jump to one label.
    struct Labels {
        arena: SpsLowArena,
        label: DefId,
    }

    impl Labels {
        fn new() -> Self {
            let mut arena = SpsLowArena::default();
            let label = arena.admin.fresh_def();
            Self { arena, label }
        }

        fn abort(&mut self) -> CompuId {
            let stack = StackVar.build(&mut self.arena, None);
            SHole(stack).build(&mut self.arena, None)
        }

        fn jump(&mut self) -> CompuId {
            let stack = StackVar.build(&mut self.arena, None);
            JumpLabel { label: self.label, stack }.build(&mut self.arena, None)
        }

        fn bind(&mut self, kind: LabelKind, body: CompuId, tail: CompuId) -> CompuId {
            LetLabel { label: self.label, kind, body, tail }.build(&mut self.arena, None)
        }

        /// `let _ = block[k](entry) • -> body in tail`, a block nested in the label's scope.
        fn nest(&mut self, entry: EntryKind, body: CompuId, tail: CompuId) -> CompuId {
            let label = self.arena.admin.fresh_def();
            let entry = match entry {
                | EntryKind::Closure => {
                    EntryParameters::Closure { environment: Hole.build(&mut self.arena, None) }
                }
                | EntryKind::Continuation => EntryParameters::Continuation {
                    result: Hole.build(&mut self.arena, None),
                    environment: Hole.build(&mut self.arena, None),
                },
            };
            let bindee = Block { label, entry, body }.build(&mut self.arena, None);
            let binder = Hole.build(&mut self.arena, None);
            LetValue { binder, bindee, tail }.build(&mut self.arena, None)
        }

        /// A label bound around a block whose body jumps to it, followed by a jump from the
        /// binding block itself.
        fn jump_from_block(mut self, kind: LabelKind, entry: EntryKind) -> (Self, CompuId) {
            let body = self.abort();
            let nested = self.jump();
            let tail = self.jump();
            let tail = self.nest(entry, nested, tail);
            let root = self.bind(kind, body, tail);
            (self, root)
        }

        fn check(self, root: CompuId) -> Result<(), SpsLowValidationError> {
            Fixture::program(self.arena, root).map(drop)
        }
    }

    #[test]
    fn labels_accept_jumps_from_their_block_and_closed_labels_from_continuations() {
        let mut labels = Labels::new();
        let body = labels.abort();
        let tail = labels.jump();
        let root = labels.bind(LabelKind::Local, body, tail);
        assert_eq!(labels.check(root), Ok(()));

        let (labels, root) =
            Labels::new().jump_from_block(LabelKind::Closed, EntryKind::Continuation);
        assert_eq!(labels.check(root), Ok(()));
    }

    #[test]
    fn labels_reject_jumps_that_leave_their_scope_or_frame() {
        let mut labels = Labels::new();
        let root = labels.jump();
        let label = labels.label;
        assert_eq!(
            labels.check(root),
            Err(SpsLowValidationError::UnboundLabel { compu: root, label })
        );

        let (labels, root) =
            Labels::new().jump_from_block(LabelKind::Local, EntryKind::Continuation);
        let label = labels.label;
        assert!(matches!(
            labels.check(root),
            Err(SpsLowValidationError::LocalLabelAcrossBlock { label: rejected, .. }) if rejected == label
        ));

        let (labels, root) = Labels::new().jump_from_block(LabelKind::Closed, EntryKind::Closure);
        let label = labels.label;
        assert!(matches!(
            labels.check(root),
            Err(SpsLowValidationError::LabelUnderClosure { label: rejected, .. }) if rejected == label
        ));
    }

    #[test]
    fn closed_labels_read_no_variable_and_labels_bind_once() {
        // let x = () in closed label p • -> _ ! arg(x) :: • in jump p ! •
        let mut labels = Labels::new();
        let x = labels.arena.admin.fresh_def();
        let read: ValueId = x.build(&mut labels.arena, None);
        let ambient = StackVar.build(&mut labels.arena, None);
        let stack = Cons(read, ambient).build(&mut labels.arena, None);
        let body = SHole(stack).build(&mut labels.arena, None);
        let tail = labels.jump();
        let bound = labels.bind(LabelKind::Closed, body, tail);
        let binder = x.build(&mut labels.arena, None);
        let bindee = Triv.build(&mut labels.arena, None);
        let root = LetValue { binder, bindee, tail: bound }.build(&mut labels.arena, None);
        let label = labels.label;
        assert_eq!(
            labels.check(root),
            Err(SpsLowValidationError::ImplicitLabelCapture { label, captures: vec![x] })
        );

        let mut labels = Labels::new();
        let inner_body = labels.abort();
        let tail = labels.jump();
        let inner = labels.bind(LabelKind::Local, inner_body, tail);
        let outer_body = labels.abort();
        let root = labels.bind(LabelKind::Local, outer_body, inner);
        let label = labels.label;
        assert_eq!(labels.check(root), Err(SpsLowValidationError::DuplicateLabel { label }));
    }
}
