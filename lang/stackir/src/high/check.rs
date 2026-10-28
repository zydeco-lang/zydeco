//! Sanity checks for the stack-passing style ZIR.
//!
//! The closed-root check is intended for debugging. [`FragmentProgram`](crate::high::check::FragmentProgram)
//! additionally exposes the compiling scheme's stack placement as a checked boundary.

use super::syntax::*;
use super::traverse::{Edge, EntityId, Node, Occurrence, Traversal, Visitor};
use super::variables::FreeVars;
use crate::scheme::{FrameShape, PlacementError, SpsScheme, StackUse};
use std::collections::HashMap;
use zydeco_statics::{arena::StaticsArena, surface_syntax::ScopedArena};
use zydeco_surface::diagnostic::Diagnostics;

/// A lexical Stack IR tree that lies in the fragment of the scheme that compiled it.
#[derive(Debug)]
pub struct FragmentProgram {
    program: SpsProgram,
    scheme: SpsScheme,
}

#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum FragmentError {
    #[error("computation {compu:?} leaves its scheme's fragment: {error}")]
    Placement { compu: CompuId, error: PlacementError },
    #[error("computation node {compu:?} occurs more than once in lexical Stack IR")]
    SharedComputation { compu: CompuId },
    #[error("stack node {stack:?} occurs more than once in lexical Stack IR")]
    SharedStack { stack: StackId },
    #[error("value node {value:?} occurs more than once in lexical Stack IR")]
    SharedValue { value: ValueId },
    #[error("pattern node {pattern:?} occurs more than once in lexical Stack IR")]
    SharedPattern { pattern: VPatId },
    #[error("computation {compu:?} calls procedure {procedure:?} outside the tail that binds it")]
    ProcedureOutOfScope { compu: CompuId, procedure: DefId },
    #[error(
        "computation {compu:?} calls procedure {procedure:?} under a closure or fixed point, which hides the procedures bound outside it"
    )]
    ProcedureUnderClosure { compu: CompuId, procedure: DefId },
    #[error("value {value:?} names procedure {procedure:?}, but a procedure is not a value")]
    ProcedureAsValue { value: ValueId, procedure: DefId },
}

impl FragmentProgram {
    /// Recheck lexical ownership and stack placement without rebuilding the program.
    pub fn validate(&self) -> Result<(), FragmentErrors> {
        FragmentValidator::validate(&self.program, self.scheme)
    }

    pub fn try_new(program: SpsProgram, scheme: SpsScheme) -> Result<Self, FragmentErrors> {
        FragmentValidator::validate(&program, scheme)?;
        Ok(Self { program, scheme })
    }

    /// The scheme whose fragment contains this program.
    pub fn scheme(&self) -> SpsScheme {
        self.scheme
    }

    pub fn as_program(&self) -> &SpsProgram {
        &self.program
    }

    pub fn into_program(self) -> SpsProgram {
        self.program
    }
}

impl AsRef<SpsProgram> for FragmentProgram {
    fn as_ref(&self) -> &SpsProgram {
        self.as_program()
    }
}

impl From<FragmentProgram> for SpsProgram {
    fn from(program: FragmentProgram) -> Self {
        program.into_program()
    }
}

pub type FragmentErrors = Diagnostics<FragmentError>;

/// Independent observer of ownership, stack placement in one scheme's fragment, and procedure scope.
pub struct FragmentValidator<'a> {
    scheme: SpsScheme,
    arena: &'a SpsInnerArena,
    errors: Vec<FragmentError>,
    /// The procedure scopes around the current node, innermost last, each with the node that opened it.
    scopes: Vec<(EntityId, ProcedureScope)>,
    /// Scopes that open when the traversal reaches the keyed node.
    openings: HashMap<EntityId, ProcedureScope>,
}

/// A region where procedure calls are resolved.
#[derive(Clone, Copy, Debug)]
enum ProcedureScope {
    /// The tail of a procedure binding, where the procedure can be called.
    Bound(DefId),
    /// A closure or fixed-point body, which runs at a later call and hides every procedure bound outside it.
    Closed,
}

impl<'a> FragmentValidator<'a> {
    pub fn new(scheme: SpsScheme, arena: &'a SpsInnerArena) -> Self {
        Self { scheme, arena, errors: Vec::new(), scopes: Vec::new(), openings: HashMap::new() }
    }

    fn validate(program: &SpsProgram, scheme: SpsScheme) -> Result<(), FragmentErrors> {
        let arena = &program.arena().inner;
        let mut validator = FragmentValidator::new(scheme, arena);
        Traversal { arena }.run(program.root().into(), &mut validator);
        validator.into_result()
    }

    pub fn errors(&self) -> &[FragmentError] {
        &self.errors
    }

    pub fn into_result(self) -> Result<(), FragmentErrors> {
        match Diagnostics::with_errors(self.errors) {
            | Some(errors) => Err(errors),
            | None => Ok(()),
        }
    }

    fn usage(&self, computation: &Computation<LetJoin>, edge: Edge) -> StackUse {
        match computation {
            | Computation::Join(LetJoin::Stack(Let { bindee, tail, .. })) => StackUse::Let {
                bindee: self.shape(*bindee),
                tail_branch: self.arena.compus[tail].is_branch(),
            },
            | computation if computation.is_branch() => {
                StackUse::Branch { guarded: matches!(edge, Edge::StackLetTail(_)) }
            }
            | Computation::Hole(SHole(stack))
            | Computation::ExternCall(ExternCall { stack, .. })
            | Computation::Force(SForce { stack, .. })
            | Computation::CallProc(CallProc { stack, .. })
            | Computation::Ret(SReturn { stack, .. })
            | Computation::Fix(SFix { stack, .. })
            | Computation::LetArg(Let { bindee: stack, .. })
            | Computation::CoCase(SCoMatch { scrut: stack, .. }) => {
                StackUse::Consumer(self.shape(*stack))
            }
            | _ => StackUse::Other,
        }
    }

    fn shape(&self, stack: StackId) -> FrameShape {
        match &self.arena.stacks[&stack] {
            | Stack::Var(_) => FrameShape::Ambient,
            | Stack::Kont(_) => FrameShape::Frame,
            | Stack::Arg(Cons(_, rest)) | Stack::Tag(Cons(_, rest))
                if matches!(self.arena.stacks[rest], Stack::Var(_)) =>
            {
                FrameShape::Frame
            }
            | Stack::Arg(_) | Stack::Tag(_) => FrameShape::Compound,
        }
    }

    /// Open the scopes that start at `node` and check the procedure names it uses.
    fn resolve(&mut self, node: Node<'_>) -> Option<FragmentError> {
        if let Some(scope) = self.openings.remove(&node.id()) {
            self.scopes.push((node.id(), scope));
        }
        match node {
            | Node::Value(_, Value::Closure(_)) => {
                self.scopes.push((node.id(), ProcedureScope::Closed));
                None
            }
            | Node::Value(value, Value::Var(def)) => self
                .scopes
                .iter()
                .any(|(_, scope)| matches!(scope, ProcedureScope::Bound(bound) if bound == def))
                .then_some(FragmentError::ProcedureAsValue { value, procedure: *def }),
            | Node::Computation(_, Computation::LetProc(LetProc { procedure, tail, .. })) => {
                self.openings.insert((*tail).into(), ProcedureScope::Bound(*procedure));
                None
            }
            | Node::Computation(_, Computation::Fix(SFix { body, .. })) => {
                self.openings.insert((*body).into(), ProcedureScope::Closed);
                None
            }
            | Node::Computation(compu, Computation::CallProc(CallProc { procedure, .. })) => {
                let mut closed = false;
                for (_, scope) in self.scopes.iter().rev() {
                    match scope {
                        | ProcedureScope::Closed => closed = true,
                        | ProcedureScope::Bound(bound) if bound == procedure => {
                            return closed.then_some(FragmentError::ProcedureUnderClosure {
                                compu,
                                procedure: *procedure,
                            });
                        }
                        | ProcedureScope::Bound(_) => {}
                    }
                }
                Some(FragmentError::ProcedureOutOfScope { compu, procedure: *procedure })
            }
            | _ => None,
        }
    }
}

impl Visitor for FragmentValidator<'_> {
    fn enter(&mut self, node: Node<'_>, edge: Edge, occurrence: Occurrence) {
        if occurrence != Occurrence::First {
            self.errors.push(match node {
                | Node::Pattern(pattern, _) => FragmentError::SharedPattern { pattern },
                | Node::Value(value, _) => FragmentError::SharedValue { value },
                | Node::Stack(stack, _) => FragmentError::SharedStack { stack },
                | Node::Computation(compu, _) => FragmentError::SharedComputation { compu },
            });
            return;
        }
        if let Some(error) = self.resolve(node) {
            self.errors.push(error);
        }
        if let Node::Computation(compu, computation) = node
            && let Some(error) = self.scheme.check(self.usage(computation, edge))
        {
            self.errors.push(FragmentError::Placement { compu, error });
        }
    }

    fn exit(&mut self, node: Node<'_>) {
        while self.scopes.last().is_some_and(|(opener, _)| *opener == node.id()) {
            self.scopes.pop();
        }
    }
}

/// Check that the given stack IR arena is well-formed enough for debugging.
///
/// This function panics if the root computation has free variables.
pub fn check(program: &SpsProgram, scoped: &ScopedArena, statics: &StaticsArena) {
    check_closed_root(program, scoped, statics);
}

/// Ensure that the program root is closed (has no free variables).
fn check_closed_root(program: &SpsProgram, scoped: &ScopedArena, statics: &StaticsArena) {
    let fv = program.root().free_vars(program.arena());
    let fv_str = fv
        .iter()
        .map(|def| {
            let name = program.arena().admin.def_name(scoped, statics, def);
            format!("{}{}", name.plain(), def.concise())
        })
        .collect::<Vec<_>>()
        .join(", ");
    if !fv.is_empty() {
        panic!("stack IR root {:?} is not closed; free variables: {}", program.root(), fv_str);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Fixture {
        arena: SpsArena,
        scrut: ValueId,
        branch: CompuId,
    }

    impl Fixture {
        fn new() -> Self {
            let mut arena = SpsArena::default();
            let scrut = Triv.build(&mut arena, None);
            let stack = StackVar.build(&mut arena, None);
            let value = Triv.build(&mut arena, None);
            let branch = SReturn { stack, value }.build(&mut arena, None);
            Self { arena, scrut, branch }
        }

        fn coprod_match(&mut self) -> CompuId {
            let binder = Triv.build(&mut self.arena, None);
            SCoprodMatch { scrut: self.scrut, arms: vec![Matcher { binder, tail: self.branch }] }
                .build(&mut self.arena, None)
        }

        fn comparison(&mut self) -> CompuId {
            let operands = [1, 2].map(|value| {
                Literal::Integer(IntegerLiteral::Int(value)).build(&mut self.arena, None)
            });
            let stack = StackVar.build(&mut self.arena, None);
            let value = Triv.build(&mut self.arena, None);
            let when_false = SReturn { stack, value }.build(&mut self.arena, None);
            CompareBranch {
                operation: ComparisonOp::Integer(IntegerType::Int, ComparisonPredicate::Lt),
                operands,
                when_true: self.branch,
                when_false,
            }
            .build(&mut self.arena, None)
        }
    }

    #[test]
    fn branch_join_accepts_a_guarded_coproduct_match() {
        let mut fixture = Fixture::new();
        let body = fixture.coprod_match();
        let bindee = StackVar.build(&mut fixture.arena, None);
        let root = Let { binder: StackVar, bindee, tail: body }.build(&mut fixture.arena, None);

        let program = SpsProgram::new(fixture.arena, root);
        assert!(FragmentProgram::try_new(program, SpsScheme::BranchJoin).is_ok());
    }

    #[test]
    fn branch_join_rejects_an_unguarded_coproduct_match() {
        let mut fixture = Fixture::new();
        let root = fixture.coprod_match();
        let program = SpsProgram::new(fixture.arena, root);

        assert_eq!(
            FragmentProgram::try_new(program, SpsScheme::BranchJoin).unwrap_err(),
            FragmentError::Placement { compu: root, error: PlacementError::UnguardedBranch }.into()
        );
    }

    #[test]
    fn comparisons_require_a_join_and_distinct_successor_occurrences() {
        let mut fixture = Fixture::new();
        let branch = fixture.comparison();
        let mut validator = FragmentValidator::new(SpsScheme::BranchJoin, &fixture.arena.inner);
        Traversal { arena: &fixture.arena.inner }.run(branch.into(), &mut validator);
        assert_eq!(
            validator.into_result().unwrap_err(),
            FragmentError::Placement { compu: branch, error: PlacementError::UnguardedBranch }
                .into()
        );
        let mut arena = fixture.arena;
        let bindee = StackVar.build(&mut arena, None);
        let root = Let { binder: StackVar, bindee, tail: branch }.build(&mut arena, None);
        let mut validator = FragmentValidator::new(SpsScheme::BranchJoin, &arena.inner);
        Traversal { arena: &arena.inner }.run(root.into(), &mut validator);
        validator.into_result().unwrap();
        let Computation::Compare(compare) = &mut arena.inner.compus[&branch] else {
            unreachable!()
        };
        compare.when_false = compare.when_true;
        assert_eq!(
            FragmentProgram::try_new(SpsProgram::new(arena, root), SpsScheme::BranchJoin)
                .unwrap_err(),
            FragmentError::SharedComputation { compu: fixture.branch }.into()
        );
    }

    #[test]
    fn branch_join_rejects_a_non_branch_stack_let() {
        let mut fixture = Fixture::new();
        let bindee = StackVar.build(&mut fixture.arena, None);
        let root =
            Let { binder: StackVar, bindee, tail: fixture.branch }.build(&mut fixture.arena, None);
        let program = SpsProgram::new(fixture.arena, root);

        assert_eq!(
            FragmentProgram::try_new(program, SpsScheme::BranchJoin).unwrap_err(),
            FragmentError::Placement { compu: root, error: PlacementError::NonBranchStackLet }
                .into()
        );
    }

    #[test]
    fn duplicating_accepts_a_bare_branch_and_rejects_any_stack_let() {
        let mut fixture = Fixture::new();
        let root = fixture.coprod_match();
        let program = SpsProgram::new(fixture.arena, root);
        assert!(FragmentProgram::try_new(program, SpsScheme::Duplicating).is_ok());

        let mut fixture = Fixture::new();
        let body = fixture.coprod_match();
        let bindee = StackVar.build(&mut fixture.arena, None);
        let root = Let { binder: StackVar, bindee, tail: body }.build(&mut fixture.arena, None);
        let program = SpsProgram::new(fixture.arena, root);
        assert_eq!(
            FragmentProgram::try_new(program, SpsScheme::Duplicating).unwrap_err(),
            FragmentError::Placement { compu: root, error: PlacementError::UnexpectedStackLet }
                .into()
        );
    }

    #[test]
    fn full_accepts_single_frame_lets_and_rejects_other_stack_shapes() {
        let full = SpsScheme::Full;
        let program = |arena: SpsArena, root| {
            FragmentProgram::try_new(SpsProgram::new(arena, root), full)
                .map(drop)
                .map_err(|errors| errors.into_iter().next().unwrap())
        };
        let consumer = |arena: &mut SpsArena, stack: StackId| -> CompuId {
            let thunk: ValueId = Triv.build(arena, None);
            SForce { thunk, stack }.build(arena, None)
        };
        let frame = |arena: &mut SpsArena, rest: StackId| -> StackId {
            let value: ValueId = Triv.build(arena, None);
            Cons(value, rest).build(arena, None)
        };

        let mut arena = SpsArena::default();
        let ambient = StackVar.build(&mut arena, None);
        let tail = consumer(&mut arena, ambient);
        let ambient = StackVar.build(&mut arena, None);
        let bindee = frame(&mut arena, ambient);
        let root = Let { binder: StackVar, bindee, tail }.build(&mut arena, None);
        assert_eq!(program(arena, root), Ok(()));

        let mut arena = SpsArena::default();
        let ambient = StackVar.build(&mut arena, None);
        let tail = consumer(&mut arena, ambient);
        let bindee = StackVar.build(&mut arena, None);
        let root = Let { binder: StackVar, bindee, tail }.build(&mut arena, None);
        let error = PlacementError::IdentityStackLet;
        assert_eq!(program(arena, root), Err(FragmentError::Placement { compu: root, error }));

        let mut arena = SpsArena::default();
        let ambient = StackVar.build(&mut arena, None);
        let tail = consumer(&mut arena, ambient);
        let ambient = StackVar.build(&mut arena, None);
        let deeper = frame(&mut arena, ambient);
        let bindee = frame(&mut arena, deeper);
        let root = Let { binder: StackVar, bindee, tail }.build(&mut arena, None);
        let error = PlacementError::CompoundStackLet;
        assert_eq!(program(arena, root), Err(FragmentError::Placement { compu: root, error }));

        let mut arena = SpsArena::default();
        let ambient = StackVar.build(&mut arena, None);
        let stack = frame(&mut arena, ambient);
        let root = consumer(&mut arena, stack);
        let error = PlacementError::NonAmbientConsumer;
        assert_eq!(program(arena, root), Err(FragmentError::Placement { compu: root, error }));
    }

    /// One procedure and the computations that bind and call it.
    struct Procedures {
        arena: SpsArena,
        procedure: DefId,
    }

    impl Procedures {
        fn new() -> Self {
            let mut arena = SpsArena::default();
            let procedure = arena.admin.fresh();
            Self { arena, procedure }
        }

        fn call_with(&mut self, stack: StackId) -> CompuId {
            CallProc { procedure: self.procedure, stack }.build(&mut self.arena, None)
        }

        fn call(&mut self) -> CompuId {
            let stack = StackVar.build(&mut self.arena, None);
            self.call_with(stack)
        }

        fn returns_unit(&mut self) -> CompuId {
            let stack = StackVar.build(&mut self.arena, None);
            let value = Triv.build(&mut self.arena, None);
            SReturn { stack, value }.build(&mut self.arena, None)
        }

        fn bind(&mut self, body: CompuId, tail: CompuId) -> CompuId {
            LetProc { procedure: self.procedure, stack: StackVar, body, tail }
                .build(&mut self.arena, None)
        }

        fn check(self, root: CompuId, scheme: SpsScheme) -> Result<(), FragmentError> {
            FragmentProgram::try_new(SpsProgram::new(self.arena, root), scheme)
                .map(drop)
                .map_err(|errors| errors.into_iter().next().unwrap())
        }
    }

    #[test]
    fn procedures_are_called_in_their_tail_including_under_continuations() {
        let mut procedures = Procedures::new();
        let resumed = procedures.call();
        let binder: VPatId = Triv.build(&mut procedures.arena, None);
        let kont = Kont { binder, body: resumed }.build(&mut procedures.arena, None);
        let tail = procedures.call_with(kont);
        let body = procedures.returns_unit();
        let root = procedures.bind(body, tail);
        assert_eq!(procedures.check(root, SpsScheme::BranchJoin), Ok(()));
    }

    #[test]
    fn procedures_reject_calls_outside_their_tail() {
        let mut procedures = Procedures::new();
        let recursive = procedures.call();
        let tail = procedures.call();
        let root = procedures.bind(recursive, tail);
        let procedure = procedures.procedure;
        let error = FragmentError::ProcedureOutOfScope { compu: recursive, procedure };
        assert_eq!(procedures.check(root, SpsScheme::BranchJoin), Err(error));

        let mut procedures = Procedures::new();
        let root = procedures.call();
        let procedure = procedures.procedure;
        let error = FragmentError::ProcedureOutOfScope { compu: root, procedure };
        assert_eq!(procedures.check(root, SpsScheme::BranchJoin), Err(error));
    }

    #[test]
    fn procedures_reject_calls_under_closures_and_fixed_points() {
        let mut procedures = Procedures::new();
        let call = procedures.call();
        let value = Closure { stack: StackVar, body: call }.build(&mut procedures.arena, None);
        let stack = StackVar.build(&mut procedures.arena, None);
        let tail = SReturn { stack, value }.build(&mut procedures.arena, None);
        let body = procedures.returns_unit();
        let root = procedures.bind(body, tail);
        let procedure = procedures.procedure;
        let error = FragmentError::ProcedureUnderClosure { compu: call, procedure };
        assert_eq!(procedures.check(root, SpsScheme::BranchJoin), Err(error));

        let mut procedures = Procedures::new();
        let call = procedures.call();
        let param = procedures.arena.admin.fresh();
        let stack = StackVar.build(&mut procedures.arena, None);
        let tail = SFix { param, stack, body: call }.build(&mut procedures.arena, None);
        let body = procedures.returns_unit();
        let root = procedures.bind(body, tail);
        let procedure = procedures.procedure;
        let error = FragmentError::ProcedureUnderClosure { compu: call, procedure };
        assert_eq!(procedures.check(root, SpsScheme::BranchJoin), Err(error));
    }

    #[test]
    fn procedures_are_not_values() {
        let mut procedures = Procedures::new();
        let value = procedures.procedure.build(&mut procedures.arena, None);
        let stack = StackVar.build(&mut procedures.arena, None);
        let tail = SReturn { stack, value }.build(&mut procedures.arena, None);
        let body = procedures.returns_unit();
        let root = procedures.bind(body, tail);
        let procedure = procedures.procedure;
        let error = FragmentError::ProcedureAsValue { value, procedure };
        assert_eq!(procedures.check(root, SpsScheme::BranchJoin), Err(error));
    }

    #[test]
    fn full_requires_a_procedure_call_to_receive_the_ambient_stack() {
        let mut procedures = Procedures::new();
        let tail = procedures.call();
        let body = procedures.returns_unit();
        let root = procedures.bind(body, tail);
        assert_eq!(procedures.check(root, SpsScheme::Full), Ok(()));

        let mut procedures = Procedures::new();
        let argument: ValueId = Triv.build(&mut procedures.arena, None);
        let ambient: StackId = StackVar.build(&mut procedures.arena, None);
        let stack = Cons(argument, ambient).build(&mut procedures.arena, None);
        let tail = procedures.call_with(stack);
        let body = procedures.returns_unit();
        let root = procedures.bind(body, tail);
        let error = PlacementError::NonAmbientConsumer;
        assert_eq!(
            procedures.check(root, SpsScheme::Full),
            Err(FragmentError::Placement { compu: tail, error })
        );
    }

    #[test]
    fn branch_join_rejects_implicit_arena_sharing() {
        let mut fixture = Fixture::new();
        let value = Triv.build(&mut fixture.arena, None);
        let pair = VCons::new(vec![value, value], ProductLayout { arity: 2 })
            .build(&mut fixture.arena, None);
        let stack = StackVar.build(&mut fixture.arena, None);
        let root = SReturn { stack, value: pair }.build(&mut fixture.arena, None);
        let program = SpsProgram::new(fixture.arena, root);

        assert_eq!(
            FragmentProgram::try_new(program, SpsScheme::BranchJoin).unwrap_err(),
            FragmentError::SharedValue { value }.into()
        );
    }
}
