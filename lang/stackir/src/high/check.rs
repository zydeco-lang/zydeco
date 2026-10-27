//! Sanity checks for the stack-passing style ZIR.
//!
//! The closed-root check is intended for debugging. [`FragmentProgram`](crate::high::check::FragmentProgram)
//! additionally exposes the compiling scheme's stack placement as a checked boundary.

use super::syntax::*;
use super::traverse::{Edge, Node, Occurrence, Traversal, Visitor};
use super::variables::FreeVars;
use crate::scheme::{PlacementError, SpsScheme, StackUse};
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

/// Independent observer of ownership and of stack placement in one scheme's fragment.
pub struct FragmentValidator<'a> {
    scheme: SpsScheme,
    arena: &'a SpsInnerArena,
    errors: Vec<FragmentError>,
}

impl<'a> FragmentValidator<'a> {
    pub fn new(scheme: SpsScheme, arena: &'a SpsInnerArena) -> Self {
        Self { scheme, arena, errors: Vec::new() }
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
            | Computation::Join(LetJoin::Stack(Let { tail, .. })) => {
                StackUse::Let { tail_branch: self.arena.compus[tail].is_branch() }
            }
            | computation if computation.is_branch() => {
                StackUse::Branch { guarded: matches!(edge, Edge::StackLetTail(_)) }
            }
            | _ => StackUse::Other,
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
        } else if let Node::Computation(compu, computation) = node
            && let Some(error) = self.scheme.check(self.usage(computation, edge))
        {
            self.errors.push(FragmentError::Placement { compu, error });
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
