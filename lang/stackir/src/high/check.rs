//! Sanity checks for the stack-passing style ZIR.
//!
//! The closed-root check is intended for debugging. [`BranchJoinProgram`](crate::high::check::BranchJoinProgram)
//! additionally exposes the paper's stack-join placement as a checked boundary.

use super::syntax::*;
use super::traverse::{Edge, Node, Occurrence, Traversal, Visitor};
use super::variables::FreeVars;
use zydeco_statics::{arena::StaticsArena, surface_syntax::ScopedArena};
use zydeco_surface::diagnostic::Diagnostics;

/// A lexical Stack IR tree whose stack joins occur exactly at coproduct and scalar-comparison branches.
#[derive(Debug)]
pub struct BranchJoinProgram {
    program: StackirProgram,
}

#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum BranchJoinError {
    #[error("branch {compu:?} is not immediately guarded by a stack let-binding")]
    UnguardedBranch { compu: CompuId },
    #[error("stack let-binding {compu:?} has body {body:?}, which is not a branch")]
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
    /// Recheck lexical ownership and branch joins without rebuilding the program.
    pub fn validate(&self) -> Result<(), BranchJoinErrors> {
        BranchJoinValidator::validate(&self.program)
    }

    pub fn try_new(program: StackirProgram) -> Result<Self, BranchJoinErrors> {
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
    type Error = BranchJoinErrors;

    fn try_from(program: StackirProgram) -> Result<Self, Self::Error> {
        Self::try_new(program)
    }
}

impl From<BranchJoinProgram> for StackirProgram {
    fn from(program: BranchJoinProgram) -> Self {
        program.into_program()
    }
}

pub type BranchJoinErrors = Diagnostics<BranchJoinError>;

/// Independent observer of ownership and immediate branch-join placement.
#[derive(Default)]
pub struct BranchJoinValidator {
    errors: Vec<BranchJoinError>,
}

impl BranchJoinValidator {
    fn validate(program: &StackirProgram) -> Result<(), BranchJoinErrors> {
        let mut validator = Self::default();
        Traversal { arena: &program.arena().inner }.run(program.root().into(), &mut validator);
        validator.into_result()
    }

    pub fn errors(&self) -> &[BranchJoinError] {
        &self.errors
    }

    pub fn into_result(self) -> Result<(), BranchJoinErrors> {
        match Diagnostics::with_errors(self.errors) {
            | Some(errors) => Err(errors),
            | None => Ok(()),
        }
    }
}

impl Visitor for BranchJoinValidator {
    fn enter(&mut self, node: Node<'_>, edge: Edge, occurrence: Occurrence) {
        if let Edge::BranchJoin(parent) = edge
            && let Node::Computation(body, computation) = node
            && !computation.is_branch()
        {
            self.errors.push(BranchJoinError::NonBranchStackLet { compu: parent, body });
        }
        if occurrence != Occurrence::First {
            self.errors.push(match node {
                | Node::Pattern(pattern, _) => BranchJoinError::SharedPattern { pattern },
                | Node::Value(value, _) => BranchJoinError::SharedValue { value },
                | Node::Stack(stack, _) => BranchJoinError::SharedStack { stack },
                | Node::Computation(compu, _) => BranchJoinError::SharedComputation { compu },
            });
        } else if let Node::Computation(compu, computation) = node
            && computation.is_branch()
            && !matches!(edge, Edge::BranchJoin(_))
        {
            self.errors.push(BranchJoinError::UnguardedBranch { compu });
        }
    }
}

/// Check that the given stack IR arena is well-formed enough for debugging.
///
/// This function panics if the root computation has free variables.
pub fn check(program: &StackirProgram, scoped: &ScopedArena, statics: &StaticsArena) {
    check_closed_root(program, scoped, statics);
}

/// Ensure that the program root is closed (has no free variables).
fn check_closed_root(program: &StackirProgram, scoped: &ScopedArena, statics: &StaticsArena) {
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

        fn comparison(&mut self) -> CompuId {
            let operands = [1, 2].map(|value| {
                Literal::Integer(IntegerLiteral::Int(value)).build(&mut self.arena, None)
            });
            let stack = Bullet.build(&mut self.arena, None);
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
        let bindee = Bullet.build(&mut fixture.arena, None);
        let root = Let { binder: Bullet, bindee, tail: body }.build(&mut fixture.arena, None);

        let program = StackirProgram::new(fixture.arena, root);
        assert!(BranchJoinProgram::try_new(program).is_ok());
    }

    #[test]
    fn branch_join_rejects_an_unguarded_coproduct_match() {
        let mut fixture = Fixture::new();
        let root = fixture.coprod_match();
        let program = StackirProgram::new(fixture.arena, root);

        assert_eq!(
            BranchJoinProgram::try_new(program).unwrap_err(),
            BranchJoinError::UnguardedBranch { compu: root }.into()
        );
    }

    #[test]
    fn comparisons_require_a_join_and_distinct_successor_occurrences() {
        let mut fixture = Fixture::new();
        let branch = fixture.comparison();
        let mut validator = BranchJoinValidator::default();
        Traversal { arena: &fixture.arena.inner }.run(branch.into(), &mut validator);
        assert_eq!(
            validator.into_result().unwrap_err(),
            BranchJoinError::UnguardedBranch { compu: branch }.into()
        );
        let mut arena = fixture.arena;
        let bindee = Bullet.build(&mut arena, None);
        let root = Let { binder: Bullet, bindee, tail: branch }.build(&mut arena, None);
        let mut validator = BranchJoinValidator::default();
        Traversal { arena: &arena.inner }.run(root.into(), &mut validator);
        validator.into_result().unwrap();
        let Computation::Compare(compare) = &mut arena.inner.compus[&branch] else {
            unreachable!()
        };
        compare.when_false = compare.when_true;
        assert_eq!(
            BranchJoinProgram::try_new(StackirProgram::new(arena, root)).unwrap_err(),
            BranchJoinError::SharedComputation { compu: fixture.branch }.into()
        );
    }

    #[test]
    fn branch_join_rejects_a_non_branch_stack_let() {
        let mut fixture = Fixture::new();
        let bindee = Bullet.build(&mut fixture.arena, None);
        let root =
            Let { binder: Bullet, bindee, tail: fixture.branch }.build(&mut fixture.arena, None);
        let program = StackirProgram::new(fixture.arena, root);

        assert_eq!(
            BranchJoinProgram::try_new(program).unwrap_err(),
            BranchJoinError::NonBranchStackLet { compu: root, body: fixture.branch }.into()
        );
    }

    #[test]
    fn branch_join_rejects_implicit_arena_sharing() {
        let mut fixture = Fixture::new();
        let value = Triv.build(&mut fixture.arena, None);
        let pair = VCons::new(vec![value, value], ProductLayout { arity: 2 })
            .build(&mut fixture.arena, None);
        let stack = Bullet.build(&mut fixture.arena, None);
        let root = SReturn { stack, value: pair }.build(&mut fixture.arena, None);
        let program = StackirProgram::new(fixture.arena, root);

        assert_eq!(
            BranchJoinProgram::try_new(program).unwrap_err(),
            BranchJoinError::SharedValue { value }.into()
        );
    }
}
