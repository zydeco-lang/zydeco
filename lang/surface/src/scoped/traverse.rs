//! Structural traversal of resolved syntax with independent analysis visitors.

use super::syntax::*;
use std::{collections::HashMap, ops::ControlFlow};

/// One borrowed syntax node. Definition IDs are leaves, not edges to defining terms.
#[derive(Clone, Copy, Debug)]
pub enum Node<'arena> {
    Definition(DefId),
    Pattern(PatId, &'arena Pattern),
    Term(TermId, &'arena Term<DefId>),
}

impl Node<'_> {
    pub fn id(self) -> EntityId {
        match self {
            | Self::Definition(id) => id.into(),
            | Self::Pattern(id, _) => id.into(),
            | Self::Term(id, _) => id.into(),
        }
    }

    /// Enumerate structural edges in the scoped analysis order.
    fn children(self, mut child: impl FnMut(EntityId)) {
        match self {
            | Self::Definition(_) => {}
            | Self::Pattern(_, pattern) => match pattern {
                | Pattern::Ann(Ann { tm, ty }) => {
                    child((*tm).into());
                    child((*ty).into());
                }
                | Pattern::Hole(_) | Pattern::Lit(_) | Pattern::Triv(_) => {}
                | Pattern::Var(def) => child((*def).into()),
                | Pattern::Named(Named(_, inner))
                | Pattern::Ctor(Ctor(_, inner))
                | Pattern::Project(ProjectionPattern(_, inner)) => child((*inner).into()),
                | Pattern::View(ViewPattern { function, pattern }) => {
                    child((*function).into());
                    child((*pattern).into());
                }
                | Pattern::Alias(Alias(patterns)) => {
                    patterns.iter().for_each(|pattern| child((*pattern).into()));
                }
                | Pattern::Cons(patterns) => {
                    patterns.iter().for_each(|pattern| child((*pattern).into()));
                }
            },
            | Self::Term(_, term) => match term {
                | Term::Meta(meta) => child(meta.1.into()),
                | Term::TypeOf(TypeOf(inner))
                | Term::SourceBoundary(SourceBoundary(inner))
                | Term::SignatureBoundary(SignatureBoundary(inner))
                | Term::Sealed(Sealed(inner))
                | Term::Named(Named(_, inner))
                | Term::Label(Label(_, inner))
                | Term::Thunk(Thunk(inner))
                | Term::Force(Force(inner))
                | Term::Ret(Return(inner))
                | Term::Residual(Residual(inner))
                | Term::Block(Block(inner))
                | Term::Ctor(Ctor(_, inner))
                | Term::Dtor(Dtor(inner, _))
                | Term::Proj(Proj(inner, _)) => child((*inner).into()),
                | Term::Internal(_) | Term::Hole(_) | Term::Triv(_) | Term::Lit(_) => {}
                | Term::Var(def) => child((*def).into()),
                | Term::Ann(Ann { tm, ty }) => {
                    child((*tm).into());
                    child((*ty).into());
                }
                | Term::Cons(terms) => terms.iter().for_each(|term| child((*term).into())),
                | Term::Abs(Abs(pattern, body))
                | Term::ValAbs(Abs(pattern, body))
                | Term::Fix(Fix(pattern, body))
                | Term::Pi(Pi(pattern, body))
                | Term::ValPi(ValPi(pattern, body))
                | Term::Sigma(Sigma(pattern, body)) => {
                    child((*pattern).into());
                    child((*body).into());
                }
                | Term::App(App(function, argument)) => {
                    child((*function).into());
                    child((*argument).into());
                }
                | Term::ManifestExists(inner) => {
                    let ManifestExists { binder, definition, body } = &**inner;
                    child((*definition).into());
                    child((*binder).into());
                    child((*body).into());
                }
                | Term::Pack(inner) => {
                    let Pack { mode: _, binder, definition, body } = &**inner;
                    child((*definition).into());
                    child((*binder).into());
                    child((*body).into());
                }
                | Term::Do(inner) => {
                    let Bind { binder, bindee, tail } = &**inner;
                    child((*bindee).into());
                    child((*binder).into());
                    child((*tail).into());
                }
                | Term::Let(inner) => {
                    let Let { binder, bindee, tail } = &**inner;
                    child((*bindee).into());
                    child((*binder).into());
                    child((*tail).into());
                }
                | Term::MobileParam(_) | Term::MobileBind(_) => {
                    unreachable!("mobile syntax must be eliminated during name resolution")
                }
                | Term::RecGroup(RecGroup { definitions, tail }) => {
                    definitions.iter().for_each(|RecursiveDefinition { binder, bindee }| {
                        child((*binder).into());
                        child((*bindee).into());
                    });
                    child((*tail).into());
                }
                | Term::MoBlock(inner) => {
                    let MoBlock { body, basis } = &**inner;
                    child(basis.monad.into());
                    child(basis.algebra.into());
                    child((*body).into());
                }
                | Term::Data(Data { arms }) => {
                    arms.iter().for_each(|DataArm { name: _, param }| child((*param).into()));
                }
                | Term::CoData(CoData { arms }) => {
                    arms.iter().for_each(|CoDataArm { name: _, out }| child((*out).into()));
                }
                | Term::Match(Match { scrut, arms }) => {
                    child((*scrut).into());
                    arms.iter().for_each(|Matcher { binder, tail }| {
                        child((*binder).into());
                        child((*tail).into());
                    });
                }
                | Term::CoMatchClauses(CoMatchClauses { clauses }) => {
                    clauses.iter().for_each(|CoPatternClause { spine, tail }| {
                        spine.iter().for_each(|item| {
                            if let CoPatternItem::Pat(pattern) = item {
                                child((*pattern).into());
                            }
                        });
                        child((*tail).into());
                    });
                }
                | Term::CoMatch(CoMatch { arms }) => {
                    arms.iter().for_each(|CoMatcher { dtor: _, tail }| child((*tail).into()));
                }
            },
        }
    }
}

/// Analysis callbacks; the traversal driver alone schedules children.
/// A break stops immediately, leaving visitor state at the completed prefix.
#[auto_impl::auto_impl(&mut, Box)]
pub trait Visitor {
    type Break;

    fn enter(&mut self, _node: Node<'_>) -> ControlFlow<Self::Break> {
        ControlFlow::Continue(())
    }

    fn exit(&mut self, _node: Node<'_>) -> ControlFlow<Self::Break> {
        ControlFlow::Continue(())
    }
}

/// Independent visitors receiving the same events, first then second.
/// Both must use the enclosing traversal's sharing and boundary policy.
pub struct Together<A, B> {
    pub first: A,
    pub second: B,
}

impl<A, B> Visitor for Together<A, B>
where
    A: Visitor,
    B: Visitor<Break = A::Break>,
{
    type Break = A::Break;

    fn enter(&mut self, node: Node<'_>) -> ControlFlow<Self::Break> {
        self.first.enter(node)?;
        self.second.enter(node)
    }

    fn exit(&mut self, node: Node<'_>) -> ControlFlow<Self::Break> {
        self.first.exit(node)?;
        self.second.exit(node)
    }
}

/// Whether repeated edges revisit an already completed node.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Sharing {
    /// Summarize each entity once, including shared source roots.
    #[default]
    UniqueNodes,
    /// Visit every occurrence, for analyses depending on the path to a node.
    Occurrences,
}

/// An edge returns to a node whose children are still being visited.
#[derive(Clone, Copy, Debug, PartialEq, Eq, thiserror::Error)]
#[error("cycle in scoped syntax at {node:?}")]
pub struct TraversalCycle {
    pub node: EntityId,
}

/// A traversal configuration over an immutable resolved arena.
/// Each run has fresh visit state and follows structural source/signature boundaries.
pub struct Traversal<'arena> {
    arena: &'arena ScopedArena,
    sharing: Sharing,
}

#[derive(Clone, Copy)]
enum VisitState {
    Active,
    Complete,
}

enum Step<'arena> {
    Enter(EntityId),
    Exit(Node<'arena>),
}

impl<'arena> Traversal<'arena> {
    pub fn new(arena: &'arena ScopedArena) -> Self {
        Self { arena, sharing: Sharing::UniqueNodes }
    }

    pub fn with_sharing(mut self, sharing: Sharing) -> Self {
        self.sharing = sharing;
        self
    }

    /// Visit one root in depth-first order without using the Rust call stack.
    /// Exit callbacks see completed children. A break has no balancing exits or rollback.
    pub fn run<V: Visitor>(
        &self, root: EntityId, visitor: &mut V,
    ) -> Result<ControlFlow<V::Break>, TraversalCycle> {
        let mut states = HashMap::new();
        let mut pending = vec![Step::Enter(root)];
        while let Some(step) = pending.pop() {
            let result = match step {
                | Step::Enter(id) => {
                    match states.get(&id) {
                        | Some(VisitState::Active) => return Err(TraversalCycle { node: id }),
                        | Some(VisitState::Complete) => continue,
                        | None => {}
                    }
                    states.insert(id, VisitState::Active);
                    let node = match id {
                        | EntityId::Def(id) => Node::Definition(id),
                        | EntityId::Pat(id) => Node::Pattern(id, &self.arena.pats[&id]),
                        | EntityId::Term(id) => Node::Term(id, &self.arena.terms[&id]),
                    };
                    if let ControlFlow::Break(value) = visitor.enter(node) {
                        return Ok(ControlFlow::Break(value));
                    }
                    pending.push(Step::Exit(node));
                    let start = pending.len();
                    node.children(|child| pending.push(Step::Enter(child)));
                    pending[start..].reverse();
                    ControlFlow::Continue(())
                }
                | Step::Exit(node) => {
                    match self.sharing {
                        | Sharing::UniqueNodes => {
                            states.insert(node.id(), VisitState::Complete);
                        }
                        | Sharing::Occurrences => {
                            states.remove(&node.id());
                        }
                    }
                    visitor.exit(node)
                }
            };
            if let ControlFlow::Break(value) = result {
                return Ok(ControlFlow::Break(value));
            }
        }
        Ok(ControlFlow::Continue(()))
    }
}

#[cfg(test)]
mod tests;
