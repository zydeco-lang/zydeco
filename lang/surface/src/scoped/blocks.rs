use crate::{
    bitter::syntax::{self as b, Term},
    scoped::{syntax::*, *},
};
use zydeco_utils::prelude::{DepGraph, IdAllocator};

/// One syntactic contribution discovered within a `begin` boundary.
#[derive(Clone, Debug)]
pub(super) enum MobileCandidate {
    Parameter { source: TermId, binder: PatId },
    Definition { source: TermId, binder: PatId, bindee: TermId },
}

impl MobileCandidate {
    fn source(&self) -> TermId {
        match self {
            | Self::Parameter { source, .. } | Self::Definition { source, .. } => *source,
        }
    }

    fn binder(&self) -> PatId {
        match self {
            | Self::Parameter { binder, .. } | Self::Definition { binder, .. } => *binder,
        }
    }

    fn binding_id(&self) -> BindingId {
        BindingId::Term(self.source())
    }

    fn resolve(
        &self, resolver: &mut Resolver<'_>, block: TermId, local: &Local, global: &Global,
        source_order: usize,
    ) -> Result<Binding> {
        let id = self.binding_id();
        let mut local = local.clone();
        local.under.push_back(BindingSite { owner: ContextOwner::Block(block), id });
        let inner = match self {
            | Self::Parameter { binder, .. } => {
                let _ = binder.resolve(resolver, (local, global))?;
                BindingForm::Parameter(Parameter { binder: *binder })
            }
            | Self::Definition { binder, bindee, .. } => {
                bindee.resolve(resolver, (local.clone(), global))?;
                let _ = binder.resolve(resolver, (local, global))?;
                BindingForm::Definition(Definition { binder: *binder, bindee: *bindee })
            }
        };
        Ok(Binding { id, inner, metas: im::Vector::new(), source_order })
    }
}

/// A syntax-directed collector which treats nested `begin` terms as new
/// closure boundaries.
pub(super) struct BlockCandidateCollector<'a> {
    arena: &'a BitterArena,
}

impl<'a> BlockCandidateCollector<'a> {
    pub(super) fn new(arena: &'a BitterArena) -> Self {
        Self { arena }
    }

    pub(super) fn collect(&self, body: TermId) -> Vec<MobileCandidate> {
        self.term(body)
    }

    fn pattern(&self, pattern: PatId) -> Vec<MobileCandidate> {
        match &self.arena.pats[&pattern] {
            | b::Pattern::Ann(b::Ann { tm, ty }) => {
                [self.pattern(*tm), self.term(*ty)].into_iter().flatten().collect()
            }
            | b::Pattern::Named(b::Named(_, inner)) | b::Pattern::Ctor(b::Ctor(_, inner)) => {
                self.pattern(*inner)
            }
            | b::Pattern::Cons(patterns) => {
                patterns.iter().flat_map(|pattern| self.pattern(*pattern)).collect()
            }
            | b::Pattern::Hole(_) | b::Pattern::Var(_) | b::Pattern::Triv(_) => Vec::new(),
        }
    }

    fn term(&self, term: TermId) -> Vec<MobileCandidate> {
        match &self.arena.terms[&term] {
            | Term::MobileParam(b::MobileParam { binder, tail }) => {
                std::iter::once(MobileCandidate::Parameter { source: term, binder: *binder })
                    .chain(self.pattern(*binder))
                    .chain(self.term(*tail))
                    .collect()
            }
            | Term::MobileBind(b::MobileBind { binder, bindee, tail }) => {
                std::iter::once(MobileCandidate::Definition {
                    source: term,
                    binder: *binder,
                    bindee: *bindee,
                })
                .chain(self.pattern(*binder))
                .chain(self.term(*bindee))
                .chain(self.term(*tail))
                .collect()
            }
            | Term::Block(_) => Vec::new(),
            | Term::Residual(_) => {
                unreachable!("residual nodes are introduced only after candidate collection")
            }
            | Term::Meta(b::MetaT(_, inner))
            | Term::Sealed(b::Sealed(inner))
            | Term::Thunk(b::Thunk(inner))
            | Term::Force(b::Force(inner))
            | Term::Ret(b::Return(inner))
            | Term::MoBlock(b::MoBlock(inner)) => self.term(*inner),
            | Term::Ann(b::Ann { tm, ty }) => {
                [self.term(*tm), self.term(*ty)].into_iter().flatten().collect()
            }
            | Term::Named(b::Named(_, inner))
            | Term::Label(b::Label(_, inner))
            | Term::Ctor(b::Ctor(_, inner))
            | Term::Dtor(b::Dtor(inner, _))
            | Term::Proj(b::Proj(inner, _)) => self.term(*inner),
            | Term::Cons(items) => items.iter().flat_map(|item| self.term(*item)).collect(),
            | Term::Abs(b::Abs(pattern, body))
            | Term::Fix(b::Fix(pattern, body))
            | Term::Pi(b::Pi(pattern, body))
            | Term::Sigma(b::Sigma(pattern, body)) => {
                [self.pattern(*pattern), self.term(*body)].into_iter().flatten().collect()
            }
            | Term::App(b::App(function, argument)) => {
                [self.term(*function), self.term(*argument)].into_iter().flatten().collect()
            }
            | Term::ManifestExists(b::ManifestExists { binder, definition, body }) => {
                [self.pattern(*binder), self.term(*definition), self.term(*body)]
                    .into_iter()
                    .flatten()
                    .collect()
            }
            | Term::Do(b::Bind { binder, bindee, tail })
            | Term::Let(b::Let { binder, bindee, tail }) => {
                [self.pattern(*binder), self.term(*bindee), self.term(*tail)]
                    .into_iter()
                    .flatten()
                    .collect()
            }
            | Term::Data(b::Data { arms }) => {
                arms.iter().flat_map(|arm| self.term(arm.param)).collect()
            }
            | Term::CoData(b::CoData { arms }) => {
                arms.iter().flat_map(|arm| self.term(arm.out)).collect()
            }
            | Term::Match(b::Match { scrut, arms }) => std::iter::once(self.term(*scrut))
                .chain(arms.iter().flat_map(|arm| [self.pattern(arm.binder), self.term(arm.tail)]))
                .flatten()
                .collect(),
            | Term::CoMatch(b::CoMatch { arms }) => {
                arms.iter().flat_map(|arm| self.term(arm.tail)).collect()
            }
            | Term::RecGroup(_) => {
                unreachable!("recursive groups are introduced only after name resolution")
            }
            | Term::Internal(_) | Term::Hole(_) | Term::Var(_) | Term::Triv(_) | Term::Lit(_) => {
                Vec::new()
            }
        }
    }
}

/// Installs block-wide names before any occurrence in the block is resolved.
struct BlockScope {
    local: Local,
}

impl BlockScope {
    fn new(
        resolver: &Resolver<'_>, block: TermId, candidates: &[MobileCandidate], mut local: Local,
    ) -> Result<Self> {
        let binders = candidates.iter().try_fold(
            im::HashMap::<VarName, DefId>::new(),
            |binders, candidate| {
                candidate.binder().binders(&resolver.bitter).into_iter().try_fold(
                    binders,
                    |binders, (name, definition)| -> Result<_> {
                        if let Some(previous) = binders.get(&name) {
                            Err(ResolveError::DuplicateDefinition(
                                previous.span(resolver).clone().make(name.clone()),
                                definition.span(resolver).clone().make(name.clone()),
                            ))?
                        }
                        Ok(binders.update(name, definition))
                    },
                )
            },
        )?;
        let owner = ContextOwner::Block(block);
        local.boundary = Some(block);
        local.under_map = local.under_map.union(
            candidates
                .iter()
                .flat_map(|candidate| {
                    let site = BindingSite { owner, id: candidate.binding_id() };
                    candidate
                        .binder()
                        .binders(&resolver.bitter)
                        .into_iter()
                        .map(|(_, definition)| definition)
                        .map(move |definition| (definition, site))
                })
                .collect(),
        );
        local.var_to_def = local.var_to_def.union(binders);
        Ok(Self { local })
    }
}

/// Converts a resolved context DAG into the ordinary term forms used by the
/// checker, retaining recursive type components explicitly.
struct ContextElaboration<'a> {
    context: &'a BindingContext,
}

impl<'a> ContextElaboration<'a> {
    fn new(context: &'a BindingContext) -> Self {
        Self { context }
    }

    fn build(
        &self, resolver: &mut Resolver<'_>, residual: TermId, block: TermId,
    ) -> Result<TermId> {
        self.context.topological_order().into_iter().rev().try_fold(residual, |tail, node| {
            match self.context.nodes[&node].clone() {
                | ContextNode::Acyclic(binding) => {
                    let source = Self::term_source(binding.id);
                    let term = match binding.inner {
                        | BindingForm::Parameter(Parameter { binder }) => {
                            b::Abs(binder, tail).into()
                        }
                        | BindingForm::Definition(Definition { binder, bindee }) => {
                            b::Let { binder, bindee, tail }.into()
                        }
                        | BindingForm::External(_) => {
                            unreachable!("nested blocks cannot contribute externals")
                        }
                    };
                    Ok(resolver.alloc_scoped_term(source, term))
                }
                | ContextNode::Recursive(bindings) => {
                    let source = bindings
                        .first()
                        .map(|binding| Self::term_source(binding.id))
                        .unwrap_or(block);
                    let definitions = bindings
                        .into_iter()
                        .map(|binding| match binding.inner {
                            | BindingForm::Definition(Definition { binder, bindee }) => {
                                Ok(b::AliasBody { binder, bindee })
                            }
                            | BindingForm::Parameter(_) => {
                                Err(ResolveError::RecursiveParameter(source.span(resolver).clone())
                                    .into())
                            }
                            | BindingForm::External(_) => {
                                unreachable!("nested blocks cannot contribute externals")
                            }
                        })
                        .collect::<Result<Vec<_>>>()?;
                    Ok(resolver.alloc_scoped_term(source, b::RecGroup { definitions, tail }.into()))
                }
            }
        })
    }

    fn term_source(binding: BindingId) -> TermId {
        match binding {
            | BindingId::Term(term) => term,
            | BindingId::Declaration(_) => {
                unreachable!("a nested block binding must originate at a term")
            }
        }
    }
}

impl Resolver<'_> {
    pub(super) fn resolve_block(
        &mut self, block: TermId, body: TermId, local: Local, global: &Global,
    ) -> Result<Term<DefId>> {
        let candidates = BlockCandidateCollector::new(&self.bitter).collect(body);
        let scope = BlockScope::new(self, block, &candidates, local)?;
        let dependencies = candidates.iter().fold(DepGraph::new(), |mut graph, candidate| {
            graph.add(candidate.binding_id(), []);
            graph
        });
        self.block_deps.insert_new(block, dependencies);
        let bindings = candidates
            .iter()
            .enumerate()
            .map(|(source_order, candidate)| {
                candidate.resolve(self, block, &scope.local, global, source_order)
            })
            .collect::<Result<Vec<_>>>()?
            .into_iter()
            .map(|binding| (binding.id, binding))
            .collect();
        body.resolve(self, (scope.local, global))?;
        let dependencies =
            self.block_deps.remove(&block).expect("the active block dependency graph must exist");
        let context = BindingContext::from_bindings(IdAllocator::new(), bindings, dependencies);
        let elaborated = ContextElaboration::new(&context).build(self, body, block)?;
        self.blocks.insert_new(
            block,
            ContextualTerm { context, body: BlockBody { residual: body, elaborated } },
        );
        Ok(b::Block(elaborated).into())
    }

    fn alloc_scoped_term(&mut self, source: TermId, term: Term<DefId>) -> TermId {
        let id = self.allocator.alloc();
        self.terms.insert_new(id, term);
        let textual = *self
            .bitter
            .textual
            .back(&source.into())
            .expect("a source term must retain its textual origin");
        self.bitter.textual.insert_new(textual, id.into());
        id
    }
}
