use super::scope::ResolveEnv;
use crate::diagnostic::{CollectReported, ReportedError};
use crate::{
    bitter::syntax::{self as b, Term},
    scoped::{syntax::*, *},
};
use zydeco_utils::prelude::IdAllocator;
type FoldResult<T> = std::result::Result<T, ReportedError>;

/// One syntactic contribution discovered within a `begin` boundary.
#[derive(Clone, Debug)]
pub(super) enum MobileCandidate {
    Parameter { source: TermId, flavor: ParameterFlavor, binder: PatId },
    Definition { source: TermId, binder: PatId, bindee: TermId },
}

impl MobileCandidate {
    fn source(&self) -> TermId {
        match self {
            | Self::Parameter { source, .. } | Self::Definition { source, .. } => *source,
        }
    }

    fn binding_id(&self) -> BindingId {
        self.source()
    }

    fn resolve<O: ResolutionObserver>(
        &self, resolver: &mut ResolveFolder<'_, O>, block: TermId, mut env: ResolveEnv<'_>,
        source_order: usize,
    ) -> FoldResult<Binding> {
        let id = self.binding_id();
        env.local.under.push_back_mut(BindingSite { owner: block, id });
        let inner = match self {
            | Self::Parameter { flavor, binder, .. } => {
                resolver.pattern(*binder, env)?;
                BindingForm::Parameter(Parameter { flavor: *flavor, binder: *binder })
            }
            | Self::Definition { binder, bindee, .. } => {
                let bindee_result = resolver.term(*bindee, env.clone());
                let binder_result = resolver.pattern(*binder, env);
                bindee_result?;
                binder_result?;
                BindingForm::Definition(Definition { binder: *binder, bindee: *bindee })
            }
        };
        Ok(Binding { id, inner, metas: rpds::VectorSync::new_sync(), source_order })
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
            | b::Pattern::Project(b::ProjectionPattern(_, inner)) => self.pattern(*inner),
            | b::Pattern::View(b::ViewPattern { function, pattern }) => {
                [self.term(*function), self.pattern(*pattern)].into_iter().flatten().collect()
            }
            | b::Pattern::Alias(b::Alias(patterns)) => {
                patterns.iter().flat_map(|pattern| self.pattern(*pattern)).collect()
            }
            | b::Pattern::Cons(patterns) => {
                patterns.iter().flat_map(|pattern| self.pattern(*pattern)).collect()
            }
            | b::Pattern::Hole(_)
            | b::Pattern::Var(_)
            | b::Pattern::Lit(_)
            | b::Pattern::Triv(_) => Vec::new(),
        }
    }

    fn term(&self, term: TermId) -> Vec<MobileCandidate> {
        match &self.arena.terms[&term] {
            | Term::MobileParam(b::MobileParam { flavor, binder, tail }) => {
                std::iter::once(MobileCandidate::Parameter {
                    source: term,
                    flavor: *flavor,
                    binder: *binder,
                })
                .chain(self.pattern(*binder))
                .chain(self.term(*tail))
                .collect()
            }
            | Term::MobileBind(binding) => {
                let b::MobileBind { binder, bindee, tail } = &**binding;
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
            | Term::Block(_) | Term::SourceBoundary(_) | Term::SignatureBoundary(_) => Vec::new(),
            | Term::Residual(_) => {
                unreachable!("residual nodes are introduced only after candidate collection")
            }
            | Term::Meta(term) => {
                let b::MetaT(_, inner) = &**term;
                self.term(*inner)
            }
            | Term::TypeOf(b::TypeOf(inner))
            | Term::Sealed(b::Sealed(inner))
            | Term::Thunk(b::Thunk(inner))
            | Term::Force(b::Force(inner))
            | Term::Ret(b::Return(inner)) => self.term(*inner),
            | Term::MoBlock(term) => {
                let b::MoBlock { body, basis } = &**term;
                [self.term(basis.monad), self.term(basis.algebra), self.term(*body)]
                    .into_iter()
                    .flatten()
                    .collect()
            }
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
            | Term::ValAbs(b::Abs(pattern, body))
            | Term::Fix(b::Fix(pattern, body))
            | Term::Pi(b::Pi(pattern, body))
            | Term::ValPi(b::ValPi(pattern, body))
            | Term::Sigma(b::Sigma(pattern, body)) => {
                [self.pattern(*pattern), self.term(*body)].into_iter().flatten().collect()
            }
            | Term::App(b::App(function, argument)) => {
                [self.term(*function), self.term(*argument)].into_iter().flatten().collect()
            }
            | Term::ManifestExists(term) => {
                let b::ManifestExists { binder, definition, body } = &**term;
                [self.pattern(*binder), self.term(*definition), self.term(*body)]
                    .into_iter()
                    .flatten()
                    .collect()
            }
            | Term::Pack(term) => {
                let b::Pack { mode: _, binder, definition, body } = &**term;
                [self.pattern(*binder), self.term(*definition), self.term(*body)]
                    .into_iter()
                    .flatten()
                    .collect()
            }
            | Term::Do(term) => {
                let b::Bind { binder, bindee, tail } = &**term;
                [self.pattern(*binder), self.term(*bindee), self.term(*tail)]
                    .into_iter()
                    .flatten()
                    .collect()
            }
            | Term::Let(term) => {
                let b::Let { binder, bindee, tail } = &**term;
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
            | Term::CoMatchClauses(b::CoMatchClauses { clauses }) => clauses
                .iter()
                .flat_map(|clause| {
                    clause
                        .spine
                        .iter()
                        .filter_map(|item| match item {
                            | b::CoPatternItem::Pat(pattern) => Some(self.pattern(*pattern)),
                            | b::CoPatternItem::Dtor(_) => None,
                        })
                        .chain(std::iter::once(self.term(clause.tail)))
                })
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
    fn new<O: ResolutionObserver>(
        resolver: &mut ResolveFolder<'_, O>, block: TermId, candidates: &[MobileCandidate],
        mut local: Local,
    ) -> FoldResult<Self> {
        let projected = candidates
            .iter()
            .flat_map(|candidate| {
                let binder = match candidate {
                    | MobileCandidate::Parameter { binder, .. }
                    | MobileCandidate::Definition { binder, .. } => binder,
                };
                let site = BindingSite { owner: block, id: candidate.binding_id() };
                binder
                    .binders(&resolver.bitter)
                    .iter()
                    .map(|(name, definition)| (name.clone(), *definition, site))
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        let mut binders = rpds::HashTrieMapSync::<VarName, DefId>::new_sync();
        let mut rejected = false;
        for (name, definition, site) in projected {
            if let Some(previous) = binders.get(&name) {
                resolver.report(ResolveError::DuplicateDefinition(
                    previous.span(resolver).make(name.clone()),
                    definition.span(resolver).make(name.clone()),
                ));
                rejected = true;
            } else {
                binders.insert_mut(name, definition);
            }
            local.under_map.insert_mut(definition, site);
        }
        if rejected {
            return Err(ReportedError);
        }
        local.boundary = Some(block);
        local =
            local.bind_group(binders.iter().map(|(name, definition)| (name.clone(), *definition)));
        Ok(Self { local })
    }
}

/// Converts a resolved context DAG into the ordinary term forms used by the
/// checker, retaining recursive type components explicitly.
struct ContextElaboration<'a> {
    context: &'a BindingContext,
}

impl ContextElaboration<'_> {
    fn build<O: ResolutionObserver>(
        &self, resolver: &mut ResolveFolder<'_, O>, residual: TermId, block: TermId,
    ) -> FoldResult<TermId> {
        let mut rejected = false;
        for (_, node) in self.context.nodes.iter() {
            if let ContextNode::Recursive(bindings) = node
                && bindings.iter().any(|binding| matches!(binding.inner, BindingForm::Parameter(_)))
            {
                let source = bindings.first().map_or(block, |binding| binding.id);
                resolver.report(ResolveError::RecursiveParameter(*source.span(resolver)));
                rejected = true;
            }
        }
        if rejected {
            return Err(ReportedError);
        }
        Ok(self.context.topological_order().into_iter().rev().fold(
            residual,
            |tail, node| match self.context.nodes[&node].clone() {
                | ContextNode::Acyclic(binding) => {
                    let term = match binding.inner {
                        | BindingForm::Parameter(Parameter { flavor, binder }) => match flavor {
                            | ParameterFlavor::Plain => b::Abs(binder, tail).into(),
                            | ParameterFlavor::Value => b::Term::ValAbs(b::Abs(binder, tail)),
                        },
                        | BindingForm::Definition(Definition { binder, bindee }) => {
                            b::Let { binder, bindee, tail }.into()
                        }
                    };
                    resolver.builder.term(binding.id, term)
                }
                | ContextNode::Recursive(bindings) => {
                    let source = bindings.first().map_or(block, |binding| binding.id);
                    let definitions = bindings
                        .into_iter()
                        .map(|binding| match binding.inner {
                            | BindingForm::Definition(Definition { binder, bindee }) => {
                                b::RecursiveDefinition { binder, bindee }
                            }
                            | BindingForm::Parameter(_) => unreachable!(
                                "recursive parameters were rejected before elaboration"
                            ),
                        })
                        .collect();
                    resolver.builder.term(source, b::RecGroup { definitions, tail }.into())
                }
            },
        ))
    }
}

impl<O: ResolutionObserver> ResolveFolder<'_, O> {
    pub(super) fn resolve_block(
        &mut self, block: TermId, body: TermId, env: ResolveEnv<'_>,
    ) -> FoldResult<Term<DefId>> {
        let candidates = BlockCandidateCollector::new(&self.bitter).collect(body);
        let scope = BlockScope::new(self, block, &candidates, env.local)?;
        let env = ResolveEnv { local: scope.local, global: env.global };
        self.dependencies.begin_block(block, candidates.iter().map(MobileCandidate::binding_id));
        let bindings = candidates
            .iter()
            .enumerate()
            .map(|(order, candidate)| candidate.resolve(self, block, env.clone(), order))
            .collect_reported();
        let residual = self.term(body, env);
        let bindings = match (bindings, residual) {
            | (Ok(bindings), Ok(_)) => {
                bindings.into_iter().map(|binding| (binding.id, binding)).collect()
            }
            | _ => {
                self.dependencies.abort_block(block);
                return Err(ReportedError);
            }
        };
        let dependencies = self.dependencies.finish_block(block);
        let context = BindingContext::from_bindings(IdAllocator::new(), bindings, dependencies);
        let elaborated = ContextElaboration { context: &context }.build(self, body, block)?;
        self.builder.blocks.insert_new(
            block,
            ContextualTerm { context, body: BlockBody { residual: body, elaborated } },
        );
        Ok(b::Block(elaborated).into())
    }
}
