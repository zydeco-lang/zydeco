//! Scope-sensitive rebuilding with independent event consumers.
use super::{
    alloc::ScopedBuilder, dependencies::DependencyAnalyzer, scope::ResolveEnv, syntax::*, *,
};
use crate::{
    diagnostic::{CollectReported, Diagnostics, ReportedError},
    fold::Folder,
};
use std::collections::HashSet;
use zydeco_utils::prelude::FrozenArena;

type FoldResult<T> = std::result::Result<T, ReportedError>;

pub struct ResolveFolder<'a, O = ()> {
    pub spans: &'a SpanArena,
    pub bitter: FrozenArena<BitterArena>,
    pub(super) builder: ScopedBuilder,
    pub(super) dependencies: DependencyAnalyzer,
    observers: ((ReferenceIndex, DocumentationObserver), O),
    diagnostics: Vec<ResolveError>,
    failed_providers: HashSet<TermId>,
}

pub struct ResolveSourceOut {
    pub arena: FrozenArena<ScopedArena>,
    pub root: TermId,
}

/// Observations remain available after rejection; only accepted syntax is published.
pub struct ResolutionOutput<T> {
    pub program: Result<ResolveSourceOut>,
    pub observations: T,
    pub diagnostics: Vec<ResolveError>,
}

#[derive(Clone, Copy)]
enum Publication {
    Strict,
    Completion,
}

impl<'a> ResolveFolder<'a> {
    pub fn new(spans: &'a SpanArena, bitter: FrozenArena<BitterArena>) -> Self {
        let mut bitter = bitter.into_inner();
        let origins = std::mem::take(&mut bitter.origins);
        let builder = ScopedBuilder::new(&bitter, origins);
        Self {
            spans,
            bitter: FrozenArena::new(bitter),
            builder,
            dependencies: DependencyAnalyzer::default(),
            observers: ((ReferenceIndex::default(), DocumentationObserver::default()), ()),
            diagnostics: Vec::new(),
            failed_providers: HashSet::new(),
        }
    }

    pub fn with_observer<O: ResolutionObserver>(self, observer: O) -> ResolveFolder<'a, O> {
        ResolveFolder {
            spans: self.spans,
            bitter: self.bitter,
            builder: self.builder,
            dependencies: self.dependencies,
            observers: (self.observers.0, observer),
            diagnostics: self.diagnostics,
            failed_providers: self.failed_providers,
        }
    }

    pub fn run_completion(
        self, root: TermId, target: crate::textual::syntax::TermId,
    ) -> CompletionResolution {
        let output =
            self.with_observer(CompletionObserver::new(target)).run(root, Publication::Completion);
        CompletionResolution {
            site: output.observations,
            diagnostics: output.diagnostics,
            program: output.program,
        }
    }
}

impl<O: ResolutionObserver> ResolveFolder<'_, O> {
    pub fn run_source(self, root: TermId) -> Result<ResolveSourceOut> {
        self.run_observed(root).program
    }

    pub fn run_observed(self, root: TermId) -> ResolutionOutput<O::Output> {
        self.run(root, Publication::Strict)
    }

    fn run(mut self, root: TermId, policy: Publication) -> ResolutionOutput<O::Output> {
        let resolved =
            self.term(root, ResolveEnv { local: Local::for_body(), global: &Global::default() });
        self.dependencies.assert_closed();
        let ((users, documentation), observations) = self.observers.finish();
        let rejected = resolved.is_err()
            || matches!(policy, Publication::Strict) && !self.diagnostics.is_empty();
        let program = if rejected {
            Err(Box::new(
                Diagnostics::with_errors(self.diagnostics.clone())
                    .expect("rejection has a recorded diagnostic"),
            ))
        } else {
            Ok(ResolveSourceOut {
                arena: FrozenArena::new(self.builder.finish(
                    self.bitter.into_inner(),
                    users,
                    documentation,
                )),
                root,
            })
        };
        ResolutionOutput { program, observations, diagnostics: self.diagnostics }
    }

    pub(super) fn report(&mut self, error: ResolveError) -> ReportedError {
        self.diagnostics.push(error);
        ReportedError
    }

    fn scope_event(&mut self, id: TermId, kind: ScopeKind, env: &ResolveEnv<'_>) {
        self.observers.scope(&ScopeEvent {
            occurrence: id,
            origin: self.builder.origins.source(&id.into()),
            kind,
            scope: env.scope(),
        });
    }

    fn reference(&mut self, id: TermId, name: VarName, env: &ResolveEnv<'_>) -> Term<DefId> {
        let Some(binding) = env.scope().lookup(&name) else {
            self.report(ResolveError::UnboundVar(id.span(self).make(name)));
            return Term::Hole(Hole);
        };
        let event = ResolvedReference {
            occurrence: id,
            definition: binding.definition,
            dependency: binding.dependency,
            active_bindings: &env.local.under,
        };
        self.dependencies.reference(&event);
        self.observers.reference(&event);
        Term::Var(binding.definition)
    }

    pub(super) fn pattern<'e>(
        &mut self, id: PatId, mut env: ResolveEnv<'e>,
    ) -> FoldResult<ResolvedPattern<'e>> {
        let pat = self.bitter.pats[&id].clone();
        match &pat {
            | Pattern::Ann(Ann { tm, ty }) => {
                self.term(*ty, env.clone())?;
                env = self.pattern(*tm, env)?.env;
            }
            | Pattern::Var(def) => {
                let name = self.bitter.defs[def].clone();
                self.builder.defs.insert_new(*def, name.clone());
                env.local = env.local.bind_group([(name, *def)]);
            }
            | Pattern::Named(Named(_, inner))
            | Pattern::Ctor(Ctor(_, inner))
            | Pattern::Project(ProjectionPattern(_, inner)) => {
                env = self.pattern(*inner, env)?.env;
            }
            | Pattern::View(ViewPattern { function, pattern }) => {
                self.term(*function, env.clone())?;
                env = self.pattern(*pattern, env)?.env;
            }
            | Pattern::Alias(Alias(patterns)) => {
                for pat in patterns {
                    env = self.pattern(*pat, env)?.env;
                }
            }
            | Pattern::Cons(patterns) => {
                for pat in patterns {
                    env = self.pattern(*pat, env)?.env;
                }
            }
            | Pattern::Hole(_) | Pattern::Lit(_) | Pattern::Triv(_) => {}
        }
        self.builder.pats.insert_new(id, pat);
        Ok(ResolvedPattern { pattern: id, env })
    }

    fn provider(&mut self, id: TermId) -> FoldResult<()> {
        if self.failed_providers.contains(&id) {
            return Err(ReportedError);
        }
        if self.builder.terms.get(&id).is_none() {
            let result =
                self.term(id, ResolveEnv { local: Local::for_body(), global: &Global::default() });
            if result.is_err() {
                self.failed_providers.insert(id);
            }
            result?;
        }
        Ok(())
    }

    pub(super) fn term(&mut self, id: TermId, env: ResolveEnv<'_>) -> FoldResult<TermId> {
        let term = self.bitter.terms[&id].clone();
        // This classification is exhaustive: new constructors require a scope review.
        let term = match term {
            | Term::Meta(term) => {
                if term.0.is(crate::metadata::MetadataKind::Doc.name()) {
                    self.scope_event(id, ScopeKind::Documentation, &env);
                }
                self.term(term.1, env)?;
                term.into()
            }
            | Term::Hole(hole) => {
                self.scope_event(id, ScopeKind::Hole, &env);
                hole.into()
            }
            | Term::SourceBoundary(SourceBoundary(inner)) => {
                self.provider(inner)?;
                SourceBoundary(inner).into()
            }
            | Term::SignatureBoundary(SignatureBoundary(inner)) => {
                self.provider(inner)?;
                SignatureBoundary(inner).into()
            }
            | Term::Ann(Ann { tm, ty }) => {
                let ty = self.term(ty, env.clone());
                let tm = self.term(tm, env);
                Ann { tm: tm?, ty: ty? }.into()
            }
            | Term::Abs(Abs(binder, body)) => {
                let binder = self.pattern(binder, env)?;
                self.term(body, binder.env)?;
                Abs(binder.pattern, body).into()
            }
            | Term::ValAbs(Abs(binder, body)) => {
                let binder = self.pattern(binder, env)?;
                self.term(body, binder.env)?;
                Term::ValAbs(Abs(binder.pattern, body))
            }
            | Term::Fix(Fix(binder, body)) => {
                let binder = self.pattern(binder, env)?;
                self.term(body, binder.env)?;
                Fix(binder.pattern, body).into()
            }
            | Term::Pi(Pi(binder, body)) => {
                let binder = self.pattern(binder, env)?;
                self.term(body, binder.env)?;
                Pi(binder.pattern, body).into()
            }
            | Term::ValPi(ValPi(binder, body)) => {
                let binder = self.pattern(binder, env)?;
                self.term(body, binder.env)?;
                ValPi(binder.pattern, body).into()
            }
            | Term::Sigma(Sigma(binder, body)) => {
                let binder = self.pattern(binder, env)?;
                self.term(body, binder.env)?;
                Sigma(binder.pattern, body).into()
            }
            | Term::ManifestExists(term) => {
                let definition = self.term(term.definition, env.clone());
                let binder = self.pattern(term.binder, env);
                definition?;
                let binder = binder?;
                self.term(term.body, binder.env)?;
                term.into()
            }
            | Term::Pack(term) => {
                let definition = self.term(term.definition, env.clone());
                let binder = self.pattern(term.binder, env);
                definition?;
                let binder = binder?;
                self.term(term.body, binder.env)?;
                term.into()
            }
            | Term::Do(term) => {
                let bindee = self.term(term.bindee, env.clone());
                let binder = self.pattern(term.binder, env);
                bindee?;
                let binder = binder?;
                self.term(term.tail, binder.env)?;
                term.into()
            }
            | Term::Let(term) => {
                let bindee = self.term(term.bindee, env.clone());
                let binder = self.pattern(term.binder, env);
                bindee?;
                let binder = binder?;
                self.term(term.tail, binder.env)?;
                term.into()
            }
            | Term::MobileParam(MobileParam { tail, .. }) => {
                if env.local.boundary.is_none() {
                    return Err(self.report(ResolveError::UnenclosedThat(*id.span(self))));
                }
                self.term(tail, env)?;
                Residual(tail).into()
            }
            | Term::MobileBind(term) => {
                if env.local.boundary.is_none() {
                    return Err(self.report(ResolveError::UnenclosedThat(*id.span(self))));
                }
                self.term(term.tail, env)?;
                Residual(term.tail).into()
            }
            | Term::Block(Block(body)) => self.resolve_block(id, body, env)?,
            | Term::MoBlock(term) => {
                let monad = self.term(term.basis.monad, env.clone());
                let algebra = self.term(term.basis.algebra, env.clone());
                let body = self.term(term.body, env);
                monad?;
                algebra?;
                body?;
                term.into()
            }
            | Term::Match(term) => {
                let scrut = self.term(term.scrut, env.clone());
                let arms = term
                    .arms
                    .iter()
                    .map(|arm| {
                        let binder = self.pattern(arm.binder, env.clone())?;
                        self.term(arm.tail, binder.env)
                    })
                    .collect_reported();
                scrut?;
                arms?;
                term.into()
            }
            | Term::CoMatchClauses(term) => {
                term.clauses
                    .iter()
                    .map(|clause| {
                        let mut clause_env = env.clone();
                        for item in clause.spine.iter() {
                            if let CoPatternItem::Pat(pattern) = item {
                                clause_env = self.pattern(*pattern, clause_env)?.env;
                            }
                        }
                        self.term(clause.tail, clause_env)
                    })
                    .collect_reported()?;
                term.into()
            }
            | Term::Residual(_) | Term::RecGroup(_) => {
                unreachable!("generated context syntax is not a resolver input")
            }
            | ordinary @ (Term::Var(_)
            | Term::TypeOf(_)
            | Term::Internal(_)
            | Term::Sealed(_)
            | Term::Named(_)
            | Term::Label(_)
            | Term::Triv(_)
            | Term::Cons(_)
            | Term::App(_)
            | Term::Thunk(_)
            | Term::Force(_)
            | Term::Ret(_)
            | Term::Data(_)
            | Term::CoData(_)
            | Term::Ctor(_)
            | Term::CoMatch(_)
            | Term::Dtor(_)
            | Term::Proj(_)
            | Term::Lit(_)) => ordinary.fold_with(&mut OrdinaryFolder {
                resolver: self,
                occurrence: id,
                env: &env,
            })?,
        };
        self.builder.terms.insert_new(id, term);
        Ok(id)
    }
}

pub(super) struct ResolvedPattern<'a> {
    pub pattern: PatId,
    pub env: ResolveEnv<'a>,
}

struct OrdinaryFolder<'a, 'r, 'e, O> {
    resolver: &'r mut ResolveFolder<'a, O>,
    occurrence: TermId,
    env: &'r ResolveEnv<'e>,
}
impl<O: ResolutionObserver> Folder for OrdinaryFolder<'_, '_, '_, O> {
    type InputRef = VarName;
    type OutputRef = DefId;
    type Error = ReportedError;
    fn fold_def(&mut self, _id: DefId) -> FoldResult<DefId> {
        unreachable!("ordinary terms have no binders")
    }
    fn fold_pat(&mut self, _id: PatId) -> FoldResult<PatId> {
        unreachable!("ordinary terms have no binding patterns")
    }
    fn fold_term(&mut self, id: TermId) -> FoldResult<TermId> {
        self.resolver.term(id, self.env.clone())
    }
    fn fold_var(&mut self, name: VarName) -> FoldResult<Term<DefId>> {
        Ok(self.resolver.reference(self.occurrence, name, self.env))
    }
    fn fold_items<T, U>(
        &mut self, items: impl IntoIterator<Item = T>,
        mut fold: impl FnMut(&mut Self, T) -> FoldResult<U>,
    ) -> FoldResult<Vec<U>> {
        items.into_iter().map(|item| fold(self, item)).collect_reported()
    }
}

#[cfg(test)]
mod tests;
