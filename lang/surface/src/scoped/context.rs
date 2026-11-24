//! Free-variable summaries over the elaborated scoped graph.

use super::{
    syntax::*,
    traverse::{Node, Traversal, Visitor},
};
use std::{collections::HashSet, convert::Infallible, ops::ControlFlow};

/// Free-variable and source-boundary summaries needed while checking resolved terms.
#[derive(Debug, Default)]
pub struct TermContexts {
    coctxs_term_local: ArenaPagedAssoc<TermId, CoContext>,
    coctxs_pat_local: ArenaAssoc<PatId, CoContext>,
    boundary_terms: rustc_hash::FxHashSet<TermId>,
    boundary_pats: rustc_hash::FxHashSet<PatId>,
}

impl TermContexts {
    /// Collect one free-variable summary per term in postorder.
    pub fn collect(scoped: &ScopedArena, root: TermId) -> Self {
        let mut collector = ContextCollector::default();
        let ControlFlow::Continue(()) = Traversal::new(scoped)
            .run(root.into(), &mut collector)
            .expect("resolved syntax must be acyclic");
        collector.finish()
    }

    /// Variables used freely by one resolved term.
    pub fn at(&self, term: &TermId) -> &CoContext {
        &self.coctxs_term_local[term]
    }

    /// Whether any definition in `defs` may occur freely inside `entity`.
    pub fn mentions_any(&self, entity: EntityId, defs: &HashSet<DefId>) -> bool {
        if defs.is_empty() {
            return false;
        }
        match entity {
            | EntityId::Term(term) => match self.coctxs_term_local.get(&term) {
                | Some(coctx) => defs.iter().any(|def| coctx.contains(def)),
                | None => true,
            },
            | EntityId::Pat(pat) => match self.coctxs_pat_local.get(&pat) {
                | Some(coctx) => defs.iter().any(|def| coctx.contains(def)),
                | None => true,
            },
            | EntityId::Def(def) => defs.contains(&def),
        }
    }

    /// Whether `def` may occur freely inside `entity`.
    pub fn mentions_def(&self, entity: EntityId, def: &DefId) -> bool {
        match entity {
            | EntityId::Term(term) => match self.coctxs_term_local.get(&term) {
                | Some(coctx) => coctx.contains(def),
                | None => true,
            },
            | EntityId::Pat(pat) => match self.coctxs_pat_local.get(&pat) {
                | Some(coctx) => coctx.contains(def),
                | None => true,
            },
            | EntityId::Def(found) => found == *def,
        }
    }

    /// Whether `entity` contains an unresolved `SourceBoundary`.
    pub fn has_source_boundary(&self, entity: EntityId) -> bool {
        match entity {
            | EntityId::Term(term) => {
                self.coctxs_term_local.get(&term).is_none() || self.boundary_terms.contains(&term)
            }
            | EntityId::Pat(pat) => {
                self.coctxs_pat_local.get(&pat).is_none() || self.boundary_pats.contains(&pat)
            }
            | EntityId::Def(_) => false,
        }
    }
}

/// A postorder analysis that can share a traversal with other visitors.
/// Its node summaries are independent of the incoming lexical environment.
#[derive(Default)]
pub struct ContextCollector {
    ctxs_pat_local: ArenaAssoc<PatId, Context>,
    coctxs_pat_local: ArenaAssoc<PatId, CoContext>,
    coctxs_term_local: ArenaPagedAssoc<TermId, CoContext>,
    boundary_terms: rustc_hash::FxHashSet<TermId>,
    boundary_pats: rustc_hash::FxHashSet<PatId>,
}

impl ContextCollector {
    /// Retain term and pattern summaries along with source-boundary markers.
    /// Only nodes reached by a completed traversal have summaries.
    pub fn finish(self) -> TermContexts {
        TermContexts {
            coctxs_term_local: self.coctxs_term_local,
            coctxs_pat_local: self.coctxs_pat_local,
            boundary_terms: self.boundary_terms,
            boundary_pats: self.boundary_pats,
        }
    }
}

impl Visitor for ContextCollector {
    type Break = Infallible;

    fn exit(&mut self, node: Node<'_>) -> ControlFlow<Self::Break> {
        match node {
            | Node::Definition(_) => {}
            | Node::Pattern(id, pattern) => self.pattern(id, pattern),
            | Node::Term(id, term) => self.term(id, term),
        }
        ControlFlow::Continue(())
    }
}

impl ContextCollector {
    /// Updates [`Self::ctxs_pat_local`] and [`Self::coctxs_pat_local`].
    fn pattern(&mut self, pat: PatId, item: &Pattern) {
        if self.ctxs_pat_local.get(&pat).is_some() {
            return;
        }
        let has_boundary = match item {
            | Pattern::Ann(Ann { tm, ty }) => {
                self.boundary_pats.contains(tm) || self.boundary_terms.contains(ty)
            }
            | Pattern::Hole(_) | Pattern::Lit(_) | Pattern::Var(_) | Pattern::Triv(_) => false,
            | Pattern::Named(Named(_, inner))
            | Pattern::Ctor(Ctor(_, inner))
            | Pattern::Project(ProjectionPattern(_, inner)) => self.boundary_pats.contains(inner),
            | Pattern::View(ViewPattern { function, pattern }) => {
                self.boundary_pats.contains(pattern) || self.boundary_terms.contains(function)
            }
            | Pattern::Alias(Alias(inner)) => {
                inner.iter().any(|item| self.boundary_pats.contains(item))
            }
            | Pattern::Cons(inner) => inner.iter().any(|item| self.boundary_pats.contains(item)),
        };
        if has_boundary {
            self.boundary_pats.insert(pat);
        }
        match item {
            | Pattern::Ann(inner) => {
                let Ann { tm, ty } = inner;
                self.ctxs_pat_local.insert_new(pat, self.ctxs_pat_local[tm].to_owned());
                self.coctxs_pat_local.insert_new(pat, {
                    let co_tm = self.coctxs_pat_local[tm].to_owned();
                    let co_ty = self.coctxs_term_local[ty].to_owned();
                    co_tm + co_ty
                });
            }
            | Pattern::Hole(inner) => {
                let Hole = inner;
                self.ctxs_pat_local.insert_new(pat, Context::new());
                self.coctxs_pat_local.insert_new(pat, CoContext::new());
            }
            | Pattern::Lit(_) => {
                self.ctxs_pat_local.insert_new(pat, Context::new());
                self.coctxs_pat_local.insert_new(pat, CoContext::new());
            }
            | Pattern::Var(inner) => {
                let def = inner;
                self.ctxs_pat_local.insert_new(pat, Context::singleton(*def));
                self.coctxs_pat_local.insert_new(pat, CoContext::new());
            }
            | Pattern::Named(inner) => {
                let Named(_name, inner) = inner;
                self.ctxs_pat_local.insert_new(pat, self.ctxs_pat_local[inner].to_owned());
                self.coctxs_pat_local.insert_new(pat, self.coctxs_pat_local[inner].to_owned());
            }
            | Pattern::Triv(Triv) => {
                self.ctxs_pat_local.insert_new(pat, Context::new());
                self.coctxs_pat_local.insert_new(pat, CoContext::new());
            }
            | Pattern::Ctor(inner) => {
                let Ctor(_ctorv, body) = inner;
                self.ctxs_pat_local.insert_new(pat, self.ctxs_pat_local[body].to_owned());
                self.coctxs_pat_local.insert_new(pat, self.coctxs_pat_local[body].to_owned());
            }
            | Pattern::Project(ProjectionPattern(_, inner)) => {
                self.ctxs_pat_local.insert_new(pat, self.ctxs_pat_local[inner].to_owned());
                self.coctxs_pat_local.insert_new(pat, self.coctxs_pat_local[inner].to_owned());
            }
            | Pattern::View(ViewPattern { function, pattern }) => {
                let local = self.ctxs_pat_local[pattern].to_owned();
                let colocal = self.coctxs_pat_local[pattern].to_owned()
                    + self.coctxs_term_local[function].to_owned();
                self.ctxs_pat_local.insert_new(pat, local);
                self.coctxs_pat_local.insert_new(pat, colocal);
            }
            | Pattern::Alias(Alias(inner)) => {
                let local = inner
                    .iter()
                    .fold(Context::new(), |ctx, item| ctx + self.ctxs_pat_local[item].to_owned());
                let colocal = inner.iter().fold(CoContext::new(), |ctx, item| {
                    ctx + self.coctxs_pat_local[item].to_owned()
                });
                self.ctxs_pat_local.insert_new(pat, local);
                self.coctxs_pat_local.insert_new(pat, colocal);
            }
            | Pattern::Cons(inner) => {
                let local = inner
                    .iter()
                    .fold(Context::new(), |ctx, item| ctx + self.ctxs_pat_local[item].to_owned());
                let colocal = inner.iter().fold(CoContext::new(), |ctx, item| {
                    ctx + self.coctxs_pat_local[item].to_owned()
                });
                self.ctxs_pat_local.insert_new(pat, local);
                self.coctxs_pat_local.insert_new(pat, colocal);
            }
        }
    }

    /// Record what a telescope leaves free: each binder scopes over the binders after it
    /// and the body.
    fn telescope_coctx(&mut self, term: TermId, pats: &[PatId], body: &TermId) {
        let co = pats.iter().rev().fold(self.coctxs_term_local[body].to_owned(), |co, pat| {
            co - self.ctxs_pat_local[pat].to_owned() + self.coctxs_pat_local[pat].to_owned()
        });
        self.coctxs_term_local.insert_new(term, co);
    }

    /// Updates [`Self::coctxs_term_local`].
    // Remaining worklist: split local co-context collection by term form.
    #[allow(clippy::too_many_lines)]
    fn term(&mut self, term: TermId, item: &Term) {
        if self.coctxs_term_local.get(&term).is_some() {
            return;
        }
        let has_boundary = match item {
            | Term::SourceBoundary(_) => true,
            | Term::Internal(_) | Term::Hole(_) | Term::Var(_) | Term::Triv(_) | Term::Lit(_) => {
                false
            }
            | Term::Meta(inner) => self.boundary_terms.contains(&inner.body),
            | Term::TypeOf(TypeOf(operand)) => self.boundary_terms.contains(operand),
            | Term::SignatureBoundary(SignatureBoundary(inner))
            | Term::Sealed(Sealed(inner))
            | Term::Named(Named(_, inner))
            | Term::Label(Label(_, inner))
            | Term::Thunk(Thunk(inner))
            | Term::Force(Force(inner))
            | Term::Ret(Return(inner))
            | Term::Residual(Residual(inner))
            | Term::Block(Block(inner))
            | Term::CbvBlock(CbvBlock { body: inner })
            | Term::Ctor(Ctor(_, inner))
            | Term::Dtor(Dtor(inner, _))
            | Term::Proj(Proj(inner, _)) => self.boundary_terms.contains(inner),
            | Term::Ann(inner) => {
                self.boundary_terms.contains(&inner.tm) || self.boundary_terms.contains(&inner.ty)
            }
            | Term::Seal(inner) => {
                self.boundary_terms.contains(&inner.tm) || self.boundary_terms.contains(&inner.ty)
            }
            | Term::Cons(inner) => inner.iter().any(|item| self.boundary_terms.contains(item)),
            | Term::Abs(Abs(pats, body)) | Term::Pi(Pi(pats, body)) => {
                pats.iter().any(|pat| self.boundary_pats.contains(pat))
                    || self.boundary_terms.contains(body)
            }
            | Term::ValAbs(Abs(pat, body))
            | Term::Fix(Fix(pat, body))
            | Term::ValPi(ValPi(pat, body))
            | Term::Sigma(Sigma(pat, body)) => {
                self.boundary_pats.contains(pat) || self.boundary_terms.contains(body)
            }
            | Term::App(App(head, arguments)) => {
                self.boundary_terms.contains(head)
                    || arguments.iter().any(|argument| self.boundary_terms.contains(argument))
            }
            | Term::ManifestExists(inner) => {
                self.boundary_pats.contains(&inner.binder)
                    || self.boundary_terms.contains(&inner.definition)
                    || self.boundary_terms.contains(&inner.body)
            }
            | Term::Pack(inner) => {
                self.boundary_pats.contains(&inner.binder)
                    || self.boundary_terms.contains(&inner.definition)
                    || self.boundary_terms.contains(&inner.body)
            }
            | Term::Do(inner) => {
                self.boundary_pats.contains(&inner.binder)
                    || self.boundary_terms.contains(&inner.bindee)
                    || self.boundary_terms.contains(&inner.tail)
            }
            | Term::Let(inner) => {
                self.boundary_pats.contains(&inner.binder)
                    || self.boundary_terms.contains(&inner.bindee)
                    || self.boundary_terms.contains(&inner.tail)
            }
            | Term::RecGroup(RecGroup { definitions, tail }) => {
                definitions.iter().any(|definition| {
                    self.boundary_pats.contains(&definition.binder)
                        || self.boundary_terms.contains(&definition.bindee)
                }) || self.boundary_terms.contains(tail)
            }
            | Term::MoBlock(inner) => {
                self.boundary_terms.contains(&inner.body)
                    || self.boundary_terms.contains(&inner.basis.monad)
                    || self.boundary_terms.contains(&inner.basis.algebra)
            }
            | Term::Data(Data { arms }) => {
                arms.iter().any(|arm| self.boundary_terms.contains(&arm.param))
            }
            | Term::CoData(CoData { arms }) => {
                arms.iter().any(|arm| self.boundary_terms.contains(&arm.out))
            }
            | Term::Match(Match { scrut, arms }) => {
                self.boundary_terms.contains(scrut)
                    || arms.iter().any(|arm| {
                        self.boundary_pats.contains(&arm.binder)
                            || self.boundary_terms.contains(&arm.tail)
                    })
            }
            | Term::CoMatchClauses(CoMatchClauses { clauses }) => clauses.iter().any(|clause| {
                self.boundary_terms.contains(&clause.tail)
                    || clause.spine.iter().any(|item| match item {
                        | CoPatternItem::Pat(pat) => self.boundary_pats.contains(pat),
                        | CoPatternItem::Dtor(_) => false,
                    })
            }),
            | Term::CoMatch(CoMatch { arms }) => {
                arms.iter().any(|arm| self.boundary_terms.contains(&arm.tail))
            }
        };
        if has_boundary {
            self.boundary_terms.insert(term);
        }
        match item {
            | Term::Meta(inner) => {
                let MetaTerm { body, .. } = &**inner;
                let co_term = self.coctxs_term_local[body].to_owned();
                self.coctxs_term_local.insert_new(term, co_term);
            }
            | Term::TypeOf(TypeOf(operand)) => {
                let co_operand = self.coctxs_term_local[operand].to_owned();
                self.coctxs_term_local.insert_new(term, co_operand);
            }
            | Term::SourceBoundary(inner) => {
                let SourceBoundary(inner) = inner;
                let co_inner = self.coctxs_term_local[inner].to_owned();
                self.coctxs_term_local.insert_new(term, co_inner);
            }
            | Term::SignatureBoundary(inner) => {
                let SignatureBoundary(inner) = inner;
                let co_inner = self.coctxs_term_local[inner].to_owned();
                self.coctxs_term_local.insert_new(term, co_inner);
            }
            | Term::Internal(_) => {
                self.coctxs_term_local.insert_new(term, CoContext::new());
            }
            | Term::Sealed(inner) => {
                let Sealed(inner) = inner;
                let co_inner = self.coctxs_term_local[inner].to_owned();
                self.coctxs_term_local.insert_new(term, co_inner);
            }
            | Term::Ann(inner) => {
                let Ann { tm, ty } = inner;
                self.coctxs_term_local.insert_new(term, {
                    let co_tm = self.coctxs_term_local[tm].to_owned();
                    let co_ty = self.coctxs_term_local[ty].to_owned();
                    co_tm + co_ty
                });
            }
            | Term::Seal(inner) => {
                let Seal { tm, ty } = inner;
                self.coctxs_term_local.insert_new(term, {
                    let co_tm = self.coctxs_term_local[tm].to_owned();
                    let co_ty = self.coctxs_term_local[ty].to_owned();
                    co_tm + co_ty
                });
            }
            | Term::Hole(inner) => {
                let Hole = inner;
                self.coctxs_term_local.insert_new(term, CoContext::new());
            }
            | Term::Var(inner) => {
                let def = inner;
                self.coctxs_term_local.insert_new(term, CoContext::singleton(*def));
            }
            | Term::Named(inner) => {
                let Named(_name, inner) = inner;
                let co_inner = self.coctxs_term_local[inner].to_owned();
                self.coctxs_term_local.insert_new(term, co_inner);
            }
            | Term::Label(inner) => {
                let Label(_name, inner) = inner;
                let co_inner = self.coctxs_term_local[inner].to_owned();
                self.coctxs_term_local.insert_new(term, co_inner);
            }
            | Term::Triv(Triv) => {
                self.coctxs_term_local.insert_new(term, CoContext::new());
            }
            | Term::Cons(inner) => {
                let colocal = inner.iter().fold(CoContext::new(), |ctx, item| {
                    ctx + self.coctxs_term_local[item].to_owned()
                });
                self.coctxs_term_local.insert_new(term, colocal);
            }
            | Term::Abs(Abs(pats, body)) => self.telescope_coctx(term, pats, body),
            | Term::ValAbs(inner) => {
                let Abs(pat, body) = inner;
                let co_body = self.coctxs_term_local[body].to_owned();
                let cx_pat = self.ctxs_pat_local[pat].to_owned();
                let co_pat = self.coctxs_pat_local[pat].to_owned();
                self.coctxs_term_local.insert_new(term, co_body - cx_pat + co_pat);
            }
            | Term::App(App(head, arguments)) => {
                let co = arguments
                    .iter()
                    .fold(self.coctxs_term_local[head].to_owned(), |co, argument| {
                        co + self.coctxs_term_local[argument].to_owned()
                    });
                self.coctxs_term_local.insert_new(term, co);
            }
            | Term::Fix(inner) => {
                let Fix(pat, body) = inner;
                let co_body = self.coctxs_term_local[body].to_owned();
                let cx_pat = self.ctxs_pat_local[pat].to_owned();
                let co_pat = self.coctxs_pat_local[pat].to_owned();
                self.coctxs_term_local.insert_new(term, co_body - cx_pat + co_pat);
            }
            | Term::Pi(Pi(pats, body)) => self.telescope_coctx(term, pats, body),
            | Term::ValPi(inner) => {
                let ValPi(pat, body) = inner;
                let co_body = self.coctxs_term_local[body].to_owned();
                let cx_pat = self.ctxs_pat_local[pat].to_owned();
                let co_pat = self.coctxs_pat_local[pat].to_owned();
                self.coctxs_term_local.insert_new(term, co_body - cx_pat + co_pat);
            }
            | Term::Sigma(inner) => {
                let Sigma(pat, body) = inner;
                let co_body = self.coctxs_term_local[body].to_owned();
                let cx_pat = self.ctxs_pat_local[pat].to_owned();
                let co_pat = self.coctxs_pat_local[pat].to_owned();
                self.coctxs_term_local.insert_new(term, co_body - cx_pat + co_pat);
            }
            | Term::ManifestExists(inner) => {
                let ManifestExists { binder, definition, body } = &**inner;
                let co_body = self.coctxs_term_local[body].to_owned();
                let cx_binder = self.ctxs_pat_local[binder].to_owned();
                let co_binder = self.coctxs_pat_local[binder].to_owned();
                let co_definition = self.coctxs_term_local[definition].to_owned();
                self.coctxs_term_local
                    .insert_new(term, co_body - cx_binder + co_binder + co_definition);
            }
            | Term::Pack(inner) => {
                let Pack { mode: _, binder, definition, body } = &**inner;
                let co_body = self.coctxs_term_local[body].to_owned();
                let cx_binder = self.ctxs_pat_local[binder].to_owned();
                let co_binder = self.coctxs_pat_local[binder].to_owned();
                let co_definition = self.coctxs_term_local[definition].to_owned();
                self.coctxs_term_local
                    .insert_new(term, co_body - cx_binder + co_binder + co_definition);
            }
            | Term::Thunk(inner) => {
                let Thunk(body) = inner;
                let co_body = self.coctxs_term_local[body].to_owned();
                self.coctxs_term_local.insert_new(term, co_body);
            }
            | Term::Force(inner) => {
                let Force(body) = inner;
                let co_body = self.coctxs_term_local[body].to_owned();
                self.coctxs_term_local.insert_new(term, co_body);
            }
            | Term::Ret(inner) => {
                let Return(body) = inner;
                let co_body = self.coctxs_term_local[body].to_owned();
                self.coctxs_term_local.insert_new(term, co_body);
            }
            | Term::Do(inner) => {
                let Bind { binder, bindee, tail } = &**inner;
                let co_tail = self.coctxs_term_local[tail].to_owned();
                let cx_binder = self.ctxs_pat_local[binder].to_owned();
                let co_binder = self.coctxs_pat_local[binder].to_owned();
                let co_bindee = self.coctxs_term_local[bindee].to_owned();
                self.coctxs_term_local
                    .insert_new(term, co_tail - cx_binder + co_binder + co_bindee);
            }
            | Term::Let(inner) => {
                let Let { binder, bindee, tail } = &**inner;
                let co_tail = self.coctxs_term_local[tail].to_owned();
                let cx_binder = self.ctxs_pat_local[binder].to_owned();
                let co_binder = self.coctxs_pat_local[binder].to_owned();
                let co_bindee = self.coctxs_term_local[bindee].to_owned();
                self.coctxs_term_local
                    .insert_new(term, co_tail - cx_binder + co_binder + co_bindee);
            }
            | Term::Residual(inner) => {
                let Residual(body) = inner;
                let co_body = self.coctxs_term_local[body].to_owned();
                self.coctxs_term_local.insert_new(term, co_body);
            }
            | Term::Block(inner) => {
                let Block(body) = inner;
                let co_body = self.coctxs_term_local[body].to_owned();
                self.coctxs_term_local.insert_new(term, co_body);
            }
            | Term::RecGroup(inner) => {
                let RecGroup { definitions, tail } = inner;
                let bound = definitions.iter().fold(Context::new(), |ctx, definition| {
                    ctx + self.ctxs_pat_local[&definition.binder].to_owned()
                });
                let free_definitions =
                    definitions.iter().fold(CoContext::new(), |ctx, definition| {
                        ctx + self.coctxs_pat_local[&definition.binder].to_owned()
                            + self.coctxs_term_local[&definition.bindee].to_owned()
                    });
                let free_tail = self.coctxs_term_local[tail].to_owned();
                self.coctxs_term_local.insert_new(term, free_definitions + free_tail - bound);
            }
            | Term::MoBlock(inner) => {
                let MoBlock { body, basis } = &**inner;
                let co_body = self.coctxs_term_local[body].to_owned();
                let co_basis = self.coctxs_term_local[&basis.monad].to_owned()
                    + self.coctxs_term_local[&basis.algebra].to_owned();
                self.coctxs_term_local.insert_new(term, co_body + co_basis);
            }
            | Term::CbvBlock(inner) => {
                let CbvBlock { body } = inner;
                let co_body = self.coctxs_term_local[body].to_owned();
                self.coctxs_term_local.insert_new(term, co_body);
            }
            | Term::Data(inner) => {
                let Data { arms } = inner;
                let co_arms =
                    CoContext::from_iter(arms.iter().flat_map(|DataArm { name: _, param }| {
                        self.coctxs_term_local[param].to_owned()
                    }));
                self.coctxs_term_local.insert_new(term, co_arms);
            }
            | Term::CoData(inner) => {
                let CoData { arms } = inner;
                let co_arms =
                    CoContext::from_iter(arms.iter().flat_map(|CoDataArm { name: _, out }| {
                        self.coctxs_term_local[out].to_owned()
                    }));
                self.coctxs_term_local.insert_new(term, co_arms);
            }
            | Term::Ctor(inner) => {
                let Ctor(_name, body) = inner;
                let co_body = self.coctxs_term_local[body].to_owned();
                self.coctxs_term_local.insert_new(term, co_body);
            }
            | Term::Match(inner) => {
                let Match { scrut, arms } = inner;
                let co_arms =
                    CoContext::from_iter(arms.iter().flat_map(|Matcher { binder, tail }| {
                        let cx_binder = self.ctxs_pat_local[binder].to_owned();
                        let co_binder = self.coctxs_pat_local[binder].to_owned();
                        let co_tail = self.coctxs_term_local[tail].to_owned();
                        co_tail - cx_binder + co_binder
                    }));
                let co_scrut = self.coctxs_term_local[scrut].to_owned();
                self.coctxs_term_local.insert_new(term, co_arms + co_scrut);
            }
            | Term::CoMatchClauses(inner) => {
                let CoMatchClauses { clauses } = inner;
                let co_clauses = CoContext::from_iter(clauses.iter().flat_map(
                    |CoPatternClause { spine, tail }| {
                        spine.iter().rev().fold(
                            self.coctxs_term_local[tail].to_owned(),
                            |free, item| match item {
                                | CoPatternItem::Pat(pattern) => {
                                    free - self.ctxs_pat_local[pattern].to_owned()
                                        + self.coctxs_pat_local[pattern].to_owned()
                                }
                                | CoPatternItem::Dtor(_) => free,
                            },
                        )
                    },
                ));
                self.coctxs_term_local.insert_new(term, co_clauses);
            }
            | Term::CoMatch(inner) => {
                let CoMatch { arms } = inner;
                let co_arms =
                    CoContext::from_iter(arms.iter().flat_map(|CoMatcher { dtor: _, tail }| {
                        self.coctxs_term_local[tail].to_owned()
                    }));
                self.coctxs_term_local.insert_new(term, co_arms);
            }
            | Term::Dtor(inner) => {
                let Dtor(body, _name) = inner;
                let co_body = self.coctxs_term_local[body].to_owned();
                self.coctxs_term_local.insert_new(term, co_body);
            }
            | Term::Proj(inner) => {
                let Proj(head, _name) = inner;
                let co_head = self.coctxs_term_local[head].to_owned();
                self.coctxs_term_local.insert_new(term, co_head);
            }
            | Term::Lit(inner) => {
                let _lit = inner;
                self.coctxs_term_local.insert_new(term, CoContext::new());
            }
        }
    }
}
