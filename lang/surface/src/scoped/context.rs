//! Free-variable summaries over the elaborated scoped graph.

use super::{
    syntax::*,
    traverse::{Node, Traversal, Visitor},
};
use std::{convert::Infallible, ops::ControlFlow};

/// Free-variable summaries needed while checking resolved terms.
#[derive(Debug, Default)]
pub struct TermContexts {
    coctxs_term_local: ArenaPagedAssoc<TermId, CoContext>,
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
}

/// A postorder analysis that can share a traversal with other visitors.
/// Its node summaries are independent of the incoming lexical environment.
#[derive(Default)]
pub struct ContextCollector {
    ctxs_pat_local: ArenaAssoc<PatId, Context>,
    coctxs_pat_local: ArenaAssoc<PatId, CoContext>,
    coctxs_term_local: ArenaPagedAssoc<TermId, CoContext>,
}

impl ContextCollector {
    /// Retain term summaries and release temporary pattern summaries.
    /// Only nodes reached by a completed traversal have summaries.
    pub fn finish(self) -> TermContexts {
        TermContexts { coctxs_term_local: self.coctxs_term_local }
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

    /// Updates [`Self::coctxs_term_local`].
    fn term(&mut self, term: TermId, item: &Term<DefId>) {
        if self.coctxs_term_local.get(&term).is_some() {
            return;
        }
        match item {
            | Term::Meta(inner) => {
                let MetaT(_meta, inner) = &**inner;
                let co_term = self.coctxs_term_local[inner].to_owned();
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
            | Term::Abs(inner) | Term::ValAbs(inner) => {
                let Abs(pat, body) = inner;
                let co_body = self.coctxs_term_local[body].to_owned();
                let cx_pat = self.ctxs_pat_local[pat].to_owned();
                let co_pat = self.coctxs_pat_local[pat].to_owned();
                self.coctxs_term_local.insert_new(term, co_body - cx_pat + co_pat);
            }
            | Term::App(inner) => {
                let App(a, b) = inner;
                let co_a = self.coctxs_term_local[a].to_owned();
                let co_b = self.coctxs_term_local[b].to_owned();
                self.coctxs_term_local.insert_new(term, co_a + co_b);
            }
            | Term::Fix(inner) => {
                let Fix(pat, body) = inner;
                let co_body = self.coctxs_term_local[body].to_owned();
                let cx_pat = self.ctxs_pat_local[pat].to_owned();
                let co_pat = self.coctxs_pat_local[pat].to_owned();
                self.coctxs_term_local.insert_new(term, co_body - cx_pat + co_pat);
            }
            | Term::Pi(inner) => {
                let Pi(pat, body) = inner;
                let co_body = self.coctxs_term_local[body].to_owned();
                let cx_pat = self.ctxs_pat_local[pat].to_owned();
                let co_pat = self.coctxs_pat_local[pat].to_owned();
                self.coctxs_term_local.insert_new(term, co_body - cx_pat + co_pat);
            }
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
            | Term::MobileParam(_) | Term::MobileBind(_) => {
                unreachable!("mobile syntax must be eliminated during name resolution")
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
