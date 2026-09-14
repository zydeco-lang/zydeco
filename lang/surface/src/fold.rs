//! Owned, one-level rebuilding for the shared bitter and scoped syntax family.
//!
//! `fold_with` describes child positions. A folder owns lookup, recursion, scope,
//! identity, and recovery; these operations do not allocate or memoize on their own.
use crate::bitter::syntax::*;

pub trait Folder: Sized {
    type InputRef;
    type OutputRef;
    type Error;
    fn fold_def(&mut self, id: DefId) -> Result<DefId, Self::Error>;
    fn fold_pat(&mut self, id: PatId) -> Result<PatId, Self::Error>;
    fn fold_term(&mut self, id: TermId) -> Result<TermId, Self::Error>;
    fn fold_var(&mut self, reference: Self::InputRef)
    -> Result<Term<Self::OutputRef>, Self::Error>;

    /// Recovery-aware folders override this to visit every independent item.
    /// Errors must already be recorded when such a folder propagates rejection.
    fn fold_items<T, U>(
        &mut self, items: impl IntoIterator<Item = T>,
        mut fold: impl FnMut(&mut Self, T) -> Result<U, Self::Error>,
    ) -> Result<Vec<U>, Self::Error> {
        items.into_iter().map(|item| fold(self, item)).collect()
    }
}

impl Pattern {
    pub fn fold_with<F: Folder>(self, folder: &mut F) -> Result<Self, F::Error> {
        Ok(match self {
            | Self::Ann(Ann { tm, ty }) => {
                let tm = folder.fold_pat(tm);
                let ty = folder.fold_term(ty);
                Ann { tm: tm?, ty: ty? }.into()
            }
            | Self::Var(def) => folder.fold_def(def)?.into(),
            | Self::Named(Named(name, pat)) => Named(name, folder.fold_pat(pat)?).into(),
            | Self::Ctor(Ctor(name, pat)) => Ctor(name, folder.fold_pat(pat)?).into(),
            | Self::Project(ProjectionPattern(field, pat)) => {
                ProjectionPattern(field, folder.fold_pat(pat)?).into()
            }
            | Self::View(ViewPattern { function, pattern }) => {
                let function = folder.fold_term(function);
                let pattern = folder.fold_pat(pattern);
                ViewPattern { function: function?, pattern: pattern? }.into()
            }
            | Self::Alias(Alias(ConsN(patterns, tail))) => {
                let patterns = folder.fold_items(patterns, F::fold_pat);
                let tail = folder.fold_pat(tail);
                Alias(ConsN(patterns?, tail?)).into()
            }
            | Self::Cons(patterns) => Self::Cons(folder.fold_items(patterns, F::fold_pat)?),
            | Self::Hole(_) | Self::Triv(_) | Self::Lit(_) => self,
        })
    }
}

impl CoPatternItem {
    pub fn fold_with<F: Folder>(self, folder: &mut F) -> Result<Self, F::Error> {
        Ok(match self {
            | Self::Pat(pat) => Self::Pat(folder.fold_pat(pat)?),
            | Self::Dtor(_) => self,
        })
    }
}
impl CoPatternSpine {
    pub fn fold_with<F: Folder>(self, folder: &mut F) -> Result<Self, F::Error> {
        let head = self.head.fold_with(folder);
        let tail = folder.fold_items(self.tail, |folder, item| item.fold_with(folder));
        Ok(Self { head: head?, tail: tail? })
    }
}

impl<R> Term<R> {
    /// Structural copying order: binders precede their bodies, and a monadic body
    /// precedes its basis. Semantic folders override scheduling where scope requires it.
    pub fn fold_with<F: Folder<InputRef = R>>(
        self, folder: &mut F,
    ) -> Result<Term<F::OutputRef>, F::Error> {
        Ok(match self {
            | Self::Meta(term) => {
                let MetaT(meta, term) = *term;
                MetaT(meta, folder.fold_term(term)?).into()
            }
            | Self::TypeOf(TypeOf(term)) => TypeOf(folder.fold_term(term)?).into(),
            | Self::SourceBoundary(SourceBoundary(term)) => {
                SourceBoundary(folder.fold_term(term)?).into()
            }
            | Self::SignatureBoundary(SignatureBoundary(term)) => {
                SignatureBoundary(folder.fold_term(term)?).into()
            }
            | Self::Sealed(Sealed(term)) => Sealed(folder.fold_term(term)?).into(),
            | Self::Ann(Ann { tm, ty }) => {
                let tm = folder.fold_term(tm);
                let ty = folder.fold_term(ty);
                Ann { tm: tm?, ty: ty? }.into()
            }
            | Self::Var(reference) => folder.fold_var(reference)?,
            | Self::Named(Named(name, term)) => Named(name, folder.fold_term(term)?).into(),
            | Self::Label(Label(name, term)) => Label(name, folder.fold_term(term)?).into(),
            | Self::Cons(terms) => Term::Cons(folder.fold_items(terms, F::fold_term)?),
            | Self::Abs(Abs(pat, body)) => {
                let pat = folder.fold_pat(pat);
                let body = folder.fold_term(body);
                Abs(pat?, body?).into()
            }
            | Self::ValAbs(Abs(pat, body)) => {
                let pat = folder.fold_pat(pat);
                let body = folder.fold_term(body);
                Term::ValAbs(Abs(pat?, body?))
            }
            | Self::App(App(a, b)) => {
                let a = folder.fold_term(a);
                let b = folder.fold_term(b);
                App(a?, b?).into()
            }
            | Self::Fix(Fix(pat, body)) => {
                let pat = folder.fold_pat(pat);
                let body = folder.fold_term(body);
                Fix(pat?, body?).into()
            }
            | Self::Pi(Pi(pat, body)) => {
                let pat = folder.fold_pat(pat);
                let body = folder.fold_term(body);
                Pi(pat?, body?).into()
            }
            | Self::ValPi(ValPi(pat, body)) => {
                let pat = folder.fold_pat(pat);
                let body = folder.fold_term(body);
                ValPi(pat?, body?).into()
            }
            | Self::Sigma(Sigma(pat, body)) => {
                let pat = folder.fold_pat(pat);
                let body = folder.fold_term(body);
                Sigma(pat?, body?).into()
            }
            | Self::ManifestExists(term) => {
                let ManifestExists { binder, definition, body } = *term;
                let binder = folder.fold_pat(binder);
                let definition = folder.fold_term(definition);
                let body = folder.fold_term(body);
                ManifestExists { binder: binder?, definition: definition?, body: body? }.into()
            }
            | Self::Pack(term) => {
                let Pack { mode, binder, definition, body } = *term;
                let binder = folder.fold_pat(binder);
                let definition = folder.fold_term(definition);
                let body = folder.fold_term(body);
                Pack { mode, binder: binder?, definition: definition?, body: body? }.into()
            }
            | Self::Thunk(Thunk(term)) => Thunk(folder.fold_term(term)?).into(),
            | Self::Force(Force(term)) => Force(folder.fold_term(term)?).into(),
            | Self::Ret(Return(term)) => Return(folder.fold_term(term)?).into(),
            | Self::Do(term) => {
                let Bind { binder, bindee, tail } = *term;
                let binder = folder.fold_pat(binder);
                let bindee = folder.fold_term(bindee);
                let tail = folder.fold_term(tail);
                Bind { binder: binder?, bindee: bindee?, tail: tail? }.into()
            }
            | Self::Let(term) => {
                let Let { binder, bindee, tail } = *term;
                let binder = folder.fold_pat(binder);
                let bindee = folder.fold_term(bindee);
                let tail = folder.fold_term(tail);
                Let { binder: binder?, bindee: bindee?, tail: tail? }.into()
            }
            | Self::MobileParam(MobileParam { flavor, binder, tail }) => {
                let binder = folder.fold_pat(binder);
                let tail = folder.fold_term(tail);
                MobileParam { flavor, binder: binder?, tail: tail? }.into()
            }
            | Self::MobileBind(term) => {
                let MobileBind { binder, bindee, tail } = *term;
                let binder = folder.fold_pat(binder);
                let bindee = folder.fold_term(bindee);
                let tail = folder.fold_term(tail);
                MobileBind { binder: binder?, bindee: bindee?, tail: tail? }.into()
            }
            | Self::Residual(Residual(term)) => Residual(folder.fold_term(term)?).into(),
            | Self::Block(Block(term)) => Block(folder.fold_term(term)?).into(),
            | Self::RecGroup(RecGroup { definitions, tail }) => {
                let definitions = folder.fold_items(
                    definitions,
                    |folder, RecursiveDefinition { binder, bindee }| {
                        let binder = folder.fold_pat(binder);
                        let bindee = folder.fold_term(bindee);
                        Ok(RecursiveDefinition { binder: binder?, bindee: bindee? })
                    },
                );
                let tail = folder.fold_term(tail);
                RecGroup { definitions: definitions?, tail: tail? }.into()
            }
            | Self::MoBlock(term) => {
                let MoBlock { body, basis } = *term;
                let body = folder.fold_term(body);
                let monad = folder.fold_term(basis.monad);
                let algebra = folder.fold_term(basis.algebra);
                MoBlock { body: body?, basis: MonadicBasis { monad: monad?, algebra: algebra? } }
                    .into()
            }
            | Self::Data(Data { arms }) => Data {
                arms: folder.fold_items(arms, |folder, DataArm { name, param }| {
                    Ok(DataArm { name, param: folder.fold_term(param)? })
                })?,
            }
            .into(),
            | Self::CoData(CoData { arms }) => CoData {
                arms: folder.fold_items(arms, |folder, CoDataArm { name, out }| {
                    Ok(CoDataArm { name, out: folder.fold_term(out)? })
                })?,
            }
            .into(),
            | Self::Ctor(Ctor(name, term)) => Ctor(name, folder.fold_term(term)?).into(),
            | Self::Match(Match { scrut, arms }) => {
                let scrut = folder.fold_term(scrut);
                let arms = folder.fold_items(arms, |folder, Matcher { binder, tail }| {
                    let binder = folder.fold_pat(binder);
                    let tail = folder.fold_term(tail);
                    Ok(Matcher { binder: binder?, tail: tail? })
                });
                Match { scrut: scrut?, arms: arms? }.into()
            }
            | Self::CoMatchClauses(CoMatchClauses { clauses }) => CoMatchClauses {
                clauses: folder.fold_items(
                    clauses,
                    |folder, CoPatternClause { spine, tail }| {
                        let spine = spine.fold_with(folder);
                        let tail = folder.fold_term(tail);
                        Ok(CoPatternClause { spine: spine?, tail: tail? })
                    },
                )?,
            }
            .into(),
            | Self::CoMatch(CoMatch { arms }) => CoMatch {
                arms: folder.fold_items(arms, |folder, CoMatcher { dtor, tail }| {
                    Ok(CoMatcher { dtor, tail: folder.fold_term(tail)? })
                })?,
            }
            .into(),
            | Self::Dtor(Dtor(term, name)) => Dtor(folder.fold_term(term)?, name).into(),
            | Self::Proj(Proj(term, name)) => Proj(folder.fold_term(term)?, name).into(),
            | Self::Internal(term) => Term::Internal(term),
            | Self::Hole(term) => Term::Hole(term),
            | Self::Triv(term) => Term::Triv(term),
            | Self::Lit(term) => Term::Lit(term),
        })
    }
}
