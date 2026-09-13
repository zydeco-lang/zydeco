//! Owned, one-level rebuilding for the shared bitter and scoped syntax family.
//!
//! `fold_with` describes child positions; a [`Folder`] decides what following an
//! ID means, including arena lookup, recursion, scope, and identity reuse. These
//! operations do not issue arena IDs, memoize, or infer binding scope on their own.

use crate::bitter::syntax::*;

/// A same-representation transformation of syntax children and references.
pub trait Folder {
    /// Source names in bitter syntax, or definition IDs in scoped syntax.
    type Ref;

    fn fold_def(&mut self, id: DefId) -> DefId;
    fn fold_pat(&mut self, id: PatId) -> PatId;
    fn fold_term(&mut self, id: TermId) -> TermId;

    /// References are separate from definitions so a binder rewrite need not
    /// change occurrences, and a resolved rewrite can apply its own renaming.
    fn fold_ref(&mut self, reference: Self::Ref) -> Self::Ref {
        reference
    }
}

impl Pattern {
    /// Rebuild immediate children in structural copying order.
    pub fn fold_with(self, folder: &mut impl Folder) -> Self {
        match self {
            | Self::Ann(Ann { tm, ty }) => {
                Ann { tm: folder.fold_pat(tm), ty: folder.fold_term(ty) }.into()
            }
            | Self::Var(definition) => folder.fold_def(definition).into(),
            | Self::Named(Named(name, pattern)) => Named(name, folder.fold_pat(pattern)).into(),
            | Self::Ctor(Ctor(name, pattern)) => Ctor(name, folder.fold_pat(pattern)).into(),
            | Self::Project(ProjectionPattern(field, pattern)) => {
                ProjectionPattern(field, folder.fold_pat(pattern)).into()
            }
            | Self::View(ViewPattern { function, pattern }) => ViewPattern {
                function: folder.fold_term(function),
                pattern: folder.fold_pat(pattern),
            }
            .into(),
            | Self::Alias(Alias(ConsN(patterns, tail))) => Alias(ConsN(
                patterns.into_iter().map(|pattern| folder.fold_pat(pattern)).collect(),
                folder.fold_pat(tail),
            ))
            .into(),
            | Self::Cons(patterns) => {
                Self::Cons(patterns.into_iter().map(|pattern| folder.fold_pat(pattern)).collect())
            }
            | Self::Hole(_) | Self::Triv(_) | Self::Lit(_) => self,
        }
    }
}

impl CoPatternItem {
    pub fn fold_with(self, folder: &mut impl Folder) -> Self {
        match self {
            | Self::Pat(pattern) => Self::Pat(folder.fold_pat(pattern)),
            | Self::Dtor(_) => self,
        }
    }
}

impl CoPatternSpine {
    pub fn fold_with(self, folder: &mut impl Folder) -> Self {
        Self {
            head: self.head.fold_with(folder),
            tail: self.tail.into_iter().map(|item| item.fold_with(folder)).collect(),
        }
    }
}

impl<Ref> Term<Ref> {
    /// Rebuild immediate children in structural copying order. Binder-bearing
    /// records process the binder first; monadic blocks process the body first.
    /// This order preserves bitter copying and does not establish lexical scope.
    pub fn fold_with(self, folder: &mut impl Folder<Ref = Ref>) -> Self {
        match self {
            | Self::Meta(term) => {
                let MetaT(meta, term) = *term;
                MetaT(meta, folder.fold_term(term)).into()
            }
            | Self::TypeOf(TypeOf(operand)) => TypeOf(folder.fold_term(operand)).into(),
            | Self::SourceBoundary(SourceBoundary(term)) => {
                SourceBoundary(folder.fold_term(term)).into()
            }
            | Self::SignatureBoundary(SignatureBoundary(term)) => {
                SignatureBoundary(folder.fold_term(term)).into()
            }
            | Self::Sealed(Sealed(term)) => Sealed(folder.fold_term(term)).into(),
            | Self::Ann(Ann { tm, ty }) => {
                Ann { tm: folder.fold_term(tm), ty: folder.fold_term(ty) }.into()
            }
            | Self::Var(reference) => Self::Var(folder.fold_ref(reference)),
            | Self::Named(Named(name, term)) => Named(name, folder.fold_term(term)).into(),
            | Self::Label(Label(name, term)) => Label(name, folder.fold_term(term)).into(),
            | Self::Cons(terms) => {
                Self::Cons(terms.into_iter().map(|term| folder.fold_term(term)).collect())
            }
            | Self::Abs(Abs(parameter, body)) => {
                Abs(folder.fold_pat(parameter), folder.fold_term(body)).into()
            }
            | Self::ValAbs(Abs(parameter, body)) => {
                Self::ValAbs(Abs(folder.fold_pat(parameter), folder.fold_term(body)))
            }
            | Self::App(App(function, argument)) => {
                App(folder.fold_term(function), folder.fold_term(argument)).into()
            }
            | Self::Fix(Fix(binder, body)) => {
                Fix(folder.fold_pat(binder), folder.fold_term(body)).into()
            }
            | Self::Pi(Pi(parameter, body)) => {
                Pi(folder.fold_pat(parameter), folder.fold_term(body)).into()
            }
            | Self::ValPi(ValPi(parameter, body)) => {
                ValPi(folder.fold_pat(parameter), folder.fold_term(body)).into()
            }
            | Self::Sigma(Sigma(parameter, body)) => {
                Sigma(folder.fold_pat(parameter), folder.fold_term(body)).into()
            }
            | Self::ManifestExists(term) => {
                let ManifestExists { binder, definition, body } = *term;
                ManifestExists {
                    binder: folder.fold_pat(binder),
                    definition: folder.fold_term(definition),
                    body: folder.fold_term(body),
                }
                .into()
            }
            | Self::Pack(term) => {
                let Pack { mode, binder, definition, body } = *term;
                Pack {
                    mode,
                    binder: folder.fold_pat(binder),
                    definition: folder.fold_term(definition),
                    body: folder.fold_term(body),
                }
                .into()
            }
            | Self::Thunk(Thunk(term)) => Thunk(folder.fold_term(term)).into(),
            | Self::Force(Force(term)) => Force(folder.fold_term(term)).into(),
            | Self::Ret(Return(term)) => Return(folder.fold_term(term)).into(),
            | Self::Do(term) => {
                let Bind { binder, bindee, tail } = *term;
                Bind {
                    binder: folder.fold_pat(binder),
                    bindee: folder.fold_term(bindee),
                    tail: folder.fold_term(tail),
                }
                .into()
            }
            | Self::Let(term) => {
                let Let { binder, bindee, tail } = *term;
                Let {
                    binder: folder.fold_pat(binder),
                    bindee: folder.fold_term(bindee),
                    tail: folder.fold_term(tail),
                }
                .into()
            }
            | Self::MobileParam(MobileParam { flavor, binder, tail }) => MobileParam {
                flavor,
                binder: folder.fold_pat(binder),
                tail: folder.fold_term(tail),
            }
            .into(),
            | Self::MobileBind(term) => {
                let MobileBind { binder, bindee, tail } = *term;
                MobileBind {
                    binder: folder.fold_pat(binder),
                    bindee: folder.fold_term(bindee),
                    tail: folder.fold_term(tail),
                }
                .into()
            }
            | Self::Residual(Residual(body)) => Residual(folder.fold_term(body)).into(),
            | Self::Block(Block(body)) => Block(folder.fold_term(body)).into(),
            | Self::RecGroup(RecGroup { definitions, tail }) => RecGroup {
                definitions: definitions
                    .into_iter()
                    .map(|RecursiveDefinition { binder, bindee }| RecursiveDefinition {
                        binder: folder.fold_pat(binder),
                        bindee: folder.fold_term(bindee),
                    })
                    .collect(),
                tail: folder.fold_term(tail),
            }
            .into(),
            | Self::MoBlock(term) => {
                let MoBlock { body, basis } = *term;
                MoBlock {
                    body: folder.fold_term(body),
                    basis: MonadicBasis {
                        monad: folder.fold_term(basis.monad),
                        algebra: folder.fold_term(basis.algebra),
                    },
                }
                .into()
            }
            | Self::Data(Data { arms }) => Data {
                arms: arms
                    .into_iter()
                    .map(|DataArm { name, param }| DataArm { name, param: folder.fold_term(param) })
                    .collect(),
            }
            .into(),
            | Self::CoData(CoData { arms }) => CoData {
                arms: arms
                    .into_iter()
                    .map(|CoDataArm { name, out }| CoDataArm { name, out: folder.fold_term(out) })
                    .collect(),
            }
            .into(),
            | Self::Ctor(Ctor(name, term)) => Ctor(name, folder.fold_term(term)).into(),
            | Self::Match(Match { scrut, arms }) => Match {
                scrut: folder.fold_term(scrut),
                arms: arms
                    .into_iter()
                    .map(|Matcher { binder, tail }| Matcher {
                        binder: folder.fold_pat(binder),
                        tail: folder.fold_term(tail),
                    })
                    .collect(),
            }
            .into(),
            | Self::CoMatchClauses(CoMatchClauses { clauses }) => CoMatchClauses {
                clauses: clauses
                    .into_iter()
                    .map(|CoPatternClause { spine, tail }| CoPatternClause {
                        spine: spine.fold_with(folder),
                        tail: folder.fold_term(tail),
                    })
                    .collect(),
            }
            .into(),
            | Self::CoMatch(CoMatch { arms }) => CoMatch {
                arms: arms
                    .into_iter()
                    .map(|CoMatcher { dtor, tail }| CoMatcher {
                        dtor,
                        tail: folder.fold_term(tail),
                    })
                    .collect(),
            }
            .into(),
            | Self::Dtor(Dtor(term, name)) => Dtor(folder.fold_term(term), name).into(),
            | Self::Proj(Proj(term, name)) => Proj(folder.fold_term(term), name).into(),
            | Self::Internal(_) | Self::Hole(_) | Self::Triv(_) | Self::Lit(_) => self,
        }
    }
}
